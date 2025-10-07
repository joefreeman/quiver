use quiver_core::bytecode::{Constant, Function, Instruction, TypeId};
use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus, StepResult};
use quiver_core::program::Program;
use quiver_core::types::{TupleTypeInfo, Type};
use quiver_core::value::Value;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::mpsc::{Receiver, SyncSender, sync_channel};
use std::sync::{Arc, Mutex, MutexGuard, RwLock, RwLockWriteGuard};
use std::thread::{self, JoinHandle};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum SchedulerCommand {
    Execute {
        request_id: u64,
        process_id: ProcessId,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
        variables: Option<HashMap<String, (Type, usize)>>,
    },
    SpawnProcess {
        request_id: u64,
        id: ProcessId,
        persistent: bool,
    },
    GetProcessStatuses {
        request_id: u64,
    },
    GetProcessInfo {
        request_id: u64,
        id: ProcessId,
    },
    GetVariables {
        request_id: u64,
        process_id: ProcessId,
        mapping: HashMap<String, usize>,
    },
    RegisterConstant {
        index: usize,
        constant: Constant,
    },
    RegisterFunction {
        index: usize,
        function: Function,
    },
    RegisterBuiltin {
        index: usize,
        name: String,
    },
    RegisterType {
        type_id: TypeId,
        info: TupleTypeInfo,
    },
    Shutdown,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum SchedulerResponse {
    Execute {
        request_id: u64,
        result: Result<(Option<Value>, Option<HashMap<String, (Type, usize)>>), Error>,
    },
    SpawnProcess {
        request_id: u64,
        result: Result<(), Error>,
    },
    GetProcessStatuses {
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, Error>,
    },
    GetProcessInfo {
        request_id: u64,
        result: Result<Option<ProcessInfo>, Error>,
    },
    GetVariables {
        request_id: u64,
        result: Result<HashMap<String, Value>, Error>,
    },
}

#[derive(Debug)]
pub struct NativeRuntime {
    program: Arc<RwLock<Program>>,
    next_process_id: Arc<AtomicUsize>,
    executor: Arc<Mutex<Executor>>,
    command_tx: SyncSender<SchedulerCommand>,
    pending: Arc<Mutex<HashMap<u64, SyncSender<SchedulerResponse>>>>,
    thread_handle: Option<JoinHandle<()>>,
    next_request_id: Arc<AtomicU64>,
}

impl NativeRuntime {
    pub fn new(program: Program) -> Self {
        let program = Arc::new(RwLock::new(program));
        let next_process_id = Arc::new(AtomicUsize::new(0));

        // Initialize executor with current program state
        let program_snapshot = program.read().unwrap();
        let executor = Arc::new(Mutex::new(Executor::new(&*program_snapshot)));
        drop(program_snapshot);

        let (command_tx, command_rx) = sync_channel(100);
        let pending: Arc<Mutex<HashMap<u64, SyncSender<SchedulerResponse>>>> =
            Arc::new(Mutex::new(HashMap::new()));
        let next_request_id = Arc::new(AtomicU64::new(0));

        let executor_clone = Arc::clone(&executor);
        let next_process_id_clone = Arc::clone(&next_process_id);
        let pending_clone = Arc::clone(&pending);
        let thread_handle = thread::spawn(move || {
            scheduler_thread(
                executor_clone,
                next_process_id_clone,
                command_rx,
                pending_clone,
            );
        });

        Self {
            program,
            next_process_id,
            executor,
            command_tx,
            pending,
            thread_handle: Some(thread_handle),
            next_request_id,
        }
    }

    pub fn executor(&self) -> MutexGuard<'_, Executor> {
        self.executor.lock().unwrap()
    }

    pub fn program(&self) -> Arc<RwLock<Program>> {
        Arc::clone(&self.program)
    }

    pub fn program_mut(&self) -> RwLockWriteGuard<'_, Program> {
        self.program.write().unwrap()
    }

    pub fn register_constant(&self, constant: Constant) -> usize {
        let mut program = self.program.write().unwrap();
        let index = program.register_constant(constant.clone());
        drop(program);
        self.sync_constant(index, constant);
        index
    }

    pub fn register_function(&self, function: Function) -> usize {
        let mut program = self.program.write().unwrap();
        let index = program.register_function(function.clone());
        drop(program);
        self.sync_function(index, function);
        index
    }

    pub fn register_builtin(&self, name: String) -> usize {
        let mut program = self.program.write().unwrap();
        let index = program.register_builtin(name.clone());
        drop(program);
        self.sync_builtin(index, name);
        index
    }

    pub fn register_type(
        &self,
        name: Option<String>,
        fields: Vec<(Option<String>, Type)>,
    ) -> TypeId {
        let mut program = self.program.write().unwrap();
        let type_id = program.register_type(name.clone(), fields.clone());
        let info = (name, fields);
        drop(program);
        self.sync_type(type_id, info);
        type_id
    }

    /// Update the Program with a new one, syncing only the differences to the executor.
    pub fn update_program(&self, new_program: Program) {
        let old_program = self.program.read().unwrap();

        // Sync new/modified constants
        let old_constants = old_program.get_constants();
        let new_constants = new_program.get_constants();
        for (index, constant) in new_constants.iter().enumerate() {
            if index >= old_constants.len() || &old_constants[index] != constant {
                self.sync_constant(index, constant.clone());
            }
        }

        // Sync new/modified functions
        let old_functions = old_program.get_functions();
        let new_functions = new_program.get_functions();
        for (index, function) in new_functions.iter().enumerate() {
            if index >= old_functions.len() || &old_functions[index] != function {
                self.sync_function(index, function.clone());
            }
        }

        // Sync new/modified builtins
        let old_builtins = old_program.get_builtins();
        let new_builtins = new_program.get_builtins();
        for (index, builtin) in new_builtins.iter().enumerate() {
            if index >= old_builtins.len() || &old_builtins[index] != builtin {
                self.sync_builtin(index, builtin.clone());
            }
        }

        // Sync new/modified types
        let old_types = old_program.get_types();
        let new_types = new_program.get_types();
        for (type_id, info) in new_types.iter() {
            if !old_types.contains_key(type_id) || &old_types[type_id] != info {
                self.sync_type(*type_id, info.clone());
            }
        }

        drop(old_program);
        *self.program.write().unwrap() = new_program;
    }

    pub fn spawn_process(&self, persistent: bool) -> Result<ProcessId, Error> {
        let id = ProcessId(self.next_process_id.fetch_add(1, Ordering::SeqCst));
        self.spawn_process_with_id(id, persistent)?;
        Ok(id)
    }

    pub fn get_process_statuses(&self) -> Result<HashMap<ProcessId, ProcessStatus>, Error> {
        self.get_process_statuses_internal()
    }

    pub fn get_process_info(&self, id: ProcessId) -> Result<Option<ProcessInfo>, Error> {
        self.get_process_info_internal(id)
    }

    pub fn execute_instructions(
        &self,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
        process_id: Option<ProcessId>,
        variables: Option<HashMap<String, (Type, usize)>>,
    ) -> Result<(Option<Value>, Option<HashMap<String, (Type, usize)>>), Error> {
        let pid = if let Some(id) = process_id {
            id
        } else {
            self.spawn_process(false)?
        };

        self.execute(pid, instructions, parameter, variables)
    }

    pub fn execute_function(&self, entry: usize) -> Result<Option<Value>, Error> {
        let function = self
            .program
            .read()
            .unwrap()
            .get_function(entry)
            .ok_or(Error::FunctionUndefined(entry))?
            .clone();

        let pid = self.spawn_process(false)?;
        let (result, _) = self.execute(pid, function.instructions, Some(Value::nil()), None)?;
        Ok(result)
    }

    pub fn get_stack(&self, process_id: ProcessId) -> Option<Vec<Value>> {
        self.executor
            .lock()
            .unwrap()
            .get_process(process_id)
            .map(|p| p.stack.clone())
    }

    pub fn frame_count(&self, process_id: ProcessId) -> Option<usize> {
        self.executor
            .lock()
            .unwrap()
            .get_process(process_id)
            .map(|p| p.frames.len())
    }

    pub fn get_variables(
        &self,
        process_id: ProcessId,
        mapping: &HashMap<String, usize>,
    ) -> Result<HashMap<String, Value>, Error> {
        self.get_variables_internal(process_id, mapping.clone())
    }

    fn send_request<F>(&self, build_command: F) -> Result<SchedulerResponse, Error>
    where
        F: FnOnce(u64) -> SchedulerCommand,
    {
        let request_id = self.next_request_id.fetch_add(1, Ordering::SeqCst);
        let (response_tx, response_rx) = sync_channel(1);
        self.pending.lock().unwrap().insert(request_id, response_tx);

        let command = build_command(request_id);
        self.command_tx
            .send(command)
            .map_err(|_| Error::InvalidArgument("Scheduler thread died".to_string()))?;

        response_rx
            .recv()
            .map_err(|_| Error::InvalidArgument("Scheduler thread died".to_string()))
    }

    fn execute(
        &self,
        process_id: ProcessId,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
        variables: Option<HashMap<String, (Type, usize)>>,
    ) -> Result<(Option<Value>, Option<HashMap<String, (Type, usize)>>), Error> {
        let response = self.send_request(|request_id| SchedulerCommand::Execute {
            request_id,
            process_id,
            instructions,
            parameter,
            variables,
        })?;

        match response {
            SchedulerResponse::Execute { result, .. } => result,
            _ => Err(Error::InvalidArgument(
                "Unexpected response type".to_string(),
            )),
        }
    }

    fn spawn_process_with_id(&self, id: ProcessId, persistent: bool) -> Result<(), Error> {
        let response = self.send_request(|request_id| SchedulerCommand::SpawnProcess {
            request_id,
            id,
            persistent,
        })?;

        match response {
            SchedulerResponse::SpawnProcess { result, .. } => result,
            _ => Err(Error::InvalidArgument(
                "Unexpected response type".to_string(),
            )),
        }
    }

    fn get_process_statuses_internal(&self) -> Result<HashMap<ProcessId, ProcessStatus>, Error> {
        let response =
            self.send_request(|request_id| SchedulerCommand::GetProcessStatuses { request_id })?;

        match response {
            SchedulerResponse::GetProcessStatuses { result, .. } => result,
            _ => Err(Error::InvalidArgument(
                "Unexpected response type".to_string(),
            )),
        }
    }

    fn get_process_info_internal(&self, id: ProcessId) -> Result<Option<ProcessInfo>, Error> {
        let response =
            self.send_request(|request_id| SchedulerCommand::GetProcessInfo { request_id, id })?;

        match response {
            SchedulerResponse::GetProcessInfo { result, .. } => result,
            _ => Err(Error::InvalidArgument(
                "Unexpected response type".to_string(),
            )),
        }
    }

    fn get_variables_internal(
        &self,
        process_id: ProcessId,
        mapping: HashMap<String, usize>,
    ) -> Result<HashMap<String, Value>, Error> {
        let response = self.send_request(|request_id| SchedulerCommand::GetVariables {
            request_id,
            process_id,
            mapping,
        })?;

        match response {
            SchedulerResponse::GetVariables { result, .. } => result,
            _ => Err(Error::InvalidArgument(
                "Unexpected response type".to_string(),
            )),
        }
    }

    fn sync_constant(&self, index: usize, constant: Constant) {
        let _ = self
            .command_tx
            .send(SchedulerCommand::RegisterConstant { index, constant });
    }

    fn sync_function(&self, index: usize, function: Function) {
        let _ = self
            .command_tx
            .send(SchedulerCommand::RegisterFunction { index, function });
    }

    fn sync_builtin(&self, index: usize, name: String) {
        let _ = self
            .command_tx
            .send(SchedulerCommand::RegisterBuiltin { index, name });
    }

    fn sync_type(&self, type_id: TypeId, info: TupleTypeInfo) {
        let _ = self
            .command_tx
            .send(SchedulerCommand::RegisterType { type_id, info });
    }

    pub fn shutdown(&mut self) {
        let _ = self.command_tx.send(SchedulerCommand::Shutdown);
        if let Some(handle) = self.thread_handle.take() {
            let _ = handle.join();
        }
    }
}

impl Drop for NativeRuntime {
    fn drop(&mut self) {
        self.shutdown();
    }
}

fn scheduler_thread(
    executor: Arc<Mutex<Executor>>,
    next_process_id: Arc<AtomicUsize>,
    command_rx: Receiver<SchedulerCommand>,
    pending: Arc<Mutex<HashMap<u64, SyncSender<SchedulerResponse>>>>,
) {
    loop {
        match command_rx.recv() {
            Ok(SchedulerCommand::Execute {
                request_id,
                process_id,
                instructions,
                parameter,
                variables,
            }) => {
                let exec_result = execute_in_scheduler(
                    &executor,
                    &next_process_id,
                    process_id,
                    instructions,
                    parameter,
                );

                // Compact locals if variables map was provided
                let result = match exec_result {
                    Ok(value) => {
                        let compacted_variables = if let Some(vars) = variables {
                            let mut exec = executor.lock().unwrap();
                            let process = match exec.get_process_mut(process_id) {
                                Some(p) => p,
                                None => {
                                    if let Some(tx) = pending.lock().unwrap().remove(&request_id) {
                                        let _ = tx.send(SchedulerResponse::Execute {
                                            request_id,
                                            result: Ok((value, None)),
                                        });
                                    }
                                    continue;
                                }
                            };

                            let mut referenced: Vec<(String, Type, usize)> = vars
                                .iter()
                                .map(|(name, (typ, index))| (name.clone(), typ.clone(), *index))
                                .collect();
                            referenced.sort_by_key(|(_, _, index)| *index);

                            let mut new_locals = Vec::new();
                            let mut new_variables = HashMap::new();

                            for (name, typ, old_index) in referenced {
                                if old_index < process.locals.len() {
                                    new_variables.insert(name, (typ, new_locals.len()));
                                    new_locals.push(process.locals[old_index].clone());
                                }
                            }

                            process.locals = new_locals;
                            Some(new_variables)
                        } else {
                            None
                        };
                        Ok((value, compacted_variables))
                    }
                    Err(e) => Err(e),
                };

                if let Some(tx) = pending.lock().unwrap().remove(&request_id) {
                    let _ = tx.send(SchedulerResponse::Execute { request_id, result });
                }
            }
            Ok(SchedulerCommand::RegisterConstant { index, constant }) => {
                let mut exec = executor.lock().unwrap();
                exec.register_constant(index, constant);
            }
            Ok(SchedulerCommand::RegisterFunction { index, function }) => {
                let mut exec = executor.lock().unwrap();
                exec.register_function(index, function);
            }
            Ok(SchedulerCommand::RegisterBuiltin { index, name }) => {
                let mut exec = executor.lock().unwrap();
                exec.register_builtin(index, name);
            }
            Ok(SchedulerCommand::RegisterType { type_id, info }) => {
                let mut exec = executor.lock().unwrap();
                exec.register_type(type_id, info);
            }
            Ok(SchedulerCommand::SpawnProcess {
                request_id,
                id,
                persistent,
            }) => {
                let mut exec = executor.lock().unwrap();
                exec.spawn_process(id, persistent);
                if let Some(tx) = pending.lock().unwrap().remove(&request_id) {
                    let _ = tx.send(SchedulerResponse::SpawnProcess {
                        request_id,
                        result: Ok(()),
                    });
                }
            }
            Ok(SchedulerCommand::GetProcessStatuses { request_id }) => {
                let exec = executor.lock().unwrap();
                let statuses = exec.get_process_statuses();
                if let Some(tx) = pending.lock().unwrap().remove(&request_id) {
                    let _ = tx.send(SchedulerResponse::GetProcessStatuses {
                        request_id,
                        result: Ok(statuses),
                    });
                }
            }
            Ok(SchedulerCommand::GetProcessInfo { request_id, id }) => {
                let exec = executor.lock().unwrap();
                let info = exec.get_process_info(id);
                if let Some(tx) = pending.lock().unwrap().remove(&request_id) {
                    let _ = tx.send(SchedulerResponse::GetProcessInfo {
                        request_id,
                        result: Ok(info),
                    });
                }
            }
            Ok(SchedulerCommand::GetVariables {
                request_id,
                process_id,
                mapping,
            }) => {
                let exec = executor.lock().unwrap();
                let result = match exec.get_process(process_id) {
                    Some(process) => {
                        let mut vars = HashMap::new();
                        for (name, &index) in &mapping {
                            if index >= process.locals.len() {
                                break;
                            }
                            vars.insert(name.clone(), process.locals[index].clone());
                        }
                        if vars.len() == mapping.len() {
                            Ok(vars)
                        } else {
                            Err(Error::VariableUndefined(
                                "One or more variables out of bounds".to_string(),
                            ))
                        }
                    }
                    None => Err(Error::InvalidArgument(format!(
                        "Process {:?} not found",
                        process_id
                    ))),
                };
                if let Some(tx) = pending.lock().unwrap().remove(&request_id) {
                    let _ = tx.send(SchedulerResponse::GetVariables { request_id, result });
                }
            }
            Ok(SchedulerCommand::Shutdown) | Err(_) => {
                break;
            }
        }
    }
}

fn execute_in_scheduler(
    executor: &Arc<Mutex<Executor>>,
    next_process_id: &Arc<AtomicUsize>,
    process_id: ProcessId,
    instructions: Vec<Instruction>,
    parameter: Option<Value>,
) -> Result<Option<Value>, Error> {
    if instructions.is_empty() {
        return Ok(None);
    }

    {
        let mut exec = executor.lock().unwrap();
        exec.remove_from_queue(process_id);

        let process = exec
            .get_process_mut(process_id)
            .ok_or(Error::InvalidArgument(format!(
                "Process {:?} not found",
                process_id
            )))?;

        process.stack.push(parameter.unwrap_or_else(Value::nil));

        let frame = quiver_core::process::Frame::new(instructions.clone(), 0, 0);
        process.frames.push(frame);

        exec.set_active(process_id);
    }

    let result = run(executor, next_process_id);

    // Pop frame
    {
        let mut exec = executor.lock().unwrap();
        if let Some(process) = exec.get_process_mut(process_id) {
            process.frames.pop();
        }
    }

    result
}

fn run(
    executor: &Arc<Mutex<Executor>>,
    next_process_id: &Arc<AtomicUsize>,
) -> Result<Option<Value>, Error> {
    let initial_process = executor.lock().unwrap().get_active();
    let mut initial_result = None;

    // Use step() to execute until completion
    loop {
        // Call step with a reasonable batch size
        let step_result = executor.lock().unwrap().step(1000, next_process_id)?;

        // Check if initial process completed and grab its result
        // Must check even when Idle, since the process may have just finished
        if initial_result.is_none() {
            let mut exec = executor.lock().unwrap();
            if let Some(init_pid) = initial_process {
                // Check if process is not waiting - if so, extract result from stack
                let is_waiting = exec.is_waiting(init_pid);
                if !is_waiting {
                    if let Some(process) = exec.get_process_mut(init_pid) {
                        // Check if there are no more instructions to execute in current frame
                        let no_more_instructions = process
                            .frames
                            .last()
                            .map_or(true, |frame| frame.counter >= frame.instructions.len());

                        if no_more_instructions && !process.stack.is_empty() {
                            initial_result = process.stack.pop();
                        }
                    }
                }
            }
        }

        // Continue until executor is idle
        if step_result == StepResult::Idle {
            break;
        }
    }

    Ok(initial_result)
}
