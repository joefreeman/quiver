use crate::builtins::BUILTIN_REGISTRY;
use crate::bytecode::{Constant, Function, Instruction, TypeId};
use crate::program::Program;
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use crate::vm::{Binary, Error, Value};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::mpsc::{Receiver, SyncSender, sync_channel};
use std::sync::{Arc, Mutex, RwLock};
use std::thread::{self, JoinHandle};

/// Number of instruction units to execute per process before yielding to the next process
const TIME_SLICE_UNITS: usize = 100;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProcessId(pub usize);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ProcessStatus {
    Running,
    Queued,
    Waiting,
    Sleeping,
    Terminated,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessInfo {
    pub id: ProcessId,
    pub status: ProcessStatus,
    pub stack_size: usize,
    pub locals_size: usize,
    pub frames_count: usize,
    pub mailbox_size: usize,
    pub persistent: bool,
    pub result: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct Frame {
    instructions: Vec<Instruction>,
    locals_base: usize,
    captures_count: usize,
    counter: usize,
}

impl Frame {
    pub fn new(instructions: Vec<Instruction>, locals_base: usize, captures_count: usize) -> Self {
        Self {
            instructions,
            locals_base,
            captures_count,
            counter: 0,
        }
    }
}

#[derive(Debug)]
pub struct Process {
    pub id: ProcessId,
    pub stack: Vec<Value>,
    pub locals: Vec<Value>,
    pub frames: Vec<Frame>,
    pub mailbox: VecDeque<Value>,
    pub persistent: bool,
    pub cursor: usize,
    pub result: Option<Value>,
    pub awaiters: Vec<ProcessId>,
}

impl Process {
    pub fn new(id: ProcessId, persistent: bool) -> Self {
        Self {
            id,
            stack: Vec::new(),
            locals: Vec::new(),
            frames: Vec::new(),
            mailbox: VecDeque::new(),
            persistent,
            cursor: 0,
            result: None,
            awaiters: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Scheduler {
    processes: HashMap<ProcessId, Process>,
    queue: VecDeque<ProcessId>,
    waiting: HashSet<ProcessId>,
    active: Option<ProcessId>,
    // Program data owned by scheduler
    constants: Vec<Constant>,
    functions: Vec<Function>,
    builtins: Vec<String>,
    types: HashMap<TypeId, TupleTypeInfo>,
    // Heap for runtime-allocated binaries
    heap: Vec<Vec<u8>>,
}

impl TypeLookup for Scheduler {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.types.get(type_id)
    }
}

impl Scheduler {
    pub fn get_constant(&self, index: usize) -> Option<&Constant> {
        self.constants.get(index)
    }

    pub fn with_binary_bytes<F, R>(&self, binary: &Binary, f: F) -> Result<R, Error>
    where
        F: FnOnce(&[u8]) -> Result<R, Error>,
    {
        match binary {
            Binary::Constant(index) => {
                let constant = self
                    .get_constant(*index)
                    .ok_or(Error::ConstantUndefined(*index))?;
                match constant {
                    Constant::Binary(bytes) => f(bytes),
                    _ => Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    }),
                }
            }
            Binary::Heap(index) => {
                let bytes = self.heap.get(*index).ok_or_else(|| {
                    Error::InvalidArgument(format!("Heap binary index {} not found", index))
                })?;
                f(bytes)
            }
        }
    }

    /// Convenience method that clones the binary data
    /// For cases where the closure API would be cumbersome (e.g., multiple binary accesses)
    pub fn get_binary_bytes(&self, binary: &Binary) -> Result<Vec<u8>, Error> {
        self.with_binary_bytes(binary, |bytes| Ok(bytes.to_vec()))
    }

    pub fn allocate_binary(&mut self, bytes: Vec<u8>) -> Result<Binary, Error> {
        if bytes.len() > crate::vm::MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                bytes.len(),
                crate::vm::MAX_BINARY_SIZE
            )));
        }
        let index = self.heap.len();
        self.heap.push(bytes);
        Ok(Binary::Heap(index))
    }

    pub fn new(program: &Program) -> Self {
        Self {
            processes: HashMap::new(),
            queue: VecDeque::new(),
            waiting: HashSet::new(),
            active: None,
            // Clone Program's data
            constants: program.get_constants().clone(),
            functions: program.get_functions().clone(),
            builtins: program.get_builtins().clone(),
            types: program.get_types(),
            heap: Vec::new(),
        }
    }

    pub fn spawn_process(&mut self, id: ProcessId, persistent: bool) {
        let process = Process::new(id, persistent);
        self.processes.insert(id, process);
        self.queue.push_back(id);
    }

    pub fn get_process(&self, id: ProcessId) -> Option<&Process> {
        self.processes.get(&id)
    }

    pub fn get_process_mut(&mut self, id: ProcessId) -> Option<&mut Process> {
        self.processes.get_mut(&id)
    }

    pub fn get_current_process(&self) -> Option<&Process> {
        self.active.and_then(|id| self.processes.get(&id))
    }

    pub fn get_current_process_mut(&mut self) -> Option<&mut Process> {
        self.active.and_then(|id| self.processes.get_mut(&id))
    }

    pub fn suspend_process(&mut self, id: ProcessId) {
        self.queue.retain(|&pid| pid != id);
    }

    pub fn wake_process(&mut self, id: ProcessId) {
        if self.waiting.remove(&id) {
            self.queue.push_back(id);
        }
    }

    pub fn mark_waiting(&mut self, id: ProcessId) {
        self.waiting.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn terminate_process(&mut self, id: ProcessId) {
        self.processes.remove(&id);
        self.queue.retain(|&pid| pid != id);
        self.waiting.remove(&id);
    }

    pub fn schedule_next(&mut self) -> Option<ProcessId> {
        self.active = self.queue.pop_front();
        self.active
    }

    fn get_status(&self, id: ProcessId, process: &Process) -> ProcessStatus {
        if self.active == Some(id) {
            ProcessStatus::Running
        } else if self.queue.contains(&id) {
            ProcessStatus::Queued
        } else if self.waiting.contains(&id) {
            ProcessStatus::Waiting
        } else if process.persistent {
            ProcessStatus::Sleeping
        } else {
            ProcessStatus::Terminated
        }
    }

    pub fn get_process_statuses(&self) -> HashMap<ProcessId, ProcessStatus> {
        self.processes
            .iter()
            .map(|(id, process)| (*id, self.get_status(*id, process)))
            .collect()
    }

    pub fn get_process_info(&self, id: ProcessId) -> Option<ProcessInfo> {
        self.processes.get(&id).map(|process| ProcessInfo {
            id,
            status: self.get_status(id, process),
            stack_size: process.stack.len(),
            locals_size: process.locals.len(),
            frames_count: process.frames.len(),
            mailbox_size: process.mailbox.len(),
            persistent: process.persistent,
            result: process.result.clone(),
        })
    }

    // Program data accessors
    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    pub fn get_builtin(&self, index: usize) -> Option<&String> {
        self.builtins.get(index)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
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

pub struct SchedulerHandle {
    pub scheduler: Arc<Mutex<Scheduler>>,
    command_tx: SyncSender<SchedulerCommand>,
    pending: Arc<Mutex<HashMap<u64, SyncSender<SchedulerResponse>>>>,
    thread_handle: Option<JoinHandle<()>>,
    next_process_id: Arc<AtomicUsize>,
    next_request_id: Arc<AtomicU64>,
}

impl std::fmt::Debug for SchedulerHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SchedulerHandle")
            .field("scheduler", &self.scheduler)
            .field("command_tx", &self.command_tx)
            .field("pending", &self.pending)
            .field("thread_handle", &"<thread handle>")
            .field("next_process_id", &self.next_process_id)
            .field("next_request_id", &self.next_request_id)
            .finish()
    }
}

impl SchedulerHandle {
    pub fn new(program: Arc<RwLock<Program>>, next_process_id: Arc<AtomicUsize>) -> Self {
        // Initialize scheduler with current program state
        let program_snapshot = program.read().unwrap();
        let scheduler = Arc::new(Mutex::new(Scheduler::new(&*program_snapshot)));
        drop(program_snapshot);

        let (command_tx, command_rx) = sync_channel(100);
        let (response_tx, response_rx) = sync_channel(100);
        let response_rx = Arc::new(Mutex::new(response_rx));
        let pending: Arc<Mutex<HashMap<u64, SyncSender<SchedulerResponse>>>> =
            Arc::new(Mutex::new(HashMap::new()));
        let next_request_id = Arc::new(AtomicU64::new(0));

        let scheduler_clone = Arc::clone(&scheduler);
        let next_process_id_clone = Arc::clone(&next_process_id);
        let thread_handle = thread::spawn(move || {
            scheduler_thread(
                scheduler_clone,
                next_process_id_clone,
                command_rx,
                response_tx,
            );
        });

        // Spawn response handler thread to route responses to pending senders
        let pending_clone = Arc::clone(&pending);
        let response_rx_clone = Arc::clone(&response_rx);
        thread::spawn(move || {
            loop {
                let response = match response_rx_clone.lock().unwrap().recv() {
                    Ok(r) => r,
                    Err(_) => break, // Channel closed
                };

                // Route response by request_id
                let request_id = match &response {
                    SchedulerResponse::Execute { request_id, .. } => *request_id,
                    SchedulerResponse::SpawnProcess { request_id, .. } => *request_id,
                    SchedulerResponse::GetProcessStatuses { request_id, .. } => *request_id,
                    SchedulerResponse::GetProcessInfo { request_id, .. } => *request_id,
                    SchedulerResponse::GetVariables { request_id, .. } => *request_id,
                };

                if let Some(tx) = pending_clone.lock().unwrap().remove(&request_id) {
                    let _ = tx.send(response);
                }
            }
        });

        Self {
            scheduler,
            command_tx,
            pending,
            thread_handle: Some(thread_handle),
            next_process_id,
            next_request_id,
        }
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

    pub fn execute(
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

    pub fn spawn_process(&self, id: ProcessId, persistent: bool) -> Result<(), Error> {
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

    pub fn get_process_statuses(&self) -> Result<HashMap<ProcessId, ProcessStatus>, Error> {
        let response =
            self.send_request(|request_id| SchedulerCommand::GetProcessStatuses { request_id })?;

        match response {
            SchedulerResponse::GetProcessStatuses { result, .. } => result,
            _ => Err(Error::InvalidArgument(
                "Unexpected response type".to_string(),
            )),
        }
    }

    pub fn get_process_info(&self, id: ProcessId) -> Result<Option<ProcessInfo>, Error> {
        let response =
            self.send_request(|request_id| SchedulerCommand::GetProcessInfo { request_id, id })?;

        match response {
            SchedulerResponse::GetProcessInfo { result, .. } => result,
            _ => Err(Error::InvalidArgument(
                "Unexpected response type".to_string(),
            )),
        }
    }

    pub fn get_variables(
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

    pub fn sync_constant(&self, index: usize, constant: Constant) {
        let _ = self
            .command_tx
            .send(SchedulerCommand::RegisterConstant { index, constant });
    }

    pub fn sync_function(&self, index: usize, function: Function) {
        let _ = self
            .command_tx
            .send(SchedulerCommand::RegisterFunction { index, function });
    }

    pub fn sync_builtin(&self, index: usize, name: String) {
        let _ = self
            .command_tx
            .send(SchedulerCommand::RegisterBuiltin { index, name });
    }

    pub fn sync_type(&self, type_id: TypeId, info: TupleTypeInfo) {
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

impl Drop for SchedulerHandle {
    fn drop(&mut self) {
        self.shutdown();
    }
}

fn scheduler_thread(
    scheduler: Arc<Mutex<Scheduler>>,
    next_process_id: Arc<AtomicUsize>,
    command_rx: Receiver<SchedulerCommand>,
    response_tx: SyncSender<SchedulerResponse>,
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
                    &scheduler,
                    &next_process_id,
                    process_id,
                    instructions,
                    parameter,
                );

                // Compact locals if variables map was provided
                let result = match exec_result {
                    Ok(value) => {
                        let compacted_variables = if let Some(vars) = variables {
                            let mut sched = scheduler.lock().unwrap();
                            let process = match sched.get_process_mut(process_id) {
                                Some(p) => p,
                                None => {
                                    let _ = response_tx.send(SchedulerResponse::Execute {
                                        request_id,
                                        result: Ok((value, None)),
                                    });
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

                let _ = response_tx.send(SchedulerResponse::Execute { request_id, result });
            }
            Ok(SchedulerCommand::RegisterConstant { index, constant }) => {
                let mut sched = scheduler.lock().unwrap();
                // Ensure the vec is large enough
                if index >= sched.constants.len() {
                    sched.constants.resize(index + 1, Constant::Integer(0));
                }
                sched.constants[index] = constant;
            }
            Ok(SchedulerCommand::RegisterFunction { index, function }) => {
                let mut sched = scheduler.lock().unwrap();
                if index >= sched.functions.len() {
                    sched.functions.resize(
                        index + 1,
                        Function {
                            instructions: vec![],
                            function_type: None,
                            captures: vec![],
                        },
                    );
                }
                sched.functions[index] = function;
            }
            Ok(SchedulerCommand::RegisterBuiltin { index, name }) => {
                let mut sched = scheduler.lock().unwrap();
                if index >= sched.builtins.len() {
                    sched.builtins.resize(index + 1, String::new());
                }
                sched.builtins[index] = name;
            }
            Ok(SchedulerCommand::RegisterType { type_id, info }) => {
                let mut sched = scheduler.lock().unwrap();
                sched.types.insert(type_id, info);
            }
            Ok(SchedulerCommand::SpawnProcess {
                request_id,
                id,
                persistent,
            }) => {
                let mut sched = scheduler.lock().unwrap();
                sched.spawn_process(id, persistent);
                let _ = response_tx.send(SchedulerResponse::SpawnProcess {
                    request_id,
                    result: Ok(()),
                });
            }
            Ok(SchedulerCommand::GetProcessStatuses { request_id }) => {
                let sched = scheduler.lock().unwrap();
                let statuses = sched.get_process_statuses();
                let _ = response_tx.send(SchedulerResponse::GetProcessStatuses {
                    request_id,
                    result: Ok(statuses),
                });
            }
            Ok(SchedulerCommand::GetProcessInfo { request_id, id }) => {
                let sched = scheduler.lock().unwrap();
                let info = sched.get_process_info(id);
                let _ = response_tx.send(SchedulerResponse::GetProcessInfo {
                    request_id,
                    result: Ok(info),
                });
            }
            Ok(SchedulerCommand::GetVariables {
                request_id,
                process_id,
                mapping,
            }) => {
                let sched = scheduler.lock().unwrap();
                let result = match sched.get_process(process_id) {
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
                let _ = response_tx.send(SchedulerResponse::GetVariables { request_id, result });
            }
            Ok(SchedulerCommand::Shutdown) | Err(_) => {
                break;
            }
        }
    }
}

fn execute_in_scheduler(
    scheduler: &Arc<Mutex<Scheduler>>,
    next_process_id: &Arc<AtomicUsize>,
    process_id: ProcessId,
    instructions: Vec<Instruction>,
    parameter: Option<Value>,
) -> Result<Option<Value>, Error> {
    if instructions.is_empty() {
        return Ok(None);
    }

    {
        let mut sched = scheduler.lock().unwrap();
        sched.queue.retain(|&id| id != process_id);

        let process = sched
            .get_process_mut(process_id)
            .ok_or(Error::InvalidArgument(format!(
                "Process {:?} not found",
                process_id
            )))?;

        process.stack.push(parameter.unwrap_or_else(Value::nil));

        let frame = Frame::new(instructions.clone(), 0, 0);
        process.frames.push(frame);

        sched.active = Some(process_id);
    }

    let result = run(scheduler, next_process_id);

    // Pop frame
    {
        let mut sched = scheduler.lock().unwrap();
        if let Some(process) = sched.get_process_mut(process_id) {
            process.frames.pop();
        }
    }

    result
}

fn run(
    scheduler: &Arc<Mutex<Scheduler>>,
    next_process_id: &Arc<AtomicUsize>,
) -> Result<Option<Value>, Error> {
    let initial_process = scheduler.lock().unwrap().active;
    let mut initial_result = None;

    loop {
        let mut units_this_slice = 0;

        // Execute instructions for current process
        while let Some(instruction) = get_instruction(scheduler) {
            let result = match instruction {
                Instruction::Constant(index) => handle_constant(scheduler, index),
                Instruction::Pop => handle_pop(scheduler),
                Instruction::Duplicate => handle_duplicate(scheduler),
                Instruction::Over => handle_over(scheduler),
                Instruction::Swap => handle_swap(scheduler),
                Instruction::Load(index) => handle_load(scheduler, index),
                Instruction::Store(index) => handle_store(scheduler, index),
                Instruction::Tuple(type_id) => handle_tuple(scheduler, type_id),
                Instruction::Get(index) => handle_get(scheduler, index),
                Instruction::IsInteger => handle_is_integer(scheduler),
                Instruction::IsBinary => handle_is_binary(scheduler),
                Instruction::IsTuple(type_id) => handle_is_tuple(scheduler, type_id),
                Instruction::Jump(offset) => Ok(handle_jump(scheduler, offset)),
                Instruction::JumpIf(offset) => handle_jump_if(scheduler, offset),
                Instruction::Call => handle_call(scheduler),
                Instruction::TailCall(recurse) => handle_tail_call(scheduler, recurse),
                Instruction::Return => handle_return(scheduler),
                Instruction::Function(function_index) => handle_function(scheduler, function_index),
                Instruction::Clear(count) => handle_clear(scheduler, count),
                Instruction::Allocate(count) => handle_allocate(scheduler, count),
                Instruction::Builtin(index) => handle_builtin(scheduler, index),
                Instruction::Equal(count) => handle_equal(scheduler, count),
                Instruction::Not => handle_not(scheduler),
                Instruction::Spawn => handle_spawn(scheduler, &next_process_id),
                Instruction::Send => handle_send(scheduler),
                Instruction::Self_ => handle_self(scheduler),
                Instruction::Receive => handle_receive(scheduler),
                Instruction::Acknowledge => handle_acknowledge(scheduler),
            };
            result?;

            // Don't increment counter for Call/TailCall (they manage their own frames)
            // or Receive/Call when process was marked waiting (so it re-executes when woken)
            let should_increment =
                !matches!(instruction, Instruction::Call | Instruction::TailCall(_));
            if should_increment {
                let mut sched = scheduler.lock().unwrap();
                let current_pid = sched.active;

                // If process was marked waiting by Receive, don't increment and break out
                let was_marked_waiting = if matches!(instruction, Instruction::Receive) {
                    current_pid.map_or(false, |pid| sched.waiting.contains(&pid))
                } else {
                    false
                };

                if was_marked_waiting {
                    // Break out of instruction loop so scheduler can switch processes
                    break;
                } else if let Some(process) = sched.get_current_process_mut() {
                    if let Some(frame) = process.frames.last_mut() {
                        frame.counter += 1;
                    }
                }
            } else {
                // For Call instruction, check if process was marked waiting (awaiting a process)
                if matches!(instruction, Instruction::Call) {
                    let sched = scheduler.lock().unwrap();
                    let current_pid = sched.active;

                    let was_marked_waiting =
                        current_pid.map_or(false, |pid| sched.waiting.contains(&pid));

                    if was_marked_waiting {
                        // Break out of instruction loop so scheduler can switch processes
                        break;
                    }
                }
            }

            // Track instruction units consumed
            units_this_slice += 1;

            // Yield to other processes if time slice exhausted
            if units_this_slice >= TIME_SLICE_UNITS {
                let mut sched = scheduler.lock().unwrap();
                if !sched.queue.is_empty() {
                    if let Some(pid) = sched.active {
                        sched.queue.push_back(pid);
                    }
                    break;
                }
            }
        }

        // Current process finished its instructions (or is waiting)
        let mut sched = scheduler.lock().unwrap();
        let current_pid = sched.active;

        // Pop initial result if this is the initial process and we haven't done so yet
        // Only do this if the process is not waiting (it's actually done, not blocked)
        if current_pid == initial_process && initial_result.is_none() {
            let is_waiting = current_pid.map_or(false, |pid| sched.waiting.contains(&pid));
            if !is_waiting {
                initial_result = sched.get_current_process_mut().and_then(|p| p.stack.pop());
            }
        }

        // Store result and wake awaiters when process finishes (frames are empty)
        if let Some(pid) = current_pid {
            let should_store_result = sched
                .get_process(pid)
                .map(|p| p.frames.is_empty() && p.result.is_none())
                .unwrap_or(false);

            if should_store_result {
                if let Some(process) = sched.get_process_mut(pid) {
                    process.result = process.stack.last().cloned();
                    let awaiters = std::mem::take(&mut process.awaiters);
                    for waiter in awaiters {
                        sched.wake_process(waiter);
                    }
                }
            }
        }

        // Schedule next process from run queue
        if sched.schedule_next().is_none() {
            break;
        }
    }

    // Clear active when scheduler is idle
    scheduler.lock().unwrap().active = None;

    Ok(initial_result)
}

fn get_instruction(scheduler: &Arc<Mutex<Scheduler>>) -> Option<Instruction> {
    let sched = scheduler.lock().unwrap();
    let process = sched.get_current_process()?;
    let frame = process.frames.last()?;
    frame.instructions.get(frame.counter).cloned()
}

// Placeholder implementations for all handle_* functions
// These will be moved from vm.rs

fn handle_constant(scheduler: &Arc<Mutex<Scheduler>>, index: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let constant = sched
        .get_constant(index)
        .ok_or(Error::ConstantUndefined(index))?;
    let value = match constant {
        Constant::Integer(integer) => Value::Integer(*integer),
        Constant::Binary(_) => Value::Binary(Binary::Constant(index)),
    };

    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;
    process.stack.push(value);
    Ok(())
}

fn handle_pop(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;
    process.stack.pop().ok_or(Error::StackUnderflow)?;
    Ok(())
}

fn handle_duplicate(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;
    let value = process.stack.last().ok_or(Error::StackUnderflow)?.clone();
    process.stack.push(value);
    Ok(())
}

fn handle_over(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    if process.stack.len() < 2 {
        return Err(Error::StackUnderflow);
    }
    let index = process.stack.len() - 2;
    let value = process.stack[index].clone();
    process.stack.push(value);
    Ok(())
}

fn handle_swap(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let len = process.stack.len();
    if len < 2 {
        return Err(Error::StackUnderflow);
    }
    process.stack.swap(len - 1, len - 2);
    Ok(())
}

fn handle_load(scheduler: &Arc<Mutex<Scheduler>>, index: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
    let actual_index = frame.locals_base + index;

    let value = process
        .locals
        .get(actual_index)
        .ok_or(Error::VariableUndefined(format!("local[{}]", index)))?
        .clone();

    process.stack.push(value);
    Ok(())
}

fn handle_store(scheduler: &Arc<Mutex<Scheduler>>, index: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
    let actual_index = frame.locals_base + index;

    let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    if actual_index >= process.locals.len() {
        return Err(Error::VariableUndefined(format!("local[{}]", index)));
    }

    process.locals[actual_index] = value;
    Ok(())
}

fn handle_tuple(scheduler: &Arc<Mutex<Scheduler>>, type_id: TypeId) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let (_, fields) = sched
        .lookup_type(&type_id)
        .ok_or_else(|| Error::TypeMismatch {
            expected: "known tuple type".to_string(),
            found: format!("unknown TypeId({:?})", type_id),
        })?;
    let size = fields.len();

    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let mut values = Vec::new();
    for _ in 0..size {
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        values.push(value);
    }
    values.reverse();
    process.stack.push(Value::Tuple(type_id, values));
    Ok(())
}

fn handle_get(scheduler: &Arc<Mutex<Scheduler>>, index: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    match value {
        Value::Tuple(_, elements) => {
            let element = elements
                .get(index)
                .ok_or(Error::FieldAccessInvalid(index))?
                .clone();
            process.stack.push(element);
            Ok(())
        }
        _ => Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        }),
    }
}

fn handle_is_integer(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    let result = if matches!(value, Value::Integer(_)) {
        Value::ok()
    } else {
        Value::nil()
    };

    process.stack.push(result);
    Ok(())
}

fn handle_is_binary(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    let result = if matches!(value, Value::Binary(_)) {
        Value::ok()
    } else {
        Value::nil()
    };

    process.stack.push(result);
    Ok(())
}

fn handle_is_tuple(scheduler: &Arc<Mutex<Scheduler>>, type_id: TypeId) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    let is_match = if let Value::Tuple(actual_type_id, _) = &value {
        if actual_type_id == &type_id {
            // Fast path: exact match
            true
        } else {
            // Use Type::is_compatible for structural checking
            let actual_type = Type::Tuple(*actual_type_id);
            let expected_type = Type::Tuple(type_id);
            actual_type.is_compatible(&expected_type, &*sched)
        }
    } else {
        false
    };

    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    process
        .stack
        .push(if is_match { Value::ok() } else { Value::nil() });
    Ok(())
}

fn handle_jump(scheduler: &Arc<Mutex<Scheduler>>, offset: isize) {
    let mut sched = scheduler.lock().unwrap();
    if let Some(process) = sched.get_current_process_mut() {
        if let Some(frame) = process.frames.last_mut() {
            frame.counter = frame.counter.wrapping_add_signed(offset);
        }
    }
}

fn handle_jump_if(scheduler: &Arc<Mutex<Scheduler>>, offset: isize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let condition = process.stack.pop().ok_or(Error::StackUnderflow)?;

    let should_jump = match &condition {
        Value::Tuple(type_id, fields) => !(type_id == &TypeId::NIL && fields.is_empty()),
        _ => true,
    };

    if should_jump {
        if let Some(frame) = process.frames.last_mut() {
            frame.counter = frame.counter.wrapping_add_signed(offset);
        }
    }

    Ok(())
}

fn handle_call(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let function_value = process.stack.last().ok_or(Error::StackUnderflow)?.clone();

    match function_value {
        Value::Function(function_index, captures) => {
            // Pop function and parameter now that we know we won't block
            process.stack.pop();
            let parameter = process.stack.pop().ok_or(Error::StackUnderflow)?;

            let func = sched
                .get_function(function_index)
                .ok_or(Error::FunctionUndefined(function_index))?;
            let instructions = func.instructions.clone();
            drop(sched);

            let mut sched = scheduler.lock().unwrap();
            let process = sched
                .get_current_process_mut()
                .ok_or(Error::InvalidArgument("No current process".to_string()))?;

            let locals_base = process.locals.len();
            let captures_count = captures.len();
            process.stack.push(parameter);
            process.locals.extend(captures);

            process
                .frames
                .push(Frame::new(instructions, locals_base, captures_count));

            Ok(())
        }
        Value::Builtin(name) => {
            // Pop function and parameter now that we know we won't block
            process.stack.pop();
            let parameter = process.stack.pop().ok_or(Error::StackUnderflow)?;

            let builtin = BUILTIN_REGISTRY
                .get_implementation(&name)
                .ok_or_else(|| Error::InvalidArgument(format!("Unrecognised builtin: {}", name)))?;

            let result = builtin(&parameter, &mut *sched)?;

            let process = sched
                .get_current_process_mut()
                .ok_or(Error::InvalidArgument("No current process".to_string()))?;

            process.stack.push(result);

            // Unlike regular calls, builtins don't create a new frame
            // So we need to manually increment the counter
            if let Some(frame) = process.frames.last_mut() {
                frame.counter += 1;
            }

            Ok(())
        }
        Value::Pid(target_pid) => {
            let current_pid = sched
                .active
                .ok_or(Error::InvalidArgument("No current process".to_string()))?;

            let target = sched
                .get_process(target_pid)
                .ok_or(Error::InvalidArgument(format!(
                    "Process {:?} not found",
                    target_pid
                )))?;

            if let Some(result) = &target.result {
                // Process has finished - pop values and return result
                let result = result.clone();

                let process = sched
                    .get_current_process_mut()
                    .ok_or(Error::InvalidArgument("No current process".to_string()))?;

                process.stack.pop(); // Pop pid
                process.stack.pop(); // Pop parameter (ignored)
                process.stack.push(result);

                // Increment counter to continue
                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }

                Ok(())
            } else {
                // Process still running - block until it finishes without popping
                let target = sched
                    .get_process_mut(target_pid)
                    .ok_or(Error::InvalidArgument(format!(
                        "Process {:?} not found",
                        target_pid
                    )))?;

                target.awaiters.push(current_pid);
                drop(sched);

                let mut sched = scheduler.lock().unwrap();
                sched.mark_waiting(current_pid);

                // Don't increment counter - will retry when woken
                Ok(())
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "function".to_string(),
            found: function_value.type_name().to_string(),
        }),
    }
}

fn handle_tail_call(scheduler: &Arc<Mutex<Scheduler>>, recurse: bool) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    if recurse {
        let argument = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
        let locals_base = frame.locals_base;
        let captures_count = frame.captures_count;
        let instructions = frame.instructions.clone();

        // Clear current frame's locals, but keep captures
        process.locals.truncate(locals_base + captures_count);

        process.stack.push(argument);
        *process.frames.last_mut().unwrap() = Frame::new(instructions, locals_base, captures_count);
        Ok(())
    } else {
        let function_value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let argument = process.stack.pop().ok_or(Error::StackUnderflow)?;

        match function_value {
            Value::Function(function, captures) => {
                let func = sched
                    .get_function(function)
                    .ok_or(Error::FunctionUndefined(function))?;
                let instructions = func.instructions.clone();
                drop(sched);

                let mut sched = scheduler.lock().unwrap();
                let process = sched
                    .get_current_process_mut()
                    .ok_or(Error::InvalidArgument("No current process".to_string()))?;

                let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
                let locals_base = frame.locals_base;

                // Clear current frame's locals
                process.locals.truncate(locals_base);

                // Extend with captures for new function
                let captures_count = captures.len();
                process.locals.extend(captures);

                process.stack.push(argument);
                *process.frames.last_mut().unwrap() =
                    Frame::new(instructions, locals_base, captures_count);
                Ok(())
            }
            _ => Err(Error::CallInvalid),
        }
    }
}

fn handle_return(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let return_value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    let frame = process.frames.pop().ok_or(Error::FrameUnderflow)?;

    let locals_to_clear = process.locals.len() - frame.locals_base - frame.captures_count;

    if locals_to_clear > 0 {
        let new_len = process.locals.len() - locals_to_clear;
        process.locals.truncate(new_len);
    }

    process.stack.push(return_value);

    Ok(())
}

fn handle_function(scheduler: &Arc<Mutex<Scheduler>>, function_index: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let func = sched
        .get_function(function_index)
        .ok_or(Error::FunctionUndefined(function_index))?;
    let capture_locals = func.captures.clone();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
    let locals_base = frame.locals_base;

    // Collect captured values from current frame's locals using the function's capture list
    let captures = capture_locals
        .iter()
        .map(|&index| {
            process
                .locals
                .get(locals_base + index)
                .cloned()
                .ok_or(Error::VariableUndefined(format!("capture local {}", index)))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let function_value = Value::Function(function_index, captures);
    process.stack.push(function_value);

    Ok(())
}

fn handle_clear(scheduler: &Arc<Mutex<Scheduler>>, count: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    if count > process.locals.len() {
        return Err(Error::StackUnderflow);
    }
    process.locals.truncate(process.locals.len() - count);
    Ok(())
}

fn handle_allocate(scheduler: &Arc<Mutex<Scheduler>>, count: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    process
        .locals
        .extend(std::iter::repeat(Value::nil()).take(count));
    Ok(())
}

fn handle_builtin(scheduler: &Arc<Mutex<Scheduler>>, index: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let builtin_name = sched
        .get_builtin(index)
        .ok_or(Error::BuiltinUndefined(index))?
        .clone();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    process.stack.push(Value::Builtin(builtin_name));
    Ok(())
}

fn handle_equal(scheduler: &Arc<Mutex<Scheduler>>, count: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let values = {
        if count > process.stack.len() {
            return Err(Error::StackUnderflow);
        }

        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(process.stack.pop().ok_or(Error::StackUnderflow)?);
        }
        values.reverse();
        values
    };

    let first = &values[0];
    let all_equal = values
        .iter()
        .all(|value| values_equal(&sched, first, value));

    let result = if all_equal {
        first.clone()
    } else {
        Value::nil()
    };

    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    process.stack.push(result);
    Ok(())
}

fn handle_not(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    let result = match &value {
        Value::Tuple(type_id, fields) => {
            if type_id == &TypeId::NIL && fields.is_empty() {
                Value::ok()
            } else {
                Value::nil()
            }
        }
        _ => Value::nil(),
    };

    process.stack.push(result);
    Ok(())
}

fn handle_spawn(
    scheduler: &Arc<Mutex<Scheduler>>,
    next_process_id: &Arc<AtomicUsize>,
) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let function_value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    let (function_index, captures) = match function_value {
        Value::Function(idx, caps) => (idx, caps),
        _ => {
            return Err(Error::TypeMismatch {
                expected: "function".to_string(),
                found: function_value.type_name().to_string(),
            });
        }
    };

    let func = sched
        .get_function(function_index)
        .ok_or(Error::FunctionUndefined(function_index))?;
    let instructions = func.instructions.clone();
    drop(sched);

    let new_pid = ProcessId(next_process_id.fetch_add(1, Ordering::SeqCst));

    let mut sched = scheduler.lock().unwrap();
    sched.spawn_process(new_pid, false);

    let new_process = sched
        .get_process_mut(new_pid)
        .ok_or(Error::InvalidArgument(
            "Failed to create process".to_string(),
        ))?;

    let captures_count = captures.len();
    new_process.stack.push(Value::nil());
    new_process.locals.extend(captures);
    new_process
        .frames
        .push(Frame::new(instructions, 0, captures_count));

    let calling_process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    calling_process.stack.push(Value::Pid(new_pid));

    Ok(())
}

fn handle_send(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let pid_value = process.stack.pop().ok_or(Error::StackUnderflow)?;
    let message = process.stack.pop().ok_or(Error::StackUnderflow)?;

    let target_pid = match pid_value {
        Value::Pid(pid) => pid,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "pid".to_string(),
                found: pid_value.type_name().to_string(),
            });
        }
    };

    let target_process = sched
        .get_process_mut(target_pid)
        .ok_or(Error::InvalidArgument(format!(
            "Process {:?} not found",
            target_pid
        )))?;

    target_process.mailbox.push_back(message);

    drop(sched);

    let mut sched = scheduler.lock().unwrap();
    sched.wake_process(target_pid);

    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    process.stack.push(Value::ok());
    Ok(())
}

fn handle_self(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let current_pid = sched
        .active
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    process.stack.push(Value::Pid(current_pid));
    Ok(())
}

fn handle_receive(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let current_pid = sched
        .active
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let process = sched
        .get_process_mut(current_pid)
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let cursor = process.cursor;
    if cursor < process.mailbox.len() {
        let message = process.mailbox[cursor].clone();
        process.cursor = cursor + 1;
        process.stack.push(message);
    } else {
        drop(sched);
        let mut sched = scheduler.lock().unwrap();
        sched.mark_waiting(current_pid);
    }

    Ok(())
}

fn handle_acknowledge(scheduler: &Arc<Mutex<Scheduler>>) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let cursor = process.cursor;
    if 0 < cursor && cursor - 1 < process.mailbox.len() {
        process.mailbox.remove(cursor - 1);
    }
    process.cursor = 0;
    Ok(())
}

fn values_equal(scheduler: &Scheduler, a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Binary(a), Value::Binary(b)) => {
            match (scheduler.get_binary_bytes(a), scheduler.get_binary_bytes(b)) {
                (Ok(bytes_a), Ok(bytes_b)) => bytes_a == bytes_b,
                _ => false,
            }
        }
        (Value::Tuple(type_a, elements_a), Value::Tuple(type_b, elements_b)) => {
            type_a == type_b
                && elements_a.len() == elements_b.len()
                && elements_a
                    .iter()
                    .zip(elements_b.iter())
                    .all(|(a, b)| values_equal(scheduler, a, b))
        }
        (Value::Function(idx_a, caps_a), Value::Function(idx_b, caps_b)) => {
            idx_a == idx_b
                && caps_a.len() == caps_b.len()
                && caps_a
                    .iter()
                    .zip(caps_b.iter())
                    .all(|(a, b)| values_equal(scheduler, a, b))
        }
        (Value::Builtin(a), Value::Builtin(b)) => a == b,
        (Value::Pid(a), Value::Pid(b)) => a == b,
        _ => false,
    }
}
