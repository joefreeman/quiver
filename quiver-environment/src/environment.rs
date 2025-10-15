use crate::runtime::{Event, Runtime, SchedulerCommand};
use quiver_core::bytecode::Instruction;
use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::TupleTypeInfo;
use quiver_core::value::Value;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

enum PendingCallback {
    Execute(Box<dyn FnOnce(Result<ProcessId, Error>)>, usize), // callback, executor_id
    Wake(Box<dyn FnOnce(Result<(), Error>)>),
    GetResult(Box<dyn FnOnce(Result<(Value, Vec<Vec<u8>>), Error>)>),
    GetProcessStatuses(Box<dyn FnOnce(Result<HashMap<ProcessId, ProcessStatus>, Error>)>),
    GetProcessInfo(Box<dyn FnOnce(Result<Option<ProcessInfo>, Error>)>),
    GetLocals(Box<dyn FnOnce(Result<Vec<Value>, Error>)>),
    CompactLocals(Box<dyn FnOnce(Result<(), Error>)>),
}

/// Environment manages program state, process registry, and coordinates
/// execution across multiple executors in a platform-agnostic way.
pub struct Environment<R: Runtime> {
    runtime: R,
    program: Program,
    executor_count: usize,
    process_registry: HashMap<ProcessId, usize>,
    next_executor: usize,
    next_request_id: u64,
    pending_callbacks: HashMap<u64, PendingCallback>,
    next_process_id: usize,
    awaiting_map: HashMap<ProcessId, Vec<ProcessId>>,
    awaiting_result_requests: HashMap<u64, ProcessId>,
    event_queue: Arc<Mutex<VecDeque<Event>>>,
}

impl<R: Runtime> Environment<R> {
    pub fn new(mut runtime: R, program: Program, executor_count: usize) -> Self {
        let event_queue = Arc::new(Mutex::new(VecDeque::new()));

        // Start executors with callbacks that push to the event queue
        for _ in 0..executor_count {
            let queue_clone = event_queue.clone();
            let callback = Box::new(move |event: Event| {
                queue_clone.lock().unwrap().push_back(event);
            });
            runtime
                .start_executor(&program, callback)
                .expect("Failed to start executor");
        }

        Self {
            runtime,
            program,
            executor_count,
            process_registry: HashMap::new(),
            next_executor: 0,
            next_request_id: 0,
            pending_callbacks: HashMap::new(),
            next_process_id: 0,
            awaiting_map: HashMap::new(),
            awaiting_result_requests: HashMap::new(),
            event_queue,
        }
    }

    /// Get a mutable reference to the runtime for platform-specific operations
    pub fn runtime_mut(&mut self) -> &mut R {
        &mut self.runtime
    }

    /// Create a temporary Executor for read-only operations like formatting.
    pub fn executor(&self) -> Executor {
        Executor::new(&self.program)
    }

    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn program_mut(&mut self) -> &mut Program {
        &mut self.program
    }

    /// Update the Program with a new one, syncing new items to the executors.
    /// Assumes that items are only appended, never modified.
    pub fn update_program(&mut self, new_program: Program) {
        let old_constants_len = self.program.get_constants().len();
        let old_functions_len = self.program.get_functions().len();
        let old_builtins_len = self.program.get_builtins().len();
        let old_types = self.program.get_types();

        let new_constants = new_program.get_constants()[old_constants_len..].to_vec();
        let new_functions = new_program.get_functions()[old_functions_len..].to_vec();
        let new_builtins = new_program.get_builtins()[old_builtins_len..].to_vec();

        let new_types: Vec<(quiver_core::bytecode::TypeId, TupleTypeInfo)> = new_program
            .get_types()
            .iter()
            .filter(|(type_id, _)| !old_types.contains_key(type_id))
            .map(|(type_id, info)| (*type_id, info.clone()))
            .collect();

        // Update program first
        self.program = new_program;

        // Send single UpdateProgram command to all executors
        if !new_constants.is_empty()
            || !new_functions.is_empty()
            || !new_builtins.is_empty()
            || !new_types.is_empty()
        {
            for executor_id in 0..self.executor_count {
                let _ = self.runtime.send_command(
                    executor_id,
                    SchedulerCommand::UpdateProgram {
                        constants: new_constants.clone(),
                        functions: new_functions.clone(),
                        builtins: new_builtins.clone(),
                        types: new_types.clone(),
                    },
                );
            }
        }
    }

    /// Execute instructions in a new process.
    pub fn execute<F>(&mut self, instructions: Vec<Instruction>, persistent: bool, callback: F)
    where
        F: FnOnce(Result<ProcessId, Error>) + 'static,
    {
        let new_pid = ProcessId(self.next_process_id);
        self.next_process_id += 1;

        let executor_id = self.next_executor % self.executor_count;
        self.next_executor += 1;

        self.process_registry.insert(new_pid, executor_id);

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::Execute {
            request_id,
            process_id: new_pid,
            instructions,
            persistent,
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        self.pending_callbacks.insert(
            request_id,
            PendingCallback::Execute(Box::new(callback), executor_id),
        );
    }

    /// Wake an existing persistent process with new instructions.
    pub fn wake<F>(&mut self, process_id: ProcessId, instructions: Vec<Instruction>, callback: F)
    where
        F: FnOnce(Result<(), Error>) + 'static,
    {
        let executor_id = match self.process_registry.get(&process_id) {
            Some(&id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::Wake {
            request_id,
            process_id,
            instructions,
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        self.pending_callbacks
            .insert(request_id, PendingCallback::Wake(Box::new(callback)));
    }

    /// Get the result of a process execution.
    pub fn get_result<F>(&mut self, process_id: ProcessId, callback: F)
    where
        F: FnOnce(Result<(Value, Vec<Vec<u8>>), Error>) + 'static,
    {
        let executor_id = match self.process_registry.get(&process_id) {
            Some(&id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::GetResult {
            request_id,
            process_id,
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        self.pending_callbacks
            .insert(request_id, PendingCallback::GetResult(Box::new(callback)));
    }

    /// Get the number of executors in this runtime.
    pub fn executor_count(&self) -> usize {
        self.executor_count
    }

    /// Get the status of all processes on a specific executor.
    pub fn get_process_statuses<F>(&mut self, executor_id: usize, callback: F)
    where
        F: FnOnce(Result<HashMap<ProcessId, ProcessStatus>, Error>) + 'static,
    {
        if executor_id >= self.executor_count {
            callback(Err(Error::InvalidArgument(format!(
                "Invalid executor_id: {}",
                executor_id
            ))));
            return;
        }

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::GetProcesses { request_id };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        self.pending_callbacks.insert(
            request_id,
            PendingCallback::GetProcessStatuses(Box::new(callback)),
        );
    }

    /// Get information about a specific process.
    pub fn get_process_info<F>(&mut self, process_id: ProcessId, callback: F)
    where
        F: FnOnce(Result<Option<ProcessInfo>, Error>) + 'static,
    {
        let executor_id = match self.process_registry.get(&process_id) {
            Some(&id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::GetProcess {
            request_id,
            id: process_id,
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        self.pending_callbacks.insert(
            request_id,
            PendingCallback::GetProcessInfo(Box::new(callback)),
        );
    }

    /// Get local variables from a process.
    pub fn get_locals<F>(&mut self, process_id: ProcessId, indices: &[usize], callback: F)
    where
        F: FnOnce(Result<Vec<Value>, Error>) + 'static,
    {
        let executor_id = match self.process_registry.get(&process_id) {
            Some(&id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::GetLocals {
            request_id,
            process_id,
            indices: indices.to_vec(),
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        self.pending_callbacks
            .insert(request_id, PendingCallback::GetLocals(Box::new(callback)));
    }

    /// Compact locals in a persistent process to keep only referenced variables.
    pub fn compact_locals<F>(
        &mut self,
        process_id: ProcessId,
        referenced_indices: &[usize],
        callback: F,
    ) where
        F: FnOnce(Result<(), Error>) + 'static,
    {
        let executor_id = match self.process_registry.get(&process_id) {
            Some(&id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::CompactLocals {
            request_id,
            process_id,
            referenced_indices: referenced_indices.to_vec(),
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        self.pending_callbacks.insert(
            request_id,
            PendingCallback::CompactLocals(Box::new(callback)),
        );
    }

    /// Check if there are pending callbacks waiting for responses.
    pub fn has_pending(&self) -> bool {
        !self.pending_callbacks.is_empty()
    }

    /// Process all pending events from the event queue.
    /// Returns the number of events processed.
    pub fn process_pending_events(&mut self) -> usize {
        let mut count = 0;

        loop {
            let event = self.event_queue.lock().unwrap().pop_front();
            match event {
                Some(ev) => {
                    self.process_event(ev);
                    count += 1;
                }
                None => break,
            }
        }

        count
    }

    fn process_event(&mut self, event: Event) {
        match event {
            Event::SpawnRequested {
                caller,
                function_index,
                captures,
                heap_data,
            } => {
                self.handle_spawn_request(caller, function_index, captures, heap_data);
            }
            Event::DeliverMessage {
                target,
                value,
                heap_data,
            } => {
                self.handle_deliver_message(target, value, heap_data);
            }
            Event::AwaitResult { target, caller } => {
                self.handle_await_result(target, caller);
            }
            Event::ProcessCompleted {
                process_id,
                result,
                heap_data,
            } => {
                self.handle_process_completed(process_id, result, heap_data);
            }
            _ => {
                let request_id = match &event {
                    Event::ExecuteResponse { request_id, .. } => *request_id,
                    Event::WakeResponse { request_id, .. } => *request_id,
                    Event::GetProcessesResponse { request_id, .. } => *request_id,
                    Event::GetProcessResponse { request_id, .. } => *request_id,
                    Event::GetLocalsResponse { request_id, .. } => *request_id,
                    Event::GetResultResponse { request_id, .. } => *request_id,
                    Event::CompactLocalsResponse { request_id, .. } => *request_id,
                    _ => return,
                };

                if let Some(awaiter_pid) = self.awaiting_result_requests.remove(&request_id) {
                    if let Event::GetResultResponse {
                        result, heap_data, ..
                    } = event
                    {
                        match result {
                            Ok(value) => {
                                if let Some(&awaiter_executor_id) =
                                    self.process_registry.get(&awaiter_pid)
                                {
                                    let _ = self.runtime.send_command(
                                        awaiter_executor_id,
                                        SchedulerCommand::NotifyResult {
                                            process_id: awaiter_pid,
                                            result: value,
                                            heap_data,
                                        },
                                    );
                                }
                                let mut target_to_remove = None;
                                for (target_pid, awaiters) in self.awaiting_map.iter_mut() {
                                    if let Some(pos) =
                                        awaiters.iter().position(|&pid| pid == awaiter_pid)
                                    {
                                        awaiters.remove(pos);
                                        if awaiters.is_empty() {
                                            target_to_remove = Some(*target_pid);
                                        }
                                        break;
                                    }
                                }
                                if let Some(target_pid) = target_to_remove {
                                    self.awaiting_map.remove(&target_pid);
                                }
                            }
                            Err(_) => {}
                        }
                    }
                    return;
                }

                if let Some(pending) = self.pending_callbacks.remove(&request_id) {
                    match (pending, event) {
                        (
                            PendingCallback::Execute(callback, _executor_id),
                            Event::ExecuteResponse { result, .. },
                        ) => {
                            callback(result);
                        }
                        (PendingCallback::Wake(callback), Event::WakeResponse { result, .. }) => {
                            callback(result);
                        }
                        (
                            PendingCallback::GetResult(callback),
                            Event::GetResultResponse {
                                result, heap_data, ..
                            },
                        ) => {
                            let result_with_heap = result.map(|value| (value, heap_data));
                            callback(result_with_heap);
                        }
                        (
                            PendingCallback::GetProcessStatuses(callback),
                            Event::GetProcessesResponse { result, .. },
                        ) => {
                            callback(result);
                        }
                        (
                            PendingCallback::GetProcessInfo(callback),
                            Event::GetProcessResponse { result, .. },
                        ) => {
                            callback(result);
                        }
                        (
                            PendingCallback::GetLocals(callback),
                            Event::GetLocalsResponse { result, .. },
                        ) => {
                            callback(result);
                        }
                        (
                            PendingCallback::CompactLocals(callback),
                            Event::CompactLocalsResponse { result, .. },
                        ) => {
                            callback(result);
                        }
                        _ => {
                            eprintln!(
                                "Warning: Mismatched response/callback types for request {}",
                                request_id
                            );
                        }
                    }
                }
            }
        }
    }

    fn handle_spawn_request(
        &mut self,
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
    ) {
        let new_pid = ProcessId(self.next_process_id);
        self.next_process_id += 1;

        let executor_id = self.next_executor % self.executor_count;
        self.next_executor += 1;

        self.process_registry.insert(new_pid, executor_id);

        let _ = self.runtime.send_command(
            executor_id,
            SchedulerCommand::Spawn {
                process_id: new_pid,
                function_index,
                captures,
                heap_data,
            },
        );

        if let Some(&caller_executor_id) = self.process_registry.get(&caller) {
            let _ = self.runtime.send_command(
                caller_executor_id,
                SchedulerCommand::NotifySpawn {
                    process_id: caller,
                    pid_value: Value::Pid(new_pid),
                    heap_data: Vec::new(),
                },
            );
        }
    }

    fn handle_deliver_message(&mut self, target: ProcessId, value: Value, heap_data: Vec<Vec<u8>>) {
        if let Some(&executor_id) = self.process_registry.get(&target) {
            let _ = self.runtime.send_command(
                executor_id,
                SchedulerCommand::NotifyMessage {
                    process_id: target,
                    message: value,
                    heap_data,
                },
            );
        }
    }

    fn handle_await_result(&mut self, target: ProcessId, caller: ProcessId) {
        if let Some(&target_executor_id) = self.process_registry.get(&target) {
            let request_id = self.next_request_id;
            self.next_request_id += 1;

            let command = SchedulerCommand::GetResult {
                request_id,
                process_id: target,
            };

            if self
                .runtime
                .send_command(target_executor_id, command)
                .is_ok()
            {
                self.awaiting_result_requests.insert(request_id, caller);

                let awaiting_map_entry = self.awaiting_map.entry(target).or_insert_with(Vec::new);
                if !awaiting_map_entry.contains(&caller) {
                    awaiting_map_entry.push(caller);
                }
            }
        }
    }

    fn handle_process_completed(
        &mut self,
        process_id: ProcessId,
        result: Value,
        heap_data: Vec<Vec<u8>>,
    ) {
        if let Some(awaiters) = self.awaiting_map.remove(&process_id) {
            for awaiter_pid in awaiters {
                if let Some(&executor_id) = self.process_registry.get(&awaiter_pid) {
                    let _ = self.runtime.send_command(
                        executor_id,
                        SchedulerCommand::NotifyResult {
                            process_id: awaiter_pid,
                            result: result.clone(),
                            heap_data: heap_data.clone(),
                        },
                    );
                }
            }
        }
    }

    pub fn shutdown(&mut self) {
        for executor_id in 0..self.executor_count {
            let _ = self.runtime.stop_executor(executor_id);
        }
    }
}
