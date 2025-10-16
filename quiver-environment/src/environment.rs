use crate::runtime::{Event, Runtime, SchedulerCommand};
use quiver_core::bytecode::Instruction;
use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::TupleTypeInfo;
use quiver_core::value::Value;
use std::collections::HashMap;

type PendingCallback = Box<dyn FnOnce(Event)>;

/// Manages process ID allocation and process-to-executor mapping.
struct ProcessRegistry {
    processes: HashMap<ProcessId, usize>,
    next_process_id: usize,
    next_executor: usize,
    executor_count: usize,
}

impl ProcessRegistry {
    fn new(executor_count: usize) -> Self {
        Self {
            processes: HashMap::new(),
            next_process_id: 0,
            next_executor: 0,
            executor_count,
        }
    }

    fn allocate_process(&mut self) -> ProcessId {
        let pid = ProcessId(self.next_process_id);
        self.next_process_id += 1;

        let executor_id = self.next_executor % self.executor_count;
        self.next_executor += 1;

        self.processes.insert(pid, executor_id);
        pid
    }

    fn get_executor(&self, process_id: ProcessId) -> Option<usize> {
        self.processes.get(&process_id).copied()
    }
}

/// Manages request ID generation and pending callback tracking.
struct RequestTracker {
    next_request_id: u64,
    pending_callbacks: HashMap<u64, PendingCallback>,
}

impl RequestTracker {
    fn new() -> Self {
        Self {
            next_request_id: 0,
            pending_callbacks: HashMap::new(),
        }
    }

    fn allocate_request_id(&mut self) -> u64 {
        let request_id = self.next_request_id;
        self.next_request_id += 1;
        request_id
    }

    fn register_callback(&mut self, request_id: u64, callback: PendingCallback) {
        self.pending_callbacks.insert(request_id, callback);
    }

    fn take_callback(&mut self, request_id: u64) -> Option<PendingCallback> {
        self.pending_callbacks.remove(&request_id)
    }

    fn has_pending(&self) -> bool {
        !self.pending_callbacks.is_empty()
    }
}

/// Manages inter-process coordination for await operations.
struct ProcessCoordinator {
    awaiting_map: HashMap<ProcessId, Vec<ProcessId>>,
    awaiting_result_requests: HashMap<u64, ProcessId>,
}

impl ProcessCoordinator {
    fn new() -> Self {
        Self {
            awaiting_map: HashMap::new(),
            awaiting_result_requests: HashMap::new(),
        }
    }

    fn register_await(&mut self, request_id: u64, target: ProcessId, caller: ProcessId) {
        self.awaiting_result_requests.insert(request_id, caller);

        let awaiting_map_entry = self.awaiting_map.entry(target).or_insert_with(Vec::new);
        if !awaiting_map_entry.contains(&caller) {
            awaiting_map_entry.push(caller);
        }
    }

    fn take_awaiter(&mut self, request_id: u64) -> Option<ProcessId> {
        self.awaiting_result_requests.remove(&request_id)
    }

    fn take_awaiters(&mut self, process_id: ProcessId) -> Option<Vec<ProcessId>> {
        self.awaiting_map.remove(&process_id)
    }

    fn remove_awaiter(&mut self, awaiter_pid: ProcessId) -> Option<ProcessId> {
        for (target_pid, awaiters) in self.awaiting_map.iter_mut() {
            if let Some(pos) = awaiters.iter().position(|&pid| pid == awaiter_pid) {
                awaiters.remove(pos);
                let target_to_remove = if awaiters.is_empty() {
                    Some(*target_pid)
                } else {
                    None
                };
                return target_to_remove;
            }
        }
        None
    }
}

/// Environment manages program state, process registry, and coordinates
/// execution across multiple executors in a platform-agnostic way.
pub struct Environment<R: Runtime> {
    runtime: R,
    program: Program,
    executor_count: usize,
    process_registry: ProcessRegistry,
    request_tracker: RequestTracker,
    process_coordinator: ProcessCoordinator,
}

impl<R: Runtime> Environment<R> {
    pub fn new(mut runtime: R, program: Program, executor_count: usize) -> Self {
        // Start executors
        for _ in 0..executor_count {
            runtime
                .start_executor(&program)
                .expect("Failed to start executor");
        }

        Self {
            runtime,
            program,
            executor_count,
            process_registry: ProcessRegistry::new(executor_count),
            request_tracker: RequestTracker::new(),
            process_coordinator: ProcessCoordinator::new(),
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
        let new_pid = self.process_registry.allocate_process();
        let executor_id = self.process_registry.get_executor(new_pid).unwrap();

        let request_id = self.request_tracker.allocate_request_id();

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

        let wrapped_callback = Box::new(move |event: Event| {
            if let Event::ExecuteResponse { result, .. } = event {
                callback(result);
            } else {
                eprintln!("Warning: Unexpected event type for ExecuteResponse");
            }
        });

        self.request_tracker
            .register_callback(request_id, wrapped_callback);
    }

    /// Wake an existing persistent process with new instructions.
    pub fn wake<F>(&mut self, process_id: ProcessId, instructions: Vec<Instruction>, callback: F)
    where
        F: FnOnce(Result<(), Error>) + 'static,
    {
        let executor_id = match self.process_registry.get_executor(process_id) {
            Some(id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.request_tracker.allocate_request_id();

        let command = SchedulerCommand::Wake {
            request_id,
            process_id,
            instructions,
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        let wrapped_callback = Box::new(move |event: Event| {
            if let Event::WakeResponse { result, .. } = event {
                callback(result);
            } else {
                eprintln!("Warning: Unexpected event type for WakeResponse");
            }
        });

        self.request_tracker
            .register_callback(request_id, wrapped_callback);
    }

    /// Get the result of a process execution.
    pub fn get_result<F>(&mut self, process_id: ProcessId, callback: F)
    where
        F: FnOnce(Result<(Value, Vec<Vec<u8>>), Error>) + 'static,
    {
        let executor_id = match self.process_registry.get_executor(process_id) {
            Some(id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.request_tracker.allocate_request_id();

        let command = SchedulerCommand::GetResult {
            request_id,
            process_id,
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        let wrapped_callback = Box::new(move |event: Event| {
            if let Event::GetResultResponse {
                result, heap_data, ..
            } = event
            {
                let result_with_heap = result.map(|value| (value, heap_data));
                callback(result_with_heap);
            } else {
                eprintln!("Warning: Unexpected event type for GetResultResponse");
            }
        });

        self.request_tracker
            .register_callback(request_id, wrapped_callback);
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

        let request_id = self.request_tracker.allocate_request_id();

        let command = SchedulerCommand::GetProcesses { request_id };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        let wrapped_callback = Box::new(move |event: Event| {
            if let Event::GetProcessesResponse { result, .. } = event {
                callback(result);
            } else {
                eprintln!("Warning: Unexpected event type for GetProcessesResponse");
            }
        });

        self.request_tracker
            .register_callback(request_id, wrapped_callback);
    }

    /// Get information about a specific process.
    pub fn get_process_info<F>(&mut self, process_id: ProcessId, callback: F)
    where
        F: FnOnce(Result<Option<ProcessInfo>, Error>) + 'static,
    {
        let executor_id = match self.process_registry.get_executor(process_id) {
            Some(id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.request_tracker.allocate_request_id();

        let command = SchedulerCommand::GetProcess {
            request_id,
            id: process_id,
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        let wrapped_callback = Box::new(move |event: Event| {
            if let Event::GetProcessResponse { result, .. } = event {
                callback(result);
            } else {
                eprintln!("Warning: Unexpected event type for GetProcessResponse");
            }
        });

        self.request_tracker
            .register_callback(request_id, wrapped_callback);
    }

    /// Get local variables from a process.
    pub fn get_locals<F>(&mut self, process_id: ProcessId, indices: &[usize], callback: F)
    where
        F: FnOnce(Result<Vec<Value>, Error>) + 'static,
    {
        let executor_id = match self.process_registry.get_executor(process_id) {
            Some(id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.request_tracker.allocate_request_id();

        let command = SchedulerCommand::GetLocals {
            request_id,
            process_id,
            indices: indices.to_vec(),
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        let wrapped_callback = Box::new(move |event: Event| {
            if let Event::GetLocalsResponse { result, .. } = event {
                callback(result);
            } else {
                eprintln!("Warning: Unexpected event type for GetLocalsResponse");
            }
        });

        self.request_tracker
            .register_callback(request_id, wrapped_callback);
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
        let executor_id = match self.process_registry.get_executor(process_id) {
            Some(id) => id,
            None => {
                callback(Err(Error::InvalidArgument(format!(
                    "Process {:?} not found in registry",
                    process_id
                ))));
                return;
            }
        };

        let request_id = self.request_tracker.allocate_request_id();

        let command = SchedulerCommand::CompactLocals {
            request_id,
            process_id,
            referenced_indices: referenced_indices.to_vec(),
        };

        if let Err(_) = self.runtime.send_command(executor_id, command) {
            callback(Err(Error::InvalidArgument("Executor died".to_string())));
            return;
        }

        let wrapped_callback = Box::new(move |event: Event| {
            if let Event::CompactLocalsResponse { result, .. } = event {
                callback(result);
            } else {
                eprintln!("Warning: Unexpected event type for CompactLocalsResponse");
            }
        });

        self.request_tracker
            .register_callback(request_id, wrapped_callback);
    }

    /// Check if there are pending callbacks waiting for responses.
    pub fn has_pending(&self) -> bool {
        self.request_tracker.has_pending()
    }

    /// Process all pending events from the runtime.
    /// Returns the number of events processed.
    pub fn process_pending_events(&mut self) -> usize {
        let events = self.runtime.poll();
        let count = events.len();

        for event in events {
            self.process_event(event);
        }

        count
    }

    fn process_event(&mut self, event: Event) {
        match event {
            Event::ExecuteResponse { request_id, result } => {
                self.handle_execute_response(request_id, result);
            }
            Event::WakeResponse { request_id, result } => {
                self.handle_wake_response(request_id, result);
            }
            Event::GetResultResponse {
                request_id,
                result,
                heap_data,
            } => {
                self.handle_get_result_response(request_id, result, heap_data);
            }
            Event::GetProcessesResponse { request_id, result } => {
                self.handle_get_processes_response(request_id, result);
            }
            Event::GetProcessResponse { request_id, result } => {
                self.handle_get_process_response(request_id, result);
            }
            Event::GetLocalsResponse { request_id, result } => {
                self.handle_get_locals_response(request_id, result);
            }
            Event::CompactLocalsResponse { request_id, result } => {
                self.handle_compact_locals_response(request_id, result);
            }
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
        }
    }

    fn handle_execute_response(&mut self, request_id: u64, result: Result<ProcessId, Error>) {
        if let Some(callback) = self.request_tracker.take_callback(request_id) {
            callback(Event::ExecuteResponse { request_id, result });
        } else {
            eprintln!("Warning: No pending callback for request {}", request_id);
        }
    }

    fn handle_wake_response(&mut self, request_id: u64, result: Result<(), Error>) {
        if let Some(callback) = self.request_tracker.take_callback(request_id) {
            callback(Event::WakeResponse { request_id, result });
        } else {
            eprintln!("Warning: No pending callback for request {}", request_id);
        }
    }

    fn handle_get_result_response(
        &mut self,
        request_id: u64,
        result: Result<Value, Error>,
        heap_data: Vec<Vec<u8>>,
    ) {
        // Check if this is a GetResult for an awaiting process
        if let Some(awaiter_pid) = self.process_coordinator.take_awaiter(request_id) {
            if let Ok(value) = result {
                if let Some(awaiter_executor_id) = self.process_registry.get_executor(awaiter_pid) {
                    let _ = self.runtime.send_command(
                        awaiter_executor_id,
                        SchedulerCommand::NotifyResult {
                            process_id: awaiter_pid,
                            result: value,
                            heap_data,
                        },
                    );
                }
                if let Some(target_pid) = self.process_coordinator.remove_awaiter(awaiter_pid) {
                    self.process_coordinator.take_awaiters(target_pid);
                }
            }
            return;
        }

        // Normal GetResult callback
        if let Some(callback) = self.request_tracker.take_callback(request_id) {
            callback(Event::GetResultResponse {
                request_id,
                result,
                heap_data,
            });
        } else {
            eprintln!("Warning: No pending callback for request {}", request_id);
        }
    }

    fn handle_get_processes_response(
        &mut self,
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, Error>,
    ) {
        if let Some(callback) = self.request_tracker.take_callback(request_id) {
            callback(Event::GetProcessesResponse { request_id, result });
        } else {
            eprintln!("Warning: No pending callback for request {}", request_id);
        }
    }

    fn handle_get_process_response(
        &mut self,
        request_id: u64,
        result: Result<Option<ProcessInfo>, Error>,
    ) {
        if let Some(callback) = self.request_tracker.take_callback(request_id) {
            callback(Event::GetProcessResponse { request_id, result });
        } else {
            eprintln!("Warning: No pending callback for request {}", request_id);
        }
    }

    fn handle_get_locals_response(&mut self, request_id: u64, result: Result<Vec<Value>, Error>) {
        if let Some(callback) = self.request_tracker.take_callback(request_id) {
            callback(Event::GetLocalsResponse { request_id, result });
        } else {
            eprintln!("Warning: No pending callback for request {}", request_id);
        }
    }

    fn handle_compact_locals_response(&mut self, request_id: u64, result: Result<(), Error>) {
        if let Some(callback) = self.request_tracker.take_callback(request_id) {
            callback(Event::CompactLocalsResponse { request_id, result });
        } else {
            eprintln!("Warning: No pending callback for request {}", request_id);
        }
    }

    fn handle_spawn_request(
        &mut self,
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
    ) {
        let new_pid = self.process_registry.allocate_process();
        let executor_id = self.process_registry.get_executor(new_pid).unwrap();

        let _ = self.runtime.send_command(
            executor_id,
            SchedulerCommand::Spawn {
                process_id: new_pid,
                function_index,
                captures,
                heap_data,
            },
        );

        if let Some(caller_executor_id) = self.process_registry.get_executor(caller) {
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
        if let Some(executor_id) = self.process_registry.get_executor(target) {
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
        if let Some(target_executor_id) = self.process_registry.get_executor(target) {
            let request_id = self.request_tracker.allocate_request_id();

            let command = SchedulerCommand::GetResult {
                request_id,
                process_id: target,
            };

            if self
                .runtime
                .send_command(target_executor_id, command)
                .is_ok()
            {
                self.process_coordinator
                    .register_await(request_id, target, caller);
            }
        }
    }

    fn handle_process_completed(
        &mut self,
        process_id: ProcessId,
        result: Value,
        heap_data: Vec<Vec<u8>>,
    ) {
        if let Some(awaiters) = self.process_coordinator.take_awaiters(process_id) {
            for awaiter_pid in awaiters {
                if let Some(executor_id) = self.process_registry.get_executor(awaiter_pid) {
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
