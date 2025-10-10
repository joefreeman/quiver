use quiver_core::bytecode::{Constant, Function, Instruction, TypeId};
use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{Action, Frame, ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::TupleTypeInfo;
use quiver_core::value::Value;
use std::collections::HashMap;
use std::sync::mpsc::{Receiver, RecvTimeoutError, SyncSender, TryRecvError, sync_channel};
use std::thread::{self, JoinHandle};
use std::time::Duration;

enum PendingCallback {
    Execute(Box<dyn FnOnce(Result<ProcessId, Error>)>, usize), // callback, executor_id
    Wake(Box<dyn FnOnce(Result<(), Error>)>),
    GetResult(Box<dyn FnOnce(Result<(Value, Vec<Vec<u8>>), Error>)>),
    GetProcessStatuses(Box<dyn FnOnce(Result<HashMap<ProcessId, ProcessStatus>, Error>)>),
    GetProcessInfo(Box<dyn FnOnce(Result<Option<ProcessInfo>, Error>)>),
    GetLocals(Box<dyn FnOnce(Result<Vec<Value>, Error>)>),
    CompactLocals(Box<dyn FnOnce(Result<(), Error>)>),
}

#[derive(Debug)]
pub enum SchedulerCommand {
    Execute {
        request_id: u64,
        process_id: ProcessId,
        instructions: Vec<Instruction>,
        persistent: bool,
    },
    Wake {
        request_id: u64,
        process_id: ProcessId,
        instructions: Vec<Instruction>,
    },
    GetProcesses {
        request_id: u64,
    },
    GetProcess {
        request_id: u64,
        id: ProcessId,
    },
    GetLocals {
        request_id: u64,
        process_id: ProcessId,
        indices: Vec<usize>,
    },
    GetResult {
        request_id: u64,
        process_id: ProcessId,
    },
    CompactLocals {
        request_id: u64,
        process_id: ProcessId,
        referenced_indices: Vec<usize>,
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
    // Cross-executor notification commands
    NotifySpawn {
        process_id: ProcessId,
        pid_value: Value,
        heap_data: Vec<Vec<u8>>,
    },
    NotifyMessage {
        process_id: ProcessId,
        message: Value,
        heap_data: Vec<Vec<u8>>,
    },
    NotifyResult {
        process_id: ProcessId,
        result: Value,
        heap_data: Vec<Vec<u8>>,
    },
    // Spawn a process with a function and captures
    Spawn {
        process_id: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
    },
    Shutdown,
}

#[derive(Debug, Clone)]
pub enum Event {
    // Command responses (have request_id)
    ExecuteResponse {
        request_id: u64,
        result: Result<ProcessId, Error>,
    },
    WakeResponse {
        request_id: u64,
        result: Result<(), Error>,
    },
    GetProcessesResponse {
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, Error>,
    },
    GetProcessResponse {
        request_id: u64,
        result: Result<Option<ProcessInfo>, Error>,
    },
    GetLocalsResponse {
        request_id: u64,
        result: Result<Vec<Value>, Error>,
    },
    GetResultResponse {
        request_id: u64,
        result: Result<Value, Error>,
        heap_data: Vec<Vec<u8>>,
    },
    CompactLocalsResponse {
        request_id: u64,
        result: Result<(), Error>,
    },
    // Routing events from executors (no request_id)
    SpawnRequested {
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
    },
    DeliverMessage {
        target: ProcessId,
        value: Value,
        heap_data: Vec<Vec<u8>>,
    },
    AwaitResult {
        target: ProcessId,
        caller: ProcessId,
    },
    ProcessCompleted {
        process_id: ProcessId,
        result: Value,
        heap_data: Vec<Vec<u8>>,
    },
}

pub struct NativeRuntime {
    program: Program,
    command_txs: Vec<SyncSender<SchedulerCommand>>,
    event_rx: Receiver<Event>,
    process_registry: HashMap<ProcessId, usize>,
    next_executor: usize,
    thread_handles: Vec<JoinHandle<()>>,
    next_request_id: u64,
    pending_callbacks: HashMap<u64, PendingCallback>,
    // Cross-executor routing state
    next_process_id: usize,
    awaiting_map: HashMap<ProcessId, Vec<ProcessId>>,
    // Track GetResult requests that are for forwarding await results
    awaiting_result_requests: HashMap<u64, ProcessId>, // request_id -> awaiter_pid
}

impl NativeRuntime {
    pub fn new(program: Program, executor_count: usize) -> Self {
        let mut command_txs = Vec::new();
        let mut thread_handles = Vec::new();

        // Create shared event channel - all executors will send to this
        let (event_tx, event_rx) = sync_channel(100);

        // Create command channels
        let mut command_rxs = Vec::new();

        for _ in 0..executor_count {
            // Command channel: main -> executor
            let (command_tx, command_rx) = sync_channel(100);
            command_txs.push(command_tx);
            command_rxs.push(command_rx);
        }

        // Spawn executor threads
        for _ in 0..executor_count {
            let command_rx = command_rxs.remove(0);

            // Clone the shared response sender for this executor
            let event_tx = event_tx.clone();

            // Create executor
            let executor = Executor::new(&program);

            let thread_handle = thread::spawn(move || {
                scheduler_thread(executor, command_rx, event_tx);
            });

            thread_handles.push(thread_handle);
        }

        Self {
            program,
            command_txs,
            event_rx,
            process_registry: HashMap::new(),
            next_executor: 0,
            thread_handles,
            next_request_id: 0,
            pending_callbacks: HashMap::new(),
            next_process_id: 0,
            awaiting_map: HashMap::new(),
            awaiting_result_requests: HashMap::new(),
        }
    }

    /// Create a temporary Executor for read-only operations like formatting.
    /// This is a convenience method for operations that need access to the program state
    /// but don't need to run processes or modify state.
    pub fn executor(&self) -> Executor {
        Executor::new(&self.program)
    }

    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn program_mut(&mut self) -> &mut Program {
        &mut self.program
    }

    /// Update the Program with a new one, syncing only the differences to the executor.
    pub fn update_program(&mut self, new_program: Program) {
        // Sync new/modified constants
        let old_constants = self.program.get_constants();
        let new_constants = new_program.get_constants();
        for (index, constant) in new_constants.iter().enumerate() {
            if index >= old_constants.len() || &old_constants[index] != constant {
                self.sync_constant(index, constant.clone());
            }
        }

        // Sync new/modified functions
        let old_functions = self.program.get_functions();
        let new_functions = new_program.get_functions();
        for (index, function) in new_functions.iter().enumerate() {
            if index >= old_functions.len() || &old_functions[index] != function {
                self.sync_function(index, function.clone());
            }
        }

        // Sync new/modified builtins
        let old_builtins = self.program.get_builtins();
        let new_builtins = new_program.get_builtins();
        for (index, builtin) in new_builtins.iter().enumerate() {
            if index >= old_builtins.len() || &old_builtins[index] != builtin {
                self.sync_builtin(index, builtin.clone());
            }
        }

        // Sync new/modified types
        let old_types = self.program.get_types();
        let new_types = new_program.get_types();
        for (type_id, info) in new_types.iter() {
            if !old_types.contains_key(type_id) || &old_types[type_id] != info {
                self.sync_type(*type_id, info.clone());
            }
        }

        self.program = new_program;
    }

    /// Execute instructions in a new process.
    /// The callback will be called with the ProcessId of the newly created process.
    /// Call `poll()` to process responses and invoke callbacks.
    pub fn execute<F>(&mut self, instructions: Vec<Instruction>, persistent: bool, callback: F)
    where
        F: FnOnce(Result<ProcessId, Error>) + 'static,
    {
        // Allocate new process ID
        let new_pid = ProcessId(self.next_process_id);
        self.next_process_id += 1;

        // Select executor using round-robin
        let executor_id = self.next_executor % self.command_txs.len();
        self.next_executor += 1;

        // Register the process
        self.process_registry.insert(new_pid, executor_id);

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::Execute {
            request_id,
            process_id: new_pid,
            instructions,
            persistent,
        };

        // Send the command (non-blocking)
        if let Err(_) = self.command_txs[executor_id].send(command) {
            // If send fails, invoke callback immediately with error
            callback(Err(Error::InvalidArgument(
                "Scheduler thread died".to_string(),
            )));
            return;
        }

        // Store the callback along with executor_id so we can update process_registry in poll()
        self.pending_callbacks.insert(
            request_id,
            PendingCallback::Execute(Box::new(callback), executor_id),
        );
    }

    /// Wake an existing persistent process with new instructions.
    /// The process must be idle (no frames on the call stack).
    /// Call `poll()` to process responses and invoke callbacks.
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

        // Send the command (non-blocking)
        if let Err(_) = self.command_txs[executor_id].send(command) {
            callback(Err(Error::InvalidArgument(
                "Scheduler thread died".to_string(),
            )));
            return;
        }

        // Store the callback
        self.pending_callbacks
            .insert(request_id, PendingCallback::Wake(Box::new(callback)));
    }

    /// Get the result of a process execution.
    /// Returns the value and its associated heap data.
    /// Returns an error if the process has not yet completed or has no result.
    /// Call `poll()` to process responses and invoke callbacks.
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

        // Send the command (non-blocking)
        if let Err(_) = self.command_txs[executor_id].send(command) {
            callback(Err(Error::InvalidArgument(
                "Scheduler thread died".to_string(),
            )));
            return;
        }

        // Store the callback
        self.pending_callbacks
            .insert(request_id, PendingCallback::GetResult(Box::new(callback)));
    }

    /// Get the number of executors in this runtime.
    pub fn executor_count(&self) -> usize {
        self.command_txs.len()
    }

    /// Get the status of all processes on a specific executor.
    /// Call `poll()` to process responses and invoke callbacks.
    pub fn get_process_statuses<F>(&mut self, executor_id: usize, callback: F)
    where
        F: FnOnce(Result<HashMap<ProcessId, ProcessStatus>, Error>) + 'static,
    {
        if executor_id >= self.command_txs.len() {
            callback(Err(Error::InvalidArgument(format!(
                "Invalid executor_id: {}",
                executor_id
            ))));
            return;
        }

        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let command = SchedulerCommand::GetProcesses { request_id };

        // Send the command (non-blocking)
        if let Err(_) = self.command_txs[executor_id].send(command) {
            callback(Err(Error::InvalidArgument(
                "Scheduler thread died".to_string(),
            )));
            return;
        }

        // Store the callback
        self.pending_callbacks.insert(
            request_id,
            PendingCallback::GetProcessStatuses(Box::new(callback)),
        );
    }

    /// Get information about a specific process.
    /// Call `poll()` to process responses and invoke callbacks.
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

        // Send the command (non-blocking)
        if let Err(_) = self.command_txs[executor_id].send(command) {
            callback(Err(Error::InvalidArgument(
                "Scheduler thread died".to_string(),
            )));
            return;
        }

        // Store the callback
        self.pending_callbacks.insert(
            request_id,
            PendingCallback::GetProcessInfo(Box::new(callback)),
        );
    }

    /// Get local variables from a process.
    /// Call `poll()` to process responses and invoke callbacks.
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

        // Send the command (non-blocking)
        if let Err(_) = self.command_txs[executor_id].send(command) {
            callback(Err(Error::InvalidArgument(
                "Scheduler thread died".to_string(),
            )));
            return;
        }

        // Store the callback
        self.pending_callbacks
            .insert(request_id, PendingCallback::GetLocals(Box::new(callback)));
    }

    /// Compact locals in a persistent process to keep only referenced variables.
    /// This is used by the REPL to prevent locals from growing unbounded.
    /// Call `poll()` to process responses and invoke callbacks.
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

        // Send the command (non-blocking)
        if let Err(_) = self.command_txs[executor_id].send(command) {
            callback(Err(Error::InvalidArgument(
                "Scheduler thread died".to_string(),
            )));
            return;
        }

        // Store the callback
        self.pending_callbacks.insert(
            request_id,
            PendingCallback::CompactLocals(Box::new(callback)),
        );
    }

    /// Check if there are pending callbacks waiting for responses.
    pub fn has_pending(&self) -> bool {
        !self.pending_callbacks.is_empty()
    }

    /// Poll for responses and invoke callbacks.
    /// Returns the number of responses/events processed (including both callbacks and routing events).
    pub fn poll(&mut self) -> usize {
        let mut responses_processed = 0;

        // Check for responses with a short timeout
        // We always check even if no callbacks are pending, because routing events can arrive
        let timeout = if self.pending_callbacks.is_empty() {
            Duration::from_millis(1) // Very short timeout when no callbacks pending
        } else {
            Duration::from_millis(10)
        };

        match self.event_rx.recv_timeout(timeout) {
            Ok(event) => {
                self.process_event(event);
                responses_processed += 1;

                // Drain any additional responses that are immediately available
                loop {
                    match self.event_rx.try_recv() {
                        Ok(event) => {
                            self.process_event(event);
                            responses_processed += 1;
                        }
                        Err(_) => break,
                    }
                }
            }
            Err(RecvTimeoutError::Timeout) => {
                // No response within timeout - this is fine
            }
            Err(RecvTimeoutError::Disconnected) => {
                // All executor threads died
                eprintln!("Warning: All executor threads disconnected");
            }
        }

        responses_processed
    }

    /// Wait for all pending callbacks to complete.
    /// This is the main polling loop that should be used instead of manually calling poll().
    pub fn wait_for_callbacks(&mut self) -> Result<(), Error> {
        // First, wait for all pending callbacks
        while self.has_pending() {
            if self.poll() == 0 {
                // Sleep to avoid spinning
                std::thread::sleep(Duration::from_millis(10));
            }
        }

        // After all callbacks are done, continue polling for a short time
        // to process any final routing events and commands (like spawn notifications)
        let mut idle_iterations = 0;
        while idle_iterations < 5 {
            // Poll with short timeout
            if self.poll() > 0 {
                // Got some responses, reset counter
                idle_iterations = 0;
            } else {
                // No responses, increment counter
                idle_iterations += 1;
                std::thread::sleep(Duration::from_millis(2));
            }
        }

        Ok(())
    }

    fn process_event(&mut self, event: Event) {
        match event {
            // Handle routing events (no request_id)
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
            // Handle command responses (have request_id)
            _ => {
                let request_id = match &event {
                    Event::ExecuteResponse { request_id, .. } => *request_id,
                    Event::WakeResponse { request_id, .. } => *request_id,
                    Event::GetProcessesResponse { request_id, .. } => *request_id,
                    Event::GetProcessResponse { request_id, .. } => *request_id,
                    Event::GetLocalsResponse { request_id, .. } => *request_id,
                    Event::GetResultResponse { request_id, .. } => *request_id,
                    Event::CompactLocalsResponse { request_id, .. } => *request_id,
                    _ => return, // Already handled above
                };

                // Check if this is an await forwarding request
                if let Some(awaiter_pid) = self.awaiting_result_requests.remove(&request_id) {
                    // This GetResult is for forwarding to an awaiter
                    if let Event::GetResultResponse {
                        result, heap_data, ..
                    } = event
                    {
                        match result {
                            Ok(value) => {
                                // Forward result to awaiter
                                if let Some(&awaiter_executor_id) =
                                    self.process_registry.get(&awaiter_pid)
                                {
                                    let _ = self.command_txs[awaiter_executor_id].send(
                                        SchedulerCommand::NotifyResult {
                                            process_id: awaiter_pid,
                                            result: value.clone(),
                                            heap_data: heap_data.clone(),
                                        },
                                    );
                                }
                                // Remove from awaiting_map since result was successfully forwarded
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
                            Err(_) => {
                                // Target process hasn't completed yet, awaiter stays in awaiting_map
                            }
                        }
                    }
                    return; // Don't process as a regular callback
                }

                if let Some(pending) = self.pending_callbacks.remove(&request_id) {
                    match (pending, event) {
                        (
                            PendingCallback::Execute(callback, _executor_id),
                            Event::ExecuteResponse { result, .. },
                        ) => {
                            // Process registry already updated when command was sent
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
                            // Return value with its heap data
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
                            // Mismatched response/callback types - this shouldn't happen
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
        // Allocate new process ID
        let new_pid = ProcessId(self.next_process_id);
        self.next_process_id += 1;

        // Select executor using round-robin
        let executor_id = self.next_executor % self.command_txs.len();
        self.next_executor += 1;

        // Register the new process
        self.process_registry.insert(new_pid, executor_id);

        // Send Spawn command to selected executor for the new process
        let _ = self.command_txs[executor_id].send(SchedulerCommand::Spawn {
            process_id: new_pid,
            function_index,
            captures,
            heap_data,
        });

        // Notify caller with new PID (no heap data in PID values)
        if let Some(&caller_executor_id) = self.process_registry.get(&caller) {
            let _ = self.command_txs[caller_executor_id].send(SchedulerCommand::NotifySpawn {
                process_id: caller,
                pid_value: Value::Pid(new_pid),
                heap_data: Vec::new(),
            });
        }
    }

    fn handle_deliver_message(&mut self, target: ProcessId, value: Value, heap_data: Vec<Vec<u8>>) {
        // Route message to target's executor with heap data
        if let Some(&executor_id) = self.process_registry.get(&target) {
            let _ = self.command_txs[executor_id].send(SchedulerCommand::NotifyMessage {
                process_id: target,
                message: value,
                heap_data,
            });
        }
    }

    fn handle_await_result(&mut self, target: ProcessId, caller: ProcessId) {
        // Query the target's executor to see if it has a result
        if let Some(&target_executor_id) = self.process_registry.get(&target) {
            let request_id = self.next_request_id;
            self.next_request_id += 1;

            // Send GetResult to target's executor
            let command = SchedulerCommand::GetResult {
                request_id,
                process_id: target,
            };

            if self.command_txs[target_executor_id].send(command).is_ok() {
                // Track this as an await forwarding request, with the target for cleanup
                self.awaiting_result_requests.insert(request_id, caller);

                // Add caller to awaiting_map in case target hasn't completed yet
                let awaiting_map_entry = self.awaiting_map.entry(target).or_insert_with(Vec::new);
                // Only add if not already in the list
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
        // Notify all processes awaiting this result
        if let Some(awaiters) = self.awaiting_map.remove(&process_id) {
            for awaiter_pid in awaiters {
                if let Some(&executor_id) = self.process_registry.get(&awaiter_pid) {
                    let _ = self.command_txs[executor_id].send(SchedulerCommand::NotifyResult {
                        process_id: awaiter_pid,
                        result: result.clone(),
                        heap_data: heap_data.clone(),
                    });
                }
            }
        }
    }

    fn sync_constant(&self, index: usize, constant: Constant) {
        for command_tx in &self.command_txs {
            let _ = command_tx.send(SchedulerCommand::RegisterConstant {
                index,
                constant: constant.clone(),
            });
        }
    }

    fn sync_function(&self, index: usize, function: Function) {
        for command_tx in &self.command_txs {
            let _ = command_tx.send(SchedulerCommand::RegisterFunction {
                index,
                function: function.clone(),
            });
        }
    }

    fn sync_builtin(&self, index: usize, name: String) {
        for command_tx in &self.command_txs {
            let _ = command_tx.send(SchedulerCommand::RegisterBuiltin {
                index,
                name: name.clone(),
            });
        }
    }

    fn sync_type(&self, type_id: TypeId, info: TupleTypeInfo) {
        for command_tx in &self.command_txs {
            let _ = command_tx.send(SchedulerCommand::RegisterType {
                type_id,
                info: info.clone(),
            });
        }
    }

    pub fn shutdown(&mut self) {
        for command_tx in &self.command_txs {
            let _ = command_tx.send(SchedulerCommand::Shutdown);
        }
        for handle in self.thread_handles.drain(..) {
            let _ = handle.join();
        }
    }
}

fn scheduler_thread(
    mut executor: Executor,
    command_rx: Receiver<SchedulerCommand>,
    event_tx: SyncSender<Event>,
) {
    let mut pending_results: HashMap<u64, ProcessId> = HashMap::new();
    // Track errors that occurred during process execution
    let mut process_errors: HashMap<ProcessId, Error> = HashMap::new();
    // Track which processes we've already sent completion events for
    let mut completion_sent: std::collections::HashSet<ProcessId> =
        std::collections::HashSet::new();

    loop {
        // 1. Handle scheduler commands
        // If executor is idle (no active processes), use blocking receive with timeout.
        // Otherwise use try_recv to avoid blocking when there's work to do.
        let is_idle = !executor
            .get_process_statuses()
            .values()
            .any(|s| *s == ProcessStatus::Active);

        let command_result = if is_idle {
            // Block with timeout when idle to avoid spinning
            command_rx
                .recv_timeout(Duration::from_millis(100))
                .map_err(|e| match e {
                    RecvTimeoutError::Timeout => TryRecvError::Empty,
                    RecvTimeoutError::Disconnected => TryRecvError::Disconnected,
                })
        } else {
            // Non-blocking when there's active work
            command_rx.try_recv()
        };

        match command_result {
            Ok(SchedulerCommand::Execute {
                request_id,
                process_id,
                instructions,
                persistent,
            }) => {
                // Spawn the new process with the provided ID
                executor.spawn_process(process_id, persistent);

                // Set up the process with instructions
                let result = if let Some(process) = executor.get_process_mut(process_id) {
                    // All processes start with the parameter on the stack
                    process.stack.push(Value::nil());

                    if instructions.is_empty() {
                        // Empty instructions: just set result to nil
                        // But keep stack with parameter for consistency
                        process.result = Some(Value::nil());
                        Ok(process_id)
                    } else {
                        process.frames.push(Frame::new(instructions, 0, 0));
                        executor.add_to_queue(process_id);
                        Ok(process_id)
                    }
                } else {
                    Err(Error::InvalidArgument(format!(
                        "Process {:?} not found after spawning",
                        process_id
                    )))
                };

                let _ = event_tx.send(Event::ExecuteResponse { request_id, result });
            }
            Ok(SchedulerCommand::RegisterConstant { index, constant }) => {
                executor.register_constant(index, constant);
            }
            Ok(SchedulerCommand::RegisterFunction { index, function }) => {
                executor.register_function(index, function);
            }
            Ok(SchedulerCommand::RegisterBuiltin { index, name }) => {
                executor.register_builtin(index, name);
            }
            Ok(SchedulerCommand::RegisterType { type_id, info }) => {
                executor.register_type(type_id, info);
            }
            Ok(SchedulerCommand::Wake {
                request_id,
                process_id,
                instructions,
            }) => {
                let result = match executor.get_process_mut(process_id) {
                    Some(process) => {
                        // Check if process is persistent
                        if !process.persistent {
                            Err(Error::InvalidArgument(format!(
                                "Cannot wake non-persistent process {:?}",
                                process_id
                            )))
                        // Check if process is idle (no frames)
                        } else if !process.frames.is_empty() {
                            Err(Error::InvalidArgument(format!(
                                "Cannot wake process {:?} - not idle (has {} frames)",
                                process_id,
                                process.frames.len()
                            )))
                        } else {
                            // Clear the result and set up with new instructions
                            let initial_value = process.result.take().unwrap_or_else(Value::nil);
                            process.stack.push(initial_value);
                            // For persistent processes (like REPL), locals_base should be 0
                            // so that variable indices remain absolute across evaluations
                            process.frames.push(Frame::new(instructions, 0, 0));
                            executor.add_to_queue(process_id);
                            Ok(())
                        }
                    }
                    None => Err(Error::InvalidArgument(format!(
                        "Process {:?} not found",
                        process_id
                    ))),
                };

                let _ = event_tx.send(Event::WakeResponse { request_id, result });
            }
            Ok(SchedulerCommand::GetProcesses { request_id }) => {
                let statuses = executor.get_process_statuses();
                let _ = event_tx.send(Event::GetProcessesResponse {
                    request_id,
                    result: Ok(statuses),
                });
            }
            Ok(SchedulerCommand::GetProcess { request_id, id }) => {
                let info = executor.get_process_info(id);
                let _ = event_tx.send(Event::GetProcessResponse {
                    request_id,
                    result: Ok(info),
                });
            }
            Ok(SchedulerCommand::GetLocals {
                request_id,
                process_id,
                indices,
            }) => {
                let result = match executor.get_process(process_id) {
                    Some(process) => {
                        let mut values = Vec::new();
                        for &index in &indices {
                            if index >= process.locals.len() {
                                values.clear();
                                break;
                            }
                            values.push(process.locals[index].clone());
                        }
                        if values.len() == indices.len() {
                            Ok(values)
                        } else {
                            Err(Error::InvalidArgument(
                                "One or more local indices out of bounds".to_string(),
                            ))
                        }
                    }
                    None => Err(Error::InvalidArgument(format!(
                        "Process {:?} not found",
                        process_id
                    ))),
                };
                let _ = event_tx.send(Event::GetLocalsResponse { request_id, result });
            }
            Ok(SchedulerCommand::GetResult {
                request_id,
                process_id,
            }) => {
                // Add to pending results - will be checked each iteration
                pending_results.insert(request_id, process_id);
            }
            Ok(SchedulerCommand::CompactLocals {
                request_id,
                process_id,
                referenced_indices,
            }) => {
                let result = match executor.get_process_mut(process_id) {
                    Some(process) => {
                        // Create new locals vector with only referenced values
                        let mut new_locals = Vec::new();
                        let mut error = None;
                        for &index in &referenced_indices {
                            if index < process.locals.len() {
                                new_locals.push(process.locals[index].clone());
                            } else {
                                error = Some(Error::InvalidArgument(format!(
                                    "Referenced index {} out of bounds (locals size: {})",
                                    index,
                                    process.locals.len()
                                )));
                                break;
                            }
                        }
                        if let Some(e) = error {
                            Err(e)
                        } else {
                            process.locals = new_locals;
                            Ok(())
                        }
                    }
                    None => Err(Error::InvalidArgument(format!(
                        "Process {:?} not found",
                        process_id
                    ))),
                };
                let _ = event_tx.send(Event::CompactLocalsResponse { request_id, result });
            }
            Ok(SchedulerCommand::NotifySpawn {
                process_id,
                pid_value,
                heap_data,
            }) => {
                // Inject heap data into pid_value
                let value = executor
                    .inject_heap_data(pid_value, &heap_data)
                    .unwrap_or_else(|_| Value::nil());
                executor.notify_spawn(process_id, value);
            }
            Ok(SchedulerCommand::NotifyMessage {
                process_id,
                message,
                heap_data,
            }) => {
                // Inject heap data into message
                let value = executor
                    .inject_heap_data(message, &heap_data)
                    .unwrap_or_else(|_| Value::nil());
                executor.notify_message(process_id, value);
            }
            Ok(SchedulerCommand::NotifyResult {
                process_id,
                result,
                heap_data,
            }) => {
                // Inject heap data into result
                let value = executor
                    .inject_heap_data(result, &heap_data)
                    .unwrap_or_else(|_| Value::nil());
                executor.notify_result(process_id, value);
            }
            Ok(SchedulerCommand::Spawn {
                process_id,
                function_index,
                captures,
                heap_data,
            }) => {
                // Inject heap data into captures
                let injected_captures: Vec<Value> = captures
                    .into_iter()
                    .map(|capture| {
                        executor
                            .inject_heap_data(capture, &heap_data)
                            .unwrap_or_else(|_| Value::nil())
                    })
                    .collect();

                // Spawn the new process
                executor.spawn_process(process_id, false);

                // Set up the process with the function and captures
                if let Some(process) = executor.get_process_mut(process_id) {
                    // Push NIL as parameter, then the function, then execute Call
                    process.stack.push(Value::nil());
                    process
                        .stack
                        .push(Value::Function(function_index, injected_captures));
                    process
                        .frames
                        .push(Frame::new(vec![Instruction::Call], 0, 0));
                }

                // Add the new process to the queue so it can run
                executor.add_to_queue(process_id);
            }
            Ok(SchedulerCommand::Shutdown) => {
                break;
            }
            Err(TryRecvError::Empty) => {
                // No command available, continue to next step
            }
            Err(TryRecvError::Disconnected) => {
                // Main thread disconnected, exit
                break;
            }
        }

        // 2. Step the executor and handle routing requests
        let step_result = executor.step(1000);

        match step_result {
            Ok(None) => {
                // Normal execution, no routing needed
            }
            Ok(Some(Action::Spawn {
                caller,
                function_index,
                captures,
            })) => {
                // Extract heap data from captures for cross-executor transfer
                let (converted_captures, heap_data) = captures
                    .into_iter()
                    .try_fold(
                        (Vec::new(), Vec::new()),
                        |(mut caps, mut all_heap_data), capture| {
                            let (converted, mut capture_heap_data) =
                                executor.extract_heap_data(&capture)?;
                            caps.push(converted);
                            all_heap_data.append(&mut capture_heap_data);
                            Ok::<_, Error>((caps, all_heap_data))
                        },
                    )
                    .unwrap_or_else(|_| (Vec::new(), Vec::new()));

                // Send spawn request to runtime for routing
                let _ = event_tx.send(Event::SpawnRequested {
                    caller,
                    function_index,
                    captures: converted_captures,
                    heap_data,
                });
            }
            Ok(Some(Action::Deliver { target, value })) => {
                // Extract heap data from value for cross-executor transfer
                let (converted_value, heap_data) = executor
                    .extract_heap_data(&value)
                    .unwrap_or_else(|_| (value.clone(), Vec::new()));

                // Send deliver request to runtime for routing
                let _ = event_tx.send(Event::DeliverMessage {
                    target,
                    value: converted_value,
                    heap_data,
                });
            }
            Ok(Some(Action::AwaitResult { target, caller })) => {
                // Send await request to runtime for routing
                let _ = event_tx.send(Event::AwaitResult { target, caller });
            }
            Err(error) => {
                // Try to determine which process encountered the error
                // For now, we'll check all processes and find ones with no frames and no result
                let failed_processes: Vec<ProcessId> = executor
                    .get_process_statuses()
                    .keys()
                    .filter(|&&pid| {
                        if let Some(info) = executor.get_process_info(pid) {
                            info.frames_count == 0 && info.result.is_none() && info.persistent
                        } else {
                            false
                        }
                    })
                    .copied()
                    .collect();

                // Store the error for any failed processes
                for pid in failed_processes {
                    process_errors.insert(pid, error.clone());
                }
            }
        }

        // 3. Check pending result requests and send ProcessCompleted events
        check_pending_results(
            &mut executor,
            &mut pending_results,
            &event_tx,
            &process_errors,
            &mut completion_sent,
        );

        // 4. Check for any newly completed processes and send completion events
        // This is needed for await to work, as awaiters don't send GetResult
        check_completed_processes(&mut executor, &event_tx, &mut completion_sent);
    }
}

fn check_completed_processes(
    executor: &mut Executor,
    event_tx: &SyncSender<Event>,
    completion_sent: &mut std::collections::HashSet<ProcessId>,
) {
    let statuses = executor.get_process_statuses();
    let mut newly_completed = Vec::new();

    for (process_id, status) in statuses {
        // Check if process is terminated/completed and we haven't sent notification yet
        if matches!(status, ProcessStatus::Terminated | ProcessStatus::Sleeping) {
            if !completion_sent.contains(&process_id) {
                if let Some(process) = executor.get_process(process_id) {
                    if let Some(result) = &process.result {
                        newly_completed.push((process_id, result.clone()));
                        completion_sent.insert(process_id);
                    }
                }
            }
        }
    }

    // Send ProcessCompleted events
    for (process_id, result) in newly_completed {
        let (converted_result, heap_data) = executor
            .extract_heap_data(&result)
            .unwrap_or_else(|_| (result.clone(), Vec::new()));

        let _ = event_tx.send(Event::ProcessCompleted {
            process_id,
            result: converted_result,
            heap_data,
        });
    }
}

fn check_pending_results(
    executor: &mut Executor,
    pending_results: &mut HashMap<u64, ProcessId>,
    event_tx: &SyncSender<Event>,
    process_errors: &HashMap<ProcessId, Error>,
    completion_sent: &mut std::collections::HashSet<ProcessId>,
) {
    let mut completed = Vec::new();
    let mut newly_completed = Vec::new();

    for (&request_id, &process_id) in pending_results.iter() {
        if let Some(process) = executor.get_process_mut(process_id) {
            // Check if the process has completed its current frame
            let frame_exhausted = process
                .frames
                .last()
                .map_or(true, |frame| frame.counter >= frame.instructions.len());

            if frame_exhausted && !process.frames.is_empty() {
                // Pop the completed frame and get the result from the stack
                process.frames.pop();

                let (result, heap_data) = if !process.stack.is_empty() {
                    let value = process.stack.last().cloned();
                    // Store result in process.result if frames are now empty
                    if process.frames.is_empty() {
                        process.result = value.clone();
                        // Track that this process now has a result
                        if let Some(val) = value.clone() {
                            // Only send completion event if we haven't already
                            if !completion_sent.contains(&process_id) {
                                newly_completed.push((process_id, val));
                                completion_sent.insert(process_id);
                            }
                        }
                    }
                    match value {
                        Some(v) => {
                            // Extract heap data for API return
                            match executor.extract_heap_data(&v) {
                                Ok((converted, heap_data)) => (Ok(converted), heap_data),
                                Err(e) => (Err(e), vec![]),
                            }
                        }
                        None => (
                            Err(Error::InvalidArgument(
                                "Process completed with empty stack".to_string(),
                            )),
                            vec![],
                        ),
                    }
                } else {
                    (
                        Err(Error::InvalidArgument(
                            "Process completed with empty stack".to_string(),
                        )),
                        vec![],
                    )
                };

                let _ = event_tx.send(Event::GetResultResponse {
                    request_id,
                    result,
                    heap_data,
                });
                completed.push(request_id);
            } else if frame_exhausted && process.frames.is_empty() {
                // Process already completed, send stored result
                let (result, heap_data) = match process.result.clone() {
                    Some(v) => {
                        // Extract heap data for API return
                        match executor.extract_heap_data(&v) {
                            Ok((converted, heap_data)) => (Ok(converted), heap_data),
                            Err(e) => (Err(e), vec![]),
                        }
                    }
                    None => {
                        // Check if we have a stored error for this process
                        let error = process_errors.get(&process_id).cloned().unwrap_or_else(|| {
                            Error::InvalidArgument(format!(
                                "Process {:?} has no result",
                                process_id
                            ))
                        });
                        (Err(error), vec![])
                    }
                };

                let _ = event_tx.send(Event::GetResultResponse {
                    request_id,
                    result,
                    heap_data,
                });
                completed.push(request_id);
            }
        }
    }

    // Remove completed requests
    for request_id in completed {
        pending_results.remove(&request_id);
    }

    // Send ProcessCompleted events for newly completed processes
    for (completed_pid, result) in newly_completed {
        let (converted_result, heap_data) = executor
            .extract_heap_data(&result)
            .unwrap_or_else(|_| (result.clone(), Vec::new()));

        let _ = event_tx.send(Event::ProcessCompleted {
            process_id: completed_pid,
            result: converted_result,
            heap_data,
        });
    }
}
