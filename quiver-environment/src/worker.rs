use crate::environment::{CompletedResult, EnvironmentError, ProcessResultsMap, RuntimeResult};
use crate::messages::{Command, Event};
use crate::transport::{CommandReceiver, EventSender};
use quiver_core::bytecode::{BuiltinInfo, Constant, Function};
use quiver_core::executor::Executor;
use quiver_core::process::{Action, Frame, ProcessId, ProcessStatus};
use quiver_core::types::TupleTypeInfo;
use quiver_core::value::Value;
use std::collections::{HashMap, HashSet};

const MAX_STEP_UNITS: usize = 1000;

pub struct Worker<R: CommandReceiver, S: EventSender> {
    executor: Executor,
    awaited: HashSet<ProcessId>,
    awaiters_for_target: HashMap<ProcessId, Vec<ProcessId>>, // target -> list of awaiters
    pending_result_requests: HashMap<ProcessId, Vec<u64>>,   // process_id -> list of request_ids
    receiver: R,
    sender: S,
}

impl<R: CommandReceiver, S: EventSender> Worker<R, S> {
    pub fn new(receiver: R, sender: S) -> Self {
        Self {
            executor: Executor::new(),
            awaited: HashSet::new(),
            awaiters_for_target: HashMap::new(),
            pending_result_requests: HashMap::new(),
            receiver,
            sender,
        }
    }

    /// Process one iteration of the worker loop
    /// Returns true if work was done, false if idle
    pub fn step(&mut self, current_time_ms: u64) -> Result<bool, EnvironmentError> {
        let mut did_work = false;

        // Process commands (non-blocking)
        while let Some(cmd) = self.receiver.try_recv()? {
            self.handle_command(cmd)?;
            did_work = true;
        }

        // Execute one step
        if let Some(action) = self.executor.step(MAX_STEP_UNITS, current_time_ms) {
            self.handle_action(action)?;
            did_work = true;
        }

        // Check for newly completed processes
        self.check_completed_processes()?;

        Ok(did_work)
    }

    fn handle_command(&mut self, command: Command) -> Result<(), EnvironmentError> {
        match command {
            Command::UpdateProgram {
                constants,
                functions,
                types,
                builtins,
            } => {
                self.update_program(constants, functions, types, builtins)?;
            }
            Command::StartProcess { id, function_index } => {
                self.start_process(id, function_index)?;
            }
            Command::SpawnProcess {
                id,
                function_index,
                captures,
                heap_data,
            } => {
                self.spawn_process(id, function_index, captures, heap_data)?;
            }
            Command::ResumeProcess { id, function_index } => {
                self.resume_process(id, function_index)?;
            }
            Command::QueryAndAwait { awaiter, targets } => {
                self.query_and_await(awaiter, targets)?;
            }
            Command::UpdateAwaitResults { awaiter, results } => {
                self.update_await_results(awaiter, results)?;
            }
            Command::DeliverMessage {
                target,
                message,
                heap,
            } => {
                self.deliver_message(target, message, heap)?;
            }
            Command::NotifySpawn {
                process_id,
                spawned_pid,
                function_index,
            } => {
                self.executor
                    .notify_spawn(process_id, Value::Process(spawned_pid, function_index));
            }
            Command::GetResult {
                request_id,
                process_id,
            } => {
                self.get_result(request_id, process_id)?;
            }
            Command::GetStatuses { request_id } => {
                self.get_statuses(request_id)?;
            }
            Command::GetProcessInfo {
                request_id,
                process_id,
            } => {
                self.get_process_info(request_id, process_id)?;
            }
            Command::GetLocals {
                request_id,
                process_id,
                indices,
            } => {
                self.get_locals(request_id, process_id, indices)?;
            }
            Command::CompactLocals {
                process_id,
                keep_indices,
            } => {
                self.compact_locals(process_id, keep_indices)?;
            }
        }
        Ok(())
    }

    fn handle_action(&mut self, action: Action) -> Result<(), EnvironmentError> {
        match action {
            Action::Spawn {
                caller,
                function_index,
                captures,
            } => {
                // Extract heap data from all captures
                let mut all_heap_data = Vec::new();
                let mut extracted_captures = Vec::new();

                for capture in captures {
                    let (extracted, mut heap) = self
                        .executor
                        .extract_heap_data(&capture)
                        .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                    extracted_captures.push(extracted);
                    all_heap_data.append(&mut heap);
                }

                self.sender.send(Event::SpawnAction {
                    caller,
                    function_index,
                    captures: extracted_captures,
                    heap: all_heap_data,
                })?;
            }
            Action::Deliver { target, value } => {
                let (message, heap) = self
                    .executor
                    .extract_heap_data(&value)
                    .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;

                self.sender.send(Event::DeliverAction {
                    target,
                    message,
                    heap,
                })?;
            }
            Action::Await { caller, targets } => {
                // Send single event with all targets
                self.sender.send(Event::AwaitAction {
                    awaiter: caller,
                    targets,
                })?;
            }
        }
        Ok(())
    }

    fn update_program(
        &mut self,
        constants: Vec<Constant>,
        functions: Vec<Function>,
        types: Vec<TupleTypeInfo>,
        builtins: Vec<BuiltinInfo>,
    ) -> Result<(), EnvironmentError> {
        self.executor
            .update_program(constants, functions, types, builtins);

        Ok(())
    }

    fn start_process(
        &mut self,
        id: ProcessId,
        function_index: usize,
    ) -> Result<(), EnvironmentError> {
        // Validate function exists
        if self.executor.get_function(function_index).is_none() {
            return Err(EnvironmentError::FunctionNotFound(function_index));
        }

        // Spawn the persistent process
        self.executor.spawn_process(id, function_index, true);

        // Push nil parameter onto stack
        let process = self
            .executor
            .get_process_mut(id)
            .ok_or(EnvironmentError::ProcessNotFound(id))?;
        process.stack.push(quiver_core::value::Value::nil());

        // Push initial frame with function index (no captures)
        let process = self
            .executor
            .get_process_mut(id)
            .ok_or(EnvironmentError::ProcessNotFound(id))?;
        process.frames.push(quiver_core::process::Frame::new(
            function_index,
            0,
            0, // no captures
        ));

        Ok(())
    }

    fn spawn_process(
        &mut self,
        id: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        // Validate function exists
        if self.executor.get_function(function_index).is_none() {
            return Err(EnvironmentError::FunctionNotFound(function_index));
        }

        // Spawn the process (non-persistent)
        self.executor.spawn_process(id, function_index, false);

        // Inject heap data and populate locals with captures
        let captures_count = captures.len();
        for value in captures {
            let injected = self
                .executor
                .inject_heap_data(value, &heap_data)
                .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;

            let process = self
                .executor
                .get_process_mut(id)
                .ok_or(EnvironmentError::ProcessNotFound(id))?;
            process.locals.push(injected);
        }

        // Push nil parameter onto stack
        let process = self
            .executor
            .get_process_mut(id)
            .ok_or(EnvironmentError::ProcessNotFound(id))?;
        process.stack.push(quiver_core::value::Value::nil());

        // Push initial frame with function index
        let process = self
            .executor
            .get_process_mut(id)
            .ok_or(EnvironmentError::ProcessNotFound(id))?;
        process.frames.push(quiver_core::process::Frame::new(
            function_index,
            0,
            captures_count,
        ));

        Ok(())
    }

    fn resume_process(
        &mut self,
        id: ProcessId,
        function_index: usize,
    ) -> Result<(), EnvironmentError> {
        // Validate function exists
        if self.executor.get_function(function_index).is_none() {
            return Err(EnvironmentError::FunctionNotFound(function_index));
        }

        let process = self
            .executor
            .get_process_mut(id)
            .ok_or(EnvironmentError::ProcessNotFound(id))?;

        // Check if the process failed
        if let Some(Err(_)) = &process.result {
            return Err(EnvironmentError::ProcessFailed(id));
        }

        // Check that the process is sleeping (it's persistent and has a successful result)
        if !process.persistent || !matches!(&process.result, Some(Ok(_))) {
            return Err(EnvironmentError::ProcessNotSleeping(id));
        }

        // Push the previous result onto the stack for continuations
        let result_value = process.result.take().unwrap().unwrap();
        process.stack.push(result_value);
        process.frames.push(Frame::new(function_index, 0, 0));

        // Add back to queue
        self.executor.add_to_queue(id);

        Ok(())
    }

    fn query_and_await(
        &mut self,
        awaiter: ProcessId,
        targets: Vec<ProcessId>,
    ) -> Result<(), EnvironmentError> {
        let mut results = HashMap::new();

        // Query each target and collect results
        for target in &targets {
            let target_status = self
                .executor
                .get_process_info(*target)
                .map(|info| info.status);
            let is_completed = matches!(
                target_status,
                Some(ProcessStatus::Completed) | Some(ProcessStatus::Sleeping)
            );

            if is_completed {
                // Process completed - include result in response
                if let Some(process) = self.executor.get_process(*target) {
                    let result = process
                        .result
                        .as_ref()
                        .expect("Completed process must have result");

                    let (extracted_result, heap) = match result {
                        Ok(value) => {
                            let (extracted, heap) = self
                                .executor
                                .extract_heap_data(value)
                                .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                            (Ok(extracted), heap)
                        }
                        Err(error) => (Err(error.clone()), vec![]),
                    };

                    results.insert(*target, Some((extracted_result, heap)));
                }
            } else {
                // Process not yet completed - register as awaiter
                self.awaited.insert(*target);
                // Track awaiter for this target so we can notify later
                self.awaiters_for_target
                    .entry(*target)
                    .or_default()
                    .push(awaiter);
                // Mark as None in results
                results.insert(*target, None);
            }
        }

        // Send response with all results
        self.sender
            .send(Event::ProcessResults { awaiter, results })?;

        Ok(())
    }

    fn update_await_results(
        &mut self,
        awaiter: ProcessId,
        results: ProcessResultsMap,
    ) -> Result<(), EnvironmentError> {
        let mut has_any_result = false;

        // Process each result and update awaiter
        for (awaited, result_opt) in results {
            if let Some((result, heap)) = result_opt {
                self.notify_result(awaiter, awaited, result, heap)?;
                has_any_result = true;
            }
        }

        // If no actual results were provided, manually wake up the awaiter
        // notify_result handles this when there are results
        if !has_any_result {
            // Remove from waiting and add to queue
            self.executor.mark_active(awaiter);
        }

        Ok(())
    }

    fn notify_result(
        &mut self,
        awaiter: ProcessId,
        awaited: ProcessId,
        result: RuntimeResult,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        match result {
            Ok(value) => {
                let injected = self
                    .executor
                    .inject_heap_data(value, &heap)
                    .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;

                self.executor.notify_result(awaiter, awaited, injected);
            }
            Err(error) => {
                // Set the process result to the error and clear frames to complete it
                if let Some(process) = self.executor.get_process_mut(awaiter) {
                    process.result = Some(Err(error));
                    process.frames.clear(); // Complete the process
                }
            }
        }
        Ok(())
    }

    fn deliver_message(
        &mut self,
        target: ProcessId,
        message: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        let injected = self
            .executor
            .inject_heap_data(message, &heap)
            .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;

        self.executor.notify_message(target, injected);
        Ok(())
    }

    fn get_result(
        &mut self,
        request_id: u64,
        process_id: ProcessId,
    ) -> Result<(), EnvironmentError> {
        // Check if process exists
        let process = self
            .executor
            .get_process(process_id)
            .ok_or(EnvironmentError::ProcessNotFound(process_id))?;

        // Check if process has a result
        match &process.result {
            Some(Ok(value)) => {
                // Process completed successfully - send response immediately
                let (extracted_value, heap) = self
                    .executor
                    .extract_heap_data(value)
                    .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;

                self.sender.send(Event::ResultResponse {
                    request_id,
                    result: Ok((extracted_value, heap)),
                })?;
            }
            Some(Err(error)) => {
                // Process failed - send error response immediately
                self.sender.send(Event::ResultResponse {
                    request_id,
                    result: Err(error.clone()),
                })?;
            }
            None => {
                // Process not done yet - register the request
                self.pending_result_requests
                    .entry(process_id)
                    .or_default()
                    .push(request_id);
            }
        }

        Ok(())
    }

    fn get_statuses(&mut self, request_id: u64) -> Result<(), EnvironmentError> {
        let statuses = self.executor.get_process_statuses();
        self.sender.send(Event::StatusesResponse {
            request_id,
            result: Ok(statuses),
        })?;
        Ok(())
    }

    fn get_process_info(
        &mut self,
        request_id: u64,
        process_id: ProcessId,
    ) -> Result<(), EnvironmentError> {
        let info = self.executor.get_process_info(process_id);
        self.sender.send(Event::InfoResponse {
            request_id,
            result: Ok(info),
        })?;
        Ok(())
    }

    fn get_locals(
        &mut self,
        request_id: u64,
        process_id: ProcessId,
        indices: Vec<usize>,
    ) -> Result<(), EnvironmentError> {
        let result = match self.executor.get_process(process_id) {
            Some(process) => {
                let mut locals = Vec::new();
                for &index in &indices {
                    match process.locals.get(index) {
                        Some(value) => {
                            let (extracted, heap) = self
                                .executor
                                .extract_heap_data(value)
                                .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                            locals.push((extracted, heap));
                        }
                        None => {
                            return self.sender.send(Event::LocalsResponse {
                                request_id,
                                result: Err(EnvironmentError::LocalNotFound { process_id, index }),
                            });
                        }
                    }
                }
                Ok(locals)
            }
            None => Err(EnvironmentError::ProcessNotFound(process_id)),
        };

        self.sender
            .send(Event::LocalsResponse { request_id, result })?;
        Ok(())
    }

    fn compact_locals(
        &mut self,
        process_id: ProcessId,
        keep_indices: Vec<usize>,
    ) -> Result<(), EnvironmentError> {
        let process = self
            .executor
            .get_process_mut(process_id)
            .ok_or(EnvironmentError::ProcessNotFound(process_id))?;

        // Create a new locals vector with only the values we want to keep
        let mut new_locals = Vec::new();
        for &index in &keep_indices {
            match process.locals.get(index) {
                Some(value) => new_locals.push(value.clone()),
                None => {
                    return Err(EnvironmentError::LocalNotFound { process_id, index });
                }
            }
        }

        process.locals = new_locals;
        Ok(())
    }

    fn extract_completed_result(
        &mut self,
        process_id: ProcessId,
    ) -> Result<Option<CompletedResult>, EnvironmentError> {
        if let Some(process) = self.executor.get_process(process_id)
            && let Some(result) = &process.result
        {
            let (extracted_result, heap) = match result {
                Ok(value) => {
                    let (extracted, heap) = self
                        .executor
                        .extract_heap_data(value)
                        .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                    (Ok(extracted), heap)
                }
                Err(error) => (Err(error.clone()), vec![]),
            };
            return Ok(Some((extracted_result, heap)));
        }
        Ok(None)
    }

    fn check_completed_processes(&mut self) -> Result<(), EnvironmentError> {
        // Check awaited processes for completion
        let awaited_pids: Vec<ProcessId> = self.awaited.iter().copied().collect();
        for process_id in awaited_pids {
            if let Some((result, heap)) = self.extract_completed_result(process_id)? {
                // Get all awaiters for this process
                if let Some(awaiters) = self.awaiters_for_target.remove(&process_id) {
                    // Send result to each awaiter
                    for awaiter in awaiters {
                        let mut results = HashMap::new();
                        results.insert(process_id, Some((result.clone(), heap.clone())));
                        self.sender
                            .send(Event::ProcessResults { awaiter, results })?;
                    }
                }
                self.awaited.remove(&process_id);
            }
        }

        // Check processes with pending result requests
        let pending_pids: Vec<ProcessId> = self.pending_result_requests.keys().copied().collect();
        for process_id in pending_pids {
            if let Some((result, heap)) = self.extract_completed_result(process_id)?
                && let Some(request_ids) = self.pending_result_requests.remove(&process_id)
            {
                for request_id in request_ids {
                    self.sender.send(Event::ResultResponse {
                        request_id,
                        result: match &result {
                            Ok(value) => Ok((value.clone(), heap.clone())),
                            Err(error) => Err(error.clone()),
                        },
                    })?;
                }
            }
        }

        Ok(())
    }
}
