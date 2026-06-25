use crate::environment::{EnvironmentError, ProcessResultsMap, RuntimeResult};
use crate::messages::{Command, Event};
use crate::transport::{CommandReceiver, EventSender};
use quiver_core::effects::Effect;
use quiver_core::executor::Executor;
use quiver_core::process::{Action, Frame, ProcessId, ProcessStatus};
use quiver_core::value::Value;
use std::collections::{HashMap, HashSet};

const MAX_STEP_UNITS: usize = 1000;

/// A pending result request: the request id, plus an optional keep-set whose complement is released
/// from the process's locals once it completes (the REPL's orphaned-local reclaim).
type PendingResultRequest = (u64, Option<Vec<usize>>);

pub struct Worker<E: Effect, R: CommandReceiver<E>, S: EventSender<E>> {
    executor: Executor<E>,
    awaited: HashSet<ProcessId>,
    awaiters_for_target: HashMap<ProcessId, Vec<ProcessId>>, // target -> list of awaiters
    // process_id -> pending result requests; each request's keep-set drives orphaned-local release
    // once the process completes (see `Command::GetResult`).
    pending_result_requests: HashMap<ProcessId, Vec<PendingResultRequest>>,
    receiver: R,
    sender: S,
}

impl<E: Effect, R: CommandReceiver<E>, S: EventSender<E>> Worker<E, R, S> {
    pub fn new(
        receiver: R,
        sender: S,
        builtins: quiver_core::builtins::BuiltinRegistry<E>,
        profile: bool,
        worker_id: u16,
    ) -> Self {
        Self {
            executor: Executor::new(builtins, profile, worker_id),
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
        let (executed, action) = self.executor.step(MAX_STEP_UNITS, current_time_ms);
        if executed {
            did_work = true;
        }
        if let Some(action) = action {
            self.handle_action(action)?;
        }

        // Check for newly completed processes
        self.check_completed_processes()?;

        Ok(did_work)
    }

    /// Whether a process is queued to run immediately. Event-driven runtimes keep stepping while
    /// this is true and go idle once it is false (and no timeout is pending).
    pub fn has_runnable(&self) -> bool {
        self.executor.has_runnable()
    }

    /// Earliest absolute clock time (ms) at which a pending select timeout expires, if any. When
    /// idle, an event-driven runtime can schedule a single timer for this instant rather than
    /// polling the clock. `None` means nothing time-based is pending — wait for a command.
    pub fn next_timeout_ms(&self) -> Option<u64> {
        self.executor.next_timeout_ms()
    }

    fn handle_command(&mut self, command: Command<E>) -> Result<(), EnvironmentError> {
        match command {
            Command::UpdateProgram(update) => {
                self.update_program(update)?;
            }
            Command::StartProcess { id, function_index } => {
                self.start_process(id, function_index)?;
            }
            Command::SpawnProcess {
                id,
                function_index,
                captures,
                argument,
                heap_data,
            } => {
                self.spawn_process(id, function_index, captures, argument, heap_data)?;
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
                keep_locals,
            } => {
                self.get_result(request_id, process_id, keep_locals)?;
            }
            Command::GetStatuses { request_id } => {
                self.get_statuses(request_id)?;
            }
            Command::GetWorkerInfo { request_id } => {
                self.get_worker_info(request_id)?;
            }
            Command::GetProcessTypes { request_id } => {
                self.get_process_types(request_id)?;
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
            Command::GetExecutionStats { request_id } => {
                self.get_execution_stats(request_id)?;
            }
            Command::EffectCompletion {
                process_id,
                result,
                heap,
            } => {
                self.executor
                    .notify_effect_completion(process_id, result, heap)
                    .map_err(EnvironmentError::Executor)?;
            }
            Command::_Phantom(_) => {
                // This variant is never actually used, only for maintaining generics
                unreachable!("_Phantom variant should never be constructed")
            }
        }
        Ok(())
    }

    fn handle_action(&mut self, action: Action<E>) -> Result<(), EnvironmentError> {
        match action {
            Action::Spawn {
                caller,
                function_index,
                captures,
                argument,
            } => {
                // Extract heap data from all captures and argument
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

                let (extracted_argument, mut arg_heap) = self
                    .executor
                    .extract_heap_data(&argument)
                    .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                all_heap_data.append(&mut arg_heap);

                self.sender.send(Event::SpawnAction {
                    caller,
                    function_index,
                    captures: extracted_captures,
                    argument: extracted_argument,
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
            Action::RequestEffect { process_id, effect } => {
                // Mark process as effecting - it will be re-queued when effect completes
                self.executor.mark_effecting(process_id);

                // Send effect request to Environment
                self.sender
                    .send(Event::EffectRequest { process_id, effect })?;
            }
        }
        Ok(())
    }

    fn update_program(
        &mut self,
        update: quiver_core::executor::ProgramUpdate,
    ) -> Result<(), EnvironmentError> {
        self.executor.update_program(update);
        Ok(())
    }

    fn start_process(
        &mut self,
        id: ProcessId,
        function_index: Option<usize>,
    ) -> Result<(), EnvironmentError> {
        // Validate function exists (if provided)
        if let Some(idx) = function_index
            && self.executor.get_function(idx).is_none()
        {
            return Err(EnvironmentError::FunctionNotFound(idx));
        }

        // Spawn the persistent process (sleeping if no function)
        self.executor
            .spawn_process(id, function_index, vec![], Value::nil(), vec![], true)
            .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))
    }

    fn spawn_process(
        &mut self,
        id: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        argument: Value,
        heap_data: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        // Validate function exists
        if self.executor.get_function(function_index).is_none() {
            return Err(EnvironmentError::FunctionNotFound(function_index));
        }

        // Delegate to executor which handles heap injection and initialization
        self.executor
            .spawn_process(
                id,
                Some(function_index),
                captures,
                argument,
                heap_data,
                false,
            )
            .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))
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

        // Update the cached function index for REPL references
        self.executor.set_process_function_index(id, function_index);

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

                    let runtime_result = match result {
                        Ok(value) => {
                            let (extracted, heap) = self
                                .executor
                                .extract_heap_data(value)
                                .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                            Ok((extracted, heap))
                        }
                        Err(error) => Err(error.clone()),
                    };

                    results.insert(*target, Some(runtime_result));
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
            if let Some(result) = result_opt {
                self.notify_result(awaiter, awaited, result)?;
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
    ) -> Result<(), EnvironmentError> {
        match result {
            Ok((value, heap)) => {
                // Executor now handles heap injection
                self.executor
                    .notify_result(awaiter, awaited, value, heap)
                    .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
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
        // Executor now handles heap injection
        self.executor
            .notify_message(target, message, heap)
            .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))
    }

    fn get_result(
        &mut self,
        request_id: u64,
        process_id: ProcessId,
        keep_locals: Option<Vec<usize>>,
    ) -> Result<(), EnvironmentError> {
        // Determine completion state up front, so the (mutable) reclaim below doesn't overlap the
        // (immutable) borrow of the result value during heap extraction.
        let process = self
            .executor
            .get_process(process_id)
            .ok_or(EnvironmentError::ProcessNotFound(process_id))?;
        let completed_ok = match &process.result {
            Some(Ok(_)) => true,
            Some(Err(_)) => false,
            None => {
                // Process not done yet - register the request with its keep-set.
                self.pending_result_requests
                    .entry(process_id)
                    .or_default()
                    .push((request_id, keep_locals));
                return Ok(());
            }
        };

        // On success, reclaim the finished line's orphaned locals before reporting the result, so
        // heap inspectors reflect the post-line state. Skipped on error: a partially-executed line
        // may not have populated its kept slots.
        if completed_ok && let Some(keep) = &keep_locals {
            self.executor.release_orphan_locals(process_id, keep);
        }

        let stats = if self.executor.stats.total_instructions() > 0 {
            Some(self.executor.stats.clone())
        } else {
            None
        };
        let process = self.executor.get_process(process_id).unwrap();
        let result = match process.result.as_ref().unwrap() {
            Ok(value) => {
                let (extracted_value, heap) = self
                    .executor
                    .extract_heap_data(value)
                    .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                Ok((extracted_value, heap))
            }
            Err(error) => Err(error.clone()),
        };
        self.sender.send(Event::ResultResponse {
            request_id,
            result,
            stats,
        })?;

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

    fn get_worker_info(&mut self, request_id: u64) -> Result<(), EnvironmentError> {
        let info = self.executor.worker_info();
        self.sender.send(Event::WorkerInfoResponse {
            request_id,
            result: Ok(info),
        })?;
        Ok(())
    }

    fn get_process_types(&mut self, request_id: u64) -> Result<(), EnvironmentError> {
        let function_indices = self.executor.get_process_function_indices();
        self.sender.send(Event::ProcessTypesResponse {
            request_id,
            result: Ok(function_indices),
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
        // Build the kept values (preserving the specific LocalNotFound error on a bad index)...
        let process = self
            .executor
            .get_process(process_id)
            .ok_or(EnvironmentError::ProcessNotFound(process_id))?;

        let mut new_locals = Vec::with_capacity(keep_indices.len());
        for &index in &keep_indices {
            match process.locals.get(index) {
                Some(value) => new_locals.push(value.clone()),
                None => {
                    return Err(EnvironmentError::LocalNotFound { process_id, index });
                }
            }
        }

        // ...then swap them in via the executor so the dropped bindings are released.
        if !self.executor.replace_locals(process_id, new_locals) {
            return Err(EnvironmentError::ProcessNotFound(process_id));
        }
        Ok(())
    }

    fn get_execution_stats(&mut self, request_id: u64) -> Result<(), EnvironmentError> {
        let stats = self.executor.stats.clone();
        self.sender.send(Event::StatsResponse {
            request_id,
            result: Ok(stats),
        })?;
        Ok(())
    }

    fn extract_completed_result(
        &mut self,
        process_id: ProcessId,
    ) -> Result<Option<RuntimeResult>, EnvironmentError> {
        if let Some(process) = self.executor.get_process(process_id)
            && let Some(result) = &process.result
        {
            let extracted = match result {
                Ok(value) => {
                    let (extracted_value, heap) = self
                        .executor
                        .extract_heap_data(value)
                        .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                    Ok((extracted_value, heap))
                }
                Err(error) => Err(error.clone()),
            };
            return Ok(Some(extracted));
        }
        Ok(None)
    }

    fn check_completed_processes(&mut self) -> Result<(), EnvironmentError> {
        // Check awaited processes for completion
        let awaited_pids: Vec<ProcessId> = self.awaited.iter().copied().collect();
        for process_id in awaited_pids {
            if let Some(result) = self.extract_completed_result(process_id)? {
                // Get all awaiters for this process
                if let Some(awaiters) = self.awaiters_for_target.remove(&process_id) {
                    // Send result to each awaiter
                    for awaiter in awaiters {
                        let mut results = HashMap::new();
                        results.insert(process_id, Some(result.clone()));
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
            if let Some(result) = self.extract_completed_result(process_id)?
                && let Some(requests) = self.pending_result_requests.remove(&process_id)
            {
                // On success, reclaim each requester's orphaned locals (idempotent across identical
                // keep-sets) before reporting, mirroring the immediate path in `get_result`.
                if result.is_ok() {
                    for (_, keep_locals) in &requests {
                        if let Some(keep) = keep_locals {
                            self.executor.release_orphan_locals(process_id, keep);
                        }
                    }
                }
                let stats = if self.executor.stats.total_instructions() > 0 {
                    Some(self.executor.stats.clone())
                } else {
                    None
                };
                for (request_id, _) in requests {
                    self.sender.send(Event::ResultResponse {
                        request_id,
                        result: result.clone(),
                        stats: stats.clone(),
                    })?;
                }
            }
        }

        Ok(())
    }
}
