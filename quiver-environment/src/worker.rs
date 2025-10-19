use crate::environment::EnvironmentError;
use crate::messages::{Command, Event};
use crate::transport::{CommandReceiver, EventSender};
use quiver_core::bytecode::{Constant, Function};
use quiver_core::executor::Executor;
use quiver_core::process::{Action, Frame, ProcessId, ProcessStatus};
use quiver_core::types::TupleTypeInfo;
use quiver_core::value::Value;
use std::collections::HashSet;

const MAX_STEP_UNITS: usize = 1000;

pub struct Worker<R: CommandReceiver, S: EventSender> {
    executor: Executor,
    awaited: HashSet<ProcessId>,
    receiver: R,
    sender: S,
}

impl<R: CommandReceiver, S: EventSender> Worker<R, S> {
    pub fn new(receiver: R, sender: S) -> Self {
        Self {
            executor: Executor::new(),
            awaited: HashSet::new(),
            receiver,
            sender,
        }
    }

    /// Process one iteration of the worker loop
    /// Returns true if work was done, false if idle
    pub fn step(&mut self) -> Result<bool, EnvironmentError> {
        let mut did_work = false;

        // Process commands (non-blocking)
        while let Some(cmd) = self.receiver.try_recv()? {
            self.handle_command(cmd)?;
            did_work = true;
        }

        // Execute one step
        if let Some(action) = self.executor.step(MAX_STEP_UNITS) {
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
            Command::StartProcess {
                id,
                function_index,
                captures,
                heap_data,
                persistent,
            } => {
                self.start_process(id, function_index, captures, heap_data, persistent)?;
            }
            Command::ResumeProcess { id, function_index } => {
                self.resume_process(id, function_index)?;
            }
            Command::RegisterAwaiter { target, awaiter: _ } => {
                // Check if target is already completed
                let target_status = self
                    .executor
                    .get_process_info(target)
                    .map(|info| info.status);
                let is_completed = matches!(
                    target_status,
                    Some(ProcessStatus::Completed) | Some(ProcessStatus::Sleeping)
                );

                if is_completed {
                    // Process already completed - send completion event immediately
                    if let Some(process) = self.executor.get_process(target) {
                        if let Some(result) = &process.result {
                            let (extracted_result, heap) = match result {
                                Ok(value) => {
                                    let (extracted, heap) =
                                        self.executor.extract_heap_data(value).map_err(|e| {
                                            EnvironmentError::HeapData(format!("{:?}", e))
                                        })?;
                                    (Ok(extracted), heap)
                                }
                                Err(error) => (Err(error.clone()), vec![]),
                            };

                            self.sender.send(Event::Completed {
                                process_id: target,
                                result: extracted_result,
                                heap,
                            })?;
                        }
                    }
                } else {
                    // Process not yet completed - mark as awaited
                    self.awaited.insert(target);
                }
            }
            Command::NotifyResult {
                process_id,
                result,
                heap,
            } => {
                self.notify_result(process_id, result, heap)?;
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
            } => {
                self.executor
                    .notify_spawn(process_id, Value::Pid(spawned_pid));
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
            Action::AwaitResult { caller, target } => {
                self.sender.send(Event::AwaitAction { caller, target })?;
            }
        }
        Ok(())
    }

    fn update_program(
        &mut self,
        constants: Vec<Constant>,
        functions: Vec<Function>,
        types: Vec<TupleTypeInfo>,
        builtins: Vec<String>,
    ) -> Result<(), EnvironmentError> {
        self.executor
            .update_program(constants, functions, types, builtins);

        Ok(())
    }

    fn start_process(
        &mut self,
        id: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
        persistent: bool,
    ) -> Result<(), EnvironmentError> {
        // Get function instructions
        let instructions = self
            .executor
            .get_function(function_index)
            .ok_or_else(|| EnvironmentError::FunctionNotFound(function_index))?
            .instructions
            .clone();

        // Spawn the process
        self.executor.spawn_process(id, persistent);

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
                .ok_or_else(|| EnvironmentError::ProcessNotFound(id))?;
            process.locals.push(injected);
        }

        // Push nil parameter onto stack
        let process = self
            .executor
            .get_process_mut(id)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(id))?;
        process.stack.push(quiver_core::value::Value::nil());

        // Push initial frame with instructions
        let process = self
            .executor
            .get_process_mut(id)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(id))?;

        process.frames.push(quiver_core::process::Frame::new(
            instructions,
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
        // Get function instructions
        let instructions = self
            .executor
            .get_function(function_index)
            .ok_or_else(|| EnvironmentError::FunctionNotFound(function_index))?
            .instructions
            .clone();

        let process = self
            .executor
            .get_process_mut(id)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(id))?;

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
        process.frames.push(Frame::new(instructions, 0, 0));

        // Add back to queue
        self.executor.add_to_queue(id);

        Ok(())
    }

    fn notify_result(
        &mut self,
        process_id: ProcessId,
        result: Result<Value, quiver_core::error::Error>,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        match result {
            Ok(value) => {
                let injected = self
                    .executor
                    .inject_heap_data(value, &heap)
                    .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;

                self.executor.notify_result(process_id, injected);
            }
            Err(error) => {
                // Set the process result to the error and clear frames to complete it
                if let Some(process) = self.executor.get_process_mut(process_id) {
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
        let result = match self.executor.get_process(process_id) {
            Some(process) => match &process.result {
                Some(Ok(value)) => {
                    let (extracted_value, heap) = self
                        .executor
                        .extract_heap_data(value)
                        .map_err(|e| EnvironmentError::HeapData(format!("{:?}", e)))?;
                    Ok(Some((extracted_value, heap)))
                }
                Some(Err(error)) => Err(error.clone()),
                None => Ok(None),
            },
            None => {
                // Process not found - this shouldn't happen in normal operation
                return Err(EnvironmentError::ProcessNotFound(process_id));
            }
        };

        self.sender
            .send(Event::ResultResponse { request_id, result })?;
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
                            return Ok(self.sender.send(Event::LocalsResponse {
                                request_id,
                                result: Err(EnvironmentError::LocalNotFound { process_id, index }),
                            })?);
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
            .ok_or_else(|| EnvironmentError::ProcessNotFound(process_id))?;

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

    fn check_completed_processes(&mut self) -> Result<(), EnvironmentError> {
        // Get all process statuses
        let statuses = self.executor.get_process_statuses();

        // Find newly completed processes that are being awaited
        let mut completed = Vec::new();
        for (&process_id, status) in &statuses {
            // If terminated (non-persistent) or sleeping (persistent), it's completed
            let is_completed = matches!(status, ProcessStatus::Completed | ProcessStatus::Sleeping);

            // Only process if completed and being awaited
            if is_completed && self.awaited.contains(&process_id) {
                if let Some(process) = self.executor.get_process(process_id) {
                    if let Some(result) = &process.result {
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

                        completed.push((process_id, extracted_result, heap));
                    }
                }
            }
        }

        // Send completion events and remove from awaited set
        for (process_id, result, heap) in completed {
            self.sender.send(Event::Completed {
                process_id,
                result,
                heap,
            })?;
            self.awaited.remove(&process_id);
        }

        Ok(())
    }
}
