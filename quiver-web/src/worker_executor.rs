use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{Action, Frame, ProcessId, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::value::Value;
use quiver_environment::runtime::{Event, SchedulerCommand};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

/// Executor that runs inside a Web Worker
/// Similar to scheduler_thread in NativeRuntime but for WASM
#[wasm_bindgen]
pub struct WorkerExecutor {
    executor: Executor,
    pending_results: HashMap<u64, ProcessId>,
    process_errors: HashMap<ProcessId, Error>,
    completion_sent: std::collections::HashSet<ProcessId>,
}

#[wasm_bindgen]
impl WorkerExecutor {
    #[wasm_bindgen(constructor)]
    pub fn new(program_data: JsValue) -> Result<WorkerExecutor, JsValue> {
        let bytecode: quiver_core::bytecode::Bytecode =
            serde_wasm_bindgen::from_value(program_data).map_err(|e| {
                JsValue::from_str(&format!("Failed to deserialize bytecode: {}", e))
            })?;

        let program = Program::from_bytecode(bytecode);

        Ok(Self {
            executor: Executor::new(&program),
            pending_results: HashMap::new(),
            process_errors: HashMap::new(),
            completion_sent: std::collections::HashSet::new(),
        })
    }

    /// Process a command and return events (may return multiple events)
    #[wasm_bindgen]
    pub fn process_command(&mut self, command_data: JsValue) -> Result<JsValue, JsValue> {
        let command: SchedulerCommand = serde_wasm_bindgen::from_value(command_data)
            .map_err(|e| JsValue::from_str(&format!("Failed to deserialize command: {}", e)))?;

        let mut events = Vec::new();

        // Process the command
        match command {
            SchedulerCommand::Execute {
                request_id,
                process_id,
                instructions,
                persistent,
            } => {
                self.executor.spawn_process(process_id, persistent);

                let result = if let Some(process) = self.executor.get_process_mut(process_id) {
                    process.stack.push(Value::nil());

                    if instructions.is_empty() {
                        process.result = Some(Value::nil());
                        Ok(process_id)
                    } else {
                        process.frames.push(Frame::new(instructions, 0, 0));
                        self.executor.add_to_queue(process_id);
                        Ok(process_id)
                    }
                } else {
                    Err(Error::InvalidArgument(format!(
                        "Process {:?} not found after spawning",
                        process_id
                    )))
                };

                events.push(Event::ExecuteResponse { request_id, result });
            }
            SchedulerCommand::UpdateProgram {
                constants,
                functions,
                builtins,
                types,
            } => {
                for constant in constants {
                    self.executor.append_constant(constant);
                }
                for function in functions {
                    self.executor.append_function(function);
                }
                for builtin in builtins {
                    self.executor.append_builtin(builtin);
                }
                for (type_id, info) in types {
                    self.executor.register_type(type_id, info);
                }
            }
            SchedulerCommand::Wake {
                request_id,
                process_id,
                instructions,
            } => {
                let result = match self.executor.get_process_mut(process_id) {
                    Some(process) => {
                        if !process.persistent {
                            Err(Error::InvalidArgument(format!(
                                "Cannot wake non-persistent process {:?}",
                                process_id
                            )))
                        } else if !process.frames.is_empty() {
                            Err(Error::InvalidArgument(format!(
                                "Cannot wake process {:?} - not idle (has {} frames)",
                                process_id,
                                process.frames.len()
                            )))
                        } else {
                            let initial_value = process.result.take().unwrap_or_else(Value::nil);
                            process.stack.push(initial_value);
                            process.frames.push(Frame::new(instructions, 0, 0));
                            self.executor.add_to_queue(process_id);
                            Ok(())
                        }
                    }
                    None => Err(Error::InvalidArgument(format!(
                        "Process {:?} not found",
                        process_id
                    ))),
                };

                events.push(Event::WakeResponse { request_id, result });
            }
            SchedulerCommand::GetProcesses { request_id } => {
                let statuses = self.executor.get_process_statuses();
                events.push(Event::GetProcessesResponse {
                    request_id,
                    result: Ok(statuses),
                });
            }
            SchedulerCommand::GetProcess { request_id, id } => {
                let info = self.executor.get_process_info(id);
                events.push(Event::GetProcessResponse {
                    request_id,
                    result: Ok(info),
                });
            }
            SchedulerCommand::GetLocals {
                request_id,
                process_id,
                indices,
            } => {
                let result = match self.executor.get_process(process_id) {
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
                events.push(Event::GetLocalsResponse { request_id, result });
            }
            SchedulerCommand::GetResult {
                request_id,
                process_id,
            } => {
                self.pending_results.insert(request_id, process_id);
            }
            SchedulerCommand::CompactLocals {
                request_id,
                process_id,
                referenced_indices,
            } => {
                let result = match self.executor.get_process_mut(process_id) {
                    Some(process) => {
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
                events.push(Event::CompactLocalsResponse { request_id, result });
            }
            SchedulerCommand::NotifySpawn {
                process_id,
                pid_value,
                heap_data,
            } => {
                let value = self
                    .executor
                    .inject_heap_data(pid_value, &heap_data)
                    .unwrap_or_else(|_| Value::nil());
                self.executor.notify_spawn(process_id, value);
            }
            SchedulerCommand::NotifyMessage {
                process_id,
                message,
                heap_data,
            } => {
                let value = self
                    .executor
                    .inject_heap_data(message, &heap_data)
                    .unwrap_or_else(|_| Value::nil());
                self.executor.notify_message(process_id, value);
            }
            SchedulerCommand::NotifyResult {
                process_id,
                result,
                heap_data,
            } => {
                let value = self
                    .executor
                    .inject_heap_data(result, &heap_data)
                    .unwrap_or_else(|_| Value::nil());
                self.executor.notify_result(process_id, value);
            }
            SchedulerCommand::Spawn {
                process_id,
                function_index,
                captures,
                heap_data,
            } => {
                let injected_captures: Vec<Value> = captures
                    .into_iter()
                    .map(|capture| {
                        self.executor
                            .inject_heap_data(capture, &heap_data)
                            .unwrap_or_else(|_| Value::nil())
                    })
                    .collect();

                self.executor.spawn_process(process_id, false);

                if let Some(process) = self.executor.get_process_mut(process_id) {
                    process.stack.push(Value::nil());
                    process
                        .stack
                        .push(Value::Function(function_index, injected_captures));
                    process.frames.push(Frame::new(
                        vec![quiver_core::bytecode::Instruction::Call],
                        0,
                        0,
                    ));
                }

                self.executor.add_to_queue(process_id);
            }
            SchedulerCommand::Shutdown => {}
        }

        // Step the executor and collect additional events
        self.step_executor(&mut events);

        // Serialize all events
        serde_wasm_bindgen::to_value(&events)
            .map_err(|e| JsValue::from_str(&format!("Failed to serialize events: {}", e)))
    }

    fn step_executor(&mut self, events: &mut Vec<Event>) {
        // Step until idle or max iterations
        for _ in 0..1000 {
            let step_result = self.executor.step(100);

            match step_result {
                Ok(None) => {}
                Ok(Some(Action::Spawn {
                    caller,
                    function_index,
                    captures,
                })) => {
                    let (converted_captures, heap_data) = captures
                        .into_iter()
                        .try_fold(
                            (Vec::new(), Vec::new()),
                            |(mut caps, mut all_heap_data), capture| {
                                let (converted, mut capture_heap_data) =
                                    self.executor.extract_heap_data(&capture)?;
                                caps.push(converted);
                                all_heap_data.append(&mut capture_heap_data);
                                Ok::<_, Error>((caps, all_heap_data))
                            },
                        )
                        .unwrap_or_else(|_| (Vec::new(), Vec::new()));

                    events.push(Event::SpawnRequested {
                        caller,
                        function_index,
                        captures: converted_captures,
                        heap_data,
                    });
                }
                Ok(Some(Action::Deliver { target, value })) => {
                    let (converted_value, heap_data) = self
                        .executor
                        .extract_heap_data(&value)
                        .unwrap_or_else(|_| (value.clone(), Vec::new()));

                    events.push(Event::DeliverMessage {
                        target,
                        value: converted_value,
                        heap_data,
                    });
                }
                Ok(Some(Action::AwaitResult { target, caller })) => {
                    events.push(Event::AwaitResult { target, caller });
                }
                Err(error) => {
                    let failed_processes: Vec<ProcessId> = self
                        .executor
                        .get_process_statuses()
                        .keys()
                        .filter(|&&pid| {
                            if let Some(info) = self.executor.get_process_info(pid) {
                                info.frames_count == 0 && info.result.is_none() && info.persistent
                            } else {
                                false
                            }
                        })
                        .copied()
                        .collect();

                    for pid in failed_processes {
                        self.process_errors.insert(pid, error.clone());
                    }
                }
            }

            // Check pending results and completed processes
            self.check_pending_results(events);
            self.check_completed_processes(events);

            // Break if executor is idle
            let is_idle = !self
                .executor
                .get_process_statuses()
                .values()
                .any(|s| *s == ProcessStatus::Active);

            if is_idle {
                break;
            }
        }
    }

    fn check_pending_results(&mut self, events: &mut Vec<Event>) {
        let mut completed = Vec::new();
        let mut newly_completed = Vec::new();

        for (&request_id, &process_id) in self.pending_results.iter() {
            if let Some(process) = self.executor.get_process_mut(process_id) {
                let frame_exhausted = process
                    .frames
                    .last()
                    .map_or(true, |frame| frame.counter >= frame.instructions.len());

                if frame_exhausted && !process.frames.is_empty() {
                    process.frames.pop();

                    let (result, heap_data) = if !process.stack.is_empty() {
                        let value = process.stack.last().cloned();
                        if process.frames.is_empty() {
                            process.result = value.clone();
                            if let Some(val) = value.clone() {
                                if !self.completion_sent.contains(&process_id) {
                                    newly_completed.push((process_id, val));
                                    self.completion_sent.insert(process_id);
                                }
                            }
                        }
                        match value {
                            Some(v) => match self.executor.extract_heap_data(&v) {
                                Ok((converted, heap_data)) => (Ok(converted), heap_data),
                                Err(e) => (Err(e), vec![]),
                            },
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

                    events.push(Event::GetResultResponse {
                        request_id,
                        result,
                        heap_data,
                    });
                    completed.push(request_id);
                } else if frame_exhausted && process.frames.is_empty() {
                    let (result, heap_data) = match process.result.clone() {
                        Some(v) => match self.executor.extract_heap_data(&v) {
                            Ok((converted, heap_data)) => (Ok(converted), heap_data),
                            Err(e) => (Err(e), vec![]),
                        },
                        None => {
                            let error = self
                                .process_errors
                                .get(&process_id)
                                .cloned()
                                .unwrap_or_else(|| {
                                    Error::InvalidArgument(format!(
                                        "Process {:?} has no result",
                                        process_id
                                    ))
                                });
                            (Err(error), vec![])
                        }
                    };

                    events.push(Event::GetResultResponse {
                        request_id,
                        result,
                        heap_data,
                    });
                    completed.push(request_id);
                }
            }
        }

        for request_id in completed {
            self.pending_results.remove(&request_id);
        }

        for (completed_pid, result) in newly_completed {
            let (converted_result, heap_data) = self
                .executor
                .extract_heap_data(&result)
                .unwrap_or_else(|_| (result.clone(), Vec::new()));

            events.push(Event::ProcessCompleted {
                process_id: completed_pid,
                result: converted_result,
                heap_data,
            });
        }
    }

    fn check_completed_processes(&mut self, events: &mut Vec<Event>) {
        let statuses = self.executor.get_process_statuses();
        let mut newly_completed = Vec::new();

        for (process_id, status) in statuses {
            if matches!(status, ProcessStatus::Terminated | ProcessStatus::Sleeping) {
                if !self.completion_sent.contains(&process_id) {
                    if let Some(process) = self.executor.get_process(process_id) {
                        if let Some(result) = &process.result {
                            newly_completed.push((process_id, result.clone()));
                            self.completion_sent.insert(process_id);
                        }
                    }
                }
            }
        }

        for (process_id, result) in newly_completed {
            let (converted_result, heap_data) = self
                .executor
                .extract_heap_data(&result)
                .unwrap_or_else(|_| (result.clone(), Vec::new()));

            events.push(Event::ProcessCompleted {
                process_id,
                result: converted_result,
                heap_data,
            });
        }
    }
}
