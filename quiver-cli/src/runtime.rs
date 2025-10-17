use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{Action, Frame, ProcessId, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::value::Value;
use quiver_environment::runtime::{CommandSender, Event, SchedulerCommand};
use std::collections::HashMap;
use std::sync::mpsc::{Receiver, RecvTimeoutError, SyncSender, TryRecvError, sync_channel};
use std::thread::{self, JoinHandle};
use std::time::Duration;

struct ExecutorHandle {
    command_tx: SyncSender<SchedulerCommand>,
    event_rx: Receiver<Event>,
    thread_handle: JoinHandle<()>,
}

/// A cloneable command sender for the native runtime.
/// Holds clones of all executor command channels.
#[derive(Clone)]
pub struct NativeCommandSender {
    command_txs: Vec<SyncSender<SchedulerCommand>>,
}

impl CommandSender for NativeCommandSender {
    fn send_command(&mut self, executor_id: usize, command: SchedulerCommand) -> Result<(), Error> {
        if executor_id < self.command_txs.len() {
            self.command_txs[executor_id]
                .send(command)
                .map_err(|_| Error::InvalidArgument("Executor thread died".to_string()))
        } else {
            Err(Error::InvalidArgument(format!(
                "Invalid executor_id: {}",
                executor_id
            )))
        }
    }
}

pub struct NativeRuntime {
    executors: Vec<Option<ExecutorHandle>>,
}

impl NativeRuntime {
    pub fn new() -> Self {
        Self {
            executors: Vec::new(),
        }
    }

    /// Get a cloneable command sender for this runtime.
    /// Should be called after starting all executors.
    pub fn command_sender(&self) -> NativeCommandSender {
        let command_txs = self
            .executors
            .iter()
            .filter_map(|executor| executor.as_ref().map(|h| h.command_tx.clone()))
            .collect();

        NativeCommandSender { command_txs }
    }

    /// Start an executor with the given program.
    /// Returns the executor ID.
    pub fn start_executor(
        &mut self,
        program: &Program,
    ) -> Result<usize, Error> {
        let executor_id = self.executors.len();

        let (command_tx, command_rx) = sync_channel(100);
        let (event_tx, event_rx) = sync_channel(100);
        let executor = Executor::new(program);

        let thread_handle = thread::spawn(move || {
            scheduler_thread(executor, command_rx, event_tx);
        });

        let handle = ExecutorHandle {
            command_tx,
            event_rx,
            thread_handle,
        };

        self.executors.push(Some(handle));

        Ok(executor_id)
    }

    /// Poll for events from all executors. Returns collected events.
    /// This method is specific to native runtime and moves events from worker threads to the main thread.
    pub fn poll(&mut self) -> Vec<Event> {
        let mut events = Vec::new();
        for executor in &self.executors {
            if let Some(handle) = executor {
                while let Ok(event) = handle.event_rx.try_recv() {
                    events.push(event);
                }
            }
        }
        events
    }

    /// Stop a specific executor.
    pub fn stop_executor(&mut self, executor_id: usize) -> Result<(), Error> {
        if executor_id >= self.executors.len() {
            return Err(Error::InvalidArgument(format!(
                "Invalid executor_id: {}",
                executor_id
            )));
        }

        if let Some(handle) = self.executors[executor_id].take() {
            let _ = handle.command_tx.send(SchedulerCommand::Shutdown);
            let _ = handle.thread_handle.join();
        }

        Ok(())
    }
}

fn scheduler_thread(
    mut executor: Executor,
    command_rx: Receiver<SchedulerCommand>,
    event_tx: SyncSender<Event>,
) {
    let mut pending_results: HashMap<u64, ProcessId> = HashMap::new();
    let mut process_errors: HashMap<ProcessId, Error> = HashMap::new();
    let mut completion_sent: std::collections::HashSet<ProcessId> =
        std::collections::HashSet::new();

    loop {
        let is_idle = !executor
            .get_process_statuses()
            .values()
            .any(|s| *s == ProcessStatus::Active);

        let command_result = if is_idle {
            command_rx
                .recv_timeout(Duration::from_millis(100))
                .map_err(|e| match e {
                    RecvTimeoutError::Timeout => TryRecvError::Empty,
                    RecvTimeoutError::Disconnected => TryRecvError::Disconnected,
                })
        } else {
            command_rx.try_recv()
        };

        match command_result {
            Ok(SchedulerCommand::Execute {
                request_id,
                process_id,
                instructions,
                persistent,
            }) => {
                executor.spawn_process(process_id, persistent);

                let result = if let Some(process) = executor.get_process_mut(process_id) {
                    process.stack.push(Value::nil());

                    if instructions.is_empty() {
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
            Ok(SchedulerCommand::UpdateProgram {
                constants,
                functions,
                builtins,
                types,
            }) => {
                for constant in constants {
                    executor.append_constant(constant);
                }
                for function in functions {
                    executor.append_function(function);
                }
                for builtin in builtins {
                    executor.append_builtin(builtin);
                }
                for (type_id, info) in types {
                    executor.register_type(type_id, info);
                }
            }
            Ok(SchedulerCommand::Wake {
                request_id,
                process_id,
                instructions,
            }) => {
                let result = match executor.get_process_mut(process_id) {
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
                pending_results.insert(request_id, process_id);
            }
            Ok(SchedulerCommand::CompactLocals {
                request_id,
                process_id,
                referenced_indices,
            }) => {
                let result = match executor.get_process_mut(process_id) {
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
                let _ = event_tx.send(Event::CompactLocalsResponse { request_id, result });
            }
            Ok(SchedulerCommand::NotifySpawn {
                process_id,
                pid_value,
                heap_data,
            }) => {
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
                let injected_captures: Vec<Value> = captures
                    .into_iter()
                    .map(|capture| {
                        executor
                            .inject_heap_data(capture, &heap_data)
                            .unwrap_or_else(|_| Value::nil())
                    })
                    .collect();

                executor.spawn_process(process_id, false);

                if let Some(process) = executor.get_process_mut(process_id) {
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

                executor.add_to_queue(process_id);
            }
            Ok(SchedulerCommand::Shutdown) => {
                break;
            }
            Err(TryRecvError::Empty) => {}
            Err(TryRecvError::Disconnected) => {
                break;
            }
        }

        let step_result = executor.step(1000);

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
                                executor.extract_heap_data(&capture)?;
                            caps.push(converted);
                            all_heap_data.append(&mut capture_heap_data);
                            Ok::<_, Error>((caps, all_heap_data))
                        },
                    )
                    .unwrap_or_else(|_| (Vec::new(), Vec::new()));

                let _ = event_tx.send(Event::SpawnRequested {
                    caller,
                    function_index,
                    captures: converted_captures,
                    heap_data,
                });
            }
            Ok(Some(Action::Deliver { target, value })) => {
                let (converted_value, heap_data) = executor
                    .extract_heap_data(&value)
                    .unwrap_or_else(|_| (value.clone(), Vec::new()));

                let _ = event_tx.send(Event::DeliverMessage {
                    target,
                    value: converted_value,
                    heap_data,
                });
            }
            Ok(Some(Action::AwaitResult { target, caller })) => {
                let _ = event_tx.send(Event::AwaitResult { target, caller });
            }
            Err(error) => {
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

                for pid in failed_processes {
                    process_errors.insert(pid, error.clone());
                }
            }
        }

        check_pending_results(
            &mut executor,
            &mut pending_results,
            &event_tx,
            &process_errors,
            &mut completion_sent,
        );

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
                            if !completion_sent.contains(&process_id) {
                                newly_completed.push((process_id, val));
                                completion_sent.insert(process_id);
                            }
                        }
                    }
                    match value {
                        Some(v) => match executor.extract_heap_data(&v) {
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

                let _ = event_tx.send(Event::GetResultResponse {
                    request_id,
                    result,
                    heap_data,
                });
                completed.push(request_id);
            } else if frame_exhausted && process.frames.is_empty() {
                let (result, heap_data) = match process.result.clone() {
                    Some(v) => match executor.extract_heap_data(&v) {
                        Ok((converted, heap_data)) => (Ok(converted), heap_data),
                        Err(e) => (Err(e), vec![]),
                    },
                    None => {
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

    for request_id in completed {
        pending_results.remove(&request_id);
    }

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
