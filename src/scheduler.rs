use crate::builtins::BUILTIN_REGISTRY;
use crate::bytecode::{Constant, Instruction, TypeId};
use crate::program::Program;
use crate::types::TypeLookup;
use crate::vm::{BinaryRef, Error, Value};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::mpsc::{Receiver, SyncSender, sync_channel};
use std::sync::{Arc, Mutex, RwLock};
use std::thread::{self, JoinHandle};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProcessId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum ProcessStatus {
    Running,
    Queued,
    Waiting,
    Sleeping,
    Terminated,
}

#[derive(Debug, Clone)]
pub struct ProcessInfo {
    pub id: ProcessId,
    pub status: ProcessStatus,
    pub stack_size: usize,
    pub locals_size: usize,
    pub frames_count: usize,
    pub mailbox_size: usize,
    pub persistent: bool,
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
        }
    }
}

#[derive(Debug)]
pub struct Scheduler {
    processes: HashMap<ProcessId, Process>,
    queue: VecDeque<ProcessId>,
    waiting: HashSet<ProcessId>,
    active: Option<ProcessId>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            processes: HashMap::new(),
            queue: VecDeque::new(),
            waiting: HashSet::new(),
            active: None,
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

    pub fn get_process_statuses(&self) -> HashMap<ProcessId, ProcessStatus> {
        self.processes
            .iter()
            .map(|(id, process)| {
                let status = if self.active == Some(*id) {
                    ProcessStatus::Running
                } else if self.queue.contains(id) {
                    ProcessStatus::Queued
                } else if self.waiting.contains(id) {
                    ProcessStatus::Waiting
                } else if process.persistent {
                    ProcessStatus::Sleeping
                } else {
                    ProcessStatus::Terminated
                };

                (*id, status)
            })
            .collect()
    }
}

pub enum SchedulerCommand {
    Execute {
        process_id: Option<ProcessId>,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
        response_tx: SyncSender<Result<Option<Value>, Error>>,
    },
    Shutdown,
}

pub struct SchedulerHandle {
    pub scheduler: Arc<Mutex<Scheduler>>,
    command_tx: SyncSender<SchedulerCommand>,
    thread_handle: Option<JoinHandle<()>>,
    next_process_id: Arc<std::sync::atomic::AtomicUsize>,
}

impl std::fmt::Debug for SchedulerHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SchedulerHandle")
            .field("scheduler", &self.scheduler)
            .field("command_tx", &self.command_tx)
            .field("thread_handle", &"<thread handle>")
            .field("next_process_id", &self.next_process_id)
            .finish()
    }
}

impl SchedulerHandle {
    pub fn new(
        program: Arc<RwLock<Program>>,
        next_process_id: Arc<std::sync::atomic::AtomicUsize>,
    ) -> Self {
        let scheduler = Arc::new(Mutex::new(Scheduler::new()));
        let (command_tx, command_rx) = sync_channel(100);

        let scheduler_clone = Arc::clone(&scheduler);
        let next_process_id_clone = Arc::clone(&next_process_id);
        let thread_handle = thread::spawn(move || {
            scheduler_thread(program, scheduler_clone, next_process_id_clone, command_rx);
        });

        Self {
            scheduler,
            command_tx,
            thread_handle: Some(thread_handle),
            next_process_id,
        }
    }

    pub fn execute(
        &self,
        process_id: Option<ProcessId>,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
    ) -> Result<Option<Value>, Error> {
        let (response_tx, response_rx) = sync_channel(1);
        self.command_tx
            .send(SchedulerCommand::Execute {
                process_id,
                instructions,
                parameter,
                response_tx,
            })
            .map_err(|_| Error::InvalidArgument("Scheduler thread died".to_string()))?;

        response_rx
            .recv()
            .map_err(|_| Error::InvalidArgument("Scheduler thread died".to_string()))?
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
    program: Arc<RwLock<Program>>,
    scheduler: Arc<Mutex<Scheduler>>,
    next_process_id: Arc<std::sync::atomic::AtomicUsize>,
    command_rx: Receiver<SchedulerCommand>,
) {
    loop {
        match command_rx.recv() {
            Ok(SchedulerCommand::Execute {
                process_id,
                instructions,
                parameter,
                response_tx,
            }) => {
                let result = execute_in_scheduler(
                    &program,
                    &scheduler,
                    &next_process_id,
                    process_id,
                    instructions,
                    parameter,
                );
                let _ = response_tx.send(result);
            }
            Ok(SchedulerCommand::Shutdown) | Err(_) => {
                break;
            }
        }
    }
}

fn execute_in_scheduler(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    next_process_id: &Arc<std::sync::atomic::AtomicUsize>,
    process_id: Option<ProcessId>,
    instructions: Vec<Instruction>,
    parameter: Option<Value>,
) -> Result<Option<Value>, Error> {
    if instructions.is_empty() {
        return Ok(None);
    }

    // Setup process
    let pid = process_id.unwrap_or_else(|| {
        // This should not be reached - VM should create temp process IDs
        ProcessId(0)
    });

    {
        let mut sched = scheduler.lock().unwrap();
        let process = sched
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument(format!(
                "Process {:?} not found",
                pid
            )))?;

        process.stack.push(parameter.unwrap_or_else(Value::nil));

        let frame = Frame::new(instructions.clone(), 0, 0);
        process.frames.push(frame);

        sched.active = Some(pid);
    }

    let result = run(program, scheduler, next_process_id);

    // Pop frame
    {
        let mut sched = scheduler.lock().unwrap();
        if let Some(process) = sched.get_process_mut(pid) {
            process.frames.pop();
        }
    }

    result
}

fn run(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    next_process_id: &Arc<std::sync::atomic::AtomicUsize>,
) -> Result<Option<Value>, Error> {
    let initial_process = scheduler.lock().unwrap().active;
    let mut initial_result = None;

    loop {
        // Execute instructions for current process
        while let Some(instruction) = get_instruction(scheduler) {
            let result = match instruction {
                Instruction::Constant(index) => handle_constant(program, scheduler, index),
                Instruction::Pop => handle_pop(scheduler),
                Instruction::Duplicate => handle_duplicate(scheduler),
                Instruction::Copy(depth) => handle_copy(scheduler, depth),
                Instruction::Swap => handle_swap(scheduler),
                Instruction::Load(index) => handle_load(scheduler, index),
                Instruction::Store(index) => handle_store(scheduler, index),
                Instruction::Tuple(type_id) => handle_tuple(program, scheduler, type_id),
                Instruction::Get(index) => handle_get(scheduler, index),
                Instruction::IsInteger => handle_is_integer(scheduler),
                Instruction::IsBinary => handle_is_binary(scheduler),
                Instruction::IsTuple(type_id) => handle_is_tuple(program, scheduler, type_id),
                Instruction::Jump(offset) => Ok(handle_jump(scheduler, offset)),
                Instruction::JumpIf(offset) => handle_jump_if(scheduler, offset),
                Instruction::Call => handle_call(program, scheduler),
                Instruction::TailCall(recurse) => handle_tail_call(program, scheduler, recurse),
                Instruction::Return => handle_return(scheduler),
                Instruction::Function(function_index) => {
                    handle_function(program, scheduler, function_index)
                }
                Instruction::Clear(count) => handle_clear(scheduler, count),
                Instruction::Allocate(count) => handle_allocate(scheduler, count),
                Instruction::Builtin(index) => handle_builtin(program, scheduler, index),
                Instruction::Equal(count) => handle_equal(program, scheduler, count),
                Instruction::Not => handle_not(scheduler),
                Instruction::Spawn => handle_spawn(program, scheduler, &next_process_id),
                Instruction::Send => handle_send(scheduler),
                Instruction::Self_ => handle_self(scheduler),
                Instruction::Receive => handle_receive(scheduler),
                Instruction::Acknowledge => handle_acknowledge(scheduler),
            };
            result?;

            // Don't increment counter for Call/TailCall (they manage their own frames)
            // or Receive when process was marked waiting (so it re-executes when woken)
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
            }
        }

        // Current process finished its instructions
        let mut sched = scheduler.lock().unwrap();
        let current_pid = sched.active;

        if current_pid == initial_process && initial_result.is_none() {
            initial_result = sched.get_current_process_mut().and_then(|p| p.stack.pop());
        }

        // Clean up non-persistent processes that have finished
        if let Some(pid) = current_pid {
            let should_remove = sched
                .get_process(pid)
                .map(|p| !p.persistent && p.frames.is_empty())
                .unwrap_or(false);

            if should_remove {
                sched.terminate_process(pid);
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

fn handle_constant(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    index: usize,
) -> Result<(), Error> {
    let prog = program.read().unwrap();
    let constant = prog
        .get_constant(index)
        .ok_or(Error::ConstantUndefined(index))?;
    let value = match constant {
        Constant::Integer(integer) => Value::Integer(*integer),
        Constant::Binary(_) => Value::Binary(BinaryRef::Constant(index)),
    };
    drop(prog);

    let mut sched = scheduler.lock().unwrap();
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

fn handle_copy(scheduler: &Arc<Mutex<Scheduler>>, depth: usize) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    if depth >= process.stack.len() {
        return Err(Error::StackUnderflow);
    }
    let index = process.stack.len() - 1 - depth;
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

fn handle_tuple(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    type_id: TypeId,
) -> Result<(), Error> {
    let prog = program.read().unwrap();
    let (_, fields) = prog
        .lookup_type(&type_id)
        .ok_or_else(|| Error::TypeMismatch {
            expected: "known tuple type".to_string(),
            found: format!("unknown TypeId({:?})", type_id),
        })?;
    let size = fields.len();
    drop(prog);

    let mut sched = scheduler.lock().unwrap();
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

fn handle_is_tuple(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    type_id: TypeId,
) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
    drop(sched);

    let is_match = if let Value::Tuple(actual_type_id, _) = &value {
        if actual_type_id == &type_id {
            // Fast path: exact match
            true
        } else {
            // Use Type::is_compatible for structural checking
            let prog = program.read().unwrap();
            let actual_type = crate::types::Type::Tuple(*actual_type_id);
            let expected_type = crate::types::Type::Tuple(type_id);
            actual_type.is_compatible(&expected_type, &*prog)
        }
    } else {
        false
    };

    let mut sched = scheduler.lock().unwrap();
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

fn handle_call(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
) -> Result<(), Error> {
    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    let function_value = process.stack.pop().ok_or(Error::StackUnderflow)?;

    match function_value {
        Value::Function(function_index, captures) => {
            let parameter = process.stack.pop().ok_or(Error::StackUnderflow)?;
            drop(sched);

            let prog = program.read().unwrap();
            let func = prog
                .get_function(function_index)
                .ok_or(Error::FunctionUndefined(function_index))?;
            let instructions = func.instructions.clone();
            drop(prog);

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
            let parameter = process.stack.pop().ok_or(Error::StackUnderflow)?;
            drop(sched);

            let builtin = BUILTIN_REGISTRY
                .get_implementation(&name)
                .ok_or_else(|| Error::InvalidArgument(format!("Unrecognised builtin: {}", name)))?;

            let prog = program.read().unwrap();
            let result = builtin(&parameter, &*prog)?;
            drop(prog);

            let mut sched = scheduler.lock().unwrap();
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
        _ => Err(Error::TypeMismatch {
            expected: "function".to_string(),
            found: function_value.type_name().to_string(),
        }),
    }
}

fn handle_tail_call(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    recurse: bool,
) -> Result<(), Error> {
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
                drop(sched);

                let prog = program.read().unwrap();
                let func = prog
                    .get_function(function)
                    .ok_or(Error::FunctionUndefined(function))?;
                let instructions = func.instructions.clone();
                drop(prog);

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

fn handle_function(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    function_index: usize,
) -> Result<(), Error> {
    let prog = program.read().unwrap();
    let func = prog
        .get_function(function_index)
        .ok_or(Error::FunctionUndefined(function_index))?;
    let capture_locals = func.captures.clone();
    drop(prog);

    let mut sched = scheduler.lock().unwrap();
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

fn handle_builtin(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    index: usize,
) -> Result<(), Error> {
    let prog = program.read().unwrap();
    let builtin_name = prog
        .get_builtin(index)
        .ok_or(Error::BuiltinUndefined(index))?
        .clone();
    drop(prog);

    let mut sched = scheduler.lock().unwrap();
    let process = sched
        .get_current_process_mut()
        .ok_or(Error::InvalidArgument("No current process".to_string()))?;

    process.stack.push(Value::Builtin(builtin_name));
    Ok(())
}

fn handle_equal(
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    count: usize,
) -> Result<(), Error> {
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

    drop(sched);

    let first = &values[0];
    let all_equal = values
        .iter()
        .all(|value| values_equal(program, first, value));

    let result = if all_equal {
        first.clone()
    } else {
        Value::nil()
    };

    let mut sched = scheduler.lock().unwrap();
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
    program: &Arc<RwLock<Program>>,
    scheduler: &Arc<Mutex<Scheduler>>,
    next_process_id: &Arc<std::sync::atomic::AtomicUsize>,
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

    drop(sched);

    let prog = program.read().unwrap();
    let func = prog
        .get_function(function_index)
        .ok_or(Error::FunctionUndefined(function_index))?;
    let instructions = func.instructions.clone();
    drop(prog);

    let new_pid = ProcessId(next_process_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst));

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

fn values_equal(program: &Arc<RwLock<Program>>, a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Binary(a), Value::Binary(b)) => {
            let prog = program.read().unwrap();
            match (prog.get_binary_bytes(a), prog.get_binary_bytes(b)) {
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
                    .all(|(a, b)| values_equal(program, a, b))
        }
        (Value::Function(idx_a, caps_a), Value::Function(idx_b, caps_b)) => {
            idx_a == idx_b
                && caps_a.len() == caps_b.len()
                && caps_a
                    .iter()
                    .zip(caps_b.iter())
                    .all(|(a, b)| values_equal(program, a, b))
        }
        (Value::Builtin(a), Value::Builtin(b)) => a == b,
        (Value::Pid(a), Value::Pid(b)) => a == b,
        _ => false,
    }
}
