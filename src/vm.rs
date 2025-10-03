use crate::builtins::BUILTIN_REGISTRY;
use crate::bytecode::{Constant, Instruction, TypeId};
use crate::program::Program;
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

/// Maximum binary size in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

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

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryRef {
    /// Reference to a binary stored in the constants table
    Constant(usize),
    /// Reference-counted heap-allocated binary
    Heap(Rc<Vec<u8>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Binary(BinaryRef),
    Tuple(TypeId, Vec<Value>),
    Function(usize, Vec<Value>),
    Builtin(String),
    Pid(ProcessId),
}

impl Value {
    /// Create a NIL tuple value
    pub fn nil() -> Self {
        Value::Tuple(TypeId::NIL, vec![])
    }

    /// Create an OK tuple value
    pub fn ok() -> Self {
        Value::Tuple(TypeId::OK, vec![])
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Binary(_) => "binary",
            Value::Tuple(_, _) => "tuple",
            Value::Function(_, _) => "function",
            Value::Builtin(_) => "builtin",
            Value::Pid(_) => "pid",
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    // Stack operation errors
    StackUnderflow,

    // Function and call errors
    CallInvalid,
    FunctionUndefined(usize),
    BuiltinUndefined(usize),
    FrameUnderflow,

    // Variable and constant access errors
    VariableUndefined(String),
    ConstantUndefined(usize),

    // Data access errors
    FieldAccessInvalid(usize),

    // Type system errors
    TypeMismatch { expected: String, found: String },
    ArityMismatch { expected: usize, found: usize },
    InvalidArgument(String),

    // Tuple and structure errors
    TupleEmpty,

    // Scope management errors
    ScopeCountInvalid { expected: usize, found: usize },
    ScopeUnderflow,
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
    pub current_message: Option<Value>,
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
            current_message: None,
        }
    }
}

#[derive(Debug)]
pub struct Scheduler {
    processes: HashMap<ProcessId, Process>,
    queue: VecDeque<ProcessId>,
    waiting: HashSet<ProcessId>,
    active: Option<ProcessId>,
    next_id: usize,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            processes: HashMap::new(),
            queue: VecDeque::new(),
            waiting: HashSet::new(),
            active: None,
            next_id: 0,
        }
    }

    pub fn spawn_process(&mut self, persistent: bool) -> ProcessId {
        let id = ProcessId(self.next_id);
        self.next_id += 1;

        let process = Process::new(id, persistent);
        self.processes.insert(id, process);
        self.queue.push_back(id);

        id
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
        // Remove from run queue if present
        self.queue.retain(|&pid| pid != id);
    }

    pub fn wake_process(&mut self, id: ProcessId) {
        // Remove from waiting queue and add to run queue
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

#[derive(Debug)]
pub struct VM {
    program: Program,
    scheduler: Scheduler,
}

impl TypeLookup for VM {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.program.lookup_type(type_id)
    }
}

impl VM {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            scheduler: Scheduler::new(),
        }
    }

    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn program_mut(&mut self) -> &mut Program {
        &mut self.program
    }

    pub fn spawn_process(&mut self, persistent: bool) -> ProcessId {
        self.scheduler.spawn_process(persistent)
    }

    pub fn get_process(&self, id: ProcessId) -> Option<&Process> {
        self.scheduler.get_process(id)
    }

    pub fn get_process_mut(&mut self, id: ProcessId) -> Option<&mut Process> {
        self.scheduler.get_process_mut(id)
    }

    /// Create a new heap-allocated binary
    pub fn create_heap_binary(&mut self, bytes: Vec<u8>) -> Result<BinaryRef, Error> {
        if bytes.len() > MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                bytes.len(),
                MAX_BINARY_SIZE
            )));
        }
        Ok(BinaryRef::Heap(Rc::new(bytes)))
    }

    /// Clone a binary reference (cheap operation)
    pub fn clone_binary(&self, binary_ref: &BinaryRef) -> BinaryRef {
        binary_ref.clone()
    }

    pub fn execute_instructions(
        &mut self,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
        process_id: Option<ProcessId>,
    ) -> Result<Option<Value>, Error> {
        if instructions.is_empty() {
            return Ok(None);
        }

        // Get or create process
        let pid = if let Some(id) = process_id {
            id
        } else {
            // Create a temporary non-persistent process
            self.scheduler.spawn_process(false)
        };

        // Get process and push parameter if provided
        let process = self
            .scheduler
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument(format!(
                "Process {:?} not found",
                pid
            )))?;

        if let Some(param) = parameter {
            process.stack.push(param);
        }

        // For persistent processes with no active frames, reuse the existing locals space
        // (this enables REPL variable persistence across evaluations)
        let locals_base = if process.persistent && process.frames.is_empty() {
            0
        } else {
            process.locals.len()
        };
        let frame = Frame::new(instructions.clone(), locals_base, 0);
        process.frames.push(frame);

        // Set as current process and run
        self.scheduler.active = Some(pid);
        let result = self.run();

        // Pop frame
        if let Some(process) = self.scheduler.get_process_mut(pid) {
            process.frames.pop();
        }

        result
    }

    pub fn execute_function(&mut self, entry: usize) -> Result<Option<Value>, Error> {
        let function = self
            .program
            .get_function(entry)
            .ok_or(Error::FunctionUndefined(entry))?;
        let instructions = function.instructions.clone();

        // Create a temporary non-persistent process
        let pid = self.scheduler.spawn_process(false);

        let process = self
            .scheduler
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument(format!(
                "Process {:?} not found",
                pid
            )))?;

        process.stack.push(Value::nil());
        process.frames.push(Frame::new(instructions, 0, 0));

        self.scheduler.active = Some(pid);
        self.run()
    }

    fn run(&mut self) -> Result<Option<Value>, Error> {
        // Keep track of initial process to return its result
        let initial_process = self.scheduler.active;
        let mut initial_result = None;

        loop {
            // Execute instructions for current process
            while let Some(instruction) = self.get_instruction() {
                match instruction {
                    Instruction::Constant(index) => self.handle_constant(index)?,
                    Instruction::Pop => self.handle_pop()?,
                    Instruction::Duplicate => self.handle_duplicate()?,
                    Instruction::Copy(depth) => self.handle_copy(depth)?,
                    Instruction::Swap => self.handle_swap()?,
                    Instruction::Load(index) => self.handle_load(index)?,
                    Instruction::Store(index) => self.handle_store(index)?,
                    Instruction::Tuple(type_id) => self.handle_tuple(type_id)?,
                    Instruction::Get(index) => self.handle_get(index)?,
                    Instruction::IsInteger => self.handle_is_integer()?,
                    Instruction::IsBinary => self.handle_is_binary()?,
                    Instruction::IsTuple(type_id) => self.handle_is_tuple(type_id)?,
                    Instruction::Jump(offset) => self.handle_jump(offset)?,
                    Instruction::JumpIf(offset) => self.handle_jump_if(offset)?,
                    Instruction::Call => self.handle_call()?,
                    Instruction::TailCall(recurse) => self.handle_tail_call(recurse)?,
                    Instruction::Return => self.handle_return()?,
                    Instruction::Function(function_index) => {
                        self.handle_function(function_index)?
                    }
                    Instruction::Clear(count) => self.handle_clear(count)?,
                    Instruction::Allocate(count) => self.handle_allocate(count)?,
                    Instruction::Builtin(index) => self.handle_builtin(index)?,
                    Instruction::Equal(count) => self.handle_equal(count)?,
                    Instruction::Not => self.handle_not()?,
                    Instruction::Spawn => self.handle_spawn()?,
                    Instruction::Send => self.handle_send()?,
                    Instruction::Self_ => self.handle_self()?,
                    Instruction::Receive => self.handle_receive()?,
                    Instruction::Acknowledge => self.handle_acknowledge()?,
                }
                if !matches!(instruction, Instruction::Call | Instruction::TailCall(_)) {
                    if let Some(process) = self.scheduler.get_current_process_mut() {
                        if let Some(frame) = process.frames.last_mut() {
                            frame.counter += 1;
                        }
                    }
                }
            }

            // Current process finished its instructions
            // If this was the initial process, save its result
            if self.scheduler.active == initial_process && initial_result.is_none() {
                initial_result = self
                    .scheduler
                    .get_current_process_mut()
                    .and_then(|p| p.stack.pop());
            }

            // Schedule next process from run queue
            if self.scheduler.schedule_next().is_none() {
                // No more processes to run
                break;
            }
        }

        // Clear active when scheduler is idle
        self.scheduler.active = None;

        Ok(initial_result)
    }

    fn get_instruction(&mut self) -> Option<Instruction> {
        let process = self.scheduler.get_current_process()?;
        let frame = process.frames.last()?;
        frame.instructions.get(frame.counter).cloned()
    }

    fn handle_constant(&mut self, index: usize) -> Result<(), Error> {
        let constant = self
            .program
            .get_constant(index)
            .ok_or(Error::ConstantUndefined(index))?;
        let value = match constant {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Binary(_) => Value::Binary(BinaryRef::Constant(index)),
        };
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        process.stack.push(value);
        Ok(())
    }

    fn handle_pop(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        process.stack.pop().ok_or(Error::StackUnderflow)?;
        Ok(())
    }

    fn handle_duplicate(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let value = process.stack.last().ok_or(Error::StackUnderflow)?;
        process.stack.push(value.clone());
        Ok(())
    }

    fn handle_copy(&mut self, depth: usize) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        if depth >= process.stack.len() {
            return Err(Error::StackUnderflow);
        }
        let stack_index = process.stack.len() - 1 - depth;
        let value = process.stack[stack_index].clone();
        process.stack.push(value);
        Ok(())
    }

    fn handle_swap(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let a = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let b = process.stack.pop().ok_or(Error::StackUnderflow)?;
        process.stack.push(a);
        process.stack.push(b);
        Ok(())
    }

    fn handle_load(&mut self, index: usize) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
        let locals_index = frame.locals_base + index;
        let value = process
            .locals
            .get(locals_index)
            .ok_or(Error::VariableUndefined(format!("local {}", index)))?
            .clone();
        process.stack.push(value);
        Ok(())
    }

    fn handle_store(&mut self, index: usize) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
        let locals_index = frame.locals_base + index;

        // Check that local was reserved
        if locals_index >= process.locals.len() {
            return Err(Error::VariableUndefined(format!(
                "local {} not allocated",
                index
            )));
        }

        process.locals[locals_index] = value;
        Ok(())
    }

    fn handle_tuple(&mut self, type_id: TypeId) -> Result<(), Error> {
        // Look up the type to get the field count
        let Some((_, fields)) = self.program.lookup_type(&type_id) else {
            return Err(Error::TypeMismatch {
                expected: "known tuple type".to_string(),
                found: format!("unknown TypeId({:?})", type_id),
            });
        };

        let size = fields.len();
        let process = self
            .scheduler
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

    fn handle_get(&mut self, index: usize) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        match value {
            Value::Tuple(_type_id, fields) => {
                fields.get(index).ok_or(Error::FieldAccessInvalid(index))?;
                process.stack.push(fields[index].clone());
                Ok(())
            }
            value => Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: value.type_name().to_string(),
            }),
        }
    }

    fn handle_is_integer(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let is_match = matches!(value, Value::Integer(_));
        process.stack.push(Value::Tuple(
            if is_match { TypeId::OK } else { TypeId::NIL },
            vec![],
        ));
        Ok(())
    }

    fn handle_is_binary(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let is_match = matches!(value, Value::Binary(_));
        process.stack.push(Value::Tuple(
            if is_match { TypeId::OK } else { TypeId::NIL },
            vec![],
        ));
        Ok(())
    }

    fn handle_is_tuple(&mut self, expected_type_id: TypeId) -> Result<(), Error> {
        let value = {
            let process = self
                .scheduler
                .get_current_process_mut()
                .ok_or(Error::InvalidArgument("No current process".to_string()))?;
            process.stack.pop().ok_or(Error::StackUnderflow)?
        };

        let is_match = if let Value::Tuple(actual_type_id, _) = &value {
            if actual_type_id == &expected_type_id {
                // Fast path: exact match
                true
            } else {
                // Use Type::is_compatible for structural checking
                let actual_type = Type::Tuple(*actual_type_id);
                let expected_type = Type::Tuple(expected_type_id);
                actual_type.is_compatible(&expected_type, self)
            }
        } else {
            false
        };

        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        process.stack.push(Value::Tuple(
            if is_match { TypeId::OK } else { TypeId::NIL },
            vec![],
        ));
        Ok(())
    }

    fn handle_jump(&mut self, offset: isize) -> Result<(), Error> {
        self.jump(offset);
        Ok(())
    }

    fn handle_jump_if(&mut self, offset: isize) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        // Jump if value is truthy (NOT NIL)
        if !matches!(value, Value::Tuple(TypeId::NIL, _)) {
            self.jump(offset);
        }
        Ok(())
    }

    fn handle_call(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let callee = process.stack.pop().ok_or(Error::StackUnderflow)?;

        match callee {
            Value::Function(function, captures) => {
                let func = self
                    .program
                    .get_function(function)
                    .ok_or(Error::FunctionUndefined(function))?;
                let instructions = func.instructions.clone();

                // Set up new frame's locals_base at current end of locals
                let locals_base = process.locals.len();

                // Extend locals with captures
                let captures_count = captures.len();
                process.locals.extend(captures);

                process
                    .frames
                    .push(Frame::new(instructions, locals_base, captures_count));
                Ok(())
            }
            Value::Builtin(name) => {
                let argument = process.stack.pop().ok_or(Error::StackUnderflow)?;
                let builtin_fn =
                    BUILTIN_REGISTRY
                        .get_implementation(&name)
                        .ok_or(Error::InvalidArgument(format!(
                            "Unrecognised builtin: {}",
                            name
                        )))?;
                let result = builtin_fn(&argument, &self.program)?;
                process.stack.push(result);
                // Unlike regular calls, builtins don't create a new frame
                // So we need to manually increment the counter
                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }
                Ok(())
            }
            _ => Err(Error::CallInvalid),
        }
    }

    fn handle_tail_call(&mut self, recurse: bool) -> Result<(), Error> {
        let process = self
            .scheduler
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
            *process.frames.last_mut().unwrap() =
                Frame::new(instructions, locals_base, captures_count);
            Ok(())
        } else {
            let callee = process.stack.pop().ok_or(Error::StackUnderflow)?;
            match callee {
                Value::Function(function, captures) => {
                    let argument = process.stack.pop().ok_or(Error::StackUnderflow)?;
                    let func = self
                        .program
                        .get_function(function)
                        .ok_or(Error::FunctionUndefined(function))?;
                    let instructions = func.instructions.clone();

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

    fn handle_return(&mut self) -> Result<(), Error> {
        let current_pid = self
            .scheduler
            .active
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;

        let process = self
            .scheduler
            .get_process_mut(current_pid)
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;

        let frame = process.frames.pop().ok_or(Error::FrameUnderflow)?;
        // Clear frame's locals
        process.locals.truncate(frame.locals_base);

        if process.frames.is_empty() {
            if process.persistent {
                self.scheduler.suspend_process(current_pid);
            } else {
                self.scheduler.terminate_process(current_pid);
            }
        }

        Ok(())
    }

    fn handle_function(&mut self, function_index: usize) -> Result<(), Error> {
        let func = self
            .program
            .get_function(function_index)
            .ok_or(Error::FunctionUndefined(function_index))?;
        let capture_locals = func.captures.clone();

        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
        let locals_base = frame.locals_base;

        // Collect captured values from current frame's locals
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

        process
            .stack
            .push(Value::Function(function_index, captures));
        Ok(())
    }

    fn handle_builtin(&mut self, index: usize) -> Result<(), Error> {
        let function_name = self
            .program
            .get_builtin(index)
            .ok_or(Error::BuiltinUndefined(index))?
            .clone();
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        process.stack.push(Value::Builtin(function_name));
        Ok(())
    }

    fn handle_equal(&mut self, count: usize) -> Result<(), Error> {
        if count == 0 {
            return Err(Error::InvalidArgument(
                "Cannot compare zero values".to_string(),
            ));
        }

        let values = {
            let process = self
                .scheduler
                .get_current_process_mut()
                .ok_or(Error::InvalidArgument("No current process".to_string()))?;

            // Pop the specified number of values from the stack
            let mut values = Vec::with_capacity(count);
            for _ in 0..count {
                values.push(process.stack.pop().ok_or(Error::StackUnderflow)?);
            }

            // Reverse to get them in the original order (since we popped in reverse)
            values.reverse();
            values
        };

        // Check if all elements are equal to the first element
        let first = &values[0];
        let all_equal = values.iter().all(|value| values_equal(first, value, self));

        let result = if all_equal {
            first.clone() // Return the first value if all are equal
        } else {
            Value::nil() // Return NIL if not all equal
        };

        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        process.stack.push(result);
        Ok(())
    }

    fn handle_not(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let result = match value {
            Value::Tuple(type_id, fields) => {
                // Check if it's NIL (empty tuple with TypeId::NIL)
                if type_id == TypeId::NIL && fields.is_empty() {
                    Value::ok() // Return Ok
                } else {
                    Value::nil() // Return NIL
                }
            }
            _ => Value::nil(), // Any non-tuple becomes NIL
        };

        process.stack.push(result);
        Ok(())
    }

    fn handle_clear(&mut self, count: usize) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        if count > process.locals.len() {
            return Err(Error::StackUnderflow);
        }
        process.locals.truncate(process.locals.len() - count);
        Ok(())
    }

    fn handle_allocate(&mut self, count: usize) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        process
            .locals
            .extend(std::iter::repeat(Value::nil()).take(count));
        Ok(())
    }

    fn jump(&mut self, offset: isize) {
        if let Some(process) = self.scheduler.get_current_process_mut() {
            if let Some(frame) = process.frames.last_mut() {
                frame.counter = frame.counter.wrapping_add_signed(offset);
            }
        }
    }

    pub fn get_stack(&self, process_id: ProcessId) -> Option<Vec<Value>> {
        self.scheduler
            .get_process(process_id)
            .map(|p| p.stack.clone())
    }

    pub fn frame_count(&self, process_id: ProcessId) -> Option<usize> {
        self.scheduler
            .get_process(process_id)
            .map(|p| p.frames.len())
    }

    pub fn cleanup_locals(
        &mut self,
        process_id: ProcessId,
        variables: &HashMap<String, usize>,
    ) -> HashMap<String, usize> {
        let process = match self.scheduler.get_process_mut(process_id) {
            Some(p) => p,
            None => return HashMap::new(),
        };

        // Collect referenced indices in sorted order
        let mut referenced: Vec<(String, usize)> = variables
            .iter()
            .map(|(name, &index)| (name.clone(), index))
            .collect();
        referenced.sort_by_key(|(_, index)| *index);

        // Build new locals array with only referenced values and create remapping
        let mut new_locals = Vec::new();
        let mut new_variables = HashMap::new();

        for (name, old_index) in referenced {
            if old_index < process.locals.len() {
                new_variables.insert(name, new_locals.len());
                new_locals.push(process.locals[old_index].clone());
            }
        }

        process.locals = new_locals;
        new_variables
    }

    pub fn get_variables(
        &self,
        process_id: ProcessId,
        mapping: &HashMap<String, usize>,
    ) -> Result<HashMap<String, Value>, Error> {
        let process = self
            .scheduler
            .get_process(process_id)
            .ok_or(Error::InvalidArgument(format!(
                "Process {:?} not found",
                process_id
            )))?;

        let mut variables = HashMap::new();
        for (name, &index) in mapping {
            if index >= process.locals.len() {
                return Err(Error::VariableUndefined(format!(
                    "local index {} out of bounds (locals.len = {})",
                    index,
                    process.locals.len()
                )));
            }
            variables.insert(name.clone(), process.locals[index].clone());
        }
        Ok(variables)
    }

    pub fn format_value(&self, value: &Value) -> String {
        crate::format::format_value(&self.program, value)
    }

    pub fn format_type(&self, type_def: &Type) -> String {
        crate::format::format_type(&self.program, type_def)
    }

    pub fn get_process_statuses(&self) -> HashMap<ProcessId, ProcessStatus> {
        self.scheduler.get_process_statuses()
    }

    fn handle_spawn(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
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

        let func = self
            .program
            .get_function(function_index)
            .ok_or(Error::FunctionUndefined(function_index))?;
        let instructions = func.instructions.clone();

        let new_pid = self.scheduler.spawn_process(false);
        let new_process = self
            .scheduler
            .get_process_mut(new_pid)
            .ok_or(Error::InvalidArgument(
                "Failed to create process".to_string(),
            ))?;

        // Set up new process with captures and parameter (nil)
        let captures_count = captures.len();
        new_process.locals.extend(captures);
        new_process.stack.push(Value::nil());
        // locals_base=0 (frame starts at beginning), captures_count tracks how many are captures
        new_process
            .frames
            .push(Frame::new(instructions, 0, captures_count));

        // Push new process ID onto current process stack
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        process.stack.push(Value::Pid(new_pid));

        Ok(())
    }

    fn handle_send(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
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

        // Add message to target process mailbox
        let target_process =
            self.scheduler
                .get_process_mut(target_pid)
                .ok_or(Error::InvalidArgument(format!(
                    "Process {:?} not found",
                    target_pid
                )))?;
        target_process.mailbox.push_back(message);

        // Wake process if it's waiting
        self.scheduler.wake_process(target_pid);

        Ok(())
    }

    fn handle_self(&mut self) -> Result<(), Error> {
        let current_pid = self
            .scheduler
            .active
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;

        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;
        process.stack.push(Value::Pid(current_pid));

        Ok(())
    }

    fn handle_receive(&mut self) -> Result<(), Error> {
        let current_pid = self
            .scheduler
            .active
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;

        let has_message = {
            let process = self
                .scheduler
                .get_process(current_pid)
                .ok_or(Error::InvalidArgument("No current process".to_string()))?;
            !process.mailbox.is_empty()
        };

        if has_message {
            // Take the first message from the mailbox
            let process = self
                .scheduler
                .get_process_mut(current_pid)
                .ok_or(Error::InvalidArgument("No current process".to_string()))?;
            let message = process.mailbox.remove(0).unwrap();
            process.current_message = Some(message.clone());
            process.stack.push(message);
        } else {
            // No messages - mark process as waiting and stop executing
            self.scheduler.mark_waiting(current_pid);
            // Clear active so get_instruction() returns None and execution stops
            self.scheduler.active = None;
        }

        Ok(())
    }

    fn handle_acknowledge(&mut self) -> Result<(), Error> {
        let process = self
            .scheduler
            .get_current_process_mut()
            .ok_or(Error::InvalidArgument("No current process".to_string()))?;

        // Clear the current message
        process.current_message = None;

        Ok(())
    }
}

/// Helper function to compare values for equality
fn values_equal(a: &Value, b: &Value, vm: &VM) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Binary(a), Value::Binary(b)) => {
            match (
                vm.program.get_binary_bytes(a),
                vm.program.get_binary_bytes(b),
            ) {
                (Ok(bytes_a), Ok(bytes_b)) => bytes_a == bytes_b,
                _ => false, // If we can't access bytes, consider unequal
            }
        }
        (Value::Tuple(type_a, elems_a), Value::Tuple(type_b, elems_b)) => {
            type_a == type_b
                && elems_a.len() == elems_b.len()
                && elems_a
                    .iter()
                    .zip(elems_b.iter())
                    .all(|(x, y)| values_equal(x, y, vm))
        }
        (Value::Function(a, cap_a), Value::Function(b, cap_b)) => {
            a == b
                && cap_a.len() == cap_b.len()
                && cap_a
                    .iter()
                    .zip(cap_b.iter())
                    .all(|(x, y)| values_equal(x, y, vm))
        }
        (Value::Pid(a), Value::Pid(b)) => a == b,
        _ => false,
    }
}
