use crate::builtins::BUILTIN_REGISTRY;
use crate::bytecode::{Constant, Function, Instruction, TypeId};
use crate::error::Error;
use crate::process::{Action, Frame, Process, ProcessId, ProcessInfo, ProcessStatus};
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use crate::value::{Binary, MAX_BINARY_SIZE, Value};
use std::collections::{HashMap, HashSet, VecDeque};

pub struct Executor {
    processes: HashMap<ProcessId, Process>,
    queue: VecDeque<ProcessId>,
    receiving: HashSet<ProcessId>,
    awaiting: HashSet<ProcessId>,
    // Program data owned by executor
    constants: Vec<Constant>,
    functions: Vec<Function>,
    builtins: Vec<String>,
    types: Vec<TupleTypeInfo>,
    // Heap for runtime-allocated binaries
    heap: Vec<Vec<u8>>,
}

impl TypeLookup for Executor {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.types.get(type_id.0)
    }
}

impl Executor {
    pub fn get_constant(&self, index: usize) -> Option<&Constant> {
        self.constants.get(index)
    }

    pub fn with_binary_bytes<F, R>(&self, binary: &Binary, f: F) -> Result<R, Error>
    where
        F: FnOnce(&[u8]) -> Result<R, Error>,
    {
        match binary {
            Binary::Constant(index) => {
                let constant = self
                    .get_constant(*index)
                    .ok_or(Error::ConstantUndefined(*index))?;
                match constant {
                    Constant::Binary(bytes) => f(bytes),
                    _ => Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    }),
                }
            }
            Binary::Heap(index) => {
                let bytes = self.heap.get(*index).ok_or_else(|| {
                    Error::InvalidArgument(format!("Heap binary index {} not found", index))
                })?;
                f(bytes)
            }
        }
    }

    /// Convenience method that clones the binary data
    /// For cases where the closure API would be cumbersome (e.g., multiple binary accesses)
    pub fn get_binary_bytes(&self, binary: &Binary) -> Result<Vec<u8>, Error> {
        self.with_binary_bytes(binary, |bytes| Ok(bytes.to_vec()))
    }

    pub fn allocate_binary(&mut self, bytes: Vec<u8>) -> Result<Binary, Error> {
        if bytes.len() > MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                bytes.len(),
                MAX_BINARY_SIZE
            )));
        }
        let index = self.heap.len();
        self.heap.push(bytes);
        Ok(Binary::Heap(index))
    }

    pub fn new() -> Self {
        Self {
            processes: HashMap::new(),
            queue: VecDeque::new(),
            receiving: HashSet::new(),
            awaiting: HashSet::new(),
            constants: vec![],
            functions: vec![],
            builtins: vec![],
            types: vec![],
            heap: vec![],
        }
    }

    pub fn spawn_process(&mut self, id: ProcessId, persistent: bool) {
        let process = Process::new(persistent);
        self.processes.insert(id, process);
        self.queue.push_back(id);
    }

    pub fn get_process(&self, id: ProcessId) -> Option<&Process> {
        self.processes.get(&id)
    }

    pub fn get_process_mut(&mut self, id: ProcessId) -> Option<&mut Process> {
        self.processes.get_mut(&id)
    }

    pub fn suspend_process(&mut self, id: ProcessId) {
        self.queue.retain(|&pid| pid != id);
    }

    /// Notify a process that spawned a new process with the new PID
    pub fn notify_spawn(&mut self, id: ProcessId, pid: Value) {
        if self.awaiting.remove(&id) {
            if let Some(process) = self.processes.get_mut(&id) {
                // For spawn notifications, just push the PID onto the stack and increment counter
                process.stack.push(pid);

                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }

                self.queue.push_back(id);
            }
        }
    }

    /// Notify a process that was awaiting a result with the result value
    pub fn notify_result(&mut self, id: ProcessId, result: Value) {
        if self.awaiting.remove(&id) {
            if let Some(process) = self.processes.get_mut(&id) {
                // When awaiting on a PID, the stack has [parameter, pid]
                // We need to pop both and push the result, then increment counter
                process.stack.pop(); // Pop pid
                process.stack.pop(); // Pop parameter (ignored for await)
                process.stack.push(result);

                // Increment counter to move past the Call instruction
                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }

                self.queue.push_back(id);
            }
        }
    }

    pub fn notify_message(&mut self, id: ProcessId, message: Value) {
        if let Some(process) = self.processes.get_mut(&id) {
            process.mailbox.push_back(message);
            if self.receiving.remove(&id) {
                self.queue.push_back(id);
            }
        }
    }

    pub fn mark_receiving(&mut self, id: ProcessId) {
        self.receiving.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_awaiting(&mut self, id: ProcessId) {
        self.awaiting.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn add_to_queue(&mut self, process_id: ProcessId) {
        self.queue.push_back(process_id);
    }

    fn get_status(&self, id: ProcessId, process: &Process) -> ProcessStatus {
        if self.queue.contains(&id) {
            ProcessStatus::Active
        } else if self.receiving.contains(&id) || self.awaiting.contains(&id) {
            ProcessStatus::Waiting
        } else if process.persistent {
            ProcessStatus::Sleeping
        } else {
            ProcessStatus::Terminated
        }
    }

    pub fn get_process_statuses(&self) -> HashMap<ProcessId, ProcessStatus> {
        self.processes
            .iter()
            .map(|(id, process)| (*id, self.get_status(*id, process)))
            .collect()
    }

    pub fn get_process_info(&self, id: ProcessId) -> Option<ProcessInfo> {
        self.processes.get(&id).map(|process| ProcessInfo {
            id,
            status: self.get_status(id, process),
            stack_size: process.stack.len(),
            locals_size: process.locals.len(),
            frames_count: process.frames.len(),
            mailbox_size: process.mailbox.len(),
            persistent: process.persistent,
            result: process.result.clone(),
        })
    }

    // Program data accessors
    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    pub fn get_builtin(&self, index: usize) -> Option<&String> {
        self.builtins.get(index)
    }

    pub fn update_program(
        &mut self,
        mut constants: Vec<Constant>,
        mut functions: Vec<Function>,
        mut types: Vec<TupleTypeInfo>,
        mut builtins: Vec<String>,
    ) {
        self.constants.append(&mut constants);
        self.functions.append(&mut functions);
        self.types.append(&mut types);
        self.builtins.append(&mut builtins);
    }

    /// Execute up to max_units instruction units for a single process.
    /// Returns None to continue, or Some(request) for a routing request that needs to be handled by the scheduler.
    pub fn step(&mut self, max_units: usize) -> Result<Option<Action>, Error> {
        // Pop process from queue
        let current_pid = match self.queue.pop_front() {
            Some(pid) => pid,
            None => return Ok(None),
        };

        let mut units_executed = 0;
        let mut pending_request = None;

        // Execute instructions for current process
        while units_executed < max_units {
            let instruction = self
                .get_process(current_pid)
                .and_then(|p| p.frames.last())
                .and_then(|f| f.instructions.get(f.counter).cloned());

            let Some(instruction) = instruction else {
                break; // Process finished or no more instructions in current frame
            };

            let step_result = self.execute_instruction(current_pid, instruction);

            units_executed += 1;

            // Handle instruction result
            match step_result {
                Ok(request) => {
                    pending_request = request;
                }
                Err(error) => {
                    // Clear frames and set error result to complete the process
                    if let Some(process) = self.get_process_mut(current_pid) {
                        process.frames.clear();
                        process.result = Some(Err(error.clone()));
                    }
                    // Process is now completed with error - don't return error from step()
                    return Ok(None);
                }
            }

            // Check if process should yield (moved to receiving or awaiting, or has pending request)
            // Pending request check ensures only ONE routing request per step
            if self.receiving.contains(&current_pid)
                || self.awaiting.contains(&current_pid)
                || pending_request.is_some()
            {
                break;
            }
        }

        // Auto-pop any exhausted frames before checking if process is finished
        if let Some(process) = self.get_process_mut(current_pid) {
            while let Some(frame) = process.frames.last() {
                if frame.counter >= frame.instructions.len() {
                    // Frame exhausted - pop it without stack manipulation
                    // (the result is already on the stack from the last instruction)
                    let frame = process.frames.pop().unwrap();
                    let is_last_frame = process.frames.is_empty();

                    // Clear locals from the popped frame
                    // For persistent processes, only keep locals if this was the last (top-level) frame
                    let should_clear_locals = !process.persistent || !is_last_frame;
                    if should_clear_locals {
                        let locals_to_clear = process
                            .locals
                            .len()
                            .saturating_sub(frame.locals_base + frame.captures_count);
                        if locals_to_clear > 0 {
                            process
                                .locals
                                .truncate(process.locals.len() - locals_to_clear);
                        }
                    }

                    // Increment counter of the calling frame (if there is one)
                    if let Some(calling_frame) = process.frames.last_mut() {
                        calling_frame.counter += 1;
                    }
                } else {
                    break;
                }
            }
        }

        let process = self.get_process(current_pid);
        let finished = process.map(|p| p.frames.is_empty()).unwrap_or(false);

        if finished {
            // Store result
            if let Some(process) = self.get_process_mut(current_pid) {
                let result_value = process.stack.pop().unwrap_or_else(Value::nil);
                process.result = Some(Ok(result_value));
            }
        } else {
            let should_requeue =
                !self.receiving.contains(&current_pid) && !self.awaiting.contains(&current_pid);

            if should_requeue {
                // Process not finished - re-queue it so it can continue
                // This handles both: time slice exhaustion AND yielding after routing (e.g., Send)
                self.queue.push_back(current_pid);
            }
        }

        // Return pending routing request if one was set, otherwise None
        Ok(pending_request)
    }

    fn execute_instruction(
        &mut self,
        pid: ProcessId,
        instruction: Instruction,
    ) -> Result<Option<Action>, Error> {
        match instruction {
            Instruction::Constant(index) => self.handle_constant(pid, index),
            Instruction::Pop => self.handle_pop(pid),
            Instruction::Duplicate => self.handle_duplicate(pid),
            Instruction::Over => self.handle_over(pid),
            Instruction::Swap => self.handle_swap(pid),
            Instruction::Load(index) => self.handle_load(pid, index),
            Instruction::Store(index) => self.handle_store(pid, index),
            Instruction::Tuple(type_id) => self.handle_tuple(pid, type_id),
            Instruction::Get(index) => self.handle_get(pid, index),
            Instruction::IsInteger => self.handle_is_integer(pid),
            Instruction::IsBinary => self.handle_is_binary(pid),
            Instruction::IsTuple(type_id) => self.handle_is_tuple(pid, type_id),
            Instruction::Jump(offset) => self.handle_jump(pid, offset),
            Instruction::JumpIf(offset) => self.handle_jump_if(pid, offset),
            Instruction::Call => self.handle_call(pid),
            Instruction::TailCall(recurse) => self.handle_tail_call(pid, recurse),
            Instruction::Function(function_index) => self.handle_function(pid, function_index),
            Instruction::Clear(count) => self.handle_clear(pid, count),
            Instruction::Allocate(count) => self.handle_allocate(pid, count),
            Instruction::Builtin(index) => self.handle_builtin(pid, index),
            Instruction::Equal(count) => self.handle_equal(pid, count),
            Instruction::Not => self.handle_not(pid),
            Instruction::Spawn => self.handle_spawn(pid),
            Instruction::Send => self.handle_send(pid),
            Instruction::Self_ => self.handle_self(pid),
            Instruction::Receive => self.handle_receive(pid),
            Instruction::Acknowledge => self.handle_acknowledge(pid),
        }
    }

    fn handle_constant(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action>, Error> {
        let constant = self
            .get_constant(index)
            .ok_or(Error::ConstantUndefined(index))?;
        let value = match constant {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Binary(_) => Value::Binary(Binary::Constant(index)),
        };

        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.stack.push(value);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_pop(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.stack.pop().ok_or(Error::StackUnderflow)?;

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_duplicate(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        let value = process.stack.last().ok_or(Error::StackUnderflow)?.clone();
        process.stack.push(value);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_over(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        if process.stack.len() < 2 {
            return Err(Error::StackUnderflow);
        }
        let index = process.stack.len() - 2;
        let value = process.stack[index].clone();
        process.stack.push(value);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_swap(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let len = process.stack.len();
        if len < 2 {
            return Err(Error::StackUnderflow);
        }
        process.stack.swap(len - 1, len - 2);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_load(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
        let actual_index = frame.locals_base + index;

        let value = process
            .locals
            .get(actual_index)
            .ok_or(Error::VariableUndefined(format!("local[{}]", index)))?
            .clone();

        process.stack.push(value);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_store(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
        let actual_index = frame.locals_base + index;

        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        if actual_index >= process.locals.len() {
            return Err(Error::VariableUndefined(format!("local[{}]", index)));
        }

        process.locals[actual_index] = value;

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_tuple(&mut self, pid: ProcessId, type_id: TypeId) -> Result<Option<Action>, Error> {
        let (_, fields) = self
            .types
            .get(type_id.0)
            .ok_or_else(|| Error::TypeMismatch {
                expected: "known tuple type".to_string(),
                found: format!("unknown TypeId({:?})", type_id),
            })?;
        let size = fields.len();

        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let mut values = Vec::new();
        for _ in 0..size {
            let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
            values.push(value);
        }
        values.reverse();
        process.stack.push(Value::Tuple(type_id, values));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_get(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        match value {
            Value::Tuple(_, elements) => {
                let element = elements
                    .get(index)
                    .ok_or(Error::FieldAccessInvalid(index))?
                    .clone();
                process.stack.push(element);

                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }
                Ok(None)
            }
            _ => Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: value.type_name().to_string(),
            }),
        }
    }

    fn handle_is_integer(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let result = if matches!(value, Value::Integer(_)) {
            Value::ok()
        } else {
            Value::nil()
        };

        process.stack.push(result);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_is_binary(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let result = if matches!(value, Value::Binary(_)) {
            Value::ok()
        } else {
            Value::nil()
        };

        process.stack.push(result);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_is_tuple(
        &mut self,
        pid: ProcessId,
        type_id: TypeId,
    ) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let is_match = if let Value::Tuple(actual_type_id, _) = &value {
            if actual_type_id == &type_id {
                // Fast path: exact match
                true
            } else {
                // Use Type::is_compatible for structural checking
                let actual_type = Type::Tuple(*actual_type_id);
                let expected_type = Type::Tuple(type_id);
                actual_type.is_compatible(&expected_type, self)
            }
        } else {
            false
        };

        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        process
            .stack
            .push(if is_match { Value::ok() } else { Value::nil() });

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_jump(&mut self, pid: ProcessId, offset: isize) -> Result<Option<Action>, Error> {
        if let Some(process) = self.get_process_mut(pid) {
            if let Some(frame) = process.frames.last_mut() {
                // Jump modifies counter directly
                // Add 1 to offset because in the old code, Jump got the centralized increment
                frame.counter = frame.counter.wrapping_add_signed(offset + 1);
            }
        }
        Ok(None)
    }

    fn handle_jump_if(&mut self, pid: ProcessId, offset: isize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let condition = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let should_jump = match &condition {
            Value::Tuple(type_id, fields) => !(type_id == &TypeId::NIL && fields.is_empty()),
            _ => true,
        };

        if let Some(frame) = process.frames.last_mut() {
            if should_jump {
                // Jump modifies counter directly
                // Add 1 to offset because in the old code, JumpIf got the centralized increment
                frame.counter = frame.counter.wrapping_add_signed(offset + 1);
            } else {
                // Not jumping, increment normally
                frame.counter += 1;
            }
        }

        Ok(None)
    }

    fn handle_call(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let function_value = {
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            process.stack.last().ok_or(Error::StackUnderflow)?.clone()
        };

        match function_value {
            Value::Function(function_index, captures) => {
                // Get function instructions before modifying process
                let func = self
                    .get_function(function_index)
                    .ok_or(Error::FunctionUndefined(function_index))?;
                let instructions = func.instructions.clone();

                // Now modify process
                let process = self
                    .get_process_mut(pid)
                    .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

                // Pop function and parameter
                process.stack.pop();
                let parameter = process.stack.pop().ok_or(Error::StackUnderflow)?;

                let locals_base = process.locals.len();
                let captures_count = captures.len();
                process.stack.push(parameter);
                process.locals.extend(captures);

                process
                    .frames
                    .push(Frame::new(instructions, locals_base, captures_count));

                // Don't increment counter - new frame starts at 0
                Ok(None)
            }
            Value::Builtin(name) => {
                // Pop function and parameter
                let parameter = {
                    let process = self
                        .get_process_mut(pid)
                        .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                    process.stack.pop(); // Pop function
                    process.stack.pop().ok_or(Error::StackUnderflow)?
                };

                let builtin = BUILTIN_REGISTRY.get_implementation(&name).ok_or_else(|| {
                    Error::InvalidArgument(format!("Unrecognised builtin: {}", name))
                })?;

                let result = builtin(&parameter, self)?;

                let process = self
                    .get_process_mut(pid)
                    .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

                process.stack.push(result);

                // Unlike regular calls, builtins don't create a new frame
                // So we need to manually increment the counter
                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }

                Ok(None)
            }
            Value::Pid(target_pid) => {
                // Mark caller as awaiting result
                self.mark_awaiting(pid);

                // Return routing request for scheduler to handle
                // Don't increment counter - will be incremented in notify_result
                Ok(Some(Action::AwaitResult {
                    target: target_pid,
                    caller: pid,
                }))
            }
            _ => Err(Error::TypeMismatch {
                expected: "function".to_string(),
                found: function_value.type_name().to_string(),
            }),
        }
    }

    fn handle_tail_call(&mut self, pid: ProcessId, recurse: bool) -> Result<Option<Action>, Error> {
        if recurse {
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

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

            // Don't increment counter - frame was reset to 0
            Ok(None)
        } else {
            let (function_value, argument) = {
                let process = self
                    .get_process_mut(pid)
                    .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                let function_value = process.stack.pop().ok_or(Error::StackUnderflow)?;
                let argument = process.stack.pop().ok_or(Error::StackUnderflow)?;
                (function_value, argument)
            };

            match function_value {
                Value::Function(function, captures) => {
                    let func = self
                        .get_function(function)
                        .ok_or(Error::FunctionUndefined(function))?;
                    let instructions = func.instructions.clone();

                    let process = self
                        .get_process_mut(pid)
                        .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

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

                    // Don't increment counter - frame was reset to 0
                    Ok(None)
                }
                _ => Err(Error::CallInvalid),
            }
        }
    }

    fn handle_function(
        &mut self,
        pid: ProcessId,
        function_index: usize,
    ) -> Result<Option<Action>, Error> {
        let func = self
            .get_function(function_index)
            .ok_or(Error::FunctionUndefined(function_index))?;
        let capture_locals = func.captures.clone();
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

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

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_clear(&mut self, pid: ProcessId, count: usize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        if count > process.locals.len() {
            return Err(Error::StackUnderflow);
        }
        process.locals.truncate(process.locals.len() - count);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_allocate(&mut self, pid: ProcessId, count: usize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        process
            .locals
            .extend(std::iter::repeat(Value::nil()).take(count));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_builtin(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action>, Error> {
        let builtin_name = self
            .get_builtin(index)
            .ok_or(Error::BuiltinUndefined(index))?
            .clone();
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        process.stack.push(Value::Builtin(builtin_name));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_equal(&mut self, pid: ProcessId, count: usize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

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

        let first = &values[0];
        let all_equal = values.iter().all(|value| self.values_equal(first, value));

        let result = if all_equal {
            first.clone()
        } else {
            Value::nil()
        };

        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        process.stack.push(result);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_not(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

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

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_spawn(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

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

        // Mark caller as awaiting - will be notified with Value::Pid(new_pid)
        self.mark_awaiting(pid);

        // Return routing request for scheduler to handle
        // Don't increment counter - will be incremented in notify_spawn
        Ok(Some(Action::Spawn {
            caller: pid,
            function_index,
            captures,
        }))
    }

    fn handle_send(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let pid_value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let message = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let target_pid = match pid_value {
            Value::Pid(target) => target,
            _ => {
                return Err(Error::TypeMismatch {
                    expected: "pid".to_string(),
                    found: pid_value.type_name().to_string(),
                });
            }
        };

        // Push ok onto stack before returning routing request
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.stack.push(Value::ok());

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }

        // Return routing request for scheduler to handle
        Ok(Some(Action::Deliver {
            target: target_pid,
            value: message,
        }))
    }

    fn handle_self(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        process.stack.push(Value::Pid(pid));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_receive(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let cursor = process.cursor;
        if cursor < process.mailbox.len() {
            let message = process.mailbox[cursor].clone();
            process.cursor = cursor + 1;
            process.stack.push(message);

            // Increment counter to move past the Receive instruction
            if let Some(frame) = process.frames.last_mut() {
                frame.counter += 1;
            }
        } else {
            self.mark_receiving(pid);
            // Don't increment counter - will retry Receive when a message arrives
        }

        Ok(None)
    }

    fn handle_acknowledge(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let cursor = process.cursor;
        if 0 < cursor && cursor - 1 < process.mailbox.len() {
            process.mailbox.remove(cursor - 1);
        }
        process.cursor = 0;

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Binary(a), Value::Binary(b)) => {
                match (self.get_binary_bytes(a), self.get_binary_bytes(b)) {
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
                        .all(|(a, b)| self.values_equal(a, b))
            }
            (Value::Function(idx_a, caps_a), Value::Function(idx_b, caps_b)) => {
                idx_a == idx_b
                    && caps_a.len() == caps_b.len()
                    && caps_a
                        .iter()
                        .zip(caps_b.iter())
                        .all(|(a, b)| self.values_equal(a, b))
            }
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Pid(a), Value::Pid(b)) => a == b,
            _ => false,
        }
    }

    /// Extract heap data from a value for cross-executor transfer.
    /// Returns the value with heap indices remapped to 0..n and the extracted heap data.
    pub fn extract_heap_data(&self, value: &Value) -> Result<(Value, Vec<Vec<u8>>), Error> {
        let mut heap_data = Vec::new();
        let mut heap_map = HashMap::new();
        let converted = self.extract_value_recursive(value, &mut heap_data, &mut heap_map)?;
        Ok((converted, heap_data))
    }

    fn extract_value_recursive(
        &self,
        value: &Value,
        heap_data: &mut Vec<Vec<u8>>,
        heap_map: &mut HashMap<usize, usize>,
    ) -> Result<Value, Error> {
        match value {
            Value::Binary(Binary::Heap(heap_index)) => {
                // Check if we've already extracted this heap entry
                if let Some(&new_index) = heap_map.get(heap_index) {
                    return Ok(Value::Binary(Binary::Heap(new_index)));
                }

                // Read the binary data from heap
                let bytes = self.get_binary_bytes(&Binary::Heap(*heap_index))?;

                // Add to extracted heap data with new index
                let new_index = heap_data.len();
                heap_data.push(bytes);
                heap_map.insert(*heap_index, new_index);

                Ok(Value::Binary(Binary::Heap(new_index)))
            }
            Value::Binary(Binary::Constant(_)) => {
                // Constants don't need extraction
                Ok(value.clone())
            }
            Value::Tuple(type_id, elements) => {
                // Recursively extract from all tuple elements
                let converted_elements = elements
                    .iter()
                    .map(|elem| self.extract_value_recursive(elem, heap_data, heap_map))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::Tuple(*type_id, converted_elements))
            }
            Value::Function(func_index, captures) => {
                // Recursively extract from all captures
                let converted_captures = captures
                    .iter()
                    .map(|cap| self.extract_value_recursive(cap, heap_data, heap_map))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::Function(*func_index, converted_captures))
            }
            // Other value types don't contain heap references
            Value::Integer(_) | Value::Builtin(_) | Value::Pid(_) => Ok(value.clone()),
        }
    }

    /// Inject heap data into this executor and remap heap indices in the value.
    pub fn inject_heap_data(
        &mut self,
        value: Value,
        heap_data: &[Vec<u8>],
    ) -> Result<Value, Error> {
        // Allocate all heap entries and build remap table
        let mut remap = HashMap::new();
        for (old_index, bytes) in heap_data.iter().enumerate() {
            let new_index = self.heap.len();
            self.heap.push(bytes.clone());
            remap.insert(old_index, new_index);
        }

        // Remap heap indices in the value
        self.inject_value_recursive(value, &remap)
    }

    fn inject_value_recursive(
        &self,
        value: Value,
        remap: &HashMap<usize, usize>,
    ) -> Result<Value, Error> {
        match value {
            Value::Binary(Binary::Heap(old_index)) => {
                // Remap to new heap index
                let new_index = remap.get(&old_index).copied().ok_or_else(|| {
                    Error::InvalidArgument(format!("Heap index {} not in remap table", old_index))
                })?;
                Ok(Value::Binary(Binary::Heap(new_index)))
            }
            Value::Binary(Binary::Constant(_)) => {
                // Constants don't need remapping
                Ok(value)
            }
            Value::Tuple(type_id, elements) => {
                // Recursively remap all tuple elements
                let remapped_elements = elements
                    .into_iter()
                    .map(|elem| self.inject_value_recursive(elem, remap))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::Tuple(type_id, remapped_elements))
            }
            Value::Function(func_index, captures) => {
                // Recursively remap all captures
                let remapped_captures = captures
                    .into_iter()
                    .map(|cap| self.inject_value_recursive(cap, remap))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::Function(func_index, remapped_captures))
            }
            // Other value types don't contain heap references
            Value::Integer(_) | Value::Builtin(_) | Value::Pid(_) => Ok(value),
        }
    }
}
