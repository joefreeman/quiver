use crate::builtins::BUILTIN_REGISTRY;
use crate::bytecode::{BuiltinInfo, Constant, Function, Instruction, TypeId};
use crate::error::Error;
use crate::process::{Action, Frame, Process, ProcessId, ProcessInfo, ProcessStatus, SelectState};
use crate::types::{CallableType, ProcessType, TupleTypeInfo, Type, TypeLookup};
use crate::value::{Binary, MAX_BINARY_SIZE, Value};
use std::collections::{HashMap, HashSet, VecDeque};

/// Result of processing a select source
enum SelectResult {
    /// Select should complete with this value
    Complete(Value),
    /// A receive function was called, return from handle_select to let it execute
    CalledFunction,
    /// Continue to the next source
    Continue,
}

pub struct Executor {
    processes: HashMap<ProcessId, Process>,
    queue: VecDeque<ProcessId>,
    waiting: HashSet<ProcessId>,
    // Program data owned by executor
    constants: Vec<Constant>,
    functions: Vec<Function>,
    builtins: Vec<BuiltinInfo>,
    types: Vec<TupleTypeInfo>,
    // Heap for runtime-allocated binaries
    heap: Vec<Vec<u8>>,
}

impl TypeLookup for Executor {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.types.get(type_id.0)
    }
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
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
            waiting: HashSet::new(),
            constants: vec![],
            functions: vec![],
            builtins: vec![],
            types: vec![],
            heap: vec![],
        }
    }

    pub fn spawn_process(&mut self, id: ProcessId, function_index: usize, persistent: bool) {
        let process = Process::new(function_index, persistent);
        self.processes.insert(id, process);
        self.queue.push_back(id);
    }

    fn get_current_instruction(&self, process_id: ProcessId) -> Option<Instruction> {
        self.get_process(process_id)
            .and_then(|p| p.frames.last())
            .and_then(|f| {
                self.functions[f.function_index]
                    .instructions
                    .get(f.counter)
                    .cloned()
            })
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
        let was_waiting = self.waiting.remove(&id);

        if let Some(process) = self.processes.get_mut(&id) {
            // For spawn notifications, just push the PID onto the stack and increment counter
            process.stack.push(pid);

            if let Some(frame) = process.frames.last_mut() {
                frame.counter += 1;
            }

            // Only re-queue if it was actually waiting (not already queued by something else)
            if was_waiting {
                self.queue.push_back(id);
            }
        }
    }

    /// Notify a process that was waiting for a result with the result value
    pub fn notify_result(&mut self, awaiter: ProcessId, awaited: ProcessId, result: Value) {
        // Store the result in the process's awaiting map
        if let Some(process) = self.get_process_mut(awaiter) {
            process.awaiting.insert(awaited, Some(result));
        }

        // Re-queue awaiter to retry its Select instruction
        if self.waiting.remove(&awaiter) {
            self.queue.push_back(awaiter);
        }
    }

    pub fn notify_message(&mut self, id: ProcessId, message: Value) {
        if let Some(process) = self.processes.get_mut(&id) {
            process.mailbox.push_back(message);
            if self.waiting.remove(&id) {
                self.queue.push_back(id);
            }
        }
    }

    pub fn mark_waiting(&mut self, id: ProcessId) {
        self.waiting.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_active(&mut self, id: ProcessId) {
        if self.waiting.remove(&id) {
            self.queue.push_back(id);
        }
    }

    pub fn add_to_queue(&mut self, process_id: ProcessId) {
        self.queue.push_back(process_id);
    }

    fn get_status(&self, id: ProcessId, process: &Process) -> ProcessStatus {
        if self.queue.contains(&id) {
            ProcessStatus::Active
        } else if self.waiting.contains(&id) {
            ProcessStatus::Waiting
        } else if matches!(&process.result, Some(Err(_))) {
            ProcessStatus::Failed
        } else if matches!(&process.result, Some(Ok(_))) {
            // Has a successful result
            if process.persistent {
                ProcessStatus::Sleeping
            } else {
                ProcessStatus::Completed
            }
        } else {
            // No result yet - must still be active
            ProcessStatus::Active
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

    pub fn get_builtin(&self, index: usize) -> Option<&BuiltinInfo> {
        self.builtins.get(index)
    }

    pub fn update_program(
        &mut self,
        mut constants: Vec<Constant>,
        mut functions: Vec<Function>,
        mut types: Vec<TupleTypeInfo>,
        mut builtins: Vec<BuiltinInfo>,
    ) {
        self.constants.append(&mut constants);
        self.functions.append(&mut functions);
        self.types.append(&mut types);
        self.builtins.append(&mut builtins);
    }

    /// Execute up to max_units instruction units for a single process.
    /// Returns None to continue, or Some(request) for a routing request that needs to be handled by the scheduler.
    pub fn step(&mut self, max_units: usize, current_time_ms: u64) -> Option<Action> {
        // Check for expired timeouts before processing
        self.check_expired_timeouts(current_time_ms);
        // Pop process from queue
        let current_pid = self.queue.pop_front()?;

        let mut units_executed = 0;
        let mut pending_request = None;

        // Execute instructions for current process
        while units_executed < max_units {
            let Some(instruction) = self.get_current_instruction(current_pid) else {
                break; // Process finished or no more instructions in current frame
            };

            let step_result = self.execute_instruction(current_pid, instruction, current_time_ms);

            units_executed += 1;

            // Handle instruction result
            match step_result {
                Ok(request) => {
                    pending_request = request;
                }
                Err(error) => {
                    if let Some(process) = self.get_process_mut(current_pid) {
                        process.result = Some(Err(error.clone()));
                    }
                    return None;
                }
            }

            // Check if process should yield (moved to waiting, or has pending request)
            // Pending request check ensures only ONE routing request per step
            if self.waiting.contains(&current_pid) || pending_request.is_some() {
                break;
            }
        }

        // Auto-pop any exhausted frames before checking if process is finished
        loop {
            // Check if current instruction exists
            if self.get_current_instruction(current_pid).is_some() {
                break; // Current frame still has instructions to execute
            }

            // Check if there's a frame to pop
            let has_frames = self
                .get_process(current_pid)
                .map(|p| !p.frames.is_empty())
                .unwrap_or(false);

            if !has_frames {
                break; // No frames to pop
            }

            // Now get mutable borrow to modify process
            let process = self
                .get_process_mut(current_pid)
                .expect("Process should exist");

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

            // Check if we're in an active select and returning to the select instruction
            let should_skip_increment = if let Some(ref select_state) = process.select_state {
                let current_frame = process.frames.len().saturating_sub(1);
                let current_instruction = process.frames.last().map(|f| f.counter).unwrap_or(0);
                select_state.frame == current_frame
                    && select_state.instruction == current_instruction
            } else {
                false
            };

            // Increment counter of calling frame unless we're in an active select
            if !should_skip_increment && let Some(calling_frame) = process.frames.last_mut() {
                calling_frame.counter += 1;
            }
        }

        let process = self.get_process(current_pid);
        let finished = process.map(|p| p.frames.is_empty()).unwrap_or(false);

        if finished {
            // Store result
            let result_value = if let Some(process) = self.get_process_mut(current_pid) {
                // If error is already set (during execution), use nil as placeholder
                if let Some(Err(_)) = process.result {
                    Value::nil()
                } else {
                    // No error yet - pop result from stack
                    let Some(result) = process.stack.pop() else {
                        // Stack underflow - process finished with no result on stack
                        process.result = Some(Err(Error::StackUnderflow));
                        return None;
                    };
                    process.result = Some(Ok(result.clone()));
                    result
                }
            } else {
                Value::nil()
            };

            // Notify any processes awaiting this one
            let awaiters: Vec<ProcessId> = self
                .processes
                .iter()
                .filter_map(|(pid, proc)| {
                    if proc.awaiting.contains_key(&current_pid) {
                        Some(*pid)
                    } else {
                        None
                    }
                })
                .collect();

            for awaiter in awaiters {
                self.notify_result(awaiter, current_pid, result_value.clone());
            }
        } else {
            let should_requeue = !self.waiting.contains(&current_pid);

            if should_requeue {
                // Process not finished - re-queue it so it can continue
                // This handles both: time slice exhaustion AND yielding after routing (e.g., Send)
                self.queue.push_back(current_pid);
            }
        }

        // Return pending routing request if one was set, otherwise None
        pending_request
    }

    fn execute_instruction(
        &mut self,
        pid: ProcessId,
        instruction: Instruction,
        current_time_ms: u64,
    ) -> Result<Option<Action>, Error> {
        match instruction {
            Instruction::Constant(index) => self.handle_constant(pid, index),
            Instruction::Pop => self.handle_pop(pid),
            Instruction::Duplicate => self.handle_duplicate(pid),
            Instruction::Pick(n) => self.handle_pick(pid, n),
            Instruction::Rotate(n) => self.handle_rotate(pid, n),
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
            Instruction::Select(n) => self.handle_select(pid, n, current_time_ms),
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

    fn handle_pick(&mut self, pid: ProcessId, n: usize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        if process.stack.len() <= n {
            return Err(Error::StackUnderflow);
        }
        let index = process.stack.len() - 1 - n;
        let value = process.stack[index].clone();
        process.stack.push(value);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_rotate(&mut self, pid: ProcessId, n: usize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let len = process.stack.len();
        if len < n {
            return Err(Error::StackUnderflow);
        }
        // Rotate the top n items: move item at depth (n-1) to the top
        // Example: [a, b, c] with n=3 becomes [b, c, a]
        let item = process.stack.remove(len - n);
        process.stack.push(item);

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
        let type_info = self
            .types
            .get(type_id.0)
            .ok_or_else(|| Error::TypeMismatch {
                expected: "known tuple type".to_string(),
                found: format!("unknown TypeId({:?})", type_id),
            })?;
        let size = type_info.fields.len();

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
                // Check if type_id refers to a partial type
                let expected_type = if self
                    .lookup_type(&type_id)
                    .is_some_and(|info| info.is_partial)
                {
                    Type::Partial(type_id)
                } else {
                    Type::Tuple(type_id)
                };
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
        if let Some(process) = self.get_process_mut(pid)
            && let Some(frame) = process.frames.last_mut()
        {
            // Jump modifies counter directly
            // Add 1 to offset because in the old code, Jump got the centralized increment
            frame.counter = frame.counter.wrapping_add_signed(offset + 1);
        }
        Ok(None)
    }

    fn handle_jump_if(&mut self, pid: ProcessId, offset: isize) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let condition = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let should_jump = !condition.is_nil();

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
                // Verify function exists
                self.get_function(function_index)
                    .ok_or(Error::FunctionUndefined(function_index))?;

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
                    .push(Frame::new(function_index, locals_base, captures_count));

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
            let function_index = frame.function_index;

            // Clear current frame's locals, but keep captures
            process.locals.truncate(locals_base + captures_count);

            process.stack.push(argument);
            *process.frames.last_mut().unwrap() =
                Frame::new(function_index, locals_base, captures_count);

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
                Value::Function(function_index, captures) => {
                    // Verify function exists
                    self.get_function(function_index)
                        .ok_or(Error::FunctionUndefined(function_index))?;

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
                        Frame::new(function_index, locals_base, captures_count);

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
            .extend(std::iter::repeat_n(Value::nil(), count));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_builtin(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action>, Error> {
        let builtin_name = self
            .get_builtin(index)
            .ok_or(Error::BuiltinUndefined(index))?
            .name
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

        let result = if value.is_nil() {
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

    fn handle_spawn(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        // Check if we're inside a receive function
        let is_receiving = self
            .get_process(pid)
            .and_then(|p| p.select_state.as_ref())
            .and_then(|s| s.receiving.as_ref())
            .is_some();

        if is_receiving {
            return Err(Error::OperationNotAllowed {
                operation: "spawn".to_string(),
                context: "receive function".to_string(),
            });
        }

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

        // Mark caller as waiting - will be notified with Value::Pid(new_pid)
        self.mark_waiting(pid);

        // Return routing request for scheduler to handle
        // Don't increment counter - will be incremented in notify_spawn
        Ok(Some(Action::Spawn {
            caller: pid,
            function_index,
            captures,
        }))
    }

    fn handle_send(&mut self, pid: ProcessId) -> Result<Option<Action>, Error> {
        // Check if we're inside a receive function
        let is_receiving = self
            .get_process(pid)
            .and_then(|p| p.select_state.as_ref())
            .and_then(|s| s.receiving.as_ref())
            .is_some();

        if is_receiving {
            return Err(Error::OperationNotAllowed {
                operation: "send".to_string(),
                context: "receive function".to_string(),
            });
        }

        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let pid_value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let message = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let target_pid = match pid_value {
            Value::Process(target, _) => target,
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

        let function_index = process.function_index;
        process.stack.push(Value::Process(pid, function_index));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    /// Check if we're continuing from a receive function call and pop result if needed
    fn handle_select_continuation(&mut self, pid: ProcessId) -> Result<Option<Value>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let Some(ref select_state) = process.select_state else {
            return Ok(None); // Not a continuation
        };

        // Verify we're handling the same select instruction
        let current_frame = process.frames.len().saturating_sub(1);
        let current_instruction = process.frames.last().map(|f| f.counter).unwrap_or(0);

        if select_state.frame != current_frame || select_state.instruction != current_instruction {
            return Err(Error::InvalidArgument(
                "Cannot nest select instructions (select in receive handler)".to_string(),
            ));
        }

        // If we just finished executing a receive function, pop the result
        if select_state.receiving.is_some() {
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            Ok(Some(process.stack.pop().ok_or(Error::StackUnderflow)?))
        } else {
            Ok(None)
        }
    }

    /// Initialize select state on first execution
    fn initialize_select(
        &mut self,
        pid: ProcessId,
        n: usize,
        current_time_ms: u64,
    ) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Validate stack has enough sources
        if process.stack.len() < n {
            return Err(Error::InvalidArgument(format!(
                "Select requires {} sources on stack, found {}",
                n,
                process.stack.len()
            )));
        }

        // Pop sources from stack
        let start_idx = process.stack.len() - n;
        let sources: Vec<Value> = process.stack.drain(start_idx..).collect();

        // Count receive sources to initialize cursors
        let receive_count = sources
            .iter()
            .filter(|s| matches!(s, Value::Function(_, _) | Value::Builtin(_)))
            .count();

        // Scan for process sources to determine if we need to await
        let pid_targets: Vec<ProcessId> = sources
            .iter()
            .filter_map(|s| {
                if let Value::Process(p, _) = s {
                    Some(*p)
                } else {
                    None
                }
            })
            .collect();

        let current_frame = process.frames.len().saturating_sub(1);
        let current_instruction = process.frames.last().map(|f| f.counter).unwrap_or(0);

        // If we have PIDs, defer start_time until await completes
        let start_time = if pid_targets.is_empty() {
            Some(current_time_ms)
        } else {
            None
        };

        process.select_state = Some(SelectState {
            frame: current_frame,
            instruction: current_instruction,
            sources,
            cursors: vec![0; receive_count],
            start_time,
            receiving: None,
        });

        // If we found PIDs, register awaits before processing sources
        if !pid_targets.is_empty() {
            for target in &pid_targets {
                process.awaiting.insert(*target, None);
            }

            self.mark_waiting(pid);
            return Ok(Some(Action::Await {
                targets: pid_targets,
                caller: pid,
            }));
        }

        Ok(None)
    }

    /// Ensure select start time is set (lazily after awaits complete)
    fn ensure_select_start_time(
        &mut self,
        pid: ProcessId,
        current_time_ms: u64,
    ) -> Result<u64, Error> {
        let process = self
            .get_process(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let select_state = process
            .select_state
            .as_ref()
            .ok_or(Error::InvalidArgument("Select state missing".to_string()))?;

        if let Some(t) = select_state.start_time {
            Ok(t)
        } else {
            // Set it now - awaits are complete, time to start evaluating
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            if let Some(ref mut state) = process.select_state {
                state.start_time = Some(current_time_ms);
            }
            Ok(current_time_ms)
        }
    }

    /// Process select sources in order, completing when a source is ready
    fn process_select_sources(
        &mut self,
        pid: ProcessId,
        receive_result: Option<Value>,
        start_time: u64,
        current_time_ms: u64,
    ) -> Result<Option<Action>, Error> {
        let select_state = self
            .get_process(pid)
            .and_then(|p| p.select_state.clone())
            .ok_or(Error::InvalidArgument("Select state missing".to_string()))?;

        for (src_idx, source) in select_state.sources.iter().enumerate() {
            match source {
                Value::Integer(timeout_ms) => {
                    if let Some(value) =
                        self.handle_select_timeout(*timeout_ms, start_time, current_time_ms)?
                    {
                        return self.complete_select(pid, value);
                    }
                }
                Value::Process(target_pid, _) => {
                    if let Some(value) = self.handle_select_process(pid, *target_pid)? {
                        return self.complete_select(pid, value);
                    }
                }
                Value::Function(_, _) | Value::Builtin(_) => {
                    // Receive sources may complete, call a function, or continue to next source
                    match self.handle_select_receive(
                        pid,
                        src_idx,
                        source,
                        &select_state,
                        receive_result.as_ref(),
                    )? {
                        SelectResult::Complete(value) => {
                            return self.complete_select(pid, value);
                        }
                        SelectResult::CalledFunction => {
                            // Receive function was called, return Ok(None) to let it execute
                            return Ok(None);
                        }
                        SelectResult::Continue => {
                            // No match, continue to next source
                        }
                    }
                }
                _ => {
                    return Err(Error::InvalidArgument(format!(
                        "Invalid select source: {:?}",
                        source
                    )));
                }
            }
        }

        // No sources ready - mark as waiting
        self.mark_waiting(pid);
        Ok(None)
    }

    /// Check if timeout has elapsed, returning nil if so
    fn handle_select_timeout(
        &mut self,
        timeout_ms: i64,
        start_time: u64,
        current_time_ms: u64,
    ) -> Result<Option<Value>, Error> {
        let elapsed = current_time_ms.saturating_sub(start_time);
        if elapsed >= timeout_ms.max(0) as u64 {
            Ok(Some(Value::nil()))
        } else {
            Ok(None)
        }
    }

    /// Check if an awaited process has completed, returning its result if so
    fn handle_select_process(
        &mut self,
        pid: ProcessId,
        target_pid: ProcessId,
    ) -> Result<Option<Value>, Error> {
        let process = self
            .get_process(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Check if result is available (we've already awaited upfront)
        if let Some(result_opt) = process.awaiting.get(&target_pid)
            && let Some(result) = result_opt
        {
            return Ok(Some(result.clone()));
        }

        Ok(None)
    }

    /// Handle a receive source, checking for completed receive or scanning mailbox
    fn handle_select_receive(
        &mut self,
        pid: ProcessId,
        src_idx: usize,
        source: &Value,
        select_state: &SelectState,
        receive_result: Option<&Value>,
    ) -> Result<SelectResult, Error> {
        // Calculate receive function index (count of receive sources before this one)
        let receive_idx = select_state.sources[..src_idx]
            .iter()
            .filter(|s| matches!(s, Value::Function(_, _) | Value::Builtin(_)))
            .count();

        // Check if we just finished executing this receive function
        if let Some((idx, message_value)) = &select_state.receiving
            && *idx == receive_idx
            && let Some(value) =
                self.handle_receive_result(pid, receive_idx, message_value, receive_result)?
        {
            return Ok(SelectResult::Complete(value));
        }

        // Nil result - cursor was incremented, continue scanning mailbox
        // Or we haven't checked this receive source yet
        self.scan_mailbox_for_message(pid, receive_idx, source, select_state)
    }

    /// Handle the result from a just-executed receive function
    fn handle_receive_result(
        &mut self,
        pid: ProcessId,
        receive_idx: usize,
        message_value: &Value,
        receive_result: Option<&Value>,
    ) -> Result<Option<Value>, Error> {
        // Use the result we popped earlier (in handle_select_continuation)
        let result = receive_result.ok_or(Error::InvalidArgument(
            "Receive result should be present when receiving is set".to_string(),
        ))?;

        // Check if result is nil or Ok
        if !result.is_nil() && !result.is_ok() {
            return Err(Error::InvalidArgument(
                "Receive function must return [] or Ok".to_string(),
            ));
        }

        if result.is_ok() {
            // Ok result - remove message from mailbox and complete
            let process = self
                .get_process(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            let select_state = process
                .select_state
                .as_ref()
                .ok_or(Error::InvalidArgument("Select state missing".to_string()))?;
            let msg_idx = select_state.cursors.get(receive_idx).copied().unwrap_or(0);

            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            if msg_idx < process.mailbox.len() {
                process.mailbox.remove(msg_idx);
            }
            Ok(Some(message_value.clone()))
        } else {
            // Nil result - increment cursor and reset receiving
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            if let Some(state) = &mut process.select_state {
                state.cursors[receive_idx] += 1;
                state.receiving = None;
            }
            Ok(None)
        }
    }

    /// Scan mailbox for a type-compatible message, calling receive function if found
    fn scan_mailbox_for_message(
        &mut self,
        pid: ProcessId,
        receive_idx: usize,
        source: &Value,
        select_state: &SelectState,
    ) -> Result<SelectResult, Error> {
        let mut cursor = self
            .get_process(pid)
            .and_then(|p| p.select_state.as_ref())
            .and_then(|s| s.cursors.get(receive_idx).copied())
            .unwrap_or(0);

        let process = self
            .get_process(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Loop through all messages starting from cursor
        for (msg_idx, message) in process.mailbox.iter().enumerate().skip(cursor) {
            // Check if type is compatible
            let expected_type = match source {
                Value::Function(func_id, _) => self
                    .functions
                    .get(*func_id)
                    .and_then(|f| f.function_type.as_ref())
                    .map(|ft| &ft.parameter),
                Value::Builtin(name) => self
                    .builtins
                    .iter()
                    .find(|b| &b.name == name)
                    .map(|b| &b.parameter_type),
                _ => unreachable!(),
            };

            let message_type = self.value_to_type(message);
            let type_compatible = expected_type
                .map(|expected| message_type.is_compatible(expected, self))
                .unwrap_or(true);

            if type_compatible {
                // Found a compatible message
                let message = message.clone();

                // Check if this is an identity function (no body)
                let is_identity = match source {
                    Value::Function(func_id, _) => self
                        .functions
                        .get(*func_id)
                        .map(|f| f.instructions.is_empty())
                        .unwrap_or(false),
                    Value::Builtin(_) => false,
                    _ => unreachable!(),
                };

                if is_identity {
                    // Identity function - skip calling, just complete with message
                    let process = self
                        .get_process_mut(pid)
                        .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                    if msg_idx < process.mailbox.len() {
                        process.mailbox.remove(msg_idx);
                    }
                    return Ok(SelectResult::Complete(message));
                } else {
                    // Function has a body - set receiving state and call it
                    self.call_receive_function(pid, receive_idx, msg_idx, message, source)?;
                    return Ok(SelectResult::CalledFunction);
                }
            } else {
                // Type not compatible - update cursor to skip this message
                cursor = msg_idx + 1;
            }
        }

        // Update cursor to reflect all skipped messages
        if cursor > select_state.cursors.get(receive_idx).copied().unwrap_or(0) {
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            if let Some(state) = &mut process.select_state
                && receive_idx < state.cursors.len()
            {
                state.cursors[receive_idx] = cursor;
            }
        }

        Ok(SelectResult::Continue)
    }

    /// Call a receive function and prepare for re-entry
    fn call_receive_function(
        &mut self,
        pid: ProcessId,
        receive_idx: usize,
        msg_idx: usize,
        message: Value,
        source: &Value,
    ) -> Result<(), Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        if let Some(state) = &mut process.select_state {
            state.receiving = Some((receive_idx, message.clone()));
            state.cursors[receive_idx] = msg_idx;
        }

        process.stack.push(message);
        process.stack.push(source.clone());

        // Call the function - when it returns, handle_select will be called again
        self.handle_call(pid)?;
        Ok(())
    }

    fn handle_select(
        &mut self,
        pid: ProcessId,
        n: usize,
        current_time_ms: u64,
    ) -> Result<Option<Action>, Error> {
        // Phase 1: Check if we're continuing from a receive function call
        let receive_result = self.handle_select_continuation(pid)?;

        // Phase 2: Initialize if this is the first time
        if receive_result.is_none() {
            let has_select_state = self
                .get_process(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?
                .select_state
                .is_some();

            if !has_select_state {
                return self.initialize_select(pid, n, current_time_ms);
            }
        }

        // Phase 3: Ensure start time is set (lazily after awaits complete)
        let start_time = self.ensure_select_start_time(pid, current_time_ms)?;

        // Phase 4: Process sources by type
        self.process_select_sources(pid, receive_result, start_time, current_time_ms)
    }

    /// Complete a select by cleaning up state and pushing result
    fn complete_select(&mut self, pid: ProcessId, result: Value) -> Result<Option<Action>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Clean up select state
        process.select_state = None;

        // Push result
        process.stack.push(result);

        // Increment frame counter
        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }

        Ok(None)
    }

    /// Convert a runtime Value to its Type representation
    fn value_to_type(&self, value: &Value) -> Type {
        match value {
            Value::Integer(_) => Type::Integer,
            Value::Binary(_) => Type::Binary,
            Value::Tuple(type_id, _) => {
                // Check if this is a partial type
                if self
                    .lookup_type(type_id)
                    .is_some_and(|info| info.is_partial)
                {
                    Type::Partial(*type_id)
                } else {
                    Type::Tuple(*type_id)
                }
            }
            Value::Function(func_idx, _) => {
                // Get the function's type signature
                if let Some(func_type) = self
                    .functions
                    .get(*func_idx)
                    .and_then(|f| f.function_type.as_ref())
                {
                    Type::Callable(Box::new(func_type.clone()))
                } else {
                    // Function without type information - shouldn't happen in well-typed code
                    // Return a generic callable type with unknown types
                    Type::Callable(Box::new(CallableType {
                        parameter: Type::Union(vec![]), // Bottom type (never)
                        result: Type::Union(vec![]),
                        receive: Type::Union(vec![]),
                    }))
                }
            }
            Value::Builtin(name) => {
                // Look up builtin type from the resolved builtins list
                let builtin_info = self
                    .builtins
                    .iter()
                    .find(|b| &b.name == name)
                    .expect("Builtin should be registered");
                Type::Callable(Box::new(CallableType {
                    parameter: builtin_info.parameter_type.clone(),
                    result: builtin_info.result_type.clone(),
                    receive: Type::Union(vec![]), // Builtins don't receive messages
                }))
            }
            Value::Process(_, function_idx) => {
                // Get the process type from the function that spawned it
                if let Some(func_type) = self
                    .functions
                    .get(*function_idx)
                    .and_then(|f| f.function_type.as_ref())
                {
                    Type::Process(Box::new(ProcessType {
                        receive: Some(Box::new(func_type.receive.clone())),
                        returns: Some(Box::new(func_type.result.clone())),
                    }))
                } else {
                    // Process without type information
                    Type::Process(Box::new(ProcessType {
                        receive: None,
                        returns: None,
                    }))
                }
            }
        }
    }

    fn check_expired_timeouts(&mut self, current_time_ms: u64) {
        // Scan waiting processes for expired select timeouts
        let expired: Vec<ProcessId> = self
            .waiting
            .iter()
            .filter(|pid| {
                if let Some(process) = self.get_process(**pid)
                    && let Some(ref select_state) = process.select_state
                    && let Some(start_time) = select_state.start_time
                {
                    // Check if any timeout sources have expired
                    let elapsed = current_time_ms.saturating_sub(start_time);
                    return select_state.sources.iter().any(|source| {
                        if let Value::Integer(timeout_ms) = source {
                            elapsed >= (*timeout_ms).max(0) as u64
                        } else {
                            false
                        }
                    });
                }
                false
            })
            .copied()
            .collect();

        // Re-queue expired processes to retry their Select instruction
        for pid in expired {
            self.queue.push_back(pid);
            self.waiting.remove(&pid);
        }
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
            (Value::Process(a, func_a), Value::Process(b, func_b)) => a == b && func_a == func_b,
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
            Value::Integer(_) | Value::Builtin(_) | Value::Process(_, _) => Ok(value.clone()),
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
        Self::inject_value_recursive(value, &remap)
    }

    fn inject_value_recursive(value: Value, remap: &HashMap<usize, usize>) -> Result<Value, Error> {
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
                    .map(|elem| Self::inject_value_recursive(elem, remap))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::Tuple(type_id, remapped_elements))
            }
            Value::Function(func_index, captures) => {
                // Recursively remap all captures
                let remapped_captures = captures
                    .into_iter()
                    .map(|cap| Self::inject_value_recursive(cap, remap))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::Function(func_index, remapped_captures))
            }
            // Other value types don't contain heap references
            Value::Integer(_) | Value::Builtin(_) | Value::Process(_, _) => Ok(value),
        }
    }
}
