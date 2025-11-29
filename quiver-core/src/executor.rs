use crate::binary::BinaryData;
use crate::bytecode::{ConcreteType, Constant, Function, Instruction};
use crate::effects::Effect;
use crate::error::Error;
use crate::process::{Action, Frame, Process, ProcessId, ProcessInfo, ProcessStatus, SelectState};
use crate::types::{BuiltinInfo, TupleTypeInfo, Type};
use crate::value::{Binary, MAX_BINARY_SIZE, Value};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::time::Instant;

/// Bundled program update data for incremental compilation.
/// Contains full tuple type information for merging with Environment's Program state.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramUpdate {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    /// Full tuple type information (name, fields, is_partial)
    pub tuples: Vec<TupleTypeInfo>,
    /// Types used by IsType instructions for pattern matching
    pub types: Vec<Type>,
    /// Builtin information (name and resolved types)
    pub builtins: Vec<BuiltinInfo>,
    pub resources: Vec<String>,
    /// For each type_id, the set of concrete types compatible with it (for IsType checks)
    pub type_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each function_id, the set of concrete types compatible with its parameter
    pub function_param_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each builtin_id, the set of concrete types compatible with its parameter
    pub builtin_param_compatibility: Vec<HashSet<ConcreteType>>,
}

/// Instruction type for profiling statistics (groups parameterized instructions)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InstructionType {
    Constant,
    Pop,
    Duplicate,
    Pick,
    Rotate,
    Reset,
    Load,
    Store,
    Tuple,
    Get,
    IsType,
    Jump,
    JumpIf,
    Call,
    TailCall,
    Function,
    Builtin,
    Equal,
    Not,
    Spawn,
    Send,
    Self_,
    Select,
    Process,
}

impl InstructionType {
    fn from_instruction(instr: &Instruction) -> Self {
        match instr {
            Instruction::Constant(_) => InstructionType::Constant,
            Instruction::Pop => InstructionType::Pop,
            Instruction::Duplicate => InstructionType::Duplicate,
            Instruction::Pick(_) => InstructionType::Pick,
            Instruction::Rotate(_) => InstructionType::Rotate,
            Instruction::Reset(_) => InstructionType::Reset,
            Instruction::Load(_) => InstructionType::Load,
            Instruction::Store => InstructionType::Store,
            Instruction::Tuple(_) => InstructionType::Tuple,
            Instruction::Get(_) => InstructionType::Get,
            Instruction::IsType(_) => InstructionType::IsType,
            Instruction::Jump(_) => InstructionType::Jump,
            Instruction::JumpIf(_) => InstructionType::JumpIf,
            Instruction::Call => InstructionType::Call,
            Instruction::TailCall(_) => InstructionType::TailCall,
            Instruction::Function(_) => InstructionType::Function,
            Instruction::Builtin(_) => InstructionType::Builtin,
            Instruction::Equal(_) => InstructionType::Equal,
            Instruction::Not => InstructionType::Not,
            Instruction::Spawn => InstructionType::Spawn,
            Instruction::Send => InstructionType::Send,
            Instruction::Self_ => InstructionType::Self_,
            Instruction::Select => InstructionType::Select,
            Instruction::Process(_, _) => InstructionType::Process,
        }
    }
}

/// Execution statistics for profiling
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ExecutionStats {
    /// Statistics by instruction type: (count, total_time_ns)
    pub instruction_stats: HashMap<InstructionType, (u64, u64)>,

    /// Per-builtin statistics by index: (count, total_time_ns)
    pub builtin_stats: HashMap<usize, (u64, u64)>,
}

impl ExecutionStats {
    pub fn new() -> Self {
        Self::default()
    }

    /// Merge another ExecutionStats into this one (for aggregating from multiple executors)
    pub fn merge(&mut self, other: &ExecutionStats) {
        for (key, (count, time)) in &other.instruction_stats {
            let entry = self.instruction_stats.entry(*key).or_insert((0, 0));
            entry.0 += count;
            entry.1 += time;
        }
        for (idx, (count, time)) in &other.builtin_stats {
            let entry = self.builtin_stats.entry(*idx).or_insert((0, 0));
            entry.0 += count;
            entry.1 += time;
        }
    }

    pub fn total_instructions(&self) -> u64 {
        self.instruction_stats
            .values()
            .map(|(count, _)| count)
            .sum()
    }

    pub fn total_time_ns(&self) -> u64 {
        self.instruction_stats.values().map(|(_, time)| time).sum()
    }
}

/// Result of processing a select source
enum SelectResult {
    /// Select should complete with this value
    Complete(Value),
    /// A receive function was called, return from handle_select to let it execute
    CalledFunction,
    /// Continue to the next source
    Continue,
}

pub struct Executor<E: Effect> {
    processes: HashMap<ProcessId, Process>,
    process_function_indices: HashMap<ProcessId, usize>, // Maps process ID to its function index (for REPL)
    queue: VecDeque<ProcessId>,
    spawning: HashSet<ProcessId>,
    selecting: HashSet<ProcessId>,
    effecting: HashSet<ProcessId>,
    // Program data owned by executor
    constants: Vec<Constant>,
    functions: Vec<Function>,
    builtins: Vec<String>,  // Builtin names (for effect dispatch)
    tuples: Vec<usize>,     // Tuple arities
    resources: Vec<String>, // Resource type names
    /// For each type_id, the set of concrete types compatible with it (for IsType checks)
    type_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each function_id, the set of concrete types compatible with its parameter
    function_param_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each builtin_id, the set of concrete types compatible with its parameter
    builtin_param_compatibility: Vec<HashSet<ConcreteType>>,
    // Heap for runtime-allocated binaries (using BinaryData for O(1) operations)
    heap: Vec<BinaryData>,
    // Builtin registry for executing builtin functions
    builtins_registry: crate::builtins::BuiltinRegistry<E>,
    // Profiling
    pub stats: ExecutionStats,
    profile: bool,
}

// Note: TupleLookup is not implemented for Executor anymore since tuples only stores arities
// Type compatibility is now precomputed at compile time
impl<E: Effect> Executor<E> {
    pub fn get_constant(&self, index: usize) -> Option<&Constant> {
        self.constants.get(index)
    }

    /// Get BinaryData from heap by index
    pub fn get_heap_binary(&self, index: usize) -> Option<&BinaryData> {
        self.heap.get(index)
    }

    /// Get BinaryData from either constants or heap
    pub fn get_binary_data(&self, binary: &Binary) -> Result<&BinaryData, Error> {
        match binary {
            Binary::Constant(index) => {
                let constant = self
                    .get_constant(*index)
                    .ok_or(Error::ConstantUndefined(*index))?;
                match constant {
                    Constant::Binary(_bytes) => {
                        // For constants, we need to return a reference, but we only have Vec<u8>
                        // This is a limitation - we'll need to handle this differently
                        // For now, return an error - we'll fix this properly
                        Err(Error::InvalidArgument(
                            "Getting BinaryData from constant not yet implemented".to_string(),
                        ))
                    }
                    _ => Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    }),
                }
            }
            Binary::Heap(index) => self.heap.get(*index).ok_or_else(|| {
                Error::InvalidArgument(format!("Heap binary index {} not found", index))
            }),
        }
    }

    /// Allocate BinaryData on the heap and return a Binary reference
    pub fn allocate_binary_data(&mut self, data: BinaryData) -> Result<Binary, Error> {
        if data.len() > MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                data.len(),
                MAX_BINARY_SIZE
            )));
        }
        let index = self.heap.len();
        self.heap.push(data);
        Ok(Binary::Heap(index))
    }

    /// Create a binary from Vec<u8>
    pub fn allocate_binary(&mut self, bytes: Vec<u8>) -> Result<Binary, Error> {
        self.allocate_binary_data(BinaryData::new(bytes))
    }

    pub fn new(builtins_registry: crate::builtins::BuiltinRegistry<E>, profile: bool) -> Self {
        // Pre-initialize with NIL (index 0) and OK (index 1) tuple arities.
        // This matches Program::new() so incremental updates are consistent.
        Self {
            processes: HashMap::new(),
            process_function_indices: HashMap::new(),
            queue: VecDeque::new(),
            spawning: HashSet::new(),
            selecting: HashSet::new(),
            effecting: HashSet::new(),
            constants: vec![],
            functions: vec![],
            builtins: vec![],
            tuples: vec![0, 0], // NIL and OK have 0 fields
            resources: vec![],
            type_compatibility: vec![],
            function_param_compatibility: vec![],
            builtin_param_compatibility: vec![],
            heap: vec![],
            builtins_registry,
            stats: ExecutionStats::new(),
            profile,
        }
    }

    /// Spawn a new process
    /// If function_index is None, creates a sleeping process ready for resume (used by REPL)
    pub fn spawn_process(
        &mut self,
        id: ProcessId,
        function_index: Option<usize>,
        captures: Vec<Value>,
        argument: Value,
        heap_data: Vec<Vec<u8>>,
        persistent: bool,
    ) -> Result<(), Error> {
        // Create the process
        let mut process = Process::new(persistent);

        // If no function, create a sleeping process directly
        let Some(function_index) = function_index else {
            process.result = Some(Ok(Value::nil()));
            self.processes.insert(id, process);
            // Not added to queue - it's sleeping, waiting for resume
            return Ok(());
        };

        self.processes.insert(id, process);

        // Inject heap data and populate locals with captures
        let captures_count = captures.len();
        for value in captures {
            let injected = self.inject_heap_data(value, &heap_data)?;

            let process = self
                .get_process_mut(id)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            process.locals.push(injected);
        }

        // Push argument onto stack
        let injected_arg = self.inject_heap_data(argument, &heap_data)?;
        let process = self
            .get_process_mut(id)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.stack.push(injected_arg);

        // Push initial frame with function index
        let process = self
            .get_process_mut(id)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.frames.push(crate::process::Frame::new(
            function_index,
            0,
            captures_count,
        ));

        // Cache the function index for REPL references
        self.set_process_function_index(id, function_index);

        // Add to queue
        self.queue.push_back(id);

        Ok(())
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

    pub fn get_builtins_registry(&self) -> &crate::builtins::BuiltinRegistry<E> {
        &self.builtins_registry
    }

    pub fn suspend_process(&mut self, id: ProcessId) {
        self.queue.retain(|&pid| pid != id);
    }

    /// Notify a process that spawned a new process with the new PID
    pub fn notify_spawn(&mut self, id: ProcessId, pid: Value) {
        let was_spawning = self.spawning.remove(&id);

        if let Some(process) = self.processes.get_mut(&id) {
            // For spawn notifications, just push the PID onto the stack and increment counter
            process.stack.push(pid);

            if let Some(frame) = process.frames.last_mut() {
                frame.counter += 1;
            }

            // Only re-queue if it was actually spawning (not already queued by something else)
            if was_spawning {
                self.queue.push_back(id);
            }
        }
    }

    /// Notify a process that was waiting for a result with the result value
    pub fn notify_result(
        &mut self,
        awaiter: ProcessId,
        awaited: ProcessId,
        result: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), Error> {
        // Inject heap data into the result value
        let injected_result = self.inject_heap_data(result, &heap)?;

        // Store the result in the process's awaiting map
        if let Some(process) = self.get_process_mut(awaiter) {
            process.awaiting.insert(awaited, Some(injected_result));
        }

        // Re-queue awaiter to retry its Select instruction
        if self.selecting.remove(&awaiter) {
            self.queue.push_back(awaiter);
        }

        Ok(())
    }

    /// Notify a process that an effect operation completed
    pub fn notify_effect_completion(
        &mut self,
        process_id: ProcessId,
        result: Result<Value, String>,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), Error> {
        let was_effecting = self.effecting.remove(&process_id);

        // Convert result to either Ok(Value) or Err(Error)
        let value_result = match result {
            Ok(v) => Ok(self.inject_heap_data(v, &heap)?),
            Err(err_msg) => Err(Error::InvalidArgument(format!(
                "Effect operation failed: {}",
                err_msg
            ))),
        };

        // Get process and update based on result
        let process = self
            .get_process_mut(process_id)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        match value_result {
            Ok(value) => {
                // Success: push value and increment counter
                process.stack.push(value);
                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }
            }
            Err(error) => {
                // Error: set error and terminate the process
                process.result = Some(Err(error));
                process.frames.clear();
            }
        }

        // Re-queue if it was effecting
        if was_effecting {
            self.queue.push_back(process_id);
        }

        Ok(())
    }

    pub fn notify_message(
        &mut self,
        id: ProcessId,
        message: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), Error> {
        // Inject heap data into the message value
        let injected_message = self.inject_heap_data(message, &heap)?;

        if let Some(process) = self.processes.get_mut(&id) {
            process.mailbox.push_back(injected_message);
        }

        // Re-queue if the process is selecting (waiting for messages)
        if self.selecting.remove(&id) {
            self.queue.push_back(id);
        }

        Ok(())
    }

    pub fn mark_spawning(&mut self, id: ProcessId) {
        self.spawning.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_selecting(&mut self, id: ProcessId) {
        self.selecting.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_effecting(&mut self, id: ProcessId) {
        self.effecting.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_active(&mut self, id: ProcessId) {
        let was_spawning = self.spawning.remove(&id);
        let was_selecting = self.selecting.remove(&id);
        if was_spawning || was_selecting {
            self.queue.push_back(id);
        }
    }

    pub fn add_to_queue(&mut self, process_id: ProcessId) {
        self.queue.push_back(process_id);
    }

    fn get_status(&self, id: ProcessId, process: &Process) -> ProcessStatus {
        if self.queue.contains(&id) {
            ProcessStatus::Active
        } else if self.spawning.contains(&id)
            || self.selecting.contains(&id)
            || self.effecting.contains(&id)
        {
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

    /// Get process function indices for REPL process references
    /// Returns a map from process ID to the function index that process is running
    pub fn get_process_function_indices(&self) -> HashMap<ProcessId, usize> {
        self.process_function_indices.clone()
    }

    /// Record the function index for a process (called on spawn and resume)
    pub fn set_process_function_index(&mut self, id: ProcessId, function_index: usize) {
        self.process_function_indices.insert(id, function_index);
    }

    pub fn get_process_info(&self, id: ProcessId) -> Option<ProcessInfo> {
        self.processes.get(&id).map(|process| {
            // Extract heap data from result if present
            let result = match &process.result {
                Some(Ok(value)) => match self.extract_heap_data(value) {
                    Ok((extracted_value, heap)) => Some(Ok((extracted_value, heap))),
                    Err(_) => process.result.clone().map(|r| r.map(|v| (v, vec![]))),
                },
                Some(Err(e)) => Some(Err(e.clone())),
                None => None,
            };

            // Get function_index from cached entry point (preferred for process type)
            // Falls back to frames for processes that weren't tracked (shouldn't happen normally)
            let function_index = self
                .process_function_indices
                .get(&id)
                .copied()
                .or_else(|| process.frames.first().map(|f| f.function_index));

            ProcessInfo {
                id,
                status: self.get_status(id, process),
                function_index,
                stack_size: process.stack.len(),
                locals_count: process.locals.len(),
                frames_count: process.frames.len(),
                mailbox_size: process.mailbox.len(),
                persistent: process.persistent,
                result,
            }
        })
    }

    // Program data accessors
    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    /// Get builtin name by index
    pub fn get_builtin_name(&self, index: usize) -> Option<&str> {
        self.builtins.get(index).map(|s| s.as_str())
    }

    /// Re-queue a process for execution (e.g., after I/O completion)
    pub fn requeue_process(&mut self, process_id: ProcessId) {
        self.queue.push_back(process_id);
    }

    /// Remove a process from the execution queue
    pub fn remove_from_queue(&mut self, process_id: ProcessId) {
        self.queue.retain(|&p| p != process_id);
    }

    /// Update executor with program data (appends to existing data).
    ///
    /// Appends new constants, functions, tuples, and builtins to the existing program state.
    /// Compatibility tables are replaced (they should be recomputed for the full program).
    ///
    /// On a fresh executor (empty state), this is equivalent to initializing with complete data.
    pub fn update_program(&mut self, update: ProgramUpdate) {
        self.constants.extend(update.constants);
        self.functions.extend(update.functions);
        // Extract arities from TupleTypeInfo - executor only needs arities at runtime
        self.tuples
            .extend(update.tuples.iter().map(|t| t.fields.len()));
        self.builtins
            .extend(update.builtins.iter().map(|b| b.name.clone()));
        self.resources = update.resources;
        self.type_compatibility = update.type_compatibility;
        self.function_param_compatibility = update.function_param_compatibility;
        self.builtin_param_compatibility = update.builtin_param_compatibility;
    }

    /// Execute up to max_units instruction units for a single process.
    /// Returns (did_work, optional_action) where did_work indicates if any instructions were executed.
    pub fn step(&mut self, max_units: usize, current_time_ms: u64) -> (bool, Option<Action<E>>) {
        // Check for expired timeouts before processing
        self.check_expired_timeouts(current_time_ms);
        // Pop process from queue
        let Some(current_pid) = self.queue.pop_front() else {
            return (false, None); // No processes to run
        };

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
                        process.frames.clear();
                    }
                }
            }

            // Check if process should yield (moved to spawning/selecting/effecting, or has pending request)
            // Pending request check ensures only ONE routing request per step
            if self.spawning.contains(&current_pid)
                || self.selecting.contains(&current_pid)
                || self.effecting.contains(&current_pid)
                || pending_request.is_some()
            {
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

            // Clear locals from the popped frame (including captures)
            // For persistent processes, only keep locals if this was the last (top-level) frame
            let should_clear_locals = !process.persistent || !is_last_frame;
            if should_clear_locals {
                process.locals.truncate(frame.locals_base);
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
                        return (true, None); // Did work but hit error
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

            // Get the process result to check if it's an error
            let process_result = self.get_process(current_pid).and_then(|p| p.result.clone());

            for awaiter in awaiters {
                match &process_result {
                    Some(Ok(_)) => {
                        // Success - notify with the result value
                        self.notify_result(awaiter, current_pid, result_value.clone(), vec![])
                            .ok(); // Ignore errors since this is internal notification
                    }
                    Some(Err(error)) => {
                        // Error - propagate to awaiter by setting their result
                        if let Some(awaiter_process) = self.get_process_mut(awaiter) {
                            awaiter_process.result = Some(Err(error.clone()));
                            awaiter_process.frames.clear();
                        }
                    }
                    None => {
                        // No result yet (shouldn't happen at this point)
                        self.notify_result(awaiter, current_pid, result_value.clone(), vec![])
                            .ok();
                    }
                }
            }
        } else {
            let should_requeue =
                !self.spawning.contains(&current_pid) && !self.selecting.contains(&current_pid);

            if should_requeue {
                // Process not finished - re-queue it so it can continue
                // This handles both: time slice exhaustion AND yielding after routing (e.g., Send)
                self.queue.push_back(current_pid);
            }
        }

        // Return (did_work=true, pending_request) - we always do work if we got here
        (true, pending_request)
    }

    fn execute_instruction(
        &mut self,
        pid: ProcessId,
        instruction: Instruction,
        current_time_ms: u64,
    ) -> Result<Option<Action<E>>, Error> {
        let start = if self.profile {
            Some(Instant::now())
        } else {
            None
        };

        let result = match instruction {
            Instruction::Constant(index) => self.handle_constant(pid, index),
            Instruction::Pop => self.handle_pop(pid),
            Instruction::Duplicate => self.handle_duplicate(pid),
            Instruction::Pick(n) => self.handle_pick(pid, n),
            Instruction::Rotate(n) => self.handle_rotate(pid, n),
            Instruction::Load(index) => self.handle_load(pid, index),
            Instruction::Store => self.handle_store(pid),
            Instruction::Tuple(type_id) => self.handle_tuple(pid, type_id),
            Instruction::Get(index) => self.handle_get(pid, index),
            Instruction::IsType(type_id) => self.handle_is_type(pid, type_id),
            Instruction::Jump(offset) => self.handle_jump(pid, offset),
            Instruction::JumpIf(offset) => self.handle_jump_if(pid, offset),
            Instruction::Call => self.handle_call(pid),
            Instruction::TailCall(recurse) => self.handle_tail_call(pid, recurse),
            Instruction::Function(function_index) => self.handle_function(pid, function_index),
            Instruction::Reset(index) => self.handle_reset(pid, index),
            Instruction::Builtin(index) => self.handle_builtin(pid, index),
            Instruction::Equal(count) => self.handle_equal(pid, count),
            Instruction::Not => self.handle_not(pid),
            Instruction::Spawn => self.handle_spawn(pid),
            Instruction::Send => self.handle_send(pid),
            Instruction::Self_ => self.handle_self(pid),
            Instruction::Select => self.handle_select(pid, current_time_ms),
            Instruction::Process(process_id, function_index) => {
                self.handle_process_ref(pid, process_id, function_index)
            }
        };

        if let Some(start) = start {
            let elapsed = start.elapsed().as_nanos() as u64;
            let instr_type = InstructionType::from_instruction(&instruction);
            let entry = self
                .stats
                .instruction_stats
                .entry(instr_type)
                .or_insert((0, 0));
            entry.0 += 1;
            entry.1 += elapsed;
        }

        result
    }

    fn handle_constant(
        &mut self,
        pid: ProcessId,
        index: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let constant = self
            .get_constant(index)
            .ok_or(Error::ConstantUndefined(index))?;
        let value = match constant {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Binary(bytes) => {
                // Allocate constant binary to heap and get reference
                // TODO: Cache this so we don't re-allocate the same constant multiple times
                let binary_ref = self.allocate_binary(bytes.clone())?;
                Value::Binary(binary_ref)
            }
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

    fn handle_pop(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.stack.pop().ok_or(Error::StackUnderflow)?;

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_duplicate(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
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

    fn handle_pick(&mut self, pid: ProcessId, n: usize) -> Result<Option<Action<E>>, Error> {
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

    fn handle_rotate(&mut self, pid: ProcessId, n: usize) -> Result<Option<Action<E>>, Error> {
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

    fn handle_load(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let frame = process.frames.last().ok_or(Error::FrameUnderflow)?;
        let actual_index = frame.locals_base + index;

        let value = process
            .locals
            .get(actual_index)
            .cloned()
            .ok_or_else(|| Error::VariableUndefined(format!("local[{}]", index)))?;

        process.stack.push(value);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_store(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        process.locals.push(value);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_tuple(&mut self, pid: ProcessId, type_id: usize) -> Result<Option<Action<E>>, Error> {
        // tuples now stores arities directly
        let size = *self
            .tuples
            .get(type_id)
            .ok_or_else(|| Error::TypeMismatch {
                expected: "known tuple type".to_string(),
                found: format!("unknown type ({:?})", type_id),
            })?;

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

    fn handle_get(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action<E>>, Error> {
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

    fn handle_is_type(
        &mut self,
        pid: ProcessId,
        pattern_type_id: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        // Use precomputed type compatibility instead of runtime type checking
        let is_match = self.check_type_compatible(&value, pattern_type_id);

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

    /// Check if a value is compatible with a pattern type using precomputed compatibility table
    fn check_type_compatible(&self, value: &Value, pattern_type_id: usize) -> bool {
        let concrete = self.get_concrete_type(value);
        self.type_compatibility
            .get(pattern_type_id)
            .map(|set| set.contains(&concrete))
            .unwrap_or(false)
    }

    /// Get the concrete type for a runtime value using O(1) lookup
    fn get_concrete_type(&self, value: &Value) -> ConcreteType {
        match value {
            Value::Integer(_) => ConcreteType::Integer,
            Value::Binary(_) => ConcreteType::Binary,
            Value::Tuple(tuple_id, _) => ConcreteType::Tuple(*tuple_id),
            Value::Function(func_id, _) => ConcreteType::Function(*func_id),
            Value::Builtin(builtin_id) => ConcreteType::Builtin(*builtin_id),
            Value::Process(_, func_id) => ConcreteType::Process(*func_id),
            Value::Resource(_, resource_type_id) => ConcreteType::Resource(*resource_type_id),
        }
    }

    /// Check if a message is compatible with a function/builtin's parameter type
    fn check_message_compatible(&self, message: &Value, source: &Value) -> bool {
        let concrete = self.get_concrete_type(message);
        match source {
            Value::Function(func_id, _) => self
                .function_param_compatibility
                .get(*func_id)
                .map(|set| set.contains(&concrete))
                .unwrap_or(true),
            Value::Builtin(builtin_id) => self
                .builtin_param_compatibility
                .get(*builtin_id)
                .map(|set| set.contains(&concrete))
                .unwrap_or(true),
            _ => true,
        }
    }

    fn handle_jump(&mut self, pid: ProcessId, offset: isize) -> Result<Option<Action<E>>, Error> {
        if let Some(process) = self.get_process_mut(pid)
            && let Some(frame) = process.frames.last_mut()
        {
            // Jump modifies counter directly
            // Add 1 to offset because in the old code, Jump got the centralized increment
            frame.counter = frame.counter.wrapping_add_signed(offset + 1);
        }
        Ok(None)
    }

    fn handle_jump_if(
        &mut self,
        pid: ProcessId,
        offset: isize,
    ) -> Result<Option<Action<E>>, Error> {
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

    fn handle_call(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
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
            Value::Builtin(builtin_id) => {
                // Pop function and parameter
                let parameter = {
                    let process = self
                        .get_process_mut(pid)
                        .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                    process.stack.pop(); // Pop function
                    process.stack.pop().ok_or(Error::StackUnderflow)?
                };

                // Look up builtin name for registry lookup
                let builtin_name = self
                    .builtins
                    .get(builtin_id)
                    .ok_or_else(|| {
                        Error::InvalidArgument(format!("Unknown builtin id: {}", builtin_id))
                    })?
                    .clone();

                let builtin = self
                    .builtins_registry
                    .get_implementation(&builtin_name)
                    .ok_or_else(|| {
                        Error::InvalidArgument(format!("Unrecognised builtin: {}", builtin_name))
                    })?;

                let start = if self.profile {
                    Some(Instant::now())
                } else {
                    None
                };

                let result = builtin(pid, &parameter, self)?;

                if let Some(start) = start {
                    let elapsed = start.elapsed().as_nanos() as u64;
                    let entry = self.stats.builtin_stats.entry(builtin_id).or_insert((0, 0));
                    entry.0 += 1;
                    entry.1 += elapsed;
                }

                // Handle both immediate and action results
                match result {
                    crate::builtins::BuiltinResult::Value(value) => {
                        // Immediate result: push value and increment counter
                        let process = self
                            .get_process_mut(pid)
                            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

                        process.stack.push(value);

                        // Unlike regular calls, builtins don't create a new frame
                        // So we need to manually increment the counter
                        if let Some(frame) = process.frames.last_mut() {
                            frame.counter += 1;
                        }
                        Ok(None)
                    }
                    crate::builtins::BuiltinResult::Action(action) => {
                        // Check if this is an effect request and mark process as effecting
                        if let Action::RequestEffect { process_id, .. } = &action {
                            self.mark_effecting(*process_id);
                        }

                        // Action result: return the action for Environment to handle
                        // Don't increment counter - will be incremented in notify_* method
                        Ok(Some(action))
                    }
                }
            }
            _ => Err(Error::TypeMismatch {
                expected: "function".to_string(),
                found: function_value.type_name().to_string(),
            }),
        }
    }

    fn handle_tail_call(
        &mut self,
        pid: ProcessId,
        recurse: bool,
    ) -> Result<Option<Action<E>>, Error> {
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
    ) -> Result<Option<Action<E>>, Error> {
        let func = self
            .get_function(function_index)
            .ok_or(Error::FunctionUndefined(function_index))?;
        let capture_count = func.captures;

        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Pop capture values from stack (in reverse order, then reverse to restore)
        let mut captures = Vec::with_capacity(capture_count);
        for _ in 0..capture_count {
            captures.push(process.stack.pop().ok_or(Error::StackUnderflow)?);
        }
        captures.reverse();

        let function_value = Value::Function(function_index, captures);
        process.stack.push(function_value);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_reset(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let frame = process.frames.last_mut().ok_or(Error::FrameUnderflow)?;
        let target = frame.locals_base + index;
        if target > process.locals.len() {
            return Err(Error::StackUnderflow);
        }
        process.locals.truncate(target);
        frame.counter += 1;
        Ok(None)
    }

    fn handle_builtin(&mut self, pid: ProcessId, index: usize) -> Result<Option<Action<E>>, Error> {
        // Verify builtin exists
        if index >= self.builtins.len() {
            return Err(Error::BuiltinUndefined(index));
        }
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Push builtin by index
        process.stack.push(Value::Builtin(index));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_equal(&mut self, pid: ProcessId, count: usize) -> Result<Option<Action<E>>, Error> {
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

    fn handle_not(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
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

    fn handle_spawn(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
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
        let argument = process.stack.pop().ok_or(Error::StackUnderflow)?;

        let (function_index, captures) = match function_value {
            Value::Function(idx, caps) => (idx, caps),
            _ => {
                return Err(Error::TypeMismatch {
                    expected: "function".to_string(),
                    found: function_value.type_name().to_string(),
                });
            }
        };

        // Mark caller as spawning - will be notified with Value::Pid(new_pid)
        self.mark_spawning(pid);

        // Return routing request for scheduler to handle
        // Don't increment counter - will be incremented in notify_spawn
        Ok(Some(Action::Spawn {
            caller: pid,
            function_index,
            captures,
            argument,
        }))
    }

    fn handle_send(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
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

        let target_value = process.stack.pop().ok_or(Error::StackUnderflow)?;
        let message = process.stack.pop().ok_or(Error::StackUnderflow)?;

        match target_value {
            Value::Process(target_pid, _) => {
                // Push process back onto stack
                let process = self
                    .get_process_mut(pid)
                    .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                process.stack.push(target_value);

                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }

                // Return routing request for scheduler to handle
                Ok(Some(Action::Deliver {
                    target: target_pid,
                    value: message,
                }))
            }
            Value::Resource(_resource_id, _) => {
                // Resources are opaque handles - cannot send to them directly
                // Use built-in functions like __file_write__ or __tcp_socket_write__ instead
                Err(Error::TypeMismatch {
                    expected: "process".to_string(),
                    found: "resource".to_string(),
                })
            }
            _ => Err(Error::TypeMismatch {
                expected: "process".to_string(),
                found: target_value.type_name().to_string(),
            }),
        }
    }

    fn handle_self(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let function_index = process
            .frames
            .first()
            .ok_or(Error::FrameUnderflow)?
            .function_index;
        process.stack.push(Value::Process(pid, function_index));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_process_ref(
        &mut self,
        pid: ProcessId,
        process_id: usize,
        function_index: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        process
            .stack
            .push(Value::Process(process_id, function_index));

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
        current_time_ms: u64,
    ) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Pop a single value from stack (either a tuple of sources or a single source)
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        // Extract sources: if it's a tuple, use its elements; otherwise use the value itself
        let sources: Vec<Value> = match value {
            Value::Tuple(_, elements) => elements,
            single => vec![single],
        };

        // Count receive sources to initialize cursors
        let receive_count = sources
            .iter()
            .filter(|s| matches!(s, Value::Function(_, _) | Value::Builtin(_)))
            .count();

        // Scan for process sources to determine if we need to await
        let pid_targets: Vec<ProcessId> = sources
            .iter()
            .filter_map(|s| {
                if let Value::Process(p, _) = *s {
                    Some(p)
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

            self.mark_selecting(pid);
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
    ) -> Result<Option<Action<E>>, Error> {
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
                Value::Resource(_resource_id, _) => {
                    // Resources are opaque handles - cannot select on them directly
                    // Use built-in functions like __file_read__, __tcp_socket_read__, or __tcp_listener_accept__ instead
                    return Err(Error::TypeMismatch {
                        expected: "process, timeout, or receive function".to_string(),
                        found: "resource".to_string(),
                    });
                }
                _ => {
                    return Err(Error::InvalidArgument(format!(
                        "Invalid select source: {:?}",
                        source
                    )));
                }
            }
        }

        // No sources ready - mark as selecting
        self.mark_selecting(pid);
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
            // Check if type is compatible using precomputed parameter compatibility
            let type_compatible = self.check_message_compatible(message, source);

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
        current_time_ms: u64,
    ) -> Result<Option<Action<E>>, Error> {
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
                return self.initialize_select(pid, current_time_ms);
            }
        }

        // Phase 3: Ensure start time is set (lazily after awaits complete)
        let start_time = self.ensure_select_start_time(pid, current_time_ms)?;

        // Phase 4: Process sources by type
        self.process_select_sources(pid, receive_result, start_time, current_time_ms)
    }

    /// Complete a select by cleaning up state and pushing result
    fn complete_select(
        &mut self,
        pid: ProcessId,
        result: Value,
    ) -> Result<Option<Action<E>>, Error> {
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

    fn check_expired_timeouts(&mut self, current_time_ms: u64) {
        // Scan selecting processes for expired select timeouts
        let expired: Vec<ProcessId> = self
            .selecting
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
            self.selecting.remove(&pid);
        }
    }

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Binary(a), Value::Binary(b)) => {
                // Compare binary data content
                match (a, b) {
                    (Binary::Constant(idx_a), Binary::Constant(idx_b)) => {
                        // Both are constants - compare the constant data
                        if let (Some(Constant::Binary(bytes_a)), Some(Constant::Binary(bytes_b))) =
                            (self.get_constant(*idx_a), self.get_constant(*idx_b))
                        {
                            bytes_a == bytes_b
                        } else {
                            false
                        }
                    }
                    (Binary::Heap(idx_a), Binary::Heap(idx_b)) => {
                        // Both are heap - compare the heap data
                        if let (Some(data_a), Some(data_b)) =
                            (self.heap.get(*idx_a), self.heap.get(*idx_b))
                        {
                            // Compare lengths first (fast path)
                            if data_a.len() != data_b.len() {
                                return false;
                            }
                            // Compare bytes
                            data_a.to_vec() == data_b.to_vec()
                        } else {
                            false
                        }
                    }
                    (Binary::Constant(idx_c), Binary::Heap(idx_h))
                    | (Binary::Heap(idx_h), Binary::Constant(idx_c)) => {
                        // One is constant, one is heap - compare bytes
                        if let (Some(Constant::Binary(bytes_c)), Some(data_h)) =
                            (self.get_constant(*idx_c), self.heap.get(*idx_h))
                        {
                            bytes_c.as_slice() == data_h.to_vec().as_slice()
                        } else {
                            false
                        }
                    }
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

    /// Extract heap data from a value for serialization across thread boundaries
    /// Returns (value, heap_data) where heap_data is a Vec of flattened binary data
    pub fn extract_heap_data(&self, value: &Value) -> Result<(Value, Vec<Vec<u8>>), Error> {
        // Collect all unique heap indices referenced by this value
        let mut heap_indices = HashSet::new();
        collect_heap_indices(value, &mut heap_indices);

        // Sort indices for deterministic ordering
        let mut indices_vec: Vec<usize> = heap_indices.into_iter().collect();
        indices_vec.sort_unstable();

        // Create index mapping from old heap index to new compact index
        let mut index_map = HashMap::new();
        for (new_idx, &old_idx) in indices_vec.iter().enumerate() {
            index_map.insert(old_idx, new_idx);
        }

        // Extract and flatten binary data
        let mut heap_data = Vec::new();
        for &old_idx in &indices_vec {
            if let Some(binary_data) = self.heap.get(old_idx) {
                heap_data.push(binary_data.to_vec());
            } else {
                return Err(Error::InvalidArgument(format!(
                    "Heap index {} not found",
                    old_idx
                )));
            }
        }

        // Remap value indices
        let remapped_value = remap_heap_indices(value, &index_map)?;

        Ok((remapped_value, heap_data))
    }
}

/// Recursively collect all heap indices referenced by a value
fn collect_heap_indices(value: &Value, indices: &mut HashSet<usize>) {
    match value {
        Value::Binary(Binary::Heap(idx)) => {
            indices.insert(*idx);
        }
        Value::Tuple(_, elements) => {
            for elem in elements {
                collect_heap_indices(elem, indices);
            }
        }
        Value::Function(_, captures) => {
            for capture in captures {
                collect_heap_indices(capture, indices);
            }
        }
        _ => {}
    }
}

/// Remap heap indices in a value according to the provided mapping
fn remap_heap_indices(value: &Value, index_map: &HashMap<usize, usize>) -> Result<Value, Error> {
    match value {
        Value::Binary(Binary::Heap(old_idx)) => {
            if let Some(&new_idx) = index_map.get(old_idx) {
                Ok(Value::Binary(Binary::Heap(new_idx)))
            } else {
                Err(Error::InvalidArgument(format!(
                    "Heap index {} not in mapping",
                    old_idx
                )))
            }
        }
        Value::Binary(binary @ Binary::Constant(_)) => Ok(Value::Binary(*binary)),
        Value::Tuple(type_id, elements) => {
            let remapped_elements: Result<Vec<_>, _> = elements
                .iter()
                .map(|elem| remap_heap_indices(elem, index_map))
                .collect();
            Ok(Value::Tuple(*type_id, remapped_elements?))
        }
        Value::Function(func_idx, captures) => {
            let remapped_captures: Result<Vec<_>, _> = captures
                .iter()
                .map(|capture| remap_heap_indices(capture, index_map))
                .collect();
            Ok(Value::Function(*func_idx, remapped_captures?))
        }
        Value::Integer(i) => Ok(Value::Integer(*i)),
        Value::Builtin(name) => Ok(Value::Builtin(*name)),
        Value::Process(pid, func_idx) => Ok(Value::Process(*pid, *func_idx)),
        Value::Resource(id, type_name) => Ok(Value::Resource(*id, *type_name)),
    }
}

impl<E: Effect> Executor<E> {
    /// Inject heap data into this executor and remap heap indices in the value.
    /// Binary heap data is allocated to the executor's heap and indices are remapped.
    pub fn inject_heap_data(
        &mut self,
        value: Value,
        heap_data: &[Vec<u8>],
    ) -> Result<Value, Error> {
        // Allocate all heap data to executor and build index mapping
        let mut index_map = HashMap::new();
        for (new_idx, bytes) in heap_data.iter().enumerate() {
            let heap_idx = self.heap.len();
            self.heap.push(BinaryData::new(bytes.clone()));
            index_map.insert(new_idx, heap_idx);
        }

        // Remap value indices
        remap_heap_indices(&value, &index_map)
    }
}
