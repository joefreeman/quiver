use crate::bytecode::{Constant, Function, Instruction, TypeId};
use crate::program::Program;
use crate::scheduler::Scheduler;
use crate::types::Type;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, MutexGuard, RwLock, RwLockWriteGuard};

// Re-export scheduler types for public API
pub use crate::scheduler::{ProcessId, ProcessInfo, ProcessStatus, SchedulerHandle};

/// Maximum binary size in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Binary {
    /// Reference to a binary stored in the constants table
    Constant(usize),
    /// Reference to a binary stored in the scheduler's heap
    Heap(usize),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Value {
    Integer(i64),
    Binary(Binary),
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

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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

#[derive(Debug)]
pub struct VM {
    program: Arc<RwLock<Program>>,
    scheduler_handle: SchedulerHandle,
    next_process_id: Arc<AtomicUsize>,
}

impl VM {
    pub fn scheduler(&self) -> MutexGuard<'_, Scheduler> {
        self.scheduler_handle.scheduler.lock().unwrap()
    }

    pub fn new(program: Program) -> Self {
        let program = Arc::new(RwLock::new(program));
        let next_process_id = Arc::new(AtomicUsize::new(0));
        let scheduler_handle =
            SchedulerHandle::new(Arc::clone(&program), Arc::clone(&next_process_id));

        Self {
            program,
            scheduler_handle,
            next_process_id,
        }
    }

    pub fn program(&self) -> Arc<RwLock<Program>> {
        Arc::clone(&self.program)
    }

    pub fn program_mut(&self) -> RwLockWriteGuard<'_, Program> {
        self.program.write().unwrap()
    }

    pub fn register_constant(&self, constant: Constant) -> usize {
        let mut program = self.program.write().unwrap();
        let index = program.register_constant(constant.clone());
        drop(program);
        self.scheduler_handle.sync_constant(index, constant);
        index
    }

    pub fn register_function(&self, function: Function) -> usize {
        let mut program = self.program.write().unwrap();
        let index = program.register_function(function.clone());
        drop(program);
        self.scheduler_handle.sync_function(index, function);
        index
    }

    pub fn register_builtin(&self, name: String) -> usize {
        let mut program = self.program.write().unwrap();
        let index = program.register_builtin(name.clone());
        drop(program);
        self.scheduler_handle.sync_builtin(index, name);
        index
    }

    pub fn register_type(
        &self,
        name: Option<String>,
        fields: Vec<(Option<String>, Type)>,
    ) -> TypeId {
        let mut program = self.program.write().unwrap();
        let type_id = program.register_type(name.clone(), fields.clone());
        let info = (name, fields);
        drop(program);
        self.scheduler_handle.sync_type(type_id, info);
        type_id
    }

    pub fn inject_function_captures(&self, function_index: usize, captures: Vec<Value>) {
        let mut program = self.program.write().unwrap();
        program.inject_function_captures(function_index, captures);
        let function = program.get_function(function_index).unwrap().clone();
        drop(program);
        self.scheduler_handle
            .sync_function(function_index, function);
    }

    /// Update the Program with a new one, syncing only the differences to the scheduler.
    pub fn update_program(&self, new_program: Program) {
        let old_program = self.program.read().unwrap();

        // Sync new/modified constants
        let old_constants = old_program.get_constants();
        let new_constants = new_program.get_constants();
        for (index, constant) in new_constants.iter().enumerate() {
            if index >= old_constants.len() || &old_constants[index] != constant {
                self.scheduler_handle.sync_constant(index, constant.clone());
            }
        }

        // Sync new/modified functions
        let old_functions = old_program.get_functions();
        let new_functions = new_program.get_functions();
        for (index, function) in new_functions.iter().enumerate() {
            if index >= old_functions.len() || &old_functions[index] != function {
                self.scheduler_handle.sync_function(index, function.clone());
            }
        }

        // Sync new/modified builtins
        let old_builtins = old_program.get_builtins();
        let new_builtins = new_program.get_builtins();
        for (index, builtin) in new_builtins.iter().enumerate() {
            if index >= old_builtins.len() || &old_builtins[index] != builtin {
                self.scheduler_handle.sync_builtin(index, builtin.clone());
            }
        }

        // Sync new/modified types
        let old_types = old_program.get_types();
        let new_types = new_program.get_types();
        for (type_id, info) in new_types.iter() {
            if !old_types.contains_key(type_id) || &old_types[type_id] != info {
                self.scheduler_handle.sync_type(*type_id, info.clone());
            }
        }

        drop(old_program);
        *self.program.write().unwrap() = new_program;
    }

    pub fn spawn_process(&self, persistent: bool) -> Result<ProcessId, Error> {
        let id = ProcessId(self.next_process_id.fetch_add(1, Ordering::SeqCst));
        self.scheduler_handle.spawn_process(id, persistent)?;
        Ok(id)
    }

    pub fn get_process_statuses(&self) -> Result<HashMap<ProcessId, ProcessStatus>, Error> {
        self.scheduler_handle.get_process_statuses()
    }

    pub fn get_process_info(&self, id: ProcessId) -> Result<Option<ProcessInfo>, Error> {
        self.scheduler_handle.get_process_info(id)
    }

    /// Clone a binary reference (cheap operation)
    pub fn clone_binary(&self, binary: &Binary) -> Binary {
        binary.clone()
    }

    pub fn execute_instructions(
        &self,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
        process_id: Option<ProcessId>,
        variables: Option<HashMap<String, (Type, usize)>>,
    ) -> Result<(Option<Value>, Option<HashMap<String, (Type, usize)>>), Error> {
        let pid = if let Some(id) = process_id {
            id
        } else {
            self.spawn_process(false)?
        };

        self.scheduler_handle
            .execute(pid, instructions, parameter, variables)
    }

    pub fn execute_function(&self, entry: usize) -> Result<Option<Value>, Error> {
        let function = self
            .program
            .read()
            .unwrap()
            .get_function(entry)
            .ok_or(Error::FunctionUndefined(entry))?
            .clone();

        let pid = self.spawn_process(false)?;
        let (result, _) =
            self.scheduler_handle
                .execute(pid, function.instructions, Some(Value::nil()), None)?;
        Ok(result)
    }

    pub fn get_stack(&self, process_id: ProcessId) -> Option<Vec<Value>> {
        self.scheduler_handle
            .scheduler
            .lock()
            .unwrap()
            .get_process(process_id)
            .map(|p| p.stack.clone())
    }

    pub fn frame_count(&self, process_id: ProcessId) -> Option<usize> {
        self.scheduler_handle
            .scheduler
            .lock()
            .unwrap()
            .get_process(process_id)
            .map(|p| p.frames.len())
    }

    pub fn get_variables(
        &self,
        process_id: ProcessId,
        mapping: &HashMap<String, usize>,
    ) -> Result<HashMap<String, Value>, Error> {
        self.scheduler_handle
            .get_variables(process_id, mapping.clone())
    }
}
