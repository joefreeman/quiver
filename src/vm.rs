use crate::bytecode::{Instruction, TypeId};
use crate::program::Program;
use crate::types::{TupleTypeInfo, TypeLookup};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};

// Re-export scheduler types for public API
pub use crate::scheduler::{ProcessId, ProcessStatus, SchedulerHandle};

/// Maximum binary size in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryRef {
    /// Reference to a binary stored in the constants table
    Constant(usize),
    /// Reference-counted heap-allocated binary
    Heap(Arc<Vec<u8>>),
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

#[derive(Debug)]
pub struct VM {
    program: Arc<RwLock<Program>>,
    scheduler_handle: SchedulerHandle,
    next_process_id: Arc<AtomicUsize>,
}

impl TypeLookup for VM {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.program
            .read()
            .unwrap()
            .lookup_type(type_id)
            .map(|info| {
                // This is a hack to get around the lifetime issue
                // The TypeInfo is stored in the Program which is behind an Arc
                // so it will live as long as the VM does
                unsafe { &*(info as *const TupleTypeInfo) }
            })
    }
}

impl VM {
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

    pub fn program_mut(&self) -> std::sync::RwLockWriteGuard<'_, Program> {
        self.program.write().unwrap()
    }

    pub fn spawn_process(&self, persistent: bool) -> ProcessId {
        let id = ProcessId(self.next_process_id.fetch_add(1, Ordering::SeqCst));
        let mut scheduler = self.scheduler_handle.scheduler.lock().unwrap();
        scheduler.spawn_process(id, persistent);
        id
    }

    pub fn get_process_statuses(&self) -> std::collections::HashMap<ProcessId, ProcessStatus> {
        self.scheduler_handle
            .scheduler
            .lock()
            .unwrap()
            .get_process_statuses()
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
        Ok(BinaryRef::Heap(Arc::new(bytes)))
    }

    /// Clone a binary reference (cheap operation)
    pub fn clone_binary(&self, binary_ref: &BinaryRef) -> BinaryRef {
        binary_ref.clone()
    }

    pub fn execute_instructions(
        &self,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
        process_id: Option<ProcessId>,
    ) -> Result<Option<Value>, Error> {
        let pid = if let Some(id) = process_id {
            id
        } else {
            self.spawn_process(false)
        };

        self.scheduler_handle
            .execute(Some(pid), instructions, parameter)
    }

    pub fn execute_function(&self, entry: usize) -> Result<Option<Value>, Error> {
        let function = self
            .program
            .read()
            .unwrap()
            .get_function(entry)
            .ok_or(Error::FunctionUndefined(entry))?
            .clone();

        let pid = self.spawn_process(false);
        self.scheduler_handle
            .execute(Some(pid), function.instructions, Some(Value::nil()))
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

    pub fn cleanup_locals(
        &self,
        process_id: ProcessId,
        variables: &std::collections::HashMap<String, (crate::types::Type, usize)>,
    ) -> std::collections::HashMap<String, (crate::types::Type, usize)> {
        let mut scheduler = self.scheduler_handle.scheduler.lock().unwrap();
        let process = match scheduler.get_process_mut(process_id) {
            Some(p) => p,
            None => return std::collections::HashMap::new(),
        };

        let mut referenced: Vec<(String, crate::types::Type, usize)> = variables
            .iter()
            .map(|(name, (typ, index))| (name.clone(), typ.clone(), *index))
            .collect();
        referenced.sort_by_key(|(_, _, index)| *index);

        let mut new_locals = Vec::new();
        let mut new_variables = std::collections::HashMap::new();

        for (name, typ, old_index) in referenced {
            if old_index < process.locals.len() {
                new_variables.insert(name, (typ, new_locals.len()));
                new_locals.push(process.locals[old_index].clone());
            }
        }

        process.locals = new_locals;
        new_variables
    }

    pub fn get_variables(
        &self,
        process_id: ProcessId,
        mapping: &std::collections::HashMap<String, usize>,
    ) -> Result<std::collections::HashMap<String, Value>, Error> {
        let scheduler = self.scheduler_handle.scheduler.lock().unwrap();
        let process = scheduler
            .get_process(process_id)
            .ok_or(Error::InvalidArgument(format!(
                "Process {:?} not found",
                process_id
            )))?;

        let mut variables = std::collections::HashMap::new();
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
}
