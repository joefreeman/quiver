use crate::bytecode::TypeId;
use crate::process::ProcessId;
use serde::{Deserialize, Serialize};

/// Maximum binary size in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Binary {
    /// Reference to a binary stored in the constants table
    Constant(usize),
    /// Reference to a binary stored in the executor's heap
    Heap(usize),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
