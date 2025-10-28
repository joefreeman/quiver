use crate::process::ProcessId;
use crate::types::{NIL, OK};
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
    Tuple(usize, Vec<Value>),
    Function(usize, Vec<Value>),
    Builtin(String),
    Process(ProcessId, usize),
}

impl Value {
    /// Create a NIL tuple value
    pub fn nil() -> Self {
        Value::Tuple(NIL, vec![])
    }

    /// Create an OK tuple value
    pub fn ok() -> Self {
        Value::Tuple(OK, vec![])
    }

    /// Check if this value is NIL
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Tuple(id, fields) if *id == NIL && fields.is_empty())
    }

    /// Check if this value is OK
    pub fn is_ok(&self) -> bool {
        matches!(self, Value::Tuple(id, fields) if *id == OK && fields.is_empty())
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Binary(_) => "binary",
            Value::Tuple(_, _) => "tuple",
            Value::Function(_, _) => "function",
            Value::Builtin(_) => "builtin",
            Value::Process(_, _) => "process",
        }
    }
}
