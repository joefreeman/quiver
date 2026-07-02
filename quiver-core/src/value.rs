use crate::process::ProcessId;
use crate::types::{NIL, OK};
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Maximum binary size in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

/// Resource identifier
pub type ResourceId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Binary {
    /// Reference to a binary stored in the constants table
    Constant(usize),
    /// Reference to a binary stored in the executor's heap
    Heap(usize),
}

/// The shared payload of a tuple or function value: its elements, plus a cached
/// over-approximation of whether any element (transitively) references an executor-heap
/// binary. The flag lets the executor's retain/release accounting skip the recursive
/// walk for the (common) values that own no heap binaries, keeping stack/locals traffic
/// O(1) instead of O(size of value). Computed once at construction from the elements'
/// own cached flags, so it costs O(arity), not a deep walk.
#[derive(Debug, Serialize, Deserialize)]
pub struct Fields {
    has_heap_refs: bool,
    values: Vec<Value>,
}

impl Fields {
    pub fn new(values: Vec<Value>) -> Self {
        let has_heap_refs = values.iter().any(Value::has_heap_refs);
        Fields {
            has_heap_refs,
            values,
        }
    }

    /// True if any element may (transitively) reference an executor-heap binary.
    pub fn has_heap_refs(&self) -> bool {
        self.has_heap_refs
    }
}

impl std::ops::Deref for Fields {
    type Target = [Value];

    fn deref(&self) -> &[Value] {
        &self.values
    }
}

// Equality is over the elements only; the cached flag is derived from them.
impl PartialEq for Fields {
    fn eq(&self, other: &Self) -> bool {
        self.values == other.values
    }
}

impl<'a> IntoIterator for &'a Fields {
    type Item = &'a Value;
    type IntoIter = std::slice::Iter<'a, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Integer(BigInt),
    Binary(Binary),
    Reference(u64), // Unique ref: (worker_id << 48) | counter
    // Tuple/Function payloads are reference-counted so cloning a value is O(1) (refcount bump)
    // rather than a deep copy. Values are immutable, so sharing is safe.
    Tuple(usize, Arc<Fields>),
    Function(usize, Arc<Fields>),
    Builtin(usize), // builtin_id (index into builtins table)
    Process(ProcessId, usize),
    Resource(ResourceId, usize), // resource_id, resource_type_id
}

impl Value {
    /// Create a NIL tuple value
    pub fn nil() -> Self {
        Value::Tuple(NIL, Arc::new(Fields::new(vec![])))
    }

    /// Create an OK tuple value
    pub fn ok() -> Self {
        Value::Tuple(OK, Arc::new(Fields::new(vec![])))
    }

    /// Construct a tuple value from owned fields.
    pub fn tuple(type_id: usize, fields: Vec<Value>) -> Self {
        Value::Tuple(type_id, Arc::new(Fields::new(fields)))
    }

    /// Construct a function value from owned captures.
    pub fn function(function_index: usize, captures: Vec<Value>) -> Self {
        Value::Function(function_index, Arc::new(Fields::new(captures)))
    }

    /// True if this value may (transitively) reference an executor-heap binary and thus
    /// needs retain/release accounting. O(1): composite values cache the answer.
    pub fn has_heap_refs(&self) -> bool {
        match self {
            Value::Binary(Binary::Heap(_)) => true,
            Value::Tuple(_, fields) | Value::Function(_, fields) => fields.has_heap_refs,
            _ => false,
        }
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
            Value::Reference(_) => "ref",
            Value::Tuple(_, _) => "tuple",
            Value::Function(_, _) => "function",
            Value::Builtin(_) => "builtin",
            Value::Process(_, _) => "process",
            Value::Resource(_, _) => "resource",
        }
    }
}
