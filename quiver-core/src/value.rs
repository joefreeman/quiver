use crate::process::ProcessId;
use crate::types::{NIL, OK};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
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

/// A boxed arbitrary-precision integer that does **not** fit in an i64 — the canonical
/// big form of `Value::BigInt`. The private field forces construction through
/// [`Big::new`] (or `From<BigInt>`, which serde's `from` attribute also routes through),
/// which asserts the canonical invariant in debug builds. Small integers must use
/// `Value::Int`; build integer values via [`Value::integer`] to get the split right.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(from = "BigInt", into = "BigInt")]
pub struct Big(Box<BigInt>);

impl Big {
    pub fn new(n: BigInt) -> Self {
        debug_assert!(
            n.to_i64().is_none(),
            "non-canonical Big: {n} fits in i64 and must be Value::Int"
        );
        Big(Box::new(n))
    }
}

impl From<BigInt> for Big {
    fn from(n: BigInt) -> Self {
        Big::new(n)
    }
}

impl From<Big> for BigInt {
    fn from(n: Big) -> Self {
        *n.0
    }
}

impl std::ops::Deref for Big {
    type Target = BigInt;

    fn deref(&self) -> &BigInt {
        &self.0
    }
}

/// A borrowed, allocation-free view of an integer value, letting consumers take the
/// machine-word fast path and fall back to arbitrary precision only when needed.
#[derive(Debug, Clone, Copy)]
pub enum IntRef<'a> {
    Small(i64),
    Big(&'a BigInt),
}

impl IntRef<'_> {
    /// Widen to an owned `BigInt` (allocates for the small case, clones for the big).
    pub fn to_bigint(self) -> BigInt {
        match self {
            IntRef::Small(n) => BigInt::from(n),
            IntRef::Big(n) => n.clone(),
        }
    }

    /// The value as an i64, or `None` if out of range. By the canonical invariant the
    /// big form is always out of range, so this never inspects the digits.
    pub fn to_i64(self) -> Option<i64> {
        match self {
            IntRef::Small(n) => Some(n),
            IntRef::Big(_) => None,
        }
    }
}

impl std::fmt::Display for IntRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntRef::Small(n) => n.fmt(f),
            IntRef::Big(n) => n.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    // Integers are canonical: `Int` for anything that fits an i64, `BigInt` strictly for
    // values outside that range (enforced by `Big`). The invariant is what keeps derived
    // equality, literal pattern matching and hashing representation-blind.
    Int(i64),
    BigInt(Big),
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

    /// Construct an integer value from a machine integer.
    pub fn int(n: i64) -> Self {
        Value::Int(n)
    }

    /// Construct an integer value in canonical form: `Int` when the value fits an i64,
    /// `BigInt` otherwise. All integer construction from `BigInt`s must go through here.
    pub fn integer(n: BigInt) -> Self {
        match n.to_i64() {
            Some(small) => Value::Int(small),
            None => Value::BigInt(Big::new(n)),
        }
    }

    /// View this value as an integer without allocating, or `None` if it isn't one.
    pub fn as_int(&self) -> Option<IntRef<'_>> {
        match self {
            Value::Int(n) => Some(IntRef::Small(*n)),
            Value::BigInt(n) => Some(IntRef::Big(n)),
            _ => None,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) | Value::BigInt(_) => "integer",
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

#[cfg(test)]
mod tests {
    use super::*;

    // Boxing the big-integer variant is what keeps `Value` at two words + discriminant;
    // an inline `BigInt` would push every stack slot, local and tuple field to 40 bytes.
    #[test]
    fn value_stays_small() {
        assert!(std::mem::size_of::<Value>() <= 24);
    }
}
