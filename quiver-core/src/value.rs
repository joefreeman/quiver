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
pub struct Payload {
    has_heap_refs: bool,
    elements: Vec<Value>,
    /// Annotations attached to the owning value: `(key id, value)` pairs, sorted by key id.
    /// Invisible to equality and pattern matching — only `GetAnnotation` observes them.
    /// Boxed (not `Option<Vec>`) so the common unannotated case costs one pointer-sized
    /// `None` rather than an inline three-word `Vec`.
    #[allow(clippy::box_collection)]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    annotations: Option<Box<Vec<(usize, Value)>>>,
    /// A type-consuming builtin's explicit type argument (`__type_name__<'t>` → the
    /// resolved type id), carried on the *value* so an instantiated builtin flows
    /// through bindings and generic code intact. Unlike annotations it is operational
    /// (the implementation reads it), so equality compares it. Always `None` on tuple
    /// and function payloads.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    type_argument: Option<usize>,
}

impl Payload {
    pub fn new(elements: Vec<Value>) -> Self {
        let has_heap_refs = elements.iter().any(Value::has_heap_refs);
        Payload {
            has_heap_refs,
            elements,
            annotations: None,
            type_argument: None,
        }
    }

    /// Construct a payload with annotations attached. Keys are sorted and must be unique.
    pub fn with_annotations(elements: Vec<Value>, mut annotations: Vec<(usize, Value)>) -> Self {
        if annotations.is_empty() {
            return Payload::new(elements);
        }
        annotations.sort_by_key(|(key, _)| *key);
        debug_assert!(
            annotations.windows(2).all(|w| w[0].0 != w[1].0),
            "duplicate annotation key"
        );
        let has_heap_refs = elements
            .iter()
            .chain(annotations.iter().map(|(_, value)| value))
            .any(Value::has_heap_refs);
        Payload {
            has_heap_refs,
            elements,
            annotations: Some(Box::new(annotations)),
            type_argument: None,
        }
    }

    /// The same payload carrying a type argument (see the field). Builder-style, used
    /// when constructing an instantiated builtin value or re-attaching annotations to
    /// one.
    pub fn with_type_argument(mut self, type_argument: Option<usize>) -> Self {
        self.type_argument = type_argument;
        self
    }

    /// A type-consuming builtin's explicit type argument, if the owning value carries one.
    pub fn type_argument(&self) -> Option<usize> {
        self.type_argument
    }

    /// True if any element or annotation may (transitively) reference an executor-heap binary.
    pub fn has_heap_refs(&self) -> bool {
        self.has_heap_refs
    }

    /// The annotations attached to the owning value (empty if none).
    pub fn annotations(&self) -> &[(usize, Value)] {
        self.annotations.as_deref().map_or(&[], |a| a.as_slice())
    }

    /// Look up an annotation by key id.
    pub fn get_annotation(&self, key: usize) -> Option<&Value> {
        let annotations = self.annotations();
        annotations
            .binary_search_by_key(&key, |(k, _)| *k)
            .ok()
            .map(|i| &annotations[i].1)
    }

    /// All values reachable from this payload: elements, then annotation values. This is the
    /// iterator retain/release and the heap-transfer walks must use, so annotations are
    /// accounted exactly like elements.
    pub fn all_values(&self) -> impl Iterator<Item = &Value> {
        self.elements
            .iter()
            .chain(self.annotations().iter().map(|(_, value)| value))
    }
}

impl std::ops::Deref for Payload {
    type Target = [Value];

    fn deref(&self) -> &[Value] {
        &self.elements
    }
}

// Equality is over the elements only: the cached flag is derived from them, and
// annotations are deliberately invisible — values differing only in annotations are
// equal, and annotated nil still matches `=[]`.
impl PartialEq for Payload {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl<'a> IntoIterator for &'a Payload {
    type Item = &'a Value;
    type IntoIter = std::slice::Iter<'a, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.elements.iter()
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

#[derive(Debug, Clone, Serialize, Deserialize)]
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
    Tuple(usize, Arc<Payload>),
    Function(usize, Arc<Payload>),
    // builtin_id (index into builtins table), plus annotations when attached. The payload's
    // elements are always empty — the slot exists only so builtins can carry annotations
    // like any other callable; the bare, un-annotated form is `None` (no allocation). The
    // `Option` fits the variant's existing padding, so `Value` stays at 24 bytes.
    Builtin(usize, Option<Arc<Payload>>),
    Process(ProcessId, usize),
    Resource(ResourceId, usize), // resource_id, resource_type_id
}

// Manual because annotations must be invisible to equality: a builtin that has gained an
// annotation payload still equals its bare form. Tuple/function payloads get the same
// treatment inside `Payload`'s own `PartialEq` (elements only).
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::BigInt(a), Value::BigInt(b)) => a == b,
            (Value::Binary(a), Value::Binary(b)) => a == b,
            (Value::Reference(a), Value::Reference(b)) => a == b,
            (Value::Tuple(a, p), Value::Tuple(b, q)) => a == b && p == q,
            (Value::Function(a, p), Value::Function(b, q)) => a == b && p == q,
            // The type argument is operational (a differently-instantiated builtin
            // behaves differently), so it participates; annotations stay invisible.
            (Value::Builtin(a, p), Value::Builtin(b, q)) => {
                a == b
                    && p.as_deref().and_then(Payload::type_argument)
                        == q.as_deref().and_then(Payload::type_argument)
            }
            (Value::Process(a, x), Value::Process(b, y)) => a == b && x == y,
            (Value::Resource(a, x), Value::Resource(b, y)) => a == b && x == y,
            _ => false,
        }
    }
}

impl Value {
    /// Create a NIL tuple value
    pub fn nil() -> Self {
        Value::Tuple(NIL, Arc::new(Payload::new(vec![])))
    }

    /// Create an OK tuple value
    pub fn ok() -> Self {
        Value::Tuple(OK, Arc::new(Payload::new(vec![])))
    }

    /// Construct a tuple value from owned fields.
    pub fn tuple(type_id: usize, fields: Vec<Value>) -> Self {
        Value::Tuple(type_id, Arc::new(Payload::new(fields)))
    }

    /// Construct a function value from owned captures.
    pub fn function(function_index: usize, captures: Vec<Value>) -> Self {
        Value::Function(function_index, Arc::new(Payload::new(captures)))
    }

    /// Construct a bare (un-annotated) builtin value.
    pub fn builtin(builtin_id: usize) -> Self {
        Value::Builtin(builtin_id, None)
    }

    /// Construct a builtin value carrying an explicit type argument (a type-consuming
    /// builtin's instantiation), or the bare form when there is none.
    pub fn builtin_typed(builtin_id: usize, type_argument: Option<usize>) -> Self {
        match type_argument {
            None => Value::Builtin(builtin_id, None),
            Some(_) => Value::Builtin(
                builtin_id,
                Some(Arc::new(
                    Payload::new(vec![]).with_type_argument(type_argument),
                )),
            ),
        }
    }

    /// A type-consuming builtin's explicit type argument, if this value carries one.
    pub fn type_argument(&self) -> Option<usize> {
        match self {
            Value::Builtin(_, Some(payload)) => payload.type_argument(),
            _ => None,
        }
    }

    /// True if this value may (transitively) reference an executor-heap binary and thus
    /// needs retain/release accounting. O(1): composite values cache the answer.
    pub fn has_heap_refs(&self) -> bool {
        match self {
            Value::Binary(Binary::Heap(_)) => true,
            Value::Tuple(_, fields) | Value::Function(_, fields) => fields.has_heap_refs,
            Value::Builtin(_, Some(payload)) => payload.has_heap_refs,
            _ => false,
        }
    }

    /// Attach (or replace) an annotation on a tuple, function or builtin value,
    /// copy-on-annotate: the payload elements are cloned into a fresh `Payload` carrying
    /// the new annotation. Returns `None` for values that cannot carry annotations
    /// (anything else).
    pub fn annotated(&self, key: usize, annotation: Value) -> Option<Value> {
        let payload = match self {
            Value::Tuple(_, payload) | Value::Function(_, payload) => Some(payload.as_ref()),
            Value::Builtin(_, payload) => payload.as_deref(),
            _ => return None,
        };
        let mut annotations: Vec<(usize, Value)> = payload
            .map(Payload::annotations)
            .unwrap_or_default()
            .iter()
            .filter(|(existing, _)| *existing != key)
            .cloned()
            .collect();
        annotations.push((key, annotation));
        let elements = payload.map(|p| p.elements.clone()).unwrap_or_default();
        // Re-attach preserves a builtin's type argument — it is operational, not metadata.
        let type_argument = payload.and_then(Payload::type_argument);
        let payload = Arc::new(
            Payload::with_annotations(elements, annotations).with_type_argument(type_argument),
        );
        Some(match self {
            Value::Tuple(id, _) => Value::Tuple(*id, payload),
            Value::Function(id, _) => Value::Function(*id, payload),
            Value::Builtin(id, _) => Value::Builtin(*id, Some(payload)),
            _ => unreachable!(),
        })
    }

    /// Look up an annotation on a tuple, function or builtin value by key id.
    pub fn get_annotation(&self, key: usize) -> Option<&Value> {
        match self {
            Value::Tuple(_, fields) | Value::Function(_, fields) => fields.get_annotation(key),
            Value::Builtin(_, Some(payload)) => payload.get_annotation(key),
            _ => None,
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
            Value::Builtin(..) => "builtin",
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
