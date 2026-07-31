use crate::process::ProcessId;
use crate::types::{NIL, OK};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};
use std::rc::Rc;

/// Maximum binary size in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

/// Resource identifier
pub type ResourceId = usize;

/// A binary value's bytes, or a reference to a program constant's.
///
/// `Data` owns its bytes through an `Rc`, so a binary's lifetime is Rust's to manage: it dies
/// when the last value referring to it does. This replaced an index into a per-worker heap
/// table whose slots were refcounted by hand, which made *copying* a value O(size of the value)
/// — every heap slot under it had to be counted again — and so made building a list of binaries
/// quadratic. A refcount on the value graph makes the same copy O(1).
///
/// Not `Copy`, necessarily: cloning one is now a refcount bump rather than a 16-byte memcpy.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Binary {
    /// Reference to a binary stored in the constants table. Constants outlive every value and
    /// are identical on every worker, so they stay references — a literal costs nothing to
    /// carry and crosses a process boundary as an index rather than as bytes.
    Constant(usize),
    /// Bytes owned by the value.
    Data(Rc<crate::binary::BinaryData>),
}

/// The shared payload of a tuple or function value: its elements, and its annotations if
/// any. Reference-counted, so cloning a value is O(1) whatever it contains.
#[derive(Debug, Serialize, Deserialize)]
#[serde(from = "PayloadData")]
pub struct Payload {
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

/// The serialized shape of [`Payload`]. Deserialization routes through the ordinary
/// constructors rather than building the struct directly, so a deserialized payload is
/// indistinguishable from a constructed one.
#[derive(Deserialize)]
struct PayloadData {
    elements: Vec<Value>,
    // Boxed to mirror the field it deserializes (see `Payload::annotations`).
    #[allow(clippy::box_collection)]
    #[serde(default)]
    annotations: Option<Box<Vec<(usize, Value)>>>,
    #[serde(default)]
    type_argument: Option<usize>,
}

impl From<PayloadData> for Payload {
    fn from(data: PayloadData) -> Self {
        let payload = match data.annotations {
            Some(annotations) => Payload::with_annotations(data.elements, *annotations),
            None => Payload::new(data.elements),
        };
        payload.with_type_argument(data.type_argument)
    }
}

impl Payload {
    pub fn new(elements: Vec<Value>) -> Self {
        Payload {
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
        Payload {
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

    /// All values reachable from this payload: elements, then annotation values. Any walk over
    /// a value must use this rather than the elements alone — an annotation carries a value like
    /// any other field, so it can carry a binary, a resource handle or a pid like any other
    /// field. (The environment's resource-ownership walk was the one place that forgot.)
    pub fn all_values(&self) -> impl Iterator<Item = &Value> {
        self.elements
            .iter()
            .chain(self.annotations().iter().map(|(_, value)| value))
    }

    /// This payload as a shared handle — the single funnel every `Rc<Payload>` is built
    /// through. A payload carrying nothing yields [`EMPTY_PAYLOAD`] instead of a fresh
    /// allocation, so a field-less tuple costs a refcount bump.
    ///
    /// `#[inline]` because this sits in front of every tuple construction and takes `self`
    /// by value: out of line, the 56-byte `Payload` move would be a real memcpy on the
    /// hottest path in the runtime. (Measured no difference either way on a noisy machine —
    /// it is the safe default for a wrapper this small, not a tuned result.)
    #[inline]
    pub fn shared(self) -> Rc<Payload> {
        if self.carries_nothing() {
            return Payload::empty();
        }
        Rc::new(self)
    }

    /// Whether this payload has no content of its own, and so is interchangeable with every
    /// other such payload. Elements and annotations are observable; the type argument is
    /// operational (equality compares it), so a payload carrying one is never shared.
    #[inline]
    fn carries_nothing(&self) -> bool {
        self.elements.is_empty() && self.annotations.is_none() && self.type_argument.is_none()
    }

    /// The shared payload directly, for callers that know they have nothing to carry — so
    /// they skip building a `Payload` only for [`shared`](Self::shared) to discard it.
    /// Worth having: `nil`/`ok` run on every match verdict.
    #[inline]
    pub fn empty() -> Rc<Payload> {
        // `try_with`: a value can be constructed while a thread is tearing down (a TLS
        // destructor dropping a structure that rebuilds one), and TLS destruction order is
        // unspecified. Falling back to a fresh payload costs an allocation on a path that
        // runs once per thread exit at most.
        EMPTY_PAYLOAD
            .try_with(Rc::clone)
            .unwrap_or_else(|_| Rc::new(Payload::new(Vec::new())))
    }

    /// Every value in this payload, mutably — the `&mut` twin of [`all_values`](Self::all_values).
    /// `elements` and `annotations` are distinct fields, so borrowing both at once is disjoint.
    fn all_values_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        self.elements.iter_mut().chain(
            self.annotations
                .as_deref_mut()
                .into_iter()
                .flat_map(|entries| entries.iter_mut().map(|(_, value)| value)),
        )
    }

    /// Whether any value here owns a payload of its own — i.e. whether dropping this one
    /// would recurse. The gate on [`Drop`]'s slow path.
    fn owns_payloads(&self) -> bool {
        self.all_values().any(Value::owns_payload)
    }
}

/// Move every payload this one owns onto `stack`, leaving trivially-droppable values behind.
/// After this, `payload`'s own drop terminates at the fast path.
fn take_payloads(payload: &mut Payload, stack: &mut Vec<Rc<Payload>>) {
    for value in payload.all_values_mut() {
        if value.owns_payload() {
            // `Value::Int` owns nothing, so the vacated slot costs no refcount traffic —
            // cheaper than leaving a nil (which would clone the interned empty payload).
            match std::mem::replace(value, Value::Int(0)) {
                Value::Tuple(_, owned)
                | Value::Function(_, owned)
                | Value::Builtin(_, Some(owned)) => stack.push(owned),
                _ => unreachable!("owns_payload gated the replacement"),
            }
        }
    }
}

/// Tear `payload`'s owned subtree down using `stack` as the work-list, descending only into
/// payloads we uniquely own — a shared one must stay intact, and its handle simply decrements.
fn drain_payloads(payload: &mut Payload, stack: &mut Vec<Rc<Payload>>) {
    take_payloads(payload, stack);
    while let Some(owned) = stack.pop() {
        // Sole owner: vacate its children before it drops, so its own drop terminates at the
        // fast path below rather than recursing.
        if let Ok(mut owned) = Rc::try_unwrap(owned) {
            take_payloads(&mut owned, stack);
        }
    }
}

thread_local! {
    /// Work-list for [`Payload`]'s drop, reused across drops. A fresh `Vec` per drop measured
    /// as ~9 extra allocations per unit of work on cons-heavy code — enough to undo what
    /// `handle_tuple`'s `with_capacity` had just saved. Depth-first popping keeps this shallow
    /// (a cons chain never exceeds one entry), so retaining it costs almost nothing.
    static DROP_STACK: std::cell::RefCell<Vec<Rc<Payload>>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Capacity retained between drops. A pathologically wide value could grow the work-list far
/// beyond what any later drop needs; releasing the excess keeps the per-thread cost bounded.
const DROP_STACK_RETAIN: usize = 1024;

impl Drop for Payload {
    /// Drop iteratively. The default (recursive) glue walks one stack frame per level of
    /// nesting, and `'list<'t> = Nil | Cons['t, ^]` — the idiomatic list — is exactly a deep
    /// nest, so a long enough list overflows the stack on teardown (measured at 66 bytes of
    /// stack per cons cell, i.e. ~16k cells per MiB). That is why the native worker reserved a
    /// 256 MiB stack, and why peak RSS on long lists exceeded the data by the stack it
    /// committed. Mirrors [`BinaryData`](crate::binary::BinaryData)'s drop, for the same
    /// reason.
    fn drop(&mut self) {
        // Fast path: nothing here owns a payload, so the field drops that follow terminate
        // immediately. This covers every leaf, and — because `drain_payloads` vacates a
        // payload before it drops — every payload the walk descends into. That is also what
        // keeps the borrow below non-reentrant.
        if !self.owns_payloads() {
            return;
        }
        // `try_with` as well as `try_borrow_mut`: values are dropped during thread teardown
        // (a TLS destructor releasing a structure that owns them), and by then this work-list
        // may itself have been destroyed — TLS destruction order is unspecified.
        let reused = DROP_STACK
            .try_with(|cell| match cell.try_borrow_mut() {
                Ok(mut stack) => {
                    drain_payloads(self, &mut stack);
                    if stack.capacity() > DROP_STACK_RETAIN {
                        stack.shrink_to(DROP_STACK_RETAIN);
                    }
                    true
                }
                // Re-entrant. The fast path above should make this unreachable, but
                // correctness must not rest on that — fall back to a private work-list.
                Err(_) => false,
            })
            .unwrap_or(false);
        if !reused {
            drain_payloads(self, &mut Vec::new());
        }
    }
}

thread_local! {
    /// The one payload behind every value that carries nothing. A tuple's identity is its
    /// `tuple_id`, which lives in the `Value` rather than the payload, so `[]`, `Ok`, `Nil`,
    /// `Done` and every other field-less tuple can share a single immutable payload. That is
    /// the majority of tuple construction — and, since `IsType` and `Equal` answer with
    /// `Ok`/`[]`, every pattern test allocated one before this existed.
    ///
    /// Sharing is sound because payloads are immutable: annotations attach copy-on-write (see
    /// [`Value::annotated`]), equality is structural, and nothing anywhere takes
    /// `Rc::get_mut` or compares payloads by pointer.
    ///
    /// Per-thread, necessarily (`Rc` is not `Sync`) and preferably: each worker interns its
    /// own, so the refcount is never a cache line contended between workers.
    static EMPTY_PAYLOAD: Rc<Payload> = Rc::new(Payload::new(Vec::new()));
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
    Tuple(usize, Rc<Payload>),
    Function(usize, Rc<Payload>),
    // builtin_id (index into builtins table), plus annotations when attached. The payload's
    // elements are always empty — the slot exists only so builtins can carry annotations
    // like any other callable; the bare, un-annotated form is `None` (no allocation). The
    // `Option` fits the variant's existing padding, so `Value` stays at 24 bytes.
    Builtin(usize, Option<Rc<Payload>>),
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
    /// The same value with every table reference rewritten through `remaps` — for
    /// transplanting a compile-time value (a cached module value) between programs.
    /// Heap binary references are execution-local, not table references, and pass
    /// through untouched, as do process/resource/ref identities (which cannot occur
    /// in compile-time values anyway).
    pub fn remap_ids(&self, remaps: &crate::bytecode::IdRemaps) -> Value {
        fn remap_payload(payload: &Payload, remaps: &crate::bytecode::IdRemaps) -> Payload {
            let elements = payload
                .elements
                .iter()
                .map(|value| value.remap_ids(remaps))
                .collect();
            let annotations = payload
                .annotations()
                .iter()
                .map(|(key, value)| {
                    (
                        *remaps.annotation_keys.get(key).unwrap_or(key),
                        value.remap_ids(remaps),
                    )
                })
                .collect();
            Payload::with_annotations(elements, annotations).with_type_argument(
                payload
                    .type_argument()
                    .map(|type_id| *remaps.types.get(&type_id).unwrap_or(&type_id)),
            )
        }
        match self {
            Value::Int(_) | Value::BigInt(_) | Value::Reference(_) => self.clone(),
            Value::Binary(Binary::Constant(idx)) => {
                Value::Binary(Binary::Constant(*remaps.constants.get(idx).unwrap_or(idx)))
            }
            Value::Binary(Binary::Data(_)) => self.clone(),
            Value::Tuple(tuple_id, payload) => Value::Tuple(
                *remaps.tuples.get(tuple_id).unwrap_or(tuple_id),
                remap_payload(payload, remaps).shared(),
            ),
            Value::Function(function_id, payload) => Value::Function(
                *remaps.functions.get(function_id).unwrap_or(function_id),
                remap_payload(payload, remaps).shared(),
            ),
            Value::Builtin(builtin_id, payload) => Value::Builtin(
                *remaps.builtins.get(builtin_id).unwrap_or(builtin_id),
                payload
                    .as_ref()
                    .map(|payload| remap_payload(payload, remaps).shared()),
            ),
            Value::Process(..) | Value::Resource(..) => self.clone(),
        }
    }

    /// Create a NIL tuple value
    pub fn nil() -> Self {
        Value::Tuple(NIL, Payload::empty())
    }

    /// Create an OK tuple value
    pub fn ok() -> Self {
        Value::Tuple(OK, Payload::empty())
    }

    /// Construct a tuple value from owned fields.
    pub fn tuple(type_id: usize, fields: Vec<Value>) -> Self {
        Value::Tuple(type_id, Payload::new(fields).shared())
    }

    /// Construct a function value from owned captures.
    pub fn function(function_index: usize, captures: Vec<Value>) -> Self {
        Value::Function(function_index, Payload::new(captures).shared())
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
                Some(
                    Payload::new(vec![])
                        .with_type_argument(type_argument)
                        .shared(),
                ),
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

    /// Whether this value owns an `Rc<Payload>` — i.e. whether dropping it can recurse.
    /// Used by [`Payload`]'s iterative drop to decide what to move onto its work-list.
    #[inline]
    fn owns_payload(&self) -> bool {
        matches!(
            self,
            Value::Tuple(..) | Value::Function(..) | Value::Builtin(_, Some(_))
        )
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
        let payload = Payload::with_annotations(elements, annotations)
            .with_type_argument(type_argument)
            .shared();
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
