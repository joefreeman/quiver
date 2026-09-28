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

/// The shared payload of a tuple or function value: its elements, and — for the small
/// minority of values that carry either — its annotations and type argument. Reference-counted,
/// so cloning a value is O(1) whatever it contains.
#[derive(Debug, Deserialize)]
#[serde(from = "PayloadData")]
pub struct Payload {
    /// The elements, stored in the payload itself when there are one or two of them (see
    /// [`Elements`]).
    elements: Elements,
    /// Everything a payload only *sometimes* carries, in one box, so an ordinary tuple pays a
    /// single pointer-sized `None` for all of it. Both parts are rare and neither is on a read
    /// path, which is what makes the shared indirection the right trade: `type_argument` in
    /// particular was 16 inline bytes on *every* tuple in the system to serve instantiated
    /// builtins alone.
    ///
    /// Not a serde field: `Payload` deserializes via [`PayloadData`] (see the `from` attribute),
    /// which reads the flat shape and routes through the ordinary constructors.
    #[serde(skip)]
    extras: Option<Box<Extras>>,
}

/// Where a payload's elements live. A payload sits behind an `Rc`, so holding the elements
/// *in* it makes a small tuple one heap allocation rather than two — but only for a caller
/// that never builds a `Vec`, since that vector is itself the second allocation (see
/// [`Elements::from_reversed_fn`]).
///
/// One variant per small arity rather than an array plus a length: the length is then static
/// per variant, every array is fully initialised, and the whole type needs no `unsafe` — no
/// `MaybeUninit`, no raw slice reconstruction, no hand-written `Drop`.
///
/// One and two, because that is where the mass is. Measured payload-allocation arities: a JSON
/// parse is 70% two-field and 78% at most two; an arithmetic loop is 100% two-field (every call
/// argument is a pair); list building is 98%; HTML rendering, the widest of the four, is 64% at
/// most two and 95% at most three. The cost is that the enum is sized to `Two`, so a one-field
/// tuple occupies a two-field footprint and a `Heap` payload carries 48 bytes it never uses —
/// which is what stops this growing to three or four.
///
/// The arrays are what make the slice free: `&[Value; 2]` coerces to `&[Value]` with the
/// elements guaranteed adjacent, where two separate fields would need `repr(C)` and pointer
/// arithmetic to span. (`One` could hold a bare `Value` and reach a slice through
/// `slice::from_ref`; it is an array only to match `Two`.)
#[derive(Debug)]
enum Elements {
    /// No elements. Most such payloads are interned by [`Payload::shared`] and never reach
    /// here, but one carrying annotations is not — an `:error`-bearing nil is exactly that,
    /// and `%parse` builds one per failed alternative.
    Zero,
    One([Value; 1]),
    Two([Value; 2]),
    /// Any larger arity: a separate buffer, and a second allocation.
    Heap(Box<[Value]>),
}

impl From<Vec<Value>> for Elements {
    /// Handed a `Vec`, a payload saves no allocation — the vector is the second one, and
    /// `into_boxed_slice` was reusing it for free. Small arities still convert, so that the
    /// representation is canonical whatever built it, and so the vector is released rather
    /// than retained.
    fn from(elements: Vec<Value>) -> Self {
        if elements.is_empty() {
            return Elements::Zero;
        }
        let elements = match <[Value; 2]>::try_from(elements) {
            Ok(two) => return Elements::Two(two),
            Err(elements) => elements,
        };
        match <[Value; 1]>::try_from(elements) {
            Ok(one) => Elements::One(one),
            Err(elements) => Elements::Heap(elements.into_boxed_slice()),
        }
    }
}

impl Elements {
    /// Build from `size` elements produced **last first** — the order popping a stack yields
    /// them — without an intermediate `Vec`. This is the path that makes the inline storage
    /// pay: it is the only way a small tuple costs one allocation instead of two.
    fn from_reversed_fn(size: usize, next: &mut impl FnMut() -> Value) -> Elements {
        match size {
            0 => Elements::Zero,
            1 => Elements::One([next()]),
            2 => {
                let second = next();
                let first = next();
                Elements::Two([first, second])
            }
            _ => {
                let mut values = Vec::with_capacity(size);
                for _ in 0..size {
                    values.push(next());
                }
                values.reverse();
                Elements::Heap(values.into_boxed_slice())
            }
        }
    }
}

impl std::ops::Deref for Elements {
    type Target = [Value];

    fn deref(&self) -> &[Value] {
        match self {
            Elements::Zero => &[],
            Elements::One(values) => values,
            Elements::Two(values) => values,
            Elements::Heap(values) => values,
        }
    }
}

impl std::ops::DerefMut for Elements {
    fn deref_mut(&mut self) -> &mut [Value] {
        match self {
            Elements::Zero => &mut [],
            Elements::One(values) => values,
            Elements::Two(values) => values,
            Elements::Heap(values) => values,
        }
    }
}

/// The occasional cargo of a [`Payload`]. Never constructed empty — a payload with nothing to
/// carry holds `None` instead, so `Some(extras)` always means at least one of these is present.
#[derive(Debug)]
struct Extras {
    /// Annotations attached to the owning value: `(key id, value)` pairs, sorted by key id.
    /// Invisible to equality and pattern matching — only `GetAnnotation` observes them.
    annotations: Vec<(usize, Value)>,
    /// A type-consuming builtin's explicit type argument (`__data_decode__<'t>` → the
    /// resolved type id), carried on the *value* so an instantiated builtin flows
    /// through bindings and generic code intact. Unlike annotations it is operational
    /// (the implementation reads it), so equality compares it. Always `None` on tuple
    /// and function payloads.
    type_argument: Option<usize>,
}

impl Extras {
    /// Whether this carries nothing, and so should be dropped for a bare `None`. The invariant
    /// the type's "never constructed empty" contract rests on.
    fn is_empty(&self) -> bool {
        self.annotations.is_empty() && self.type_argument.is_none()
    }
}

/// Hand-written so the serialized shape is unchanged by the boxing: `elements`, plus
/// `annotations` and `type_argument` when present. That is exactly what [`PayloadData`]
/// reads back, and only self-describing formats (`serde_json`) are in use, so an omitted
/// field and an absent one are the same thing.
impl Serialize for Payload {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeStruct;

        let annotations = self.annotations();
        let type_argument = self.type_argument();
        let fields =
            1 + usize::from(!annotations.is_empty()) + usize::from(type_argument.is_some());

        let mut payload = serializer.serialize_struct("Payload", fields)?;
        payload.serialize_field("elements", &*self.elements)?;
        if !annotations.is_empty() {
            payload.serialize_field("annotations", annotations)?;
        }
        if let Some(type_argument) = type_argument {
            payload.serialize_field("type_argument", &type_argument)?;
        }
        payload.end()
    }
}

/// The serialized shape of [`Payload`]. Deserialization routes through the ordinary
/// constructors rather than building the struct directly, so a deserialized payload is
/// indistinguishable from a constructed one.
#[derive(Deserialize)]
struct PayloadData {
    elements: Vec<Value>,
    // `Option` so an absent field deserializes without allocating; boxed only to keep this
    // shape one word wide, since it is unboxed straight into `Extras`.
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
            elements: elements.into(),
            extras: None,
        }
    }

    /// Build from `size` elements produced **last first** — the order popping a stack yields
    /// them — without an intermediate `Vec`. The runtime's tuple construction goes through
    /// here: with a `Vec` the inline storage saves nothing (see [`Elements::from_reversed_fn`]),
    /// so this is what turns a small tuple into one allocation instead of two.
    pub fn from_reversed_fn(size: usize, mut next: impl FnMut() -> Value) -> Self {
        Payload {
            elements: Elements::from_reversed_fn(size, &mut next),
            extras: None,
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
            elements: elements.into(),
            extras: Some(Box::new(Extras {
                annotations,
                type_argument: None,
            })),
        }
    }

    /// The same payload carrying a type argument (see the field). Builder-style, used
    /// when constructing an instantiated builtin value or re-attaching annotations to
    /// one.
    ///
    /// Setting it back to `None` drops an otherwise-empty box, keeping [`Extras`]'s
    /// never-empty invariant — on which `carries_nothing`, and so empty-payload interning,
    /// depends.
    pub fn with_type_argument(mut self, type_argument: Option<usize>) -> Self {
        match (&mut self.extras, type_argument) {
            (Some(extras), _) => {
                extras.type_argument = type_argument;
                if extras.is_empty() {
                    self.extras = None;
                }
            }
            (None, Some(_)) => {
                self.extras = Some(Box::new(Extras {
                    annotations: Vec::new(),
                    type_argument,
                }));
            }
            (None, None) => {}
        }
        self
    }

    /// A type-consuming builtin's explicit type argument, if the owning value carries one.
    pub fn type_argument(&self) -> Option<usize> {
        self.extras.as_ref().and_then(|e| e.type_argument)
    }

    /// The annotations attached to the owning value (empty if none).
    pub fn annotations(&self) -> &[(usize, Value)] {
        self.extras
            .as_ref()
            .map_or(&[], |e| e.annotations.as_slice())
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
    /// by value: out of line, the `Payload` move would be a real memcpy on the hottest path
    /// in the runtime. (Measured no difference either way on a noisy machine — it is the safe
    /// default for a wrapper this small, not a tuned result.)
    #[inline]
    pub fn shared(self) -> Rc<Payload> {
        if self.carries_nothing() {
            return Payload::empty();
        }
        Rc::new(self)
    }

    /// Whether this payload has no content of its own, and so is interchangeable with every
    /// other such payload. Elements and annotations are observable; the type argument is
    /// operational (equality compares it), so a payload carrying one is never shared. Both
    /// live in `extras`, which is `None` exactly when neither is present.
    #[inline]
    fn carries_nothing(&self) -> bool {
        self.elements.is_empty() && self.extras.is_none()
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
    /// `elements` and `extras` are distinct fields, so borrowing both at once is disjoint.
    fn all_values_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        self.elements.iter_mut().chain(
            self.extras
                .as_deref_mut()
                .into_iter()
                .flat_map(|extras| extras.annotations.iter_mut().map(|(_, value)| value)),
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
    /// `handle_build`'s `with_capacity` had just saved. Depth-first popping keeps this shallow
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
    /// the majority of tuple construction — and, since `TestType` and `TestEqual` answer with
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
    /// Compare element trees **iteratively**. `Value`'s `PartialEq` delegates a composite to
    /// this, so without a work-list here the pair would recurse once per level of nesting and
    /// abort on a long enough list. On `Payload` rather than `Value` for the same reason as
    /// [`Clone`] and [`Drop`] on the wire types: the delegation bottoms out after one step.
    ///
    /// Annotations are invisible to equality, as everywhere else.
    fn eq(&self, other: &Self) -> bool {
        let mut pending: Vec<(&Value, &Value)> = Vec::new();
        if self.elements.len() != other.elements.len() {
            return false;
        }
        pending.extend(self.elements.iter().zip(other.elements.iter()));
        while let Some((a, b)) = pending.pop() {
            match (a, b) {
                (Value::Tuple(ta, pa), Value::Tuple(tb, pb))
                | (Value::Function(ta, pa), Value::Function(tb, pb)) => {
                    if ta != tb || pa.elements.len() != pb.elements.len() {
                        return false;
                    }
                    pending.extend(pa.elements.iter().zip(pb.elements.iter()));
                }
                // Leaves, and `Builtin` (whose payload carries no elements), compare directly:
                // this cannot re-enter, because every composite is handled above.
                (a, b) if a == b => {}
                _ => return false,
            }
        }
        true
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
    /// Every resource handle in this value: in elements, captures and annotations alike, since
    /// each can carry one. Iterative, as a value may be a long list, and each shared payload is
    /// walked once, so a value built with heavy sharing costs no more than its distinct nodes.
    pub fn resources(&self) -> Vec<ResourceId> {
        let mut found = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut pending = vec![self];
        while let Some(value) = pending.pop() {
            match value {
                Value::Resource(id, _) => found.push(*id),
                Value::Tuple(_, payload)
                | Value::Function(_, payload)
                | Value::Builtin(_, Some(payload))
                    if visited.insert(Rc::as_ptr(payload)) =>
                {
                    pending.extend(payload.all_values());
                }
                _ => {}
            }
        }
        found
    }

    /// The same value with every table reference rewritten through `remaps` — for
    /// transplanting a compile-time value (a cached module value) between programs.
    /// Heap binary references are execution-local, not table references, and pass
    /// through untouched, as do process/resource/ref identities (which cannot occur
    /// in compile-time values anyway).
    /// The same value with every table reference rewritten through `remaps` — for
    /// transplanting a compile-time value (a cached module value) between programs.
    /// Heap binary references are execution-local, not table references, and pass
    /// through untouched, as do process/resource/ref identities (which cannot occur
    /// in compile-time values anyway).
    ///
    /// **Iterative**, like every other walk over value structure: a module value is normally
    /// shallow, but nothing enforces that, and the failure mode is an uncatchable abort.
    pub fn remap_ids(&self, remaps: &crate::bytecode::IdRemaps) -> Value {
        /// Which composite a frame rebuilds once its children are remapped.
        enum Node {
            Tuple(usize),
            Function(usize),
            Builtin(usize),
        }

        struct Frame<'a> {
            node: Node,
            src: &'a Payload,
            done: Vec<Value>,
        }

        fn child(payload: &Payload, index: usize) -> Option<&Value> {
            payload.elements.get(index).or_else(|| {
                payload
                    .annotations()
                    .get(index - payload.elements.len())
                    .map(|(_, value)| value)
            })
        }

        fn assemble(frame: Frame, remaps: &crate::bytecode::IdRemaps) -> Value {
            let Frame {
                node,
                src,
                mut done,
            } = frame;
            let annotations = src
                .annotations()
                .iter()
                .map(|(key, _)| {
                    crate::bytecode::IdRemaps::map(&remaps.annotation_keys, "annotation key", *key)
                })
                .zip(done.split_off(src.elements.len()))
                .collect();
            let payload =
                Payload::with_annotations(done, annotations)
                    .with_type_argument(src.type_argument().map(|type_id| {
                        crate::bytecode::IdRemaps::map(&remaps.types, "type", type_id)
                    }))
                    .shared();
            match node {
                Node::Tuple(id) => Value::Tuple(id, payload),
                Node::Function(id) => Value::Function(id, payload),
                Node::Builtin(id) => Value::Builtin(id, Some(payload)),
            }
        }

        fn leaf(value: &Value, remaps: &crate::bytecode::IdRemaps) -> Value {
            match value {
                Value::Binary(Binary::Constant(idx)) => Value::Binary(Binary::Constant(
                    crate::bytecode::IdRemaps::map(&remaps.constants, "constant", *idx),
                )),
                Value::Builtin(id, None) => Value::Builtin(
                    crate::bytecode::IdRemaps::map(&remaps.builtins, "builtin", *id),
                    None,
                ),
                other => other.clone(),
            }
        }

        fn node_of<'a>(
            value: &'a Value,
            remaps: &crate::bytecode::IdRemaps,
        ) -> Option<(Node, &'a Payload)> {
            match value {
                Value::Tuple(id, payload) => Some((
                    Node::Tuple(crate::bytecode::IdRemaps::map(&remaps.tuples, "tuple", *id)),
                    payload,
                )),
                Value::Function(id, payload) => Some((
                    Node::Function(crate::bytecode::IdRemaps::map(
                        &remaps.functions,
                        "function",
                        *id,
                    )),
                    payload,
                )),
                Value::Builtin(id, Some(payload)) => Some((
                    Node::Builtin(crate::bytecode::IdRemaps::map(
                        &remaps.builtins,
                        "builtin",
                        *id,
                    )),
                    payload,
                )),
                _ => None,
            }
        }

        let mut stack: Vec<Frame<'_>> = Vec::new();
        let mut value = self;

        'descend: loop {
            let mut converted = loop {
                let Some((node, payload)) = node_of(value, remaps) else {
                    break leaf(value, remaps);
                };
                stack.push(Frame {
                    node,
                    src: payload,
                    done: Vec::with_capacity(payload.elements.len()),
                });
                match child(payload, 0) {
                    Some(next) => value = next,
                    None => break assemble(stack.pop().expect("just pushed"), remaps),
                }
            };

            loop {
                let Some(frame) = stack.last_mut() else {
                    return converted;
                };
                frame.done.push(converted);
                match child(frame.src, frame.done.len()) {
                    Some(next) => {
                        value = next;
                        continue 'descend;
                    }
                    None => converted = assemble(stack.pop().expect("just borrowed"), remaps),
                }
            }
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
        // Copied out and rebuilt: annotating is a cold path, and `with_annotations` puts the
        // elements back into whichever `Elements` variant fits.
        let elements = payload.map(|p| p.elements.to_vec()).unwrap_or_default();
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

    /// Collect the code this value keeps alive: function table indices (closures, and
    /// through every payload) and constant table indices (constant-backed binaries).
    /// The code-reclamation sweep's per-value root walk. Iterative, to survive deep
    /// cons lists. A pid's root-function index is identity, not a code reference, and
    /// is deliberately not collected — a reclaimed stub keeps what identity tests read.
    pub fn collect_code_refs(
        &self,
        functions: &mut std::collections::HashSet<usize>,
        constants: &mut std::collections::HashSet<usize>,
    ) {
        let mut stack: Vec<&Value> = vec![self];
        while let Some(value) = stack.pop() {
            match value {
                Value::Binary(Binary::Constant(index)) => {
                    constants.insert(*index);
                }
                Value::Function(index, payload) => {
                    functions.insert(*index);
                    stack.extend(payload.all_values());
                }
                Value::Tuple(_, payload) => stack.extend(payload.all_values()),
                Value::Builtin(_, Some(payload)) => stack.extend(payload.all_values()),
                _ => {}
            }
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
