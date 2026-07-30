//! The form a value takes between processes.
//!
//! A [`Value`](crate::value::Value) is meaningful only inside the worker that owns it: a
//! binary is an index into that worker's heap, and a tuple's payload is a refcounted handle
//! into that worker's memory. Neither survives a process boundary.
//!
//! [`WireValue`] is the self-contained form — binaries carry their bytes — so it depends on
//! no executor state and can cross a worker, and eventually a node, boundary. It makes
//! explicit what the transfer already did: Quiver's message semantics have always been
//! *copy* (only resources move; see the spec's "Resource ownership"), and every send already
//! materialised its binaries. What it replaces is the `(Value, Vec<Vec<u8>>)` pair, where the
//! value's heap indices pointed into a side-channel vec that had to be remapped on both
//! sides — a representation that was only ever a wire format wearing a `Value`'s clothes.

use crate::process::ProcessId;
use crate::value::ResourceId;
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

/// A value in transit between processes. Mirrors [`Value`](crate::value::Value) except for
/// binaries, which carry bytes rather than a heap index.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum WireValue {
    Int(i64),
    BigInt(BigInt),
    /// A heap binary, materialised. This is the copy a send has always performed.
    Binary(Vec<u8>),
    /// A constant binary. Every worker loads the same constants table, so this crosses as a
    /// reference rather than as bytes — a program's literals are never copied per message.
    Constant(usize),
    Reference(u64),
    Tuple(usize, WirePayload),
    Function(usize, WirePayload),
    Builtin(usize, Option<WirePayload>),
    Process(ProcessId, usize),
    Resource(ResourceId, usize),
}

/// The wire form of a tuple's or function's payload. Unlike
/// [`Payload`](crate::value::Payload) this carries no `has_heap_refs` cache — that is a
/// property of a value's placement in a particular heap, so it is recomputed on arrival.
#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct WirePayload {
    pub elements: Vec<WireValue>,
    /// `(key id, value)`, as on `Payload`, and boxed for the same reason: a payload sits
    /// inline in its parent's element buffer, so an unboxed `Vec` would put 24 bytes on
    /// every node of a wire tree to serve the rare annotated one. Annotation keys are
    /// program-global ids, so they cross unchanged.
    #[allow(clippy::box_collection)]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    annotations: Option<Box<Vec<(usize, WireValue)>>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub type_argument: Option<usize>,
}

impl WireValue {
    /// The wire form of nil — a spawn with no argument, an entry point's init.
    pub fn nil() -> Self {
        WireValue::Tuple(crate::types::NIL, WirePayload::default())
    }

    /// The wire form of `Ok`.
    pub fn ok() -> Self {
        WireValue::Tuple(crate::types::OK, WirePayload::default())
    }

    /// A wire tuple with the given fields and no annotations.
    pub fn tuple(type_id: usize, elements: Vec<WireValue>) -> Self {
        WireValue::Tuple(
            type_id,
            WirePayload {
                elements,
                ..WirePayload::default()
            },
        )
    }

    /// Whether this is nil — the shape every failure takes. Mirrors
    /// [`Value::is_nil`](crate::value::Value::is_nil): annotations are not elements, so a
    /// *stamped* nil is still nil.
    pub fn is_nil(&self) -> bool {
        matches!(self, WireValue::Tuple(id, payload)
            if *id == crate::types::NIL && payload.elements.is_empty())
    }

    /// Whether this is `Ok`. Mirrors [`Value::is_ok`](crate::value::Value::is_ok).
    pub fn is_ok(&self) -> bool {
        matches!(self, WireValue::Tuple(id, payload)
            if *id == crate::types::OK && payload.elements.is_empty())
    }

    /// A `Value` and heap pair equivalent to this wire value, for **display only**.
    ///
    /// Formatting has ~100 lines of shape-specific handling (`Str`, `Rational`, `Surd`, field
    /// labels) written against `Value`, and duplicating it for the wire form would be worse
    /// than this adapter. Binaries are numbered positionally into the returned vec, which is
    /// exactly what a [`crate::format::BinaryLookup`] expects. The values it builds are inert
    /// — they index no executor heap and must never be executed or stored.
    pub fn for_display(&self) -> (crate::value::Value, Vec<Vec<u8>>) {
        let mut heap = Vec::new();
        let value = self.display_value(&mut heap);
        (value, heap)
    }

    fn display_value(&self, heap: &mut Vec<Vec<u8>>) -> crate::value::Value {
        use crate::value::{Binary, Value};
        match self {
            WireValue::Int(n) => Value::Int(*n),
            WireValue::BigInt(n) => Value::integer(n.clone()),
            WireValue::Binary(bytes) => {
                heap.push(bytes.clone());
                Value::Binary(Binary::Heap(heap.len() - 1))
            }
            WireValue::Constant(index) => Value::Binary(Binary::Constant(*index)),
            WireValue::Reference(id) => Value::Reference(*id),
            WireValue::Tuple(type_id, payload) => {
                Value::Tuple(*type_id, payload.display_payload(heap))
            }
            WireValue::Function(index, payload) => {
                Value::Function(*index, payload.display_payload(heap))
            }
            WireValue::Builtin(id, payload) => Value::Builtin(
                *id,
                payload
                    .as_ref()
                    .map(|payload| payload.display_payload(heap)),
            ),
            WireValue::Process(pid, function_index) => Value::Process(*pid, *function_index),
            WireValue::Resource(id, type_id) => Value::Resource(*id, *type_id),
        }
    }

    /// The bytes this value and everything under it carries. Diagnostic — used to reason
    /// about what a send actually costs.
    pub fn byte_size(&self) -> usize {
        match self {
            WireValue::Binary(bytes) => bytes.len(),
            WireValue::Tuple(_, payload) | WireValue::Function(_, payload) => payload.byte_size(),
            WireValue::Builtin(_, Some(payload)) => payload.byte_size(),
            _ => 0,
        }
    }
}

impl WirePayload {
    /// A payload with the given elements and annotations. Mirrors
    /// [`Payload::with_annotations`](crate::value::Payload::with_annotations).
    pub fn with_annotations(
        elements: Vec<WireValue>,
        annotations: Vec<(usize, WireValue)>,
    ) -> Self {
        WirePayload {
            elements,
            annotations: (!annotations.is_empty()).then(|| Box::new(annotations)),
            type_argument: None,
        }
    }

    /// The same payload carrying a type argument. Builder-style, as on `Payload`.
    pub fn with_type_argument(mut self, type_argument: Option<usize>) -> Self {
        self.type_argument = type_argument;
        self
    }

    /// The annotations attached to the owning value (empty if none).
    pub fn annotations(&self) -> &[(usize, WireValue)] {
        self.annotations.as_deref().map_or(&[], |a| a.as_slice())
    }

    /// All values reachable from this payload: elements, then annotation values. The twin of
    /// [`Payload::all_values`](crate::value::Payload::all_values), and the iterator every
    /// walk over a transferred value must use — an annotation carries a value like any other
    /// field, so it can carry a resource handle like any other field.
    pub fn all_values(&self) -> impl Iterator<Item = &WireValue> {
        self.elements
            .iter()
            .chain(self.annotations().iter().map(|(_, value)| value))
    }

    fn display_payload(&self, heap: &mut Vec<Vec<u8>>) -> std::rc::Rc<crate::value::Payload> {
        use crate::value::Payload;
        Payload::with_annotations(
            self.elements
                .iter()
                .map(|value| value.display_value(heap))
                .collect(),
            self.annotations()
                .iter()
                .map(|(key, value)| (*key, value.display_value(heap)))
                .collect(),
        )
        .with_type_argument(self.type_argument)
        .shared()
    }

    fn byte_size(&self) -> usize {
        self.all_values().map(WireValue::byte_size).sum()
    }
}
