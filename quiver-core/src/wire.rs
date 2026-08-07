//! The form a value takes between processes.
//!
//! A [`Value`](crate::value::Value) is meaningful only inside the worker that owns it: its
//! payloads, and a binary's rope spine, are `Rc` handles into that worker's memory, and `Rc`
//! is not `Send`. So a value cannot cross a process boundary — which the type system enforces
//! rather than a convention.
//!
//! [`WireValue`] is the form that can: `Send`, self-contained, and carrying a binary's bytes as
//! an `Arc` handle. What it replaces is the `(Value, Vec<Vec<u8>>)` pair, where the value's
//! binary references were indices into a side-channel vec that had to be remapped on both
//! sides — a representation that was only ever a wire format wearing a `Value`'s clothes.
//!
//! Tuple structure is copied and binary *bytes* are shared, which is the split BEAM makes
//! for the same reason: a message's shape is small and process-local, while its buffers are
//! large and immutable. Quiver's semantics are unaffected either way — values are immutable
//! and a send is a copy (only resources move; see the spec's "Resource ownership") — so
//! whether the bytes are copied or shared is not observable to a program.

use crate::process::ProcessId;
use crate::value::ResourceId;
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// A value in transit between processes. Mirrors [`Value`](crate::value::Value), except that
/// its binaries carry a `Send` handle to their bytes rather than the process-local rope.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum WireValue {
    Int(i64),
    BigInt(BigInt),
    /// A heap binary, as a shared handle. `Arc`, so the bytes cross a worker boundary by
    /// pointer rather than by copy — the sender's leaf and the receiver's are the same
    /// allocation. (A rope is realised into a fresh handle at the boundary; only its spine
    /// is process-local.)
    Binary(Arc<[u8]>),
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

/// The wire form of a tuple's or function's payload.
#[derive(Debug, PartialEq, Default, Serialize, Deserialize)]
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
    /// The wire twin of [`crate::value::Value::collect_code_refs`], for the values the
    /// environment holds host-side (resolved-but-unpolled request results, aggregation
    /// slices) — roots for the code-reclamation sweep.
    pub fn collect_code_refs(
        &self,
        functions: &mut std::collections::HashSet<usize>,
        constants: &mut std::collections::HashSet<usize>,
    ) {
        fn push<'a>(stack: &mut Vec<&'a WireValue>, payload: &'a WirePayload) {
            stack.extend(payload.elements.iter());
            if let Some(annotations) = &payload.annotations {
                stack.extend(annotations.iter().map(|(_, value)| value));
            }
        }
        let mut stack: Vec<&WireValue> = vec![self];
        while let Some(value) = stack.pop() {
            match value {
                WireValue::Constant(index) => {
                    constants.insert(*index);
                }
                WireValue::Function(index, payload) => {
                    functions.insert(*index);
                    push(&mut stack, payload);
                }
                WireValue::Tuple(_, payload) => push(&mut stack, payload),
                WireValue::Builtin(_, Some(payload)) => push(&mut stack, payload),
                _ => {}
            }
        }
    }

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
        // Field-by-field rather than `..Default::default()`: the struct-update syntax moves
        // the remaining fields out of a temporary, which `Drop` forbids.
        WireValue::Tuple(
            type_id,
            WirePayload {
                elements,
                annotations: None,
                type_argument: None,
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

    /// Build the display `Value` iteratively, for the same reason every other walk over value
    /// structure is iterative: a message nests once per list element, and a recursive render
    /// aborted the process on a 200,000-element list. One frame per composite.
    fn display_value(&self, heap: &mut Vec<Vec<u8>>) -> crate::value::Value {
        use crate::value::Value;

        struct Frame<'a> {
            node: WireNode,
            payload: &'a WirePayload,
            done: Vec<Value>,
        }

        fn assemble(frame: Frame) -> Value {
            let Frame {
                node,
                payload,
                mut done,
            } = frame;
            let annotations = payload
                .annotations()
                .iter()
                .map(|(key, _)| *key)
                .zip(done.split_off(payload.elements.len()))
                .collect();
            let built = crate::value::Payload::with_annotations(done, annotations)
                .with_type_argument(payload.type_argument)
                .shared();
            match node {
                WireNode::Tuple(id) => Value::Tuple(id, built),
                WireNode::Function(id) => Value::Function(id, built),
                WireNode::Builtin(id) => Value::Builtin(id, Some(built)),
            }
        }

        let mut stack: Vec<Frame<'_>> = Vec::new();
        let mut value = self;

        'descend: loop {
            let mut converted = loop {
                let Some((node, payload)) = WireNode::of(value) else {
                    break value.display_leaf(heap);
                };
                stack.push(Frame {
                    node,
                    payload,
                    done: Vec::with_capacity(payload.elements.len()),
                });
                match wire_child(payload, 0) {
                    Some(next) => value = next,
                    None => break assemble(stack.pop().expect("just pushed")),
                }
            };

            loop {
                let Some(frame) = stack.last_mut() else {
                    return converted;
                };
                frame.done.push(converted);
                match wire_child(frame.payload, frame.done.len()) {
                    Some(next) => {
                        value = next;
                        continue 'descend;
                    }
                    None => converted = assemble(stack.pop().expect("just borrowed")),
                }
            }
        }
    }

    /// The display form of a wire value that owns no payload.
    fn display_leaf(&self, heap: &mut Vec<Vec<u8>>) -> crate::value::Value {
        use crate::value::{Binary, Value};
        match self {
            WireValue::Int(n) => Value::Int(*n),
            WireValue::BigInt(n) => Value::integer(n.clone()),
            WireValue::Binary(bytes) => Value::Binary(Binary::Data(std::rc::Rc::new(
                crate::binary::BinaryData::Owned(bytes.clone()),
            ))),
            WireValue::Constant(index) => Value::Binary(Binary::Constant(*index)),
            WireValue::Reference(id) => Value::Reference(*id),
            WireValue::Builtin(id, None) => Value::Builtin(*id, None),
            WireValue::Process(pid, function_index) => Value::Process(*pid, *function_index),
            WireValue::Resource(id, type_id) => Value::Resource(*id, *type_id),
            WireValue::Tuple(..) | WireValue::Function(..) | WireValue::Builtin(_, Some(_)) => {
                let _ = heap;
                unreachable!("composites are handled by the frame stack")
            }
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

/// Which composite an iterative walk over wire values is rebuilding when its children are done.
#[derive(Clone, Copy)]
enum WireNode {
    Tuple(usize),
    Function(usize),
    Builtin(usize),
}

impl WireNode {
    /// The node kind a composite wire value rebuilds as, with its payload.
    fn of(value: &WireValue) -> Option<(WireNode, &WirePayload)> {
        match value {
            WireValue::Tuple(id, payload) => Some((WireNode::Tuple(*id), payload)),
            WireValue::Function(id, payload) => Some((WireNode::Function(*id), payload)),
            WireValue::Builtin(id, Some(payload)) => Some((WireNode::Builtin(*id), payload)),
            _ => None,
        }
    }

    fn rebuild(self, payload: WirePayload) -> WireValue {
        match self {
            WireNode::Tuple(id) => WireValue::Tuple(id, payload),
            WireNode::Function(id) => WireValue::Function(id, payload),
            WireNode::Builtin(id) => WireValue::Builtin(id, Some(payload)),
        }
    }
}

/// The `index`-th child of a payload in `all_values` order: elements, then annotation values.
fn wire_child(payload: &WirePayload, index: usize) -> Option<&WireValue> {
    payload.elements.get(index).or_else(|| {
        payload
            .annotations()
            .get(index - payload.elements.len())
            .map(|(_, value)| value)
    })
}

impl Clone for WirePayload {
    /// Deep-copy iteratively. The derived `Clone` walks one stack frame per level of nesting,
    /// and a message nests once per list element — so cloning a result to fan it out to several
    /// requesters, or a `Changed` wakeup to several subscribers, aborted the process on a
    /// 1,000,000-element list.
    ///
    /// On `WirePayload` rather than [`WireValue`] for the same reason as [`Drop`]: the value
    /// stays freely destructurable, and its derived `Clone` bottoms out here after one step.
    fn clone(&self) -> Self {
        struct Frame<'a> {
            node: WireNode,
            src: &'a WirePayload,
            done: Vec<WireValue>,
        }

        fn assemble(frame: Frame) -> WireValue {
            frame.node.rebuild(copy_payload(frame.src, frame.done))
        }

        fn copy_payload(src: &WirePayload, mut done: Vec<WireValue>) -> WirePayload {
            let annotations = src
                .annotations()
                .iter()
                .map(|(key, _)| *key)
                .zip(done.split_off(src.elements.len()))
                .collect();
            WirePayload::with_annotations(done, annotations).with_type_argument(src.type_argument)
        }

        let mut stack: Vec<Frame<'_>> = Vec::new();
        let mut done: Vec<WireValue> = Vec::with_capacity(self.elements.len());
        let mut index = 0usize;
        let mut src = self;

        loop {
            match wire_child(src, index) {
                Some(value) => match WireNode::of(value) {
                    // A composite: descend, saving this level's progress.
                    Some((node, payload)) => {
                        stack.push(Frame {
                            node,
                            src,
                            done: std::mem::take(&mut done),
                        });
                        src = payload;
                        index = 0;
                        done = Vec::with_capacity(payload.elements.len());
                    }
                    // A leaf: its derived `Clone` copies no children, so it cannot recurse.
                    None => {
                        done.push(value.clone());
                        index += 1;
                    }
                },
                None => match stack.pop() {
                    Some(frame) => {
                        let finished = assemble(Frame {
                            node: frame.node,
                            src,
                            done,
                        });
                        src = frame.src;
                        done = frame.done;
                        done.push(finished);
                        index = done.len();
                    }
                    None => return copy_payload(src, done),
                },
            }
        }
    }
}

thread_local! {
    /// Work-list for [`WirePayload`]'s drop, reused across drops — a fresh `Vec` each time
    /// would cost an allocation per composite on the message path.
    static WIRE_DROP_STACK: std::cell::RefCell<Vec<WirePayload>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Capacity retained between drops, so one pathological message does not reserve a large buffer
/// for the rest of the thread's life.
const WIRE_DROP_RETAIN: usize = 1024;

/// Move every payload this one owns onto `stack`, leaving it empty so its own field drops
/// terminate immediately.
fn take_wire_payloads(payload: &mut WirePayload, stack: &mut Vec<WirePayload>) {
    let annotations = payload.annotations.take().map(|a| *a).unwrap_or_default();
    let children = payload
        .elements
        .drain(..)
        .chain(annotations.into_iter().map(|(_, value)| value));
    for value in children {
        // Moving a payload out of a `WireValue` is legal precisely because `WireValue` has no
        // `Drop` of its own — which is why this impl lives here rather than one level up.
        match value {
            WireValue::Tuple(_, payload) | WireValue::Function(_, payload) => stack.push(payload),
            WireValue::Builtin(_, Some(payload)) => stack.push(payload),
            _ => {}
        }
    }
}

impl Drop for WirePayload {
    /// Drop iteratively. The derived glue walks one stack frame per level of nesting, and a
    /// message nests once per list element — so discarding a deeply-nested message aborted the
    /// process. That happens on ordinary paths: the CLI drops a result after printing it, and
    /// the environment drops a message whose target has gone, both on `main`'s 8 MiB stack.
    ///
    /// This is deliberately on `WirePayload` rather than on [`WireValue`]. Implementing `Drop`
    /// for the value would make it illegal to move out of its fields, which is exactly what
    /// [`Executor::from_wire`](crate::executor::Executor::from_wire) does to adopt a binary's
    /// handle instead of copying its bytes. Owning the recursion one level down costs nothing
    /// and keeps the value freely destructurable.
    fn drop(&mut self) {
        if self.elements.is_empty() && self.annotations.is_none() {
            return;
        }
        let reused = WIRE_DROP_STACK
            .try_with(|cell| match cell.try_borrow_mut() {
                Ok(mut stack) => {
                    take_wire_payloads(self, &mut stack);
                    while let Some(mut payload) = stack.pop() {
                        take_wire_payloads(&mut payload, &mut stack);
                    }
                    if stack.capacity() > WIRE_DROP_RETAIN {
                        stack.shrink_to(WIRE_DROP_RETAIN);
                    }
                    true
                }
                // Re-entrant: the fast path above should preclude it, but correctness must not
                // rest on that.
                Err(_) => false,
            })
            .unwrap_or(false);
        if !reused {
            let mut stack = Vec::new();
            take_wire_payloads(self, &mut stack);
            while let Some(mut payload) = stack.pop() {
                take_wire_payloads(&mut payload, &mut stack);
            }
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

    /// Take this payload's parts, for a receiver rebuilding a `Payload` from it — which owns
    /// the wire form and is about to drop it, so its buffers should move rather than be copied.
    ///
    /// `&mut self` rather than `self`: this type implements [`Drop`] (see the impl), which
    /// makes moving out of its fields illegal. Taking leaves it empty, so the drop that follows
    /// hits its fast path.
    pub fn take_parts(&mut self) -> (Vec<WireValue>, Vec<(usize, WireValue)>, Option<usize>) {
        (
            std::mem::take(&mut self.elements),
            self.annotations.take().map(|a| *a).unwrap_or_default(),
            self.type_argument,
        )
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

    fn byte_size(&self) -> usize {
        self.all_values().map(WireValue::byte_size).sum()
    }
}
