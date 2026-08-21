//! Serde round trips for the value and wire types.
//!
//! The web transport is the reason this matters: `WebWorkerHandle::send` does
//! `serde_json::to_string(&command)`, so every `WireValue` in a message crosses a WASM worker
//! boundary as JSON and back. Native workers pass values by move and never exercise these
//! impls, so a break here would show up only in the browser.
//!
//! `Value`'s round trip covers the other serialized path: a `ModuleArtifact` holds a
//! compile-time value and is written to (and read back from) the artifact cache.

use quiver_core::binary::BinaryData;
use quiver_core::value::{Binary, Value};
use quiver_core::wire::{WirePayload, WireValue};
use std::rc::Rc;
use std::sync::Arc;

#[test]
fn wire_json_round_trip() {
    let bytes: Arc<[u8]> = vec![1u8, 2, 3, 250].into();
    let wire = WireValue::Tuple(
        7,
        WirePayload::with_annotations(
            vec![WireValue::Binary(bytes.clone()), WireValue::Int(-4)],
            vec![(1, WireValue::Binary(vec![9u8, 9].into()))],
        ),
    );
    let json = serde_json::to_string(&wire).expect("serialize");
    println!("wire json: {json}");
    let back: WireValue = serde_json::from_str(&json).expect("deserialize");
    assert_eq!(back, wire, "wire value survives a JSON round trip");
}

#[test]
fn value_json_round_trip() {
    let value = Value::tuple(
        7,
        vec![
            Value::Binary(Binary::Data(Rc::new(BinaryData::new(vec![1, 2, 3])))),
            // A rope: must come back as the same bytes, flattened.
            Value::Binary(Binary::Data(Rc::new(BinaryData::concat(
                Rc::new(BinaryData::new(vec![4, 5])),
                Rc::new(BinaryData::new(vec![6])),
            )))),
            Value::Binary(Binary::Constant(2)),
        ],
    );
    let json = serde_json::to_string(&value).expect("serialize");
    println!("value json: {json}");
    let back: Value = serde_json::from_str(&json).expect("deserialize");
    let Value::Tuple(_, fields) = &back else {
        panic!("expected a tuple")
    };
    let bytes = |v: &Value| match v {
        Value::Binary(Binary::Data(d)) => d.to_vec(),
        other => panic!("expected owned bytes, got {other:?}"),
    };
    assert_eq!(bytes(&fields[0]), vec![1, 2, 3]);
    assert_eq!(
        bytes(&fields[1]),
        vec![4, 5, 6],
        "rope flattens to its bytes"
    );
    assert!(matches!(fields[2], Value::Binary(Binary::Constant(2))));
}

/// The two things a payload carries *besides* its elements. Both live in one boxed `Extras`
/// and are serialized by a hand-written impl that emits each field only when present, so
/// neither is exercised by the round trip above — and neither can be checked with `assert_eq!`,
/// since `Value`'s equality ignores annotations by design and compares builtins by id and type
/// argument alone.
#[test]
fn value_extras_survive_a_json_round_trip() {
    const DOC: usize = 3;
    const TYPE_ID: usize = 11;

    let annotated = Value::tuple(7, vec![Value::Int(1)])
        .annotated(DOC, Value::Int(42))
        .expect("a tuple can carry an annotation");
    let back: Value = serde_json::from_str(&serde_json::to_string(&annotated).expect("serialize"))
        .expect("deserialize");
    assert_eq!(
        back.get_annotation(DOC),
        Some(&Value::Int(42)),
        "annotation survives"
    );
    assert_eq!(back, annotated, "and the elements still compare equal");

    let typed = Value::builtin_typed(5, Some(TYPE_ID));
    let back: Value = serde_json::from_str(&serde_json::to_string(&typed).expect("serialize"))
        .expect("deserialize");
    assert_eq!(
        back.type_argument(),
        Some(TYPE_ID),
        "an instantiated builtin keeps its type argument"
    );

    // A bare builtin must not gain one — the empty `Extras` is dropped, not serialized as a
    // present-but-empty field.
    let bare = Value::builtin(5);
    let back: Value =
        serde_json::from_str(&serde_json::to_string(&bare).expect("serialize")).expect("des");
    assert_eq!(back.type_argument(), None, "a bare builtin carries none");
}
