//! The labels type-directed builtins build tuples with. A whole-type test reads a value by its
//! label, so a decoded value must be labelled as what it is known to be — its type's member,
//! closed against the type around it — and never as a fragment of that type whose recursive
//! reference would fit anything.

use crate::common::*;

const LIST: &str = r#"
    'l = Nil | Cons['int, ^]
    'one = Cons['int, Nil]
    test = #('l | []) { | ='one => One | ='l => List | No }
"#;

#[test]
fn test_data_decode_labels_recursive_values_honestly() {
    // A two-element list must not pass as a one-element one.
    quiver()
        .evaluate(&format!(
            r#"{LIST} %data.decode<'l> "Cons[1, Cons[2, Nil]]" ~> test ~"#
        ))
        .expect("List");
    // A one-element one does: a type test checks what the value holds, however it was built.
    quiver()
        .evaluate(&format!(
            r#"{LIST} %data.decode<'l> "Cons[1, Nil]" ~> test ~"#
        ))
        .expect("One");
}

#[test]
fn test_json_decode_labels_recursive_values_honestly() {
    quiver()
        .evaluate(&format!(
            r#"{LIST} %json.parse "[1, 2]" ~> %json.decode<'l> ~ ~> test ~"#
        ))
        .expect("List");
}

#[test]
fn test_json_encode_labels_its_documents_honestly() {
    // The document's cells are `'%json`'s own members, closed against it: a two-element array
    // is a JSON value, and not a one-element one.
    quiver()
        .evaluate(&format!(
            r#"
            {LIST}
            'single = Array[Cons['%json, Nil]]
            %json.encode<'l> Cons[1, Cons[2, Nil]] ~> {{ ='single => Single | ='%json => Json | No }}
            "#
        ))
        .expect("Json");
    quiver()
        .evaluate(&format!(
            r#"{LIST} %json.encode<'l> Cons[1, Cons[2, Nil]] ~> %json.decode<'l> ~ ~> test ~"#
        ))
        .expect("List");
}

const NESTED: &str = r#"
    'j = Z[z: 'int] | W[items: (Nil | Cons[^1, ^])]
    'flat = W[items: (Nil | Cons[Z[z: 'int], ^])]
    test = #('j | []) { | ='flat => Flat | ='j => J | No }
"#;

#[test]
fn test_labels_close_references_reaching_past_the_nearest_binder() {
    // An element's `^1` reaches past the list to `'j`, so the element's label depends on both
    // binders the decoder entered. Labelled honestly, the decoded value is a `'j` and not a
    // `'flat` when it holds a nested `W`; when its contents fit `'flat`, a test finds so.
    quiver()
        .evaluate(&format!(
            r#"{NESTED} %data.decode<'j> "W[items: Cons[Z[z: 1], Cons[W[items: Nil], Nil]]]" ~> test ~"#
        ))
        .expect("J");
    quiver()
        .evaluate(&format!(
            r#"{NESTED} %data.decode<'j> "W[items: Cons[Z[z: 1], Nil]]" ~> test ~"#
        ))
        .expect("Flat");
    quiver()
        .evaluate(&format!(
            r#"{NESTED} %json.parse "\{{\"items\": [\{{\"z\": 1}}, \{{\"items\": []}}]}}" ~> %json.decode<'j> ~ ~> test ~"#
        ))
        .expect("J");
    // As does the literal, built by the compiler at exactly its shape.
    quiver()
        .evaluate(&format!(
            r#"{NESTED} W[items: Cons[Z[z: 1], Nil]] ~> test ~"#
        ))
        .expect("Flat");
}

#[test]
fn test_labels_close_optional_recursive_fields() {
    // Closing `(^1 | [])` leaves `'l | []` in the field.
    quiver()
        .evaluate(
            r#"
            'l = End | L[n: 'int, next: (^1 | [])]
            'short = L[n: 'int, next: []]
            %data.decode<'l> "L[n: 1, next: L[n: 2, next: []]]"
              ~> { ='short => Short | ='l => List | No }
            "#,
        )
        .expect("List");
}

#[test]
fn test_labels_survive_into_later_entries() {
    // A later entry's compile adds to the session's tables; the value decoded earlier keeps
    // testing as it did.
    quiver()
        .evaluate(&format!(
            r#"{LIST} d = %data.decode<'l> "Cons[1, Cons[2, Nil]]""#
        ))
        .then_evaluate("d ~> test ~")
        .expect("List")
        .then_evaluate(r#"%data.decode<'l> "Cons[3, Cons[4, Nil]]" ~> test ~"#)
        .expect("List");
}
