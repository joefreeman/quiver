mod common;
use common::*;

// Quiver data notation (`%data`): encode walks any data value into the language's
// literal syntax; decode is type-consuming — the expected type drives the parse, tuple
// names resolve only against its members, and anything that doesn't fit answers nil.

// === encode =============================================================================

#[test]
fn test_encode_primitives_and_tuples() {
    quiver().evaluate(r#"%data.encode -42"#).expect(r#""-42""#);
    quiver()
        .evaluate(r#"%data.encode 123456789012345678901234567890"#)
        .expect(r#""123456789012345678901234567890""#);
    quiver()
        .evaluate(r#"%data.encode <0a1b>"#)
        .expect(r#""<0a1b>""#);
    quiver().evaluate(r#"%data.encode []"#).expect(r#""[]""#);
    quiver().evaluate(r#"%data.encode Ok"#).expect(r#""Ok""#);
    quiver()
        .evaluate(r#"%data.encode Point[x: 1, y: [2, Blue]]"#)
        .expect(r#""Point[x: 1, y: [2, Blue]]""#);
}

#[test]
fn test_encode_strings() {
    // Quotable text uses the string sugar, with the language's escapes — including
    // `\{`, so encoded text reads back as literal *code* unchanged (no interpolation).
    quiver()
        .evaluate(r#"%data.encode "a\"b\\c\{d\ne""#)
        .expect(r#""\"a\\\"b\\\\c\\{d\\ne\"""#);
    // Bytes no string literal can carry fall back to the ordinary tuple form.
    quiver()
        .evaluate(r#"%data.encode Str[<ff00>]"#)
        .expect(r#""Str[<ff00>]""#);
}

#[test]
fn test_encode_rejects_non_data() {
    // Functions, builtins, processes, refs, and resources have no meaning outside the
    // program: encoding one is a runtime error, not a silent placeholder.
    quiver()
        .evaluate(r#"f = #'int { $ }; %data.encode f"#)
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "cannot encode a function: %data notation carries data only (integers, \
             binaries, and tuples)"
                .to_string(),
        ));
    quiver()
        .evaluate(r#"%ref [] ~> %data.encode ~"#)
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "cannot encode a ref: %data notation carries data only (integers, \
             binaries, and tuples)"
                .to_string(),
        ));
}

#[test]
fn test_encode_drops_annotations() {
    // Annotations are data *about* the value: invisible to the notation.
    quiver()
        .evaluate(r#"P[x: 1] ~> { :note "hi" } ~> %data.encode ~"#)
        .expect(r#""P[x: 1]""#);
}

// === decode =============================================================================

#[test]
fn test_decode_primitives() {
    quiver()
        .evaluate(r#"%data.decode<'int> "  -42  ""#)
        .expect("-42");
    quiver()
        .evaluate(r#"%data.decode<'int> "123456789012345678901234567890""#)
        .expect("123456789012345678901234567890");
    quiver()
        .evaluate(r#"%data.decode<'bin> "<0a1b>""#)
        .expect("<0a1b>");
    quiver()
        .evaluate(r#"%data.decode<Str['bin]> "\"hi\"""#)
        .expect(r#""hi""#);
    // The tuple form of a Str decodes too — the sugar is a synonym, not a format.
    quiver()
        .evaluate(r#"%data.decode<Str['bin]> "Str[<6869>]""#)
        .expect(r#""hi""#);
}

#[test]
fn test_decode_tuples_labels_and_layout() {
    // Labels are optional per field but must match the expected shape when written;
    // a trailing comma and interior whitespace are fine, as in code.
    quiver()
        .evaluate(r#"%data.decode<P[x: 'int, y: 'int]> "P[x: 1, y: 2]""#)
        .expect("P[x: 1, y: 2]");
    quiver()
        .evaluate("%data.decode<P[x: 'int, y: 'int]> \"P[ 1,\n  2, ]\"")
        .expect("P[x: 1, y: 2]");
    quiver()
        .evaluate(r#"%data.decode<P[x: 'int]> "P[y: 1]""#)
        .expect("[]");
}

#[test]
fn test_decode_accepts_grouped_binaries() {
    // The notation is the literal syntax, so a hand-written binary may be grouped — under
    // the same rules: whole-byte groups, separated but not padded. `encode` never emits
    // grouping, so this only ever matters for text a person wrote.
    quiver()
        .evaluate(r#"%data.decode<'bin> "<6a09e667 bb67ae85>""#)
        .expect("<6a09e667bb67ae85>");
    quiver()
        .evaluate(r#"%data.decode<'bin> "<0a1 b2c>""#)
        .expect("[]");
    quiver()
        .evaluate(r#"%data.decode<'bin> "< 0a1b>""#)
        .expect("[]");
    quiver()
        .evaluate(r#"%data.decode<'bin> "<0a1b >""#)
        .expect("[]");
    quiver().evaluate(r#"%data.decode<'bin> "<>""#).expect("<>");
    // A line break both separates groups and lets the brackets sit apart from the digits,
    // so a table written across lines reads back like the source literal it mirrors.
    quiver()
        .evaluate(r#"%data.decode<'bin> "<\n  0a1b 2c3d\n  4e5f 6071\n>""#)
        .expect("<0a1b2c3d4e5f6071>");
    quiver()
        .evaluate(r#"%data.decode<'bin> "<\n>""#)
        .expect("<>");
}

#[test]
fn test_decode_unions_and_recursion() {
    // Ordered choice with its prefix-lookahead rule: a bare named-empty tuple is never
    // followed by a glued `[`.
    quiver()
        .evaluate(r#"%data.decode<('int | 'bin)> "<0a>""#)
        .expect("<0a>");
    quiver()
        .evaluate(r#"'u = Ok | Ok['int]; %data.decode<'u> "Ok[5]""#)
        .expect("Ok[5]");
    quiver()
        .evaluate(r#"'list = Nil | Cons['int, ^]; %data.decode<'list> "Cons[1, Cons[2, Nil]]""#)
        .expect("Cons[1, Cons[2, Nil]]");
    quiver()
        .evaluate(
            r#"'tree = Leaf['int] | Node[^, ^]
               %data.decode<'tree> "Node[Leaf[1], Node[Leaf[2], Leaf[3]]]""#,
        )
        .expect("Node[Leaf[1], Node[Leaf[2], Leaf[3]]]");
}

#[test]
fn test_decode_cycle_through_sibling_boundaries() {
    // Regression: a cycle followed back to the root, then descending a *different*
    // member with its own inner union, must resolve that member's cycles at the
    // target's depth — boundaries pushed by the abandoned sibling branch (here the
    // Object pair list) must not skew them. '%json is the natural witness: an Array
    // inside an Object value reaches Array's list only via the pair's root cycle.
    quiver()
        .evaluate(r#"%data.decode<'%json> "Object[Cons[[\"a\", Array[Cons[1, Nil]]], Nil]]""#)
        .expect(r#"Object[Cons[["a", Array[Cons[1, Nil]]], Nil]]"#);
}

#[test]
fn test_decode_failures_answer_nil() {
    // Unknown name for the expected type, arity mismatch, odd hex, trailing input, a
    // raw newline in a string, and a partial expected type (no layout to construct).
    quiver()
        .evaluate(r#"%data.decode<(A | B)> "C""#)
        .expect("[]");
    quiver()
        .evaluate(r#"%data.decode<P['int]> "P[1, 2]""#)
        .expect("[]");
    quiver()
        .evaluate(r#"%data.decode<'bin> "<0a1>""#)
        .expect("[]");
    quiver()
        .evaluate(r#"%data.decode<'int> "4 2""#)
        .expect("[]");
    quiver()
        .evaluate("%data.decode<Str['bin]> \"\\\"a\nb\\\"\"")
        .expect("[]");
    quiver()
        .evaluate(r#"%data.decode<(x: 'int)> "[x: 1]""#)
        .expect("[]");
    // A nil *member* decodes successfully — indistinguishable from failure by design,
    // exactly like any other nil: keep decoded unions nil-free if it matters.
    quiver()
        .evaluate(r#"%data.decode<('int | [])> "[]""#)
        .expect("[]");
}

#[test]
fn test_decode_static_type_and_required_argument() {
    // The declared type argument is the static result (plus nil).
    quiver()
        .evaluate(r#"%data.decode<'int> "5""#)
        .expect_type("'int | []");
    // An *applied* decode must be instantiated; the requirement follows the member.
    quiver()
        .evaluate(r#""5" ~> %data.decode ~"#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeArgumentsRequired {
            builtin: "data_decode".to_string(),
            declared: 1,
        });
}

#[test]
fn test_round_trip() {
    quiver()
        .evaluate(
            r#"'pt = Point[x: 'int, y: Str['bin], z: (Nil | Cons['int, ^])]
               v = Point[x: -12345678901234567890123, y: "a\"b\\c\{d\ne", z: Cons[1, Cons[2, Nil]]]
               d = %data.encode v ~> %data.decode<'pt> ~
               [v] ~> { | =[&d] => Same | Different }"#,
        )
        .expect("Same");
}

#[test]
fn test_decode_through_reference_and_binding() {
    // The instantiation rides the value: bind it, pass it, call it later.
    quiver()
        .evaluate(r#"d = %data.decode<('int | Quit)>; d "Quit""#)
        .expect("Quit");
}

#[test]
fn test_decoded_values_dispatch_through_sibling_patterns() {
    // Decoded values are `(Ev['w] | [])`-typed and their tuple ids carry the full
    // union field type; sibling patterns on the wrapped field must all stay reachable
    // (regression: the first branch's complement used to poison the later branches'
    // runtime tests — see tests/narrowing.rs for the code-built form).
    quiver()
        .evaluate(
            r#"'wire = Submit | Toggle['int] | Del['int]
               f = #(Ev['wire] | []) {
                 $ ~> {
                   | =Ev[Submit] => S
                   | =Ev[Toggle[id]] => T[id]
                   | =Ev[Del[id]] => D[id]
                   | Missed
                 }
               }
               [
                 %data.decode<Ev['wire]> "Ev[Submit]" ~> f ~,
                 %data.decode<Ev['wire]> "Ev[Toggle[1]]" ~> f ~,
                 %data.decode<Ev['wire]> "Ev[Del[7]]" ~> f ~,
                 %data.decode<Ev['wire]> "garbage" ~> f ~,
               ]"#,
        )
        .expect("[S, T[1], D[7], Missed]");
}
