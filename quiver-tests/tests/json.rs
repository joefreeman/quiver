mod common;
use common::*;

// The runtime JSON codec — `%json.parse` (`Str['bin] -> '%json | []`) and `%json.stringify`
// (`'%json -> Str['bin]`) — and the document-model query/update API (`get`, `set`, `update`,
// `delete`, `merge`, and the `%dict`/`%list` bridges), all over `'%json`. The codec shares
// the `%parse` combinators with the `%json{…}` dialect but builds `'%json` values directly
// rather than `%meta` code (see std/json.qv); the dialect itself is covered in dialects.rs.
//
// A literal `{` in a Quiver string opens an interpolation hole, so inline JSON objects escape
// it as `\{`; runtime input from files/sockets needs no such escaping.

#[test]
fn test_parse_scalars() {
    quiver().evaluate(r#""null" ~> %json.parse"#).expect("Null");
    quiver().evaluate(r#""true" ~> %json.parse"#).expect("True");
    quiver()
        .evaluate(r#""false" ~> %json.parse"#)
        .expect("False");
    quiver().evaluate(r#""42" ~> %json.parse"#).expect("42");
    quiver().evaluate(r#""-17" ~> %json.parse"#).expect("-17");
    quiver()
        .evaluate(r#""\"hi\"" ~> %json.parse"#)
        .expect(r#""hi""#);
}

#[test]
fn test_parse_array() {
    quiver()
        .evaluate(r#""[1, 2, 3]" ~> %json.parse"#)
        .expect("Array[Cons[1, Cons[2, Cons[3, Nil]]]]");
    // Nested and empty arrays.
    quiver()
        .evaluate(r#""[[1], []]" ~> %json.parse"#)
        .expect("Array[Cons[Array[Cons[1, Nil]], Cons[Array[Nil], Nil]]]");
}

#[test]
fn test_parse_object_preserves_key_order() {
    quiver()
        .evaluate(r#""\{\"a\": 1, \"b\": true}" ~> %json.parse"#)
        .expect(r#"Object[Cons[["a", 1], Cons[["b", True], Nil]]]"#);
    quiver()
        .evaluate(r#""\{}" ~> %json.parse"#)
        .expect("Object[Nil]");
}

#[test]
fn test_parse_tolerates_whitespace_and_trailing_comma() {
    quiver()
        .evaluate(r#""  [1, 2, ]  " ~> %json.parse"#)
        .expect("Array[Cons[1, Cons[2, Nil]]]");
    quiver()
        .evaluate(r#""\{ \"a\": 1, }" ~> %json.parse"#)
        .expect(r#"Object[Cons[["a", 1], Nil]]"#);
}

#[test]
fn test_parse_string_escapes() {
    // `\n` inside a JSON string decodes to a newline byte; the value display re-escapes it.
    quiver()
        .evaluate(r#""[\"a\\nb\"]" ~> %json.parse"#)
        .expect(r#"Array[Cons["a\nb", Nil]]"#);
}

#[test]
fn test_parse_malformed_is_nil() {
    quiver().evaluate(r#""[1, 2" ~> %json.parse"#).expect("[]"); // unterminated array
    quiver().evaluate(r#""tru" ~> %json.parse"#).expect("[]"); // incomplete keyword
    quiver().evaluate(r#""nope" ~> %json.parse"#).expect("[]"); // non-keyword identifier
    quiver().evaluate(r#""[1 2]" ~> %json.parse"#).expect("[]"); // missing separator
}

#[test]
fn test_stringify() {
    quiver()
        .evaluate(r#"%json{ [1, 2, 3] } ~> %json.stringify"#)
        .expect(r#""[1,2,3]""#);
    quiver()
        .evaluate(r#"%json{ { "a": 1, "b": true } } ~> %json.stringify"#)
        .expect(r#""{\"a\":1,\"b\":true}""#);
    // Empty containers.
    quiver()
        .evaluate(r#"%json{ [] } ~> %json.stringify"#)
        .expect(r#""[]""#);
}

#[test]
fn test_stringify_escapes_strings() {
    quiver()
        .evaluate(r#"%json{ "a\nb" } ~> %json.stringify"#)
        .expect(r#""\"a\\nb\"""#);
}

#[test]
fn test_round_trip() {
    // `parse` is nilable, so narrow with `=('%json)v` before feeding `stringify`.
    quiver()
        .evaluate(r#""[1, [2, 3], [], -4]" ~> %json.parse ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""[1,[2,3],[],-4]""#);
    quiver()
        .evaluate(r#""\{\"k\": [true, null]}" ~> %json.parse ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"k\":[true,null]}""#);
}

#[test]
fn test_parse_deeply_nested_structure() {
    // Object -> array -> object, mixed with a scalar sibling and an empty array, pins the
    // exact '%json tree a symmetric parse/stringify bug could otherwise hide.
    quiver()
        .evaluate(r#""\{\"a\": [\{\"b\": 1}, 2], \"c\": []}" ~> %json.parse"#)
        .expect(
            r#"Object[Cons[["a", Array[Cons[Object[Cons[["b", 1], Nil]], Cons[2, Nil]]]], Cons[["c", Array[Nil]], Nil]]]"#,
        );
}

#[test]
fn test_parse_numbers_are_exact_rationals() {
    // Non-integer JSON numbers parse to exact rationals — no float rounding.
    quiver()
        .evaluate(r#""3.14" ~> %json.parse"#)
        .expect("157/50");
    quiver().evaluate(r#""-0.5" ~> %json.parse"#).expect("-1/2");
    quiver()
        .evaluate(r#""2.5e-3" ~> %json.parse"#)
        .expect("1/400");
    // Whole-valued numbers stay 'int (lowered from Rational[_, 1]): `x.0` and integer
    // exponent results included.
    quiver().evaluate(r#""42" ~> %json.parse"#).expect("42");
    quiver().evaluate(r#""42.0" ~> %json.parse"#).expect("42");
    quiver()
        .evaluate(r#""1.5e3" ~> %json.parse"#)
        .expect("1500");
}

#[test]
fn test_parse_large_exponent_stays_exact() {
    // Bignum: a magnitude that overflows an f64 to `inf` is exact here, and round-trips
    // through `stringify` unchanged.
    quiver()
        .evaluate(r#""1e30" ~> %json.parse"#)
        .expect("1000000000000000000000000000000");
    quiver()
        .evaluate(
            r#"doc = "1000000000000000000000000000000"; doc ~> %json.parse ~> =('%json)v; v ~> %json.stringify ~> =&doc"#,
        )
        .expect("Ok");
}

#[test]
fn test_stringify_rationals() {
    // Terminating rational -> its exact decimal.
    quiver()
        .evaluate(r#"[314, 100] ~> %num.div ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""3.14""#);
    // Non-terminating -> rounded (half away from zero) to 12 fractional digits.
    quiver()
        .evaluate(r#"[1, 3] ~> %num.div ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""0.333333333333""#);
    quiver()
        .evaluate(r#"[2, 3] ~> %num.div ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""0.666666666667""#);
    quiver()
        .evaluate(r#"[-2, 7] ~> %num.div ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""-0.285714285714""#);
    // Below 12 fractional digits of significance rounds to 0 (and never "-0").
    quiver()
        .evaluate(r#"[1, 10000000000000] ~> %num.div ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""0""#);
}

#[test]
fn test_decimal_round_trips() {
    // Decimals originating from JSON round-trip exactly — including a value f64 cannot
    // represent (2.675), and exponent forms that normalize to plain decimals.
    let cases = [
        (r#""3.14""#, r#""3.14""#),
        (r#""-0.5""#, r#""-0.5""#),
        (r#""2.675""#, r#""2.675""#),
        (r#""2.5e-3""#, r#""0.0025""#),
        (r#""0.1""#, r#""0.1""#),
        (r#""1e-12""#, r#""0.000000000001""#),
    ];
    for (input, output) in cases {
        quiver()
            .evaluate(&format!(
                "{input} ~> %json.parse ~> =('%json)v; v ~> %json.stringify"
            ))
            .expect(output);
    }
}

#[test]
fn test_round_trip_complex_document() {
    // A realistic, deeply-nested document in canonical compact form: nested objects/arrays,
    // every scalar type, empty containers at depth, and strings needing quote/backslash/
    // newline escaping. `stringify` emits canonical compact JSON, so re-stringifying a parse
    // of an already-canonical string must reproduce it byte-for-byte — asserted with `=&doc`,
    // which exercises parse and stringify together across the whole nesting in one shot.
    quiver()
        .evaluate(
            r#"doc = "\{\"user\":\{\"name\":\"Ada \\\"L\\\"\",\"age\":36,\"active\":true,\"roles\":[\"admin\",\"dev\"],\"manager\":null},\"scores\":[10,-5,0],\"empty_obj\":\{},\"empty_arr\":[],\"path\":\"a\\\\b\\nc\"}"; doc ~> %json.parse ~> =('%json)v; v ~> %json.stringify ~> =&doc"#,
        )
        .expect("Ok");
}

#[test]
fn test_get_object_key_and_array_index() {
    quiver()
        .evaluate(r#"%json{ { "a": 1, "b": [10, 20] } } ~> %json.get [~, "a"]"#)
        .expect("1");
    // An index applied to the array a key answered — one polymorphic `get`, chained.
    quiver()
        .evaluate(r#"%json{ { "b": [10, 20] } } ~> %json.get [~, "b"] ~> %json.get [~, 1]"#)
        .expect("20");
}

#[test]
fn test_get_misses_are_nil() {
    // Absent key; key applied to an array; index applied to an object; index out of range;
    // negative index.
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.get [~, "z"]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"%json{ [1, 2] } ~> %json.get [~, "a"]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.get [~, 0]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"%json{ [1, 2] } ~> %json.get [~, 5]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"%json{ [1, 2] } ~> %json.get [~, -1]"#)
        .expect("[]");
}

#[test]
fn test_get_accepts_nil_so_lookups_chain() {
    // The first miss answers nil; the later `get` accepts it and answers nil again, so a
    // deep lookup is one pipeline with no narrowing between steps.
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.get [~, "z"] ~> %json.get [~, "x"]"#)
        .expect("[]");
}

#[test]
fn test_get_path() {
    quiver()
        .evaluate(r#"%json{ { "users": [{ "name": "ada" }] } } ~> %json.get [~, %list{ "users", 0, "name" }]"#)
        .expect(r#""ada""#);
    // The empty path names the value itself.
    quiver()
        .evaluate(r#"doc = %json{ { "a": 1 } }; %json.get [doc, %list{}] ~> =&doc"#)
        .expect("Ok");
    // A path over a missing intermediate is nil.
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.get [~, %list{ "z", "x" }]"#)
        .expect("[]");
}

#[test]
fn test_get_duplicate_keys_last_wins() {
    // Only `parse` can introduce duplicates; `get` answers the last binding, matching what
    // mainstream parsers keep.
    quiver()
        .evaluate(r#""\{\"a\": 1, \"a\": 2}" ~> %json.parse ~> %json.get [~, "a"]"#)
        .expect("2");
}

#[test]
fn test_set_object_key() {
    // Present: replaced in place, order kept. Absent: appended at the end.
    quiver()
        .evaluate(r#"%json{ { "a": 1, "b": 2 } } ~> %json.set [~, "a", 9] ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"a\":9,\"b\":2}""#);
    quiver()
        .evaluate(
            r#"%json{ { "a": 1 } } ~> %json.set [~, "c", 3] ~> =('%json)v; v ~> %json.stringify"#,
        )
        .expect(r#""{\"a\":1,\"c\":3}""#);
    // Duplicates collapse: the first occurrence is rewritten, the rest are dropped.
    quiver()
        .evaluate(r#""\{\"a\": 1, \"b\": 2, \"a\": 3}" ~> %json.parse ~> %json.set [~, "a", 9] ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"a\":9,\"b\":2}""#);
}

#[test]
fn test_set_array_index_and_path() {
    quiver()
        .evaluate(
            r#"%json{ [1, 2, 3] } ~> %json.set [~, 1, 9] ~> =('%json)v; v ~> %json.stringify"#,
        )
        .expect(r#""[1,9,3]""#);
    quiver()
        .evaluate(r#"%json{ { "tags": [1, 2] } } ~> %json.set [~, %list{ "tags", 0 }, 9] ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"tags\":[9,2]}""#);
    // The empty path replaces the whole value.
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.set [~, %list{}, 5]"#)
        .expect("5");
}

#[test]
fn test_set_misses_are_nil() {
    // Out-of-range index; a path through a missing container (no vivification); a nil
    // replacement value propagates instead of being embedded.
    quiver()
        .evaluate(r#"%json{ [1, 2] } ~> %json.set [~, 5, 9]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.set [~, %list{ "z", "x" }, 9]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.set [~, "a", %json.get [%json{ {} }, "z"]]"#)
        .expect("[]");
}

#[test]
fn test_update() {
    quiver()
        .evaluate(r#"%json{ { "n": 2 } } ~> %json.update [~, "n", #{ =('int)i; %num.mul [i, 10] }] ~> %json.get [~, "n"]"#)
        .expect("20");
    // A path key: the leaf is transformed and every level rebuilt around it.
    quiver()
        .evaluate(r#"%json{ { "a": { "n": [5, 7] } } } ~> %json.update [~, %list{ "a", "n", 1 }, #{ =('int)i; %num.mul [i, 10] }] ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"a\":{\"n\":[5,70]}}""#);
    // An absent target is nil — for a single key and through a path — and so is `f`
    // answering nil.
    quiver()
        .evaluate(r#"%json{ { "n": 2 } } ~> %json.update [~, "z", #{ ~ }]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"%json{ { "n": 2 } } ~> %json.update [~, %list{ "z", "x" }, #{ ~ }]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"%json{ { "n": 2 } } ~> %json.update [~, "n", #{ [] }]"#)
        .expect("[]");
}

#[test]
fn test_delete() {
    quiver()
        .evaluate(r#"%json{ { "a": 1, "b": 2 } } ~> %json.delete [~, "a"] ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"b\":2}""#);
    quiver()
        .evaluate(
            r#"%json{ [1, 2, 3] } ~> %json.delete [~, 1] ~> =('%json)v; v ~> %json.stringify"#,
        )
        .expect(r#""[1,3]""#);
    quiver()
        .evaluate(r#"%json{ { "a": [1, 2] } } ~> %json.delete [~, %list{ "a", 0 }] ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"a\":[2]}""#);
    // Every duplicate occurrence is removed.
    quiver()
        .evaluate(r#""\{\"a\": 1, \"b\": 2, \"a\": 3}" ~> %json.parse ~> %json.delete [~, "a"] ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"b\":2}""#);
}

#[test]
fn test_delete_is_idempotent_but_kind_strict() {
    // Absent key or index, or a missing intermediate: unchanged. Wrong kind: nil.
    quiver()
        .evaluate(r#"doc = %json{ { "a": 1 } }; %json.delete [doc, "z"] ~> =&doc"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"doc = %json{ [1] }; %json.delete [doc, 5] ~> =&doc"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"doc = %json{ { "a": 1 } }; %json.delete [doc, %list{ "z", "x" }] ~> =&doc"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"%json{ [1] } ~> %json.delete [~, "a"]"#)
        .expect("[]");
    // Deleting the whole value (the empty path) leaves nothing.
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.delete [~, %list{}]"#)
        .expect("[]");
}

#[test]
fn test_merge() {
    // First object's order with the second's values winning per key, then the second's
    // remaining pairs.
    quiver()
        .evaluate(r#"[%json{ { "a": 1, "b": 2 } }, %json{ { "b": 20, "c": 30 } }] ~> %json.merge ~> =('%json)v; v ~> %json.stringify"#)
        .expect(r#""{\"a\":1,\"b\":20,\"c\":30}""#);
    // Anything but two objects is nil.
    quiver()
        .evaluate(r#"[%json{ [1] }, %json{ {} }] ~> %json.merge"#)
        .expect("[]");
}

#[test]
fn test_dict_and_list_bridges() {
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.to_dict ~> =('%dict<'%str, '%json>)d; %dict.get [d, "a"]"#)
        .expect("1");
    // Later duplicates win, matching `get`.
    quiver()
        .evaluate(r#""\{\"a\": 1, \"a\": 2}" ~> %json.parse ~> %json.to_dict ~> =('%dict<'%str, '%json>)d; %dict.get [d, "a"]"#)
        .expect("2");
    quiver()
        .evaluate(r#"%json{ [1] } ~> %json.to_dict"#)
        .expect("[]");
    // Constructors: an Object from a pair list or a %dict, an Array from a list.
    quiver()
        .evaluate(r#"%json.object %list{ ["x", 1] } ~> %json.stringify"#)
        .expect(r#""{\"x\":1}""#);
    quiver()
        .evaluate(r#"%json.object %dict{ "y" => 2 } ~> %json.get [~, "y"]"#)
        .expect("2");
    quiver()
        .evaluate(r#"%json.array %list{ 1, 2 } ~> %json.stringify"#)
        .expect(r#""[1,2]""#);
    // Round trip: Object -> %dict -> Object preserves the entries (not the order).
    quiver()
        .evaluate(r#"%json{ { "m": { "x": 7 } } } ~> %json.to_dict ~> =('%dict<'%str, '%json>)d; %json.object d ~> %json.get [~, %list{ "m", "x" }]"#)
        .expect("7");
}

// === Typed boundary: decode<'t> / encode<'t> ============================================

#[test]
fn test_decode_scalars_and_mismatches() {
    quiver()
        .evaluate(r#"%json{ 42 } ~> %json.decode<'int>"#)
        .expect("42");
    quiver()
        .evaluate(r#"%json{ "hi" } ~> %json.decode<'%str>"#)
        .expect(r#""hi""#);
    quiver()
        .evaluate(r#"%json{ true } ~> %json.decode<(True | False)>"#)
        .expect("True");
    // Any mismatch is nil, like a failed match — including 3.5 into 'int.
    quiver()
        .evaluate(r#"%json{ "hi" } ~> %json.decode<'int>"#)
        .expect("[]");
    quiver()
        .evaluate(r#""3.5" ~> %json.parse ~> %json.decode<'int>"#)
        .expect("[]");
    quiver()
        .evaluate(r#""3.5" ~> %json.parse ~> %json.decode<'%num.coeff>"#)
        .expect("7/2");
}

#[test]
fn test_decode_object_to_labelled_tuple() {
    // Keys match by label, order-free; extra keys are ignored; the tuple's name is
    // Quiver-side only.
    quiver()
        .evaluate(
            r#""\{\"age\": 36, \"name\": \"ada\", \"x\": true}" ~> %json.parse ~> %json.decode<User[name: '%str, age: 'int]>"#,
        )
        .expect(r#"User[name: "ada", age: 36]"#);
    // A missing non-optional key is a mismatch.
    quiver()
        .evaluate(
            r#""\{\"name\": \"ada\"}" ~> %json.parse ~> %json.decode<[name: '%str, age: 'int]>"#,
        )
        .expect("[]");
}

#[test]
fn test_decode_optional_fields_collapse_absent_and_null() {
    quiver()
        .evaluate(r#""\{\"a\": 1}" ~> %json.parse ~> %json.decode<[a: 'int, b: '%str | []]>"#)
        .expect(r#"[a: 1, b: []]"#);
    quiver()
        .evaluate(r#""\{\"a\": 1, \"b\": null}" ~> %json.parse ~> %json.decode<[a: 'int, b: '%str | []]>"#)
        .expect(r#"[a: 1, b: []]"#);
}

#[test]
fn test_decode_arrays_and_nesting() {
    quiver()
        .evaluate(r#"%json{ [1, 2, 3] } ~> %json.decode<'%list<'int>>"#)
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
    quiver()
        .evaluate(
            r#"%json{ { "users": [{ "name": "ada" }] } } ~> %json.decode<[users: '%list<[name: '%str]>]>"#,
        )
        .expect(r#"[users: Cons[[name: "ada"], Nil]]"#);
    // An array element failing to decode fails the whole array.
    quiver()
        .evaluate(r#"%json{ [1, "x"] } ~> %json.decode<'%list<'int>>"#)
        .expect("[]");
}

#[test]
fn test_decode_unions_by_ordered_choice() {
    quiver()
        .evaluate(r#"%json{ "x" } ~> %json.decode<('int | '%str)>"#)
        .expect(r#""x""#);
    // Objects discriminate structurally between union members.
    quiver()
        .evaluate(
            r#""\{\"width\": 2, \"height\": 3}" ~> %json.parse ~> %json.decode<(Circle[radius: 'int] | Rect[width: 'int, height: 'int])>"#,
        )
        .expect("Rect[width: 2, height: 3]");
    quiver()
        .evaluate(
            r#""\{\"radius\": 5}" ~> %json.parse ~> %json.decode<(Circle[radius: 'int] | Rect[width: 'int, height: 'int])>"#,
        )
        .expect("Circle[radius: 5]");
}

#[test]
fn test_decode_recursive_type() {
    quiver()
        .evaluate(
            r#"'tree = Leaf[value: 'int] | Node[left: ^, right: ^]; "\{\"left\": \{\"value\": 1}, \"right\": \{\"value\": 2}}" ~> %json.parse ~> %json.decode<'tree>"#,
        )
        .expect("Node[left: Leaf[value: 1], right: Leaf[value: 2]]");
}

#[test]
fn test_decode_json_typed_part_passes_subtree_through() {
    // A '%json-typed field captures the raw subtree verbatim (null included — the
    // optional-field collapse only applies where a nil is admitted instead).
    quiver()
        .evaluate(r#"doc = %json{ { "a": [1, null] } }; %json.decode<'%json> doc ~> =&doc"#)
        .expect("Ok");
    quiver()
        .evaluate(
            r#"%json{ { "meta": { "x": [true] } } } ~> %json.decode<[meta: '%json]> ~> =[meta: m]; m ~> %json.stringify"#,
        )
        .expect(r#""{\"x\":[true]}""#);
}

#[test]
fn test_decode_accepts_nil_so_queries_chain() {
    quiver()
        .evaluate(r#"%json{ { "a": 1 } } ~> %json.get [~, "zzz"] ~> %json.decode<'int>"#)
        .expect("[]");
}

#[test]
fn test_encode_objects_lists_and_scalars() {
    quiver()
        .evaluate(
            r#"%json.encode<[name: '%str, age: 'int]> [name: "ada", age: 36] ~> %json.stringify"#,
        )
        .expect(r#""{\"name\":\"ada\",\"age\":36}""#);
    // The tuple's name is Quiver-side only; a nil optional field is omitted.
    quiver()
        .evaluate(r#"%json.encode<Point[x: 'int, y: 'int]> Point[x: 1, y: 2] ~> %json.stringify"#)
        .expect(r#""{\"x\":1,\"y\":2}""#);
    quiver()
        .evaluate(r#"%json.encode<[a: 'int, b: '%str | []]> [a: 1, b: []] ~> %json.stringify"#)
        .expect(r#""{\"a\":1}""#);
    quiver()
        .evaluate(r#"%json.encode<'%list<'int>> %list{ 1, 2, 3 } ~> %json.stringify"#)
        .expect(r#""[1,2,3]""#);
    // A nil in a nil-admitting element position becomes `null`.
    quiver()
        .evaluate(r#"%json.encode<'%list<'int | []>> %list{ 1, [] } ~> %json.stringify"#)
        .expect(r#""[1,null]""#);
    quiver()
        .evaluate(r#"%json.encode<(True | False)> False ~> %json.stringify"#)
        .expect(r#""false""#);
    // Exact rationals pass through as '%num.coeff; formatting stays stringify's.
    quiver()
        .evaluate(r#"%num.div [1, 2] ~> =('%num.coeff)h; h ~> %json.encode<'%num.coeff> ~> %json.stringify"#)
        .expect(r#""0.5""#);
}

#[test]
fn test_encode_json_typed_part_and_unencodable_values() {
    // A '%json-typed part passes through whole (normalized across tuple-id families).
    quiver()
        .evaluate(r#"doc = %json{ { "a": [1, true, null] } }; %json.encode<'%json> doc ~> =&doc"#)
        .expect("Ok");
    // A value with no JSON form is a runtime error, like %data.encode's.
    quiver()
        .evaluate(r#"%json.encode<#'int -> 'int> #'int { $ }"#)
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "cannot encode as JSON: the value does not fit the stated type's mapping \
             (functions, processes, refs, resources, binaries, dicts and unlabelled \
             tuples have no JSON form)"
                .to_string(),
        ));
}

#[test]
fn test_typed_round_trips() {
    // encode<'t> ~> decode<'t> is identity, directly and through text.
    quiver()
        .evaluate(
            r#"u = [name: "ada", age: 36, email: []]; %json.encode<[name: '%str, age: 'int, email: '%str | []]> u ~> %json.decode<[name: '%str, age: 'int, email: '%str | []]> ~> =&u"#,
        )
        .expect("Ok");
    quiver()
        .evaluate(
            r#"'tree = Leaf[value: 'int] | Node[left: ^, right: ^]; t = Node[left: Leaf[value: 1], right: Node[left: Leaf[value: 2], right: Leaf[value: 3]]]; %json.encode<'tree> t ~> %json.stringify ~> %json.parse ~> %json.decode<'tree> ~> =&t"#,
        )
        .expect("Ok");
}
