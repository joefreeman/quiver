mod common;
use common::*;

// The runtime JSON codec: `%json.parse` (`Str['bin] -> '%json | []`) and `%json.stringify`
// (`'%json -> Str['bin]`). These share the `%parse` combinators with the `%json{…}` dialect
// but build `'%json` values directly rather than `%meta` code (see std/json.qv); the dialect
// itself is covered in dialects.rs.
//
// A literal `{` in a Quiver string opens an interpolation hole, so inline JSON objects escape
// it as `\{`; runtime input from files/sockets needs no such escaping.

#[test]
fn test_parse_scalars() {
    quiver().evaluate(r#""null" %json.parse"#).expect("Null");
    quiver().evaluate(r#""true" %json.parse"#).expect("True");
    quiver().evaluate(r#""false" %json.parse"#).expect("False");
    quiver().evaluate(r#""42" %json.parse"#).expect("42");
    quiver().evaluate(r#""-17" %json.parse"#).expect("-17");
    quiver()
        .evaluate(r#""\"hi\"" %json.parse"#)
        .expect(r#""hi""#);
}

#[test]
fn test_parse_array() {
    quiver()
        .evaluate(r#""[1, 2, 3]" %json.parse"#)
        .expect("Array[Cons[1, Cons[2, Cons[3, Nil]]]]");
    // Nested and empty arrays.
    quiver()
        .evaluate(r#""[[1], []]" %json.parse"#)
        .expect("Array[Cons[Array[Cons[1, Nil]], Cons[Array[Nil], Nil]]]");
}

#[test]
fn test_parse_object_preserves_key_order() {
    quiver()
        .evaluate(r#""\{\"a\": 1, \"b\": true}" %json.parse"#)
        .expect(r#"Object[Cons[["a", 1], Cons[["b", True], Nil]]]"#);
    quiver()
        .evaluate(r#""\{}" %json.parse"#)
        .expect("Object[Nil]");
}

#[test]
fn test_parse_tolerates_whitespace_and_trailing_comma() {
    quiver()
        .evaluate(r#""  [1, 2, ]  " %json.parse"#)
        .expect("Array[Cons[1, Cons[2, Nil]]]");
    quiver()
        .evaluate(r#""\{ \"a\": 1, }" %json.parse"#)
        .expect(r#"Object[Cons[["a", 1], Nil]]"#);
}

#[test]
fn test_parse_string_escapes() {
    // `\n` inside a JSON string decodes to a newline byte; the value display re-escapes it.
    quiver()
        .evaluate(r#""[\"a\\nb\"]" %json.parse"#)
        .expect(r#"Array[Cons["a\nb", Nil]]"#);
}

#[test]
fn test_parse_malformed_is_nil() {
    quiver().evaluate(r#""[1, 2" %json.parse"#).expect("[]"); // unterminated array
    quiver().evaluate(r#""tru" %json.parse"#).expect("[]"); // incomplete keyword
    quiver().evaluate(r#""nope" %json.parse"#).expect("[]"); // non-keyword identifier
    quiver().evaluate(r#""[1 2]" %json.parse"#).expect("[]"); // missing separator
}

#[test]
fn test_stringify() {
    quiver()
        .evaluate(r#"%json{ [1, 2, 3] } %json.stringify"#)
        .expect(r#""[1,2,3]""#);
    quiver()
        .evaluate(r#"%json{ { "a": 1, "b": true } } %json.stringify"#)
        .expect(r#""{\"a\":1,\"b\":true}""#);
    // Empty containers.
    quiver()
        .evaluate(r#"%json{ [] } %json.stringify"#)
        .expect(r#""[]""#);
}

#[test]
fn test_stringify_escapes_strings() {
    quiver()
        .evaluate(r#"%json{ "a\nb" } %json.stringify"#)
        .expect(r#""\"a\\nb\"""#);
}

#[test]
fn test_round_trip() {
    // `parse` is nilable, so narrow with `=('%json)v` before feeding `stringify`.
    quiver()
        .evaluate(r#""[1, [2, 3], [], -4]" %json.parse =('%json)v; v %json.stringify"#)
        .expect(r#""[1,[2,3],[],-4]""#);
    quiver()
        .evaluate(r#""\{\"k\": [true, null]}" %json.parse =('%json)v; v %json.stringify"#)
        .expect(r#""{\"k\":[true,null]}""#);
}

#[test]
fn test_parse_deeply_nested_structure() {
    // Object -> array -> object, mixed with a scalar sibling and an empty array, pins the
    // exact '%json tree a symmetric parse/stringify bug could otherwise hide.
    quiver()
        .evaluate(r#""\{\"a\": [\{\"b\": 1}, 2], \"c\": []}" %json.parse"#)
        .expect(
            r#"Object[Cons[["a", Array[Cons[Object[Cons[["b", 1], Nil]], Cons[2, Nil]]]], Cons[["c", Array[Nil]], Nil]]]"#,
        );
}

#[test]
fn test_parse_numbers_are_exact_rationals() {
    // Non-integer JSON numbers parse to exact rationals — no float rounding.
    quiver().evaluate(r#""3.14" %json.parse"#).expect("157/50");
    quiver().evaluate(r#""-0.5" %json.parse"#).expect("-1/2");
    quiver().evaluate(r#""2.5e-3" %json.parse"#).expect("1/400");
    // Whole-valued numbers stay 'int (lowered from Rational[_, 1]): `x.0` and integer
    // exponent results included.
    quiver().evaluate(r#""42" %json.parse"#).expect("42");
    quiver().evaluate(r#""42.0" %json.parse"#).expect("42");
    quiver().evaluate(r#""1.5e3" %json.parse"#).expect("1500");
}

#[test]
fn test_parse_large_exponent_stays_exact() {
    // Bignum: a magnitude that overflows an f64 to `inf` is exact here, and round-trips
    // through `stringify` unchanged.
    quiver()
        .evaluate(r#""1e30" %json.parse"#)
        .expect("1000000000000000000000000000000");
    quiver()
        .evaluate(
            r#"doc = "1000000000000000000000000000000"; doc %json.parse =('%json)v; v %json.stringify =&doc"#,
        )
        .expect("Ok");
}

#[test]
fn test_stringify_rationals() {
    // Terminating rational -> its exact decimal.
    quiver()
        .evaluate(r#"[314, 100] %num.div =('%json)v; v %json.stringify"#)
        .expect(r#""3.14""#);
    // Non-terminating -> rounded (half away from zero) to 12 fractional digits.
    quiver()
        .evaluate(r#"[1, 3] %num.div =('%json)v; v %json.stringify"#)
        .expect(r#""0.333333333333""#);
    quiver()
        .evaluate(r#"[2, 3] %num.div =('%json)v; v %json.stringify"#)
        .expect(r#""0.666666666667""#);
    quiver()
        .evaluate(r#"[-2, 7] %num.div =('%json)v; v %json.stringify"#)
        .expect(r#""-0.285714285714""#);
    // Below 12 fractional digits of significance rounds to 0 (and never "-0").
    quiver()
        .evaluate(r#"[1, 10000000000000] %num.div =('%json)v; v %json.stringify"#)
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
                "{input} %json.parse =('%json)v; v %json.stringify"
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
            r#"doc = "\{\"user\":\{\"name\":\"Ada \\\"L\\\"\",\"age\":36,\"active\":true,\"roles\":[\"admin\",\"dev\"],\"manager\":null},\"scores\":[10,-5,0],\"empty_obj\":\{},\"empty_arr\":[],\"path\":\"a\\\\b\\nc\"}"; doc %json.parse =('%json)v; v %json.stringify =&doc"#,
        )
        .expect("Ok");
}
