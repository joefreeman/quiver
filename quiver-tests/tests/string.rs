mod common;
use common::*;

#[test]
fn test_interpolation() {
    // A single hole, substituted from a bound variable.
    quiver()
        .evaluate(r#"name = "world", "hello {name}""#)
        .expect("\"hello world\"");
    // Leading, middle, and trailing text around multiple holes.
    quiver()
        .evaluate(r#"a = "X", b = "Y", "[{a}-{b}]""#)
        .expect("\"[X-Y]\"");
    // Adjacent holes with no text between them.
    quiver()
        .evaluate(r#"a = "X", b = "Y", "{a}{b}""#)
        .expect("\"XY\"");
    // A hole containing an arbitrary expression that evaluates to a `Str`.
    quiver()
        .evaluate(r#""got: {["a", "b"] %str.concat}""#)
        .expect("\"got: ab\"");
    // A `\{` escape is a literal brace, not a hole.
    quiver().evaluate(r#""a \{ b""#).expect("\"a { b\"");
    // A string with no holes is an ordinary literal.
    quiver().evaluate(r#""plain""#).expect("\"plain\"");
}

#[test]
fn test_interpolation_hole_receives_chained_value() {
    // Like a tuple field, a hole receives the flowing value, so `~` is the chained value.
    quiver()
        .evaluate(r#""world" "hello, {~}""#)
        .expect("\"hello, world\"");
    // The value is copied into each hole, so it can be used more than once.
    quiver().evaluate(r#""x" "{~}-{~}""#).expect("\"x-x\"");
    // `~` can drive an expression, and a field of the flowing value can be reached.
    quiver()
        .evaluate(r#"Person[name: "Bo"] "hi {~.name}""#)
        .expect("\"hi Bo\"");
    // A multi-line hole sees the chained value too.
    quiver()
        .evaluate("\"ada\" \"\"\"\n  hi {~}\n  \"\"\"")
        .expect("\"hi ada\"");
}

#[test]
fn test_multiline_interpolation() {
    // Holes are substituted, the line structure is preserved, and `\{` is a literal brace.
    quiver()
        .evaluate("name = \"ada\", \"\"\"\n  hi {name}\n  bye\n  \"\"\"")
        .expect("\"hi ada\\nbye\"");
    // A multi-line interpolated string equals the single-line string with the same value.
    quiver()
        .evaluate("x = \"v\", [\"\"\"\n  a {x}\n  \"\"\", \"a v\"] ==")
        .expect("\"a v\"");
    // A literal brace via `\{`, and a hole containing an expression.
    quiver()
        .evaluate("\"\"\"\n  \\{ {[\"p\", \"q\"] %str.concat}\n  \"\"\"")
        .expect("\"{ pq\"");
}

#[test]
fn test_string_pattern_matches() {
    // A string-literal pattern matches the equal string and fails on a different one.
    quiver()
        .evaluate(r#"role = "admin", role ="admin""#)
        .expect("Ok");
    quiver()
        .evaluate(r#"role = "guest", role ="admin""#)
        .expect("[]");
}

#[test]
fn test_single_and_multi_line_values_are_equal() {
    // The two delimiter styles are only a surface form: they produce identical values.
    quiver()
        .evaluate("[\"a\\nb\", \"\"\"\na\nb\n\"\"\"] ==")
        .expect("\"a\\nb\"");
}

#[test]
fn test_interpolation_requires_str_hole() {
    // A hole that is not a `Str` (here an `'int`) is a compile-time type error.
    quiver()
        .evaluate(r#"n = 5, "count {n}""#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "Str".to_string(),
            found: "'int".to_string(),
        });
}

#[test]
fn test_bytes() {
    quiver()
        .evaluate(r#""hello" %str.bytes"#)
        .expect("0x68656c6c6f");
    quiver().evaluate(r#""" %str.bytes"#).expect("0x");
    quiver().evaluate(r#""🚀" %str.bytes"#).expect("0xf09f9a80");
}

#[test]
fn test_empty() {
    quiver().evaluate(r#""" %str.empty?"#).expect("Ok");
    quiver().evaluate(r#""hello" %str.empty?"#).expect("[]");
    quiver().evaluate(r#""x" %str.empty?"#).expect("[]");
}

#[test]
fn test_concat() {
    quiver()
        .evaluate(r#"["hello", " "] %str.concat"#)
        .expect("\"hello \"");
    quiver()
        .evaluate(r#"["hello", " world"] %str.concat"#)
        .expect("\"hello world\"");
    quiver()
        .evaluate(r#"["", "hello"] %str.concat"#)
        .expect("\"hello\"");
    quiver()
        .evaluate(r#"["hello", ""] %str.concat"#)
        .expect("\"hello\"");
    quiver()
        .evaluate(r#"["🚀", "!"] %str.concat"#)
        .expect("\"🚀!\"");
}

#[test]
fn test_starts_with() {
    quiver()
        .evaluate(r#"["hello world", "hello"] %str.starts_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "world"] %str.starts_with?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", "hello"] %str.starts_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello", ""] %str.starts_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hi", "hello"] %str.starts_with?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["🚀 rocket", "🚀"] %str.starts_with?"#)
        .expect("Ok");
}

#[test]
fn test_ends_with() {
    quiver()
        .evaluate(r#"["hello world", "world"] %str.ends_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "hello"] %str.ends_with?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", "hello"] %str.ends_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello", ""] %str.ends_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hi", "hello"] %str.ends_with?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["rocket 🚀", "🚀"] %str.ends_with?"#)
        .expect("Ok");
}

#[test]
fn test_contains() {
    quiver()
        .evaluate(r#"["hello world", "world"] %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "hello"] %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "o w"] %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "xyz"] %str.contains?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", ""] %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hi", "hello"] %str.contains?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", "hello"] %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["🚀 rocket 🌙", "rocket"] %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["🚀 rocket 🌙", "🌙"] %str.contains?"#)
        .expect("Ok");
}

#[test]
fn test_split() {
    quiver()
        .evaluate(r#"["hello,world,test", ","] %str.split %list.collect"#)
        .expect("Cons[\"hello\", Cons[\"world\", Cons[\"test\", Nil]]]");
    quiver()
        .evaluate(r#"["hello", ","] %str.split %list.collect"#)
        .expect("Cons[\"hello\", Nil]");
    quiver()
        .evaluate(r#"["a::b::c", "::"] %str.split %list.collect"#)
        .expect("Cons[\"a\", Cons[\"b\", Cons[\"c\", Nil]]]");
    quiver()
        .evaluate(r#"["", ","] %str.split %list.collect"#)
        .expect("Cons[\"\", Nil]");
    quiver()
        .evaluate(r#"[",hello,", ","] %str.split %list.collect"#)
        .expect("Cons[\"\", Cons[\"hello\", Cons[\"\", Nil]]]");
}

#[test]
fn test_split_returns_iterator() {
    // The result is a lazy iterator: bound to a variable it is a value (wrapped in
    // Iter[...]), so it flows through a chain without being called - no `&` needed.
    quiver()
        .evaluate(r#"parts = ["1,2,3", ","] %str.split, parts [~, " + "] %str.join"#)
        .expect("\"1 + 2 + 3\"");
    // Laziness: the first field can be taken without materialising the rest.
    quiver()
        .evaluate(r#"["a,b,c", ","] %str.split [~, 0] %iter.nth"#)
        .expect("\"a\"");
}

#[test]
fn test_length() {
    // ASCII strings
    quiver().evaluate(r#""hello" %str.length"#).expect("5");
    quiver().evaluate(r#""" %str.length"#).expect("0");
    quiver().evaluate(r#""a" %str.length"#).expect("1");

    // Multi-byte UTF-8 characters
    quiver().evaluate(r#""🚀" %str.length"#).expect("1");
    quiver().evaluate(r#""🚀🌙" %str.length"#).expect("2");
    quiver()
        .evaluate(r#""hello 🚀 world 🌙" %str.length"#)
        .expect("15");

    // Mixed ASCII and multi-byte
    quiver().evaluate(r#""café" %str.length"#).expect("4");
    quiver().evaluate(r#""日本語" %str.length"#).expect("3");
}

#[test]
fn test_slice() {
    // ASCII strings
    quiver()
        .evaluate(r#"["hello", 0, 5] %str.slice"#)
        .expect("\"hello\"");
    quiver()
        .evaluate(r#"["hello", 1, 4] %str.slice"#)
        .expect("\"ell\"");
    quiver()
        .evaluate(r#"["hello", 0, 1] %str.slice"#)
        .expect("\"h\"");
    quiver()
        .evaluate(r#"["hello", 4, 5] %str.slice"#)
        .expect("\"o\"");
    quiver()
        .evaluate(r#"["hello", 2, 2] %str.slice"#)
        .expect("\"\"");

    // Multi-byte UTF-8 characters
    quiver()
        .evaluate(r#"["🚀🌙⭐", 0, 1] %str.slice"#)
        .expect("\"🚀\"");
    quiver()
        .evaluate(r#"["🚀🌙⭐", 1, 2] %str.slice"#)
        .expect("\"🌙\"");
    quiver()
        .evaluate(r#"["🚀🌙⭐", 0, 3] %str.slice"#)
        .expect("\"🚀🌙⭐\"");

    // Mixed ASCII and multi-byte
    quiver()
        .evaluate(r#"["hello 🚀 world", 6, 7] %str.slice"#)
        .expect("\"🚀\"");
    quiver()
        .evaluate(r#"["hello 🚀 world", 0, 6] %str.slice"#)
        .expect("\"hello \"");
    quiver()
        .evaluate(r#"["hello 🚀 world", 8, 13] %str.slice"#)
        .expect("\"world\"");
}

#[test]
fn test_index_of() {
    quiver()
        .evaluate(r#"["hello world", "world"] %str.index_of"#)
        .expect("6");
    quiver()
        .evaluate(r#"["hello world", "hello"] %str.index_of"#)
        .expect("0");
    quiver()
        .evaluate(r#"["hello world", "o"] %str.index_of"#)
        .expect("4");
    quiver()
        .evaluate(r#"["hello world", "xyz"] %str.index_of"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", ""] %str.index_of"#)
        .expect("0");
    quiver()
        .evaluate(r#"["🚀 rocket 🌙", "rocket"] %str.index_of"#)
        .expect("2");
    quiver()
        .evaluate(r#"["🚀 rocket 🌙", "🌙"] %str.index_of"#)
        .expect("9");
}

#[test]
fn test_iter() {
    // Iterator now yields UTF-8 codepoints as integers
    quiver()
        .evaluate(r#""hello" %str.iter %list.collect"#)
        .expect("Cons[104, Cons[101, Cons[108, Cons[108, Cons[111, Nil]]]]]");

    quiver()
        .evaluate(r#""" %str.iter %list.collect"#)
        .expect("Nil");

    quiver()
        .evaluate(r#""🚀" %str.iter %list.collect"#)
        .expect("Cons[4036991616, Nil]");

    quiver()
        .evaluate(r#""hi🚀" %str.iter %list.collect"#)
        .expect("Cons[104, Cons[105, Cons[4036991616, Nil]]]");
}

#[test]
fn test_collect() {
    // Collect now expects integers (UTF-8 codepoints), not strings
    quiver()
        .evaluate(
            r#"
            Cons[104, Cons[101, Cons[108, Cons[108, Cons[111, Nil]]]]] %list.iter %str.collect
            "#,
        )
        .expect("\"hello\"");

    quiver()
        .evaluate(r#"Nil %list.iter %str.collect"#)
        .expect("\"\"");

    quiver()
        .evaluate(r#"Cons[4036991616, Nil] %list.iter %str.collect"#)
        .expect("\"🚀\"");
}

#[test]
fn test_iter_collect_roundtrip() {
    quiver()
        .evaluate(r#""hello world 🚀" %str.iter %str.collect"#)
        .expect("\"hello world 🚀\"");
}

#[test]
fn test_iter_with_transformations() {
    quiver()
        .evaluate(
            r#"
            "hello" %str.iter [~, 3] %iter.take %str.collect
            "#,
        )
        .expect("\"hel\"");

    quiver()
        .evaluate(
            r#"
            "hello" %str.iter [~, 2] %iter.drop %str.collect
            "#,
        )
        .expect("\"llo\"");
}

#[test]
fn test_join() {
    quiver()
        .evaluate(
            r#"
            Cons["one", Cons["two", Cons["three", Nil]]] %list.iter [~, ", "] %str.join
            "#,
        )
        .expect("\"one, two, three\"");

    quiver()
        .evaluate(r#"Cons["only", Nil] %list.iter [~, ", "] %str.join"#)
        .expect("\"only\"");

    quiver()
        .evaluate(r#"Nil %list.iter [~, ", "] %str.join"#)
        .expect("\"\"");
}
