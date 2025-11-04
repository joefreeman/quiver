mod common;
use common::*;

#[test]
fn test_bytes() {
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "hello" ~> string.bytes
            "#,
        )
        .expect("'68656c6c6f'");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "" ~> string.bytes
            "#,
        )
        .expect("''");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "🚀" ~> string.bytes
            "#,
        )
        .expect("'f09f9a80'");
}

#[test]
fn test_empty() {
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "" ~> string.empty?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "hello" ~> string.empty?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "x" ~> string.empty?
            "#,
        )
        .expect("[]");
}

#[test]
fn test_concat() {
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", " "] ~> string.concat
            "#,
        )
        .expect("\"hello \"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", " world"] ~> string.concat
            "#,
        )
        .expect("\"hello world\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["", "hello"] ~> string.concat
            "#,
        )
        .expect("\"hello\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", ""] ~> string.concat
            "#,
        )
        .expect("\"hello\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀", "!"] ~> string.concat
            "#,
        )
        .expect("\"🚀!\"");
}

#[test]
fn test_starts_with() {
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "hello"] ~> string.starts_with?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "world"] ~> string.starts_with?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", "hello"] ~> string.starts_with?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", ""] ~> string.starts_with?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hi", "hello"] ~> string.starts_with?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀 rocket", "🚀"] ~> string.starts_with?
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_ends_with() {
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "world"] ~> string.ends_with?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "hello"] ~> string.ends_with?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", "hello"] ~> string.ends_with?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", ""] ~> string.ends_with?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hi", "hello"] ~> string.ends_with?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["rocket 🚀", "🚀"] ~> string.ends_with?
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains() {
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "world"] ~> string.contains?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "hello"] ~> string.contains?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "o w"] ~> string.contains?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "xyz"] ~> string.contains?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", ""] ~> string.contains?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hi", "hello"] ~> string.contains?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", "hello"] ~> string.contains?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀 rocket 🌙", "rocket"] ~> string.contains?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀 rocket 🌙", "🌙"] ~> string.contains?
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_split() {
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello,world,test", ","] ~> string.split
            "#,
        )
        .expect("Cons[\"hello\", Cons[\"world\", Cons[\"test\", Nil]]]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", ","] ~> string.split
            "#,
        )
        .expect("Cons[\"hello\", Nil]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["a::b::c", "::"] ~> string.split
            "#,
        )
        .expect("Cons[\"a\", Cons[\"b\", Cons[\"c\", Nil]]]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["", ","] ~> string.split
            "#,
        )
        .expect("Cons[\"\", Nil]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            [",hello,", ","] ~> string.split
            "#,
        )
        .expect("Cons[\"\", Cons[\"hello\", Cons[\"\", Nil]]]");
}

#[test]
fn test_length() {
    // ASCII strings
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "hello" ~> string.length
            "#,
        )
        .expect("5");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "" ~> string.length
            "#,
        )
        .expect("0");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "a" ~> string.length
            "#,
        )
        .expect("1");

    // Multi-byte UTF-8 characters
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "🚀" ~> string.length
            "#,
        )
        .expect("1");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "🚀🌙" ~> string.length
            "#,
        )
        .expect("2");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "hello 🚀 world 🌙" ~> string.length
            "#,
        )
        .expect("15");

    // Mixed ASCII and multi-byte
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "café" ~> string.length
            "#,
        )
        .expect("4");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            "日本語" ~> string.length
            "#,
        )
        .expect("3");
}

#[test]
fn test_slice() {
    // ASCII strings
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", 0, 5] ~> string.slice
            "#,
        )
        .expect("\"hello\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", 1, 4] ~> string.slice
            "#,
        )
        .expect("\"ell\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", 0, 1] ~> string.slice
            "#,
        )
        .expect("\"h\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", 4, 5] ~> string.slice
            "#,
        )
        .expect("\"o\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", 2, 2] ~> string.slice
            "#,
        )
        .expect("\"\"");

    // Multi-byte UTF-8 characters
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀🌙⭐", 0, 1] ~> string.slice
            "#,
        )
        .expect("\"🚀\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀🌙⭐", 1, 2] ~> string.slice
            "#,
        )
        .expect("\"🌙\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀🌙⭐", 0, 3] ~> string.slice
            "#,
        )
        .expect("\"🚀🌙⭐\"");

    // Mixed ASCII and multi-byte
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello 🚀 world", 6, 7] ~> string.slice
            "#,
        )
        .expect("\"🚀\"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello 🚀 world", 0, 6] ~> string.slice
            "#,
        )
        .expect("\"hello \"");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello 🚀 world", 8, 13] ~> string.slice
            "#,
        )
        .expect("\"world\"");
}

#[test]
fn test_index_of() {
    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "world"] ~> string.index_of
            "#,
        )
        .expect("6");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "hello"] ~> string.index_of
            "#,
        )
        .expect("0");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "o"] ~> string.index_of
            "#,
        )
        .expect("4");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello world", "xyz"] ~> string.index_of
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["hello", ""] ~> string.index_of
            "#,
        )
        .expect("0");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀 rocket 🌙", "rocket"] ~> string.index_of
            "#,
        )
        .expect("2");

    quiver()
        .evaluate(
            r#"
            string = %"string"
            ["🚀 rocket 🌙", "🌙"] ~> string.index_of
            "#,
        )
        .expect("9");
}

#[test]
fn test_chars() {
    // Empty string
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "" ~> string.chars
            "#,
        )
        .expect("Nil");

    // Single character
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "a" ~> string.chars
            "#,
        )
        .expect("Cons[\"a\", Nil]");

    // ASCII string
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "hello" ~> string.chars
            "#,
        )
        .expect("Cons[\"h\", Cons[\"e\", Cons[\"l\", Cons[\"l\", Cons[\"o\", Nil]]]]]");

    // Multi-byte UTF-8 characters
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "🚀🌙" ~> string.chars
            "#,
        )
        .expect("Cons[\"🚀\", Cons[\"🌙\", Nil]]");

    // Mixed ASCII and UTF-8
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "hi🚀" ~> string.chars
            "#,
        )
        .expect("Cons[\"h\", Cons[\"i\", Cons[\"🚀\", Nil]]]");

    // String with spaces
    quiver()
        .evaluate(
            r#"
            string = %"string"
            "a b" ~> string.chars
            "#,
        )
        .expect("Cons[\"a\", Cons[\" \", Cons[\"b\", Nil]]]");
}
