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
fn test_iter() {
    // Iterator now yields UTF-8 codepoints as integers
    quiver()
        .evaluate(
            r#"
            string = %"string",
            list = %"list",
            "hello" ~> string.iter ~> list.collect
            "#,
        )
        .expect("Cons[104, Cons[101, Cons[108, Cons[108, Cons[111, Nil]]]]]");

    quiver()
        .evaluate(
            r#"
            string = %"string",
            list = %"list",
            "" ~> string.iter ~> list.collect
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            string = %"string",
            list = %"list",
            "🚀" ~> string.iter ~> list.collect
            "#,
        )
        .expect("Cons[4036991616, Nil]");

    quiver()
        .evaluate(
            r#"
            string = %"string",
            list = %"list",
            "hi🚀" ~> string.iter ~> list.collect
            "#,
        )
        .expect("Cons[104, Cons[105, Cons[4036991616, Nil]]]");
}

#[test]
fn test_collect() {
    // Collect now expects integers (UTF-8 codepoints), not strings
    quiver()
        .evaluate(
            r#"
            string = %"string",
            list = %"list",
            Cons[104, Cons[101, Cons[108, Cons[108, Cons[111, Nil]]]]]
            ~> list.iter
            ~> string.collect
            "#,
        )
        .expect("\"hello\"");

    quiver()
        .evaluate(
            r#"
            string = %"string",
            list = %"list",
            Nil
            ~> list.iter
            ~> string.collect
            "#,
        )
        .expect("\"\"");

    quiver()
        .evaluate(
            r#"
            string = %"string",
            list = %"list",
            Cons[4036991616, Nil]
            ~> list.iter
            ~> string.collect
            "#,
        )
        .expect("\"🚀\"");
}

#[test]
fn test_iter_collect_roundtrip() {
    quiver()
        .evaluate(
            r#"
            string = %"string",
            "hello world 🚀" ~> string.iter ~> string.collect
            "#,
        )
        .expect("\"hello world 🚀\"");
}

#[test]
fn test_iter_with_transformations() {
    quiver()
        .evaluate(
            r#"
            string = %"string",
            iter = %"iter",
            "hello"
            ~> string.iter
            ~> iter.take[~, 3]
            ~> string.collect
            "#,
        )
        .expect("\"hel\"");

    quiver()
        .evaluate(
            r#"
            string = %"string",
            iter = %"iter",
            "hello"
            ~> string.iter
            ~> iter.drop[~, 2]
            ~> string.collect
            "#,
        )
        .expect("\"llo\"");
}

#[test]
fn test_join() {
    quiver()
        .evaluate(
            r#"
            list = %"list",
            string = %"string",
            Cons["one", Cons["two", Cons["three", Nil]]]
            ~> list.iter
            ~> string.join[~, ", "]
            "#,
        )
        .expect("\"one, two, three\"");

    quiver()
        .evaluate(
            r#"
            list = %"list",
            string = %"string",
            Cons["only", Nil] ~> list.iter ~> string.join[~, ", "]
            "#,
        )
        .expect("\"only\"");

    quiver()
        .evaluate(
            r#"
            list = %"list",
            string = %"string",
            Nil ~> list.iter ~> string.join[~, ", "]
            "#,
        )
        .expect("\"\"");
}
