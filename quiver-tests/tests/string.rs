mod common;
use common::*;

#[test]
fn test_bytes() {
    quiver()
        .evaluate(r#""hello" ~> %str.bytes"#)
        .expect("0x68656c6c6f");
    quiver().evaluate(r#""" ~> %str.bytes"#).expect("0x");
    quiver()
        .evaluate(r#""🚀" ~> %str.bytes"#)
        .expect("0xf09f9a80");
}

#[test]
fn test_empty() {
    quiver().evaluate(r#""" ~> %str.empty?"#).expect("Ok");
    quiver().evaluate(r#""hello" ~> %str.empty?"#).expect("[]");
    quiver().evaluate(r#""x" ~> %str.empty?"#).expect("[]");
}

#[test]
fn test_concat() {
    quiver()
        .evaluate(r#"["hello", " "] ~> %str.concat"#)
        .expect("\"hello \"");
    quiver()
        .evaluate(r#"["hello", " world"] ~> %str.concat"#)
        .expect("\"hello world\"");
    quiver()
        .evaluate(r#"["", "hello"] ~> %str.concat"#)
        .expect("\"hello\"");
    quiver()
        .evaluate(r#"["hello", ""] ~> %str.concat"#)
        .expect("\"hello\"");
    quiver()
        .evaluate(r#"["🚀", "!"] ~> %str.concat"#)
        .expect("\"🚀!\"");
}

#[test]
fn test_starts_with() {
    quiver()
        .evaluate(r#"["hello world", "hello"] ~> %str.starts_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "world"] ~> %str.starts_with?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", "hello"] ~> %str.starts_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello", ""] ~> %str.starts_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hi", "hello"] ~> %str.starts_with?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["🚀 rocket", "🚀"] ~> %str.starts_with?"#)
        .expect("Ok");
}

#[test]
fn test_ends_with() {
    quiver()
        .evaluate(r#"["hello world", "world"] ~> %str.ends_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "hello"] ~> %str.ends_with?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", "hello"] ~> %str.ends_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello", ""] ~> %str.ends_with?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hi", "hello"] ~> %str.ends_with?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["rocket 🚀", "🚀"] ~> %str.ends_with?"#)
        .expect("Ok");
}

#[test]
fn test_contains() {
    quiver()
        .evaluate(r#"["hello world", "world"] ~> %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "hello"] ~> %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "o w"] ~> %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hello world", "xyz"] ~> %str.contains?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", ""] ~> %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["hi", "hello"] ~> %str.contains?"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", "hello"] ~> %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["🚀 rocket 🌙", "rocket"] ~> %str.contains?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"["🚀 rocket 🌙", "🌙"] ~> %str.contains?"#)
        .expect("Ok");
}

#[test]
fn test_split() {
    quiver()
        .evaluate(r#"["hello,world,test", ","] ~> %str.split ~> %list.collect"#)
        .expect("Cons[\"hello\", Cons[\"world\", Cons[\"test\", Nil]]]");
    quiver()
        .evaluate(r#"["hello", ","] ~> %str.split ~> %list.collect"#)
        .expect("Cons[\"hello\", Nil]");
    quiver()
        .evaluate(r#"["a::b::c", "::"] ~> %str.split ~> %list.collect"#)
        .expect("Cons[\"a\", Cons[\"b\", Cons[\"c\", Nil]]]");
    quiver()
        .evaluate(r#"["", ","] ~> %str.split ~> %list.collect"#)
        .expect("Cons[\"\", Nil]");
    quiver()
        .evaluate(r#"[",hello,", ","] ~> %str.split ~> %list.collect"#)
        .expect("Cons[\"\", Cons[\"hello\", Cons[\"\", Nil]]]");
}

#[test]
fn test_split_returns_iterator() {
    // The result is a lazy iterator: bound to a variable it is a value (wrapped in
    // Iter[...]), so it flows through a chain without being called - no `&` needed.
    quiver()
        .evaluate(r#"parts = ["1,2,3", ","] ~> %str.split, parts ~> [~, " + "] ~> %str.join"#)
        .expect("\"1 + 2 + 3\"");
    // Laziness: the first field can be taken without materialising the rest.
    quiver()
        .evaluate(r#"["a,b,c", ","] ~> %str.split ~> [~, 0] ~> %iter.nth"#)
        .expect("\"a\"");
}

#[test]
fn test_length() {
    // ASCII strings
    quiver().evaluate(r#""hello" ~> %str.length"#).expect("5");
    quiver().evaluate(r#""" ~> %str.length"#).expect("0");
    quiver().evaluate(r#""a" ~> %str.length"#).expect("1");

    // Multi-byte UTF-8 characters
    quiver().evaluate(r#""🚀" ~> %str.length"#).expect("1");
    quiver().evaluate(r#""🚀🌙" ~> %str.length"#).expect("2");
    quiver()
        .evaluate(r#""hello 🚀 world 🌙" ~> %str.length"#)
        .expect("15");

    // Mixed ASCII and multi-byte
    quiver().evaluate(r#""café" ~> %str.length"#).expect("4");
    quiver().evaluate(r#""日本語" ~> %str.length"#).expect("3");
}

#[test]
fn test_slice() {
    // ASCII strings
    quiver()
        .evaluate(r#"["hello", 0, 5] ~> %str.slice"#)
        .expect("\"hello\"");
    quiver()
        .evaluate(r#"["hello", 1, 4] ~> %str.slice"#)
        .expect("\"ell\"");
    quiver()
        .evaluate(r#"["hello", 0, 1] ~> %str.slice"#)
        .expect("\"h\"");
    quiver()
        .evaluate(r#"["hello", 4, 5] ~> %str.slice"#)
        .expect("\"o\"");
    quiver()
        .evaluate(r#"["hello", 2, 2] ~> %str.slice"#)
        .expect("\"\"");

    // Multi-byte UTF-8 characters
    quiver()
        .evaluate(r#"["🚀🌙⭐", 0, 1] ~> %str.slice"#)
        .expect("\"🚀\"");
    quiver()
        .evaluate(r#"["🚀🌙⭐", 1, 2] ~> %str.slice"#)
        .expect("\"🌙\"");
    quiver()
        .evaluate(r#"["🚀🌙⭐", 0, 3] ~> %str.slice"#)
        .expect("\"🚀🌙⭐\"");

    // Mixed ASCII and multi-byte
    quiver()
        .evaluate(r#"["hello 🚀 world", 6, 7] ~> %str.slice"#)
        .expect("\"🚀\"");
    quiver()
        .evaluate(r#"["hello 🚀 world", 0, 6] ~> %str.slice"#)
        .expect("\"hello \"");
    quiver()
        .evaluate(r#"["hello 🚀 world", 8, 13] ~> %str.slice"#)
        .expect("\"world\"");
}

#[test]
fn test_index_of() {
    quiver()
        .evaluate(r#"["hello world", "world"] ~> %str.index_of"#)
        .expect("6");
    quiver()
        .evaluate(r#"["hello world", "hello"] ~> %str.index_of"#)
        .expect("0");
    quiver()
        .evaluate(r#"["hello world", "o"] ~> %str.index_of"#)
        .expect("4");
    quiver()
        .evaluate(r#"["hello world", "xyz"] ~> %str.index_of"#)
        .expect("[]");
    quiver()
        .evaluate(r#"["hello", ""] ~> %str.index_of"#)
        .expect("0");
    quiver()
        .evaluate(r#"["🚀 rocket 🌙", "rocket"] ~> %str.index_of"#)
        .expect("2");
    quiver()
        .evaluate(r#"["🚀 rocket 🌙", "🌙"] ~> %str.index_of"#)
        .expect("9");
}

#[test]
fn test_iter() {
    // Iterator now yields UTF-8 codepoints as integers
    quiver()
        .evaluate(r#""hello" ~> %str.iter ~> %list.collect"#)
        .expect("Cons[104, Cons[101, Cons[108, Cons[108, Cons[111, Nil]]]]]");

    quiver()
        .evaluate(r#""" ~> %str.iter ~> %list.collect"#)
        .expect("Nil");

    quiver()
        .evaluate(r#""🚀" ~> %str.iter ~> %list.collect"#)
        .expect("Cons[4036991616, Nil]");

    quiver()
        .evaluate(r#""hi🚀" ~> %str.iter ~> %list.collect"#)
        .expect("Cons[104, Cons[105, Cons[4036991616, Nil]]]");
}

#[test]
fn test_collect() {
    // Collect now expects integers (UTF-8 codepoints), not strings
    quiver()
        .evaluate(
            r#"
            Cons[104, Cons[101, Cons[108, Cons[108, Cons[111, Nil]]]]]
            ~> %list.iter
            ~> %str.collect
            "#,
        )
        .expect("\"hello\"");

    quiver()
        .evaluate(r#"Nil ~> %list.iter ~> %str.collect"#)
        .expect("\"\"");

    quiver()
        .evaluate(r#"Cons[4036991616, Nil] ~> %list.iter ~> %str.collect"#)
        .expect("\"🚀\"");
}

#[test]
fn test_iter_collect_roundtrip() {
    quiver()
        .evaluate(r#""hello world 🚀" ~> %str.iter ~> %str.collect"#)
        .expect("\"hello world 🚀\"");
}

#[test]
fn test_iter_with_transformations() {
    quiver()
        .evaluate(
            r#"
            "hello"
            ~> %str.iter
            ~> [~, 3] ~> %iter.take
            ~> %str.collect
            "#,
        )
        .expect("\"hel\"");

    quiver()
        .evaluate(
            r#"
            "hello"
            ~> %str.iter
            ~> [~, 2] ~> %iter.drop
            ~> %str.collect
            "#,
        )
        .expect("\"llo\"");
}

#[test]
fn test_join() {
    quiver()
        .evaluate(
            r#"
            Cons["one", Cons["two", Cons["three", Nil]]]
            ~> %list.iter
            ~> [~, ", "] ~> %str.join
            "#,
        )
        .expect("\"one, two, three\"");

    quiver()
        .evaluate(r#"Cons["only", Nil] ~> %list.iter ~> [~, ", "] ~> %str.join"#)
        .expect("\"only\"");

    quiver()
        .evaluate(r#"Nil ~> %list.iter ~> [~, ", "] ~> %str.join"#)
        .expect("\"\"");
}
