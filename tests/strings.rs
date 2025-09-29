mod common;
use common::*;

#[test]
fn test_string_literal() {
    quiver().evaluate("\"hello\"").expect("Str['68656c6c6f']");
}

#[test]
fn test_utf8_string() {
    quiver().evaluate("\"café\"").expect("Str['636166c3a9']");
}

#[test]
fn test_emoji_string() {
    quiver().evaluate("\"👋\"").expect("Str['f09f918b']");
}
