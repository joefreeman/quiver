mod common;
use common::*;

#[test]
fn test_integer_literal() {
    quiver().evaluate("42").expect("42");
}

#[test]
fn test_binary_literal() {
    quiver().evaluate("'00ff'").expect("'00ff'");
}

#[test]
fn test_string_literal() {
    quiver().evaluate("\"hello\"").expect("'68656c6c6f'");
}

#[test]
fn test_utf8_string() {
    quiver().evaluate("\"café\"").expect("'636166c3a9'");
}

#[test]
fn test_emoji_string() {
    quiver().evaluate("\"👋\"").expect("'f09f918b'");
}
