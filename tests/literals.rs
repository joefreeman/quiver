mod common;
use common::*;

#[test]
fn test_integer_literal() {
    quiver().evaluate("42").expect_int(42);
}

#[test]
fn test_binary_literal() {
    quiver().evaluate("'00ff'").expect_binary(&[0x00, 0xff]);
}

#[test]
fn test_string_literal() {
    quiver().evaluate("\"hello\"").expect_binary(b"hello");
}

#[test]
fn test_utf8_string() {
    quiver()
        .evaluate("\"café\"")
        .expect_binary("café".as_bytes());
}

#[test]
fn test_emoji_string() {
    quiver().evaluate("\"👋\"").expect_binary("👋".as_bytes());
}
