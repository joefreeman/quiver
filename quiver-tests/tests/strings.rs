mod common;
use common::*;

#[test]
fn test_string_literal() {
    quiver().evaluate("\"hello\" ~> .0").expect("'68656c6c6f'");
}

#[test]
fn test_utf8_string() {
    quiver().evaluate("\"café\" ~> .0").expect("'636166c3a9'");
}

#[test]
fn test_emoji_string() {
    quiver().evaluate("\"👋\" ~> .0").expect("'f09f918b'");
}
