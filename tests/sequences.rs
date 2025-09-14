mod common;
use common::*;

#[test]
fn test_sequence_returns_last() {
    quiver().evaluate("1, 2, 3").expect("3");
}

#[test]
fn test_sequence_with_nil() {
    quiver().evaluate("1, [], 3").expect("[]");
}
