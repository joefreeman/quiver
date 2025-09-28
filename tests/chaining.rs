mod common;
use common::*;

#[test]
fn test_operation_chaining() {
    quiver()
        .evaluate("[1, 2] ~> <add> ~> [~, 2] ~> <multiply>")
        .expect("6");
}

#[test]
fn test_member_access_chaining() {
    quiver().evaluate("Point[x: 1, y: 2] ~> .x").expect("1");
}

#[test]
fn test_nested_tupple_construction() {
    quiver().evaluate("42 ~> A[B[C[~]]]").expect("A[B[C[42]]]");
}
