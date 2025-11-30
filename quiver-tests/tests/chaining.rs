mod common;
use common::*;

#[test]
fn test_operation_chaining() {
    quiver()
        .evaluate("[1, 2] ~> __add__ ~> [~, 2] ~> __multiply__")
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

#[test]
fn test_nested_ripple_contexts() {
    quiver()
        .evaluate("1 ~> [2 ~> [~, ~], ~]")
        .expect("[[2, 2], 1]");
}

#[test]
fn test_standalone_ripple() {
    quiver().evaluate("42 ~> ~").expect("42");
}

#[test]
fn test_standalone_ripple_with_nested_chain() {
    // The ripple in "5 ~> ~" refers to 5, not to the outer 10
    // The outer value 10 is dropped (implicit continuation allows ignoring values)
    quiver().evaluate("10 ~> [5 ~> ~]").expect("[5]");
}

#[test]
fn test_nested_chain_outer_value_used() {
    // The first field uses the outer value
    quiver().evaluate("10 ~> [~, 5 ~> ~]").expect("[10, 5]");
}

#[test]
fn test_nested_chain_value_dropped() {
    // Outer value 1 is dropped, tuple [2, 3] is returned
    quiver().evaluate("1 ~> [2, 3 ~> ~]").expect("[2, 3]");
}
