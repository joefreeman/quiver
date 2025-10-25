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
    // So the outer value 10 is unused - this should be an error
    quiver().evaluate("10 ~> [5 ~> ~]").expect_compile_error(
        quiver_compiler::compiler::Error::ValueIgnored(
            "Tuple construction ignores piped value; use ripple (e.g., [~, 2]) or assignment pattern (e.g., =[x, y])".to_string(),
        ),
    );
}

#[test]
fn test_nested_chain_outer_value_used() {
    // The first field uses the outer value, so this is OK
    quiver().evaluate("10 ~> [~, 5 ~> ~]").expect("[10, 5]");
}

#[test]
fn test_nested_chain_in_tuple_error() {
    // Similar to test_standalone_ripple_with_nested_chain
    quiver().evaluate("1 ~> [2, 3 ~> ~]").expect_compile_error(
        quiver_compiler::compiler::Error::ValueIgnored(
            "Tuple construction ignores piped value; use ripple (e.g., [~, 2]) or assignment pattern (e.g., =[x, y])".to_string(),
        ),
    );
}
