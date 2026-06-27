mod common;
use common::*;

#[test]
fn test_operation_chaining() {
    quiver()
        .evaluate("[1, 2] ~> __integer_add__ ~> [~, 2] ~> __integer_multiply__")
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

#[test]
fn test_sequence_threads_previous_result() {
    // A `,`-separated step operates on the previous step's result (threading), not on the block
    // parameter. So `~` in the second step is the first step's `5`, not the argument `9`.
    quiver().evaluate("f = #'int { 5, ~ }, 9 ~> f").expect("5");
}

#[test]
fn test_sequence_threads_through_multiple_steps() {
    // The value flows step to step: 1 -> 11 -> 111.
    quiver()
        .evaluate(
            "f = #'int { 1, [~, 10] ~> __integer_add__, [~, 100] ~> __integer_add__ }, 0 ~> f",
        )
        .expect("111");
}

#[test]
fn test_dollar_is_always_the_parameter_across_steps() {
    // `$` always refers to the function parameter, regardless of threading; `~` would be `5`.
    quiver().evaluate("f = #'int { 5, $ }, 9 ~> f").expect("9");
}

#[test]
fn test_sequence_short_circuits_on_nil() {
    // A step that yields nil short-circuits the rest of the sequence to nil (threading keeps the
    // existing short-circuit semantics).
    quiver().evaluate("[], 5").expect("[]");
}

#[test]
fn test_sequence_binding_persists_across_steps() {
    // Bindings persist across steps; naming a binding ignores the threaded value.
    quiver()
        .evaluate("f = #'int { x = 5, [x, $] }, 9 ~> f")
        .expect("[5, 9]");
}
