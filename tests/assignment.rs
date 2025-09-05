mod common;
use common::*;

#[test]
fn test_simple_assignment() {
    quiver().evaluate("x = 1, y = 2, [x, y] ~> +").expect_int(3);
}

#[test]
fn test_tuple_destructuring() {
    quiver()
        .evaluate("[a, b] = [1, 2], [a, b] ~> +")
        .expect_int(3);
}

#[test]
fn test_named_field_assignment() {
    quiver().evaluate("A[a: a] = A[a: 1], a").expect_int(1);
}

#[test]
fn test_nested_field_assignment() {
    quiver()
        .evaluate("A[a: B[b: b]] = A[a: B[b: 2]], b")
        .expect_int(2);
}

#[test]
fn test_partial_tuple_assignment() {
    quiver()
        .evaluate("(x, y) = [x: 1, y: 2, z: 3], [x, y] ~> +")
        .expect_int(3);
}

#[test]
fn test_start_assignment() {
    quiver()
        .evaluate("* = [a: 1, b: 2], [a, b] ~> +")
        .expect_int(3);
}

#[test]
fn test_ignore_placeholder() {
    quiver().evaluate("[a, _] = [1, 2], a").expect_int(1);
}

#[test]
fn test_failed_assignment_length() {
    quiver().evaluate("[a] = [1, 2]").expect_nil();
}

#[test]
fn test_failed_assignment_on_mismatched_literal() {
    quiver().evaluate("a = 4, 3 = a").expect_nil();
}
#[test]
fn test_failed_assignment_on_mismatched_literal_in_tuple() {
    quiver().evaluate("[a, 3] = [1, 2]").expect_nil();
}

#[test]
fn test_failed_assignment_with_unrecognised_partial_field() {
    // TODO: compile time error?
    quiver().evaluate("(c) = [a: 1, b: 2]").expect_nil();
}

#[test]
fn test_failed_assignment_type() {
    // TODO: compile time error?
    quiver().evaluate("A[a] = B[1]").expect_nil();
}
