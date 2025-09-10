mod common;
use common::*;

#[test]
fn test_not_nil() {
    quiver().evaluate("[] ~> !").expect_ok();
}

#[test]
fn test_not_ok() {
    quiver().evaluate("Ok ~> !").expect_nil();
}

#[test]
fn test_not_integer() {
    quiver().evaluate("42 ~> !").expect_nil();
}

#[test]
fn test_not_tuple() {
    quiver().evaluate("[1, 2] ~> !").expect_nil();
}

#[test]
fn test_not_equality_result() {
    quiver().evaluate("[1, 2] ~> == ~> !").expect_ok();
    quiver().evaluate("[42, 42] ~> == ~> !").expect_nil();
}

#[test]
fn test_double_not() {
    quiver().evaluate("[] ~> ! ~> !").expect_nil();
    quiver().evaluate("42 ~> ! ~> !").expect_ok();
}
