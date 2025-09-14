mod common;
use common::*;

#[test]
fn test_not_nil() {
    quiver().evaluate("[] ~> !").expect("Ok");
}

#[test]
fn test_not_ok() {
    quiver().evaluate("Ok ~> !").expect("[]");
}

#[test]
fn test_not_integer() {
    quiver().evaluate("42 ~> !").expect("[]");
}

#[test]
fn test_not_tuple() {
    quiver().evaluate("[1, 2] ~> !").expect("[]");
}

#[test]
fn test_not_equality_result() {
    quiver().evaluate("[1, 2] ~> == ~> !").expect("Ok");
    quiver().evaluate("[42, 42] ~> == ~> !").expect("[]");
}

#[test]
fn test_double_not() {
    quiver().evaluate("[] ~> ! ~> !").expect("[]");
    quiver().evaluate("42 ~> ! ~> !").expect("Ok");
}
