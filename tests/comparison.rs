mod common;
use common::*;

#[test]
fn test_less_than_true() {
    quiver().evaluate("[1, 5, 7] ~> <").expect_ok();
}

#[test]
fn test_less_than_false() {
    quiver().evaluate("[3, 2, 1] ~> <").expect_nil();
}

#[test]
fn test_greater_than() {
    quiver().evaluate("[7, 2, 1] ~> >").expect_ok();
}

#[test]
fn test_less_than_or_equal() {
    quiver().evaluate("[1, 2, 2] ~> <=").expect_ok();
}

#[test]
fn test_greater_than_or_equal() {
    quiver().evaluate("[4, 3, 3] ~> >=").expect_ok();
}
