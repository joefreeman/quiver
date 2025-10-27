mod common;
use common::*;

#[test]
fn test_less_than() {
    quiver().evaluate("[1, 5] ~> __compare__").expect("-1");
}

#[test]
fn test_greater_than() {
    quiver().evaluate("[7, 2] ~> __compare__").expect("1");
}

#[test]
fn test_equal() {
    quiver().evaluate("[2, 2] ~> __compare__").expect("0");
}
