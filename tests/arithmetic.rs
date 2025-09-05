mod common;
use common::*;

#[test]
fn test_addition() {
    quiver().evaluate("[1, 2, 3] ~> +").expect_int(6);
}

#[test]
fn test_subtraction() {
    quiver().evaluate("[8, 5, 2] ~> -").expect_int(1);
}

#[test]
fn test_multiplication() {
    quiver().evaluate("[4, 5] ~> *").expect_int(20);
}

#[test]
fn test_division() {
    quiver().evaluate("[10, 2] ~> /").expect_int(5);
}

#[test]
fn test_modulo() {
    quiver().evaluate("[9, 5] ~> %").expect_int(4);
}

// TODO: test divide by zero
