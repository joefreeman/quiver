mod common;
use common::*;

#[test]
fn test_addition() {
    quiver().evaluate("[1, 2] ~> <add>").expect_int(3);
}

#[test]
fn test_subtraction() {
    quiver().evaluate("[8, 5] ~> <subtract>").expect_int(3);
}

#[test]
fn test_multiplication() {
    quiver().evaluate("[4, 5] ~> <multiply>").expect_int(20);
}

#[test]
fn test_division() {
    quiver().evaluate("[10, 2] ~> <divide>").expect_int(5);
}

#[test]
fn test_modulo() {
    quiver().evaluate("[9, 5] ~> <modulo>").expect_int(4);
}

// TODO: test divide by zero
