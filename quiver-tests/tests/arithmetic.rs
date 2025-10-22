mod common;
use common::*;

#[test]
fn test_addition() {
    quiver().evaluate("[1, 2] ~> __add__").expect("3");
}

#[test]
fn test_subtraction() {
    quiver().evaluate("[8, 5] ~> __subtract__").expect("3");
}

#[test]
fn test_multiplication() {
    quiver().evaluate("[4, 5] ~> __multiply__").expect("20");
}

#[test]
fn test_division() {
    quiver().evaluate("[10, 2] ~> __divide__").expect("5");
}

#[test]
fn test_modulo() {
    quiver().evaluate("[9, 5] ~> __modulo__").expect("4");
}

// TODO: test divide by zero
