mod common;
use common::*;

#[test]
fn test_hexadecimal_lowercase_prefix() {
    quiver().evaluate("0xff").expect("255");
}

#[test]
fn test_hexadecimal_uppercase_digits() {
    quiver().evaluate("0xABCD").expect("43981");
}

#[test]
fn test_hexadecimal_mixed_case_digits() {
    quiver().evaluate("0xAbCd").expect("43981");
}

#[test]
fn test_hexadecimal_zero() {
    quiver().evaluate("0x0").expect("0");
}

#[test]
fn test_hexadecimal_negative() {
    quiver().evaluate("-0x10").expect("-16");
}

#[test]
fn test_binary_lowercase_prefix() {
    quiver().evaluate("0b1010").expect("10");
}

#[test]
fn test_binary_zero() {
    quiver().evaluate("0b0").expect("0");
}

#[test]
fn test_binary_one() {
    quiver().evaluate("0b1").expect("1");
}

#[test]
fn test_binary_negative() {
    quiver().evaluate("-0b101").expect("-5");
}

#[test]
fn test_decimal_still_works() {
    quiver().evaluate("42").expect("42");
}

#[test]
fn test_negative_decimal_still_works() {
    quiver().evaluate("-42").expect("-42");
}
