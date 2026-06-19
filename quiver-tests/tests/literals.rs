mod common;
use common::*;

#[test]
fn test_binary_lowercase_digits() {
    quiver().evaluate("0xff").expect("0xff");
}

#[test]
fn test_binary_uppercase_digits() {
    quiver().evaluate("0xABCD").expect("0xabcd");
}

#[test]
fn test_binary_mixed_case_digits() {
    quiver().evaluate("0xAbCd").expect("0xabcd");
}

#[test]
fn test_binary_multiple_bytes() {
    quiver().evaluate("0x00ff").expect("0x00ff");
    quiver().evaluate("0xabcd").expect("0xabcd");
}

#[test]
fn test_empty_binary() {
    quiver().evaluate("0x").expect("0x");
}

#[test]
fn test_binary_odd_digits_is_error() {
    // Binary literals require an even number of hex digits.
    quiver().evaluate("0xf").expect_parse_failure();
}

#[test]
fn test_binary_integer_literal_removed() {
    // `0b...` is no longer an integer literal.
    quiver().evaluate("0b1010").expect_parse_failure();
}

#[test]
fn test_decimal_still_works() {
    quiver().evaluate("42").expect("42");
}

#[test]
fn test_negative_decimal_still_works() {
    quiver().evaluate("-42").expect("-42");
}

#[test]
fn test_zero() {
    quiver().evaluate("0").expect("0");
}
