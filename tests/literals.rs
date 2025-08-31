mod common;

use common::*;

#[test]
fn test_positive_integers() {
    quiver().evaluate("42").expect_int(42);
    quiver().evaluate("1").expect_int(1);
    quiver().evaluate("999").expect_int(999);
    quiver().evaluate("123456789").expect_int(123456789);
}

#[test]
fn test_negative_integers() {
    quiver().evaluate("-42").expect_int(-42);
    quiver().evaluate("-1").expect_int(-1);
    quiver().evaluate("-999").expect_int(-999);
    quiver().evaluate("-123456789").expect_int(-123456789);
}

#[test]
fn test_zero() {
    quiver().evaluate("0").expect_int(0);
    quiver().evaluate("-0").expect_int(0);
}

#[test]
fn test_large_integers() {
    quiver()
        .evaluate("9223372036854775807")
        .expect_int(i64::MAX);
    quiver()
        .evaluate("-9223372036854775808")
        .expect_int(i64::MIN);
}

#[test]
fn test_empty_tuple() {
    quiver().evaluate("[]").expect_nil();
}

#[test]
fn test_binary_literals() {
    quiver().evaluate("'00'").expect_binary(0);
    quiver().evaluate("'ff'").expect_binary(0);
    quiver().evaluate("'0fa27c'").expect_binary(0);
    quiver().evaluate("'deadbeef'").expect_binary(0);
}

#[test]
fn test_hex_case_insensitive() {
    quiver().evaluate("'FF'").expect_binary(0);
    quiver().evaluate("'DeAdBeEf'").expect_binary(0);
    quiver().evaluate("'aB12Cd34'").expect_binary(0);
}

#[test]
fn test_empty_binary() {
    quiver().evaluate("''").expect_binary(0);
}

#[test]
fn test_odd_length_hex() {
    quiver().evaluate("'f'").expect_binary(0);
    quiver().evaluate("'123'").expect_binary(0);
}

#[test]
fn test_string_literals() {
    quiver().evaluate("\"hello\"").expect_binary(0);
    quiver().evaluate("\"world\"").expect_binary(0);
    quiver().evaluate("\"\"").expect_binary(0);
}

#[test]
fn test_string_escaping() {
    quiver().evaluate("\"hello\\nworld\"").expect_binary(0);
    quiver()
        .evaluate("\"quote: \\\"text\\\"\"")
        .expect_binary(0);
    quiver().evaluate("\"backslash: \\\\\"").expect_binary(0);
    quiver().evaluate("\"tab:\\t\"").expect_binary(0);
}

#[test]
fn test_multiline_strings() {
    quiver().evaluate("\"line1\\nline2\"").expect_binary(0);
    quiver()
        .evaluate("\"first\\nsecond\\nthird\"")
        .expect_binary(0);
}

#[test]
fn test_unicode_in_strings() {
    quiver().evaluate("\"hello 🦀\"").expect_binary(0);
    quiver().evaluate("\"café\"").expect_binary(0);
    quiver().evaluate("\"Ω μ ∑\"").expect_binary(0);
}

#[test]
fn test_comments_ignored() {
    quiver().evaluate("42 // this is a comment").expect_int(42);
    quiver().evaluate("// comment at start\n42").expect_int(42);
    quiver().evaluate("// only comment").expect_nil();
}

#[test]
fn test_line_comments() {
    quiver().evaluate("42 // comment").expect_int(42);
    quiver()
        .evaluate("123 // another comment with symbols !@#$%")
        .expect_int(123);
}

#[test]
fn test_comments_with_code_following() {
    quiver().evaluate("// This is ignored\n42").expect_int(42);
    quiver()
        .evaluate("5 // comment\n// another comment\n")
        .expect_int(5);
}

#[test]
fn test_whitespace_handling() {
    quiver().evaluate("  42  ").expect_int(42);
    quiver().evaluate("\t42\t").expect_int(42);
    quiver().evaluate("\n42\n").expect_int(42);
    quiver().evaluate("   \t\n  42  \t\n  ").expect_int(42);
}

#[test]
fn test_mixed_whitespace_and_comments() {
    quiver()
        .evaluate("  // comment\n  42  // another comment\n  ")
        .expect_int(42);
    quiver()
        .evaluate("  // only comments\n  // more comments  ")
        .expect_nil();
}
