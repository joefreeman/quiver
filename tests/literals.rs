mod common;

use common::*;

#[test]
fn test_positive_integers() {
    expect_int("42", 42);
    expect_int("1", 1);
    expect_int("999", 999);
    expect_int("123456789", 123456789);
}

#[test] 
fn test_negative_integers() {
    expect_int("-42", -42);
    expect_int("-1", -1);
    expect_int("-999", -999);
    expect_int("-123456789", -123456789);
}

#[test]
fn test_zero() {
    expect_int("0", 0);
    expect_int("-0", 0);
}

#[test]
fn test_large_integers() {
    expect_int("9223372036854775807", i64::MAX);
    expect_int("-9223372036854775808", i64::MIN);
}

#[test]
fn test_empty_tuple() {
    expect_nil("[]");
}

#[test]
fn test_binary_literals() {
    expect_binary("'00'", 0);
    expect_binary("'ff'", 0);
    expect_binary("'0fa27c'", 0);
    expect_binary("'deadbeef'", 0);
}

#[test]
fn test_hex_case_insensitive() {
    expect_binary("'FF'", 0);
    expect_binary("'DeAdBeEf'", 0);
    expect_binary("'aB12Cd34'", 0);
}

#[test]
fn test_empty_binary() {
    expect_binary("''", 0);
}

#[test] 
fn test_odd_length_hex() {
    expect_binary("'f'", 0);
    expect_binary("'123'", 0);
}

#[test]
fn test_string_literals() {
    expect_binary("\"hello\"", 0);
    expect_binary("\"world\"", 0);
    expect_binary("\"\"", 0);
}

#[test]
fn test_string_escaping() {
    expect_binary("\"hello\\nworld\"", 0);
    expect_binary("\"quote: \\\"text\\\"\"", 0);
    expect_binary("\"backslash: \\\\\"", 0);
    expect_binary("\"tab:\\t\"", 0);
}

#[test]
fn test_multiline_strings() {
    expect_binary("\"line1\\nline2\"", 0);
    expect_binary("\"first\\nsecond\\nthird\"", 0);
}

#[test]
fn test_unicode_in_strings() {
    expect_binary("\"hello 🦀\"", 0);
    expect_binary("\"café\"", 0);
    expect_binary("\"Ω μ ∑\"", 0);
}

#[test]
fn test_comments_ignored() {
    expect_int("42 // this is a comment", 42);
    expect_int("// comment at start\n42", 42);
    expect_nil("// only comment");
}

#[test]
fn test_line_comments() {
    expect_int("42 // comment", 42);
    expect_int("123 // another comment with symbols !@#$%", 123);
}

#[test]
fn test_comments_with_code_following() {
    expect_int("// This is ignored\n42", 42);
    expect_int("5 // comment\n// another comment\n", 5);
}

#[test] 
fn test_whitespace_handling() {
    expect_int("  42  ", 42);
    expect_int("\t42\t", 42);
    expect_int("\n42\n", 42);
    expect_int("   \t\n  42  \t\n  ", 42);
}

#[test]
fn test_mixed_whitespace_and_comments() {
    expect_int("  // comment\n  42  // another comment\n  ", 42);
    expect_nil("  // only comments\n  // more comments  ");
}