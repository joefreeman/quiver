mod common;
use common::*;

#[test]
fn test_binary_lowercase_digits() {
    quiver().evaluate("<ff>").expect("<ff>");
}

#[test]
fn test_binary_uppercase_digits() {
    quiver().evaluate("<ABCD>").expect("<abcd>");
}

#[test]
fn test_binary_mixed_case_digits() {
    quiver().evaluate("<AbCd>").expect("<abcd>");
}

#[test]
fn test_binary_multiple_bytes() {
    quiver().evaluate("<00ff>").expect("<00ff>");
    quiver().evaluate("<abcd>").expect("<abcd>");
}

#[test]
fn test_empty_binary() {
    quiver().evaluate("<>").expect("<>");
}

#[test]
fn test_binary_odd_digits_is_error() {
    // Binary literals require an even number of hex digits.
    quiver().evaluate("<f>").expect_parse_failure();
}

#[test]
fn test_binary_digits_may_be_grouped() {
    // Grouping is presentation: the value is the concatenation, however it was divided.
    quiver()
        .evaluate("<6a09e667 bb67ae85>")
        .expect("<6a09e667bb67ae85>");
    quiver().evaluate("<0a 1b 2c>").expect("<0a1b2c>");
    quiver()
        .evaluate("<6a09e667 bb67ae85> ~> =<6a09e667bb67ae85>")
        .expect("Ok");
}

#[test]
fn test_binary_group_must_be_whole_bytes() {
    // Checked per group, not on the total — `0a1 b2c` is six digits but neither group is
    // a whole number of bytes.
    quiver().evaluate("<0a1 b2c>").expect_parse_failure();
}

#[test]
fn test_binary_whitespace_separates_but_does_not_pad_a_line() {
    // On one line the brackets stay tight against the digits; across lines they sit apart.
    quiver().evaluate("< 0a1b>").expect_parse_failure();
    quiver().evaluate("<0a1b >").expect_parse_failure();
}

#[test]
fn test_binary_may_be_written_across_lines() {
    // A newline starts a new row, which is how a table of constants is written. Rows are
    // layout only, so the value is the same run of bytes.
    quiver()
        .evaluate("<\n  6a09e667 bb67ae85\n  3c6ef372 a54ff53a\n>")
        .expect("<6a09e667bb67ae85…> (16 bytes)");
    quiver().evaluate("<\n  0a1b\n> ~> =<0a1b>").expect("Ok");
    // Blank lines and ragged indentation collapse — neither carries bytes.
    quiver()
        .evaluate("<\n\n      0a1b\n\n  2c3d\n\n>")
        .expect("<0a1b2c3d>");
}

#[test]
fn test_binary_across_lines_still_needs_whole_byte_groups() {
    quiver()
        .evaluate("<\n  0a1b\n  2c3\n>")
        .expect_parse_failure();
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
