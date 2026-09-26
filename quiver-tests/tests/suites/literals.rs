use crate::common::*;

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
        .expect("<6a09e667bb67ae85>");
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
    quiver()
        .evaluate("<\n  0a1b\n> ~> =<0a1b>")
        .expect("<0a1b>");
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

// Constant folding: a literal whose fields are all data is interned once and shared by every
// evaluation. These pin that the sharing stays invisible, which is what makes it sound.

#[test]
fn a_folded_literal_is_not_observably_shared() {
    // Both `[1, 2]`s are the same interned constant, so they are the same `Rc` at runtime.
    // Annotating one must not be visible through the other — annotation is copy-on-write,
    // and this is the test that says so out loud.
    quiver()
        .evaluate("a = [1, 2] ~> { :note 7 }; b = [1, 2]; [a ~> :note<'int>, b ~> :note<'int>]")
        .expect("[7, []]");
}

#[test]
fn a_folded_literal_destructures_and_compares() {
    quiver()
        .evaluate("[1, 2] ~> =[x, y]; [y, x]")
        .expect("[2, 1]");
    quiver()
        .evaluate("a = [1, 2]; [1, 2] ~> =^a")
        .expect("[1, 2]");
}

#[test]
fn a_nested_literal_folds_whole() {
    quiver()
        .evaluate("[[1, 2], A[3, [4]]] ~> =[[_, b], A[_, [d]]]; [b, d]")
        .expect("[2, 4]");
}

#[test]
fn a_literal_mixing_data_and_a_binding_still_builds_correctly() {
    quiver()
        .evaluate("x = 5; p = [[1, 2], x]; p ~> =[[a, _], b]; [a, b]")
        .expect("[1, 5]");
}
