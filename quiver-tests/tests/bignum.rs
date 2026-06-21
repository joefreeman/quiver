mod common;
use common::*;

// Quiver integers (and the rationals built on them) are arbitrary-precision: arithmetic
// that would overflow a 64-bit integer now produces the exact result.

#[test]
fn test_multiplication_exceeds_i64() {
    // i64::MAX squared is far beyond i64::MAX; the exact product is returned.
    quiver()
        .evaluate("[9223372036854775807, 9223372036854775807] ~> %math.mul")
        .expect("85070591730234615847396907784232501249");
}

#[test]
fn test_addition_exceeds_i64() {
    quiver()
        .evaluate("[9223372036854775807, 9223372036854775807] ~> %math.add")
        .expect("18446744073709551614");
}

#[test]
fn test_large_rational_multiply() {
    // 1/m * 1/m = 1/(m*m) where m = i64::MAX. The denominator overflows i64 but is
    // computed exactly.
    quiver()
        .evaluate("[1/9223372036854775807, 1/9223372036854775807] ~> %num.mul")
        .expect("1/85070591730234615847396907784232501249");
}

#[test]
fn test_large_integer_literal_round_trips() {
    quiver()
        .evaluate("99999999999999999999999999999999")
        .expect("99999999999999999999999999999999");
}
