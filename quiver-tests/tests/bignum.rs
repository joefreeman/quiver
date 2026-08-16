mod common;
use common::*;

// Quiver integers (and the rationals built on them) are arbitrary-precision: arithmetic
// that would overflow a 64-bit integer now produces the exact result.

#[test]
fn test_multiplication_exceeds_i64() {
    // i64::MAX squared is far beyond i64::MAX; the exact product is returned.
    quiver()
        .evaluate("[9223372036854775807, 9223372036854775807] ~> %num.mul ~")
        .expect("85070591730234615847396907784232501249");
}

#[test]
fn test_addition_exceeds_i64() {
    quiver()
        .evaluate("[9223372036854775807, 9223372036854775807] ~> %num.add ~")
        .expect("18446744073709551614");
}

#[test]
fn test_large_rational_multiply() {
    // 1/m * 1/m = 1/(m*m) where m = i64::MAX. The denominator overflows i64 but is
    // computed exactly.
    quiver()
        .evaluate("[1/9223372036854775807, 1/9223372036854775807] ~> %num.mul ~")
        .expect("1/85070591730234615847396907784232501249");
}

#[test]
fn test_large_integer_literal_round_trips() {
    quiver()
        .evaluate("99999999999999999999999999999999")
        .expect("99999999999999999999999999999999");
}

// --- Small/big boundary cases ---
//
// The runtime keeps integers in canonical form: `Value::Int(i64)` for anything in i64
// range, a boxed BigInt strictly outside it. These tests pin the promotion points
// (overflow must widen), the renormalization direction (results that fit must come back
// small — asserted by the `Big` constructor in debug builds), and the i64 edge cases
// that checked arithmetic must not wrap through.

#[test]
fn test_add_promotes_at_i64_max() {
    quiver()
        .evaluate("[9223372036854775807, 1] ~> __integer_add__ ~")
        .expect("9223372036854775808");
}

#[test]
fn test_subtract_promotes_at_i64_min() {
    quiver()
        .evaluate("[-9223372036854775808, 1] ~> __integer_subtract__ ~")
        .expect("-9223372036854775809");
}

#[test]
fn test_result_renormalizes_to_small() {
    // Round-trip across the boundary: (MAX + 1) - 1 must equal MAX (and, per the debug
    // assertion in `Big`, come back in small form).
    quiver()
        .evaluate(
            "[9223372036854775807, 1] ~> __integer_add__ ~ ~> [~, 1] ~> __integer_subtract__ ~",
        )
        .expect("9223372036854775807");
}

#[test]
fn test_divide_min_by_minus_one_promotes() {
    // i64::MIN / -1 overflows two's complement; must widen, not wrap.
    quiver()
        .evaluate("[-9223372036854775808, -1] ~> __integer_divide__ ~")
        .expect("9223372036854775808");
}

#[test]
fn test_modulo_min_by_minus_one() {
    quiver()
        .evaluate("[-9223372036854775808, -1] ~> __integer_modulo__ ~")
        .expect("0");
}

#[test]
fn test_abs_of_i64_min_promotes() {
    quiver()
        .evaluate("-9223372036854775808 ~> __integer_abs__ ~")
        .expect("9223372036854775808");
}

#[test]
fn test_multiply_promotes_on_overflow() {
    quiver()
        .evaluate("[4611686018427387904, 4] ~> __integer_multiply__ ~")
        .expect("18446744073709551616");
}

#[test]
fn test_gcd_of_i64_min_pair_promotes() {
    // gcd(MIN, MIN) = 2^63, one past i64::MAX.
    quiver()
        .evaluate("[-9223372036854775808, -9223372036854775808] ~> __integer_gcd__ ~")
        .expect("9223372036854775808");
}

#[test]
fn test_compare_small_against_big() {
    // A canonical big integer is strictly outside i64 range, so its sign decides.
    quiver()
        .evaluate("[1, 99999999999999999999999999999999] ~> __integer_compare__ ~")
        .expect("-1");
    quiver()
        .evaluate("[1, -99999999999999999999999999999999] ~> __integer_compare__ ~")
        .expect("1");
    quiver()
        .evaluate("[-99999999999999999999999999999999, 1] ~> __integer_compare__ ~")
        .expect("-1");
}

#[test]
fn test_literal_match_across_boundary() {
    // A computed big value must match a big literal pattern (Constant + Equal path).
    quiver()
        .evaluate("[9223372036854775807, 1] ~> __integer_add__ ~ ~> =9223372036854775808; Ok")
        .expect("Ok");
}

#[test]
fn test_sqrt_of_big_renormalizes_to_small() {
    // sqrt(10^32 - 1) = 10^16 - 1, which fits an i64 again.
    quiver()
        .evaluate("99999999999999999999999999999999 ~> __integer_sqrt__ ~")
        .expect("9999999999999999");
}

#[test]
fn test_bitwise_rejects_out_of_range() {
    quiver()
        .evaluate("[99999999999999999999999999999999, 1] ~> __integer_and__ ~")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Integer 99999999999999999999999999999999 does not fit in a 64-bit value".to_string(),
        ));
}
