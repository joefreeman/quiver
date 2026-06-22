mod common;
use common::*;

// --- Literal desugaring: decimals and fractions become reduced `Rational` tuples.
//     Integer-valued literals stay rationals (`Rational[n, 1]`), distinct from `'int`. ---

#[test]
fn test_decimal_literal() {
    quiver().evaluate("1.5").expect("3/2");
}

#[test]
fn test_decimal_literal_reduces() {
    quiver().evaluate("0.25").expect("1/4");
    quiver().evaluate("0.30").expect("3/10");
}

#[test]
fn test_decimal_literal_negative() {
    quiver().evaluate("-1.5").expect("-3/2");
}

#[test]
fn test_fraction_literal() {
    quiver().evaluate("1/3").expect("1/3");
}

#[test]
fn test_fraction_literal_reduces() {
    quiver().evaluate("2/4").expect("1/2");
}

#[test]
fn test_fraction_literal_negative() {
    quiver().evaluate("-2/4").expect("-1/2");
}

// Integer-valued literals reduce but do NOT lower to `'int`: they remain rationals.
#[test]
fn test_integer_valued_literals_stay_rational() {
    quiver().evaluate("4/2").expect("2/1");
    quiver().evaluate("6/3").expect("2/1");
    quiver().evaluate("1.0").expect("1/1");
    quiver().evaluate("-9/3").expect("-3/1");
}

// --- A numeric literal does not disturb positional field access ---

#[test]
fn test_positional_access_still_works() {
    quiver().evaluate("x = [10, 20], x.0").expect("10");
    quiver().evaluate("x = [[1, 99], 20], x.0.1").expect("99");
}

// --- Polymorphic arithmetic: integers are closed under add/sub/mul (integer in, integer
//     out); any rational operand yields a rational, and rationals never lower to int ---

#[test]
fn test_int_add_stays_int() {
    quiver().evaluate("[2, 3] ~> %num.add").expect("5");
}

#[test]
fn test_int_sub_mul_stay_int() {
    quiver().evaluate("[5, 2] ~> %num.sub").expect("3");
    quiver().evaluate("[6, 7] ~> %num.mul").expect("42");
}

#[test]
fn test_mixed_add_is_rational() {
    quiver().evaluate("[1/2, 3] ~> %num.add").expect("7/2");
    quiver().evaluate("[3, 1/2] ~> %num.add").expect("7/2");
}

#[test]
fn test_rational_result_is_not_lowered() {
    // 1/2 + 1/2 is integral in value but stays a rational by representation.
    quiver().evaluate("[1/2, 1/2] ~> %num.add").expect("1/1");
    quiver().evaluate("[3/2, 1/2] ~> %num.sub").expect("1/1");
}

#[test]
fn test_add_exact() {
    // The classic 0.1 + 0.2 == 0.3 that floating point gets wrong.
    quiver().evaluate("[0.1, 0.2] ~> %num.add").expect("3/10");
}

#[test]
fn test_add_fractions() {
    quiver().evaluate("[1/3, 1/6] ~> %num.add").expect("1/2");
}

#[test]
fn test_mul() {
    quiver().evaluate("[2/3, 3/4] ~> %num.mul").expect("1/2");
}

// --- div is exact division and always returns a rational ---

#[test]
fn test_div_rationals() {
    quiver().evaluate("[1/2, 3/4] ~> %num.div").expect("2/3");
}

#[test]
fn test_div_ints_is_rational() {
    quiver().evaluate("[1, 2] ~> %num.div").expect("1/2");
    quiver().evaluate("[6, 3] ~> %num.div").expect("2/1");
}

#[test]
fn test_div_by_zero_is_nil() {
    quiver().evaluate("[1/2, 0] ~> %num.div").expect("[]");
    quiver().evaluate("[1, 0] ~> %num.div").expect("[]");
}

// --- neg preserves kind; conversions and accessors ---

#[test]
fn test_neg() {
    quiver().evaluate("3/4 ~> %num.neg").expect("-3/4");
    quiver().evaluate("5 ~> %num.neg").expect("-5");
}

#[test]
fn test_to_int_truncates() {
    quiver().evaluate("7/2 ~> %num.to_int").expect("3");
    quiver().evaluate("-7/2 ~> %num.to_int").expect("-3");
    quiver().evaluate("5 ~> %num.to_int").expect("5");
}

#[test]
fn test_numer_denom() {
    quiver().evaluate("3/4 ~> %num.numer").expect("3");
    quiver().evaluate("3/4 ~> %num.denom").expect("4");
    quiver().evaluate("5 ~> %num.numer").expect("5");
    quiver().evaluate("5 ~> %num.denom").expect("1");
}

// --- Comparison predicates (polymorphic across ints and rationals) ---

#[test]
fn test_eq() {
    quiver().evaluate("[1/2, 2/4] ~> %num.eq?").expect("Ok");
    quiver().evaluate("[1/2, 1/3] ~> %num.eq?").expect("[]");
    quiver().evaluate("[2, 2] ~> %num.eq?").expect("Ok");
}

#[test]
fn test_eq_mixed_int_rational() {
    // 2 (int) and 4/2 (rational) are the same number, so they compare equal,
    // even though they have different representations.
    quiver().evaluate("[2, 4/2] ~> %num.eq?").expect("Ok");
}

#[test]
fn test_lt() {
    quiver().evaluate("[1/3, 1/2] ~> %num.lt?").expect("Ok");
    quiver().evaluate("[1/2, 1/3] ~> %num.lt?").expect("[]");
    quiver().evaluate("[2, 3] ~> %num.lt?").expect("Ok");
}

#[test]
fn test_le_gt_ge() {
    quiver().evaluate("[1/2, 1/2] ~> %num.le?").expect("Ok");
    quiver().evaluate("[1/2, 1/3] ~> %num.gt?").expect("Ok");
    quiver().evaluate("[3, 2] ~> %num.ge?").expect("Ok");
    quiver().evaluate("[2, 3] ~> %num.ge?").expect("[]");
}

// --- Pattern matching against numeric literals ---

#[test]
fn test_match_decimal_literal() {
    quiver()
        .evaluate("[0.1, 0.2] ~> %num.add ~> =0.3")
        .expect("3/10");
}

#[test]
fn test_match_fraction_literal() {
    quiver().evaluate("2/4 ~> =1/2").expect("1/2");
}

#[test]
fn test_match_integer_valued_literal() {
    // `4/2` is the rational 2/1, so it matches the rational pattern `=2/1`, not `=2`.
    quiver().evaluate("4/2 ~> =2/1").expect("2/1");
    quiver().evaluate("4/2 ~> =2").expect("[]");
}

#[test]
fn test_match_literal_no_match_is_nil() {
    quiver()
        .evaluate("[0.1, 0.2] ~> %num.add ~> =0.4")
        .expect("[]");
}

// --- abs (kind-preserving: integer stays integer, rational stays rational) ---

#[test]
fn test_abs_integer() {
    quiver().evaluate("42 ~> %num.abs").expect("42");
    quiver().evaluate("-42 ~> %num.abs").expect("42");
    quiver().evaluate("0 ~> %num.abs").expect("0");
}

#[test]
fn test_abs_rational() {
    quiver().evaluate("-3/4 ~> %num.abs").expect("3/4");
    quiver().evaluate("3/4 ~> %num.abs").expect("3/4");
}
