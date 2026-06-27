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
        .expect("Ok");
}

#[test]
fn test_match_fraction_literal() {
    quiver().evaluate("2/4 ~> =1/2").expect("Ok");
}

#[test]
fn test_match_integer_valued_literal() {
    // `4/2` is the rational 2/1, so it matches the rational pattern `=2/1`, not `=2`.
    quiver().evaluate("4/2 ~> =2/1").expect("Ok");
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

// --- Rung 2: single-radical surds, a + b√n (the field ℚ(√n)) ---

#[test]
fn test_sqrt_perfect_square_is_integer() {
    quiver().evaluate("4 ~> %num.sqrt").expect("2");
    quiver().evaluate("9 ~> %num.sqrt").expect("3");
    quiver().evaluate("0 ~> %num.sqrt").expect("0");
}

#[test]
fn test_sqrt_non_square_is_surd() {
    // √8 = 2√2 (square factor extracted); √12 = 2√3.
    quiver().evaluate("2 ~> %num.sqrt").expect("√2");
    quiver().evaluate("8 ~> %num.sqrt").expect("2√2");
    quiver().evaluate("12 ~> %num.sqrt").expect("2√3");
}

#[test]
fn test_sqrt_rational() {
    // √(1/4) = 1/2; √(1/2) = (1/2)√2.
    quiver().evaluate("1/4 ~> %num.sqrt").expect("1/2");
    quiver().evaluate("1/2 ~> %num.sqrt").expect("(1/2)√2");
}

#[test]
fn test_sqrt_negative_is_nil() {
    quiver().evaluate("1 ~> %num.neg ~> %num.sqrt").expect("[]");
}

#[test]
fn test_sqrt_of_surd_is_nil() {
    // Denesting is out of scope for this rung.
    quiver()
        .evaluate("2 ~> %num.sqrt ~> %num.sqrt")
        .expect("[]");
}

#[test]
fn test_surd_add_same_radical() {
    // √2 + √2 = 2√2; √2 + √8 = √2 + 2√2 = 3√2.
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, [s, s] ~> %num.add")
        .expect("2√2");
    quiver()
        .evaluate("a = 2 ~> %num.sqrt, b = 8 ~> %num.sqrt, [a, b] ~> %num.add")
        .expect("3√2");
}

#[test]
fn test_surd_add_rational_part() {
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, [1, s] ~> %num.add")
        .expect("1 + √2");
}

#[test]
fn test_surd_cancels_to_rational() {
    // (1 + √2) − √2 = 1 (collapses to a bare integer).
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, x = [1, s] ~> %num.add, [x, s] ~> %num.sub")
        .expect("1");
}

#[test]
fn test_surd_mul_squares_to_integer() {
    // √2 · √2 = 2.
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, [s, s] ~> %num.mul")
        .expect("2");
}

#[test]
fn test_surd_mul_conjugates() {
    // (1 + √2)(1 − √2) = 1 − 2 = −1.
    quiver()
        .evaluate(
            "s = 2 ~> %num.sqrt, a = [1, s] ~> %num.add, b = [1, s ~> %num.neg] ~> %num.add, [a, b] ~> %num.mul",
        )
        .expect("-1");
}

#[test]
fn test_surd_div_rationalizes() {
    // 1/√2 = (1/2)√2.
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, [1, s] ~> %num.div")
        .expect("(1/2)√2");
}

#[test]
fn test_mixed_radicals_fail_to_nil() {
    // √2 and √3 live in different fields; arithmetic across them is unsupported.
    quiver()
        .evaluate("a = 2 ~> %num.sqrt, b = 3 ~> %num.sqrt, [a, b] ~> %num.mul")
        .expect("[]");
    quiver()
        .evaluate("a = 2 ~> %num.sqrt, b = 3 ~> %num.sqrt, [a, b] ~> %num.add")
        .expect("[]");
}

#[test]
fn test_surd_neg_and_abs() {
    quiver()
        .evaluate("2 ~> %num.sqrt ~> %num.neg")
        .expect("-√2");
    quiver()
        .evaluate("2 ~> %num.sqrt ~> %num.neg ~> %num.abs")
        .expect("√2");
}

#[test]
fn test_surd_ordering_against_rationals() {
    // √2 ≈ 1.41421356…
    quiver()
        .evaluate("2 ~> %num.sqrt ~> [~, 1] ~> %num.gt?")
        .expect("Ok");
    quiver()
        .evaluate("2 ~> %num.sqrt ~> [~, 1] ~> %num.lt?")
        .expect("[]");
    quiver()
        .evaluate("2 ~> %num.sqrt ~> [~, 3/2] ~> %num.lt?")
        .expect("Ok");
    quiver()
        .evaluate("2 ~> %num.sqrt ~> [~, 141/100] ~> %num.lt?")
        .expect("[]");
    quiver()
        .evaluate("2 ~> %num.sqrt ~> [~, 142/100] ~> %num.lt?")
        .expect("Ok");
}

#[test]
fn test_surd_negative_ordering() {
    quiver()
        .evaluate("2 ~> %num.sqrt ~> %num.neg ~> [~, 0] ~> %num.lt?")
        .expect("Ok");
}

#[test]
fn test_surd_equality() {
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, [s, s] ~> %num.eq?")
        .expect("Ok");
    // 1 + √2 equals √2 + 1 regardless of construction order.
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, a = [1, s] ~> %num.add, b = [s, 1] ~> %num.add, [a, b] ~> %num.eq?")
        .expect("Ok");
}

#[test]
fn test_different_radicals_are_not_equal() {
    quiver()
        .evaluate("a = 2 ~> %num.sqrt, b = 3 ~> %num.sqrt, [a, b] ~> %num.eq?")
        .expect("[]");
}

#[test]
fn test_golden_ratio() {
    // φ = (1 + √5)/2 ≈ 1.618; bracket it between consecutive Fibonacci ratios 8/5 and 13/8.
    // Failable results (sqrt, div) are bound before reuse so the binding narrows away `[]`.
    let phi = "r = 5 ~> %num.sqrt, s = [1, r] ~> %num.add, phi = [s, 2] ~> %num.div,";
    quiver()
        .evaluate(&format!("{phi} [phi, 8/5] ~> %num.gt?"))
        .expect("Ok");
    quiver()
        .evaluate(&format!("{phi} [phi, 13/8] ~> %num.lt?"))
        .expect("Ok");
    // φ² = φ + 1.
    quiver()
        .evaluate(&format!(
            "{phi} sq = [phi, phi] ~> %num.mul, p1 = [phi, 1] ~> %num.add, [sq, p1] ~> %num.eq?"
        ))
        .expect("Ok");
}

#[test]
fn test_surd_to_int_truncates_toward_zero() {
    // √2 ≈ 1.414, 1+√2 ≈ 2.414, 5√2 ≈ 7.07, −3+√2 ≈ −1.586, √2/2 ≈ 0.707.
    quiver()
        .evaluate("2 ~> %num.sqrt ~> %num.to_int")
        .expect("1");
    quiver()
        .evaluate("2 ~> %num.sqrt ~> %num.neg ~> %num.to_int")
        .expect("-1");
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, [1, s] ~> %num.add ~> %num.to_int")
        .expect("2");
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, [s, 5] ~> %num.mul ~> %num.to_int")
        .expect("7");
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, n = 3 ~> %num.neg, [n, s] ~> %num.add ~> %num.to_int")
        .expect("-1");
    quiver()
        .evaluate("1/2 ~> %num.sqrt ~> %num.to_int")
        .expect("0");
}

#[test]
fn test_surd_formatting() {
    // Surds render in mathematical notation rather than as raw `Surd[...]` tuples.
    quiver().evaluate("2 ~> %num.sqrt").expect("√2");
    quiver().evaluate("8 ~> %num.sqrt").expect("2√2");
    quiver().evaluate("1/2 ~> %num.sqrt").expect("(1/2)√2");
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, [1, s] ~> %num.add")
        .expect("1 + √2");
    quiver()
        .evaluate("s = 2 ~> %num.sqrt, n = s ~> %num.neg, [3, n] ~> %num.add")
        .expect("3 - √2");
    quiver()
        .evaluate("2 ~> %num.sqrt ~> %num.neg")
        .expect("-√2");
    // φ = 1/2 + (1/2)√5.
    quiver()
        .evaluate("f = 5 ~> %num.sqrt, s = [1, f] ~> %num.add, [s, 2] ~> %num.div")
        .expect("1/2 + (1/2)√5");
}
