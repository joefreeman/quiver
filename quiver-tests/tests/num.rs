mod common;
use common::*;

// --- Literal desugaring: decimals and fractions become reduced `Rational` tuples ---

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

// --- A numeric literal does not disturb positional field access ---

#[test]
fn test_positional_access_still_works() {
    quiver().evaluate("x = [10, 20], x.0").expect("10");
    quiver().evaluate("x = [[1, 99], 20], x.0.1").expect("99");
}

// --- Arithmetic via the `num` module ---

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
fn test_sub() {
    quiver().evaluate("[3/4, 1/4] ~> %num.sub").expect("1/2");
}

#[test]
fn test_mul() {
    quiver().evaluate("[2/3, 3/4] ~> %num.mul").expect("1/2");
}

#[test]
fn test_div() {
    quiver().evaluate("[1/2, 3/4] ~> %num.div").expect("2/3");
}

#[test]
fn test_div_by_zero_is_nil() {
    quiver().evaluate("[1/2, 0/1] ~> %num.div").expect("[]");
}

#[test]
fn test_neg() {
    quiver().evaluate("3/4 ~> %num.neg").expect("-3/4");
}

#[test]
fn test_from_int() {
    quiver().evaluate("5 ~> %num.from_int").expect("5/1");
}

// --- Comparison predicates ---

#[test]
fn test_eq() {
    quiver().evaluate("[1/2, 2/4] ~> %num.eq?").expect("Ok");
    quiver().evaluate("[1/2, 1/3] ~> %num.eq?").expect("[]");
}

#[test]
fn test_lt() {
    quiver().evaluate("[1/3, 1/2] ~> %num.lt?").expect("Ok");
    quiver().evaluate("[1/2, 1/3] ~> %num.lt?").expect("[]");
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
fn test_match_literal_no_match_is_nil() {
    quiver()
        .evaluate("[0.1, 0.2] ~> %num.add ~> =0.4")
        .expect("[]");
}
