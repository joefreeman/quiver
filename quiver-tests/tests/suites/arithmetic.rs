use crate::common::*;

#[test]
fn test_addition() {
    quiver().evaluate("[1, 2] ~> __integer_add__ ~").expect("3");
}

#[test]
fn test_subtraction() {
    quiver()
        .evaluate("[8, 5] ~> __integer_subtract__ ~")
        .expect("3");
}

#[test]
fn test_multiplication() {
    quiver()
        .evaluate("[4, 5] ~> __integer_multiply__ ~")
        .expect("20");
}

#[test]
fn test_division() {
    quiver()
        .evaluate("[10, 2] ~> __integer_divide__ ~")
        .expect("5");
}

#[test]
fn test_division_by_zero() {
    quiver()
        .evaluate("[10, 0] ~> __integer_divide__ ~")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Division by zero".to_string(),
        ));
}

#[test]
fn test_modulo() {
    quiver()
        .evaluate("[9, 5] ~> __integer_modulo__ ~")
        .expect("4");
}

#[test]
fn test_modulo_by_zero() {
    quiver()
        .evaluate("[10, 0] ~> __integer_modulo__ ~")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Modulo by zero".to_string(),
        ));
}

#[test]
fn test_builtin_sqrt_negative_errors() {
    quiver()
        .evaluate(
            r#"
            -4 ~> __integer_sqrt__ ~
            "#,
        )
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Cannot take square root of negative number".to_string(),
        ));
}

#[test]
fn test_builtin_factor_finds_a_prime_factor() {
    quiver()
        .evaluate("[__integer_factor__ 2, __integer_factor__ 97, __integer_factor__ 12]")
        .expect("[2, 97, 2]");
    // Which prime of a semiprime comes back is unspecified.
    quiver()
        .evaluate("__integer_factor__ 1000036000099 ~> { =(1000003 | 1000033) => Ok }")
        .expect("Ok");
    // 2⁶⁴ + 1, past trial division.
    quiver()
        .evaluate("__integer_factor__ 18446744073709551617 ~> { =(274177 | 67280421310721) => Ok }")
        .expect("Ok");
}

#[test]
fn test_builtin_factor_gives_up_past_its_budget() {
    // 2¹²⁸ + 1 = 59649589127497217 · 5704689200685129054721: both primes are far beyond what
    // the rho budget can reach.
    quiver()
        .evaluate("__integer_factor__ 340282366920938463463374607431768211457")
        .expect("[]");
}

#[test]
fn test_builtin_factor_below_two_errors() {
    quiver()
        .evaluate("1 ~> __integer_factor__ ~")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Cannot factor 1: expected an integer of at least 2".to_string(),
        ));
}
