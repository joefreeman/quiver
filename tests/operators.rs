mod common;

use common::*;

#[test]
fn test_addition() {
    expect_int("[3, 4] ~> +", 7);
    expect_int("[10, 5] ~> +", 15);
    expect_int("[0, 0] ~> +", 0);
    expect_int("[-5, 3] ~> +", -2);
    expect_int("[-10, -20] ~> +", -30);
}

#[test]
fn test_subtraction() {
    expect_int("[10, 3] ~> -", 7);
    expect_int("[5, 10] ~> -", -5);
    expect_int("[0, 0] ~> -", 0);
    expect_int("[-5, -3] ~> -", -2);
    expect_int("[100, 1] ~> -", 99);
}

#[test]
fn test_multiplication() {
    expect_int("[5, 6] ~> *", 30);
    expect_int("[0, 999] ~> *", 0);
    expect_int("[1, 42] ~> *", 42);
    expect_int("[-3, 4] ~> *", -12);
    expect_int("[-5, -6] ~> *", 30);
}

#[test]
fn test_division() {
    expect_int("[15, 3] ~> /", 5);
    expect_int("[100, 10] ~> /", 10);
    expect_int("[7, 2] ~> /", 3);
    expect_int("[-15, 3] ~> /", -5);
    expect_int("[-15, -3] ~> /", 5);
}

#[test]
fn test_modulo() {
    expect_int("[17, 5] ~> %", 2);
    expect_int("[10, 3] ~> %", 1);
    expect_int("[20, 4] ~> %", 0);
    expect_int("[7, 7] ~> %", 0);
    expect_int("[-17, 5] ~> %", -2);
}

#[test]
fn test_equality() {
    expect_int("[5, 5] ~> ==", 5);
    expect_int("[42, 42] ~> ==", 42);
    expect_int("[0, 0] ~> ==", 0);
    expect_int("[-5, -5] ~> ==", -5);
    expect_nil("[5, 3] ~> ==");
    expect_nil("[0, 1] ~> ==");
    expect_nil("[-5, 5] ~> ==");
}

#[test]
fn test_not_equal() {
    expect_int("[5, 3] ~> !=", 5);
    expect_int("[0, 1] ~> !=", 0);
    expect_int("[-5, 5] ~> !=", -5);
    expect_nil("[5, 5] ~> !=");
    expect_nil("[42, 42] ~> !=");
    expect_nil("[0, 0] ~> !=");
}

#[test]
fn test_less_than() {
    expect_int("[3, 5] ~> <", 5);
    expect_int("[-10, 0] ~> <", 0);
    expect_int("[-20, -10] ~> <", -10);
    expect_nil("[5, 3] ~> <");
    expect_nil("[5, 5] ~> <");
    expect_nil("[10, -5] ~> <");
}

#[test]
fn test_less_than_or_equal() {
    expect_int("[3, 5] ~> <=", 5);
    expect_int("[5, 5] ~> <=", 5);
    expect_int("[-10, 0] ~> <=", 0);
    expect_nil("[5, 3] ~> <=");
    expect_nil("[10, -5] ~> <=");
}

#[test]
fn test_greater_than() {
    expect_int("[5, 3] ~> >", 3);
    expect_int("[0, -10] ~> >", -10);
    expect_int("[-10, -20] ~> >", -20);
    expect_nil("[3, 5] ~> >");
    expect_nil("[5, 5] ~> >"); 
    expect_nil("[-5, 10] ~> >");
}

#[test]
fn test_greater_than_or_equal() {
    expect_int("[5, 3] ~> >=", 3);
    expect_int("[5, 5] ~> >=", 5);
    expect_int("[0, -10] ~> >=", -10);
    expect_nil("[3, 5] ~> >=");
    expect_nil("[-5, 10] ~> >=");
}

#[test]
fn test_multi_arity_addition() {
    expect_int("[1, 2, 3, 4] ~> +", 10);
    expect_int("[5, 10, 15] ~> +", 30);
    expect_int("[100] ~> +", 100);
    expect_int("[-1, 1, -1, 1] ~> +", 0);
}

#[test]
fn test_multi_arity_multiplication() {
    expect_int("[2, 3, 4] ~> *", 24);
    expect_int("[1, 2, 3, 4, 5] ~> *", 120);
    expect_int("[10] ~> *", 10);
    expect_int("[0, 100, 200] ~> *", 0);
}

#[test]
fn test_multi_arity_subtraction() {
    expect_int("[100, 10, 5, 3] ~> -", 82);
    expect_int("[0, 1, 2, 3] ~> -", -6);
    expect_int("[50] ~> -", 50);
}

#[test]
fn test_nested_operations() {
    expect_int("[[2, 3] ~> +, 4] ~> *", 20);
    expect_int("[[10, 2] ~> /, [3, 1] ~> +] ~> +", 9);
    expect_int("[[[5, 3] ~> -, 2] ~> *, 10] ~> +", 14);
}

#[test]
fn test_operator_precedence_with_tuples() {
    expect_int("[[1, 2] ~> +, [3, 4] ~> *] ~> +", 15);
    expect_int("[[10, 5] ~> -, [2, 3] ~> +] ~> *", 25);
}

#[test]
fn test_comparison_with_tuples() {
    expect_int("[[5, 3] ~> +, 8] ~> ==", 8);
    expect_nil("[[5, 3] ~> +, 7] ~> ==");
    expect_int("[[10, 3] ~> -, [2, 3] ~> +] ~> >", 5);
}

#[test]
fn test_zero_division_edge_cases() {
    expect_error("[5, 0] ~> /");
    expect_error("[0, 0] ~> /");
    expect_error("[-10, 0] ~> /");
}

#[test]
fn test_zero_modulo_edge_cases() {
    expect_error("[5, 0] ~> %");
    expect_error("[0, 0] ~> %");
    expect_error("[-10, 0] ~> %");
}

#[test]
fn test_large_number_operations() {
    expect_int("[9223372036854775806, 1] ~> +", i64::MAX);
    expect_int("[-9223372036854775807, -1] ~> +", i64::MIN);
}

#[test]
fn test_overflow_handling() {
    expect_error("[9223372036854775807, 1] ~> +");
    expect_error("[-9223372036854775808, -1] ~> +");
    expect_error("[9223372036854775807, 2] ~> *");
}

#[test]
fn test_single_element_operations() {
    expect_int("[42] ~> +", 42);
    expect_int("[42] ~> *", 42);
    expect_int("[42] ~> -", 42);
}

#[test]
fn test_empty_tuple_operations() {
    expect_error("[] ~> +");
    expect_error("[] ~> -");
    expect_error("[] ~> *");
    expect_error("[] ~> /");
    expect_error("[] ~> %");
}