mod common;
use common::*;

#[test]
fn test_addition() {
    quiver().evaluate("[3, 4] ~> +").expect_int(7);
    quiver().evaluate("[10, 5] ~> +").expect_int(15);
    quiver().evaluate("[0, 0] ~> +").expect_int(0);
    quiver().evaluate("[-5, 3] ~> +").expect_int(-2);
    quiver().evaluate("[-10, -20] ~> +").expect_int(-30);
}

#[test]
fn test_subtraction() {
    quiver().evaluate("[10, 3] ~> -").expect_int(7);
    quiver().evaluate("[5, 10] ~> -").expect_int(-5);
    quiver().evaluate("[0, 0] ~> -").expect_int(0);
    quiver().evaluate("[-5, -3] ~> -").expect_int(-2);
    quiver().evaluate("[100, 1] ~> -").expect_int(99);
}

#[test]
fn test_multiplication() {
    quiver().evaluate("[5, 6] ~> *").expect_int(30);
    quiver().evaluate("[0, 999] ~> *").expect_int(0);
    quiver().evaluate("[1, 42] ~> *").expect_int(42);
    quiver().evaluate("[-3, 4] ~> *").expect_int(-12);
    quiver().evaluate("[-5, -6] ~> *").expect_int(30);
}

#[test]
fn test_division() {
    quiver().evaluate("[15, 3] ~> /").expect_int(5);
    quiver().evaluate("[100, 10] ~> /").expect_int(10);
    quiver().evaluate("[7, 2] ~> /").expect_int(3);
    quiver().evaluate("[-15, 3] ~> /").expect_int(-5);
    quiver().evaluate("[-15, -3] ~> /").expect_int(5);
}

#[test]
fn test_modulo() {
    quiver().evaluate("[17, 5] ~> %").expect_int(2);
    quiver().evaluate("[10, 3] ~> %").expect_int(1);
    quiver().evaluate("[20, 4] ~> %").expect_int(0);
    quiver().evaluate("[7, 7] ~> %").expect_int(0);
    quiver().evaluate("[-17, 5] ~> %").expect_int(-2);
}

#[test]
fn test_equality() {
    quiver().evaluate("[5, 5] ~> ==").expect_int(5);
    quiver().evaluate("[42, 42] ~> ==").expect_int(42);
    quiver().evaluate("[0, 0] ~> ==").expect_int(0);
    quiver().evaluate("[-5, -5] ~> ==").expect_int(-5);
    quiver().evaluate("[5, 3] ~> ==").expect_nil();
    quiver().evaluate("[0, 1] ~> ==").expect_nil();
    quiver().evaluate("[-5, 5] ~> ==").expect_nil();
}

#[test]
fn test_not_equal() {
    quiver().evaluate("[5, 3] ~> !=").expect_int(5);
    quiver().evaluate("[0, 1] ~> !=").expect_int(0);
    quiver().evaluate("[-5, 5] ~> !=").expect_int(-5);
    quiver().evaluate("[5, 5] ~> !=").expect_nil();
    quiver().evaluate("[42, 42] ~> !=").expect_nil();
    quiver().evaluate("[0, 0] ~> !=").expect_nil();
}

#[test]
fn test_less_than() {
    quiver().evaluate("[3, 5] ~> <").expect_int(5);
    quiver().evaluate("[-10, 0] ~> <").expect_int(0);
    quiver().evaluate("[-20, -10] ~> <").expect_int(-10);
    quiver().evaluate("[5, 3] ~> <").expect_nil();
    quiver().evaluate("[5, 5] ~> <").expect_nil();
    quiver().evaluate("[10, -5] ~> <").expect_nil();
}

#[test]
fn test_less_than_or_equal() {
    quiver().evaluate("[3, 5] ~> <=").expect_int(5);
    quiver().evaluate("[5, 5] ~> <=").expect_int(5);
    quiver().evaluate("[-10, 0] ~> <=").expect_int(0);
    quiver().evaluate("[5, 3] ~> <=").expect_nil();
    quiver().evaluate("[10, -5] ~> <=").expect_nil();
}

#[test]
fn test_greater_than() {
    quiver().evaluate("[5, 3] ~> >").expect_int(3);
    quiver().evaluate("[0, -10] ~> >").expect_int(-10);
    quiver().evaluate("[-10, -20] ~> >").expect_int(-20);
    quiver().evaluate("[3, 5] ~> >").expect_nil();
    quiver().evaluate("[5, 5] ~> >").expect_nil();
    quiver().evaluate("[-5, 10] ~> >").expect_nil();
}

#[test]
fn test_greater_than_or_equal() {
    quiver().evaluate("[5, 3] ~> >=").expect_int(3);
    quiver().evaluate("[5, 5] ~> >=").expect_int(5);
    quiver().evaluate("[0, -10] ~> >=").expect_int(-10);
    quiver().evaluate("[3, 5] ~> >=").expect_nil();
    quiver().evaluate("[-5, 10] ~> >=").expect_nil();
}

#[test]
fn test_multi_arity_addition() {
    quiver().evaluate("[1, 2, 3, 4] ~> +").expect_int(10);
    quiver().evaluate("[5, 10, 15] ~> +").expect_int(30);
    quiver().evaluate("[100] ~> +").expect_int(100);
    quiver().evaluate("[-1, 1, -1, 1] ~> +").expect_int(0);
}

#[test]
fn test_multi_arity_multiplication() {
    quiver().evaluate("[2, 3, 4] ~> *").expect_int(24);
    quiver().evaluate("[1, 2, 3, 4, 5] ~> *").expect_int(120);
    quiver().evaluate("[10] ~> *").expect_int(10);
    quiver().evaluate("[0, 100, 200] ~> *").expect_int(0);
}

#[test]
fn test_multi_arity_subtraction() {
    quiver().evaluate("[100, 10, 5, 3] ~> -").expect_int(82);
    quiver().evaluate("[0, 1, 2, 3] ~> -").expect_int(-6);
    quiver().evaluate("[50] ~> -").expect_int(50);
}

#[test]
fn test_nested_operations() {
    quiver().evaluate("[[2, 3] ~> +, 4] ~> *").expect_int(20);
    quiver()
        .evaluate("[[10, 2] ~> /, [3, 1] ~> +] ~> +")
        .expect_int(9);
    quiver()
        .evaluate("[[[5, 3] ~> -, 2] ~> *, 10] ~> +")
        .expect_int(14);
}

#[test]
fn test_operator_precedence_with_tuples() {
    quiver()
        .evaluate("[[1, 2] ~> +, [3, 4] ~> *] ~> +")
        .expect_int(15);
    quiver()
        .evaluate("[[10, 5] ~> -, [2, 3] ~> +] ~> *")
        .expect_int(25);
}

#[test]
fn test_comparison_with_tuples() {
    quiver().evaluate("[[5, 3] ~> +, 8] ~> ==").expect_int(8);
    quiver().evaluate("[[5, 3] ~> +, 7] ~> ==").expect_nil();
    quiver()
        .evaluate("[[10, 3] ~> -, [2, 3] ~> +] ~> >")
        .expect_int(5);
}

#[test]
fn test_zero_division_edge_cases() {
    quiver().evaluate("[5, 0] ~> /").expect_error();
    quiver().evaluate("[0, 0] ~> /").expect_error();
    quiver().evaluate("[-10, 0] ~> /").expect_error();
}

#[test]
fn test_zero_modulo_edge_cases() {
    quiver().evaluate("[5, 0] ~> %").expect_error();
    quiver().evaluate("[0, 0] ~> %").expect_error();
    quiver().evaluate("[-10, 0] ~> %").expect_error();
}

#[test]
fn test_large_number_operations() {
    quiver()
        .evaluate("[9223372036854775806, 1] ~> +")
        .expect_int(i64::MAX);
    quiver()
        .evaluate("[-9223372036854775807, -1] ~> +")
        .expect_int(i64::MIN);
}

#[test]
fn test_overflow_handling() {
    quiver()
        .evaluate("[9223372036854775807, 1] ~> +")
        .expect_error();
    quiver()
        .evaluate("[-9223372036854775808, -1] ~> +")
        .expect_error();
    quiver()
        .evaluate("[9223372036854775807, 2] ~> *")
        .expect_error();
}

#[test]
fn test_single_element_operations() {
    quiver().evaluate("[42] ~> +").expect_int(42);
    quiver().evaluate("[42] ~> *").expect_int(42);
    quiver().evaluate("[42] ~> -").expect_int(42);
}

#[test]
fn test_empty_tuple_operations() {
    quiver().evaluate("[] ~> +").expect_error();
    quiver().evaluate("[] ~> -").expect_error();
    quiver().evaluate("[] ~> *").expect_error();
    quiver().evaluate("[] ~> /").expect_error();
    quiver().evaluate("[] ~> %").expect_error();
}
