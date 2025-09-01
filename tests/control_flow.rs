mod common;
use common::*;

#[test]
fn test_truthiness_basics() {
    quiver().evaluate("[]").expect_nil();
    quiver().evaluate("42").expect_int(42);
    quiver().evaluate("-1").expect_int(-1);
    quiver().evaluate("0").expect_int(0);
}

#[test]
fn test_simple_comma_expressions() {
    quiver().evaluate("42, 100").expect_int(100);
    quiver().evaluate("1, 2, 3, 4").expect_int(4);
    quiver().evaluate("[], 42").expect_int(42);
}

#[test]
fn test_comma_sequence_short_circuit() {
    quiver().evaluate("42, [], 100").expect_nil();
    quiver().evaluate("1, 2, [], 4").expect_nil();
    quiver().evaluate("[], 100").expect_nil();
}

#[test]
fn test_simple_pipe_alternatives() {
    quiver().evaluate("[] | 42").expect_int(42);
    quiver().evaluate("100 | 42").expect_int(100);
    quiver().evaluate("[], [] | 50").expect_int(50);
}

#[test]
fn test_multiple_pipe_alternatives() {
    quiver().evaluate("[] | [] | 42").expect_int(42);
    quiver().evaluate("100 | 200 | 300").expect_int(100);
    quiver().evaluate("[], [], [] | 99").expect_int(99);
}

#[test]
fn test_comma_pipe_combination() {
    quiver().evaluate("[], 42 | 100").expect_int(42);
    quiver().evaluate("[], [] | 50, 60").expect_int(60);
    quiver().evaluate("[], [] | [], []").expect_nil();
}

#[test]
fn test_arrow_conditionals() {
    quiver().evaluate("5 => 42").expect_int(42);
    quiver().evaluate("[5, 3] ~> > => 100").expect_int(100);
    quiver().evaluate("[] => 42").expect_nil();
    quiver().evaluate("[3, 5] ~> > => 100").expect_nil();
}

#[test]
fn test_arrow_no_fallthrough() {
    quiver().evaluate("5 => []").expect_nil();
    quiver().evaluate("[5, 3] ~> > => []").expect_nil();
    quiver().evaluate("42 => [], 100").expect_nil();
}

#[test]
fn test_condition_consequence_patterns() {
    quiver()
        .evaluate("[5, 3] ~> > => 100 | 200")
        .expect_int(100);
    quiver()
        .evaluate("[3, 5] ~> > => 100 | 200")
        .expect_int(200);
    quiver().evaluate("[5, 3] ~> > => [] | 200").expect_nil();
}

#[test]
fn test_multiple_arrow_conditions() {
    quiver()
        .evaluate("[10, 0] ~> > => 1 | [10, 0] ~> < => -1 | 0")
        .expect_int(1);
    quiver()
        .evaluate("[-5, 0] ~> > => 1 | [-5, 0] ~> < => -1 | 0")
        .expect_int(-1);
    quiver()
        .evaluate("[0, 0] ~> > => 1 | [0, 0] ~> < => -1 | 0")
        .expect_int(0);
}

#[test]
fn test_absolute_value_function() {
    quiver()
        .evaluate("x = 5; [x, 0] ~> > => x | [x, 0] ~> < => [0, x] ~> - | 0")
        .expect_int(5);
    quiver()
        .evaluate("x = -5; [x, 0] ~> > => x | [x, 0] ~> < => [0, x] ~> - | 0")
        .expect_int(5);
    quiver()
        .evaluate("x = 0; [x, 0] ~> > => x | [x, 0] ~> < => [0, x] ~> - | 0")
        .expect_int(0);
}

#[test]
fn test_mixed_condition_consequence() {
    quiver()
        .evaluate("[5, 0] ~> >, 42 => 100 | 200")
        .expect_int(100);
    quiver()
        .evaluate("[0, 5] ~> >, 42 => 100 | 200")
        .expect_int(200);
    quiver()
        .evaluate("[5, 0] ~> >, [] => 100 | 200")
        .expect_nil();
}

#[test]
fn test_complex_conditional_chains() {
    quiver()
        .evaluate("x = 10; [x, 5] ~> > => [x, 0] ~> > => 1 | -1 | [x, 15] ~> < => 2 | 0")
        .expect_int(1);
    quiver()
        .evaluate("x = -10; [x, 5] ~> > => [x, 0] ~> > => 1 | -1 | [x, 15] ~> < => 2 | 0")
        .expect_int(2);
}

#[test]
fn test_nested_conditionals() {
    quiver()
        .evaluate("x = 5; y = 3; [x, y] ~> > => ([x, 10] ~> > => 100 | 50) | 0")
        .expect_int(50);
    quiver()
        .evaluate("x = 15; y = 3; [x, y] ~> > => ([x, 10] ~> > => 100 | 50) | 0")
        .expect_int(100);
    quiver()
        .evaluate("x = 1; y = 3; [x, y] ~> > => ([x, 10] ~> > => 100 | 50) | 0")
        .expect_int(0);
}

#[test]
fn test_function_with_conditionals() {
    quiver().evaluate("classify = #int { [$, 0] ~> > => \"positive\" | [$, 0] ~> < => \"negative\" | \"zero\" }; 5 ~> classify").expect_int(0);
    quiver().evaluate("classify = #int { [$, 0] ~> > => \"positive\" | [$, 0] ~> < => \"negative\" | \"zero\" }; -5 ~> classify").expect_int(0);
    quiver().evaluate("classify = #int { [$, 0] ~> > => \"positive\" | [$, 0] ~> < => \"negative\" | \"zero\" }; 0 ~> classify").expect_int(0);
}

#[test]
fn test_max_function() {
    quiver()
        .evaluate("max = #[int, int] { [$0, $1] ~> >= => $0 | $1 }; [5, 3] ~> max")
        .expect_int(5);
    quiver()
        .evaluate("max = #[int, int] { [$0, $1] ~> >= => $0 | $1 }; [3, 5] ~> max")
        .expect_int(5);
    quiver()
        .evaluate("max = #[int, int] { [$0, $1] ~> >= => $0 | $1 }; [7, 7] ~> max")
        .expect_int(7);
}

#[test]
fn test_min_function() {
    quiver()
        .evaluate("min = #[int, int] { [$0, $1] ~> <= => $0 | $1 }; [5, 3] ~> min")
        .expect_int(3);
    quiver()
        .evaluate("min = #[int, int] { [$0, $1] ~> <= => $0 | $1 }; [3, 5] ~> min")
        .expect_int(3);
    quiver()
        .evaluate("min = #[int, int] { [$0, $1] ~> <= => $0 | $1 }; [7, 7] ~> min")
        .expect_int(7);
}

#[test]
fn test_guard_patterns() {
    quiver()
        .evaluate("x = 10; x => ([x, 5] ~> > => x | 0)")
        .expect_int(10);
    quiver()
        .evaluate("x = 3; x => ([x, 5] ~> > => x | 0)")
        .expect_int(0);
    quiver()
        .evaluate("x = []; x => ([x, 5] ~> > => x | 0)")
        .expect_nil();
}

#[test]
fn test_pattern_matching_with_conditionals() {
    quiver()
        .evaluate("Point[x: a, y: b] = Point[x: 5, y: 3] => ([a, b] ~> > => a | b) | 0")
        .expect_int(5);
    quiver()
        .evaluate("Point[x: a, y: b] = Point[x: 2, y: 7] => ([a, b] ~> > => a | b) | 0")
        .expect_int(7);
    quiver().evaluate("[a, b] = [] => a | 99").expect_int(99);
}

#[test]
fn test_short_circuit_evaluation() {
    quiver()
        .evaluate("side_effect = 0; x = (side_effect = 1), 42; side_effect")
        .expect_int(1);
    quiver()
        .evaluate("side_effect = 0; x = [], (side_effect = 1); side_effect")
        .expect_int(0);
}

#[test]
fn test_complex_expression_sequences() {
    quiver()
        .evaluate("a = 5, b = 10, [a, b] ~> +")
        .expect_int(15);
    quiver()
        .evaluate("a = 5, [], b = 10, [a, b] ~> +")
        .expect_nil();
    quiver()
        .evaluate("a = 5, b = 10 | a = 20, b = 30, [a, b] ~> +")
        .expect_int(15);
}

#[test]
fn test_alternative_with_assignments() {
    quiver().evaluate("x = [] | x = 42; x").expect_int(42);
    quiver().evaluate("x = 10 | x = 42; x").expect_int(10);
}

#[test]
fn test_conditional_assignment() {
    quiver()
        .evaluate("x = [5, 3] ~> > => 100 | 200; x")
        .expect_int(100);
    quiver()
        .evaluate("x = [3, 5] ~> > => 100 | 200; x")
        .expect_int(200);
}

#[test]
fn test_recursive_conditionals() {
    quiver()
        .evaluate("countdown = #int { [$, 0] ~> == => 0 | [$, 1] ~> - ~> & }; 3 ~> countdown")
        .expect_int(0);
    quiver().evaluate("factorial_cond = #int { [$, 1] ~> <= => 1 | [$, [$, 1] ~> - ~> &] ~> * }; 4 ~> factorial_cond").expect_int(24);
}

#[test]
fn test_early_return_patterns() {
    quiver()
        .evaluate("process = #int { [$, 0] ~> == => 99 | [$, 2] ~> * }; 0 ~> process")
        .expect_int(99);
    quiver()
        .evaluate("process = #int { [$, 0] ~> == => 99 | [$, 2] ~> * }; 5 ~> process")
        .expect_int(10);
}

#[test]
fn test_fallthrough_vs_no_fallthrough() {
    quiver().evaluate("42, [] | 100").expect_int(100);
    quiver().evaluate("42 => [] | 100").expect_nil();
    quiver().evaluate("42, 50 | 100").expect_int(50);
    quiver().evaluate("42 => 50 | 100").expect_int(50);
}

#[test]
fn test_complex_mixed_operators() {
    quiver()
        .evaluate("x = 5; [x, 0] ~> >, [x, 10] ~> + => x | [x, 3] ~> < => 0 | x")
        .expect_int(5);
    quiver()
        .evaluate("x = -5; [x, 0] ~> >, [x, 10] ~> + => x | [x, 3] ~> < => 0 | x")
        .expect_int(0);
    quiver()
        .evaluate("x = 0; [x, 0] ~> >, [x, 10] ~> + => x | [x, 3] ~> < => 0 | x")
        .expect_int(0);
}

#[test]
fn test_nested_arrow_sequences() {
    quiver()
        .evaluate("x = 10; [x, 5] ~> > => ([x, 15] ~> < => 1 | 2) | 3")
        .expect_int(1);
    quiver()
        .evaluate("x = 20; [x, 5] ~> > => ([x, 15] ~> < => 1 | 2) | 3")
        .expect_int(2);
    quiver()
        .evaluate("x = 2; [x, 5] ~> > => ([x, 15] ~> < => 1 | 2) | 3")
        .expect_int(3);
}

#[test]
fn test_error_propagation_in_conditionals() {
    quiver().evaluate("[5, 0] ~> / => 100 | 200").expect_error();
    quiver().evaluate("[] => [5, 0] ~> / | 42").expect_int(42);
}

#[test]
fn test_block_parameter_access_across_branches() {
    quiver()
        .evaluate("A[1] ~> { A[a] = $ => 100 | B[b] = $ => 200 }")
        .expect_int(100);
    quiver()
        .evaluate("B[1] ~> { A[a] = $ => 100 | B[b] = $ => 200 }")
        .expect_int(200);
    quiver()
        .evaluate("C[1] ~> { A[a] = $ => 100 | B[b] = $ => 200 }")
        .expect_nil();
}
