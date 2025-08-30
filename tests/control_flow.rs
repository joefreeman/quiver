mod common;

use common::*;

#[test]
fn test_truthiness_basics() {
    expect_nil("[]");
    expect_int("42", 42);
    expect_int("-1", -1);
    expect_int("0", 0);
}

#[test]
fn test_simple_comma_sequences() {
    expect_int("42, 100", 100);
    expect_int("1, 2, 3, 4", 4);
    expect_int("[], 42", 42);
}

#[test]
fn test_comma_sequence_short_circuit() {
    expect_nil("42, [], 100");
    expect_nil("1, 2, [], 4");
    expect_nil("[], 100");
}

#[test]
fn test_simple_pipe_alternatives() {
    expect_int("[] | 42", 42);
    expect_int("100 | 42", 100);
    expect_int("[], [] | 50", 50);
}

#[test]
fn test_multiple_pipe_alternatives() {
    expect_int("[] | [] | 42", 42);
    expect_int("100 | 200 | 300", 100);
    expect_int("[], [], [] | 99", 99);
}

#[test]
fn test_comma_pipe_combination() {
    expect_int("[], 42 | 100", 42);
    expect_int("[], [] | 50, 60", 60);
    expect_nil("[], [] | [], []");
}

#[test]
fn test_arrow_conditionals() {
    expect_int("5 => 42", 42);
    expect_int("[5, 3] ~> > => 100", 100);
    expect_nil("[] => 42");
    expect_nil("[3, 5] ~> > => 100");
}

#[test]
fn test_arrow_no_fallthrough() {
    expect_nil("5 => []");
    expect_nil("[5, 3] ~> > => []");
    expect_nil("42 => [], 100");
}

#[test]
fn test_condition_consequence_patterns() {
    expect_int("[5, 3] ~> > => 100 | 200", 100);
    expect_int("[3, 5] ~> > => 100 | 200", 200);
    expect_nil("[5, 3] ~> > => [] | 200");
}

#[test]
fn test_multiple_arrow_conditions() {
    expect_int("[10, 0] ~> > => 1 | [10, 0] ~> < => -1 | 0", 1);
    expect_int("[-5, 0] ~> > => 1 | [-5, 0] ~> < => -1 | 0", -1);
    expect_int("[0, 0] ~> > => 1 | [0, 0] ~> < => -1 | 0", 0);
}

#[test]
fn test_absolute_value_function() {
    expect_int("x = 5; [x, 0] ~> > => x | [x, 0] ~> < => [0, x] ~> - | 0", 5);
    expect_int("x = -5; [x, 0] ~> > => x | [x, 0] ~> < => [0, x] ~> - | 0", 5);
    expect_int("x = 0; [x, 0] ~> > => x | [x, 0] ~> < => [0, x] ~> - | 0", 0);
}

#[test]
fn test_mixed_condition_consequence() {
    expect_int("[5, 0] ~> >, 42 => 100 | 200", 100);
    expect_int("[0, 5] ~> >, 42 => 100 | 200", 200);
    expect_nil("[5, 0] ~> >, [] => 100 | 200");
}

#[test]
fn test_complex_conditional_chains() {
    expect_int("x = 10; [x, 5] ~> > => [x, 0] ~> > => 1 | -1 | [x, 15] ~> < => 2 | 0", 1);
    expect_int("x = -10; [x, 5] ~> > => [x, 0] ~> > => 1 | -1 | [x, 15] ~> < => 2 | 0", 2);
}

#[test]
fn test_nested_conditionals() {
    expect_int("x = 5; y = 3; [x, y] ~> > => ([x, 10] ~> > => 100 | 50) | 0", 50);
    expect_int("x = 15; y = 3; [x, y] ~> > => ([x, 10] ~> > => 100 | 50) | 0", 100);
    expect_int("x = 1; y = 3; [x, y] ~> > => ([x, 10] ~> > => 100 | 50) | 0", 0);
}

#[test]
fn test_function_with_conditionals() {
    expect_int("classify = #int { [$, 0] ~> > => \"positive\" | [$, 0] ~> < => \"negative\" | \"zero\" }; 5 ~> classify", 0);
    expect_int("classify = #int { [$, 0] ~> > => \"positive\" | [$, 0] ~> < => \"negative\" | \"zero\" }; -5 ~> classify", 0);
    expect_int("classify = #int { [$, 0] ~> > => \"positive\" | [$, 0] ~> < => \"negative\" | \"zero\" }; 0 ~> classify", 0);
}

#[test]
fn test_max_function() {
    expect_int("max = #[int, int] { [$0, $1] ~> >= => $0 | $1 }; [5, 3] ~> max", 5);
    expect_int("max = #[int, int] { [$0, $1] ~> >= => $0 | $1 }; [3, 5] ~> max", 5);
    expect_int("max = #[int, int] { [$0, $1] ~> >= => $0 | $1 }; [7, 7] ~> max", 7);
}

#[test]
fn test_min_function() {
    expect_int("min = #[int, int] { [$0, $1] ~> <= => $0 | $1 }; [5, 3] ~> min", 3);
    expect_int("min = #[int, int] { [$0, $1] ~> <= => $0 | $1 }; [3, 5] ~> min", 3);
    expect_int("min = #[int, int] { [$0, $1] ~> <= => $0 | $1 }; [7, 7] ~> min", 7);
}

#[test]
fn test_guard_patterns() {
    expect_int("x = 10; x => ([x, 5] ~> > => x | 0)", 10);
    expect_int("x = 3; x => ([x, 5] ~> > => x | 0)", 0);
    expect_nil("x = []; x => ([x, 5] ~> > => x | 0)");
}

#[test]
fn test_pattern_matching_with_conditionals() {
    expect_int("Point[x: a, y: b] = Point[x: 5, y: 3] => ([a, b] ~> > => a | b) | 0", 5);
    expect_int("Point[x: a, y: b] = Point[x: 2, y: 7] => ([a, b] ~> > => a | b) | 0", 7);
    expect_int("[a, b] = [] => a | 99", 99);
}

#[test]
fn test_short_circuit_evaluation() {
    expect_int("side_effect = 0; x = (side_effect = 1), 42; side_effect", 1);
    expect_int("side_effect = 0; x = [], (side_effect = 1); side_effect", 0);
}

#[test]
fn test_complex_expression_sequences() {
    expect_int("a = 5, b = 10, [a, b] ~> +", 15);
    expect_nil("a = 5, [], b = 10, [a, b] ~> +");
    expect_int("a = 5, b = 10 | a = 20, b = 30, [a, b] ~> +", 15);
}

#[test]
fn test_alternative_with_assignments() {
    expect_int("x = [] | x = 42; x", 42);
    expect_int("x = 10 | x = 42; x", 10);
}

#[test]
fn test_conditional_assignment() {
    expect_int("x = [5, 3] ~> > => 100 | 200; x", 100);
    expect_int("x = [3, 5] ~> > => 100 | 200; x", 200);
}

#[test]
fn test_recursive_conditionals() {
    expect_int("countdown = #int { [$, 0] ~> == => 0 | [$, 1] ~> - ~> & }; 3 ~> countdown", 0);
    expect_int("factorial_cond = #int { [$, 1] ~> <= => 1 | [$, [$, 1] ~> - ~> &] ~> * }; 4 ~> factorial_cond", 24);
}

#[test]
fn test_early_return_patterns() {
    expect_int("process = #int { [$, 0] ~> == => 99 | [$, 2] ~> * }; 0 ~> process", 99);
    expect_int("process = #int { [$, 0] ~> == => 99 | [$, 2] ~> * }; 5 ~> process", 10);
}

#[test]
fn test_fallthrough_vs_no_fallthrough() {
    expect_int("42, [] | 100", 100);
    expect_nil("42 => [] | 100");
    expect_int("42, 50 | 100", 50);
    expect_int("42 => 50 | 100", 50);
}

#[test]
fn test_complex_mixed_operators() {
    expect_int("x = 5; [x, 0] ~> >, [x, 10] ~> + => x | [x, 3] ~> < => 0 | x", 5);
    expect_int("x = -5; [x, 0] ~> >, [x, 10] ~> + => x | [x, 3] ~> < => 0 | x", 0);
    expect_int("x = 0; [x, 0] ~> >, [x, 10] ~> + => x | [x, 3] ~> < => 0 | x", 0);
}

#[test]
fn test_nested_arrow_sequences() {
    expect_int("x = 10; [x, 5] ~> > => ([x, 15] ~> < => 1 | 2) | 3", 1);
    expect_int("x = 20; [x, 5] ~> > => ([x, 15] ~> < => 1 | 2) | 3", 2);
    expect_int("x = 2; [x, 5] ~> > => ([x, 15] ~> < => 1 | 2) | 3", 3);
}

#[test]
fn test_error_propagation_in_conditionals() {
    expect_error("[5, 0] ~> / => 100 | 200");
    expect_int("[] => [5, 0] ~> / | 42", 42);
}