mod common;

use common::*;
use quiver::vm::Value;

#[test]
fn test_simple_single_parameter_function() {
    expect_function("#int { $ }");
    expect_int("double = #int { [$, 2] ~> * }; 5 ~> double", 10);
    expect_int("increment = #int { [$, 1] ~> + }; 10 ~> increment", 11);
}

#[test]
fn test_single_parameter_function_application() {
    expect_int("identity = #int { $ }; 42 ~> identity", 42);
    expect_int("negate = #int { [0, $] ~> - }; 5 ~> negate", -5);
    expect_int("square = #int { [$, $] ~> * }; 7 ~> square", 49);
}

#[test]
fn test_multi_parameter_function() {
    expect_int("add = #[int, int] { [$0, $1] ~> + }; [3, 4] ~> add", 7);
    expect_int("multiply = #[int, int] { [$0, $1] ~> * }; [5, 6] ~> multiply", 30);
    expect_int("subtract = #[int, int] { [$1, $0] ~> - }; [10, 3] ~> subtract", -7);
}

#[test]
fn test_multi_parameter_with_tuple_access() {
    expect_int("sum_all = #[int, int, int] { [$0, $1, $2] ~> + }; [1, 2, 3] ~> sum_all", 6);
    expect_int("first = #[int, int] { $0 }; [42, 99] ~> first", 42);
    expect_int("second = #[int, int] { $1 }; [42, 99] ~> second", 99);
}

#[test]
fn test_whole_parameter_access() {
    expect_tuple("echo = #[int, int] { $ }; [1, 2] ~> echo", vec![Value::Integer(1), Value::Integer(2)]);
    expect_int("tuple_sum = #[int, int] { $ ~> + }; [7, 8] ~> tuple_sum", 15);
}

#[test]
fn test_named_tuple_parameter() {
    expect_int("distance = #Point[x: int, y: int] { [[$.x, $.x] ~> *, [$.y, $.y] ~> *] ~> + }; Point[x: 3, y: 4] ~> distance", 25);
    expect_int("get_x = #Point[x: int, y: int] { $.x }; Point[x: 10, y: 20] ~> get_x", 10);
}

#[test]
fn test_no_parameter_function() {
    expect_int("get_constant = #{ 42 }; [] ~> get_constant", 42);
    expect_int("always_zero = #{ 0 }; [] ~> always_zero", 0);
}

#[test]
fn test_function_chaining() {
    expect_int("double = #int { [$, 2] ~> * }; increment = #int { [$, 1] ~> + }; 5 ~> double ~> increment", 11);
    expect_int("add_one = #int { [$, 1] ~> + }; multiply_two = #int { [$, 2] ~> * }; 3 ~> add_one ~> multiply_two", 8);
}

#[test]
fn test_ripple_operator_in_functions() {
    expect_tuple("duplicate = #int { [~, ~] }; 42 ~> duplicate", vec![Value::Integer(42), Value::Integer(42)]);
    expect_tuple("make_point = #int { Point[x: ~, y: ~] }; 5 ~> make_point", vec![Value::Integer(5), Value::Integer(5)]);
}

#[test]
fn test_ripple_in_pipelines() {
    expect_tuple("process = #int { [$, 2] ~> * ~> Point[x: ~, y: 0] }; 3 ~> process", vec![Value::Integer(6), Value::Integer(0)]);
    expect_int("complex = #int { [$, 1] ~> + ~> [$, ~] ~> * }; 4 ~> complex", 25);
}

#[test]
fn test_functions_as_values() {
    expect_function("f = #int { [$, 2] ~> * }; f");
    expect_int("f = #int { [$, 2] ~> * }; g = f; 5 ~> g", 10);
}

#[test]
fn test_higher_order_functions() {
    expect_int("apply = #[f: fn, x: int] { x ~> f }; double = #int { [$, 2] ~> * }; [f: double, x: 5] ~> apply", 10);
    expect_int("compose = #[f: fn, g: fn, x: int] { x ~> f ~> g }; add1 = #int { [$, 1] ~> + }; mul2 = #int { [$, 2] ~> * }; [f: add1, g: mul2, x: 3] ~> compose", 8);
}

#[test]
fn test_captured_variables() {
    expect_int("multiplier = 3; make_multiplier = #int { [$, multiplier] ~> * }; 4 ~> make_multiplier", 12);
    expect_int("base = 10; add_base = #int { [$, base] ~> + }; 5 ~> add_base", 15);
}

#[test]
fn test_basic_recursion() {
    expect_int("countdown = #int { [$, 0] ~> == => 0 | [$, 1] ~> - ~> & }; 5 ~> countdown", 0);
    expect_int("simple_recursive = #int { [$, 1] ~> <= => $ | 0 ~> & }; 3 ~> simple_recursive", 3);
}

#[test]
fn test_factorial_recursion() {
    expect_int("tail_factorial = #[int, int] { [$0, 1] ~> <= => $1 | [[$0, 1] ~> -, [$0, $1] ~> *] ~> & }; factorial = #int { [$, 1] ~> tail_factorial }; 5 ~> factorial", 120);
    expect_int("tail_factorial = #[int, int] { [$0, 1] ~> <= => $1 | [[$0, 1] ~> -, [$0, $1] ~> *] ~> & }; factorial = #int { [$, 1] ~> tail_factorial }; 0 ~> factorial", 1);
}

#[test]
fn test_fibonacci_recursion() {
    expect_int("tail_fib = #[int, int, int] { [$0, 0] ~> <= => $1 | [[$0, 1] ~> -, $2, [$1, $2] ~> +] ~> & }; fibonacci = #int { [$, 0, 1] ~> tail_fib }; 7 ~> fibonacci", 13);
    expect_int("tail_fib = #[int, int, int] { [$0, 0] ~> <= => $1 | [[$0, 1] ~> -, $2, [$1, $2] ~> +] ~> & }; fibonacci = #int { [$, 0, 1] ~> tail_fib }; 1 ~> fibonacci", 0);
}

#[test]
fn test_recursive_with_captured_variables() {
    expect_int("multiplier = 3; tail_recursive_mult = #[int, int] { [$0, 0] ~> == => $1 | [[$0, 1] ~> -, [$1, multiplier] ~> +] ~> & }; recursive_mult = #int { [$, 0] ~> tail_recursive_mult }; 4 ~> recursive_mult", 12);
}

#[test]
fn test_complex_function_expressions() {
    expect_int("process = #int { [$, 1] ~> + ~> #int { [$, 2] ~> * } }; 3 ~> process", 8);
}

#[test]
fn test_nested_function_calls() {
    expect_int("add = #[int, int] { [$0, $1] ~> + }; mul = #[int, int] { [$0, $1] ~> * }; [[2, 3] ~> add, [4, 5] ~> mul] ~> add", 25);
}

#[test]
fn test_function_with_tuple_construction() {
    expect_tuple("make_pair = #int { [$, [$, 1] ~> +] }; 5 ~> make_pair", vec![Value::Integer(5), Value::Integer(6)]);
    expect_tuple("coords = #[int, int] { Point[x: $0, y: $1] }; [10, 20] ~> coords", vec![Value::Integer(10), Value::Integer(20)]);
}

#[test]
fn test_conditional_functions() {
    expect_int("abs = #int { [$, 0] ~> > => $ | [$, 0] ~> < => [0, $] ~> - | 0 }; -5 ~> abs", 5);
    expect_int("abs = #int { [$, 0] ~> > => $ | [$, 0] ~> < => [0, $] ~> - | 0 }; 5 ~> abs", 5);
    expect_int("abs = #int { [$, 0] ~> > => $ | [$, 0] ~> < => [0, $] ~> - | 0 }; 0 ~> abs", 0);
}

#[test]
fn test_sign_function() {
    expect_int("sign = #int { [$, 0] ~> > => 1 | [$, 0] ~> < => -1 | 0 }; 42 ~> sign", 1);
    expect_int("sign = #int { [$, 0] ~> > => 1 | [$, 0] ~> < => -1 | 0 }; -42 ~> sign", -1);
    expect_int("sign = #int { [$, 0] ~> > => 1 | [$, 0] ~> < => -1 | 0 }; 0 ~> sign", 0);
}

#[test]
fn test_function_field_access() {
    expect_int("math = [add: #[int, int] { [$0, $1] ~> + }, multiply: #[int, int] { [$0, $1] ~> * }]; [3, 4] ~> math.add", 7);
    expect_int("ops = [double: #int { [$, 2] ~> * }]; 5 ~> ops.double", 10);
}

#[test]
fn test_parameterized_blocks() {
    expect_int("5 ~> { [$, 2] ~> * }", 10);
    expect_int("x = 7; x ~> { [$, 1] ~> + }", 8);
}

#[test]
fn test_union_type_parameter() {
    expect_int("process = #(Circle[radius: int], Rectangle[width: int, height: int]) { | Circle[radius: r] = $ => [r, r] ~> * | Rectangle[width: w, height: h] = $ => [w, h] ~> * }; Circle[radius: 5] ~> process", 25);
}

#[test]
fn test_function_errors() {
    expect_error("f = #int { $ }; [1, 2] ~> f");
    expect_error("f = #[int, int] { [$0, $1] ~> + }; 42 ~> f");
    expect_error("42 ~> &");
}

#[test]
fn test_recursive_edge_cases() {
    expect_error("non_recursive = #int { $ }; 5 ~> non_recursive ~> &");
    expect_int("immediate_return = #int { $ ~> & }; 42 ~> immediate_return", 42);
}

#[test]
fn test_complex_captured_recursion() {
    expect_int("step = 2; counter = #[int, int] { [$0, $1] ~> >= => $0 | [[$0, step] ~> +, $1] ~> & }; [0, 10] ~> counter", 10);
}