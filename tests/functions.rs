mod common;
use common::*;
use quiver::vm::Value;

#[test]
fn test_simple_single_parameter_function() {
    quiver().evaluate("#int { $ }").expect_function();
    quiver()
        .evaluate("double = #int { [$, 2] ~> * }; 5 ~> double")
        .expect_int(10);
    quiver()
        .evaluate("increment = #int { [$, 1] ~> + }; 10 ~> increment")
        .expect_int(11);
}

#[test]
fn test_single_parameter_function_application() {
    quiver()
        .evaluate("identity = #int { $ }; 42 ~> identity")
        .expect_int(42);
    quiver()
        .evaluate("negate = #int { [0, $] ~> - }; 5 ~> negate")
        .expect_int(-5);
    quiver()
        .evaluate("square = #int { [$, $] ~> * }; 7 ~> square")
        .expect_int(49);
}

#[test]
fn test_multi_parameter_function() {
    quiver()
        .evaluate("add = #[int, int] { [$0, $1] ~> + }; [3, 4] ~> add")
        .expect_int(7);
    quiver()
        .evaluate("multiply = #[int, int] { [$0, $1] ~> * }; [5, 6] ~> multiply")
        .expect_int(30);
    quiver()
        .evaluate("subtract = #[int, int] { [$1, $0] ~> - }; [10, 3] ~> subtract")
        .expect_int(-7);
}

#[test]
fn test_multi_parameter_with_tuple_access() {
    quiver()
        .evaluate("sum_all = #[int, int, int] { [$0, $1, $2] ~> + }; [1, 2, 3] ~> sum_all")
        .expect_int(6);
    quiver()
        .evaluate("first = #[int, int] { $0 }; [42, 99] ~> first")
        .expect_int(42);
    quiver()
        .evaluate("second = #[int, int] { $1 }; [42, 99] ~> second")
        .expect_int(99);
}

#[test]
fn test_whole_parameter_access() {
    quiver()
        .evaluate("echo = #[int, int] { $ }; [1, 2] ~> echo")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
    quiver()
        .evaluate("tuple_sum = #[int, int] { $ ~> + }; [7, 8] ~> tuple_sum")
        .expect_int(15);
}

#[test]
fn test_named_tuple_parameter() {
    quiver().evaluate("distance = #Point[x: int, y: int] { [[$.x, $.x] ~> *, [$.y, $.y] ~> *] ~> + }; Point[x: 3, y: 4] ~> distance").expect_int(25);
    quiver()
        .evaluate("get_x = #Point[x: int, y: int] { $.x }; Point[x: 10, y: 20] ~> get_x")
        .expect_int(10);
}

#[test]
fn test_no_parameter_function() {
    quiver()
        .evaluate("get_constant = #{ 42 }; [] ~> get_constant")
        .expect_int(42);
    quiver()
        .evaluate("always_zero = #{ 0 }; [] ~> always_zero")
        .expect_int(0);
}

#[test]
fn test_function_chaining() {
    quiver().evaluate("double = #int { [$, 2] ~> * }; increment = #int { [$, 1] ~> + }; 5 ~> double ~> increment").expect_int(11);
    quiver().evaluate("add_one = #int { [$, 1] ~> + }; multiply_two = #int { [$, 2] ~> * }; 3 ~> add_one ~> multiply_two").expect_int(8);
}

#[test]
fn test_ripple_operator_in_functions() {
    quiver()
        .evaluate("duplicate = #int { [~, ~] }; 42 ~> duplicate")
        .expect_tuple(vec![Value::Integer(42), Value::Integer(42)]);
    quiver()
        .evaluate("make_point = #int { Point[x: ~, y: ~] }; 5 ~> make_point")
        .expect_tuple(vec![Value::Integer(5), Value::Integer(5)]);
}

#[test]
fn test_ripple_in_pipelines() {
    quiver()
        .evaluate("process = #int { [$, 2] ~> * ~> Point[x: ~, y: 0] }; 3 ~> process")
        .expect_tuple(vec![Value::Integer(6), Value::Integer(0)]);
    quiver()
        .evaluate("complex = #int { [$, 1] ~> + ~> [$, ~] ~> * }; 4 ~> complex")
        .expect_int(25);
}

#[test]
fn test_functions_as_values() {
    quiver()
        .evaluate("f = #int { [$, 2] ~> * }; f")
        .expect_function();
    quiver()
        .evaluate("f = #int { [$, 2] ~> * }; g = f; 5 ~> g")
        .expect_int(10);
}

#[test]
fn test_higher_order_functions() {
    quiver().evaluate("apply = #[f: fn, x: int] { x ~> f }; double = #int { [$, 2] ~> * }; [f: double, x: 5] ~> apply").expect_int(10);
    quiver().evaluate("compose = #[f: fn, g: fn, x: int] { x ~> f ~> g }; add1 = #int { [$, 1] ~> + }; mul2 = #int { [$, 2] ~> * }; [f: add1, g: mul2, x: 3] ~> compose").expect_int(8);
}

#[test]
fn test_captured_variables() {
    quiver()
        .evaluate(
            "multiplier = 3; make_multiplier = #int { [$, multiplier] ~> * }; 4 ~> make_multiplier",
        )
        .expect_int(12);
    quiver()
        .evaluate("base = 10; add_base = #int { [$, base] ~> + }; 5 ~> add_base")
        .expect_int(15);
}

#[test]
fn test_basic_recursion() {
    quiver()
        .evaluate("countdown = #int { [$, 0] ~> == => 0 | [$, 1] ~> - ~> & }; 5 ~> countdown")
        .expect_int(0);
    quiver()
        .evaluate("simple_recursive = #int { [$, 1] ~> <= => $ | 0 ~> & }; 3 ~> simple_recursive")
        .expect_int(3);
}

#[test]
fn test_factorial_recursion() {
    quiver().evaluate("tail_factorial = #[int, int] { [$0, 1] ~> <= => $1 | [[$0, 1] ~> -, [$0, $1] ~> *] ~> & }; factorial = #int { [$, 1] ~> tail_factorial }; 5 ~> factorial").expect_int(120);
    quiver().evaluate("tail_factorial = #[int, int] { [$0, 1] ~> <= => $1 | [[$0, 1] ~> -, [$0, $1] ~> *] ~> & }; factorial = #int { [$, 1] ~> tail_factorial }; 0 ~> factorial").expect_int(1);
}

#[test]
fn test_fibonacci_recursion() {
    quiver().evaluate("tail_fib = #[int, int, int] { [$0, 0] ~> <= => $1 | [[$0, 1] ~> -, $2, [$1, $2] ~> +] ~> & }; fibonacci = #int { [$, 0, 1] ~> tail_fib }; 7 ~> fibonacci").expect_int(13);
    quiver().evaluate("tail_fib = #[int, int, int] { [$0, 0] ~> <= => $1 | [[$0, 1] ~> -, $2, [$1, $2] ~> +] ~> & }; fibonacci = #int { [$, 0, 1] ~> tail_fib }; 1 ~> fibonacci").expect_int(0);
}

#[test]
fn test_recursive_with_captured_variables() {
    quiver().evaluate("multiplier = 3; tail_recursive_mult = #[int, int] { [$0, 0] ~> == => $1 | [[$0, 1] ~> -, [$1, multiplier] ~> +] ~> & }; recursive_mult = #int { [$, 0] ~> tail_recursive_mult }; 4 ~> recursive_mult").expect_int(12);
}

#[test]
fn test_complex_function_expressions() {
    quiver()
        .evaluate("process = #int { [$, 1] ~> + ~> #int { [$, 2] ~> * } }; 3 ~> process")
        .expect_int(8);
}

#[test]
fn test_nested_function_calls() {
    quiver().evaluate("add = #[int, int] { [$0, $1] ~> + }; mul = #[int, int] { [$0, $1] ~> * }; [[2, 3] ~> add, [4, 5] ~> mul] ~> add").expect_int(25);
}

#[test]
fn test_function_with_tuple_construction() {
    quiver()
        .evaluate("make_pair = #int { [$, [$, 1] ~> +] }; 5 ~> make_pair")
        .expect_tuple(vec![Value::Integer(5), Value::Integer(6)]);
    quiver()
        .evaluate("coords = #[int, int] { Point[x: $0, y: $1] }; [10, 20] ~> coords")
        .expect_tuple(vec![Value::Integer(10), Value::Integer(20)]);
}

#[test]
fn test_conditional_functions() {
    quiver()
        .evaluate("abs = #int { [$, 0] ~> > => $ | [$, 0] ~> < => [0, $] ~> - | 0 }; -5 ~> abs")
        .expect_int(5);
    quiver()
        .evaluate("abs = #int { [$, 0] ~> > => $ | [$, 0] ~> < => [0, $] ~> - | 0 }; 5 ~> abs")
        .expect_int(5);
    quiver()
        .evaluate("abs = #int { [$, 0] ~> > => $ | [$, 0] ~> < => [0, $] ~> - | 0 }; 0 ~> abs")
        .expect_int(0);
}

#[test]
fn test_sign_function() {
    quiver()
        .evaluate("sign = #int { [$, 0] ~> > => 1 | [$, 0] ~> < => -1 | 0 }; 42 ~> sign")
        .expect_int(1);
    quiver()
        .evaluate("sign = #int { [$, 0] ~> > => 1 | [$, 0] ~> < => -1 | 0 }; -42 ~> sign")
        .expect_int(-1);
    quiver()
        .evaluate("sign = #int { [$, 0] ~> > => 1 | [$, 0] ~> < => -1 | 0 }; 0 ~> sign")
        .expect_int(0);
}

#[test]
fn test_function_field_access() {
    quiver().evaluate("math = [add: #[int, int] { [$0, $1] ~> + }, multiply: #[int, int] { [$0, $1] ~> * }]; [3, 4] ~> math.add").expect_int(7);
    quiver()
        .evaluate("ops = [double: #int { [$, 2] ~> * }]; 5 ~> ops.double")
        .expect_int(10);
}

#[test]
fn test_parameterized_blocks() {
    quiver().evaluate("5 ~> { [$, 2] ~> * }").expect_int(10);
    quiver()
        .evaluate("x = 7; x ~> { [$, 1] ~> + }")
        .expect_int(8);
}

#[test]
fn test_union_type_parameter() {
    quiver().evaluate("process = #(Circle[radius: int], Rectangle[width: int, height: int]) { | Circle[radius: r] = $ => [r, r] ~> * | Rectangle[width: w, height: h] = $ => [w, h] ~> * }; Circle[radius: 5] ~> process").expect_int(25);
}

#[test]
fn test_function_errors() {
    quiver()
        .evaluate("f = #int { $ }; [1, 2] ~> f")
        .expect_error();
    quiver()
        .evaluate("f = #[int, int] { [$0, $1] ~> + }; 42 ~> f")
        .expect_error();
    quiver().evaluate("42 ~> &").expect_error();
}

#[test]
fn test_recursive_edge_cases() {
    quiver()
        .evaluate("non_recursive = #int { $ }; 5 ~> non_recursive ~> &")
        .expect_error();
    quiver()
        .evaluate("immediate_return = #int { $ ~> & }; 42 ~> immediate_return")
        .expect_int(42);
}

#[test]
fn test_complex_captured_recursion() {
    quiver().evaluate("step = 2; counter = #[int, int] { [$0, $1] ~> >= => $0 | [[$0, step] ~> +, $1] ~> & }; [0, 10] ~> counter").expect_int(10);
}
