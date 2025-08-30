mod common;

use common::*;
use quiver::vm::Value;

#[test]
fn test_integer_type() {
    expect_int("42", 42);
    expect_int("-17", -17);
    expect_int("0", 0);
}

#[test]
fn test_binary_type() {
    expect_binary("'deadbeef'", 0);
    expect_binary("'0123456789abcdef'", 1);
    expect_binary("'ff'", 2);
}

#[test]
fn test_string_type() {
    expect_binary("\"hello world\"", 0);
    expect_binary("\"\"", 1);
    expect_binary("\"with spaces and symbols!\"", 2);
}

#[test]
fn test_tuple_type() {
    expect_tuple("[1, 2, 3]", vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
    expect_tuple("[]", vec![]);
    expect_tuple("[42]", vec![Value::Integer(42)]);
}

#[test]
fn test_named_tuple_type() {
    expect_tuple("Point[x: 10, y: 20]", vec![Value::Integer(10), Value::Integer(20)]);
    expect_tuple("Person[name: \"Alice\", age: 30]", vec![Value::Binary(0), Value::Integer(30)]);
}

#[test]
fn test_nil_type() {
    expect_nil("[]");
    expect_nil("None[]");
}

#[test]
fn test_ok_type() {
    expect_ok("Ok[]");
}

#[test]
fn test_function_type() {
    expect_function("#int { $ }");
    expect_function("#[int, int] { [$0, $1] ~> + }");
    expect_function("#{ 42 }");
}

#[test]
fn test_type_inference_with_operations() {
    expect_int("[5, 3] ~> +", 8);
    expect_tuple("[1, 2]", vec![Value::Integer(1), Value::Integer(2)]);
    expect_int("[10, 20].0", 10);
}

#[test]
fn test_nested_type_structures() {
    expect_tuple("[[1, 2], [3, 4]]", vec![
        Value::Tuple(quiver::bytecode::TypeId::NIL, vec![Value::Integer(1), Value::Integer(2)]),
        Value::Tuple(quiver::bytecode::TypeId::NIL, vec![Value::Integer(3), Value::Integer(4)])
    ]);
}

#[test]
fn test_function_parameter_types() {
    expect_int("f = #int { [$, 2] ~> * }; 5 ~> f", 10);
    expect_int("add = #[int, int] { [$0, $1] ~> + }; [3, 4] ~> add", 7);
    expect_tuple("make_point = #[int, int] { Point[x: $0, y: $1] }; [1, 2] ~> make_point", 
                vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_union_type_patterns() {
    expect_int("process = #(int, Point[x: int, y: int]) { | x: int = $ => x | Point[x: px, y: py] = $ => [px, py] ~> + }; 42 ~> process", 42);
}

#[test]
fn test_generic_type_functions() {
    expect_function("identity = #any { $ }; identity");
    expect_int("identity = #any { $ }; 42 ~> identity", 42);
    expect_tuple("identity = #any { $ }; [1, 2] ~> identity", vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_type_coercion() {
    expect_int("x = 42; x", 42);
    expect_tuple("t = [1, 2]; t", vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_type_checking_with_pattern_matching() {
    expect_int("Point[x: a, y: b] = Point[x: 5, y: 10] => [a, b] ~> + | 0", 15);
    expect_int("[a, b] = [10, 20] => [a, b] ~> * | 0", 200);
}

#[test]
fn test_typed_destructuring() {
    expect_int("Point[x: px, y: py] = Point[x: 3, y: 4]; px", 3);
    expect_int("[first, second] = [100, 200]; second", 200);
}

#[test]
fn test_function_type_constraints() {
    expect_int("square = #int { [$, $] ~> * }; 7 ~> square", 49);
    expect_int("concat_lengths = #[string, string] { 10 }; [\"hello\", \"world\"] ~> concat_lengths", 10);
}

#[test]
fn test_recursive_type_definitions() {
    expect_int("ListNode[value: int, next: ListNode] = ListNode[value: 1, next: ListNode[value: 2, next: None[]]]; ListNode.value", 1);
}

#[test]
fn test_algebraic_data_types() {
    expect_tuple("Success[42]", vec![Value::Integer(42)]);
    expect_tuple("Error[\"not found\"]", vec![Value::Binary(0)]);
    expect_nil("None[]");
}

#[test]
fn test_tagged_union_types() {
    expect_int("handle_result = #(Success[int], Error[string]) { | Success[value] = $ => value | Error[msg] = $ => -1 }; Success[100] ~> handle_result", 100);
}

#[test]
fn test_type_guards_in_functions() {
    expect_int("safe_div = #[int, int] { [$1, 0] ~> != => [$0, $1] ~> / | 0 }; [10, 2] ~> safe_div", 5);
    expect_int("safe_div = #[int, int] { [$1, 0] ~> != => [$0, $1] ~> / | 0 }; [10, 0] ~> safe_div", 0);
}

#[test]
fn test_higher_kinded_types() {
    expect_function("Container[int] = #int { Container[value: $] }; Container[int]");
    expect_tuple("Container[int] = #int { Container[value: $] }; 42 ~> Container[int]", vec![Value::Integer(42)]);
}

#[test]
fn test_type_aliases() {
    expect_int("UserId = int; user_id = 123; user_id", 123);
    expect_tuple("Coordinates = Point[x: int, y: int]; pos = Point[x: 5, y: 10]; pos", vec![Value::Integer(5), Value::Integer(10)]);
}

#[test]
fn test_dependent_types() {
    expect_int("Vector[n: int] = #int { [n, $] }; size = 3; [size, 42] ~> Vector[n: size]", 42);
}

#[test]
fn test_type_inference_edge_cases() {
    expect_int("x = y = 42; x", 42);
    expect_tuple("t = u = [1, 2]; t", vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_function_return_type_inference() {
    expect_int("get_number = #{ 42 }; [] ~> get_number", 42);
    expect_tuple("get_pair = #{ [1, 2] }; [] ~> get_pair", vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_polymorphic_functions() {
    expect_int("first = #[any, any] { $0 }; [100, \"hello\"] ~> first", 100);
    expect_binary("second = #[any, any] { $1 }; [42, \"world\"] ~> second", 0);
}

#[test]
fn test_type_errors() {
    expect_error("f = #int { $ }; [1, 2] ~> f");
    expect_error("add = #[int, int] { [$0, $1] ~> + }; \"hello\" ~> add");
}

#[test]
fn test_complex_type_combinations() {
    expect_tuple("ComplexData[items: [Item[id: int, name: string]], meta: [count: int]] = ComplexData[items: [Item[id: 1, name: \"test\"]], meta: [count: 1]]; ComplexData.meta", 
                vec![Value::Integer(1)]);
}

#[test]
fn test_type_based_dispatch() {
    expect_int("stringify = #(int, string) { | x: int = $ => 0 | s: string = $ => 1 }; 42 ~> stringify", 0);
    expect_int("stringify = #(int, string) { | x: int = $ => 0 | s: string = $ => 1 }; \"hello\" ~> stringify", 1);
}

#[test]
fn test_existential_types() {
    expect_function("Container = #any { [value: $, type_info: \"unknown\"] }; Container");
    expect_tuple("Container = #any { [value: $, type_info: \"unknown\"] }; 42 ~> Container", vec![Value::Integer(42), Value::Binary(0)]);
}

#[test]
fn test_phantom_types() {
    expect_tuple("Tagged[T] = #any { [data: $] }; SafeInt = Tagged[int]; 42 ~> SafeInt", vec![Value::Integer(42)]);
}

#[test]
fn test_type_level_computations() {
    expect_int("Sum[A: int, B: int] = #{ [A, B] ~> + }; [] ~> Sum[A: 10, B: 20]", 30);
}

#[test]
fn test_gradual_typing() {
    expect_int("flexible = #any { $ }; typed_wrapper = #int { $ ~> flexible }; 42 ~> typed_wrapper", 42);
}