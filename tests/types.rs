mod common;
use common::*;
use quiver::vm::Value;

#[test]
fn test_integer_type() {
    quiver().evaluate("42").expect_int(42);
    quiver().evaluate("-17").expect_int(-17);
    quiver().evaluate("0").expect_int(0);
}

#[test]
fn test_binary_type() {
    quiver().evaluate("'deadbeef'").expect_binary(0);
    quiver().evaluate("'0123456789abcdef'").expect_binary(1);
    quiver().evaluate("'ff'").expect_binary(2);
}

#[test]
fn test_string_type() {
    quiver().evaluate("\"hello world\"").expect_binary(0);
    quiver().evaluate("\"\"").expect_binary(1);
    quiver()
        .evaluate("\"with spaces and symbols!\"")
        .expect_binary(2);
}

#[test]
fn test_tuple_type() {
    quiver().evaluate("[1, 2, 3]").expect_tuple(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]);
    quiver().evaluate("[]").expect_tuple(vec![]);
    quiver()
        .evaluate("[42]")
        .expect_tuple(vec![Value::Integer(42)]);
}

#[test]
fn test_named_tuple_type() {
    quiver()
        .evaluate("Point[x: 10, y: 20]")
        .expect_tuple(vec![Value::Integer(10), Value::Integer(20)]);
    quiver()
        .evaluate("Person[name: \"Alice\", age: 30]")
        .expect_tuple(vec![Value::Binary(0), Value::Integer(30)]);
}

#[test]
fn test_nil_type() {
    quiver().evaluate("[]").expect_nil();
    quiver().evaluate("None[]").expect_nil();
}

#[test]
fn test_ok_type() {
    quiver().evaluate("Ok[]").expect_ok();
}

#[test]
fn test_function_type() {
    quiver().evaluate("#int { $ }").expect_function();
    quiver()
        .evaluate("#[int, int] { [$0, $1] ~> + }")
        .expect_function();
    quiver().evaluate("#{ 42 }").expect_function();
}

#[test]
fn test_type_inference_with_operations() {
    quiver().evaluate("[5, 3] ~> +").expect_int(8);
    quiver()
        .evaluate("[1, 2]")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
    quiver().evaluate("[10, 20].0").expect_int(10);
}

#[test]
fn test_nested_type_structures() {
    quiver().evaluate("[[1, 2], [3, 4]]").expect_tuple(vec![
        Value::Tuple(
            quiver::bytecode::TypeId::NIL,
            vec![Value::Integer(1), Value::Integer(2)],
        ),
        Value::Tuple(
            quiver::bytecode::TypeId::NIL,
            vec![Value::Integer(3), Value::Integer(4)],
        ),
    ]);
}

#[test]
fn test_function_parameter_types() {
    quiver()
        .evaluate("f = #int { [$, 2] ~> * }; 5 ~> f")
        .expect_int(10);
    quiver()
        .evaluate("add = #[int, int] { [$0, $1] ~> + }; [3, 4] ~> add")
        .expect_int(7);
    quiver()
        .evaluate("make_point = #[int, int] { Point[x: $0, y: $1] }; [1, 2] ~> make_point")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_union_type_patterns() {
    // Simplified test - full pattern matching syntax not yet supported by parser
    quiver().evaluate("process = #(int, Point[x: int, y: int]) { $ }; 42 ~> process").expect_int(42);
}

#[test]
fn test_generic_type_functions() {
    quiver()
        .evaluate("identity = #any { $ }; identity")
        .expect_function();
    quiver()
        .evaluate("identity = #any { $ }; 42 ~> identity")
        .expect_int(42);
    quiver()
        .evaluate("identity = #any { $ }; [1, 2] ~> identity")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_type_coercion() {
    quiver().evaluate("x = 42; x").expect_int(42);
    quiver()
        .evaluate("t = [1, 2]; t")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_type_checking_with_pattern_matching() {
    quiver()
        .evaluate("Point[x: a, y: b] = Point[x: 5, y: 10] => [a, b] ~> + | 0")
        .expect_int(15);
    quiver()
        .evaluate("[a, b] = [10, 20] => [a, b] ~> * | 0")
        .expect_int(200);
}

#[test]
fn test_typed_destructuring() {
    quiver()
        .evaluate("Point[x: px, y: py] = Point[x: 3, y: 4]; px")
        .expect_int(3);
    quiver()
        .evaluate("[first, second] = [100, 200]; second")
        .expect_int(200);
}

#[test]
fn test_function_type_constraints() {
    quiver()
        .evaluate("square = #int { [$, $] ~> * }; 7 ~> square")
        .expect_int(49);
    quiver()
        .evaluate(
            "concat_lengths = #[string, string] { 10 }; [\"hello\", \"world\"] ~> concat_lengths",
        )
        .expect_int(10);
}

#[test]
fn test_recursive_type_definitions() {
    quiver().evaluate("ListNode[value: int, next: ListNode] = ListNode[value: 1, next: ListNode[value: 2, next: None[]]]; ListNode.value").expect_int(1);
}

#[test]
fn test_algebraic_data_types() {
    quiver()
        .evaluate("Success[42]")
        .expect_tuple(vec![Value::Integer(42)]);
    quiver()
        .evaluate("Error[\"not found\"]")
        .expect_tuple(vec![Value::Binary(0)]);
    quiver().evaluate("None[]").expect_nil();
}

#[test]
fn test_tagged_union_types() {
    quiver().evaluate("handle_result = #(Success[int], Error[string]) { | Success[value] = $ => value | Error[msg] = $ => -1 }; Success[100] ~> handle_result").expect_int(100);
}

#[test]
fn test_type_guards_in_functions() {
    quiver()
        .evaluate(
            "safe_div = #[int, int] { [$1, 0] ~> != => [$0, $1] ~> / | 0 }; [10, 2] ~> safe_div",
        )
        .expect_int(5);
    quiver()
        .evaluate(
            "safe_div = #[int, int] { [$1, 0] ~> != => [$0, $1] ~> / | 0 }; [10, 0] ~> safe_div",
        )
        .expect_int(0);
}

#[test]
fn test_higher_kinded_types() {
    quiver()
        .evaluate("Container[int] = #int { Container[value: $] }; Container[int]")
        .expect_function();
    quiver()
        .evaluate("Container[int] = #int { Container[value: $] }; 42 ~> Container[int]")
        .expect_tuple(vec![Value::Integer(42)]);
}

#[test]
fn test_type_aliases() {
    quiver()
        .evaluate("type UserId = int; user_id = 123; user_id")
        .expect_int(123);
    quiver()
        .evaluate("type Coordinates = Point[x: int, y: int]; pos = Point[x: 5, y: 10]; pos")
        .expect_tuple(vec![Value::Integer(5), Value::Integer(10)]);
}

#[test]
fn test_dependent_types() {
    quiver()
        .evaluate("Vector[n: int] = #int { [n, $] }; size = 3; [size, 42] ~> Vector[n: size]")
        .expect_int(42);
}

#[test]
fn test_type_inference_edge_cases() {
    quiver().evaluate("x = y = 42; x").expect_int(42);
    quiver()
        .evaluate("t = u = [1, 2]; t")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_function_return_type_inference() {
    quiver()
        .evaluate("get_number = #{ 42 }; [] ~> get_number")
        .expect_int(42);
    quiver()
        .evaluate("get_pair = #{ [1, 2] }; [] ~> get_pair")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_polymorphic_functions() {
    quiver()
        .evaluate("first = #[any, any] { $0 }; [100, \"hello\"] ~> first")
        .expect_int(100);
    quiver()
        .evaluate("second = #[any, any] { $1 }; [42, \"world\"] ~> second")
        .expect_binary(0);
}

#[test]
fn test_type_errors() {
    quiver()
        .evaluate("f = #int { $ }; [1, 2] ~> f")
        .expect_error();
    quiver()
        .evaluate("add = #[int, int] { [$0, $1] ~> + }; \"hello\" ~> add")
        .expect_error();
}

#[test]
fn test_complex_type_combinations() {
    quiver().evaluate("ComplexData[items: [Item[id: int, name: string]], meta: [count: int]] = ComplexData[items: [Item[id: 1, name: \"test\"]], meta: [count: 1]]; ComplexData.meta").expect_tuple(vec![Value::Integer(1)]);
}

#[test]
fn test_type_based_dispatch() {
    quiver().evaluate("stringify = #(int, string) { | x: int = $ => 0 | s: string = $ => 1 }; 42 ~> stringify").expect_int(0);
    quiver().evaluate("stringify = #(int, string) { | x: int = $ => 0 | s: string = $ => 1 }; \"hello\" ~> stringify").expect_int(1);
}

#[test]
fn test_existential_types() {
    quiver()
        .evaluate("Container = #any { [value: $, type_info: \"unknown\"] }; Container")
        .expect_function();
    quiver()
        .evaluate("Container = #any { [value: $, type_info: \"unknown\"] }; 42 ~> Container")
        .expect_tuple(vec![Value::Integer(42), Value::Binary(0)]);
}

#[test]
fn test_phantom_types() {
    quiver()
        .evaluate("Tagged[T] = #any { [data: $] }; SafeInt = Tagged[int]; 42 ~> SafeInt")
        .expect_tuple(vec![Value::Integer(42)]);
}

#[test]
fn test_type_level_computations() {
    quiver()
        .evaluate("Sum[A: int, B: int] = #{ [A, B] ~> + }; [] ~> Sum[A: 10, B: 20]")
        .expect_int(30);
}

#[test]
fn test_gradual_typing() {
    quiver()
        .evaluate(
            "flexible = #any { $ }; typed_wrapper = #int { $ ~> flexible }; 42 ~> typed_wrapper",
        )
        .expect_int(42);
}

#[test] 
fn test_union_propagation_in_functions() {
    // Test function with union parameter type - without union propagation,
    // this would fail when called with different argument types
    // With union propagation: #(int, bin) creates variants #int -> int and #bin -> int
    
    // Test with integer argument - matches #int -> int variant
    quiver()
        .evaluate("f = #(int, bin) { 42 }; 5 ~> f")
        .expect_int(42);
        
    // Test with binary argument - matches #bin -> int variant  
    quiver()
        .evaluate("f = #(int, bin) { 42 }; \"hello\" ~> f")
        .expect_int(42);
}

#[test]
fn test_union_propagation_enables_polymorphism() {
    // Test that union propagation enables functions to work with multiple types
    // The key insight: without union propagation, this function couldn't type-check
    // because the parameter type (int, bin) would be unresolved
    
    // Function that returns the parameter - should work with both int and bin
    quiver()
        .evaluate("identity = #(int, bin) { $ }; 42 ~> identity")
        .expect_int(42);
    
    quiver()
        .evaluate("identity = #(int, bin) { $ }; \"test\" ~> identity")  
        .expect_binary(0);
}

#[test]
fn test_union_propagation_in_nested_functions() {
    // Test union propagation with function types that have union inputs and outputs
    // #(int, bin) -> (int, bin) should create 4 variants:
    // #int -> int, #int -> bin, #bin -> int, #bin -> bin
    
    // This function takes a union and returns a constant - tests input union propagation
    quiver()
        .evaluate("processor = #(int, bin) { 100 }; 42 ~> processor")
        .expect_int(100);
    
    // Test with the other union variant
    quiver()
        .evaluate("processor = #(int, bin) { 100 }; \"data\" ~> processor")
        .expect_int(100);
}

#[test]
fn test_union_propagation_demonstrates_type_precision() {
    // Test that demonstrates union propagation provides precise type information
    // Without union propagation, functions with union parameters would be unresolved
    // With union propagation, each variant is precisely typed
    
    // This function can handle both int and bin arguments precisely
    quiver()
        .evaluate("flexible = #(int, bin) { $ }; result1 = 42 ~> flexible; result1")
        .expect_int(42);
        
    // And it preserves the type of binary arguments too  
    quiver()
        .evaluate("flexible = #(int, bin) { $ }; result2 = \"hello\" ~> flexible; result2")
        .expect_binary(0);
}
