mod common;

use common::*;
use quiver::{vm::Value, bytecode::TypeId};

#[test]
fn test_empty_tuple() {
    expect_nil("[]");
}

#[test]
fn test_simple_unnamed_tuple() {
    expect_tuple("[1, 2]", vec![Value::Integer(1), Value::Integer(2)]);
    expect_tuple("[42, 0]", vec![Value::Integer(42), Value::Integer(0)]);
    expect_tuple("[-5, 10, 15]", vec![Value::Integer(-5), Value::Integer(10), Value::Integer(15)]);
}

#[test]
fn test_single_element_tuple() {
    expect_tuple("[42]", vec![Value::Integer(42)]);
    expect_tuple("[0]", vec![Value::Integer(0)]);
    expect_tuple("[-100]", vec![Value::Integer(-100)]);
}

#[test]
fn test_named_tuple_construction() {
    expect_tuple("Point[10, 20]", vec![Value::Integer(10), Value::Integer(20)]);
    expect_tuple("Person[\"Alice\", 30]", vec![Value::Binary(0), Value::Integer(30)]);
}

#[test]
fn test_named_fields_tuple() {
    expect_tuple("[x: 10, y: 20]", vec![Value::Integer(10), Value::Integer(20)]);
    expect_tuple("[name: \"Bob\", age: 25]", vec![Value::Binary(0), Value::Integer(25)]);
}

#[test]
fn test_named_tuple_with_named_fields() {
    expect_tuple("Point[x: 5, y: 15]", vec![Value::Integer(5), Value::Integer(15)]);
    expect_tuple("Person[name: \"Charlie\", age: 35]", vec![Value::Binary(0), Value::Integer(35)]);
}

#[test]
fn test_mixed_named_unnamed_fields() {
    expect_tuple("[a: 1, 2, b: 3]", vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
    expect_tuple("[10, x: 20, 30]", vec![Value::Integer(10), Value::Integer(20), Value::Integer(30)]);
}

#[test]
fn test_nested_tuples() {
    expect_tuple("[[1, 2], [3, 4]]", vec![
        Value::Tuple(TypeId::NIL, vec![Value::Integer(1), Value::Integer(2)]),
        Value::Tuple(TypeId::NIL, vec![Value::Integer(3), Value::Integer(4)])
    ]);
}

#[test]
fn test_deeply_nested_tuples() {
    expect_tuple("[[[1]]]", vec![
        Value::Tuple(TypeId::NIL, vec![
            Value::Tuple(TypeId::NIL, vec![Value::Integer(1)])
        ])
    ]);
}

#[test]
fn test_positional_field_access() {
    expect_int("[10, 20].0", 10);
    expect_int("[10, 20].1", 20);
    expect_int("[5, 15, 25].2", 25);
}

#[test]
fn test_named_field_access() {
    expect_int("[x: 10, y: 20].x", 10);
    expect_int("[x: 10, y: 20].y", 20);
    expect_int("[name: \"Alice\", age: 30].age", 30);
}

#[test]
fn test_mixed_field_access() {
    expect_int("[a: 1, 2, b: 3].0", 1);
    expect_int("[a: 1, 2, b: 3].1", 2);
    expect_int("[a: 1, 2, b: 3].2", 3);
    expect_int("[a: 1, 2, b: 3].a", 1);
    expect_int("[a: 1, 2, b: 3].b", 3);
}

#[test]
fn test_chained_field_access() {
    expect_int("[[10, 20], [30, 40]].0.0", 10);
    expect_int("[[10, 20], [30, 40]].0.1", 20);
    expect_int("[[10, 20], [30, 40]].1.0", 30);
    expect_int("[[10, 20], [30, 40]].1.1", 40);
}

#[test]
fn test_complex_chained_access() {
    expect_int("[outer: [inner: [value: 42]]].outer.inner.value", 42);
    expect_int("[data: [[1, 2], [3, 4]]].data.1.0", 3);
}

#[test]
fn test_mixed_positional_named_chaining() {
    expect_int("[users: [[name: \"Alice\"], [name: \"Bob\"]]].users.0.name", 0);
    expect_int("[coords: [x: 5, y: 10]].coords.x", 5);
}

#[test]
fn test_tuple_with_computed_values() {
    expect_tuple("[[1, 2] ~> +, [3, 4] ~> *]", vec![Value::Integer(3), Value::Integer(12)]);
    expect_tuple("[x: [5, 5] ~> +, y: [10, 2] ~> /]", vec![Value::Integer(10), Value::Integer(5)]);
}

#[test] 
fn test_field_access_on_computed_tuples() {
    expect_int("[[1, 2] ~> +, [3, 4] ~> *].0", 3);
    expect_int("[[1, 2] ~> +, [3, 4] ~> *].1", 12);
    expect_int("[x: [5, 5] ~> +, y: [10, 2] ~> /].x", 10);
}

#[test]
fn test_field_access_in_operations() {
    expect_int("[[x: 10, y: 20].x, [x: 10, y: 20].y] ~> +", 30);
    expect_int("[[5, 15].0, [5, 15].1] ~> *", 75);
}

#[test]
fn test_tuple_assignment_and_access() {
    expect_int("p = [x: 5, y: 10]; p.x", 5);
    expect_int("data = [[1, 2], [3, 4]]; data.1.0", 3);
}

#[test]
fn test_invalid_field_access() {
    expect_error("[1, 2].5");
    expect_error("[1, 2].nonexistent");
    expect_error("[].0");
}

#[test]
fn test_invalid_chained_access() {
    expect_error("42.field");
    expect_error("[1, 2].0.field");
    expect_error("[1, 2].field.0");
}

#[test]
fn test_special_tuple_types() {
    expect_nil("None[]");
    expect_ok("Ok[]");
}

#[test]
fn test_tagged_types() {
    expect_tuple("WeightKg[70]", vec![Value::Integer(70)]);
    expect_tuple("Distance[100]", vec![Value::Integer(100)]);
}

#[test]
fn test_tuple_with_binary_fields() {
    expect_tuple("[name: \"Alice\", data: '0fa27c']", vec![Value::Binary(0), Value::Binary(1)]);
}

#[test]
fn test_large_tuples() {
    expect_tuple("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]", vec![
        Value::Integer(1), Value::Integer(2), Value::Integer(3), Value::Integer(4), Value::Integer(5),
        Value::Integer(6), Value::Integer(7), Value::Integer(8), Value::Integer(9), Value::Integer(10)
    ]);
}

#[test] 
fn test_tuple_equality() {
    expect_int("[[1, 2], [1, 2]] ~> ==", 0);
    expect_nil("[[1, 2], [2, 1]] ~> ==");
    expect_int("[[x: 5], [x: 5]] ~> ==", 0);
}

#[test]
fn test_whitespace_in_tuples() {
    expect_tuple("[ 1 , 2 ]", vec![Value::Integer(1), Value::Integer(2)]);
    expect_tuple("[\n  1,\n  2\n]", vec![Value::Integer(1), Value::Integer(2)]);
    expect_tuple("[  x:  10  ,  y:  20  ]", vec![Value::Integer(10), Value::Integer(20)]);
}