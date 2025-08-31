mod common;

use common::*;
use quiver::{bytecode::TypeId, vm::Value};

#[test]
fn test_empty_tuple() {
    quiver().evaluate("[]").expect_nil();
}

#[test]
fn test_simple_unnamed_tuple() {
    quiver()
        .evaluate("[1, 2]")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
    quiver()
        .evaluate("[42, 0]")
        .expect_tuple(vec![Value::Integer(42), Value::Integer(0)]);
    quiver().evaluate("[-5, 10, 15]").expect_tuple(vec![
        Value::Integer(-5),
        Value::Integer(10),
        Value::Integer(15),
    ]);
}

#[test]
fn test_single_element_tuple() {
    quiver()
        .evaluate("[42]")
        .expect_tuple(vec![Value::Integer(42)]);
    quiver()
        .evaluate("[0]")
        .expect_tuple(vec![Value::Integer(0)]);
    quiver()
        .evaluate("[-100]")
        .expect_tuple(vec![Value::Integer(-100)]);
}

#[test]
fn test_named_tuple_construction() {
    quiver()
        .evaluate("Point[10, 20]")
        .expect_tuple(vec![Value::Integer(10), Value::Integer(20)]);
    quiver()
        .evaluate("Person[\"Alice\", 30]")
        .expect_tuple(vec![Value::Binary(0), Value::Integer(30)]);
}

#[test]
fn test_named_fields_tuple() {
    quiver()
        .evaluate("[x: 10, y: 20]")
        .expect_tuple(vec![Value::Integer(10), Value::Integer(20)]);
    quiver()
        .evaluate("[name: \"Bob\", age: 25]")
        .expect_tuple(vec![Value::Binary(0), Value::Integer(25)]);
}

#[test]
fn test_named_tuple_with_named_fields() {
    quiver()
        .evaluate("Point[x: 5, y: 15]")
        .expect_tuple(vec![Value::Integer(5), Value::Integer(15)]);
    quiver()
        .evaluate("Person[name: \"Charlie\", age: 35]")
        .expect_tuple(vec![Value::Binary(0), Value::Integer(35)]);
}

#[test]
fn test_mixed_named_unnamed_fields() {
    quiver().evaluate("[a: 1, 2, b: 3]").expect_tuple(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]);
    quiver().evaluate("[10, x: 20, 30]").expect_tuple(vec![
        Value::Integer(10),
        Value::Integer(20),
        Value::Integer(30),
    ]);
}

#[test]
fn test_nested_tuples() {
    quiver().evaluate("[[1, 2], [3, 4]]").expect_tuple(vec![
        Value::Tuple(TypeId::NIL, vec![Value::Integer(1), Value::Integer(2)]),
        Value::Tuple(TypeId::NIL, vec![Value::Integer(3), Value::Integer(4)]),
    ]);
}

#[test]
fn test_deeply_nested_tuples() {
    quiver().evaluate("[[[1]]]").expect_tuple(vec![Value::Tuple(
        TypeId::NIL,
        vec![Value::Tuple(TypeId::NIL, vec![Value::Integer(1)])],
    )]);
}

#[test]
fn test_positional_field_access() {
    quiver().evaluate("[10, 20].0").expect_int(10);
    quiver().evaluate("[10, 20].1").expect_int(20);
    quiver().evaluate("[5, 15, 25].2").expect_int(25);
}

#[test]
fn test_named_field_access() {
    quiver().evaluate("[x: 10, y: 20].x").expect_int(10);
    quiver().evaluate("[x: 10, y: 20].y").expect_int(20);
    quiver()
        .evaluate("[name: \"Alice\", age: 30].age")
        .expect_int(30);
}

#[test]
fn test_mixed_field_access() {
    quiver().evaluate("[a: 1, 2, b: 3].0").expect_int(1);
    quiver().evaluate("[a: 1, 2, b: 3].1").expect_int(2);
    quiver().evaluate("[a: 1, 2, b: 3].2").expect_int(3);
    quiver().evaluate("[a: 1, 2, b: 3].a").expect_int(1);
    quiver().evaluate("[a: 1, 2, b: 3].b").expect_int(3);
}

#[test]
fn test_chained_field_access() {
    quiver().evaluate("[[10, 20], [30, 40]].0.0").expect_int(10);
    quiver().evaluate("[[10, 20], [30, 40]].0.1").expect_int(20);
    quiver().evaluate("[[10, 20], [30, 40]].1.0").expect_int(30);
    quiver().evaluate("[[10, 20], [30, 40]].1.1").expect_int(40);
}

#[test]
fn test_complex_chained_access() {
    quiver()
        .evaluate("[outer: [inner: [value: 42]]].outer.inner.value")
        .expect_int(42);
    quiver()
        .evaluate("[data: [[1, 2], [3, 4]]].data.1.0")
        .expect_int(3);
}

#[test]
fn test_mixed_positional_named_chaining() {
    quiver()
        .evaluate("[users: [[name: \"Alice\"], [name: \"Bob\"]]].users.0.name")
        .expect_int(0);
    quiver()
        .evaluate("[coords: [x: 5, y: 10]].coords.x")
        .expect_int(5);
}

#[test]
fn test_tuple_with_computed_values() {
    quiver()
        .evaluate("[[1, 2] ~> +, [3, 4] ~> *]")
        .expect_tuple(vec![Value::Integer(3), Value::Integer(12)]);
    quiver()
        .evaluate("[x: [5, 5] ~> +, y: [10, 2] ~> /]")
        .expect_tuple(vec![Value::Integer(10), Value::Integer(5)]);
}

#[test]
fn test_field_access_on_computed_tuples() {
    quiver()
        .evaluate("[[1, 2] ~> +, [3, 4] ~> *].0")
        .expect_int(3);
    quiver()
        .evaluate("[[1, 2] ~> +, [3, 4] ~> *].1")
        .expect_int(12);
    quiver()
        .evaluate("[x: [5, 5] ~> +, y: [10, 2] ~> /].x")
        .expect_int(10);
}

#[test]
fn test_field_access_in_operations() {
    quiver()
        .evaluate("[[x: 10, y: 20].x, [x: 10, y: 20].y] ~> +")
        .expect_int(30);
    quiver()
        .evaluate("[[5, 15].0, [5, 15].1] ~> *")
        .expect_int(75);
}

#[test]
fn test_tuple_assignment_and_access() {
    quiver().evaluate("p = [x: 5, y: 10]; p.x").expect_int(5);
    quiver()
        .evaluate("data = [[1, 2], [3, 4]]; data.1.0")
        .expect_int(3);
}

#[test]
fn test_invalid_field_access() {
    quiver().evaluate("[1, 2].5").expect_error();
    quiver().evaluate("[1, 2].nonexistent").expect_error();
    quiver().evaluate("[].0").expect_error();
}

#[test]
fn test_invalid_chained_access() {
    quiver().evaluate("42.field").expect_error();
    quiver().evaluate("[1, 2].0.field").expect_error();
    quiver().evaluate("[1, 2].field.0").expect_error();
}

#[test]
fn test_special_tuple_types() {
    quiver().evaluate("None[]").expect_nil();
    quiver().evaluate("Ok[]").expect_ok();
}

#[test]
fn test_tagged_types() {
    quiver()
        .evaluate("WeightKg[70]")
        .expect_tuple(vec![Value::Integer(70)]);
    quiver()
        .evaluate("Distance[100]")
        .expect_tuple(vec![Value::Integer(100)]);
}

#[test]
fn test_tuple_with_binary_fields() {
    quiver()
        .evaluate("[name: \"Alice\", data: '0fa27c']")
        .expect_tuple(vec![Value::Binary(0), Value::Binary(1)]);
}

#[test]
fn test_large_tuples() {
    quiver()
        .evaluate("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
        .expect_tuple(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4),
            Value::Integer(5),
            Value::Integer(6),
            Value::Integer(7),
            Value::Integer(8),
            Value::Integer(9),
            Value::Integer(10),
        ]);
}

#[test]
fn test_tuple_equality() {
    quiver().evaluate("[[1, 2], [1, 2]] ~> ==").expect_int(0);
    quiver().evaluate("[[1, 2], [2, 1]] ~> ==").expect_nil();
    quiver().evaluate("[[x: 5], [x: 5]] ~> ==").expect_int(0);
}

#[test]
fn test_whitespace_in_tuples() {
    quiver()
        .evaluate("[ 1 , 2 ]")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
    quiver()
        .evaluate("[\n  1,\n  2\n]")
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
    quiver()
        .evaluate("[  x:  10  ,  y:  20  ]")
        .expect_tuple(vec![Value::Integer(10), Value::Integer(20)]);
}
