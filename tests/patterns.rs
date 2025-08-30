mod common;

use common::*;
use quiver::{vm::Value, bytecode::TypeId};

#[test]
fn test_simple_variable_assignment() {
    expect_int("x = 42; x", 42);
    expect_int("y = 100; y", 100);
    expect_int("value = -50; value", -50);
}

#[test]
fn test_tuple_assignment() {
    expect_int("t = [1, 2, 3]; t.0", 1);
    expect_int("t = [1, 2, 3]; t.1", 2);
    expect_int("t = [1, 2, 3]; t.2", 3);
}

#[test]
fn test_full_tuple_destructuring() {
    expect_int("[a, b] = [10, 20]; a", 10);
    expect_int("[a, b] = [10, 20]; b", 20);
    expect_int("[x, y, z] = [1, 2, 3]; y", 2);
}

#[test] 
fn test_named_tuple_destructuring() {
    expect_int("Point[x: a, y: b] = Point[x: 5, y: 15]; a", 5);
    expect_int("Point[x: a, y: b] = Point[x: 5, y: 15]; b", 15);
}

#[test]
fn test_mixed_named_unnamed_destructuring() {
    expect_int("[a, b: c] = [1, b: 2]; a", 1);
    expect_int("[a, b: c] = [1, b: 2]; c", 2);
}

#[test]
fn test_partial_destructuring() {
    expect_int("(b, e) = X[a: 1, b: 2, c: 3, d: 4, e: 5, f: 6]; b", 2);
    expect_int("(b, e) = X[a: 1, b: 2, c: 3, d: 4, e: 5, f: 6]; e", 5);
    expect_int("(name, age) = Person[name: \"Alice\", age: 30, city: \"NYC\"]; age", 30);
}

#[test]
fn test_partial_destructuring_order_independence() {
    expect_int("(age, name) = Person[name: \"Bob\", age: 25, city: \"LA\"]; age", 25);
    expect_int("(age, name) = Person[name: \"Bob\", age: 25, city: \"LA\"]; name", 0);
}

#[test]
fn test_star_destructuring() {
    expect_int("* = X[a: 1, b: 2, c: 3, d: 4]; a", 1);
    expect_int("* = X[a: 1, b: 2, c: 3, d: 4]; b", 2);
    expect_int("* = X[a: 1, b: 2, c: 3, d: 4]; c", 3);
    expect_int("* = X[a: 1, b: 2, c: 3, d: 4]; d", 4);
}

#[test]
fn test_star_destructuring_ignores_unnamed() {
    expect_int("* = [a: 5, 10, b: 15, 20]; a", 5);
    expect_int("* = [a: 5, 10, b: 15, 20]; b", 15);
    expect_error("* = [a: 5, 10, b: 15, 20]; 10");
}

#[test]
fn test_nested_destructuring() {
    expect_int("Point[x: px, y: Point[x: nx, y: ny]] = Point[x: 1, y: Point[x: 2, y: 3]]; px", 1);
    expect_int("Point[x: px, y: Point[x: nx, y: ny]] = Point[x: 1, y: Point[x: 2, y: 3]]; nx", 2);
    expect_int("Point[x: px, y: Point[x: nx, y: ny]] = Point[x: 1, y: Point[x: 2, y: 3]]; ny", 3);
}

#[test]
fn test_deeply_nested_destructuring() {
    expect_int("[[[a]]] = [[[42]]]; a", 42);
    expect_int("Outer[Middle[Inner[value: x]]] = Outer[Middle[Inner[value: 100]]]; x", 100);
}

#[test]
fn test_assignment_with_expressions() {
    expect_int("x = [3, 4] ~> +; x", 7);
    expect_int("[a, b] = [[1, 2] ~> +, [5, 3] ~> *]; a", 3);
    expect_int("[a, b] = [[1, 2] ~> +, [5, 3] ~> *]; b", 15);
}

#[test]
fn test_destructuring_computed_values() {
    expect_int("[x, y] = [[2, 3] ~> +, [4, 5] ~> *]; x", 5);
    expect_int("[x, y] = [[2, 3] ~> +, [4, 5] ~> *]; y", 20);
}

#[test]
fn test_variable_shadowing() {
    expect_int("x = 5; x = 10; x", 10);
    expect_int("x = 5; [x, y] = [20, 30]; x", 20);
}

#[test]
fn test_wildcard_patterns() {
    expect_int("[_, b] = [1, 2]; b", 2);
    expect_int("[a, _] = [3, 4]; a", 3);
    expect_int("[_, _, c] = [1, 2, 3]; c", 3);
}

#[test]
fn test_pattern_matching_in_conditionals() {
    expect_int("x = 5 | Point[x: a, y: b] = Point[x: 10, y: 20] => a | 0", 10);
    expect_int("Point[x: a, y: b] = Point[x: 15, y: 25] => a", 15);
}

#[test]
fn test_assignment_return_values() {
    expect_ok("x = 42");
    expect_ok("[a, b] = [1, 2]");
    expect_ok("Point[x: px, y: py] = Point[x: 5, y: 10]");
}

#[test]
fn test_failed_assignments() {
    expect_nil("[a, b] = [1]");
    expect_nil("[a, b, c] = [1, 2]");
    expect_nil("(nonexistent) = X[a: 1, b: 2]");
}

#[test]
fn test_type_mismatch_assignments() {
    expect_nil("Point[x: a, y: b] = [1, 2]");
    expect_nil("[a, b] = Point[x: 1, y: 2]");
}

#[test]
fn test_assignment_chaining() {
    expect_int("a = b = 42; a", 42);
    expect_int("a = b = 42; b", 42);
}

#[test]
fn test_destructuring_with_field_access() {
    expect_int("p = Point[x: 10, y: 20]; Point[x: a, y: b] = p; a", 10);
    expect_int("data = [info: [value: 42]]; [info: [value: v]] = data; v", 42);
}

#[test]
fn test_complex_mixed_patterns() {
    expect_int("Data[items: [first, second: s], meta: [count: c]] = Data[items: [1, second: 2], meta: [count: 3]]; first", 1);
    expect_int("Data[items: [first, second: s], meta: [count: c]] = Data[items: [1, second: 2], meta: [count: 3]]; s", 2);
    expect_int("Data[items: [first, second: s], meta: [count: c]] = Data[items: [1, second: 2], meta: [count: 3]]; c", 3);
}

#[test]
fn test_partial_with_computation() {
    expect_int("(x, z) = Result[x: [2, 3] ~> +, y: 10, z: [4, 6] ~> *]; x", 5);
    expect_int("(x, z) = Result[x: [2, 3] ~> +, y: 10, z: [4, 6] ~> *]; z", 24);
}

#[test]
fn test_star_with_nested_structures() {
    expect_int("* = Config[host: \"localhost\", port: 8080, db: [name: \"test\", size: 100]]; host", 0);
    expect_int("* = Config[host: \"localhost\", port: 8080, db: [name: \"test\", size: 100]]; port", 8080);
}

#[test]
fn test_multiple_assignments() {
    expect_int("x = 1; y = 2; z = 3; [x, y, z] ~> +", 6);
    expect_int("[a, b] = [10, 20]; [c, d] = [a, b]; c", 10);
}

#[test]
fn test_assignment_in_expressions() {
    expect_tuple("[x = 5, x * 2]", vec![Value::Tuple(TypeId::OK, vec![]), Value::Integer(10)]);
    expect_int("(a = [1, 2] ~> +) * 3", 9);
}