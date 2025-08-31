mod common;
use common::*;
use quiver::{bytecode::TypeId, vm::Value};

#[test]
fn test_simple_variable_assignment() {
    quiver().evaluate("x = 42; x").expect_int(42);
    quiver().evaluate("y = 100; y").expect_int(100);
    quiver().evaluate("value = -50; value").expect_int(-50);
}

#[test]
fn test_tuple_assignment() {
    quiver().evaluate("t = [1, 2, 3]; t.0").expect_int(1);
    quiver().evaluate("t = [1, 2, 3]; t.1").expect_int(2);
    quiver().evaluate("t = [1, 2, 3]; t.2").expect_int(3);
}

#[test]
fn test_full_tuple_destructuring() {
    quiver().evaluate("[a, b] = [10, 20]; a").expect_int(10);
    quiver().evaluate("[a, b] = [10, 20]; b").expect_int(20);
    quiver().evaluate("[x, y, z] = [1, 2, 3]; y").expect_int(2);
}

#[test]
fn test_named_tuple_destructuring() {
    quiver()
        .evaluate("Point[x: a, y: b] = Point[x: 5, y: 15]; a")
        .expect_int(5);
    quiver()
        .evaluate("Point[x: a, y: b] = Point[x: 5, y: 15]; b")
        .expect_int(15);
}

#[test]
fn test_mixed_named_unnamed_destructuring() {
    quiver().evaluate("[a, b: c] = [1, b: 2]; a").expect_int(1);
    quiver().evaluate("[a, b: c] = [1, b: 2]; c").expect_int(2);
}

#[test]
fn test_partial_destructuring() {
    quiver()
        .evaluate("(b, e) = X[a: 1, b: 2, c: 3, d: 4, e: 5, f: 6]; b")
        .expect_int(2);
    quiver()
        .evaluate("(b, e) = X[a: 1, b: 2, c: 3, d: 4, e: 5, f: 6]; e")
        .expect_int(5);
    quiver()
        .evaluate("(name, age) = Person[name: \"Alice\", age: 30, city: \"NYC\"]; age")
        .expect_int(30);
}

#[test]
fn test_partial_destructuring_order_independence() {
    quiver()
        .evaluate("(age, name) = Person[name: \"Bob\", age: 25, city: \"LA\"]; age")
        .expect_int(25);
    quiver()
        .evaluate("(age, name) = Person[name: \"Bob\", age: 25, city: \"LA\"]; name")
        .expect_int(0);
}

#[test]
fn test_star_destructuring() {
    quiver()
        .evaluate("* = X[a: 1, b: 2, c: 3, d: 4]; a")
        .expect_int(1);
    quiver()
        .evaluate("* = X[a: 1, b: 2, c: 3, d: 4]; b")
        .expect_int(2);
    quiver()
        .evaluate("* = X[a: 1, b: 2, c: 3, d: 4]; c")
        .expect_int(3);
    quiver()
        .evaluate("* = X[a: 1, b: 2, c: 3, d: 4]; d")
        .expect_int(4);
}

#[test]
fn test_star_destructuring_ignores_unnamed() {
    quiver()
        .evaluate("* = [a: 5, 10, b: 15, 20]; a")
        .expect_int(5);
    quiver()
        .evaluate("* = [a: 5, 10, b: 15, 20]; b")
        .expect_int(15);
    quiver()
        .evaluate("* = [a: 5, 10, b: 15, 20]; 10")
        .expect_error();
}

#[test]
fn test_nested_destructuring() {
    quiver()
        .evaluate("Point[x: px, y: Point[x: nx, y: ny]] = Point[x: 1, y: Point[x: 2, y: 3]]; px")
        .expect_int(1);
    quiver()
        .evaluate("Point[x: px, y: Point[x: nx, y: ny]] = Point[x: 1, y: Point[x: 2, y: 3]]; nx")
        .expect_int(2);
    quiver()
        .evaluate("Point[x: px, y: Point[x: nx, y: ny]] = Point[x: 1, y: Point[x: 2, y: 3]]; ny")
        .expect_int(3);
}

#[test]
fn test_deeply_nested_destructuring() {
    quiver().evaluate("[[[a]]] = [[[42]]]; a").expect_int(42);
    quiver()
        .evaluate("Outer[Middle[Inner[value: x]]] = Outer[Middle[Inner[value: 100]]]; x")
        .expect_int(100);
}

#[test]
fn test_assignment_with_expressions() {
    quiver().evaluate("x = [3, 4] ~> +; x").expect_int(7);
    quiver()
        .evaluate("[a, b] = [[1, 2] ~> +, [5, 3] ~> *]; a")
        .expect_int(3);
    quiver()
        .evaluate("[a, b] = [[1, 2] ~> +, [5, 3] ~> *]; b")
        .expect_int(15);
}

#[test]
fn test_destructuring_computed_values() {
    quiver()
        .evaluate("[x, y] = [[2, 3] ~> +, [4, 5] ~> *]; x")
        .expect_int(5);
    quiver()
        .evaluate("[x, y] = [[2, 3] ~> +, [4, 5] ~> *]; y")
        .expect_int(20);
}

#[test]
fn test_variable_shadowing() {
    quiver().evaluate("x = 5; x = 10; x").expect_int(10);
    quiver()
        .evaluate("x = 5; [x, y] = [20, 30]; x")
        .expect_int(20);
}

#[test]
fn test_wildcard_patterns() {
    quiver().evaluate("[_, b] = [1, 2]; b").expect_int(2);
    quiver().evaluate("[a, _] = [3, 4]; a").expect_int(3);
    quiver().evaluate("[_, _, c] = [1, 2, 3]; c").expect_int(3);
}

#[test]
fn test_pattern_matching_in_conditionals() {
    quiver()
        .evaluate("x = 5 | Point[x: a, y: b] = Point[x: 10, y: 20] => a | 0")
        .expect_int(10);
    quiver()
        .evaluate("Point[x: a, y: b] = Point[x: 15, y: 25] => a")
        .expect_int(15);
}

#[test]
fn test_assignment_return_values() {
    quiver().evaluate("x = 42").expect_ok();
    quiver().evaluate("[a, b] = [1, 2]").expect_ok();
    quiver()
        .evaluate("Point[x: px, y: py] = Point[x: 5, y: 10]")
        .expect_ok();
}

#[test]
fn test_failed_assignments() {
    quiver().evaluate("[a, b] = [1]").expect_nil();
    quiver().evaluate("[a, b, c] = [1, 2]").expect_nil();
    quiver()
        .evaluate("(nonexistent) = X[a: 1, b: 2]")
        .expect_nil();
}

#[test]
fn test_type_mismatch_assignments() {
    quiver().evaluate("Point[x: a, y: b] = [1, 2]").expect_nil();
    quiver().evaluate("[a, b] = Point[x: 1, y: 2]").expect_nil();
}

#[test]
fn test_assignment_chaining() {
    quiver().evaluate("a = b = 42; a").expect_int(42);
    quiver().evaluate("a = b = 42; b").expect_int(42);
}

#[test]
fn test_destructuring_with_field_access() {
    quiver()
        .evaluate("p = Point[x: 10, y: 20]; Point[x: a, y: b] = p; a")
        .expect_int(10);
    quiver()
        .evaluate("data = [info: [value: 42]]; [info: [value: v]] = data; v")
        .expect_int(42);
}

#[test]
fn test_complex_mixed_patterns() {
    quiver().evaluate("Data[items: [first, second: s], meta: [count: c]] = Data[items: [1, second: 2], meta: [count: 3]]; first").expect_int(1);
    quiver().evaluate("Data[items: [first, second: s], meta: [count: c]] = Data[items: [1, second: 2], meta: [count: 3]]; s").expect_int(2);
    quiver().evaluate("Data[items: [first, second: s], meta: [count: c]] = Data[items: [1, second: 2], meta: [count: 3]]; c").expect_int(3);
}

#[test]
fn test_partial_with_computation() {
    quiver()
        .evaluate("(x, z) = Result[x: [2, 3] ~> +, y: 10, z: [4, 6] ~> *]; x")
        .expect_int(5);
    quiver()
        .evaluate("(x, z) = Result[x: [2, 3] ~> +, y: 10, z: [4, 6] ~> *]; z")
        .expect_int(24);
}

#[test]
fn test_star_with_nested_structures() {
    quiver()
        .evaluate(
            "* = Config[host: \"localhost\", port: 8080, db: [name: \"test\", size: 100]]; host",
        )
        .expect_int(0);
    quiver()
        .evaluate(
            "* = Config[host: \"localhost\", port: 8080, db: [name: \"test\", size: 100]]; port",
        )
        .expect_int(8080);
}

#[test]
fn test_multiple_assignments() {
    quiver()
        .evaluate("x = 1; y = 2; z = 3; [x, y, z] ~> +")
        .expect_int(6);
    quiver()
        .evaluate("[a, b] = [10, 20]; [c, d] = [a, b]; c")
        .expect_int(10);
}

#[test]
fn test_assignment_in_expressions() {
    quiver()
        .evaluate("[x = 5, x * 2]")
        .expect_tuple(vec![Value::Tuple(TypeId::OK, vec![]), Value::Integer(10)]);
    quiver().evaluate("(a = [1, 2] ~> +) * 3").expect_int(9);
}
