mod common;
use common::*;
use quiver::vm::Value;

#[test]
fn test_unnamed_tuple() {
    quiver().evaluate("[1, 2]").expect_tuple(
        None,
        vec![None, None],
        vec![Value::Integer(1), Value::Integer(2)],
    );
}

#[test]
fn test_named_tuple_with_named_fields() {
    quiver().evaluate("Point[x: 1, y: 2]").expect_tuple(
        Some("Point"),
        vec![Some("x"), Some("y")],
        vec![Value::Integer(1), Value::Integer(2)],
    );
}

#[test]
fn test_named_tuple_with_unnamed_field() {
    quiver()
        .evaluate("A[1]")
        .expect_tuple(Some("A"), vec![None], vec![Value::Integer(1)]);
}

#[test]
fn test_empty_named_tuple() {
    quiver().evaluate("Ok").expect_ok();
}

#[test]
fn test_empty_unnamed_tuple() {
    quiver().evaluate("[]").expect_nil();
}

#[test]
fn test_mixed_tuple() {
    quiver().evaluate("[a: 1, 2]").expect_tuple(
        None,
        vec![Some("a"), None],
        vec![Value::Integer(1), Value::Integer(2)],
    );
}
