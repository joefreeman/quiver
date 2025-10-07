mod common;
use common::*;

#[test]
fn test_unnamed_tuple() {
    quiver().evaluate("[1, 2]").expect("[1, 2]");
}

#[test]
fn test_named_tuple_with_named_fields() {
    quiver()
        .evaluate("Point[x: 1, y: 2]")
        .expect("Point[x: 1, y: 2]");
}

#[test]
fn test_named_tuple_with_unnamed_field() {
    quiver().evaluate("A[1]").expect("A[1]");
}

#[test]
fn test_empty_named_tuple() {
    quiver().evaluate("Ok").expect("Ok");
}

#[test]
fn test_empty_unnamed_tuple() {
    quiver().evaluate("[]").expect("[]");
}

#[test]
fn test_mixed_tuple() {
    quiver().evaluate("[a: 1, 2]").expect("[a: 1, 2]");
}
