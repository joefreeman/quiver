mod common;
use common::*;

#[test]
fn test_simple_merge() {
    quiver()
        .evaluate("Point[x: 1, y: 2] ~> [x: 3]")
        .expect("Point[x: 3, y: 2]");
}

#[test]
fn test_merge_with_name_match() {
    quiver()
        .evaluate("Point[x: 1, y: 2] ~> Point[x: 3]")
        .expect("Point[x: 3, y: 2]");
}

#[test]
fn test_merge_with_name_mismatch() {
    quiver()
        .evaluate("Point[x: 1, y: 2] ~> Other[x: 3]")
        .expect("[]");
}

#[test]
fn test_merge_mixed_fields() {
    quiver()
        .evaluate("A[1, x: 2, 3, y: 4] ~> [x: 5]")
        .expect("A[1, x: 5, 3, y: 4]");
}

#[test]
fn test_nested_merge_simple() {
    quiver()
        .evaluate("A[B[C[x: 1, y: 2, z: 3], 4], 5] ~> [[[y: 6]]]")
        .expect("A[B[C[x: 1, y: 6, z: 3], 4], 5]");
}

#[test]
fn test_nested_merge_with_name() {
    quiver()
        .evaluate("A[B[C[x: 1, y: 2, z: 3], 4], 5] ~> [B[[x: 7]]]")
        .expect("A[B[C[x: 7, y: 2, z: 3], 4], 5]");
}

#[test]
fn test_nested_merge_with_name_mismatch() {
    quiver()
        .evaluate("A[B[C[x: 1, y: 2, z: 3], 4], 5] ~> [X[[x: 8]]]")
        .expect("[]");
}
