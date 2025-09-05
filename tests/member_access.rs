mod common;
use common::*;

#[test]
fn test_nested_member_access() {
    quiver()
        .evaluate("x = A[a: [10, B[b: 20]]], x.a.1.b")
        .expect_int(20);
}
