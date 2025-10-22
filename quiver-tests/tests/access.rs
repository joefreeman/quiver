mod common;
use common::*;

#[test]
fn test_nested_member_access_as_value() {
    quiver()
        .evaluate("x = A[a: [10, B[b: 20]]], x.a.1.b")
        .expect("20");
}

#[test]
fn test_nested_member_access_as_operation() {
    quiver()
        .evaluate(
            r#"
            inc = #int { ~> =x => [x, 1] ~> __add__ },
            x = [0, [f: inc]],
            4 ~> x.1.f
            "#,
        )
        .expect("5");
}
