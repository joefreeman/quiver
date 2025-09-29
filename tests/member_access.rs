mod common;
use common::*;

#[test]
fn test_nested_member_access_as_value() {
    quiver()
        .evaluate("A[a: [10, B[b: 20]]] ~> x, x.a.1.b")
        .expect("20");
}

#[test]
fn test_nested_member_access_as_operation() {
    quiver()
        .evaluate(
            r#"
            #int { ~> x => [x, 1] ~> <add>! } ~> inc,
            [0, [f: inc]] ~> x,
            4 ~> x.1.f!
            "#,
        )
        .expect("5");
}
