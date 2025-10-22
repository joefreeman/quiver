mod common;
use common::*;

#[test]
fn test_comments() {
    quiver().evaluate("// comment").expect("");
    quiver().evaluate("//comment").expect("");
    quiver().evaluate("5 // comment").expect("5");
    quiver()
        .evaluate(
            r#"
            #{
              5 // comment
            }
            "#,
        )
        .expect("#0");
    quiver()
        .evaluate(
            r#"
            // start
            Point[ // point
              x: 10, // x
              y: 20, // y
            ] // end
            "#,
        )
        .expect("Point[x: 10, y: 20]");
}
