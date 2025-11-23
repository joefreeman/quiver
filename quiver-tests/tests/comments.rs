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
        .expect_type("#[] -> int");
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
        .expect_type("Point[x: int, y: int]");
}
