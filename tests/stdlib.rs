mod common;
use common::*;

#[test]
fn test_stdlib_io_import() {
    quiver()
        .evaluate(
            r#"
            io = %"io",
            42 ~> io.println,
            "Hello, world!" ~> io.println
        "#,
        )
        .expect_ok();
}
