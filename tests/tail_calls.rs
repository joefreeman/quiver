mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            g = #int { [$, 2] ~> * },
            f = #int { [$, 1] ~> + ~> &g },
            1 ~> f
            "#,
        )
        .expect_int(4)
}

#[test]
fn test_countdown() {
    quiver()
        .evaluate(
            r#"
            countdown = #int {
              | [$, 0] ~> == => 0
              | [$, 1] ~> - ~> &
            },
            5 ~> countdown
            "#,
        )
        .expect_int(0);
}

#[test]
fn test_factorial() {
    quiver()
        .evaluate(
            r#"
            f = #[int, int] {
            | [$0, 1] ~> <= => $1
            | [[$0, 1] ~> -, [$0, $1] ~> *] ~> &
            },
            fact = #int { [$, 1] ~> f },
            5 ~> fact
            "#,
        )
        .expect_int(120);
}
