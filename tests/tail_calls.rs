mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            #int { [$, 2] ~> <multiply> } ~> ^g,
            #int { [$, 1] ~> <add> ~> &g } ~> ^f,
            1 ~> f
            "#,
        )
        .expect("4")
}

#[test]
fn test_countdown() {
    quiver()
        .evaluate(
            r#"
            #int {
              | ^0 => 0
              | [$, 1] ~> <subtract> ~> &
            } ~> ^countdown,
            5 ~> countdown
            "#,
        )
        .expect("0");
}

#[test]
fn test_factorial() {
    quiver()
        .evaluate(
            r#"
            #[int, int] {
              | [$.0, 1] ~> <compare> ~> { ^-1 => $ | ^0 => $ } => $.1
              | [[$.0, 1] ~> <subtract>, [$.0, $.1] ~> <multiply>] ~> &
            } ~> ^f,
            #int { [$, 1] ~> f } ~> ^fact,
            5 ~> fact
            "#,
        )
        .expect("120");
}
