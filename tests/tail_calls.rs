mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            #int { x => [x, 2] ~> <multiply> } ~> g,
            #int { x => [x, 1] ~> <add> ~> &g } ~> f,
            1 ~> f!
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
              | 0 => 0
              | x => [x, 1] ~> <subtract> ~> &
            } ~> countdown,
            5 ~> countdown!
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
              | [1, y] => y
              | [x, y] => [[x, 1] ~> <subtract>, [x, y] ~> <multiply>] ~> &
            } ~> f,
            #int { x => [x, 1] ~> f! } ~> fact,
            5 ~> fact!
            "#,
        )
        .expect("120");
}
