mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            g = #int { ~> =x => [x, 2] ~> <multiply>! },
            f = #int { ~> =x => [x, 1] ~> <add>! ~> &g },
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
            countdown = #int {
              | ~> =0 => 0
              | ~> =x => [x, 1] ~> <subtract>! ~> &
            },
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
            f = #[int, int] {
              | ~> =[1, y] => y
              | ~> =[x, y] => [
                [x, 1] ~> <subtract>!,
                [x, y] ~> <multiply>!
              ] ~> &
            },
            fact = #int { ~> =x => [x, 1] ~> f! },
            5 ~> fact!
            "#,
        )
        .expect("120");
}
