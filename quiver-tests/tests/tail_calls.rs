mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            g = #int { ~> [~, 2] ~> __multiply__ },
            f = #int { ~> [~, 1] ~> __add__ ~> &g },
            1 ~> f
            "#,
        )
        .expect("4");
}

#[test]
fn test_countdown() {
    quiver()
        .evaluate(
            r#"
            countdown = #int {
              | ~> =0 => 0
              | ~> [~, 1] ~> __subtract__ ~> &
            },
            5 ~> countdown
            "#,
        )
        .expect("0");
}

#[test]
fn test_tail_call_with_arguments() {
    quiver()
        .evaluate(
            r#"
            math = %"math",
            g = #[int, int] { ~> math.mul },
            f = #int { ~> math.add[~, 1] ~> &g[~ , 2] },
            1 ~> f
            "#,
        )
        .expect("4");
}

#[test]
fn test_factorial() {
    quiver()
        .evaluate(
            r#"
            f = #[int, int] {
              | ~> =[1, y] => y
              | ~> =[x, y] => [
                [x, 1] ~> __subtract__,
                [x, y] ~> __multiply__
              ] ~> &
            },
            fact = #int { ~> f[~, 1] },
            5 ~> fact
            "#,
        )
        .expect("120");
}
