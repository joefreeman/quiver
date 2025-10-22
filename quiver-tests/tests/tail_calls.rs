mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            g = #int { ~> =x => [x, 2] ~> __multiply__ },
            f = #int { ~> =x => [x, 1] ~> __add__ ~> &g },
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
              | ~> =x => [x, 1] ~> __subtract__ ~> &
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
            g = #[int, int] { ~> =[x, y] => math.mul[x, y] },
            f = #int { ~> =x => math.add[x, 1] ~> &g[~ , 2] },
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
            fact = #int { ~> =x => f[x, 1] },
            5 ~> fact
            "#,
        )
        .expect("120");
}
