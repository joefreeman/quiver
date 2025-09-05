mod common;
use common::*;

#[test]
fn test_countdown() {
    quiver()
        .evaluate("countdown = #int { [$, 0] ~> == => 0 | [$, 1] ~> - ~> & }, 5 ~> countdown")
        .expect_int(0);
}

#[test]
fn test_factorial() {
    quiver().evaluate("f = #[int, int] { [$0, 1] ~> <= => $1 | [[$0, 1] ~> -, [$0, $1] ~> *] ~> & }, fact = #int { [$, 1] ~> f }, 5 ~> fact").expect_int(120);
}
