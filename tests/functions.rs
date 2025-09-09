mod common;
use common::*;

#[test]
fn test_simple_function() {
    quiver().evaluate("f = #{ 42 }, [] ~> f").expect_int(42);
}

#[test]
fn test_function_with_parameter() {
    quiver()
        .evaluate("inc = #int { [$, 1] ~> + }, 3 ~> inc")
        .expect_int(4);
}

#[test]
fn test_function_closure() {
    quiver()
        .evaluate("x = 1, f = #{ x }, x = 2, [] ~> f")
        .expect_int(1);
}

#[test]
fn test_function_with_tuple_parameter() {
    quiver()
        .evaluate("f = #Point[x: int, y: int] { [x, y] ~> + }, Point[x: 1, y: 2] ~> f")
        .expect_int(3);
}

#[test]
fn test_function_with_enumerated_type_parameter() {
    quiver()
        .evaluate("f = #(int | bin) { $ }, \"hello\" ~> f")
        .expect_binary(0);
}

#[test]
fn test_higher_order_function() {
    quiver()
        .evaluate(
            r#"
            apply = #[#int -> int, int] { $1 ~> $0 };
            double = #int { [$, 2] ~> * };
            [double, 5] ~> apply
            "#,
        )
        .expect_int(10);
}

#[test]
fn test_nested_function_return() {
    quiver()
        .evaluate(
            r#"
            f = #int {
              x = $,
              #int { [$, x] ~> + }
            },
            g = 3 ~> f,
            5 ~> g
            "#,
        )
        .expect_int(8);
}
