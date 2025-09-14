mod common;
use common::*;

#[test]
fn test_simple_function() {
    quiver().evaluate("f = #{ 42 }, [] ~> f").expect("42");
}

#[test]
fn test_nil_function() {
    quiver().evaluate("f = #{ [] }, [] ~> f").expect("[]");
}

#[test]
fn test_function_with_parameter() {
    quiver()
        .evaluate("inc = #int { [$, 1] ~> <add> }, 3 ~> inc")
        .expect("4");
}

#[test]
fn test_function_closure() {
    quiver()
        .evaluate("x = 1, f = #{ x }, x = 2, [] ~> f")
        .expect("1");
}

#[test]
fn test_function_with_tuple_parameter() {
    quiver()
        .evaluate("f = #Point[x: int, y: int] { [x, y] ~> <add> }, Point[x: 1, y: 2] ~> f")
        .expect("3");
}

#[test]
fn test_function_with_enumerated_type_parameter() {
    quiver()
        .evaluate("f = #(int | bin) { $ }, \"hello\" ~> f")
        .expect("'68656c6c6f'");
}

#[test]
fn test_higher_order_function() {
    quiver()
        .evaluate(
            r#"
            apply = #[#int -> int, int] { $.1 ~> $.0 };
            double = #int { [$, 2] ~> <multiply> };
            [double, 5] ~> apply
            "#,
        )
        .expect("10");
}

#[test]
fn test_nested_function_return() {
    quiver()
        .evaluate(
            r#"
            f = #int {
              x = $,
              #int { [$, x] ~> <add> }
            },
            g = 3 ~> f,
            5 ~> g
            "#,
        )
        .expect("8");
}
