mod common;
use common::*;

#[test]
fn test_simple_function() {
    quiver()
        .evaluate("#{ [] => 42 } ~> f, [] ~> f!")
        .expect("42");
}

#[test]
fn test_nil_function() {
    quiver()
        .evaluate("#{ [] => [] } ~> f, [] ~> f!")
        .expect("[]");
}

#[test]
fn test_function_with_parameter() {
    quiver()
        .evaluate("#int { x => [x, 1] ~> <add>! } ~> inc, 3 ~> inc!")
        .expect("4");
}

#[test]
fn test_function_closure() {
    quiver()
        .evaluate("1 ~> x, #{ [] => x } ~> f, 2 ~> x, [] ~> f!")
        .expect("1");
}

#[test]
fn test_function_with_tuple_parameter() {
    quiver()
        .evaluate(
            r#"
            #Point[x: int, y: int] {
              Point[x: x, y: y] => [x, y] ~> <add>!
            } ~> f,
            Point[x: 1, y: 2] ~> f!
            "#,
        )
        .expect("3");
}

#[test]
fn test_function_with_enumerated_type_parameter() {
    quiver()
        .evaluate("#(int | bin) { x => x } ~> f, \"hello\" ~> f!")
        .expect("'68656c6c6f'");
}

#[test]
fn test_higher_order_function() {
    quiver()
        .evaluate(
            r#"
            #[#int -> int, int] { [f, x] => x ~> f! } ~> apply;
            #int { x => [x, 2] ~> <multiply>! } ~> double;
            [double, 5] ~> apply!
            "#,
        )
        .expect("10");
}

#[test]
fn test_nested_function_return() {
    quiver()
        .evaluate(
            r#"
            #int {
              x => #int { y => [x, y] ~> <add>! }
            } ~> f,
            3 ~> f! ~> g,
            5 ~> g!
            "#,
        )
        .expect("8");
}
