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
        .evaluate("inc = #int { ~> =x => [x, 1] ~> <add> }, 3 ~> inc")
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
        .evaluate(
            r#"
            f = #Point[x: int, y: int] {
              ~> =Point[x: x, y: y] => [x, y] ~> <add>
            },
            Point[x: 1, y: 2] ~> f
            "#,
        )
        .expect("3");
}

#[test]
fn test_function_with_enumerated_type_parameter() {
    quiver()
        .evaluate("f = #(int | bin) { ~> =x => x }, '0a1b2c' ~> f")
        .expect("'0a1b2c'");
}

#[test]
fn test_higher_order_function() {
    quiver()
        .evaluate(
            r#"
            apply = #[#int -> int, int] { ~> =[f, x] => x ~> f },
            double = #int { ~> =x => [x, 2] ~> <multiply> },
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
              ~> =x => #int { ~> =y => [x, y] ~> <add> }
            },
            3 ~> f ~> =g,
            5 ~> g
            "#,
        )
        .expect("8");
}

#[test]
fn test_closure_captures_member_accesses() {
    quiver()
        .evaluate(
            r#"
            math = %"math",
            double_plus_one = #int {
              ~> =x => [[x, 2] ~> math.mul, 1] ~> math.add
            },
            5 ~> double_plus_one
            "#,
        )
        .expect("11");
}

#[test]
fn test_closure_captures_nested_member_access() {
    quiver()
        .evaluate(
            r#"
            obj = [inner: [value: 42]],
            get_value = #{ obj.inner.value },
            [] ~> get_value
            "#,
        )
        .expect("42");
}

#[test]
fn test_nested_function_captures() {
    quiver()
        .evaluate(
            r#"
            math = %"math",
            f = #{
              inc = #int { ~> [~, 1] ~> math.add },
              42 ~> inc
            },
            [] ~> f
            "#,
        )
        .expect("43");
}
