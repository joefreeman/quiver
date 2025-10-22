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
        .evaluate("inc = #int { ~> =x => [x, 1] ~> __add__ }, 3 ~> inc")
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
              ~> =Point[x: x, y: y] => [x, y] ~> __add__
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
            double = #int { ~> =x => [x, 2] ~> __multiply__ },
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
              ~> =x => #int { ~> =y => [x, y] ~> __add__ }
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

#[test]
fn test_function_call_syntax() {
    quiver()
        .evaluate("math = %\"math\", math.add[3, 4]")
        .expect("7");
}

#[test]
fn test_function_call_with_ripple() {
    quiver()
        .evaluate("math = %\"math\", math.add[1, 2] ~> math.mul[~, 3]")
        .expect("9");
}

#[test]
fn test_function_call_no_args() {
    quiver().evaluate("f = #{ 42 }, f[]").expect("42");
}

#[test]
fn test_function_call_field_access() {
    quiver()
        .evaluate("math = %\"math\", math ~> .add[1, 2]")
        .expect("3");
}

#[test]
fn test_function_result_covariance() {
    quiver().evaluate(
        r#"
        f = #(#bin -> (Ok | [])) { ~> },
        g = #bin { [] },
        g ~> f
        "#,
    );
}

#[test]
fn test_function_parameter_contravariance() {
    quiver().evaluate(
        r#"
        f = #(#[] -> bin) { ~> },
        g = #(Ok | []) { '00' },
        g ~> f
        "#,
    );
}

#[test]
fn test_apply_value_to_inline_function() {
    quiver()
        .evaluate("5 ~> #int { ~> [~, 2] ~> __add__ }")
        .expect("7");
}
