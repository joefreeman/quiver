mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            g = #'int { [~, 2] ~> __multiply__ },
            f = #'int { [~, 1] ~> __add__ ~> ^g },
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
            countdown = #'int {
              | =0 => 0
              | [~, 1] ~> __subtract__ ~> ^
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
            g = #['int, 'int] { %num.mul },
            f = #'int { %num.add [~, 1] ~> ^g [~ , 2] },
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
            f = #['int, 'int] {
              | =[1, y] => y
              | =[x, y] => [
                [x, 1] ~> __subtract__,
                [x, y] ~> __multiply__
              ] ~> ^
            },
            fact = #'int { f [~, 1] },
            5 ~> fact
            "#,
        )
        .expect("120");
}

#[test]
fn test_tail_call_with_nil_argument() {
    quiver().evaluate("f = #{ ^ [] }");
    quiver().evaluate("f = #{ ^ }");
}

#[test]
fn test_ripple_tail_call_with_argument() {
    // `^~ x` tail-calls the flowing value (here `g`) with `x`. Non-recursive, so it terminates.
    quiver()
        .evaluate(
            r#"
            g = #'int { [~, 2] ~> __multiply__ },
            f = #'int { &g ~> ^~ $ },
            5 ~> f
            "#,
        )
        .expect("10");
}

#[test]
fn test_ripple_tail_call_without_argument() {
    // Bare `^~` tail-calls the flowing nil-parameter function.
    quiver()
        .evaluate(
            r#"
            g = #{ 42 },
            f = #{ &g ~> ^~ },
            f []
            "#,
        )
        .expect("42");
}

#[test]
fn test_ripple_tail_call_requires_function() {
    // `^~` on a non-function flowing value is a type error.
    quiver()
        .evaluate("f = #'int { ^~ 3 }, 5 ~> f")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function".to_string(),
            found: "'int".to_string(),
        });
}
