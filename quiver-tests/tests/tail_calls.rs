mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            g = #'int { [~, 2] __integer_multiply__ },
            f = #'int { [~, 1] __integer_add__ ^g },
            1 f
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
              | [~, 1] __integer_subtract__ ^
            },
            5 countdown
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
            f = #'int { [~, 1] %num.add [~ , 2] ^g },
            1 f
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
                [x, 1] __integer_subtract__,
                [x, y] __integer_multiply__
              ] ^
            },
            fact = #'int { [~, 1] f },
            5 fact
            "#,
        )
        .expect("120");
}

#[test]
fn test_tail_call_with_nil_argument() {
    quiver().evaluate("f = #{ [] ^ }");
    quiver().evaluate("f = #{ ^ }");
}

#[test]
fn tail_call_with_argument_is_argument_first() {
    // There is no form that tail-calls the flowing function *with* an explicit argument;
    // tail-call a named function argument-first instead (`x ^g`). Bare `^~` survives only
    // without an argument (nilary target).
    quiver()
        .evaluate(
            r#"
            g = #'int { [~, 2] __integer_multiply__ },
            f = #'int { $ ^g },
            5 f
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
            f = #{ &g ^~ },
            [] f
            "#,
        )
        .expect("42");
}

#[test]
fn test_ripple_tail_call_requires_function() {
    // `^~` on a non-function flowing value is a type error. (Bare `^~`, since the
    // argument-supplying `^~ x` form was removed; the flowing int is not callable.)
    quiver()
        .evaluate("f = #'int { ^~ }, 5 f")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function".to_string(),
            found: "'int".to_string(),
        });
}
