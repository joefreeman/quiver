mod common;
use common::*;

#[test]
fn test_tail_call() {
    quiver()
        .evaluate(
            r#"
            g = #'int { [~, 2] ~> __integer_multiply__ ~ };
            f = #'int { [~, 1] ~> __integer_add__ ~ ~> ^g ~ };
            1 ~> f ~
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
              | [~, 1] ~> __integer_subtract__ ~ ~> ^ ~
            };
            5 ~> countdown ~
            "#,
        )
        .expect("0");
}

#[test]
fn test_tail_call_with_arguments() {
    quiver()
        .evaluate(
            r#"
            g = #['int, 'int] { %num.mul ~ };
            f = #'int { [~, 1] ~> %num.add ~ ~> [~ , 2] ~> ^g ~ };
            1 ~> f ~
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
                [x, 1] ~> __integer_subtract__ ~,
                [x, y] ~> __integer_multiply__ ~
              ] ~> ^ ~
            };
            fact = #'int { [~, 1] ~> f ~ };
            5 ~> fact ~
            "#,
        )
        .expect("120");
}

#[test]
fn test_tail_call_with_nil_argument() {
    quiver().evaluate("f = #[] { [] ~> ^ ~ }");
    quiver().evaluate("f = #[] { ^ }");
}

#[test]
fn tail_call_with_argument_is_argument_first() {
    // There is no form that tail-calls the flowing function *with* an explicit argument;
    // tail-call a named function argument-first instead (`x ^g`). Bare `^~` survives only
    // without an argument (nilary target).
    quiver()
        .evaluate(
            r#"
            g = #'int { [~, 2] ~> __integer_multiply__ ~ };
            f = #'int { $ ~> ^g ~ };
            5 ~> f ~
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
            g = #[] { 42 };
            f = #[] { g ~> ^~ [] };
            [] ~> f ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_ripple_tail_call_requires_function() {
    // `^~` on a non-function flowing value is a type error. (Bare `^~`, since the
    // argument-supplying `^~ x` form was removed; the flowing int is not callable.)
    quiver()
        .evaluate("f = #'int { ^~ [] }; 5 ~> f ~")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function".to_string(),
            found: "'int".to_string(),
        });
}

#[test]
fn test_self_tail_call_argument_is_type_checked() {
    // Bare `^` re-enters the function, so its argument must fit the declared
    // parameter (regression: this used to compile unchecked and fail at runtime
    // as a field access).
    quiver()
        .evaluate("f = #'int { ^ [1, 2] }; 1 ~> f ~")
        .expect_error_containing("function parameter compatible with");
}

#[test]
fn test_named_tail_call_argument_is_type_checked() {
    // `^f` is checked against f's parameter type, like a normal call.
    quiver()
        .evaluate(
            r#"
            g = #'int { $ };
            f = #'int { ^g [1, 2] };
            1 ~> f ~
            "#,
        )
        .expect_error_containing("function parameter compatible with");
}

#[test]
fn test_self_tail_call_checks_declared_parameter_not_narrowed() {
    // Inside a branch the pattern narrows the parameter's type; `^` re-enters the
    // whole function (every branch re-dispatches), so the recursion argument is
    // checked against the *declared* parameter — here the full list union, not the
    // branch's Cons narrowing.
    quiver()
        .evaluate(
            r#"
            'ints = Nil | Cons['int, ^];
            sum = #['int, 'ints] {
              | =[acc, Nil] => acc
              | =[acc, Cons[x, rest]] => ^ [__integer_add__ [acc, x], rest]
            };
            [0, Cons[1, Cons[2, Cons[3, Nil]]]] ~> sum ~
            "#,
        )
        .expect("6");
}

#[test]
fn test_bare_tail_call_checks_argument_arity() {
    // A bare `^` self tail-call's argument is checked against the function's own
    // parameter: a wrong-arity tuple is a compile error, not a runtime field fault.
    quiver()
        .evaluate(
            r#"f = #['int, 'int] {
                 =[a, b]
                 { | a ~> =0 => b | ^ [a] }
               }
               f [2, 5]"#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with ['int, 'int]".to_string(),
            found: "['int]".to_string(),
        });
}
