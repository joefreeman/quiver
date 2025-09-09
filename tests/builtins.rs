mod common;
use common::*;

// Math module tests
#[test]
fn test_math_abs_positive() {
    quiver()
        .evaluate("math = %\"math\"; 42 ~> math.abs")
        .expect_int(42);
}

#[test]
fn test_math_abs_negative() {
    quiver()
        .evaluate("math = %\"math\"; -42 ~> math.abs")
        .expect_int(42);
}

#[test]
fn test_math_abs_zero() {
    quiver()
        .evaluate("math = %\"math\"; 0 ~> math.abs")
        .expect_int(0);
}

#[test]
fn test_math_sqrt() {
    quiver()
        .evaluate("math = %\"math\"; 16 ~> math.sqrt")
        .expect_int(4);
}

#[test]
fn test_math_sqrt_zero() {
    quiver()
        .evaluate("math = %\"math\"; 0 ~> math.sqrt")
        .expect_int(0);
}

#[test]
fn test_math_sqrt_negative() {
    quiver()
        .evaluate("math = %\"math\"; -1 ~> math.sqrt")
        .expect_error();
}

#[test]
fn test_math_sin() {
    quiver()
        .evaluate("math = %\"math\"; 0 ~> math.sin")
        .expect_int(0);
}

#[test]
fn test_math_cos() {
    quiver()
        .evaluate("math = %\"math\"; 0 ~> math.cos")
        .expect_int(1);
}

// IO module tests
#[test]
fn test_io_print_integer() {
    quiver()
        .evaluate("io = %\"io\"; 42 ~> io.print")
        .expect_ok();
}

#[test]
fn test_io_println_integer() {
    quiver()
        .evaluate("io = %\"io\"; 42 ~> io.println")
        .expect_ok();
}

#[test]
fn test_io_print_string() {
    quiver()
        .evaluate("io = %\"io\"; \"Hello\" ~> io.print")
        .expect_ok();
}

#[test]
fn test_io_println_string() {
    quiver()
        .evaluate("io = %\"io\"; \"Hello\" ~> io.println")
        .expect_ok();
}

// Module destructuring tests
#[test]
fn test_math_destructuring() {
    quiver()
        .evaluate("(abs, sqrt) = %\"math\"; -25 ~> abs")
        .expect_int(25);
}

#[test]
fn test_io_destructuring() {
    quiver()
        .evaluate("(print) = %\"io\"; 42 ~> print")
        .expect_ok();
}

#[test]
fn test_partial_destructuring() {
    quiver()
        .evaluate("(println) = %\"io\"; \"Test\" ~> println")
        .expect_ok();
}

// Chaining tests
#[test]
fn test_math_chaining() {
    quiver()
        .evaluate("math = %\"math\"; -16 ~> math.abs ~> math.sqrt")
        .expect_int(4);
}

#[test]
fn test_mixed_module_usage() {
    quiver()
        .evaluate(
            r#"
        math = %"math";
        io = %"io";
        result = -42 ~> math.abs;
        result ~> io.print
    "#,
        )
        .expect_ok();
}

// Error cases
#[test]
fn test_unknown_module() {
    quiver().evaluate("%\"unknown\"").expect_error();
}

#[test]
fn test_unknown_function_access() {
    quiver()
        .evaluate("math = %\"math\"; math.unknown")
        .expect_error();
}

// Type compatibility tests
#[test]
fn test_builtin_function_type() {
    // Test that builtins can be stored in variables and passed around
    quiver()
        .evaluate(
            r#"
        math = %"math";
        abs_fn = math.abs;
        -10 ~> abs_fn
    "#,
        )
        .expect_int(10);
}

#[test]
fn test_multiple_modules() {
    quiver()
        .evaluate(
            r#"
        math = %"math";
        io = %"io";
        result = 25 ~> math.sqrt;
        result ~> io.println
    "#,
        )
        .expect_ok();
}
