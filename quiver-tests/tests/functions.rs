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
        .evaluate("inc = #'int { =x => [x, 1] ~> __integer_add__ }, 3 ~> inc")
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
            f = #Point[x: 'int, y: 'int] {
              =Point[x: x, y: y] => [x, y] ~> __integer_add__
            },
            Point[x: 1, y: 2] ~> f
            "#,
        )
        .expect("3");
}

#[test]
fn test_function_with_enumerated_type_parameter() {
    quiver()
        .evaluate("f = #('int | 'bin) { =x => x }, 0x0a1b2c ~> f")
        .expect("0x0a1b2c");
}

#[test]
fn test_higher_order_function() {
    quiver()
        .evaluate(
            r#"
            apply = #[#'int -> 'int, 'int] { =[f, x] => x ~> f },
            double = #'int { =x => [x, 2] ~> __integer_multiply__ },
            [&double, 5] ~> apply
            "#,
        )
        .expect("10");
}

#[test]
fn test_nested_function_return() {
    quiver()
        .evaluate(
            r#"
            f = #'int {
              =x => #'int { =y => [x, y] ~> __integer_add__ }
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
            double_plus_one = #'int {
              =x => [[x, 2] ~> %num.mul, 1] ~> %num.add
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
            f = #{
              inc = #'int { [~, 1] ~> %num.add },
              42 ~> inc
            },
            [] ~> f
            "#,
        )
        .expect("43");
}

#[test]
fn test_function_call_syntax() {
    quiver().evaluate("%num.add [3, 4]").expect("7");
}

#[test]
fn test_function_call_with_ripple() {
    quiver()
        .evaluate("%num.add [1, 2] ~> %num.mul [~, 3]")
        .expect("9");
}

#[test]
fn test_function_call_no_args() {
    quiver().evaluate("f = #{ 42 }, f []").expect("42");
}

#[test]
fn calling_a_field_access_directly_is_unsupported() {
    // `.field [args]` is intentionally dropped — `%num ~> .add [1, 2]` reads confusingly.
    // Bind the function first, or use a tail call.
    quiver()
        .evaluate("%num ~> .add [1, 2]")
        .expect_parse_failure();
}

#[test]
fn test_function_call_with_spread() {
    quiver()
        .evaluate(
            r#"
            f = #['int, 'int, 'int] {
              $.0
              ~> %num.add [~, $.1]
              ~> %num.add [~, $.2]
            },
            [1, 2] ~> f [..., 3]
            "#,
        )
        .expect("6");
}

#[test]
fn calling_via_a_bare_ripple() {
    // `~ [args]` applies the flowing value (a function) to the argument.
    quiver().evaluate("&%num.add ~> ~ [3, 4]").expect("7");
}

#[test]
fn calling_via_a_ripple_field_access() {
    // `~.field [args]` reads a field off the flowing value and applies the argument to it.
    quiver().evaluate("%num ~> ~.add [1, 2]").expect("3");
}

#[test]
fn test_ripple_field_access() {
    quiver().evaluate("[x: 5, y: 10] ~> ~.x").expect("5");
}

#[test]
fn test_function_result_covariance() {
    quiver().evaluate(
        r#"
        f = #(#'bin -> (Ok | [])) { ~ },
        g = #'bin { [] },
        g ~> f
        "#,
    );
}

#[test]
fn test_function_parameter_contravariance() {
    quiver().evaluate(
        r#"
        f = #(#[] -> 'bin) { ~ },
        g = #(Ok | []) { 0x00 },
        g ~> f
        "#,
    );
}

#[test]
fn test_apply_value_to_inline_function() {
    // Inline functions are not auto-called; bind first, then call
    quiver()
        .evaluate("f = #'int { [~, 2] ~> __integer_add__ }, 5 ~> f")
        .expect("7");
}

#[test]
fn test_identity_function_int() {
    quiver().evaluate("f = #'int, 42 ~> f").expect("42");
}

#[test]
fn test_identity_function_bin() {
    quiver()
        .evaluate("f = #'bin, 0x0a1b2c ~> f")
        .expect("0x0a1b2c");
}

#[test]
fn test_identity_function_tuple() {
    quiver()
        .evaluate("f = #Point[x: 'int, y: 'int], Point[x: 1, y: 2] ~> f")
        .expect("Point[x: 1, y: 2]");
}

#[test]
fn test_identity_function_inline() {
    // Inline functions are not auto-called; bind first, then call
    quiver().evaluate("f = #'int, 42 ~> f").expect("42");
}

#[test]
fn test_identity_function_with_type_parameter() {
    quiver()
        .evaluate(
            r#"
            id = #<'t>'t,
            42 ~> id
            "#,
        )
        .expect("42");
}

#[test]
fn test_dollar_parameter_reference() {
    quiver().evaluate("f = #'int { $ }, 5 ~> f").expect("5");
}

#[test]
fn test_dollar_with_tuple_field_access() {
    quiver()
        .evaluate("f = #['int, 'int] { [$.0, $.1] ~> __integer_add__ }, [10, 20] ~> f")
        .expect("30");
}

#[test]
fn test_dollar_dotless_index_shorthand() {
    quiver()
        .evaluate("f = #['int, 'int] { [$0, $1] ~> __integer_add__ }, [10, 20] ~> f")
        .expect("30");
}

#[test]
fn test_dollar_dotless_field_shorthand() {
    quiver()
        .evaluate("f = #Point[x: 'int, y: 'int] { [$x, $y] ~> __integer_add__ }, Point[x: 10, y: 20] ~> f")
        .expect("30");
}

#[test]
fn test_dollar_dotless_shorthand_then_dotted() {
    quiver()
        .evaluate("f = #[p: [x: 'int]] { $p.x }, [p: [x: 7]] ~> f")
        .expect("7");
}

#[test]
fn test_dollar_in_nested_block() {
    quiver()
        .evaluate("f = #'int { 100 ~> { __integer_add__ [~, $] } }, 7 ~> f")
        .expect("107");
}

#[test]
fn test_dollar_with_named_tuple_field() {
    quiver()
        .evaluate(
            r#"
            f = #Point[x: 'int, y: 'int] { [$.x, $.y] ~> __integer_add__ },
            Point[x: 10, y: 20] ~> f
            "#,
        )
        .expect("30");
}

#[test]
fn test_function_with_return_type() {
    quiver()
        .evaluate("f = #'int -> 'int { __integer_add__ [~, 1] }, 5 ~> f")
        .expect("6");
}

#[test]
fn test_function_return_type_mismatch() {
    quiver()
        .evaluate("f = #'int -> 'bin { __integer_add__ [~, 1] }, 5 ~> f")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'bin".to_string(),
            found: "'int".to_string(),
        });
}

#[test]
fn test_function_with_tuple_return_type() {
    quiver()
        .evaluate(
            r#"
            f = #['int, 'int] -> 'int { [$.0, $.1] ~> __integer_add__ },
            [3, 4] ~> f
            "#,
        )
        .expect("7");
}

#[test]
fn test_function_tuple_return_type_mismatch() {
    quiver()
        .evaluate(
            r#"
            f = #['int, 'int] -> 'bin { [$.0, $.1] ~> __integer_add__ },
            [3, 4] ~> f
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'bin".to_string(),
            found: "'int".to_string(),
        });
}

#[test]
fn test_identity_function_with_return_type() {
    quiver().evaluate("f = #'int -> 'int, 42 ~> f").expect("42");
}

#[test]
fn test_identity_function_return_type_mismatch() {
    quiver()
        .evaluate("f = #'int -> 'bin, 42 ~> f")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'bin".to_string(),
            found: "'int".to_string(),
        });
}

#[test]
fn test_function_with_generic_return_type() {
    quiver()
        .evaluate(
            r#"
            id = #<'t>'t -> 't { ~ },
            42 ~> id
            "#,
        )
        .expect("42");
}

#[test]
fn test_function_with_generic_return_type_string() {
    quiver()
        .evaluate(
            r#"
            id = #<'t>'t -> 't { ~ },
            "hello" ~> id
            "#,
        )
        .expect("\"hello\"");
}

#[test]
fn test_function_with_complex_return_type() {
    quiver()
        .evaluate(
            r#"
            double = #'int -> 'int { __integer_multiply__ [~, 2] },
            square = #'int -> 'int { __integer_multiply__ [~, ~] },
            5 ~> double ~> square
            "#,
        )
        .expect("100");
}

#[test]
fn test_function_with_named_tuple_return_type() {
    quiver()
        .evaluate(
            r#"
            f = #'int -> Point[x: 'int, y: 'int] {
                =n => Point[x: n, y: [n, n] ~> __integer_multiply__]
            },
            3 ~> f
            "#,
        )
        .expect("Point[x: 3, y: 9]");
}

#[test]
fn test_function_named_tuple_return_type_mismatch() {
    quiver()
        .evaluate(
            r#"
            f = #'int -> Point[x: 'int, y: 'int] { ~ },
            3 ~> f
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "Point[x: 'int, y: 'int]".to_string(),
            found: "'int".to_string(),
        });
}

#[test]
fn test_generic_function_return_type_mismatch() {
    // Generic function with type parameter t but return type bin
    // Body returns t, which doesn't match bin
    quiver()
        .evaluate("f = #<'t>'t -> 'bin { ~ }, 5 ~> f")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'bin".to_string(),
            found: "'t".to_string(),
        });
}

// A nilary function (parameter `[]`) ignores an implicitly-flowing value: it is called with nil
// and the value is discarded, like a literal. An explicit argument is still type-checked.

#[test]
fn test_nilary_call_ignores_chained_value() {
    quiver().evaluate("make = #{ 99 }, 5 ~> make").expect("99");
}

#[test]
fn test_nilary_call_ignores_block_parameter() {
    // `make` is the leading term of the body, offered the int parameter, which it ignores.
    quiver()
        .evaluate("make = #{ 99 }, use = #'int { make }, 5 ~> use")
        .expect("99");
}

#[test]
fn test_nilary_call_explicit_nil_argument() {
    quiver().evaluate("make = #{ 99 }, make []").expect("99");
}

#[test]
fn test_nilary_call_rejects_explicit_argument() {
    // An explicit argument is not an implicit flow, so handing a value to a nilary function errors.
    quiver()
        .evaluate("make = #{ 99 }, make [5]")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with []".to_string(),
            found: "['int]".to_string(),
        });
}
