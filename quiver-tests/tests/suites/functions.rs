use crate::common::*;

#[test]
fn test_simple_function() {
    quiver().evaluate("f = #[] { 42 }; [] ~> f ~").expect("42");
}

#[test]
fn test_nil_function() {
    quiver().evaluate("f = #[] { [] }; [] ~> f ~").expect("[]");
}

#[test]
fn test_function_with_parameter() {
    quiver()
        .evaluate("inc = #'int { =x => [x, 1] ~> __integer_add__ ~ }; 3 ~> inc ~")
        .expect("4");
}

#[test]
fn test_function_closure() {
    quiver()
        .evaluate("x = 1; f = #[] { x }; x = 2; [] ~> f ~")
        .expect("1");
}

#[test]
fn test_function_with_tuple_parameter() {
    quiver()
        .evaluate(
            r#"
            f = #Point[x: 'int, y: 'int] {
              =Point[x: x, y: y] => [x, y] ~> __integer_add__ ~
            };
            Point[x: 1, y: 2] ~> f ~
            "#,
        )
        .expect("3");
}

#[test]
fn test_function_with_enumerated_type_parameter() {
    quiver()
        .evaluate("f = #('int | 'bin) { =x => x }; <0a1b2c> ~> f ~")
        .expect("<0a1b2c>");
}

#[test]
fn test_higher_order_function() {
    quiver()
        .evaluate(
            r#"
            apply = #[#'int -> 'int, 'int] { =[f, x] => x ~> f ~ };
            double = #'int { =x => [x, 2] ~> __integer_multiply__ ~ };
            [double, 5] ~> apply ~
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
              =x => #'int { =y => [x, y] ~> __integer_add__ ~ }
            };
            3 ~> f ~ ~> =g;
            5 ~> g ~
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
              =x => [[x, 2] ~> %num.mul ~, 1] ~> %num.add ~
            };
            5 ~> double_plus_one ~
            "#,
        )
        .expect("11");
}

#[test]
fn test_closure_captures_nested_member_access() {
    quiver()
        .evaluate(
            r#"
            obj = [inner: [value: 42]];
            get_value = #[] { obj.inner.value };
            [] ~> get_value ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_nested_function_captures() {
    quiver()
        .evaluate(
            r#"
            f = #[] {
              inc = #'int { [~, 1] ~> %num.add ~ };
              42 ~> inc ~
            };
            [] ~> f ~
            "#,
        )
        .expect("43");
}

#[test]
fn test_function_call_syntax() {
    quiver().evaluate("[3, 4] ~> %num.add ~").expect("7");
}

#[test]
fn test_function_call_with_ripple() {
    quiver()
        .evaluate("[1, 2] ~> %num.add ~ ~> [~, 3] ~> %num.mul ~")
        .expect("9");
}

#[test]
fn test_function_call_no_args() {
    quiver().evaluate("f = #[] { 42 }; f []").expect("42");
}

#[test]
fn test_function_call_with_spread() {
    quiver()
        .evaluate(
            r#"
            f = #['int, 'int, 'int] {
              $.0 ~> [~, $.1] ~> %num.add ~ ~> [~, $.2] ~> %num.add ~
            };
            [1, 2] ~> [..., 3] ~> f ~
            "#,
        )
        .expect("6");
}

#[test]
fn applying_a_flowing_function_requires_binding() {
    // There is no form that applies the flowing value *as a function* — bind the function to a
    // name first, then call it argument-first.
    quiver().evaluate("f = %num.add; [3, 4] ~> f ~").expect("7");
}

#[test]
fn applying_a_flowing_field_function_requires_binding() {
    // Likewise to call a function read off another value: read it into a binding first, then
    // call it argument-first.
    quiver()
        .evaluate("add = %num.add; [1, 2] ~> add ~")
        .expect("3");
}

#[test]
fn test_ripple_field_access() {
    quiver().evaluate("[x: 5, y: 10] ~> ~.x").expect("5");
}

#[test]
fn test_function_result_covariance() {
    quiver().evaluate(
        r#"
        f = #(#'bin -> (Ok | [])) { ~ };
        g = #'bin { [] };
        g ~> f ~
        "#,
    );
}

#[test]
fn test_function_parameter_contravariance() {
    quiver().evaluate(
        r#"
        f = #(#[] -> 'bin) { ~ };
        g = #(Ok | []) { <00> };
        g ~> f ~
        "#,
    );
}

#[test]
fn test_apply_value_to_inline_function() {
    // Inline functions are not auto-called; bind first, then call
    quiver()
        .evaluate("f = #'int { [~, 2] ~> __integer_add__ ~ }; 5 ~> f ~")
        .expect("7");
}

#[test]
fn test_identity_function_int() {
    quiver().evaluate("f = #'int; 42 ~> f ~").expect("42");
}

#[test]
fn test_identity_function_bin() {
    quiver()
        .evaluate("f = #'bin; <0a1b2c> ~> f ~")
        .expect("<0a1b2c>");
}

#[test]
fn test_identity_function_tuple() {
    quiver()
        .evaluate("f = #Point[x: 'int, y: 'int]; Point[x: 1, y: 2] ~> f ~")
        .expect("Point[x: 1, y: 2]");
}

#[test]
fn test_identity_function_inline() {
    // Inline functions are not auto-called; bind first, then call
    quiver().evaluate("f = #'int; 42 ~> f ~").expect("42");
}

#[test]
fn test_identity_function_with_type_parameter() {
    quiver()
        .evaluate(
            r#"
            id = #<'t>'t;
            42 ~> id ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_dollar_parameter_reference() {
    quiver().evaluate("f = #'int { $ }; 5 ~> f ~").expect("5");
}

#[test]
fn test_dollar_with_tuple_field_access() {
    quiver()
        .evaluate("f = #['int, 'int] { [$.0, $.1] ~> __integer_add__ ~ }; [10, 20] ~> f ~")
        .expect("30");
}

#[test]
fn test_dollar_dotless_index_shorthand() {
    quiver()
        .evaluate("f = #['int, 'int] { [$0, $1] ~> __integer_add__ ~ }; [10, 20] ~> f ~")
        .expect("30");
}

#[test]
fn test_dollar_dotless_field_shorthand() {
    quiver()
        .evaluate(
            "f = #Point[x: 'int, y: 'int] { [$x, $y] ~> __integer_add__ ~ }; Point[x: 10, y: 20] ~> f ~",
        )
        .expect("30");
}

#[test]
fn test_dollar_dotless_shorthand_then_dotted() {
    quiver()
        .evaluate("f = #[p: [x: 'int]] { $p.x }; [p: [x: 7]] ~> f ~")
        .expect("7");
}

#[test]
fn test_dollar_in_nested_block() {
    quiver()
        .evaluate("f = #'int { 100 ~> { [~, $] ~> __integer_add__ ~ } }; 7 ~> f ~")
        .expect("107");
}

#[test]
fn test_dollar_with_named_tuple_field() {
    quiver()
        .evaluate(
            r#"
            f = #Point[x: 'int, y: 'int] { [$.x, $.y] ~> __integer_add__ ~ };
            Point[x: 10, y: 20] ~> f ~
            "#,
        )
        .expect("30");
}

#[test]
fn test_function_with_return_type() {
    quiver()
        .evaluate("f = #'int -> 'int { [~, 1] ~> __integer_add__ ~ }; 5 ~> f ~")
        .expect("6");
}

#[test]
fn test_function_return_type_mismatch() {
    quiver()
        .evaluate("f = #'int -> 'bin { [~, 1] ~> __integer_add__ ~ }; 5 ~> f ~")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "declared result: 'int is not 'bin".to_string(),
        ));
}

#[test]
fn test_function_with_tuple_return_type() {
    quiver()
        .evaluate(
            r#"
            f = #['int, 'int] -> 'int { [$.0, $.1] ~> __integer_add__ ~ };
            [3, 4] ~> f ~
            "#,
        )
        .expect("7");
}

#[test]
fn test_function_tuple_return_type_mismatch() {
    quiver()
        .evaluate(
            r#"
            f = #['int, 'int] -> 'bin { [$.0, $.1] ~> __integer_add__ ~ };
            [3, 4] ~> f ~
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "declared result: 'int is not 'bin".to_string(),
        ));
}

#[test]
fn test_call_is_typed_by_the_declared_result() {
    // The declared result is the contract callers see, not the body's sharper type.
    quiver()
        .evaluate("f = #'int -> ('int | 'bin) { 5 }; f 1")
        .expect_type("'bin | 'int");
    quiver()
        .evaluate("f = #'int -> ('int | 'bin) { 5 }; f 1 ~> __integer_add__ [~, 1]")
        .expect_type_mismatch();
    // Without a declaration, the body's type is the result.
    quiver()
        .evaluate("f = #'int { 5 }; f 1")
        .expect_type("'int");
}

#[test]
fn test_declared_result_is_not_specialized_per_argument() {
    // Return-type dispatch would answer `A`'s branch alone; a declared result stands instead.
    quiver()
        .evaluate("f = #(A | B) -> ('int | 'bin) { | =A => 1 | =B => <01> }; f A")
        .expect_type("'bin | 'int");
    quiver()
        .evaluate("f = #(A | B) { | =A => 1 | =B => <01> }; f A")
        .expect_type("'int");
}

#[test]
fn test_identity_function_with_return_type() {
    quiver()
        .evaluate("f = #'int -> 'int; 42 ~> f ~")
        .expect("42");
}

#[test]
fn test_identity_function_return_type_mismatch() {
    quiver()
        .evaluate("f = #'int -> 'bin; 42 ~> f ~")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "declared result: 'int is not 'bin".to_string(),
        ));
}

#[test]
fn test_function_with_generic_return_type() {
    quiver()
        .evaluate(
            r#"
            id = #<'t>'t -> 't { ~ };
            42 ~> id ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_function_with_generic_return_type_string() {
    quiver()
        .evaluate(
            r#"
            id = #<'t>'t -> 't { ~ };
            "hello" ~> id ~
            "#,
        )
        .expect("\"hello\"");
}

#[test]
fn test_function_with_complex_return_type() {
    quiver()
        .evaluate(
            r#"
            double = #'int -> 'int { [~, 2] ~> __integer_multiply__ ~ };
            square = #'int -> 'int { [~, ~] ~> __integer_multiply__ ~ };
            5 ~> double ~ ~> square ~
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
                =n => Point[x: n, y: [n, n] ~> __integer_multiply__ ~]
            };
            3 ~> f ~
            "#,
        )
        .expect("Point[x: 3, y: 9]");
}

#[test]
fn test_function_named_tuple_return_type_mismatch() {
    quiver()
        .evaluate(
            r#"
            f = #'int -> Point[x: 'int, y: 'int] { ~ };
            3 ~> f ~
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "declared result: 'int is not Point[x: 'int, y: 'int]".to_string(),
        ));
}

#[test]
fn test_generic_function_return_type_mismatch() {
    // Generic function with type parameter t but return type bin
    // Body returns t, which doesn't match bin
    quiver()
        .evaluate("f = #<'t>'t -> 'bin { ~ }; 5 ~> f ~")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "declared result: Cannot unify rigid type variable 't with expected type 'bin"
                .to_string(),
        ));
}

// A nilary function (parameter `[]`) ignores an implicitly-flowing value: it is called with nil
// and the value is discarded, like a literal. An explicit argument is still type-checked.

#[test]
fn test_nilary_call_ignores_chained_value() {
    // A nilary function takes nil and nothing else: the flowing value is an argument
    // like any other, so handing it to one is a type error.
    quiver()
        .evaluate("make = #[] { 99 }; 5 ~> make ~")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with []".to_string(),
            found: "'int".to_string(),
        });
    quiver().evaluate("make = #[] { 99 }; make []").expect("99");
}

#[test]
fn test_nilary_call_ignores_block_parameter() {
    // `make` as the leading term of the body names the function; calling it is written.
    quiver()
        .evaluate("make = #[] { 99 }; use = #'int { make [] }; 5 ~> use ~")
        .expect("99");
}

#[test]
fn test_nilary_call_explicit_nil_argument() {
    // A nilary function ignores the flowing value, so flowing nil into it just calls it.
    quiver()
        .evaluate("make = #[] { 99 }; [] ~> make ~")
        .expect("99");
}

// Outer-parameter access: `$$` reaches the enclosing function's argument (`$$$` one further,
// and so on), captured by value — per accessed path — when the closure is built.

#[test]
fn test_outer_parameter_whole_argument() {
    quiver()
        .evaluate("f = #'int { g = #'int { [$, $$] }; g 9 }; f 7")
        .expect("[9, 7]");
}

#[test]
fn test_outer_parameter_field_and_index() {
    quiver()
        .evaluate("f = #[user: 'int] { g = #'int { [$, $$user] }; g 9 }; f [user: 7]")
        .expect("[9, 7]");
    quiver()
        .evaluate("f = #['int, 'int] { g = #'int { [$, $$1] }; g 9 }; f [7, 8]")
        .expect("[9, 8]");
    quiver()
        .evaluate("f = #[a: [b: 'int]] { g = #'int { [$, $$a.b] }; g 9 }; f [a: [b: 7]]")
        .expect("[9, 7]");
}

#[test]
fn test_outer_parameter_three_levels() {
    quiver()
        .evaluate("f = #'int { g = #'int { h = #'int { [$, $$, $$$] }; h 2 }; g 1 }; f 0")
        .expect("[2, 1, 0]");
}

#[test]
fn test_outer_parameter_relayed_through_middle_closure() {
    // `$$$k` two closures deep: the middle closure never mentions the value, yet its own
    // capture pass must relay it from the outer function.
    quiver()
        .evaluate("f = #[k: 'int] { g = #'int { h = #'int { $$$k }; h 0 }; g 0 }; f [k: 42]")
        .expect("42");
}

#[test]
fn test_outer_parameter_skips_blocks() {
    // Blocks are transparent: only function literals count as levels, as for `$` itself.
    quiver()
        .evaluate("f = #'int { g = #'int { { { [$, $$] } } }; g 9 }; f 7")
        .expect("[9, 7]");
}

#[test]
fn test_outer_parameter_callable_field() {
    // A captured outer field that is a function is called, like any callable variable.
    quiver()
        .evaluate(
            "f = #[add: #['int, 'int] -> 'int] { g = #'int { $$add [$, 100] }; g 5 }; f [add: __integer_add__]",
        )
        .expect("105");
}

#[test]
fn test_outer_parameter_pin() {
    quiver()
        .evaluate(
            "f = #[k: 'int] { g = #'int { { $ ~> =^$$k => Yes | No } }; [g 5, g 6] }; f [k: 5]",
        )
        .expect("[Yes, No]");
}

#[test]
fn test_outer_parameter_in_spawn_and_string_hole() {
    // Spawned closures capture outer parameters like any capture (moving them to the child).
    quiver()
        .evaluate("f = #'int { p = @#[] { $$ } []; !p }; f 7")
        .expect("7");
    quiver()
        .evaluate("f = #[name: '%str] { g = #[] { \"hi {$$name}\" }; g [] }; f [name: \"joe\"]")
        .expect("\"hi joe\"");
}

#[test]
fn test_outer_parameter_capture_time_snapshot() {
    // `$$` is captured when the closure is built: a tail call re-entering the outer
    // function does not retroactively change an existing closure's view.
    quiver()
        .evaluate("f = #'int { | %num.gt? [$, 0] => #[] { $$ } | ^ 5 }; g = 0 ~> f ~; g []")
        .expect("5");
}

#[test]
fn test_outer_parameter_depth_exceeded() {
    quiver()
        .evaluate("f = #'int { $$ }; f 1")
        .expect_compile_error(quiver_compiler::compiler::Error::ParameterDepthExceeded {
            written: "$$".to_string(),
        });
}

#[test]
fn test_calling_a_union_of_functions() {
    // The argument must suit every member; the result is whichever member's.
    quiver()
        .evaluate("h = [1] ~> { | =[1] => #'int { 1 } | #'int { <01> } }; h 5")
        .expect("1")
        .expect_type("'bin | 'int");
    quiver()
        .evaluate("h = [1] ~> { | =[1] => #'int { 1 } | #'bin { $ } }; h 5")
        .expect_error_containing("expected function parameter compatible with 'bin, found 'int");
    // A generic member is instantiated for the call like any callee.
    quiver()
        .evaluate(
            "h = [1] ~> { | =[1] => #<'t>'t { $ } | #'int { 7 } }; h 5 ~> __integer_add__ [~, 1]",
        )
        .expect("6");
    quiver()
        .evaluate("%list{ #'int { 1 }, #'int { <01> } } ~> %list.map [~, #{ $ 3 }]")
        .expect("Cons[1, Cons[<01>, Nil]]");
}

#[test]
fn test_a_union_of_functions_takes_what_its_members_share() {
    // Labels every member lets callers omit may be omitted; others must be written.
    quiver()
        .evaluate("h = [1] ~> { | =[1] => #[(x): 'int] { $x } | #[(x): 'int] { 2 } }; h [5]")
        .expect("5");
    quiver()
        .evaluate("h = [1] ~> { | =[1] => #[(x): 'int] { $x } | #[x: 'int] { 2 } }; h [5]")
        .expect_type_mismatch();
    // A shared parameter is what an inferred-parameter literal argument infers from.
    quiver()
        .evaluate(
            "h = [1] ~> {
               | =[1] => #[f: #'int -> 'int] { $f 1 }
               | #[f: #'int -> 'int] { 0 }
             }
             h [f: #{ __integer_add__ [$, 1] }]",
        )
        .expect("2");
}

#[test]
fn test_uninhabited_parameter_types_are_warned_about() {
    for parameter in [
        "('int & 'bin)",
        "((x: 'int) & (x: 'bin))",
        "[a: 'int, b: ('int & 'bin)]",
        // Function types don't intersect (there are no overloaded function types).
        "((#'int -> 'int) & (#'bin -> 'bin))",
    ] {
        let result = quiver().evaluate(&format!("f = #{parameter} {{ 1 }}; f"));
        assert!(
            result.warnings().iter().any(|w| matches!(
                w.warning,
                quiver_compiler::compiler::Warning::UninhabitedParameter
            )),
            "expected an uninhabited-parameter warning for {parameter}, got {:?}",
            result.warnings()
        );
    }
    for parameter in ["('int | ('int & 'bin))", "'%list<'int>", "[]"] {
        quiver()
            .evaluate(&format!("f = #{parameter} {{ 1 }}; f"))
            .expect_no_warnings();
    }
}
