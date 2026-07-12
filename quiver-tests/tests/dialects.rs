mod common;
use common::*;
use quiver_compiler::compiler::Error;
use std::collections::HashMap;

/// A module whose `:dialect` function applies `body` (a function body whose `$` is the
/// content `Str['bin]`) to the context's content, and whose module value is `exports`.
fn dialect_module(body: &str, exports: &str) -> HashMap<Vec<String>, String> {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        format!(
            "d0 = #Str['bin] {{ {body} }}\nd = #(content: Str['bin]) {{ $content ~> d0 }}\n{{ :dialect &d; {exports} }}"
        ),
    );
    modules
}

#[test]
fn test_constant_expansion() {
    quiver()
        .with_modules(dialect_module("42", "Ok"))
        .evaluate("%m{anything}")
        .expect("42");
}

#[test]
fn test_expansion_is_statically_typed() {
    quiver()
        .with_modules(dialect_module("42", "Ok"))
        .evaluate("%m{}")
        .expect_type("'int");
}

#[test]
fn test_content_reaches_the_dialect() {
    quiver()
        .with_modules(dialect_module("$", "Ok"))
        .evaluate("%m{hello world}")
        .expect("\"hello world\"");
}

#[test]
fn test_brace_escapes_and_strings_in_content() {
    // `\{`/`\}` unescape outside strings (an escaped brace doesn't count toward depth, so
    // a literal pair needs both escaped); braces inside a `"…"` string don't count and
    // pass through verbatim.
    quiver()
        .with_modules(dialect_module("$", "Ok"))
        .evaluate(r#"%m{a \{b\} "c { d" e}"#)
        .expect("\"a {b} \\\"c { d\\\" e\"");
}

#[test]
fn test_nested_balanced_braces() {
    quiver()
        .with_modules(dialect_module("$", "Ok"))
        .evaluate(r#"%m{a { b { c } } d}"#)
        .expect("\"a { b { c } } d\"");
}

#[test]
fn test_unquote_resolves_in_caller_scope() {
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 1]", "Ok"))
        .evaluate("x = 7; %m{x}")
        .expect("7");
}

#[test]
fn test_unquote_undefined_variable_is_a_compile_error() {
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 7]", "Ok"))
        .evaluate("%m{missing}")
        .expect_compile_error(Error::VariableUndefined("missing".to_string()));
}

#[test]
fn test_flow_receives_the_flowing_value() {
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 1]", "Ok"))
        .evaluate("5 ~> %m{~}")
        .expect("5");
}

#[test]
fn test_call_applies_a_module_export() {
    quiver()
        .with_modules(dialect_module(
            "Call[member: \"double\", arg: Unquote[offset: 0, length: 1]]",
            "[double: #'int { [~, 2] ~> __integer_multiply__ }]",
        ))
        .evaluate("5 ~> %m{~}")
        .expect("10");
}

#[test]
fn test_expansions_chain() {
    quiver()
        .with_modules(dialect_module(
            "Call[member: \"double\", arg: Unquote[offset: 0, length: 1]]",
            "[double: #'int { [~, 2] ~> __integer_multiply__ }]",
        ))
        .evaluate("5 ~> %m{~} ~> %m{~}")
        .expect("20");
}

#[test]
fn test_tuple_construction() {
    quiver()
        .with_modules(dialect_module(
            r#"Tuple[
                name: "Point",
                fields: Cons[Labeled[label: "x", value: 1], Cons[Unquote[offset: 0, length: 1], Nil]],
            ]"#,
            "Ok",
        ))
        .evaluate("y = 2; %m{y}")
        .expect("Point[x: 1, 2]");
}

#[test]
fn test_nil_expansion_short_circuits() {
    quiver()
        .with_modules(dialect_module("Tuple[name: Nil, fields: Nil]", "Ok"))
        .evaluate("%m{}; 5")
        .expect("[]");
}

#[test]
fn test_expansion_inside_a_function_captures_variables() {
    // The dialect expands before capture collection, so the closure captures `x`.
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 1]", "Ok"))
        .evaluate("x = 7; f = #[] { %m{x} }; [] ~> f")
        .expect("7");
}

#[test]
fn test_meta_module_types_the_dialect_function() {
    quiver()
        .with_modules(dialect_module_with_return_type())
        .evaluate("%m{}")
        .expect("42");
}

fn dialect_module_with_return_type() -> HashMap<Vec<String>, String> {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        "d = #(content: Str['bin]) -> '%meta.expr { 42 }\n{ :dialect &d; Ok }".to_string(),
    );
    modules
}

#[test]
fn test_module_without_dialect_annotation() {
    let mut modules = HashMap::new();
    modules.insert(vec!["m".to_string()], "[add: &__integer_add__]".to_string());
    quiver()
        .with_modules(modules)
        .evaluate("%m{}")
        .expect_compile_error(Error::DialectMissing {
            module: "%m".to_string(),
        });
}

#[test]
fn test_error_payload_maps_to_a_source_position() {
    quiver()
        .with_modules(dialect_module(
            "[] ~> { :error Expected[offset: 2, message: \"a value\"] }",
            "Ok",
        ))
        .evaluate("%m{abc}")
        .expect_compile_error(Error::DialectFailed {
            module: "%m".to_string(),
            // Content starts at 1:4 (after `%m{`), so offset 2 is column 6.
            message: "failed: a value (at main:1:6)".to_string(),
        });
}

#[test]
fn test_failure_without_payload() {
    quiver()
        .with_modules(dialect_module("[]", "Ok"))
        .evaluate("%m{}")
        .expect_compile_error(Error::DialectFailed {
            module: "%m".to_string(),
            message: "failed".to_string(),
        });
}

#[test]
fn test_malformed_expr_is_an_error() {
    quiver()
        .with_modules(dialect_module("Bogus[1]", "Ok"))
        .evaluate("%m{}")
        .expect_compile_error(Error::DialectFailed {
            module: "%m".to_string(),
            message: "returned Bogus[…], which is not a '%meta.expr".to_string(),
        });
}

#[test]
fn test_json_dialect() {
    quiver()
        .evaluate(r#"x = 5; %json{ { "a": 1, "b": [2, x], "c": true } }"#)
        .expect(r#"Object[Cons[["a", 1], Cons[["b", Array[Cons[2, Cons[5, Nil]]]], Cons[["c", True], Nil]]]]"#);
}

#[test]
fn test_json_dialect_flow() {
    quiver()
        .evaluate("5 ~> %json{ [1, ~] }")
        .expect("Array[Cons[1, Cons[5, Nil]]]");
}

#[test]
fn test_json_dialect_unifies_with_module_type() {
    quiver()
        .evaluate("f = #'%json { Ok }; %json{ [null, -3, \"s\"] } ~> f")
        .expect("Ok");
}

#[test]
fn test_num_dialect_precedence() {
    quiver().evaluate("%num{ 1 + 2 * 3 }").expect("7");
}

#[test]
fn test_num_dialect_vars_and_parens() {
    // `div` yields a rational (the module's existing semantics), so `4 / 2` is `2/1`.
    quiver()
        .evaluate("x = 10; y = 4; %num{ x + (y / 2) }")
        .expect("12/1");
}

#[test]
fn test_num_dialect_flow_and_unary() {
    quiver().evaluate("5 ~> %num{ ~ * 2 - 1 }").expect("9");
    quiver().evaluate("%num{ -(1 + 2) * 4 }").expect("-12");
}

#[test]
fn test_dict_dialect_generic_values() {
    // The `from` splice unifies dict.from's 'v across mixed 'int / Str values.
    quiver()
        .evaluate(r#"d = %dict{ "a" => 1, "b" => "foo" }; [d, "b"] ~> %dict.get"#)
        .expect("\"foo\"");
}

#[test]
fn test_dict_dialect_var_and_flow() {
    quiver()
        .evaluate(r#"x = 5; %dict{ "k" => x } ~> [~, "k"] ~> %dict.get"#)
        .expect("5");
    quiver()
        .evaluate(r#"9 ~> %dict{ "flow" => ~ } ~> [~, "flow"] ~> %dict.get"#)
        .expect("9");
}

#[test]
fn test_list_dialect() {
    quiver()
        .evaluate("x = 9; %list{ 1, \"two\", x }")
        .expect("Cons[1, Cons[\"two\", Cons[9, Nil]]]");
    quiver().evaluate("%list{}").expect("Nil");
}

#[test]
fn test_list_dialect_tuples_and_flow() {
    // Elements are host expressions: `[1, 2]` is a tuple, exactly as it would be inline.
    quiver()
        .evaluate("5 ~> %list{ ~, [1, 2] }")
        .expect("Cons[5, Cons[[1, 2], Nil]]");
}

#[test]
fn test_list_dialect_nested_dialect() {
    // Nested lists are written as nested dialect invocations — a hole may itself
    // contain a dialect term, expanded recursively.
    quiver()
        .evaluate("%list{ 5, %list{ 1, 2 } }")
        .expect("Cons[5, Cons[Cons[1, Cons[2, Nil]], Nil]]");
}

#[test]
fn test_list_dialect_expression_elements() {
    quiver()
        .evaluate("inc = #'int { [~, 1] ~> __integer_add__ }; x = 2; %list{ x ~> inc, &inc }")
        .expect_type("Cons['int, Cons[(#'int -> 'int), Nil]]");
}

#[test]
fn test_list_dialect_unifies_with_module_type() {
    quiver().evaluate("%list{ 7, 8 } ~> %list.head").expect("7");
}

#[test]
fn test_spaced_brace_is_not_a_dialect() {
    // `%m {…}` (spaced) keeps its existing meaning: the module value flows into a block.
    let mut modules = HashMap::new();
    modules.insert(vec!["m".to_string()], "[val: 3]".to_string());
    quiver()
        .with_modules(modules)
        .evaluate("%m ~> { .val }")
        .expect("3");
}

#[test]
fn test_json_dialect_reports_furthest_failure() {
    // The element failure after `"b":` survives `sep_by`'s backtracking: the error
    // points at the missing-value position, not at `"b"` or the end of the content.
    quiver()
        .evaluate(r#"%json{ { "a": 1, "b": } }"#)
        .expect_compile_error(Error::DialectFailed {
            module: "%json".to_string(),
            message: "failed: '[' (at main:1:23)".to_string(),
        });
}

#[test]
fn test_malformed_field_list_tail_is_an_error() {
    // A `Nil`-with-fields tail must not silently terminate the field list (which would
    // splice `Cons[1, Nil[2]]` as `[1]`).
    quiver()
        .with_modules(dialect_module(
            "Tuple[name: Nil, fields: Cons[1, Nil[2]]]",
            "Ok",
        ))
        .evaluate("%m{}")
        .expect_compile_error(Error::DialectFailed {
            module: "%m".to_string(),
            message:
                "returned a Nil with fields where a field-list terminator (bare Nil) was expected"
                    .to_string(),
        });
}

#[test]
fn test_positional_error_payload_maps_to_a_source_position() {
    // A positionally built `Expected[2, "a value"]` (no field labels) keeps its
    // offset→source-position mapping, like `Splicer::field`'s positional fallback.
    quiver()
        .with_modules(dialect_module(
            "[] ~> { :error Expected[2, \"a value\"] }",
            "Ok",
        ))
        .evaluate("%m{abc}")
        .expect_compile_error(Error::DialectFailed {
            module: "%m".to_string(),
            message: "failed: a value (at main:1:6)".to_string(),
        });
}

#[test]
fn test_expansion_inside_nested_function_literals() {
    // Dialects in doubly nested bodies are expanded by the outermost walk exactly once.
    quiver()
        .with_modules(dialect_module("$ ~> %str.parse_int", "Ok"))
        .evaluate("f = #{ g = #{ [%m{7}, 1] ~> __integer_add__ }; g }; f")
        .expect("8");
}

#[test]
fn test_generics_in_module_imported_inside_a_function_body() {
    // A module first imported from *inside* a function body must still uniquify its own
    // generics' type parameters (not inherit the importing function's suffix).
    let mut modules = HashMap::new();
    modules.insert(
        vec!["g".to_string()],
        "id = #<'t>'t { $ }\nwrap = #<'t>'t { [$ ~> id, Nil] }\n[id: &id, wrap: &wrap]".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("f = #'int { (id, wrap) = %g; [$ ~> id, \"x\" ~> wrap] }; 3 ~> f")
        .expect("[3, [\"x\", Nil]]");
}

#[test]
fn test_escaped_quote_allows_unpaired_quote_in_content() {
    // `\"` outside strings is an escaped literal quote: content with an odd number of
    // quotes no longer swallows the closing `}` as string content.
    quiver()
        .with_modules(dialect_module("$", "Ok"))
        .evaluate(r#"%m{5\" tall}"#)
        .expect("\"5\\\" tall\"");
}

// --- Unquote: spans of the content parsed as host code, spliced in caller scope ------

#[test]
fn test_unquote_variable() {
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 1]", "Ok"))
        .evaluate("x = 5; %m{x}")
        .expect("5");
}

#[test]
fn test_unquote_callable_is_called_with_the_flowing_value() {
    // Host semantics: a bare callable in a hole is *called* with the dialect input.
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 3]", "Ok"))
        .evaluate("inc = #'int { [~, 1] ~> __integer_add__ }; 5 ~> %m{inc}")
        .expect("6");
}

#[test]
fn test_unquote_reference_passes_by_value() {
    // `&f` in a hole references without calling — and elides its binding (pure term).
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 4]", "Ok"))
        .evaluate("inc = #'int { [~, 1] ~> __integer_add__ }; f = %m{&inc}; 5 ~> f")
        .expect("6");
}

#[test]
fn test_unquote_ripple_is_the_dialect_input() {
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 1]", "Ok"))
        .evaluate("5 ~> %m{~}")
        .expect("5");
}

#[test]
fn test_unquote_hole_receives_the_flowing_value() {
    // Inside a larger hole, `~` at chain start is the value flowing into the dialect term.
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 6]", "Ok"))
        .evaluate("5 ~> %m{[~, 1]}")
        .expect("[5, 1]");
}

#[test]
fn test_unquote_duplicated_span_evaluates_once() {
    // Bind-once: the same span spliced twice shares one binding, so a nilary callable
    // hole runs once — both elements are the *same* ref.
    quiver()
        .with_modules(dialect_module(
            "Tuple[name: Nil, fields: Cons[Unquote[offset: 0, length: 3], Cons[Unquote[offset: 0, length: 3], Nil]]]",
            "Ok",
        ))
        .evaluate("ref = &%ref; %m{ref} ~> =[a, b]; a ~> =&b")
        .expect("Ok");
}

#[test]
fn test_unquote_expression_chain() {
    // A multi-term chain hole, evaluated in the caller's scope.
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 31]", "Ok"))
        .evaluate("xs = [1, 2]; %m{[xs.0, xs.1] ~> __integer_add__}")
        .expect("3");
}

#[test]
fn test_context_record_chain_callback() {
    // A dialect taking the context record (via a partial parameter naming what it uses)
    // and asking the host, through the `chain` callback, how far the first expression
    // extends — the `, junk` tail is beyond the returned end.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        r#"'cb = #['bin, 'int] -> ('int | [])
d = #(content: Str['bin], chain: 'cb) {
  $.content ~> =Str[data]
  [data, 0] ~> $.chain ~> =('int)end
  Unquote[offset: 0, length: end]
}
{ :dialect &d; Ok }"#
            .to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("x = 5; %m{x, junk}")
        .expect("5");
}
