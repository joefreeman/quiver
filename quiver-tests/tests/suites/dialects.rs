use crate::common::*;
use quiver_compiler::compiler::Error;
use std::collections::HashMap;

/// A module whose `:dialect` function applies `body` (a function body whose `$` is the
/// content `Str['bin]`) to the context's content, and whose module value is `exports`.
fn dialect_module(body: &str, exports: &str) -> HashMap<Vec<String>, String> {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        format!(
            "d0 = #Str['bin] {{ {body} }}\nd = #(content: Str['bin]) {{ $content ~> d0 }}\n{{ :dialect d; {exports} }}"
        ),
    );
    modules
}

fn line_column(error: &quiver_compiler::compiler::LocatedError) -> Option<(usize, usize)> {
    error.span.map(|span| (span.line, span.column))
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
            "Apply[callee: Member[module: \"m\", member: \"double\", arguments: Nil], arg: Unquote[offset: 0, length: 1]]",
            "[double: #'int { [~, 2] ~> __integer_multiply__ }]",
        ))
        .evaluate("5 ~> %m{~}")
        .expect("10");
}

#[test]
fn test_call_argument_span_adopts_labels_and_fills_defaults() {
    // A `Call` whose argument is an `Unquote` splices the span in *argument* position, so
    // the author's literal elaborates against the member's parameter exactly as at a
    // handwritten call: a positional entry adopts its omittable label, and the omitted
    // field takes the member's default.
    quiver()
        .with_modules(dialect_module(
            "Apply[callee: Member[module: \"m\", member: \"opts\", arguments: Nil], arg: Unquote[offset: 0, length: 3]]",
            "[opts: #[(keys): 'int, debounce: 'int = 50] { $ }]",
        ))
        .evaluate("%m{[7]}")
        .expect("[keys: 7, debounce: 50]");
}

#[test]
fn test_call_argument_span_infers_a_function_literal_parameter() {
    // The same reach: an inferring `#{ … }` in the span takes its parameter from the
    // member's declared one.
    quiver()
        .with_modules(dialect_module(
            "Apply[callee: Member[module: \"m\", member: \"apply\", arguments: Nil], arg: Unquote[offset: 0, length: 11]]",
            "[apply: #['int, #'int -> 'int] { $1 $0 }]",
        ))
        .evaluate("%m{[5, #{ $ }]}")
        .expect("5");
}

#[test]
fn test_call_argument_span_evaluates_once() {
    // Bind-once now covers the call: two splices of one `Call` over one span share a
    // binding, so the member runs once (both fields are the same ref).
    quiver()
        .with_modules(dialect_module(
            r#"call = Apply[callee: Member[module: "m", member: "id", arguments: Nil], arg: Unquote[offset: 0, length: 6]]
Tuple[name: [], fields: Cons[call, Cons[call, Nil]]]"#,
            "[id: #'ref { $ }]",
        ))
        .evaluate("ref = %ref; %m{ref []} ~> =[a, b]; a ~> { =^b => Same | Distinct }")
        .expect("Same");
}

#[test]
fn test_call_reaches_a_named_module() {
    // A `module` field names the module to call into — resolved in the dialect's package, so
    // a dialect can emit calls to a companion module it does not itself import.
    let mut modules = dialect_module(
        "Apply[callee: Member[module: \"helper\", member: \"double\", arguments: Nil], arg: Unquote[offset: 0, length: 1]]",
        "Ok",
    );
    modules.insert(
        vec!["helper".to_string()],
        "[double: #'int { [~, 2] ~> __integer_multiply__ }]".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("5 ~> %m{~}")
        .expect("10");
}

#[test]
fn test_expansions_chain() {
    quiver()
        .with_modules(dialect_module(
            "Apply[callee: Member[module: \"m\", member: \"double\", arguments: Nil], arg: Unquote[offset: 0, length: 1]]",
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
        .with_modules(dialect_module("Tuple[name: [], fields: Nil]", "Ok"))
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
        "d = #(content: Str['bin]) -> '%meta.expr { 42 }\n{ :dialect d; Ok }".to_string(),
    );
    modules
}

#[test]
fn test_module_without_dialect_annotation() {
    let mut modules = HashMap::new();
    modules.insert(vec!["m".to_string()], "[add: __integer_add__]".to_string());
    quiver()
        .with_modules(modules)
        .evaluate("%m{}")
        .expect_compile_error(Error::DialectMissing {
            module: "%m".to_string(),
        });
}

#[test]
fn test_error_payload_maps_to_a_source_position() {
    let error = quiver()
        .with_modules(dialect_module(
            "[] ~> { :error Expected[offset: 2, message: \"a value\"] }",
            "Ok",
        ))
        .evaluate("%m{abc}")
        .expect_located_compile_error();
    assert_eq!(
        error.error,
        Error::DialectFailed {
            module: "%m".to_string(),
            message: "failed: expected a value".to_string(),
        }
    );
    // Content starts at 1:4 (after `%m{`), so offset 2 is column 6.
    assert_eq!(line_column(&error), Some((1, 6)));
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
fn test_json_dialect_unifies_when_composites_alternate() {
    // `'%json`'s `Array` and `Object` members hold their `^` at different depths (an object's
    // sits one tuple deeper, inside the `[key, value]` pair), so a document that alternates
    // between them exercises cycle resolution across members. It used to be rejected —
    // object-in-object and array-in-array were fine, but either inside the other was not.
    quiver()
        .evaluate(r#"%json{ { "a": [1, 2] } } ~> %json.stringify"#)
        .expect(r#""{\"a\":[1,2]}""#);
    quiver()
        .evaluate(r#"%json{ [{ "a": 1 }] } ~> %json.stringify"#)
        .expect(r#""[{\"a\":1}]""#);
    quiver()
        .evaluate(r#"%json{ { "a": [1, { "b": [2, 3] }] } } ~> %json.stringify"#)
        .expect(r#""{\"a\":[1,{\"b\":[2,3]}]}""#);
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
fn test_dialect_call_with_literal_argument() {
    // `-1` emits `Apply[callee: Member[module: "num", member: "neg", arguments: Nil], arg: 1]`:
    // a plain-data argument splices as a literal.
    quiver().evaluate("%num{ -1 }").expect("-1");
    quiver().evaluate("%num{ 2 ^ -1 }").expect("1/2");
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
        .evaluate("inc = #'int { [~, 1] ~> __integer_add__ }; x = 2; %list{ x ~> inc, inc }")
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
    let error = quiver()
        .evaluate(r#"%json{ { "a": 1, "b": } }"#)
        .expect_located_compile_error();
    assert_eq!(
        error.error,
        Error::DialectFailed {
            module: "%json".to_string(),
            message: "failed: expected '['".to_string(),
        }
    );
    assert_eq!(line_column(&error), Some((1, 23)));
}

#[test]
fn test_malformed_field_list_tail_is_an_error() {
    // A `Nil`-with-fields tail must not silently terminate the field list (which would
    // splice `Cons[1, Nil[2]]` as `[1]`).
    quiver()
        .with_modules(dialect_module(
            "Tuple[name: [], fields: Cons[1, Nil[2]]]",
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
    let error = quiver()
        .with_modules(dialect_module(
            "[] ~> { :error Expected[2, \"a value\"] }",
            "Ok",
        ))
        .evaluate("%m{abc}")
        .expect_located_compile_error();
    assert_eq!(
        error.error,
        Error::DialectFailed {
            module: "%m".to_string(),
            message: "failed: expected a value".to_string(),
        }
    );
    assert_eq!(line_column(&error), Some((1, 6)));
}

#[test]
fn test_a_malformed_unquote_span_is_located_in_the_content() {
    let error = quiver()
        .with_modules(dialect_module("Unquote[offset: 1, length: 4]", "Ok"))
        .evaluate("x = 1\n%m{ab ~>c}")
        .expect_located_compile_error();
    assert!(
        matches!(&error.error, Error::DialectFailed { message, .. }
            if message.starts_with("returned an Unquote span that is not a single block")),
        "{error}"
    );
    assert_eq!(error.span.map(|span| span.line), Some(2));
}

#[test]
fn test_a_failing_dialect_function_is_reported_as_a_dialect_evaluation() {
    let error = quiver()
        .with_modules(dialect_module("__panic__ \"no\"", "Ok"))
        .evaluate("x = 1\n%m{abc}")
        .expect_located_compile_error();
    assert!(
        matches!(&error.error, Error::DialectEvaluationFailed { module, .. } if module == "%m"),
        "{error}"
    );
    assert!(
        error
            .to_string()
            .starts_with("Running dialect %m at compile time failed: no"),
        "{error}"
    );
    // At the invocation: the failure has no position of its own.
    assert_eq!(line_column(&error), Some((2, 1)));
}

#[test]
fn test_expansion_inside_nested_function_literals() {
    // Dialects in doubly nested bodies are expanded by the outermost walk exactly once.
    quiver()
        .with_modules(dialect_module("$ ~> %str.parse_int", "Ok"))
        .evaluate("f = #[] { g = #[] { [%m{7}, 1] ~> __integer_add__ }; g [] }; f []")
        .expect("8");
}

#[test]
fn test_generics_in_module_imported_inside_a_function_body() {
    // A module first imported from *inside* a function body must still uniquify its own
    // generics' type parameters (not inherit the importing function's suffix).
    let mut modules = HashMap::new();
    modules.insert(
        vec!["g".to_string()],
        "id = #<'t>'t { $ }\nwrap = #<'t>'t { [$ ~> id, Nil] }\n[id: id, wrap: wrap]".to_string(),
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
    // Host semantics: a hole is ordinary Quiver evaluated with the dialect input flowing
    // in, so calling the callable it names is written there like anywhere else.
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 5]", "Ok"))
        .evaluate("inc = #'int { [~, 1] ~> __integer_add__ }; 5 ~> %m{inc ~}")
        .expect("6");
}

#[test]
fn test_unquote_name_passes_by_value() {
    // A name in a hole is the function, not a call — and elides its binding (pure term).
    quiver()
        .with_modules(dialect_module("Unquote[offset: 0, length: 3]", "Ok"))
        .evaluate("inc = #'int { [~, 1] ~> __integer_add__ }; f = %m{inc}; 5 ~> f")
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
            "Tuple[name: [], fields: Cons[Unquote[offset: 0, length: 3], Cons[Unquote[offset: 0, length: 3], Nil]]]",
            "Ok",
        ))
        .evaluate("ref = %ref; %m{ref} ~> =[a, b]; a ~> { =^b => Same | Distinct }")
        .expect("Same");
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
  [data, 0] ~> $.chain ~ ~> =('int & end)
  Unquote[offset: 0, length: end]
}
{ :dialect d; Ok }"#
            .to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("x = 5; %m{x, junk}")
        .expect("5");
}

#[test]
fn test_dict_dialect_expression_keys() {
    // Keys are host expressions exactly like values: ints, tuples, variables.
    quiver()
        .evaluate("%dict{ 5 => 50, 6 => 60 } ~> [~, 6] ~> %dict.get")
        .expect("60");
    quiver()
        .evaluate("%dict{ Point[1, 2] => 9 } ~> [~, Point[1, 2]] ~> %dict.get")
        .expect("9");
    quiver()
        .evaluate(r#"k = "a"; %dict{ k => 1 } ~> [~, "a"] ~> %dict.get"#)
        .expect("1");
}

#[test]
fn test_dict_dialect_key_chain_and_flow() {
    // A key expression sees the dialect's flowing value and full call chains.
    quiver()
        .evaluate(r#"7 ~> %dict{ ~ => "seven" } ~> [~, 7] ~> %dict.get"#)
        .expect("\"seven\"");
    quiver()
        .evaluate(r#"%dict{ %num.add [1, 2] => "three" } ~> [~, 3] ~> %dict.get"#)
        .expect("\"three\"");
}

/// A dialect's list-shaped output nests once per element, and every level mints a tuple
/// type of its own — so a long literal is quadratically expensive to compile long before
/// it is deep enough to exhaust the compiler's stack walking it. The bound turns what was
/// a process abort (fatal, and in the shared server fatal for every other session too)
/// into an ordinary compile error. A *wide* literal is untouched: `[1, …, 1000]` is one
/// level and one type however many fields it has.
#[test]
fn test_dialect_expansion_depth_is_bounded() {
    let items = |n: usize| vec!["1"; n].join(", ");

    // At the bound, and one past it.
    quiver()
        .evaluate(&format!("%list{{ {} }} ~> %list.count", items(256)))
        .expect("256");
    quiver()
        .evaluate(&format!("%list{{ {} }} ~> %list.count", items(257)))
        .expect_error_containing("nesting more than 256 levels deep");

    // The same bound covers every dialect, because they share one list emitter.
    quiver()
        .evaluate(&format!(
            "%json{{ [{}] }} ~> %json.stringify ~> %str.length",
            items(300)
        ))
        .expect_error_containing("nesting more than 256 levels deep");

    // Width is not depth: a flat tuple far longer than the bound compiles fine, because
    // it is one level and one type however many fields it has.
    quiver()
        .evaluate(&format!("wide = [{}]; wide.999", items(1000)))
        .expect("1");
}

/// [`dialect_module`], with the dialect's result declared `'%meta.expr`, so a test's IR is
/// checked against the alias as well as by the splicer.
fn ir_module(body: &str) -> HashMap<Vec<String>, String> {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        format!(
            "d0 = #Str['bin] -> '%meta.expr {{ {body} }}\nd = #(content: Str['bin]) {{ $content ~> d0 }}\n{{ :dialect d; Ok }}"
        ),
    );
    modules
}

/// An expansion that is a list of IR items, as a `Cons` list expression.
fn ir_list(items: &[&str]) -> String {
    items.iter().rev().fold("Nil".to_string(), |tail, item| {
        format!("Cons[{item}, {tail}]")
    })
}

#[test]
fn test_function_literal_expansion() {
    // `#'int { %num.add [$, 1] }`
    let body = "Function[parameters: Nil, parameter: Int, result: [], annotations: Nil, body: Apply[callee: Member[module: \"num\", member: \"add\", arguments: Nil], arg: Tuple[name: [], fields: Cons[Param[0], Cons[1, Nil]]]]]";
    quiver()
        .with_modules(ir_module(body))
        .evaluate("f = %m{}; f 41")
        .expect("42");
    // A generic one, naming its own type parameter.
    let body = "Function[parameters: Cons[\"d\", Nil], parameter: Var[\"d\"], result: [], annotations: Nil, body: Param[0]]";
    quiver()
        .with_modules(ir_module(body))
        .evaluate("f = %m{}; [f <01>, f 2]")
        .expect("[<01>, 2]");
    // A module's type, by reference.
    let body = "Function[parameters: Nil, parameter: Named[module: \"list\", member: [], arguments: Cons[Int, Nil]], result: [], annotations: Nil, body: Apply[callee: Member[module: \"list\", member: \"count\", arguments: Nil], arg: Param[0]]]";
    quiver()
        .with_modules(ir_module(body))
        .evaluate("f = %m{}; f %list{ 1, 2 }")
        .expect("2");
}

#[test]
fn test_block_match_and_bindings_expansion() {
    // `#_ { | =P[x] => x | 0 }`
    let branches = ir_list(&[
        "Branch[condition: Cons[Match[value: Param[0], pattern: Tuple[name: \"P\", fields: Cons[Bind[\"x\"], Nil]]], Nil], consequence: Cons[Local[\"x\"], Nil]]",
        "Branch[condition: Cons[0, Nil], consequence: []]",
    ]);
    let body = format!(
        "Function[parameters: Nil, parameter: Top, result: [], annotations: Nil, body: Block[branches: {branches}]]"
    );
    quiver()
        .with_modules(ir_module(&body))
        .evaluate("f = %m{}; [f P[5], f Q]")
        .expect("[5, 0]");
    // Patterns: a type test, a negation, a conjunction, a string.
    let branches = ir_list(&[
        "Branch[condition: Cons[Match[value: Param[0], pattern: Is[Int]], Nil], consequence: Cons[Tuple[name: \"Int\", fields: Nil], Nil]]",
        "Branch[condition: Cons[Match[value: Param[0], pattern: \"hi\"], Nil], consequence: Cons[Tuple[name: \"Hi\", fields: Nil], Nil]]",
        "Branch[condition: Cons[Match[value: Param[0], pattern: And[Cons[Not[Nil], Cons[Bind[\"v\"], Nil]]]], Nil], consequence: Cons[Tuple[name: \"Other\", fields: Cons[Local[\"v\"], Nil]], Nil]]",
        "Branch[condition: Cons[Tuple[name: \"None\", fields: Nil], Nil], consequence: []]",
    ]);
    let body = format!(
        "Function[parameters: Nil, parameter: Top, result: [], annotations: Nil, body: Block[branches: {branches}]]"
    );
    quiver()
        .with_modules(ir_module(&body))
        .evaluate("f = %m{}; [f 1, f \"hi\", f <01>, f Nil]")
        .expect("[Int, Hi, Other[<01>], None]");
}

#[test]
fn test_expansion_names_are_hygienic() {
    // The expansion's own binding is apart from the caller's `x`, which it cannot read...
    quiver()
        .with_modules(ir_module("Local[\"x\"]"))
        .evaluate("x = 1; %m{}")
        .expect_error_containing("Undefined variable");
    // ...nor can the caller see the expansion's.
    let branches = ir_list(&[
        "Branch[condition: Cons[Match[value: 5, pattern: Bind[\"x\"]], Cons[Local[\"x\"], Nil]], consequence: []]",
    ]);
    quiver()
        .with_modules(ir_module(&format!("Block[branches: {branches}]")))
        .evaluate("x = 1; [%m{}, x]")
        .expect("[5, 1]");
    // A parameter or type variable must belong to one of the expansion's own functions.
    quiver()
        .with_modules(ir_module("Param[0]"))
        .evaluate("f = #'int { %m{} }; f 1")
        .expect_error_containing("outside a function of its own");
    quiver()
        .with_modules(ir_module(
            "Function[parameters: Nil, parameter: Var[\"t\"], result: [], annotations: Nil, body: Param[0]]",
        ))
        .evaluate("f = #<'t>'t { %m{} }; f 1")
        .expect_error_containing("outside a Function declaring it");
}

#[test]
fn test_get_spread_and_apply_expansion() {
    let pair = "Tuple[name: [], fields: Cons[Labeled[label: \"a\", value: 5], Cons[6, Nil]]]";
    quiver()
        .with_modules(ir_module(&format!("Get[of: {pair}, field: \"a\"]")))
        .evaluate("%m{}")
        .expect("5");
    quiver()
        .with_modules(ir_module(&format!("Get[of: {pair}, field: 1]")))
        .evaluate("%m{}")
        .expect("6");
    // `#_ { t = $; P[...t, b: 2] }`
    let body = "Function[parameters: Nil, parameter: Tuple[name: [], fields: Cons[Labeled[label: \"a\", value: Int], Nil]], result: [], annotations: Nil, body: Block[branches: Cons[Branch[condition: Cons[Match[value: Param[0], pattern: Bind[\"t\"]], Cons[Tuple[name: \"P\", fields: Cons[Spread[source: Local[\"t\"]], Cons[Labeled[label: \"b\", value: 2], Nil]]], Nil]], consequence: []], Nil]]]";
    quiver()
        .with_modules(ir_module(body))
        .evaluate("f = %m{}; f [a: 1]")
        .expect("P[a: 1, b: 2]");
    quiver()
        .with_modules(ir_module(
            "Apply[callee: Member[module: \"num\", member: \"add\", arguments: Nil], arg: Tuple[name: [], fields: Cons[1, Cons[2, Nil]]]]",
        ))
        .evaluate("%m{}")
        .expect("3");
}

#[test]
fn test_expansion_scopes_keep_holes_to_the_caller() {
    // Inside an expansion's own function a span cannot stand for per-call code.
    let body = "Function[parameters: Nil, parameter: Int, result: [], annotations: Nil, body: Unquote[offset: 0, length: 1]]";
    quiver()
        .with_modules(ir_module(body))
        .evaluate("g = #'bin { f = %m{$}; f 5 }; g <ff>")
        .expect_error_containing("Unquote inside a Function");
    // Inside its own block, a span still means the caller's: `~` is the dialect input.
    let branches = ir_list(&[
        "Branch[condition: Cons[Match[value: 1, pattern: 1], Nil], consequence: Cons[Unquote[offset: 0, length: 3], Nil]]",
    ]);
    quiver()
        .with_modules(ir_module(&format!("Block[branches: {branches}]")))
        .evaluate("[x: 9] ~> %m{~.x}")
        .expect("9");
    let branches = ir_list(&[
        "Branch[condition: Cons[Match[value: 1, pattern: 1], Nil], consequence: Cons[Unquote[offset: 0, length: 1], Nil]]",
    ]);
    quiver()
        .with_modules(ir_module(&format!("Block[branches: {branches}]")))
        .evaluate("f = #'int { %m{$} }; f 4")
        .expect("4");
}

#[test]
fn test_expansion_types_are_hygienic() {
    // A lowercase tuple name would inherit the caller's alias.
    quiver()
        .with_modules(ir_module(
            "Function[parameters: Nil, parameter: Tuple[name: \"secret\", fields: Nil], result: [], annotations: Nil, body: Param[0]]",
        ))
        .evaluate("'secret = Hidden; f = %m{}; f Hidden")
        .expect_error_containing("does not start with an uppercase letter");
    // A generated type parameter never captures a caller's type of the same name.
    quiver()
        .with_modules(ir_module(
            "Function[parameters: Cons[\"d\", Nil], parameter: Tuple[name: [], fields: Cons[Var[\"d\"], Cons[Named[module: \"str\", member: [], arguments: Nil], Nil]]], result: [], annotations: Nil, body: Param[0]]",
        ))
        .evaluate("'d = 'int; f = %m{}; f [<01>, \"x\"]")
        .expect("[<01>, \"x\"]");
    quiver()
        .with_modules(ir_module(
            "Function[parameters: Cons[\"t\", Cons[\"t\", Nil]], parameter: Top, result: [], annotations: Nil, body: Param[0]]",
        ))
        .evaluate("%m{}")
        .expect_error_containing("twice");
}

#[test]
fn test_function_results_bodies_and_type_arguments() {
    // A stated result is checked, as one written would be.
    quiver()
        .with_modules(ir_module(
            "Function[parameters: Nil, parameter: Int, result: Bin, annotations: Nil, body: Param[0]]",
        ))
        .evaluate("%m{}")
        .expect_error_containing("Type mismatch");
    // Any expression is a body.
    quiver()
        .with_modules(ir_module(
            "Function[parameters: Nil, parameter: Int, result: [], annotations: Nil, body: 5]",
        ))
        .evaluate("f = %m{}; f 1")
        .expect("5");
    // A member applied with type arguments, as `%data.decode<'int>` would be.
    quiver()
        .with_modules(ir_module(
            "Apply[callee: Member[module: \"data\", member: \"decode\", arguments: Cons[Int, Nil]], arg: \"42\"]",
        ))
        .evaluate("%m{}")
        .expect("42");
}

#[test]
fn test_labelled_node_without_its_label_is_an_error() {
    // A node built with labels is read only by them: a misspelt label is no field, rather than
    // the field at its position. (A dialect declaring `'%meta.expr` has it caught as a type
    // error first; one answering `_` reaches the splicer.)
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        "d = #(content: _) -> _ { Get[of: Tuple[name: [], fields: Cons[1, Nil]], fild: 0] }
         { :dialect d; Ok }"
            .to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("%m{}")
        .expect_error_containing("a Get without its `field`");
}

#[test]
fn test_member_without_a_member_is_the_module() {
    // `member: []` names the module's own value: `%ref` is a function.
    quiver()
        .with_modules(ir_module(
            "Apply[callee: Member[module: \"ref\", member: [], arguments: Nil], arg: Tuple[name: [], fields: Nil]]",
        ))
        .evaluate("%m{} ~> { | ='ref => Ok | No }")
        .expect("Ok");
}

#[test]
fn test_deep_access_is_bounded() {
    let base = "Tuple[name: [], fields: Cons[Labeled[label: \"a\", value: 1], Nil]]";
    let deep = (0..10_000).fold(base.to_string(), |of, _| {
        format!("Get[of: {of}, field: \"a\"]")
    });
    quiver()
        .with_modules(ir_module(&deep))
        .evaluate("%m{}")
        .expect_error_containing("levels deep");
}

#[test]
fn test_function_annotations_expansion() {
    // `#'int { :note "n"; $ }`, with the note read back off the closure.
    quiver()
        .with_modules(ir_module(
            "Function[parameters: Nil, parameter: Int, result: [], annotations: Cons[Annotation[key: \"note\", value: \"n\"], Nil], body: Param[0]]",
        ))
        .evaluate("f = %m{}; [f:note, f 3]")
        .expect("[\"n\", 3]");
    // An annotation is outside the function, so `Param` there has no function to reach.
    quiver()
        .with_modules(ir_module(
            "Function[parameters: Nil, parameter: Int, result: [], annotations: Cons[Annotation[key: \"note\", value: Param[0]], Nil], body: Param[0]]",
        ))
        .evaluate("%m{}")
        .expect_error_containing("outside a function of its own");
}
