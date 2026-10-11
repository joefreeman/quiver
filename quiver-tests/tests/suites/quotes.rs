use crate::common::*;
use std::collections::HashMap;

/// A module `%g` whose dialect expands to the code `body` answers, so that `%g{}` is that code,
/// spliced.
fn splicing(body: &str) -> HashMap<Vec<String>, String> {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["g".to_string()],
        format!("q = #(content: _) {{ {body} }}\n{{ :dialect q; Ok }}"),
    );
    modules
}

/// `%g{}` as the quoted code, lowered and spliced back.
fn quoting(quoted: &str) -> HashMap<Vec<String>, String> {
    splicing(&format!("%meta{{ {quoted} }}"))
}

/// Quoted code, spliced back, means what it meant written out.
fn round_trip(code: &str, expected: &str) {
    quiver().evaluate(code).expect(expected);
    quiver()
        .with_modules(quoting(code))
        .evaluate("%g{}")
        .expect(expected);
}

#[test]
fn test_quote_lowers_code_to_its_ir() {
    quiver()
        .evaluate("n = 3; %meta{ %num.add [`n, 1] }")
        .expect(
            "Apply[callee: Member[module: \"num\", member: \"add\", arguments: Nil], arg: Tuple[name: [], \
             fields: Cons[3, Cons[1, Nil]]]]",
        );
    // A binder is the quote's own, and so is `$` inside a function it writes.
    quiver().evaluate("%meta{ #'int { =x; [x, $] } }").expect(
        "Function[parameters: Nil, parameter: Int, result: [], annotations: Nil, body: Block[branches: Cons[Branch[\
             condition: Cons[Match[value: Ripple, pattern: Bind[\"x\"]], Cons[Tuple[name: [], \
             fields: Cons[Local[\"x\"], Cons[Param[0], Nil]]], Nil]], consequence: []], Nil]]]",
    );
}

#[test]
fn test_quote_round_trips_values_and_calls() {
    round_trip(
        "[1, x: <0a>, \"s\", A[b: 2]]",
        "[1, x: <0a>, \"s\", A[b: 2]]",
    );
    round_trip("%num.add [2, 3]", "5");
    round_trip("{ f = #'int { %num.mul [$, 2] }; f 3 }", "6");
    round_trip("{ 5 ~> [~, 1] ~> .1 }", "1");
    round_trip("{ t = [a: 1]; [...t, b: 2] }", "[a: 1, b: 2]");
    round_trip("{ id = #<'t>'t -> 't { $ }; id 4 }", "4");
    round_trip(
        "{ apply = #(#'int -> 'int) { $ 3 }; apply #'int { $ } }",
        "3",
    );
}

#[test]
fn test_quote_round_trips_patterns_and_branches() {
    round_trip(
        "{ g = #_ { | ='int => Int | =\"s\" => S | =[a, b: _] => a | =(A | B) => AB | Other }
           [g 1, g \"s\", g [5, b: 1], g B, g <00>] }",
        "[Int, S, 5, AB, Other]",
    );
    round_trip(
        "{ g = #('int | []) { | =(\\[] & n) => [n] | None }; [g 1, g []] }",
        "[[1], None]",
    );
}

#[test]
fn test_quote_round_trips_types() {
    round_trip(
        "{ f = #[x: 'int, y: ('%str | [])] { $x }; f [x: 1, y: []] }",
        "1",
    );
    round_trip("{ f = #(x: 'int, *'bin) { $x }; f [x: 2, <00>] }", "2");
    round_trip(
        "{ f = #'%list<'int> { %list.count $ }; f Cons[1, Nil] }",
        "1",
    );
    // A module named alone is its own value.
    round_trip("%ref [] ~> { | ='ref => Ok | No }", "Ok");
}

#[test]
fn test_quote_keeps_function_annotations() {
    quiver()
        .with_modules(quoting("#'int { :doc \"doubles\"; %num.mul [$, 2] }"))
        .evaluate("f = %g{}; [f 2, f:doc]")
        .expect("[4, \"doubles\"]");
}

#[test]
fn test_quote_holes_in_patterns_and_types() {
    quiver()
        .with_modules(splicing(
            "p = Bind[\"v\"]
             t = Int
             %meta{ { f = #`t { =`p => `(Local[\"v\"]) }; f 2 } }",
        ))
        .evaluate("%g{}")
        .expect("2");
}

#[test]
fn test_quoted_partial_patterns_and_pins() {
    quiver()
        .with_modules(quoting(
            "#[x: 'int, y: 'int] { =(x: a); $y ~> { | =^a => Same | Different } }",
        ))
        .evaluate("f = %g{}; [f [x: 1, y: 1], f [x: 1, y: 2]]")
        .expect("[Same, Different]");
}

#[test]
fn test_quote_errors() {
    quiver()
        .evaluate("%meta{ x }")
        .expect_error_containing("names `x`, which the code doesn't bind");
    quiver()
        .evaluate("%meta{ $ }")
        .expect_error_containing("outside a function of its own");
    quiver()
        .evaluate("%meta{ %list{ 1 } }")
        .expect_error_containing("can't express the dialect %list");
    quiver()
        .evaluate("'t = 'int; %meta{ #'t { $ } }")
        .expect_error_containing("which the code doesn't declare");
    quiver()
        .evaluate("%meta{ [ }")
        .expect_error_containing("is given code that doesn't parse");
    // A hole outside a quote is a parse error, in any position...
    quiver()
        .evaluate("x = 1; `x")
        .expect_error_containing("stands only in quoted code");
    quiver()
        .evaluate("p = 1; 1 ~> =`p")
        .expect_error_containing("stands only in quoted code");
    quiver()
        .evaluate("f = #`t { $ }")
        .expect_error_containing("stands only in quoted code");
    // ...including inside a hole, whose code is the quoting code's own.
    quiver()
        .evaluate("x = 1; %meta{ [`(`x)] }")
        .expect_error_containing("stands only in quoted code");
}
