use crate::common::*;
use std::collections::HashMap;

/// A module `%m` whose dialect is `body`, a function of the context's `code` (bound as `code`).
fn code_dialect(body: &str) -> HashMap<Vec<String>, String> {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        format!("d = #(code: '%meta.expr) -> ('%meta.expr | []) {{ code = $code; {body} }}\n{{ :dialect d; Ok }}"),
    );
    modules
}

/// A dialect body answering its `code` as the value it is, rather than as code to run.
const AS_DATA: &str = "%meta.reflect code ~> ='%meta.data ~> %meta.data_expr";

#[test]
fn test_code_is_the_content_as_ir() {
    quiver()
        .with_modules(code_dialect(AS_DATA))
        .evaluate("%m{ %num.add [1, 2] }")
        .expect(
            "Apply[callee: Member[module: \"num\", member: \"add\", arguments: Nil], arg: \
             Tuple[name: [], fields: Cons[1, Cons[2, Nil]]]]",
        );
    // Several steps are a block of them.
    quiver()
        .with_modules(code_dialect(AS_DATA))
        .evaluate("%m{ 1; 2 }")
        .expect(
            "Block[branches: Cons[Branch[condition: Cons[1, Cons[2, Nil]], consequence: []], Nil]]",
        );
}

#[test]
fn test_code_spliced_back_means_what_it_meant() {
    quiver()
        .with_modules(code_dialect("code"))
        .evaluate("%m{ %num.add [1, 2] }")
        .expect("3");
    quiver()
        .with_modules(code_dialect("code"))
        .evaluate("f = %m{ #'int { =x; [x, $] } }; f 5")
        .expect("[5, 5]");
    // The flowing value reaches the code as `~`.
    quiver()
        .with_modules(code_dialect("code"))
        .evaluate("4 ~> %m{ [~, ~] }")
        .expect("[4, 4]");
}

#[test]
fn test_code_can_be_transformed() {
    // `-(…)` of whatever the content computes.
    quiver()
        .with_modules(code_dialect(
            "Apply[callee: Member[module: \"num\", member: \"neg\", arguments: Nil], arg: code]",
        ))
        .evaluate("%m{ %num.add [1, 2] }")
        .expect("-3");
}

#[test]
fn test_a_hole_is_its_span() {
    // The content is ` [`x, `(%num.add [1, 2])] `: `x` at 3, the call at 8.
    quiver()
        .with_modules(code_dialect(AS_DATA))
        .evaluate("%m{ [`x, `(%num.add [1, 2])] }")
        .expect(
            "Tuple[name: [], fields: Cons[Hole[offset: 3, length: 1], \
             Cons[Hole[offset: 8, length: 15], Nil]]]",
        );
    // In a pattern and a type too.
    quiver()
        .with_modules(code_dialect(AS_DATA))
        .evaluate("%m{ =`p }")
        .expect("Match[value: Ripple, pattern: Hole[offset: 3, length: 1]]");
    quiver()
        .with_modules(code_dialect(AS_DATA))
        .evaluate("%m{ #`t { $ } }")
        .expect(
            "Function[parameters: Nil, parameter: Hole[offset: 3, length: 1], result: [], \
             annotations: Nil, body: Param[0]]",
        );
    // Returned, a hole means nothing.
    quiver()
        .with_modules(code_dialect("code"))
        .evaluate("x = 1; %m{ `x }")
        .expect_error_containing("returned a Hole");
}

#[test]
fn test_code_names_only_its_own() {
    let error = quiver()
        .with_modules(code_dialect("code"))
        .evaluate("y = 1; %m{ [1, y] }")
        .expect_located_compile_error();
    assert!(
        error
            .to_string()
            .contains("names `y`, which the code doesn't bind"),
        "{error}"
    );
    // Content starts at 1:11 (after `%m{`), so `y` at offset 5 is column 16.
    assert_eq!(
        error.span.map(|span| (span.line, span.column)),
        Some((1, 16))
    );
}

#[test]
fn test_code_that_does_not_parse_is_an_error_where_written() {
    let error = quiver()
        .with_modules(code_dialect("code"))
        .evaluate("%m{ [1, }")
        .expect_located_compile_error();
    assert!(error.to_string().contains("doesn't parse"), "{error}");
    assert!(
        error
            .span
            .is_some_and(|span| span.line == 1 && span.column >= 4)
    );
}

#[test]
fn test_content_is_parsed_only_for_a_dialect_that_asks() {
    // Not Quiver, and not read as Quiver: the dialect doesn't name `code`.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        "d = #(content: Str['bin]) { 7 }\n{ :dialect d; Ok }".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("%m{ <<not quiver>> }")
        .expect("7");
}

#[test]
fn test_reflect_spells_out_data() {
    quiver().evaluate("%meta.reflect P[x: 1, <02>]").expect(
        "Data[name: \"P\", fields: Cons[Labeled[label: \"x\", value: 1], Cons[<02>, Nil]]]",
    );
    // A string is a `Str`, and nil a tuple with neither name nor fields.
    quiver().evaluate("%meta.reflect [\"a\", []]").expect(
        "Data[name: [], fields: Cons[Data[name: \"Str\", fields: Cons[<61>, Nil]], \
             Cons[Data[name: [], fields: Nil], Nil]]]",
    );
    quiver()
        .evaluate("%meta.reflect -12345678901234567890")
        .expect("-12345678901234567890");
    // The tree is a `'%meta.data`, by its labels as well as its shape.
    quiver()
        .evaluate("%meta.reflect P[x: 1] ~> ='%meta.data ~> =Data(name: ~)")
        .expect("\"P\"");
}

#[test]
fn test_reflect_is_instantiated_by_its_module() {
    // `%meta` exports the builtin instantiated at its tree type, so it takes no more.
    quiver()
        .evaluate("%meta.reflect<'int> 1")
        .expect_error_containing("Too many type arguments");
}

#[test]
fn test_reflect_answers_nil_for_a_value_with_identity() {
    quiver()
        .evaluate("%meta.reflect [1, #'int { $ }]")
        .expect("[]");
}
