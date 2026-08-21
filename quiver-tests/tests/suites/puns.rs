// Field punning: `(a, b)` is shorthand for `[a: &a, b: &b]`, and `Foo(a, b)` for
// `Foo[a: &a, b: &b]`. An identifier inside `(…)` is a *name* — the same reading `()` already
// has in the partial type `(x: 'int)` and the partial pattern `(x, y)` — so it lends both the
// field's label and its value. The value is always taken by reference, so a callable entry
// names the function rather than calling it, and a punned tuple is pure repackaging.
//
// Only puns may appear: there is no mixing with explicit `k: v` entries, so `()` never becomes
// a second spelling for a general tuple literal.

use crate::common::*;

#[test]
fn test_pun_builds_labeled_tuple() {
    quiver()
        .evaluate("a = 1; b = 2; (a, b)")
        .expect("[a: 1, b: 2]");
}

#[test]
fn test_named_pun_keeps_tuple_name() {
    quiver()
        .evaluate("a = 1; b = 2; Foo(a, b)")
        .expect("Foo[a: 1, b: 2]");
}

#[test]
fn test_single_entry_pun() {
    quiver().evaluate("a = 1; (a)").expect("[a: 1]");
}

#[test]
fn test_trailing_comma_allowed() {
    quiver()
        .evaluate("a = 1; b = 2; (a, b,)")
        .expect("[a: 1, b: 2]");
}

// The defining behaviour: a pun is a name, so the flowing value never reaches it and a
// callable entry is referenced rather than called. This is what makes a record of functions
// — the idiom every module's export tuple uses — expressible.

#[test]
fn test_bracket_field_calls_where_a_pun_references() {
    // A field holds an expression and a pun holds a name, but neither calls: both land on
    // the function itself, recovered here by applying it.
    quiver()
        .evaluate("f = #[] { 1 }; [f: f] ~> .f ~> ~ []")
        .expect("1");
    quiver()
        .evaluate("f = #[] { 1 }; (f) ~> .f ~> ~ []")
        .expect("1");
}

#[test]
fn test_leading_ampersand_accepted_and_redundant() {
    // `&` says what a pun already means, so it is allowed (shortening `[f: &f]` by deleting
    // the label alone) and carries no information; the formatter drops it.
    quiver()
        .evaluate("f = #[] { 1 }; g = #[] { 2 }; (f, g) ~> .g ~> ~ []")
        .expect("2");
}

#[test]
fn test_record_of_functions_is_callable_through_the_field() {
    quiver()
        .evaluate("inc = #'int { __integer_add__ [~, 1] }; r = (inc); 10 ~> r.inc ~")
        .expect("11");
}

#[test]
fn test_flowing_value_does_not_reach_a_pun() {
    // A pun reads the flowing value nowhere, so piping into one discards it — as piping
    // into any tuple whose fields all ignore it does.
    quiver()
        .evaluate("f = #'int { __integer_add__ [~, 1] }; 5 ~> (f)")
        .expect_compile_error(quiver_compiler::compiler::Error::DiscardedChainValue);
    quiver()
        .evaluate("f = #'int { __integer_add__ [~, 1] }; (f) ~> .f ~> ~ 100")
        .expect("101");
}

// Punnable paths: the label is the final named segment.

#[test]
fn test_parameter_field_pun() {
    quiver()
        .evaluate("f = #[x: 'int, y: 'int] { ($x, $y) }; f [x: 1, y: 2]")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_outer_parameter_pun() {
    quiver()
        .evaluate("f = #[u: 'int] { #'int { ($$u) } }; f [u: 7] ~> ~ 9")
        .expect("[u: 7]");
}

#[test]
fn test_variable_path_pun() {
    quiver()
        .evaluate("p = [x: 1, y: 2]; (p.x, p.y)")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_nested_path_takes_final_segment() {
    quiver()
        .evaluate("a = [inner: [deep: 5]]; (a.inner.deep)")
        .expect("[deep: 5]");
}

#[test]
fn test_import_member_pun() {
    quiver()
        .evaluate("r = (%num.add); [3, 4] ~> r.add ~")
        .expect("7");
}

// A pun composes wherever a tuple literal does.

#[test]
fn test_pun_nested_in_a_tuple() {
    quiver()
        .evaluate("a = 1; b = 2; [(a, b), 9]")
        .expect("[[a: 1, b: 2], 9]");
}

#[test]
fn test_pun_as_call_argument() {
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { $a }; a = 3; b = 4; f (a, b)")
        .expect("3");
}

#[test]
fn test_pun_piped_into_a_call() {
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { $b }; a = 3; b = 4; (a, b) ~> f ~")
        .expect("4");
}

#[test]
fn test_pun_matches_the_equivalent_literal() {
    quiver()
        .evaluate("a = 1; b = 2; (a, b) ~> =[a: 1, b: 2]")
        .expect("Ok");
}

// A leading `(a, b) = p` is still a partial-pattern binding: the binding alternative is tried
// before the chain, so the same text destructures there and constructs in value position.

#[test]
fn test_partial_pattern_binding_still_destructures() {
    quiver()
        .evaluate("p = [a: 1, b: 2, c: 3]; (a, b) = p; (a, b)")
        .expect("[a: 1, b: 2]");
}

#[test]
fn test_in_chain_partial_match_unaffected() {
    quiver()
        .evaluate("[a: 1, b: 2] ~> =(a, b); (a, b)")
        .expect("[a: 1, b: 2]");
}

// Rejections. `()` takes puns and nothing else.

#[test]
fn test_empty_parens_rejected() {
    quiver().evaluate("()").expect_parse_failure();
}

#[test]
fn test_explicit_field_rejected() {
    quiver().evaluate("a = 1; (a: a)").expect_parse_failure();
}

#[test]
fn test_mixing_pun_and_explicit_field_rejected() {
    quiver().evaluate("a = 1; (a, b: 2)").expect_parse_failure();
}

#[test]
fn test_literal_entry_rejected() {
    quiver().evaluate("(1, 2)").expect_parse_failure();
}

#[test]
fn test_positional_index_has_no_label() {
    quiver()
        .evaluate("p = [1, 2]; (p.0)")
        .expect_parse_failure();
}

#[test]
fn test_annotation_retrieval_has_no_label() {
    quiver()
        .evaluate("f = #[] { 1 } ~> { :doc \"d\" }; (f:doc)")
        .expect_parse_failure();
}

#[test]
fn test_builtin_has_no_label() {
    quiver()
        .evaluate("(__integer_add__)")
        .expect_parse_failure();
}

#[test]
fn test_bare_parameter_has_no_label() {
    quiver()
        .evaluate("f = #'int { ($) }; f 1")
        .expect_parse_failure();
}

#[test]
fn test_ripple_root_rejected() {
    quiver().evaluate("[w: 5] ~> (~.w)").expect_parse_failure();
}
