mod common;
use common::*;

// Matching against the nil pattern `=[]` is the test for nil: it evaluates to Ok when the value is
// nil and [] otherwise. (This replaces the former `<>` operator.)

#[test]
fn test_nil_match_nil() {
    quiver().evaluate("[] ~> =[]").expect("Ok");
}

#[test]
fn test_nil_match_ok() {
    quiver().evaluate("Ok ~> =[]").expect("[]");
}

#[test]
fn test_nil_match_integer() {
    quiver().evaluate("42 ~> =[]").expect("[]");
}

#[test]
fn test_nil_match_tuple() {
    quiver().evaluate("[1, 2] ~> =[]").expect("[]");
}

#[test]
fn test_nil_match_of_prior_result() {
    // A verdict is inverted with a block, not a chained `=[]` (a fallible match must
    // end its chain): a failed equality falls to the Ok branch...
    quiver()
        .evaluate("a = 1; 2 ~> { | =&a => [] | Ok }")
        .expect("Ok");
    // ...while a successful one takes the nil branch.
    quiver()
        .evaluate("a = 42; 42 ~> { | =&a => [] | Ok }")
        .expect("[]");
}

#[test]
fn test_double_nil_match() {
    // Inverting a nil test likewise composes through a block.
    quiver().evaluate("[] ~> { | =[] => [] | Ok }").expect("[]");
    quiver().evaluate("42 ~> { | =[] => [] | Ok }").expect("Ok");
}

#[test]
fn test_pin_of_nil_valued_variable_matches_nil() {
    // Pinning a variable whose value is nil must match a nil value: the Equal
    // instruction's success result is a truth flag (Ok), not the compared value —
    // "equal nils" used to answer nil and read as a failed match.
    quiver().evaluate("y = []; [] ~> =&y").expect("Ok");
    quiver().evaluate("y = []; Ok ~> =&y").expect("[]");
    // A nil-valued pin in field position binds its siblings.
    quiver()
        .evaluate("y = []; [[], 7] ~> =[&y, n]; n")
        .expect("7");
}

#[test]
fn test_fallible_match_must_end_its_chain() {
    // Nothing short-circuits within a chain, so a term after a fallible match would
    // run whether or not it matched — observing bindings and narrowings that don't
    // hold. A fallible match must be the last term of its chain (its verdict then
    // gates the step boundary or branch); this holds for binding and bindingless
    // patterns alike.
    quiver()
        .evaluate(
            r#"'g = T['int] | M['int];
               f = #'g { $ ~> =T[x] ~> %num.gt? [x, 0] };
               f T[1]"#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::FallibleMatchNotChainFinal);
    quiver()
        .evaluate("a = 1; 2 ~> =&a ~> =[]")
        .expect_compile_error(quiver_compiler::compiler::Error::FallibleMatchNotChainFinal);
}

#[test]
fn test_irrefutable_match_may_continue_chain() {
    // A bare binder always succeeds, so its binding is always valid and the chain may
    // continue through it. The value flowing on is the match's verdict, and the binding is
    // in scope for the terms after it.
    quiver().evaluate("5 ~> =x ~> [~, x]").expect("[Ok, 5]");
}

#[test]
fn test_fallible_match_in_field_is_verdict_data_without_narrowing() {
    // In a tuple field nothing gates on the verdict — it is data (`ok?`-style flags
    // are fine), but the match must not narrow the scrutinee for sibling fields: a
    // failed `=T[_]` in field 0 leaves `$` a full 'g in field 1.
    quiver()
        .evaluate(
            r#"'g = T['int] | M['int];
               f = #'g { [$ ~> =T[_], $ ~> { | =M[_] => GotM | Other }] };
               [f M[1], f T[5]]"#,
        )
        .expect("[[[], GotM], [Ok, Other]]");
    // And a fallible match may not BIND in a field — the bindings could never be
    // relied on (the surrounding code runs whether or not it matched).
    quiver()
        .evaluate(
            r#"'g = T['int] | M['int];
               f = #'g { [$ ~> =T[x], 1] };
               f T[5]"#,
        )
        .expect_compile_error(
            quiver_compiler::compiler::Error::FallibleMatchBindingsInValueChain {
                bindings: vec!["x".to_string()],
            },
        );
}
