use crate::common::*;

// Matching against the nil pattern `=[]` is the test for nil: it evaluates to Ok when the value is
// nil and [] otherwise.

#[test]
fn test_nil_match_nil() {
    quiver()
        .evaluate("[] ~> { =[] => IsNil | NotNil }")
        .expect("IsNil");
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
        .evaluate("a = 1; 2 ~> { | =^a => [] | Ok }")
        .expect("Ok");
    // ...while a successful one takes the nil branch.
    quiver()
        .evaluate("a = 42; 42 ~> { | =^a => [] | Ok }")
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
    // Pinning a variable whose value is nil must match a nil value — "equal nils" used to
    // answer nil and read as a failed match.
    quiver()
        .evaluate("y = []; [] ~> { =^y => Matched | Missed }")
        .expect("Matched");
    quiver()
        .evaluate("y = []; Ok ~> { =^y => Matched | Missed }")
        .expect("Missed");
    // A nil-valued pin in field position binds its siblings.
    quiver()
        .evaluate("y = []; [[], 7] ~> =[^y, n]; n")
        .expect("7");
}

#[test]
fn test_fallible_match_mid_chain_fails_its_step() {
    // A failed match leaves its step wherever it stands, so the terms after it run only on
    // success, where its bindings and narrowings hold.
    quiver()
        .evaluate(
            r#"'g = T['int] | M['int];
               f = #'g { $ ~> =T[x] ~> [~, x] };
               [f T[1], f M[1]]"#,
        )
        .expect("[[T[1], 1], []]");
    quiver()
        .evaluate("a = 1; [2 ~> { =^a ~> [~] => Went | Stopped }, 1 ~> =^a ~> [~]]")
        .expect("[Stopped, [1]]");
    // A binding after the match is part of the same step.
    quiver()
        .evaluate("f = #('int | 'bin) { n = $ ~> ='int; [n] }; [f 3, f <01>]")
        .expect("[[3], []]");
}

#[test]
fn test_irrefutable_match_may_continue_chain() {
    // A bare binder always succeeds, so its binding is always valid and the chain may
    // continue through it. The value flowing on is the matched value, and the binding is
    // in scope for the terms after it.
    quiver().evaluate("5 ~> =x ~> [~, x]").expect("[5, 5]");
}

#[test]
fn test_fallible_match_in_field_is_verdict_data_without_narrowing() {
    // In a tuple field nothing gates on the verdict, so a fallible match there is wrapped in a
    // block, which yields the value or nil as data — and it must not narrow the scrutinee for
    // sibling fields: a failed `=T[_]` in field 0 leaves `$` a full 'g in field 1.
    quiver()
        .evaluate(
            r#"'g = T['int] | M['int];
               f = #'g { [$ ~> { =T[_] }, $ ~> { | =M[_] => GotM | Other }] };
               [f M[1], f T[5]]"#,
        )
        .expect("[[[], GotM], [T[5], Other]]");
    // A bare fallible match in a field fails the whole step, dropping the tuple built so far,
    // so its bindings and narrowing hold for the rest of the step.
    quiver()
        .evaluate(
            r#"'g = T['int] | M['int];
               f = #'g { [0, $ ~> =T[x], x] };
               [f T[5], f M[1]]"#,
        )
        .expect("[[0, T[5], 5], []]");
}

#[test]
fn test_failure_inside_a_half_built_expression_unwinds_it() {
    // A failure deep in nested tuples drops everything built so far, leaving only its nil.
    quiver()
        .evaluate(
            "f = #('int | 'bin) { | [a: 1, b: [x: $ ~> ='int, y: 2]] => Got | Other }
             [f 5, f <01>]",
        )
        .expect("[Got, Other]");
    // In a call's argument, the callee is dropped with it.
    quiver()
        .evaluate("f = #('int | 'bin) { %num.add [$ ~> ='int, 1] }; [f 5, f <01>]")
        .expect("[6, []]");
    // The next branch runs on a clean stack, with the earlier branch's locals cleared.
    quiver()
        .evaluate(
            "f = #('int | 'bin) {
               | [a: 0, b: $ ~> =('int & n), c: n] => Int[n]
               | x = $; Other[x]
             }
             [f 5, f <01>]",
        )
        .expect("[Int[5], Other[<01>]]");
}

#[test]
fn test_repeated_unwinding_leaves_the_stack_balanced() {
    // Each round fails a match inside a half-built tuple and recurses; a leak would grow the
    // stack round by round.
    quiver()
        .evaluate(
            "count = #[n: 'int, acc: 'int] {
               | $n ~> =0 => $acc
               | [pad: $n, v: <01> ~> ='int] => Never
               | ^ [n: __integer_subtract__ [$n, 1], acc: __integer_add__ [$acc, 1]]
             }
             count [n: 1000, acc: 0]",
        )
        .expect("1000");
}
