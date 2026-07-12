mod common;
use common::*;

// Failure provenance: debug builds stamp nil *results* (failed matches, exhausted
// blocks, deliberate nil results) with an `origin` annotation naming their source site.
// The stamp is row-invisible — rows keep saying exact-empty, so types are identical in
// debug and release — and fresh-only, so a propagating failure keeps its original site.
// Tooling reads the value directly; programs can reach it with a structural checked
// retrieval (`:((line: 'int))origin`).

#[test]
fn test_match_failure_is_stamped() {
    quiver()
        .debug()
        .evaluate("x = 5; x =6")
        .expect("[]")
        .expect_origin("match failed at test:1:8");
}

#[test]
fn test_release_builds_carry_no_origin() {
    quiver().evaluate("x = 5; x =6").expect_no_origin();
}

#[test]
fn test_origin_propagates_out_of_calls() {
    // The failure site is inside `f`; the nil short-circuits out of the call unchanged,
    // so the caller sees the callee's site, not its own.
    quiver()
        .debug()
        .evaluate("f = #'int { =0 => Ok }; 5 f")
        .expect_origin("match failed at test:1:13");
}

#[test]
fn test_recovery_swallow_gets_its_own_site() {
    // A user `[]` in result position is a failure by position: the fallback branch
    // discards the original diagnosis, and the fresh nil is stamped at the swallow.
    quiver()
        .debug()
        .evaluate("5 { | =6 => Ok | [] }")
        .expect_origin("nil result at test:1:18");
}

#[test]
fn test_nil_data_in_a_field_is_not_stamped() {
    // Nil in a non-result position (a tuple field) is data, not a failure — the tuple
    // result carries no origin, and neither does the [] inside it.
    quiver()
        .debug()
        .evaluate("[[], 1]")
        .expect("[[], 1]")
        .expect_no_origin();
}

#[test]
fn test_extracted_nil_result_is_stamped() {
    // ...but once that nil becomes a *result* (the step short-circuits on it), the
    // positional rule stamps it at the extraction.
    quiver()
        .debug()
        .evaluate("[[], 1] .0")
        .expect("[]")
        .expect_origin("nil result at test:1:1");
}

#[test]
fn test_program_reads_origin_by_shape() {
    // The in-language read: a structural checked retrieval — no `%debug` module needed,
    // the shape is enough. Exercises the row-invisible carve-out (exact-empty rows
    // don't prove `origin` absent).
    // (Field extraction through the bare partial waits on a named site type from a
    // future `%debug` module — partial-typed field binding resolves indices against the
    // partial's own layout. Presence is observable today.)
    quiver()
        .debug()
        .evaluate("f = #'int { =0 => Ok }; x = 5 f; x:((line: 'int))origin { =[] => NoOrigin | HasOrigin }")
        .expect("HasOrigin");
}

#[test]
fn test_checked_origin_read_is_nil_in_release() {
    // Same program, release build: same types, the retrieval just answers nil.
    quiver()
        .evaluate("f = #'int { =0 => Ok }; x = 5 f; x:((line: 'int))origin =[]")
        .expect("Ok");
}

#[test]
fn test_origin_coexists_with_error_payload() {
    // `:error` is the deliberate diagnosis channel; `origin` the automatic one. Both
    // ride the same nil.
    quiver()
        .debug()
        .evaluate(
            "div = #['int, 'int] { | =[_, 0] => [] { :error DivZero } | __integer_divide__ };
             [4, 0] div :error",
        )
        .expect("DivZero");
}

#[test]
fn test_stamped_nil_is_still_nil() {
    // Stamps are invisible to matching, equality and short-circuiting: a stamped nil
    // returned from a call still matches `=[]`, and still short-circuits a sequence.
    quiver()
        .debug()
        .evaluate("f = #'int { =0 => Ok }; 5 f { =[] => StillNil | Huh }")
        .expect("StillNil");
    quiver().debug().evaluate("5 =6; Unreachable").expect("[]");
}

#[test]
fn test_module_sites_name_the_module() {
    // A failure inside an imported module points into that module's source.
    quiver()
        .debug()
        .evaluate("[5, 0] %int.div")
        .expect_origin("nil result at int:26:18");
}
