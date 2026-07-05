mod common;
use common::*;

#[test]
fn test_block_scope() {
    quiver().evaluate("x = 1, { x = 2 }, x").expect("1");
}

#[test]
fn test_block_evaluation() {
    quiver().evaluate("x = { 1, 2, 3 }, x").expect("3");
}

#[test]
fn test_block_with_closure() {
    quiver()
        .evaluate("x = 1, { 2 =y, [x, y] __integer_add__ }")
        .expect("3");
}

#[test]
fn test_block_with_parameter() {
    quiver().evaluate("1 { =x => x }").expect("1");
}

#[test]
fn test_block_with_repeated_parameter() {
    quiver()
        .evaluate("3 { =x => [x, x] __integer_add__ }")
        .expect("6");
}

#[test]
fn test_block_with_positional_parameter() {
    quiver().evaluate("[1, 2] { =x => x.0 }").expect("1");
}

/// Format `source` the way `quiv format` does.
fn formatted(source: &str) -> String {
    let ast = quiver_compiler::parse(source).expect("source parses");
    quiver_compiler::format_program(&ast, source)
}

/// Formatting preserves runtime behaviour: redundant-block removal, `$`/union/string sugar, and
/// call-unit chain breaking are all behaviour-neutral, and the compiler now strips the same
/// redundant blocks so `{ chain }` and `chain` compile identically.
#[test]
fn test_format_preserves_behavior() {
    let cases = [
        // redundant block (as a chain term, and as a binding value)
        ("5 { [~, 1] __integer_add__ }", "6"),
        ("x = { [3, 4] __integer_add__ }, x", "7"),
        // nested redundant blocks
        ("{ { { 9 } } }", "9"),
        // a redundant block inside a narrowed branch
        (
            "'u = A['int] | B, f = #'u { | =A[x] => { [x, x] __integer_add__ } | =B => 0 }, A[5] f",
            "10",
        ),
        // a tail call wrapped in a redundant block (TCO must survive)
        (
            "count = #'int { | =0 => Done | { [~, 1] __integer_subtract__ ^ } }, 100000 count",
            "Done",
        ),
        // `$`-access sugar
        ("f = #['int, 'int] { $0 }, [7, 8] f", "7"),
    ];
    for (source, expected) in cases {
        quiver().evaluate(source).expect(expected);
        quiver().evaluate(&formatted(source)).expect(expected);
    }
}

/// A single-branch, binding-free block is a runtime no-op at *any* step count — the compiler lifts
/// its frame, so it compiles identically to the bare sequence and changes no behaviour.
#[test]
fn multi_step_block_is_a_noop() {
    quiver().evaluate("5, 6").expect("6");
    quiver().evaluate("{ 5, 6 }").expect("6");
    quiver()
        .evaluate("1 { [~, 1] __integer_add__, [~, 10] __integer_add__ }")
        .expect("12");
    // nil short-circuits identically whether or not the steps are wrapped.
    quiver().evaluate("[], 5").expect("[]");
    quiver().evaluate("{ [], 5 }").expect("[]");
    // a tail call as the final step still tail-calls after lifting.
    quiver()
        .evaluate("f = #'int { | =0 => Done | { [~, 1] __integer_subtract__, ^ } }, 100000 f")
        .expect("Done");
}

/// The formatter wraps a bare compound consequence in grouping braces; since the block is frame-free
/// the compiler lifts it, so behaviour (and bytecode) is unchanged.
#[test]
fn grouping_braces_preserve_behavior() {
    quiver()
        .evaluate("0 { =0 => Ok, Done | Other }")
        .expect("Done");
    quiver()
        .evaluate("0 { =0 => { Ok, Done } | Other }")
        .expect("Done");
}

/// Regression: a non-exhaustive block's fall-through-to-nil path must pass through the
/// same locals Reset as the success paths. It used to land *after* the Reset, leaving
/// the block parameter's slot live — every later local landed one slot high, so a
/// subsequent binding read back a stale value (here, the old block parameter).
#[test]
fn fall_through_truncates_block_locals() {
    quiver().evaluate("r = 2 { =1 => A }, [r]").expect("[[]]");
    // Same skew, observed through an identity-plus-attach block at the convergence: the
    // annotation chain's Load read the skewed slot and Annotate crashed on an integer.
    quiver()
        .evaluate("r = 2 { =1 => A } { :k 9 } :k, r")
        .expect("9");
}

/// Regression: passing a step boundary proves the step's *result* non-nil — nothing
/// more. A chain containing a match yields the verdict while its provenance points at
/// the matched value's source, and a bare binder's verdict is Ok even on nil, so
/// narrowing the source there compiled nil-handling branches away as dead.
#[test]
fn bare_binder_match_does_not_narrow_its_source() {
    quiver()
        .evaluate("f = #(('int | [])) { $ =y, $ { =[] => 1 | 2 } }, [] f")
        .expect("1");
    quiver()
        .evaluate("f = #(('int | [])) { $ =y, y =[], 1 }, [] f")
        .expect("1");
    // The sound half stays: a bare access step's result IS the variable, so passing the
    // short-circuit does narrow it (`=x, x, use-x`).
    quiver()
        .evaluate("g = #'int { =0 => 1 }, 0 g =x, x, [x, 1] __integer_add__")
        .expect("2");
}
