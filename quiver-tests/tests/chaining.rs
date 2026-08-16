mod common;
use common::*;

#[test]
fn test_operation_chaining() {
    quiver()
        .evaluate("[1, 2] ~> __integer_add__ ~ ~> [~, 2] ~> __integer_multiply__ ~")
        .expect("6");
}

#[test]
fn test_member_access_chaining() {
    quiver().evaluate("Point[x: 1, y: 2] ~> .x").expect("1");
}

#[test]
fn test_nested_tupple_construction() {
    quiver().evaluate("42 ~> A[B[C[~]]]").expect("A[B[C[42]]]");
}

#[test]
fn test_nested_ripple_contexts() {
    quiver()
        .evaluate("1 ~> [2 ~> [~, ~], ~]")
        .expect("[[2, 2], 1]");
}

#[test]
fn test_standalone_ripple() {
    quiver().evaluate("42 ~> ~").expect("42");
}

#[test]
fn test_standalone_ripple_with_nested_chain() {
    // The ripple in "5 ~> ~" refers to 5, not to the outer 10 — so no field reads the
    // outer value, and the tuple would drop it.
    quiver()
        .evaluate("10 ~> [5 ~> ~]")
        .expect_compile_error(quiver_compiler::compiler::Error::DiscardedChainValue);
}

#[test]
fn test_nested_chain_outer_value_used() {
    // The first field uses the outer value
    quiver().evaluate("10 ~> [~, 5 ~> ~]").expect("[10, 5]");
}

#[test]
fn test_chain_term_must_use_the_flowing_value() {
    // A chain threads a value through its terms, so a term that ignores the one flowing
    // into it drops the work before it — which is a mistake, not a way to start over.
    let cases = [
        "1 ~> [2, 3 ~> ~]", // no field reads the 1
        "5 ~> 99",
        "5 ~> \"abc\"",
        "5 ~> __integer_add__",
        "5 ~> #'int { $ }",
        "5 ~> __integer_add__ [1, 2]", // the argument ignores it, so the call does
    ];
    for case in cases {
        quiver()
            .evaluate(case)
            .expect_compile_error(quiver_compiler::compiler::Error::DiscardedChainValue);
    }
    // Only the head is exempt: that is where a chain chooses what it starts from.
    quiver().evaluate("f = #'int { 99 }; 5 ~> f ~").expect("99");
    quiver().evaluate("[2, 3 ~> ~]").expect("[2, 3]");
    // A block takes the value and may ignore it, which is how a step runs for its effect
    // and continues whatever its result — `;` would gate on the nil instead.
    quiver().evaluate("5 ~> { []; 1 | 2 }").expect("2");
}

#[test]
fn test_sequence_step_starts_from_the_block_value() {
    // Every step starts from the block value — the enclosing block's parameter, or the value
    // piped into it — not from the previous step's result. So `~` in the second step is the
    // argument `9`, and the first step's `5` is discarded.
    quiver()
        .evaluate("f = #'int { 5; ~ }; 9 ~> f ~")
        .expect("9");
    // Which makes `~` at the start of a step mean what `$` means in a function body.
    quiver()
        .evaluate("f = #'int { 5; $ }; 9 ~> f ~")
        .expect("9");
}

#[test]
fn test_each_step_restarts_from_the_block_value() {
    // A sequence is not a pipeline: each step starts from `0` again, so only the last one is
    // the sequence's result and the earlier steps are dead.
    quiver()
        .evaluate(
            "f = #'int { 1; [~, 10] ~> __integer_add__ ~; [~, 100] ~> __integer_add__ ~ }; 0 ~> f ~",
        )
        .expect("100");
    // To carry a value across a step boundary, name it.
    quiver()
        .evaluate(
            "f = #'int { a = [~, 10] ~> __integer_add__ ~; [a, 100] ~> __integer_add__ ~ }; 0 ~> f ~",
        )
        .expect("110");
}

#[test]
fn test_dollar_is_always_the_parameter_across_steps() {
    // `$` always refers to the function parameter, regardless of threading; `~` would be `5`.
    quiver()
        .evaluate("f = #'int { 5; $ }; 9 ~> f ~")
        .expect("9");
}

#[test]
fn test_sequence_short_circuits_on_nil() {
    // A step that yields nil short-circuits the rest of the sequence to nil (threading keeps the
    // existing short-circuit semantics).
    quiver().evaluate("[]; 5").expect("[]");
}

#[test]
fn test_sequence_binding_persists_across_steps() {
    // Bindings persist across steps; naming a binding ignores the threaded value.
    quiver()
        .evaluate("f = #'int { x = 5; [x, $] }; 9 ~> f ~")
        .expect("[5, 9]");
}

#[test]
fn test_whitespace_is_not_a_chain_separator() {
    // Whitespace does not join chain terms: an explicit `~>` is required.
    quiver()
        .evaluate("[3, 4] __integer_add__")
        .expect_parse_failure();
}

#[test]
fn test_newline_is_a_sequence_separator() {
    // A newline is a sequence separator, synonymous with semicolon — including that each step
    // restarts from the block value.
    quiver()
        .evaluate("f = #'int {\n  1\n  [~, 10] ~> __integer_add__ ~\n  [~, 100] ~> __integer_add__ ~\n}\n0 ~> f ~")
        .expect("100");
}

#[test]
fn test_newline_separated_top_level_threads() {
    // Top-level items separated by newlines form one threaded sequence; the final value is the
    // result. Bindings persist across the newlines.
    quiver()
        .evaluate("x = 5\ny = 10\n[x, y] ~> __integer_add__ ~")
        .expect("15");
}

#[test]
fn test_tilde_arrow_is_the_chain_separator() {
    // `a ~> b` chains terms: the value flows left to right.
    quiver().evaluate("[3, 4] ~> __integer_add__ ~").expect("7");
}

#[test]
fn test_leading_tilde_arrow_continues_a_chain_across_lines() {
    // A bare newline ends a chain, but a newline followed by `~>` continues it: this is ONE chain,
    // so the value flows straight through. Threads 0 -> 10 -> 110.
    quiver()
        .evaluate(
            "f = #'int {\n  $\n  ~> [~, 10] ~> __integer_add__ ~\n  ~> [~, 100] ~> __integer_add__ ~\n}\n0 ~> f ~",
        )
        .expect("110");
}

#[test]
fn test_chain_passes_nil_but_sequence_short_circuits() {
    // Within a chain, nil flows into the next term (no short-circuit)...
    quiver().evaluate("[] ~> [~, 5]").expect("[[], 5]");
    // ...but across a sequence separator (semicolon/newline), a nil step short-circuits to nil.
    quiver().evaluate("[]; [~, 5]").expect("[]");
}
