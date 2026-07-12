// The `?` state-sample operator (docs/process-state.md), v1 checked form `?('t)p`:
// a process's observable state is the argument its root function was most recently
// (tail-)entered with — the spawn init, then each root-frame tail call. Samples are
// snapshots (never waits), runtime-tested against the stated shape (mismatch → nil),
// and keep working after termination. The default two-worker test environment places
// spawned processes on a different worker than the main process, so these exercise
// the remote (routed) read path as well as the local one.
mod common;
use common::*;

#[test]
fn test_sample_spawn_init() {
    // A live (parked) process's state is its spawn argument.
    quiver()
        .evaluate(
            r#"
            f = #'int { !#'int };
            p = 7 ~> @f;
            ?('int)p
            "#,
        )
        .expect("7");
}

#[test]
fn test_sample_reflects_tail_call_transitions() {
    // Each root-frame `^` updates the observable state; it persists after termination.
    quiver()
        .evaluate(
            r#"
            'status = Loading | Done['int]
            f = #'status { | =Loading => ^ Done[42] | =Done[x] => x }
            p = Loading ~> @f
            !p
            ?('status)p
            "#,
        )
        .expect("Done[42]");
}

#[test]
fn test_sample_reflects_named_tail_call_transition() {
    // `^f` into another function transitions the state to that function's argument.
    quiver()
        .evaluate(
            r#"
            run = #['int, 'int] {
              | =[0, acc] => acc
              | =[n, acc] => ^ [__integer_subtract__ [n, 1], __integer_add__ [acc, n]]
            }
            init = #'int { ^run [$, 0] }
            p = 3 ~> @init
            !p
            ?(['int, 'int])p
            "#,
        )
        .expect("[0, 6]");
}

#[test]
fn test_sample_shape_mismatch_is_nil() {
    // The checked form gates the sample on the stated shape, like checked annotation
    // retrieval: an incompatible state answers nil.
    quiver()
        .evaluate(
            r#"
            f = #'int { $ };
            p = 5 ~> @f;
            !p;
            [?('bin)p]
            "#,
        )
        .expect("[[]]");
}

#[test]
fn test_sample_nilary_process_state_is_nil() {
    // A nilary process's state is nil, and the sample yields it.
    quiver()
        .evaluate(
            r#"
            p = @{ 42 };
            !p;
            [?([])p]
            "#,
        )
        .expect("[[]]");
}

#[test]
fn test_bare_sample_is_a_compile_error() {
    // Bare `?p` parses but is rejected in typing until inferred state types land.
    quiver()
        .evaluate("f = #'int { $ }; p = 5 ~> @f; ?p")
        .expect_error_containing("inferred state type");
}

#[test]
fn test_sample_requires_a_process() {
    quiver()
        .evaluate("x = 5; ?('int)x")
        .expect_error_containing("process");
}
