// The `?` state-sample operator, bare form `?p`: a process's
// observable state is the argument its root function was most recently (tail-)entered
// with — the spawn init, then each root-frame tail call. The state *type* rides the
// process type: inferred at spawn sites as the union of parameter types over the root
// function's tail-call closure, or granted on a declared process type with a `?'s`
// clause (`(@'msg ?'s)`). A sample is a snapshot (never waits) with no runtime test —
// soundness rests on write-site checking plus strict state subtyping at declared
// boundaries. The default two-worker test environment places spawned processes on a
// different worker than the main process, so these exercise the remote (routed) read
// path as well as the local one.
use crate::common::*;

#[test]
fn test_sample_spawn_init() {
    // A live (parked) process's state is its spawn argument.
    quiver()
        .evaluate(
            r#"
            f = #'int { !#'int };
            p = 7 ~> @f ~;
            ?p
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
            p = Loading ~> @f ~
            !p
            ?p
            "#,
        )
        .expect("Done[42]");
}

#[test]
fn test_sample_reflects_named_tail_call_transition() {
    // `^f` into another function transitions the state to that function's argument;
    // the inferred state type is the union over the tail-call closure, so `?p` types.
    quiver()
        .evaluate(
            r#"
            run = #['int, 'int] {
              | =[0, acc] => acc
              | =[n, acc] => ^ [__integer_subtract__ [n, 1], __integer_add__ [acc, n]]
            }
            init = #'int { ^run [$, 0] }
            p = 3 ~> @init ~
            !p
            ?p
            "#,
        )
        .expect("[0, 6]");
}

#[test]
fn test_sample_narrows_with_pattern() {
    // The sampled value carries the inferred state union, so ordinary pattern
    // narrowing applies (what the removed checked form used to do).
    quiver()
        .evaluate(
            r#"
            'status = Loading | Done['int]
            f = #'status { | =Loading => ^ Done[42] | =Done[x] => x }
            p = Loading ~> @f ~
            !p
            ?p ~> =Done[x]
            x
            "#,
        )
        .expect("42");
}

#[test]
fn test_sample_nilary_process_state_is_nil() {
    // A nilary process's state is nil, and the sample yields it.
    quiver()
        .evaluate(
            r#"
            p = @[] { 42 } [];
            !p;
            [?p]
            "#,
        )
        .expect("[[]]");
}

#[test]
fn test_sample_requires_a_process() {
    quiver()
        .evaluate("x = 5; ?x")
        .expect_error_containing("process");
}

#[test]
fn test_declared_type_without_clause_rejects_sampling() {
    // A declared `@'msg` parameter grants no sampling (the capability must be spelled).
    quiver()
        .evaluate(
            r#"
            f = #@'int { ?$ };
            g = #'int { !#'int };
            7 ~> @g ~ ~> f ~
            "#,
        )
        .expect_error_containing("state type");
}

#[test]
fn test_declared_state_clause_grants_sampling() {
    // A `?'s` clause on a declared process type carries the grant across the boundary:
    // the pid's inferred state ('int) satisfies the declared state covariantly, and the
    // sample inside types as the declared 'int.
    quiver()
        .evaluate(
            r#"
            f = #(@'int ?'int) { ?$ };
            g = #'int { !#'int };
            7 ~> @g ~ ~> f ~
            "#,
        )
        .expect("7");
}

#[test]
fn test_pid_message_is_filtered_by_type_not_just_accepted() {
    // The mailbox filter must discriminate in both directions: a receive typed `'int`
    // leaves a pid in the mailbox, and a later process-typed receive takes it. The pid is
    // sent *first*, so a filter that accepted any message — or rejected every pid — would
    // answer differently or hang.
    quiver()
        .evaluate(
            r#"
            g = #'int { !#'int };
            h = @#[] { ![#'int] ~> =n; ![#(@'int ?'int)] ~> =q; [n, ?q] } [];
            7 ~> @g ~ ~> h ~;
            5 ~> h ~;
            !h
            "#,
        )
        .expect("[5, 7]");
}

#[test]
fn test_message_received_pid_is_sampleable_with_clause() {
    // A pid received in a message typed with a `?'s` clause is bare-sampleable — and the
    // runtime message-compatibility check includes the state component (the relocated
    // runtime test that keeps bare `?p` sound).
    quiver()
        .evaluate(
            r#"
            g = #'int { !#'int };
            h = @#[] { ![#(@'int ?'int)] ~> =q; ?q } [];
            7 ~> @g ~ ~> h ~;
            !h
            "#,
        )
        .expect("7");
}

#[test]
fn test_callable_receive_clause_grants_sending_to_spawn() {
    // A declared callable type sheds the inferred receive (it defaults to never —
    // "grants what it spells"), so without the `!'int` clause the send below would be
    // a compile error ("cannot send to it"). The clause carries the grant.
    quiver()
        .evaluate(
            r#"
            g = #'int { !#'int }
            'w = #'int -> 'int !'int
            run_it = #'w { p = 7 ~> @$ ~; 1 ~> p ~; !p }
            g ~> run_it ~
            "#,
        )
        .expect("1");
}

#[test]
fn test_callable_states_clause_grants_sampling_of_spawn() {
    // The `?` clause on a declared callable type (states = parameter ∪ clause) makes a
    // spawn of the passed function sampleable. `?'int` here adds nothing beyond the
    // parameter, but the grant itself is what a bare `#'int -> 'int` would lack.
    quiver()
        .evaluate(
            r#"
            g = #'int { !#'int }
            'starter = #'int -> 'int ?'int
            probe = #'starter { p = 7 ~> @$ ~; ?p }
            g ~> probe ~
            "#,
        )
        .expect("7");
}

#[test]
fn test_callable_without_states_clause_rejects_sampling() {
    // Without the `?` clause the spawn's state is ungranted.
    quiver()
        .evaluate(
            r#"
            g = #'int { !#'int }
            probe = #(#'int -> 'int) { p = 7 ~> @$ ~; ?p }
            g ~> probe ~
            "#,
        )
        .expect_error_containing("state type");
}

#[test]
fn test_callable_states_clause_rejects_wider_function() {
    // States are covariant against the declaration: a function whose tail-call closure
    // goes beyond the stated `?'int` (here into Str['bin]) does not fit.
    quiver()
        .evaluate(
            r#"
            h = #Str['bin] { 0 }
            g = #'int { ^h "x" }
            take = #(#'int -> 'int ?'int) { [] }
            g ~> take ~
            "#,
        )
        .expect_error_containing("compatible");
}

#[test]
fn test_callable_receive_clause_rejects_mismatched_receiver() {
    // Receive is contravariant: a function receiving 'int does not fit a type whose
    // clause states it receives 'bin.
    quiver()
        .evaluate(
            r#"
            g = #'int { !#'int }
            take = #(#'int -> 'int !'bin) { [] }
            g ~> take ~
            "#,
        )
        .expect_error_containing("compatible");
}
