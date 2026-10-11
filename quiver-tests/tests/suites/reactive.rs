// Reactive processes / tracked renders: `%proc.track` runs a nilary
// thunk with dependency tracking on, subscribing the caller to every process the thunk
// samples with `?`. When a sampled process's state changes, the caller is sent a
// `'%proc.changed` wakeup. The default two-worker test environment places the store on a
// different worker than the main process, so these exercise the remote (routed) sample +
// atomic-subscribe path and the cross-worker wakeup delivery.
use crate::common::*;
use quiver_core::error::{Error, Operation};
use quiver_core::process::RestrictedContext;

#[test]
fn track_returns_thunk_result() {
    // A render sampling nothing returns its thunk's value; the generic `#(#[] -> 'v) -> 'v`
    // type resolves `'v` to the thunk's result type.
    quiver().evaluate("%proc.track #{ 42 }").expect("42");
}

#[test]
fn track_samples_dependency_state() {
    // Inside the render, `?store` yields the store's current state, which flows out as
    // `track`'s result.
    quiver()
        .evaluate(
            r#"
            store = 5 ~> @'int { !'int ~> { =n => ^ n } } ~
            %proc.track #{ ?store }
            "#,
        )
        .expect("5");
}

#[test]
fn track_resumes_after_nested_and_tail_calls() {
    // The render ends when the thunk's own frame returns: calls inside it return first, and a
    // tail call replaces its frame in place. Tracking is over afterwards, so a later render
    // starts afresh, and a select (refused mid-render) is allowed again.
    quiver()
        .evaluate(
            r#"
            store = 5 ~> @'int { !'int ~> { =n => ^ n } } ~
            read = #[] { ?store }
            double = #'int { %num.mul [$, 2] }
            first = %proc.track #{ read [] ~> double ~> %num.add [~, 1] }
            second = %proc.track #{ double ~> ^~ ?store }
            [first, second, { ![5] }]
            "#,
        )
        .expect("[11, 10, []]");
}

#[test]
fn wakeup_on_dependency_change() {
    // Track the store's state (subscribing to it), change the store, then receive the
    // `Changed` wakeup and re-sample: the new state is visible.
    quiver()
        .evaluate(
            r#"
            store = 5 ~> @'int { !'int ~> { =n => ^ n } } ~
            s1 = %proc.track #{ ?store }
store 10
            w = !'%proc.changed
            s2 = ?store
            [s1, s2]
            "#,
        )
        .expect("[5, 10]");
}

#[test]
fn tracked_render_rejects_effects() {
    // A render must be pure: a send inside the tracked thunk is a restricted-context error.
    quiver()
        .evaluate(
            r#"
            store = 5 ~> @'int { !'int ~> { =n => ^ n } } ~
            %proc.track #{store 1; ?store }
            "#,
        )
        .expect_runtime_error(Error::OperationNotAllowed {
            operation: Operation::Send,
            context: RestrictedContext::TrackedRender,
        });
}

#[test]
fn tracked_render_rejects_host_reads() {
    // A render must be a pure function of captures + samples: a clock read inside one
    // could steer a branch with no subscribed dependency, silently going stale.
    quiver()
        .with_io()
        .evaluate("%proc.track #{ %time.now [] }")
        .expect_runtime_error(Error::OperationNotAllowed {
            operation: Operation::HostRead,
            context: RestrictedContext::TrackedRender,
        });
}

#[test]
fn tracked_render_rejects_ref_creation() {
    // A fresh ref per render is unstable identity (e.g. keys that never match across
    // re-renders); mint in the update step and carry the ref in state instead.
    quiver()
        .evaluate("%proc.track #{ %ref [] }")
        .expect_runtime_error(Error::OperationNotAllowed {
            operation: Operation::CreateRef,
            context: RestrictedContext::TrackedRender,
        });
}
