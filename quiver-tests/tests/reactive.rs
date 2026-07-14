// Reactive processes / tracked renders: `%proc.track` runs a nilary
// thunk with dependency tracking on, subscribing the caller to every process the thunk
// samples with `?`. When a sampled process's state changes, the caller is sent a
// `'%proc.changed` wakeup. The default two-worker test environment places the store on a
// different worker than the main process, so these exercise the remote (routed) sample +
// atomic-subscribe path and the cross-worker wakeup delivery.
mod common;
use common::*;
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
            store = 5 ~> @'int { !'int ~> { =n => ^ n } }
            %proc.track #{ ?store }
            "#,
        )
        .expect("5");
}

#[test]
fn wakeup_on_dependency_change() {
    // Track the store's state (subscribing to it), change the store, then receive the
    // `Changed` wakeup and re-sample: the new state is visible.
    quiver()
        .evaluate(
            r#"
            store = 5 ~> @'int { !'int ~> { =n => ^ n } }
            s1 = %proc.track #{ ?store }
            10 ~> store
            w = !#'%proc.changed
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
            store = 5 ~> @'int { !'int ~> { =n => ^ n } }
            %proc.track #{ 1 ~> store; ?store }
            "#,
        )
        .expect_runtime_error(Error::OperationNotAllowed {
            operation: Operation::Send,
            context: RestrictedContext::TrackedRender,
        });
}
