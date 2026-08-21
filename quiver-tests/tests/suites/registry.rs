// The `%registry` name table: rendezvous for processes with no shared ancestor. These
// drive the full environment (two worker threads), so registration watchers, lookups
// and expiry cross worker boundaries.

use crate::common::quiver;

#[test]
fn register_lookup_send_await() {
    quiver()
        .evaluate("p = @#[] { !'int ~> %num.mul [~, 2] } []; %registry.register [Doubler, p]")
        .expect("Ok")
        .then_evaluate("%registry.lookup<@'int> Doubler ~> =(@'int)q; 21 ~> q ~; !p")
        .expect("42");
}

#[test]
fn lookup_of_unbound_key_is_nil() {
    quiver()
        .evaluate("%registry.lookup<@'int> Missing")
        .expect("[]");
}

#[test]
fn keys_are_data_values_compared_structurally() {
    // The key is built twice — different expressions, same value — and a structured
    // tuple key namespaces naturally.
    quiver()
        .evaluate("p = @#[] { !'int } []; %registry.register [Worker[shard: %num.add [1, 2]], p]")
        .expect("Ok")
        .then_evaluate("%registry.lookup<@'int> Worker[shard: 3] ~> =(@'int)")
        .expect("Ok");
}

#[test]
fn register_of_taken_key_is_nil_even_for_the_same_process() {
    quiver()
        .evaluate("p = @#[] { !'int } []; %registry.register [Taken, p]")
        .expect("Ok")
        .then_evaluate("%registry.register [Taken, p]")
        .expect("[]")
        .then_evaluate("q = @#[] { !'int } []; %registry.register [Taken, q]")
        .expect("[]");
}

#[test]
fn lookup_grants_exactly_what_it_spells() {
    // The registered process receives ints, results in int, and its state union is the
    // nil spawn argument. Send is contravariant, result covariant, state strict.
    quiver()
        .evaluate("p = @#[] { !'int ~> %num.mul [~, 2] } []; %registry.register [Typed, p]")
        .expect("Ok")
        .then_evaluate("%registry.lookup<@'bin> Typed")
        .expect("[]")
        .then_evaluate("%registry.lookup<@!'bin> Typed")
        .expect("[]")
        .then_evaluate("%registry.lookup<(@?'int)> Typed")
        .expect("[]")
        .then_evaluate("%registry.lookup<@!'int> Typed ~> =(@!'int)")
        .expect("Ok");
}

#[test]
fn lookup_state_grant_supports_sampling() {
    // A root function whose parameter is 'int carries an int state union, so a lookup
    // stating `?'int` is granted and the sample reads the spawn argument.
    quiver()
        .evaluate("w = 7 ~> @#'int { !'bin; $ } ~; %registry.register [Stateful, w]")
        .expect("Ok")
        .then_evaluate("%registry.lookup<(@?'int)> Stateful ~> =((@?'int))v; ?v")
        .expect("7");
}

#[test]
fn name_frees_at_normal_completion() {
    // Deterministic: the Registered watcher predates the Awaiter, so by the time `!p`
    // answers, the environment has already processed the expiry.
    quiver()
        .evaluate(
            "p = @#[] { !'int ~> %num.mul [~, 2] } []; %registry.register [Fleet, p]; \
             %registry.lookup<@'int> Fleet ~> =(@'int)q; 21 ~> q ~; !p",
        )
        .expect("42")
        .then_evaluate("%registry.lookup<@'int> Fleet")
        .expect("[]");
}

#[test]
fn name_frees_on_kill_and_can_be_reused() {
    quiver()
        .evaluate("p = @#[] { !'int } []; %registry.register [Restart, p]")
        .expect("Ok")
        .then_evaluate("%proc.kill p; !p ~> =[]; %registry.lookup<@'int> Restart")
        .expect("[]")
        .then_evaluate("q = @#[] { !'int } []; %registry.register [Restart, q]")
        .expect("Ok");
}

#[test]
fn cascade_teardown_frees_the_name() {
    // The parent registers its owned child, then the parent is killed: containment
    // teardown kills the child, whose tombstone flush frees the name.
    quiver()
        .evaluate(
            "parent = @#(@Ready) { c = @#[] { !'int } []; %registry.register [Child, c]; \
             Ready ~> $ ~; !'bin } .; \
             !Ready; %registry.lookup<@'int !'int> Child ~> =(@'int !'int)c; \
             %proc.kill parent; !c ~> =[]; %registry.lookup<@'int> Child",
        )
        .expect("[]");
}

#[test]
fn all_names_of_a_process_free_together() {
    quiver()
        .evaluate(
            "p = @#[] { !'int } []; %registry.register [First, p]; %registry.register [Second, p]",
        )
        .expect("Ok")
        .then_evaluate(
            "%proc.kill p; !p ~> =[]; \
             [first: %registry.lookup<@'int> First, second: %registry.lookup<@'int> Second]",
        )
        .expect("[first: [], second: []]");
}

#[test]
fn register_of_a_dead_process_is_nil() {
    quiver()
        .evaluate("p = @#[] { 42 } []; !p")
        .expect("42")
        .then_evaluate("%registry.register [Late, p]")
        .expect("[]");
}

#[test]
fn unregister_removes_and_answers_by_presence() {
    quiver()
        .evaluate("p = @#[] { !'int } []; %registry.register [Gone, p]")
        .expect("Ok")
        .then_evaluate("%registry.unregister Gone")
        .expect("Ok")
        .then_evaluate("%registry.unregister Gone")
        .expect("[]")
        .then_evaluate("%registry.lookup<@'int> Gone")
        .expect("[]");
}

#[test]
fn registered_service_survives_its_spawner_and_collection() {
    // The spawner detaches and registers the worker, then dies: after a collection
    // round the registry is the only holder, and the service still serves.
    quiver()
        .isolated()
        .evaluate(
            "spawner = @#[] { w = @#[] { !'int ~> %num.mul [~, 3] } []; %proc.detach w; \
             %registry.register [Svc, w] } []; !spawner",
        )
        .expect("Ok")
        .force_collection()
        .then_evaluate("%registry.lookup<@'int !'int> Svc ~> =(@'int !'int)q; 14 ~> q ~; !q")
        .expect("42");
}

#[test]
fn identity_bearing_key_is_a_runtime_error() {
    quiver()
        .evaluate("p = @#[] { !'int } []; %registry.register [[tag: %ref []], p]")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "invalid registry key: cannot encode a ref: %data notation carries data only \
             (integers, binaries, and tuples)"
                .to_string(),
        ));
}

#[test]
fn registry_read_is_rejected_in_a_receive_filter() {
    quiver()
        .evaluate(
            "p = @#[] { !'int } []; %registry.register [Filtered, p]; me = .; 42 ~> me ~; \
             !'int { %registry.lookup<@'int> Filtered }",
        )
        .expect_runtime_error(quiver_core::error::Error::OperationNotAllowed {
            operation: quiver_core::error::Operation::Registry,
            context: quiver_core::process::RestrictedContext::ReceiveFunction,
        });
}

#[test]
fn registry_is_rejected_in_module_bodies() {
    let mut modules = std::collections::HashMap::new();
    modules.insert(
        vec!["regmod".to_string()],
        "%registry.unregister Nope; [x: 1]".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("* = %regmod; x")
        .expect_error_containing("a registry operation is not supported in compile-time execution");
}

#[test]
fn lookup_requires_a_type_argument() {
    quiver()
        .evaluate("%registry.lookup Missing")
        .expect_error_containing("registry_lookup");
}

#[test]
fn session_root_registration_survives_line_completion() {
    // A persistent process's per-line completion is a sleep, not a termination: the
    // Registered watcher is exempt from its flush, so the binding holds across lines.
    quiver()
        .evaluate("%registry.register [Root, .]")
        .expect("Ok")
        .then_evaluate("%registry.lookup<@> Root ~> { =[] => Gone | Present }")
        .expect("Present");
}
