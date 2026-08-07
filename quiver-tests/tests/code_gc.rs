//! Code reclamation: a reclamation round's code phase stubs functions and constants
//! nothing live references (keeping identity — pids' root-function indices still type-
//! test), and an identical re-registration *revives* a stubbed slot in place, so the
//! REPL's whole-program merges stay correct across sweeps. A stub that is nonetheless
//! executed panics the process loudly ("reclaimed code invoked"), so every passing
//! evaluation after a sweep is itself evidence the liveness walk was right.

mod common;
use common::quiver;
use std::collections::HashMap;

#[test]
fn dead_code_is_reclaimed() {
    // The closure is rebound away, so its function (and the first line's wrapper)
    // are unreachable from any heap; the sweep must find them.
    quiver()
        .evaluate("f = #'int { [$, 3] ~> __integer_add__ }")
        .then_evaluate("f = 5")
        .force_code_collection()
        .expect_code_reclaimed_at_least(1)
        .then_evaluate("f")
        .expect("5");
}

#[test]
fn live_closure_survives_a_sweep() {
    // The closure sits in the session process's locals: the root walk must keep its
    // function, and calling it afterwards proves the code is intact, not a stub.
    quiver()
        .evaluate("f = #'int { [$, 3] ~> __integer_add__ }")
        .force_code_collection()
        .then_evaluate("f 4")
        .expect("7");
}

#[test]
fn identical_code_revives_its_reclaimed_slot() {
    // Line 1's function is reclaimed after the rebind; defining an identical function
    // afterwards re-registers the same content, which must revive the stubbed slot
    // (the REPL re-merges its whole program every line, so a fresh slot per sweep
    // would churn forever) — and the revived slot must actually run.
    quiver()
        .evaluate("f = #'int { [$, 3] ~> __integer_add__ }")
        .then_evaluate("f = 5")
        .force_code_collection()
        .expect_code_reclaimed_at_least(1)
        .then_evaluate("g = #'int { [$, 3] ~> __integer_add__ }")
        .then_evaluate("g 4")
        .expect("7");
}

#[test]
fn tombstone_result_on_another_worker_keeps_code_alive() {
    // The closure's only surviving reference after the rebind is the result retained
    // by a completed process's tombstone (placed on a worker by pid round-robin).
    // Awaiting after the sweep hands the closure back and calls it — a missed
    // cross-worker root would surface as "reclaimed code invoked".
    quiver()
        .evaluate("f = #'int { [$, 7] ~> __integer_add__ }; p = @#{ &f }; Ok")
        .then_evaluate("f = 5")
        .force_code_collection()
        .then_evaluate("!p ~> =(#'int -> 'int)h; h 3")
        .expect("10");
}

#[test]
fn abandoned_module_code_is_reclaimed_and_revives() {
    // After the first line completes nothing references the module's code (its value
    // lives compiler-side), so the sweep reclaims it; the next use re-merges identical
    // content, reviving the slots.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        "[f: #'int { [$, 11] ~> __integer_add__ }]".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("%m.f 1")
        .expect("12")
        .force_code_collection()
        .expect_code_reclaimed_at_least(1)
        .then_evaluate("%m.f 2")
        .expect("13");
}

#[test]
fn growth_triggers_a_sweep_untouched_by_hand() {
    // With a tiny growth threshold, the round (and its code phase) fires from `step()`
    // alone; the rebound-away closure must be gone without any forced collection.
    quiver()
        .with_code_collection_threshold(4)
        .evaluate("f = #'int { [$, 3] ~> __integer_add__ }")
        .then_evaluate("f = 5")
        .then_evaluate("g = 6")
        .then_evaluate("[f, g] ~> __integer_add__")
        .expect("11")
        .expect_code_reclaimed_at_least(1);
}

#[test]
fn process_only_rounds_leave_code_alone() {
    // An ordinary reclamation round (no code phase requested, growth threshold not
    // reached) must not touch code.
    quiver()
        .evaluate("f = #'int { [$, 3] ~> __integer_add__ }")
        .then_evaluate("f = 5")
        .force_collection()
        .expect_no_code_reclaimed()
        .then_evaluate("f")
        .expect("5");
}
