// Process reclamation (garbage collection of terminated processes). These drive the full
// environment (two worker threads), so children round-robin across workers and exercise
// the cross-worker sweep.

mod common;
use common::quiver;

#[test]
fn unheld_tombstone_is_reclaimed() {
    // Spawn a process and await it without ever binding its pid: once the line completes no
    // handle to it survives, so a reclamation round sweeps the tombstone.
    quiver()
        .evaluate("@#[] { 42 } [] ~> ![~]")
        .expect("42")
        .force_collection()
        .expect_reclaimed_at_least(1);
}

#[test]
fn held_pid_keeps_tombstone_and_late_await_still_works() {
    // `p` stays bound in the persistent REPL process (a root), so its tombstone is kept: a
    // later `!p` must still return the original result. This is the guarantee reclamation
    // must preserve — observable as long as a handle exists.
    quiver()
        .evaluate("p = @#[] { 42 } []; !p")
        .expect("42")
        .force_collection()
        .expect_reclaimed(0)
        .then_evaluate("!p")
        .expect("42");
}

#[test]
fn reclaims_many_unheld_tombstones_across_workers() {
    // Twenty children, round-robin across both workers, each awaited then dropped (block
    // scope). Below the auto-trigger threshold, so the forced round sweeps all of them.
    quiver()
        .evaluate(
            "loop = #'int { | =0 => Done | =n => { c = @#[] { Ok } []; !c; %num.sub [n, 1] ~> ^ ~ } }; 20 ~> loop ~",
        )
        .expect("Done")
        .force_collection()
        .expect_reclaimed_at_least(20);
}

#[test]
fn reclamation_bounds_process_population_under_load() {
    // Spawn well past the (lowered) auto-trigger threshold. Auto-collection fires repeatedly
    // during the loop, so the tracked population stays bounded instead of growing to the spawn
    // count. The threshold is lowered so a modest, fast loop still exercises several rounds.
    quiver()
        .with_collection_threshold(40)
        .evaluate(
            "loop = #'int { | =0 => Done | =n => { c = @#[] { Ok } []; !c; %num.sub [n, 1] ~> ^ ~ } }; 150 ~> loop ~",
        )
        .expect("Done")
        .force_collection()
        .expect_reclaimed_at_least(100)
        .expect_process_count_below(30);
}
