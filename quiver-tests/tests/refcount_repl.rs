//! Exercises the REPL's orphaned-local reclaim against the worker's debug `check_refcounts` at
//! process completion. Rebinding a variable that held a *non-constant* heap binary leaves the old
//! local orphaned; the worker releases it when it delivers the rebinding line's result (driven by
//! the keep-set handed to `request_result`). If the local were dropped without releasing, the
//! completion check would panic ("reachable=false"), failing this test.

mod common;
use common::*;

#[test]
fn compaction_releases_orphaned_binary_local() {
    quiver()
        // x holds a heap binary built by a builtin (not a cache-pinned constant).
        .evaluate("x = __binary_concat__ [0xaa, 0xbb]")
        // Rebind x: the old binary local is now orphaned; the worker drops (and must release) it
        // when delivering this line's result.
        .then_evaluate("x = 5")
        .then_evaluate("x")
        .expect("5");
}
