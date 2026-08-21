//! Compile-time evaluation is bounded: a step budget turns an infinite loop at a
//! module's top level into a compile error instead of a hang, and a host-set
//! cancellation flag abandons an evaluation in flight. Both surface through the
//! module-execution error, so a runaway import can never wedge a compiling session.

use crate::common::quiver;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

fn module(source: &str) -> HashMap<Vec<String>, String> {
    let mut modules = HashMap::new();
    modules.insert(vec!["m".to_string()], source.to_string());
    modules
}

#[test]
fn module_top_level_loop_exhausts_fuel() {
    quiver()
        .with_modules(module("spin = #[] { ^ [] }\nspin []\n[x: 1]"))
        .with_compile_fuel(200_000)
        .evaluate("%m.x")
        .expect_error_containing("step budget");
}

#[test]
fn cancelled_compile_reports_cancellation() {
    let cancel = Arc::new(AtomicBool::new(true));
    quiver()
        .with_modules(module("[x: 1]"))
        .with_compile_cancel(cancel)
        .evaluate("%m.x")
        .expect_error_containing("cancelled");
}

#[test]
fn default_budget_leaves_ordinary_modules_untouched() {
    quiver()
        .with_modules(module("[x: 41]"))
        .evaluate("%m.x")
        .expect("41");
}
