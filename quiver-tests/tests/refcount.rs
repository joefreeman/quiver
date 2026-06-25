//! End-to-end validation of the binary-heap reference counting wired into the interpreter.
//!
//! Each program is compiled and run synchronously (so the returned `Executor` exposes its heap),
//! then `check_refcounts` verifies the invariant `refcount[i] > 0  <=>  slot i is reachable`.
//! `execute_bytecode_sync` also asserts this internally in debug builds; these tests additionally
//! confirm the heap is *non-trivial* (binaries actually allocated and, in the second case, one
//! genuinely discarded), so a regression that drops a `retain`/`release` is caught.

use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{Compiler, PackageResolver, parse};
use quiver_core::bytecode::Function;
use quiver_core::executor::Executor;
use quiver_core::program::Program;
use quiver_core::types::{self, Type};
use quiver_io::NativeEffect;
use std::collections::HashMap;

fn builtins() -> quiver_core::builtins::BuiltinRegistry<NativeEffect> {
    let mut registry = quiver_core::builtins::BuiltinRegistry::with_modules(
        &quiver_core::builtins::core_modules(),
    );
    quiver_io::attach_network_builtins(&mut registry);
    quiver_io::attach_file_builtins(&mut registry);
    registry
}

/// Compile and sync-run `source`, returning the resulting executor for heap inspection.
fn run(source: &str) -> Executor<NativeEffect> {
    let ast = parse(source).expect("parse");
    let resolver = PackageResolver::inline();
    let builtins = builtins();
    let mut program = Program::new();
    let mut module_cache = ModuleCache::new();

    let compiled = Compiler::compile(
        ast,
        &HashMap::new(),
        &mut module_cache,
        &resolver,
        &mut program,
        types::NIL,
        &HashMap::new(),
        &builtins,
        None,
    )
    .expect("compile");

    let nil_type = program.register_type(Type::nil());
    let callable = program.register_type(Type::Callable {
        parameter: nil_type,
        result: compiled.result_type,
        receive: compiled.receive_type,
    });
    let entry = program.register_function(Function {
        instructions: compiled.instructions,
        captures: 0,
        type_id: callable,
    });
    let bytecode = program.to_bytecode(Some(entry));

    let (_value, executor) =
        quiver_core::execute_bytecode_sync(bytecode, &builtins, false).expect("execute");
    executor
}

#[test]
fn returned_binaries_stay_counted() {
    // Two heap binaries built via a builtin (not constants) and kept in the result tuple.
    let ex = run("[__binary_concat__ [0x0a1b, 0x2c3d], __binary_concat__ [0x01, 0x02]]");
    assert!(ex.check_refcounts().is_ok(), "{:?}", ex.check_refcounts());
    // Non-trivial: the two concatenations are reachable, so this isn't a vacuous pass.
    assert!(
        ex.heap_stats().reachable >= 2,
        "expected reachable heap binaries, got {:?}",
        ex.heap_stats()
    );
}

#[test]
fn discarded_binary_becomes_unreachable() {
    // `c` is built then never used in the result; once its top-level local is released on
    // completion it must end at refcount 0 (a dead, reclaimable slot), exercising the release side.
    let ex = run("c = __binary_concat__ [0x09, 0x09], __binary_concat__ [0xaa, 0xbb]");
    assert!(ex.check_refcounts().is_ok(), "{:?}", ex.check_refcounts());
    assert!(
        ex.heap_stats().dead() >= 1,
        "expected a dead (reclaimable) slot for the discarded binary, got {:?}",
        ex.heap_stats()
    );
}

#[test]
fn nested_tuples_and_field_access_balance() {
    // Build a nested structure, then project a field out of it — exercises tuple construct
    // (deep retain) and `get` (release the tuple, retain the field).
    let ex = run("p = [a: __binary_concat__ [0x01, 0x02], b: 0x03], p.a");
    assert!(ex.check_refcounts().is_ok(), "{:?}", ex.check_refcounts());
}

#[test]
fn reclamation_bounds_heap_growth() {
    // A tail-recursive loop allocates and immediately discards a heap binary each iteration.
    // With reclamation the discarded slots are reused, so the heap stays far below the iteration
    // count (without it, each iteration would strand a slot, growing the heap to ~2000).
    let ex = run(
        "count = #'int { | =0 => Ok | =n => { __binary_concat__ [0x01, 0x02], [n, 1] ~> __subtract__ ~> ^ } }, count 2000",
    );
    let stats = ex.heap_stats();
    println!("reclamation loop heap stats: {stats:?}");
    assert!(
        stats.slots < 500,
        "expected reclamation to bound the heap well below 2000, got {stats:?}"
    );
}
