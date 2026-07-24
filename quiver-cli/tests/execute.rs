//! Integration tests for compile-time (sync) execution of the top level: process work
//! there cannot be serviced (no action router at compile time) and must fail fast with
//! a pointed error rather than hang.

use std::process::Command;

fn quiv() -> Command {
    Command::new(env!("CARGO_BIN_EXE_quiv"))
}

/// Assert `program` fails under `run` with the pointed compile-time-execution error
/// naming `operation`.
fn expect_rejected(program: &str, operation: &str) {
    let out = quiv().args(["run", "-e", program]).output().unwrap();
    assert!(!out.status.success(), "expected failure for: {program}");
    let stderr = String::from_utf8(out.stderr).unwrap();
    assert!(
        stderr.contains(operation) && stderr.contains("compile-time execution"),
        "expected pointed '{operation}' error for {program}, got: {stderr}"
    );
}

#[test]
fn top_level_process_work_is_rejected_precisely_not_a_hang() {
    // Each un-serviceable routing request is rejected at its source with the operation
    // named. These used to spin forever at 100% CPU (and a top-level send was silently
    // dropped).
    expect_rejected("p = @#{ !'int }; #{ 5 }", "spawning a process");
    expect_rejected("42 ~> .; !'int; #{ 5 }", "sending a message");
    // A receive with no possible sender hits the backstop.
    expect_rejected(
        "!'int; #{ 5 }",
        "waiting to receive a message that can never arrive",
    );
}

#[test]
fn compile_falls_back_without_an_entry_for_top_level_process_work() {
    // `quiv compile` permits programs that don't evaluate to a function; a stalled
    // top-level execution takes that fallback and still emits bytecode.
    let out = quiv()
        .args([
            "compile",
            "-e",
            "f = @{ !'int ~> { | =0 => \"done\" | [] ~> ^ } }",
        ])
        .output()
        .unwrap();
    assert!(out.status.success());
    assert!(!out.stdout.is_empty(), "expected bytecode on stdout");
}

#[test]
fn entry_function_spawns_are_unaffected() {
    // Process work inside the entry function runs in the real runtime environment.
    let out = quiv()
        .args(["run", "-e", "#{ p = @#{ 42 }; ![p, 1000] }"])
        .output()
        .unwrap();
    assert!(out.status.success());
    assert_eq!(String::from_utf8(out.stdout).unwrap().trim(), "42");
}

#[test]
fn top_level_host_reads_are_rejected() {
    // Compile-time execution must be deterministic: clock and entropy reads at the top
    // level would bake the compile instant into the emitted program.
    expect_rejected("t = %time.now; #{ t }", "reading host state");
    expect_rejected("r = %random.bytes 8; #{ r }", "reading host state");
}

#[test]
fn top_level_ref_minting_is_rejected() {
    // A compile-time value must be identity-free so compiled modules can be shared
    // (and one day serialized) across the sessions that import them, so ref minting
    // is rejected alongside host-state reads. Referencing the minting function is
    // fine — the importer mints at runtime.
    expect_rejected("a = %ref; #{ a }", "creating a ref");

    let out = quiv()
        .args([
            "run",
            "-e",
            "mk = &%ref; #{ a = mk; b = mk; a ~> =&b => 1 | 2 }",
        ])
        .output()
        .unwrap();
    assert!(out.status.success(), "expected success");
    assert_eq!(String::from_utf8(out.stdout).unwrap().trim(), "2");
}
