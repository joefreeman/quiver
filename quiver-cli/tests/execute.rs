//! Integration tests for the program top level, which executes at boot in the root
//! process (the entry runs the top-level sequence, then calls the function it evaluates
//! to). Compilation never executes user code, so top-level process work, host reads, and
//! ref minting behave exactly as they do in any function body. Modules keep their
//! compile-time-evaluation restrictions — these tests cover the entry program only.

use std::process::Command;

fn quiv() -> Command {
    Command::new(env!("CARGO_BIN_EXE_quiv"))
}

/// Assert `program` runs successfully and prints `expected`.
fn expect_output(program: &str, expected: &str) {
    let out = quiv().args(["run", "-e", program]).output().unwrap();
    let stderr = String::from_utf8(out.stderr).unwrap();
    assert!(
        out.status.success(),
        "expected success for: {program}, stderr: {stderr}"
    );
    assert_eq!(String::from_utf8(out.stdout).unwrap().trim(), expected);
}

#[test]
fn top_level_process_work_runs_at_boot() {
    // The top level runs in the root process with the real runtime, so process work
    // there is ordinary: a spawned child is owned by the root process and torn down
    // with it, and the root's own mailbox services top-level sends and receives.
    // (A receive no sender can ever satisfy blocks forever — exactly as it would
    // inside the entry function.)
    expect_output("p = @#{ !'int }; #{ 5 }", "5");
    expect_output("42 ~> .; !'int; #{ 5 }", "5");
    expect_output("p = @#{ 42 }; r = !p; #{ r }", "42");
}

#[test]
fn compile_falls_back_without_an_entry_for_top_level_process_work() {
    // `quiv compile` permits programs that don't evaluate to a function (checked
    // statically); they take the no-entry fallback and still emit bytecode.
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
fn top_level_host_reads_run_at_boot() {
    // Host reads at the top level happen per program run, at boot — nothing is baked
    // into the emitted bytecode, so determinism of compilation is preserved.
    expect_output("t = %time.now; #{ t ~> { ='int => 1 | 2 } }", "1");
    expect_output("r = %random.bytes 8; #{ %bin.length r }", "8");
}

#[test]
fn top_level_ref_minting_runs_at_boot() {
    // Identity-freedom constrains *modules* (shared across importers); the program's
    // top level runs at boot, so a top-level ref is minted fresh each run.
    expect_output("a = %ref; #{ [a, 1] ~> =[&a, x]; x }", "1");

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
