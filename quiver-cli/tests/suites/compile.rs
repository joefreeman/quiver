//! Integration tests for how `quiv compile`, `run` and `inspect` take their input and place
//! their output: a file (by extension), `-` for stdin, or `--eval`; compiled `.qv` files
//! land beside their source, and everything else goes to stdout.

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};

fn quiv() -> Command {
    Command::new(env!("CARGO_BIN_EXE_quiv"))
}

fn stdout(out: &Output) -> String {
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8(out.stdout.clone()).unwrap()
}

/// Run `quiv args…` with `input` on stdin.
fn with_stdin(args: &[&str], input: &str) -> Output {
    let mut child = quiv()
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(input.as_bytes())
        .unwrap();
    child.wait_with_output().unwrap()
}

/// A uniquely-named temp directory, removed with its contents on drop.
struct TempDir(PathBuf);

impl TempDir {
    fn new() -> Self {
        // Suites share a process, so the pid alone would collide between tests.
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let mut path = std::env::temp_dir();
        path.push(format!(
            "quiv-compile-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed),
        ));
        let _ = std::fs::remove_dir_all(&path);
        std::fs::create_dir_all(&path).unwrap();
        TempDir(path)
    }

    fn file(&self, name: &str, contents: &str) -> String {
        let path = self.0.join(name);
        std::fs::write(&path, contents).unwrap();
        path.to_str().unwrap().to_string()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[test]
fn compile_writes_the_qx_beside_its_source() {
    let dir = TempDir::new();
    let source = dir.file("prog.qv", "#[] { %num.add [2, 3] }");
    let out = quiv().args(["compile", &source]).output().unwrap();
    assert_eq!(stdout(&out), "", "nothing on stdout when writing a file");

    let compiled = dir.0.join("prog.qx");
    assert!(compiled.exists());
    let out = quiv()
        .args(["run", compiled.to_str().unwrap()])
        .output()
        .unwrap();
    assert_eq!(stdout(&out).trim(), "5");
}

#[test]
fn compile_output_flag_overrides_the_destination() {
    let dir = TempDir::new();
    let source = dir.file("prog.qv", "#[] { 1 }");
    let elsewhere = dir.0.join("other.qx");
    let out = quiv()
        .args(["compile", &source, "-o", elsewhere.to_str().unwrap()])
        .output()
        .unwrap();
    stdout(&out);
    assert!(elsewhere.exists());
    assert!(!dir.0.join("prog.qx").exists());

    let out = quiv()
        .args(["compile", &source, "-o", "-"])
        .output()
        .unwrap();
    assert!(stdout(&out).starts_with('{'));
    assert!(!dir.0.join("prog.qx").exists());
}

#[test]
fn compile_without_a_file_writes_to_stdout() {
    let out = quiv()
        .args(["compile", "-e", "#[] { 1 }"])
        .output()
        .unwrap();
    assert!(stdout(&out).starts_with('{'));

    let out = with_stdin(&["compile", "-"], "#[] { 1 }");
    assert!(stdout(&out).starts_with('{'));
}

#[test]
fn compile_rejects_bytecode() {
    let compiled = stdout(
        &quiv()
            .args(["compile", "-e", "#[] { 1 }"])
            .output()
            .unwrap(),
    );
    let out = with_stdin(&["compile", "-"], &compiled);
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("already compiled"));
}

#[test]
fn inspect_accepts_source_and_bytecode_alike() {
    let dir = TempDir::new();
    let source = dir.file("prog.qv", "#[] { %num.add [2, 3] }");
    let from_source = stdout(&quiv().args(["inspect", &source]).output().unwrap());

    stdout(&quiv().args(["compile", &source]).output().unwrap());
    let qx = dir.0.join("prog.qx");
    let from_bytecode = stdout(
        &quiv()
            .args(["inspect", qx.to_str().unwrap()])
            .output()
            .unwrap(),
    );
    assert_eq!(from_source, from_bytecode);

    let from_eval = stdout(
        &quiv()
            .args(["inspect", "-e", "#[] { 1 }"])
            .output()
            .unwrap(),
    );
    assert!(from_eval.starts_with("Tables:"));
}

#[test]
fn stdin_takes_source_or_bytecode() {
    let compiled = stdout(
        &quiv()
            .args(["compile", "-e", "#[] { 7 }"])
            .output()
            .unwrap(),
    );
    assert_eq!(stdout(&with_stdin(&["run", "-"], &compiled)).trim(), "7");
    assert!(stdout(&with_stdin(&["inspect", "-"], &compiled)).starts_with("Tables:"));
    assert_eq!(stdout(&with_stdin(&["run", "-"], "#[] { 9 }")).trim(), "9");
}

#[test]
fn an_input_is_required() {
    // No implicit stdin: without a file, `-` or `--eval`, the command prints usage.
    for command in ["compile", "run", "inspect"] {
        let out = quiv().arg(command).stdin(Stdio::null()).output().unwrap();
        assert!(!out.status.success(), "{command} ran with no input");
        assert!(String::from_utf8_lossy(&out.stderr).contains("Usage"));
    }
}

#[test]
fn unknown_extensions_are_rejected() {
    let dir = TempDir::new();
    let path = dir.file("prog.txt", "#[] { 1 }");
    let out = quiv().args(["run", &path]).output().unwrap();
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("Unsupported file extension"));
}

#[test]
fn a_compile_error_in_a_module_names_its_file_and_the_import() {
    let dir = TempDir::new();
    dir.file(
        "quiver.toml",
        r#"modules = [{ std = true }, { path = "./src" }]"#,
    );
    std::fs::create_dir(dir.0.join("src")).unwrap();
    dir.file("src/bad.qv", "x = 1\n[f: #[] { %num.add [x, <01>] }]");
    dir.file("main.qv", "#[] {\n  %bad.f []\n}");
    let out = quiv()
        .current_dir(&dir.0)
        .args(["run", "main.qv"])
        .output()
        .unwrap();
    assert!(!out.status.success());
    let stderr = String::from_utf8(out.stderr).unwrap();
    let lines: Vec<&str> = stderr.lines().collect();
    assert!(
        lines[0].starts_with("src/bad.qv:2:11: Type mismatch"),
        "stderr: {stderr}"
    );
    assert_eq!(lines[1], "  imported at main.qv:2:3", "stderr: {stderr}");
}
