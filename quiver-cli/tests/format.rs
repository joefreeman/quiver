//! Integration tests for the `quiv format` command's CLI ergonomics: formatting files in place,
//! `--check` exit codes that leave files untouched, and stdout for `--eval`/stdin.

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

fn quiv() -> Command {
    Command::new(env!("CARGO_BIN_EXE_quiv"))
}

/// A uniquely-named temp file seeded with `contents`, removed on drop.
struct TempFile(PathBuf);

impl TempFile {
    fn new(name: &str, contents: &str) -> Self {
        let mut path = std::env::temp_dir();
        path.push(format!("quiv-fmt-{}-{}", std::process::id(), name));
        std::fs::write(&path, contents).unwrap();
        TempFile(path)
    }

    fn read(&self) -> String {
        std::fs::read_to_string(&self.0).unwrap()
    }

    fn path(&self) -> &str {
        self.0.to_str().unwrap()
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

const UNFORMATTED: &str = "#{[1,2]   __integer_add__}\n";
const FORMATTED: &str = "#{ [1, 2] __integer_add__ }\n";

#[test]
fn formats_a_file_in_place() {
    let file = TempFile::new("inplace.qv", UNFORMATTED);
    let status = quiv().args(["format", file.path()]).status().unwrap();
    assert!(status.success());
    assert_eq!(file.read(), FORMATTED);
}

#[test]
fn in_place_leaves_a_formatted_file_unchanged() {
    let file = TempFile::new("noop.qv", FORMATTED);
    let status = quiv().args(["format", file.path()]).status().unwrap();
    assert!(status.success());
    assert_eq!(file.read(), FORMATTED);
}

#[test]
fn check_passes_on_a_formatted_file() {
    let file = TempFile::new("check-ok.qv", FORMATTED);
    let status = quiv()
        .args(["format", "--check", file.path()])
        .status()
        .unwrap();
    assert!(status.success());
}

#[test]
fn check_fails_on_an_unformatted_file_without_writing() {
    let file = TempFile::new("check-bad.qv", UNFORMATTED);
    let status = quiv()
        .args(["format", "--check", file.path()])
        .status()
        .unwrap();
    assert_eq!(status.code(), Some(1));
    assert_eq!(file.read(), UNFORMATTED, "--check must not modify the file");
}

#[test]
fn eval_writes_to_stdout() {
    let out = quiv()
        .args(["format", "-e", "#{[1,2] __integer_add__}"])
        .output()
        .unwrap();
    assert!(out.status.success());
    assert_eq!(String::from_utf8(out.stdout).unwrap(), FORMATTED);
}

#[test]
fn stdin_writes_to_stdout() {
    let mut child = quiv()
        .arg("format")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(UNFORMATTED.as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success());
    assert_eq!(String::from_utf8(out.stdout).unwrap(), FORMATTED);
}

#[test]
fn an_unparseable_input_exits_nonzero() {
    let out = quiv()
        .args(["format", "-e", "{ unterminated"])
        .output()
        .unwrap();
    assert!(!out.status.success());
}
