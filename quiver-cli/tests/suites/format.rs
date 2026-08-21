//! Integration tests for the `quiv format` command's CLI ergonomics: formatting files and
//! directory trees in place, `--check` exit codes that leave files untouched, and stdout for
//! `--eval`/stdin.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};

fn quiv() -> Command {
    Command::new(env!("CARGO_BIN_EXE_quiv"))
}

/// Files that are not formatted as they stand; a tool error is 2.
const DRIFT: i32 = 1;
const ERROR: i32 = 2;

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

/// A uniquely-named temp directory, removed with its contents on drop.
struct TempDir(PathBuf);

impl TempDir {
    fn new(name: &str) -> Self {
        // Suites share a process, so the pid alone would collide between tests.
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let mut path = std::env::temp_dir();
        path.push(format!(
            "quiv-fmt-{}-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed),
            name
        ));
        let _ = std::fs::remove_dir_all(&path);
        std::fs::create_dir_all(&path).unwrap();
        TempDir(path)
    }

    /// Write `contents` to `relative`, creating any directories it names.
    fn write(&self, relative: &str, contents: &str) -> PathBuf {
        let path = self.0.join(relative);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, contents).unwrap();
        path
    }

    fn read(&self, relative: &str) -> String {
        std::fs::read_to_string(self.0.join(relative)).unwrap()
    }

    fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

const UNFORMATTED: &str = "#[] {[1,2]   ~>  __integer_add__}\n";
const FORMATTED: &str = "#[] { [1, 2] ~> __integer_add__ }\n";

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
    assert_eq!(status.code(), Some(DRIFT));
    assert_eq!(file.read(), UNFORMATTED, "--check must not modify the file");
}

#[test]
fn formats_a_directory_tree() {
    let dir = TempDir::new("tree");
    dir.write("a.qv", UNFORMATTED);
    dir.write("src/b.qv", UNFORMATTED);
    dir.write("src/nested/c.qv", UNFORMATTED);
    dir.write("notes.md", "not Quiver at all\n");

    let status = quiv()
        .args(["format".as_ref(), dir.path().as_os_str()])
        .status()
        .unwrap();

    assert!(status.success());
    assert_eq!(dir.read("a.qv"), FORMATTED);
    assert_eq!(dir.read("src/b.qv"), FORMATTED);
    assert_eq!(dir.read("src/nested/c.qv"), FORMATTED);
    assert_eq!(
        dir.read("notes.md"),
        "not Quiver at all\n",
        "the walk takes .qv files only"
    );
}

#[test]
fn with_no_arguments_formats_the_current_directory() {
    let dir = TempDir::new("cwd");
    dir.write("a.qv", UNFORMATTED);

    let status = quiv()
        .arg("format")
        .current_dir(dir.path())
        .status()
        .unwrap();

    assert!(status.success());
    assert_eq!(dir.read("a.qv"), FORMATTED);
}

#[test]
fn a_walk_skips_hidden_and_ignored_files() {
    let dir = TempDir::new("ignored");
    dir.write("a.qv", UNFORMATTED);
    dir.write(".hidden/h.qv", UNFORMATTED);
    dir.write("vendor/v.qv", UNFORMATTED);
    dir.write(".gitignore", "/vendor\n");
    // `.gitignore` rules apply within a repository; the marker is all the walk looks for.
    std::fs::create_dir_all(dir.path().join(".git")).unwrap();

    let status = quiv()
        .args(["format".as_ref(), dir.path().as_os_str()])
        .status()
        .unwrap();

    assert!(status.success());
    assert_eq!(dir.read("a.qv"), FORMATTED);
    assert_eq!(dir.read(".hidden/h.qv"), UNFORMATTED);
    assert_eq!(dir.read("vendor/v.qv"), UNFORMATTED);
}

#[test]
fn an_explicitly_named_file_is_formatted_though_a_walk_would_skip_it() {
    let dir = TempDir::new("explicit");
    let ignored = dir.write("vendor/v.qv", UNFORMATTED);
    dir.write(".gitignore", "/vendor\n");
    std::fs::create_dir_all(dir.path().join(".git")).unwrap();

    let status = quiv()
        .args(["format".as_ref(), ignored.as_os_str()])
        .status()
        .unwrap();

    assert!(status.success());
    assert_eq!(dir.read("vendor/v.qv"), FORMATTED);
}

#[test]
fn check_lists_every_file_that_would_change() {
    let dir = TempDir::new("check-tree");
    dir.write("a.qv", UNFORMATTED);
    dir.write("src/b.qv", UNFORMATTED);
    dir.write("src/ok.qv", FORMATTED);

    let out = quiv()
        .args([
            "format".as_ref(),
            "--check".as_ref(),
            dir.path().as_os_str(),
        ])
        .output()
        .unwrap();

    assert_eq!(out.status.code(), Some(DRIFT));
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("a.qv"), "{stdout}");
    assert!(stdout.contains("b.qv"), "{stdout}");
    assert!(!stdout.contains("ok.qv"), "{stdout}");
    assert!(stdout.contains("2 files would be reformatted"), "{stdout}");
    assert_eq!(dir.read("a.qv"), UNFORMATTED, "--check must not write");
}

#[test]
fn a_repeated_path_is_visited_once() {
    let dir = TempDir::new("dedupe");
    let file = dir.write("a.qv", UNFORMATTED);

    let out = quiv()
        .args([
            "format".as_ref(),
            "--check".as_ref(),
            file.as_os_str(),
            file.as_os_str(),
            dir.path().as_os_str(),
        ])
        .output()
        .unwrap();

    assert_eq!(out.status.code(), Some(DRIFT));
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("1 file would be reformatted"), "{stdout}");
}

#[test]
fn an_unparseable_file_does_not_stop_the_walk() {
    let dir = TempDir::new("broken");
    dir.write("broken.qv", "{ unterminated\n");
    dir.write("good.qv", UNFORMATTED);

    let out = quiv()
        .args(["format".as_ref(), dir.path().as_os_str()])
        .output()
        .unwrap();

    assert_eq!(out.status.code(), Some(ERROR));
    assert_eq!(dir.read("good.qv"), FORMATTED);
    assert_eq!(dir.read("broken.qv"), "{ unterminated\n");
    let stderr = String::from_utf8(out.stderr).unwrap();
    assert!(
        stderr.contains("broken.qv"),
        "the error names the file: {stderr}"
    );
}

#[test]
fn a_missing_path_is_an_error() {
    let dir = TempDir::new("missing");
    let out = quiv()
        .args(["format".as_ref(), dir.path().join("nope.qv").as_os_str()])
        .output()
        .unwrap();

    assert_eq!(out.status.code(), Some(ERROR));
    let stderr = String::from_utf8(out.stderr).unwrap();
    assert!(stderr.contains("nope.qv"), "{stderr}");
}

#[test]
fn eval_writes_to_stdout() {
    let out = quiv()
        .args(["format", "-e", "#[] {[1,2] ~> __integer_add__}"])
        .output()
        .unwrap();
    assert!(out.status.success());
    assert_eq!(String::from_utf8(out.stdout).unwrap(), FORMATTED);
}

#[test]
fn stdin_writes_to_stdout() {
    let mut child = quiv()
        .args(["format", "-"])
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
    assert_eq!(out.status.code(), Some(ERROR));
}

#[test]
fn stdin_cannot_be_combined_with_paths() {
    let file = TempFile::new("with-stdin.qv", UNFORMATTED);
    let out = quiv()
        .args(["format", "-", file.path()])
        .stdin(Stdio::null())
        .output()
        .unwrap();

    assert_eq!(out.status.code(), Some(ERROR));
    assert_eq!(file.read(), UNFORMATTED);
}
