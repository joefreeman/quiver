//! Integration tests for `quiv test`, whose job is to answer one question honestly: did the
//! document's `//= P` assertions still hold when it ran?
//!
//! The trap these guard is a check that never executed being reported as one that passed. An
//! assertion is a runtime check, so it establishes nothing until control reaches it — which it
//! may not, in an untaken branch, after a failed step, or in a function nothing calls. So passes
//! are counted as they happen, and the gap to what the document writes is reported as not run.

use std::io::Write;
use std::process::Command;

/// A Markdown document holding one `quiver` block, written to a temporary file.
struct Document {
    path: std::path::PathBuf,
}

impl Document {
    fn new(name: &str, fence: &str, body: &str) -> Document {
        let path = std::env::temp_dir().join(format!("quiv-test-{}-{name}.md", std::process::id()));
        let mut file = std::fs::File::create(&path).unwrap();
        write!(file, "# Doc\n\n```{fence}\n{body}\n```\n").unwrap();
        Document { path }
    }

    /// `quiv test` over the document: its exit status, and its output.
    fn run(&self) -> (bool, String) {
        let out = Command::new(env!("CARGO_BIN_EXE_quiv"))
            .arg("test")
            .arg(&self.path)
            .output()
            .unwrap();
        let mut text = String::from_utf8(out.stdout).unwrap();
        text.push_str(&String::from_utf8(out.stderr).unwrap());
        (out.status.success(), text)
    }
}

impl Drop for Document {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

#[test]
fn a_step_assertion_is_counted_and_checked() {
    let (ok, output) = Document::new("step", "quiver", "1 ~> %num.add [~, 2] //= 3").run();
    assert!(ok, "{output}");
    assert!(output.contains("1/1 assertions"), "{output}");

    let (ok, output) = Document::new("step-bad", "quiver", "1 ~> %num.add [~, 2] //= 4").run();
    assert!(!ok, "{output}");
    assert!(output.contains("0/1 assertions"), "{output}");
}

#[test]
fn an_assertion_in_an_uncalled_function_is_not_reported_as_passing() {
    // Nothing calls `f`, so its check never runs. Counting it as passed would report a green
    // document that established nothing.
    let (ok, output) = Document::new("uncalled", "quiver", "f = #'int {\n  $ //= 999\n}\nOk").run();
    assert!(ok, "{output}");
    assert!(output.contains("0/1 assertions"), "{output}");
    assert!(output.contains("1 not run"), "{output}");
}

#[test]
fn an_assertion_on_a_path_not_taken_is_not_reported_as_passing() {
    // An untaken branch, and a step after a failed one: neither check runs.
    let body = "5 ~> {\n  | =0 => 1 //= 999\n  | 2\n} //= 2\n{\n  5 ~> =6\n  7 //= 999\n} //= []";
    let (ok, output) = Document::new("untaken", "quiver", body).run();
    assert!(ok, "{output}");
    assert!(output.contains("2/4 assertions"), "{output}");
    assert!(output.contains("2 not run"), "{output}");
}

#[test]
fn an_assertion_counts_once_however_often_it_runs() {
    let body = "f = #'int {\n  $ //= ('int)\n}\nf 1\nf 2";
    let (ok, output) = Document::new("repeated", "quiver", body).run();
    assert!(ok, "{output}");
    assert!(output.contains("1/1 assertions"), "{output}");
    assert!(!output.contains("not run"), "{output}");
}

#[test]
fn an_assertion_in_a_called_function_fails_the_run() {
    // The same check, now reached: it aborts the calling step, which is reported as that step's
    // failure and names the line the check is written on.
    let (ok, output) = Document::new("called", "quiver", "f = #'int {\n  $ //= 999\n}\nf 1").run();
    assert!(!ok, "{output}");
    assert!(output.contains("expected  999"), "{output}");
    assert!(
        output.contains("inside a function this step called"),
        "{output}"
    );
    // A failing document is never coloured or summarised as if it had passed.
    assert!(!output.contains("1/1 assertions"), "{output}");
}

#[test]
fn a_program_block_checks_its_entry_point() {
    // A `quiver program` block's last step is the entry function, and the runner calls it — so
    // an assertion in that function's body runs.
    let body = "#[] {\n  %num.add [1, 2] //= 3\n}";
    let (ok, output) = Document::new("entry", "quiver program", body).run();
    assert!(ok, "{output}");
    assert!(output.contains("1/1 assertions"), "{output}");
    assert!(!output.contains("not run"), "{output}");

    let body = "#[] {\n  %num.add [1, 2] //= 4\n}";
    let (ok, output) = Document::new("entry-bad", "quiver program", body).run();
    assert!(!ok, "{output}");
    assert!(output.contains("0/1 assertions"), "{output}");
    // Blamed on the assertion's own line (the 5th of the document), not the block's first.
    assert!(output.contains(":5"), "{output}");
}
