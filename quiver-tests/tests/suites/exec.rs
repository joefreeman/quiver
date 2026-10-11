use crate::common::*;
use std::time::Duration;

// `%exec`: operating-system programs as children, against real programs (`sh`, `cat`,
// `sleep`, `env`) — the spawn, the pipes and the exit are what is under test, and only the
// real thing exercises them.

fn exec() -> TestBuilder {
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(Duration::from_secs(10))
}

#[test]
fn test_output_and_exit_status() {
    exec()
        .evaluate(
            r#"
            %exec.spawn ["sh", %list{ "-c", "echo out; echo err >&2; exit 3" }]
            ~> =(child: c, stdout: o, stderr: e)
            %io.read_both [o, e] ~> =[out, err]
            ![c] ~> =Exited[source: _, code: n]
            [Str[out], Str[err], n]
            "#,
        )
        .expect(r#"["out\n", "err\n", 3]"#);
}

#[test]
fn test_stdin_reaches_the_program_and_closing_it_ends_its_input() {
    exec()
        .evaluate(
            r#"
            %exec.spawn ["cat"] ~> =(child: c, stdin: i, stdout: o)
            %io.write [i, "hello" ~> .0]
            %io.close i
            %io.read_all o ~> =('bin & out)
            ![c] ~> =Exited[source: _, code: 0]
            Str[out]
            "#,
        )
        .expect(r#""hello""#);
}

#[test]
fn test_a_signalled_child_ends_with_its_signal() {
    exec()
        .evaluate(
            r#"
            %exec.spawn ["sleep", %list{ "30" }] ~> =(child: c)
            %exec.kill [c, Term]
            ![c, 5000] ~> =Signalled[source: _, signal: s]
            s
            "#,
        )
        .expect("15");
}

#[test]
fn test_an_ended_child_answers_its_end_again() {
    exec()
        .evaluate(
            r#"
            %exec.spawn ["sh", %list{ "-c", "exit 7" }] ~> =(child: c)
            ![c] ~> =Exited[source: _, code: a]
            ![c] ~> =Exited[source: _, code: b]
            [a, b]
            "#,
        )
        .expect("[7, 7]");
}

#[test]
fn test_a_missing_program_fails_with_not_found() {
    exec()
        .evaluate(
            r#"
            %exec.spawn ["quiver-no-such-program"] ~> :error<'%io> ~> =IoError(kind: NotFound)
            Missing
            "#,
        )
        .expect("Missing");
}

#[test]
fn test_cwd_sets_the_working_directory() {
    exec()
        .evaluate(
            r#"
            %exec.spawn ["pwd", cwd: %path.parse "/"] ~> =(stdout: o)
            %io.read_all o ~> Str[~]
            "#,
        )
        .expect(r#""/\n""#);
}

#[test]
fn test_env_changes_apply_over_the_inherited_environment() {
    exec()
        .evaluate(
            r#"
            %exec.spawn [
              "sh",
              %list{ "-c", "echo \"$QUIVER_A-$\{HOME:-unset}\"" },
              env: %list{ Set["QUIVER_A", "set"], Unset["HOME"] },
            ]
            ~> =(stdout: o)
            %io.read_all o ~> Str[~]
            "#,
        )
        .expect(r#""set-unset\n""#);
}

#[test]
fn test_a_cleared_environment_holds_only_what_is_set() {
    exec()
        .evaluate(
            r#"
            %exec.spawn ["/usr/bin/env", env: %list{ Set["ONLY", "this"] }, clear_env: Ok]
            ~> =(stdout: o)
            %io.read_all o ~> Str[~]
            "#,
        )
        .expect(r#""ONLY=this\n""#);
}

#[test]
fn test_a_nul_in_an_argument_is_rejected() {
    exec()
        .evaluate(r#"%exec.spawn ["echo", %list{ Str[<6100>] }]"#)
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "an argument or environment entry cannot contain a NUL byte".to_string(),
        ));
}

#[test]
fn test_writing_to_a_finished_child_fails() {
    // The reader is gone, so the write is a broken pipe: a failure the caller can see, not a
    // signal that kills the host. Retried briefly: a child another thread is forking at that
    // moment holds a copy of every descriptor until its exec closes them, the read end
    // included, so the pipe can outlive its reader by that window.
    exec()
        .evaluate(
            r#"
            %exec.spawn ["true"] ~> =(child: c, stdin: i)
            ![c] ~> =Exited[source: _, code: 0]
            attempt = #'int {
              | { %io.write [i, <01>] => [] | Ok } => Failed
              | =0 => StillWritable
              | { ![20] | Ok }; n = %num.sub [$, 1]; ^ n
            }
            attempt 50
            "#,
        )
        .expect("Failed");
}

#[test]
fn test_closing_a_child_kills_it() {
    // The child's stdout ends when the child does, which is how the test sees the kill.
    exec()
        .evaluate(
            r#"
            %exec.spawn ["sleep", %list{ "30" }] ~> =(child: c, stdout: o)
            %io.close c
            ![o, 5000] ~> =Closed[source: _]
            Gone
            "#,
        )
        .expect("Gone");
}

#[test]
fn test_pipes_move_to_another_process() {
    exec()
        .evaluate(
            r#"
            %exec.spawn ["sh", %list{ "-c", "echo moved" }] ~> =(child: c, stdout: o)
            reader = @'%io.readable { %io.read_all $ } o
            !reader ~> =('bin & out)
            Str[out]
            "#,
        )
        .expect(r#""moved\n""#);
}

#[test]
fn test_a_child_and_what_it_started_die_with_their_owner() {
    // The owner reads the pids of the child and of a grandchild it started in the background,
    // then ends: teardown must kill the whole group, which the test sees as both pids gone
    // from /proc (polled, since the reap is asynchronous).
    exec()
        .evaluate(
            r#"
            owner = @[] {
              %exec.spawn ["sh", %list{ "-c", "sleep 30 & echo $$ $!; exec sleep 30" }]
              ~> =(stdout: o)
              ![o] ~> =Data[source: _, data: d]
              Str[d]
            } []
            !owner ~> =('%str & pids)
            gone? = #'%str { %fs.stat [%path.parse "/proc/{$}"] ~> { ='%fs.stat => [] | Ok } }
            wait_gone = #[(pid): '%str, (tries): 'int] {
              | gone? $pid => Ok
              | $tries ~> =0 => []
              | { ![50] | Ok }; ^ [$pid, %num.sub [$tries, 1]]
            }
            %str.trim pids ~> %str.split [~, " "] ~> %list.collect
            ~> =Cons[child, Cons[grandchild, Nil]]
            [wait_gone [child, 100], wait_gone [grandchild, 100]]
            "#,
        )
        .expect("[Ok, Ok]");
}

#[test]
fn test_reading_both_outputs_cannot_deadlock() {
    // The program fills stderr's pipe before writing anything to stdout. Reading stdout to
    // its end first would wait forever on a program waiting for stderr to be drained.
    exec()
        .evaluate(
            r#"
            %exec.spawn ["sh", %list{ "-c", "head -c 200000 /dev/zero >&2; echo done" }]
            ~> =(stdout: o, stderr: e)
            %io.read_both [o, e] ~> =[out, err]
            [Str[out], %bin.length err]
            "#,
        )
        .expect(r#"["done\n", 200000]"#);
}

#[test]
fn test_run_answers_the_output_of_a_successful_program() {
    exec()
        .evaluate(r#"%exec.run ["sh", %list{ "-c", "echo out; echo err >&2" }] ~> =(stdout: o, stderr: e); [Str[o], Str[e]]"#)
        .expect(r#"["out\n", "err\n"]"#);
}

#[test]
fn test_run_feeds_input_while_reading_output() {
    // More than a pipe holds in both directions: `cat` blocks writing its output unless it
    // is read while its input is still being written.
    exec()
        .evaluate(r#"%exec.run ["cat", stdin: %bin.new 300000] ~> =(stdout: o); %bin.length o"#)
        .expect("300000");
}

#[test]
fn test_run_fails_on_a_non_zero_exit() {
    exec()
        .evaluate(
            r#"%exec.run ["sh", %list{ "-c", "echo why >&2; exit 4" }] ~> :error<'%exec.failure>"#,
        )
        .expect("Failed[code: 4, stdout: <>, stderr: <7768790a>]");
}

#[test]
fn test_run_fails_on_a_signal() {
    exec()
        .evaluate(r#"%exec.run ["sh", %list{ "-c", "kill -9 $$" }] ~> :error<'%exec.failure>"#)
        .expect("Killed[signal: 9, stdout: <>, stderr: <>]");
}

#[test]
fn test_run_fails_when_the_program_cannot_start() {
    exec()
        .evaluate(r#"%exec.run ["quiver-no-such-program"] ~> :error<'%io> ~> =IoError(kind: NotFound); Missing"#)
        .expect("Missing");
}
