//! The REPL line lifecycle as three explicit steps — `prepare` (environment prologue),
//! `compile` (no environment access), `commit` (environment epilogue) — and the
//! atomicity it guarantees: a line that fails to compile, or that is compiled but never
//! committed, leaves the session exactly as it was. `evaluate` is the composition of
//! the three, so the rest of the suite exercises it constantly; these tests drive the
//! steps individually. Programs avoid std imports, so no artifact store is attached.

use quiver::spawn_worker;
use quiver_compiler::PackageResolver;
use quiver_environment::{Environment, Repl, RequestResult, WorkerHandle};
use quiver_io::NativeEffect;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::time::{Duration, Instant};

const DEADLINE: Duration = Duration::from_secs(10);

fn session() -> (Environment<NativeEffect>, Repl<NativeEffect>) {
    let virtual_time = Arc::new(AtomicU64::new(0));
    let builtins = quiver_core::builtins::BuiltinRegistry::<NativeEffect>::with_modules(
        &quiver_core::builtins::universal_modules(),
    );
    let (waker, _wake) = quiver::native_transport::wake_channel();
    let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
    for i in 0..2 {
        workers.push(Box::new(spawn_worker(
            quiver::native_transport::SteppedClock::new(virtual_time.clone()),
            builtins.clone(),
            i as u16,
            waker.clone(),
        )));
    }
    let mut environment = Environment::<NativeEffect>::new(workers);
    environment.set_runtime_declarations(builtins.runtime_declarations().clone());
    let resolver = Box::new(PackageResolver::memory(HashMap::new()));
    let repl = Repl::new(&mut environment, resolver, builtins).expect("failed to create REPL");
    (environment, repl)
}

fn poll(environment: &mut Environment<NativeEffect>, request_id: u64) -> RequestResult {
    let deadline = Instant::now() + DEADLINE;
    loop {
        environment.step().expect("environment step failed");
        if let Some(result) = environment
            .poll_request(request_id)
            .expect("poll_request failed")
        {
            return result;
        }
        assert!(Instant::now() < deadline, "request never resolved");
        std::thread::sleep(Duration::from_micros(10));
    }
}

fn process_types(
    environment: &mut Environment<NativeEffect>,
) -> HashMap<usize, (quiver_core::types::Type, usize)> {
    let request_id = environment
        .request_process_types()
        .expect("failed to request process types");
    let RequestResult::ProcessTypes(types) = poll(environment, request_id) else {
        panic!("unexpected result for process types request");
    };
    types
}

/// Run a line through the three explicit steps and return its formatted result
/// (`None` for a line with nothing to execute, such as type definitions).
fn eval_split(
    environment: &mut Environment<NativeEffect>,
    repl: &mut Repl<NativeEffect>,
    source: &str,
) -> Option<String> {
    let types = process_types(environment);
    let prepared = repl
        .prepare(environment, source, types)
        .expect("prepare failed");
    let compiled = repl.compile(prepared).expect("compile failed");
    let request_id = repl.commit(environment, compiled).expect("commit failed")?;
    match poll(environment, request_id) {
        RequestResult::Result(Ok(value)) => Some(environment.format_value(&value)),
        RequestResult::Result(Err(e)) => panic!("evaluation failed: {e:?} for: {source}"),
        _ => panic!("unexpected result type for: {source}"),
    }
}

#[test]
fn split_line_runs_and_chains() {
    let (mut environment, mut repl) = session();
    assert_eq!(
        eval_split(&mut environment, &mut repl, "x = 40").as_deref(),
        Some("40")
    );
    assert_eq!(
        eval_split(&mut environment, &mut repl, "[x, 2] ~> __integer_add__ ~").as_deref(),
        Some("42")
    );
}

#[test]
fn failed_compile_leaves_session_unpolluted() {
    let (mut environment, mut repl) = session();
    eval_split(&mut environment, &mut repl, "x = 1");

    let types = process_types(&mut environment);
    let prepared = repl
        .prepare(&mut environment, "[x, nope] ~> __integer_add__ ~", types)
        .expect("prepare failed");
    assert!(repl.compile(prepared).is_err());

    assert_eq!(
        eval_split(&mut environment, &mut repl, "x").as_deref(),
        Some("1")
    );
}

#[test]
fn abandoned_compiled_line_leaves_session_unpolluted() {
    let (mut environment, mut repl) = session();
    eval_split(&mut environment, &mut repl, "x = 1");
    let variables_before = repl.get_variables();

    let types = process_types(&mut environment);
    let prepared = repl
        .prepare(&mut environment, "x = 99", types)
        .expect("prepare failed");
    let compiled = repl.compile(prepared).expect("compile failed");
    drop(compiled);

    assert_eq!(repl.get_variables(), variables_before);
    assert_eq!(
        eval_split(&mut environment, &mut repl, "x").as_deref(),
        Some("1")
    );
}

#[test]
fn type_definition_line_commits_without_executing() {
    let (mut environment, mut repl) = session();
    assert_eq!(
        eval_split(&mut environment, &mut repl, "'pair = ['int, 'int]"),
        None
    );
    assert_eq!(
        eval_split(&mut environment, &mut repl, "first = #'pair { $0 }").as_deref(),
        Some("#0")
    );
    assert_eq!(
        eval_split(&mut environment, &mut repl, "first [7, 8]").as_deref(),
        Some("7")
    );
}

#[test]
#[should_panic(expected = "stale line")]
fn compiling_a_superseded_line_panics() {
    let (mut environment, mut repl) = session();
    let types = process_types(&mut environment);
    let first = repl
        .prepare(&mut environment, "1", types)
        .expect("prepare failed");
    let types = process_types(&mut environment);
    let _second = repl
        .prepare(&mut environment, "2", types)
        .expect("prepare failed");
    let _ = repl.compile(first);
}
