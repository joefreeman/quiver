//! The run-as-one-line-session claim (docs plan, phase 4.5): a program compiled by
//! `compile_entry` is executable by the same convention as a REPL line — create a
//! fresh persistent process, resume it with the tree-shaken bytecode, await the
//! result. This is the convention the protocol's only code-bearing verb (`resume`)
//! relies on, so it gets a direct test rather than an assumption.

use quiver_cli::spawn_worker;
use quiver_environment::{Environment, RequestResult, WorkerHandle};
use quiver_io::NativeEffect;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::time::{Duration, Instant};

fn environment() -> Environment<NativeEffect> {
    let virtual_time = Arc::new(AtomicU64::new(0));
    let builtins = quiver_cli::build_builtin_registry();
    let (waker, _wake) = quiver_cli::native_transport::wake_channel();
    let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
    for i in 0..2 {
        workers.push(Box::new(spawn_worker(
            quiver_cli::native_transport::SteppedClock::new(virtual_time.clone()),
            builtins.clone(),
            false,
            i as u16,
            waker.clone(),
        )));
    }
    let mut environment = Environment::<NativeEffect>::new(workers);
    environment.set_runtime_declarations(builtins.runtime_declarations().clone());
    environment
}

fn compile(source: &str) -> quiver_core::bytecode::Bytecode {
    let ast = quiver_compiler::parse(source).expect("parse failed");
    let resolver = quiver_compiler::PackageResolver::inline();
    let (program, entry) = quiver_cli::compile::compile_entry(
        ast,
        &resolver,
        &quiver_cli::build_builtin_registry(),
        Default::default(),
        None,
    )
    .expect("compile failed");
    program.to_bytecode_optimized(entry)
}

fn await_result(environment: &mut Environment<NativeEffect>, request: u64) -> String {
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        environment.step().expect("step failed");
        match environment.poll_request(request).expect("poll failed") {
            Some(RequestResult::Result(Ok(value), _)) => return environment.format_value(&value),
            Some(other) => panic!("unexpected result: {other:?}"),
            None => {
                assert!(Instant::now() < deadline, "request never resolved");
                std::thread::sleep(Duration::from_micros(10));
            }
        }
    }
}

#[test]
fn a_program_is_a_one_line_session() {
    let mut environment = environment();
    // Pure allocation, no bytecode — the protocol's POST /processes.
    let pid = environment.start_process(None).expect("start failed");
    // The one code-bearing verb: resume with a compiled program.
    environment
        .resume_process(pid, compile("#{ [40, 2] ~> __integer_add__ }"))
        .expect("resume failed");
    let request = environment
        .request_result(pid, None)
        .expect("request failed");
    assert_eq!(await_result(&mut environment, request), "42");
}

#[test]
fn the_process_survives_for_another_resume() {
    // A run root is a persistent process: after its program completes it sleeps, and
    // a further resume works — which is also what makes run and REPL the same shape.
    let mut environment = environment();
    let pid = environment.start_process(None).expect("start failed");
    for expected in ["42", "9"] {
        let source = match expected {
            "42" => "#{ [40, 2] ~> __integer_add__ }",
            _ => "#{ [4, 5] ~> __integer_add__ }",
        };
        environment
            .resume_process(pid, compile(source))
            .expect("resume failed");
        let request = environment
            .request_result(pid, None)
            .expect("request failed");
        assert_eq!(await_result(&mut environment, request), expected);
    }
}

#[test]
fn top_level_work_runs_on_resume() {
    // The entry runs the program's top level in the root process before calling the
    // function it evaluates to — bindings made there feed the program body.
    let mut environment = environment();
    let pid = environment.start_process(None).expect("start failed");
    environment
        .resume_process(pid, compile("x = 40\n#{ [x, 2] ~> __integer_add__ }"))
        .expect("resume failed");
    let request = environment
        .request_result(pid, None)
        .expect("request failed");
    assert_eq!(await_result(&mut environment, request), "42");
}
