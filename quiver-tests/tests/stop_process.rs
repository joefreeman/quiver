//! Host-initiated stop of a persistent (session) process — `Environment::stop_process`,
//! the mechanism behind the REPL's Ctrl-C interrupt and session resets. In-language
//! kills deliberately no-op on persistent processes, so the stop must cross that
//! exemption: resolve a pending result request with `Killed`, clear persistence whether
//! the process is running or sleeping, and cascade teardown through its owned subtree.
//!
//! These tests drive `Environment` + `Repl` directly (rather than through the common
//! builder) because stopping mid-evaluation needs control between issuing a request and
//! polling its result. Programs avoid std imports, so no artifact store is attached.

use quiver::spawn_worker;
use quiver_compiler::PackageResolver;
use quiver_core::error::Error;
use quiver_core::process::{ProcessId, ProcessStatus};
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

/// Compile a line and hand it to the session process, returning the result request id.
fn begin(
    environment: &mut Environment<NativeEffect>,
    repl: &mut Repl<NativeEffect>,
    source: &str,
) -> u64 {
    let types_id = environment
        .request_process_types()
        .expect("failed to request process types");
    let RequestResult::ProcessTypes(types) = poll(environment, types_id) else {
        panic!("unexpected result for process types request");
    };
    repl.evaluate(environment, source, types)
        .expect("evaluation failed")
        .expect("expected executable code")
}

fn statuses(environment: &mut Environment<NativeEffect>) -> HashMap<ProcessId, ProcessStatus> {
    let request_id = environment
        .request_statuses()
        .expect("failed to request statuses");
    let RequestResult::Statuses(statuses) = poll(environment, request_id) else {
        panic!("unexpected result for statuses request");
    };
    statuses
}

/// Wait until the session's (sole) spawned child reaches the given status, returning
/// its pid. Spawns and kills route asynchronously through the environment, so both the
/// child's appearance and its teardown need a bounded wait, not a step count.
fn await_child(
    environment: &mut Environment<NativeEffect>,
    session_pid: ProcessId,
    expected: ProcessStatus,
) -> ProcessId {
    let deadline = Instant::now() + DEADLINE;
    loop {
        let statuses = statuses(environment);
        if let Some((pid, status)) = statuses.iter().find(|(pid, _)| **pid != session_pid)
            && *status == expected
        {
            return *pid;
        }
        assert!(
            Instant::now() < deadline,
            "child never reached {expected:?}; statuses: {statuses:?}"
        );
    }
}

#[test]
fn stop_resolves_blocked_evaluation_with_killed() {
    let (mut environment, mut repl) = session();

    // The session process blocks in a receive no one will answer — the locked-REPL shape.
    let request_id = begin(&mut environment, &mut repl, "!'int");

    environment
        .stop_process(repl.process_id())
        .expect("stop_process failed");

    match poll(&mut environment, request_id) {
        RequestResult::Result(Err(Error::Killed)) => {}
        other => panic!("expected the Killed error, got {other:?}"),
    }
}

#[test]
fn stop_tears_down_spawned_children() {
    let (mut environment, mut repl) = session();
    let session_pid = repl.process_id();

    // Spawn a child that waits forever, then block the session itself.
    let request_id = begin(&mut environment, &mut repl, "child = @#{ !'int } []; !'int");
    let child = await_child(&mut environment, session_pid, ProcessStatus::Waiting);

    environment
        .stop_process(session_pid)
        .expect("stop_process failed");
    match poll(&mut environment, request_id) {
        RequestResult::Result(Err(Error::Killed)) => {}
        other => panic!("expected the Killed error, got {other:?}"),
    }

    // Ownership teardown cascades from the stopped session to the child.
    assert_eq!(
        await_child(&mut environment, session_pid, ProcessStatus::Failed),
        child
    );
}

#[test]
fn stop_sleeping_session_clears_persistence_and_cascades() {
    let (mut environment, mut repl) = session();
    let session_pid = repl.process_id();

    // The line completes — the session goes to sleep on its result — while the spawned
    // child outlives it, still owned across the sleep.
    let request_id = begin(&mut environment, &mut repl, "child = @#{ !'int } []; 1");
    match poll(&mut environment, request_id) {
        RequestResult::Result(Ok(_)) => {}
        other => panic!("expected a value, got {other:?}"),
    }
    let child = await_child(&mut environment, session_pid, ProcessStatus::Waiting);
    assert_eq!(
        statuses(&mut environment).get(&session_pid),
        Some(&ProcessStatus::Sleeping)
    );

    environment
        .stop_process(session_pid)
        .expect("stop_process failed");

    // The stop keeps the completed result but sheds persistence — Sleeping becomes
    // Completed (a reclaimable tombstone) — and still tears down the subtree.
    assert_eq!(
        await_child(&mut environment, session_pid, ProcessStatus::Failed),
        child
    );
    let deadline = Instant::now() + DEADLINE;
    loop {
        if statuses(&mut environment).get(&session_pid) == Some(&ProcessStatus::Completed) {
            break;
        }
        assert!(
            Instant::now() < deadline,
            "session never shed its persistence"
        );
    }
}

/// Like [`session`], with the network builtins attached and a real io_uring backend,
/// so a program can hold a live listener when it is stopped.
fn io_session() -> (Environment<NativeEffect>, Repl<NativeEffect>) {
    let virtual_time = Arc::new(AtomicU64::new(0));
    let mut builtins = quiver_core::builtins::BuiltinRegistry::<NativeEffect>::with_modules(
        &quiver_core::builtins::universal_modules(),
    );
    for module in quiver_core::builtins::io_modules() {
        module(&mut builtins);
    }
    quiver_io::attach_network_builtins(&mut builtins);
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
    environment.set_effect_backend(Box::new(
        quiver_io::NativeEffectBackend::new(64).expect("io_uring backend"),
    ));
    let resolver = Box::new(PackageResolver::memory(HashMap::new()));
    let repl = Repl::new(&mut environment, resolver, builtins).expect("failed to create REPL");
    (environment, repl)
}

#[test]
fn stop_releases_a_listener_with_an_accept_in_flight() {
    // A submitted io_uring accept holds its own reference to the listening socket, so
    // closing the fd alone leaves the port bound — accepting connections into a backlog
    // nothing drains, so connects hang instead of being refused. The stop's resource
    // sweep must cancel the in-flight accept for the close to release the port.
    let (mut environment, mut repl) = io_session();
    let port = {
        let probe = std::net::TcpListener::bind("127.0.0.1:0").expect("bind probe");
        probe.local_addr().expect("addr").port()
    };
    let request_id = begin(
        &mut environment,
        &mut repl,
        &format!(
            "[{port}, 16] ~> __tcp_listen__ ~ ~> =(\\TcpListener)l; __tcp_listener_accept__ l"
        ),
    );
    // Bind-probe readiness: the wildcard listener makes a local bind fail, without
    // connecting — a connect would complete the very accept that must stay in flight.
    // Then wait for the accept itself to reach the ring: the bug needs an operation in
    // flight at close time, and the effect takes a few more steps to route.
    let address = format!("127.0.0.1:{port}");
    let deadline = Instant::now() + DEADLINE;
    loop {
        environment.step().expect("environment step failed");
        if std::net::TcpListener::bind(&address).is_err() && environment.io_in_flight() {
            break;
        }
        assert!(Instant::now() < deadline, "the accept never got in flight");
        std::thread::sleep(Duration::from_millis(5));
    }

    environment
        .stop_process(repl.process_id())
        .expect("stop_process failed");
    match poll(&mut environment, request_id) {
        RequestResult::Result(Err(Error::Killed)) => {}
        other => panic!("expected the Killed error, got {other:?}"),
    }

    // Released means bindable again — not merely closed in this process's fd table
    // while the cancelled-too-late accept keeps the kernel socket alive.
    let deadline = Instant::now() + DEADLINE;
    loop {
        environment.step().expect("environment step failed");
        if std::net::TcpListener::bind(&address).is_ok() {
            break;
        }
        assert!(
            Instant::now() < deadline,
            "the port is still bound after the stop"
        );
        std::thread::sleep(Duration::from_millis(5));
    }
}
