//! The serializing transport: an environment whose workers receive every command — and
//! answer every event — through a serialize/deserialize round trip, exactly as the web
//! build's workers do across WASM memories.
//!
//! What this holds still is the *delta* path for program updates: a serializing worker
//! is sent `TableUpdate::Appended` registries and `CompatibilityUpdate::Delta` derived
//! tables, and must arrive at the same state a shared-memory worker gets by handle.
//! The scenario leans on each derived table: `IsType` on late-defined patterns
//! (type_compatibility), field access on late-defined tuples (field_offsets), structural
//! equality across separately built tuples (canonical_tuples), and messages into
//! closures (parameter tables).

use quiver_compiler::PackageResolver;
use quiver_compiler::compiler::CompileOptions;
use quiver_core::builtins::BuiltinRegistry;
use quiver_environment::{Command, Environment, EnvironmentError, Event, Repl, WorkerHandle};
use quiver_io::NativeEffect;
use std::collections::HashMap;
use std::rc::Rc;

fn builtins() -> BuiltinRegistry<NativeEffect> {
    BuiltinRegistry::<NativeEffect>::with_modules(&quiver_core::builtins::universal_modules())
}

/// A worker handle that round-trips every command and event through serde, and reports
/// itself as not sharing memory — the environment then takes the delta path for every
/// program update, which is the path under test. Serialized `UpdateProgram` sizes are
/// recorded, so a test can hold the property the deltas exist for: steady-state updates
/// must not scale with the program.
struct SerializingHandle<H> {
    inner: H,
    update_sizes: std::sync::Arc<std::sync::Mutex<Vec<usize>>>,
}

impl<E: quiver_environment::Effect, H: WorkerHandle<E>> WorkerHandle<E> for SerializingHandle<H> {
    fn send(&mut self, command: Command<E>) -> Result<(), EnvironmentError> {
        let bytes = serde_json::to_vec(&command).expect("serialize command");
        if matches!(command, Command::UpdateProgram(_)) {
            self.update_sizes.lock().unwrap().push(bytes.len());
        }
        self.inner
            .send(serde_json::from_slice(&bytes).expect("deserialize command"))
    }

    fn try_recv(&mut self) -> Result<Option<Event<E>>, EnvironmentError> {
        Ok(self.inner.try_recv()?.map(|event| {
            let bytes = serde_json::to_vec(&event).expect("serialize event");
            serde_json::from_slice(&bytes).expect("deserialize event")
        }))
    }
}

type UpdateSizes = std::sync::Arc<std::sync::Mutex<Vec<usize>>>;

fn environment() -> (Environment<NativeEffect>, UpdateSizes) {
    let builtins = builtins();
    let (waker, _wake) = quiver::native_transport::wake_channel();
    let update_sizes = UpdateSizes::default();
    let workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = (0..2)
        .map(|i| {
            Box::new(SerializingHandle {
                inner: quiver::spawn_worker(
                    quiver::native_transport::SystemClock,
                    builtins.clone(),
                    i as u16,
                    waker.clone(),
                ),
                update_sizes: std::sync::Arc::clone(&update_sizes),
            }) as Box<_>
        })
        .collect();
    let mut environment = Environment::<NativeEffect>::new(workers);
    environment.set_runtime_declarations(builtins.runtime_declarations().clone());
    (environment, update_sizes)
}

fn store() -> Rc<quiver_compiler::ArtifactStore> {
    thread_local! {
        static STORE: Rc<quiver_compiler::ArtifactStore> =
            Rc::new(quiver_compiler::ArtifactStore::cache());
    }
    STORE.with(Rc::clone)
}

fn evaluate(
    environment: &mut Environment<NativeEffect>,
    repl: &mut Repl<NativeEffect>,
    source: &str,
) -> String {
    let request = repl
        .evaluate(environment, source, HashMap::new())
        .unwrap_or_else(|e| panic!("evaluate `{source}`: {e}"))
        .expect("a line with something to run");
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    loop {
        environment.step().ok();
        match environment.poll_request(request) {
            Ok(Some(quiver_environment::RequestResult::Result(Ok(value)))) => {
                break format!("{value:?}");
            }
            Ok(Some(quiver_environment::RequestResult::Result(Err(e)))) => {
                panic!("runtime error for `{source}`: {e:?}")
            }
            Ok(None) => assert!(std::time::Instant::now() < deadline, "timed out"),
            other => panic!("unexpected result: {:?}", other.is_ok()),
        }
    }
}

#[test]
fn a_serializing_worker_reaches_the_same_state_as_a_shared_one() {
    let (mut environment, update_sizes) = environment();
    let resolver = Box::new(PackageResolver::memory(HashMap::new()));
    let mut repl = Repl::new(&mut environment, resolver, builtins()).expect("repl");
    repl.set_artifact_store(store());
    repl.set_compile_options(CompileOptions {
        debug: false,
        source_name: "repl".to_string(),
        ..Default::default()
    });

    for (source, expected) in [
        // Links whole modules: the registries arrive appended, the derived tables as
        // deltas, at module scale.
        ("%num.mul [7, 6]", Some("Int(42)")),
        // A tuple defined *after* the module linked: field_offsets gains cells and
        // rows past the seeded state.
        ("p = Point[x: 3, y: 4]", None),
        ("p.y", Some("Int(4)")),
        // IsType on a pattern first named here: a type_compatibility row written
        // below the table's tail.
        (
            "p ~> { =Point[x: a, y: b] => %num.add [a, b] | -1 }",
            Some("Int(7)"),
        ),
        // Structural equality across separately built tuples: canonical_tuples.
        ("q = Point[x: 3, y: 4]", None),
        ("{ q ~> =&p; 1 | 0 }", Some("Int(1)")),
        // Closures through module combinators: function parameter rows.
        (
            "%list{1, 2, 3} ~> %list.map [~, #{ %num.mul [$, 2] }] ~> %list.fold [~, init: 0, f: %num.add]",
            Some("Int(12)"),
        ),
        // A second module linked late: additions to rows that already existed.
        ("%str.from_int 99", None),
        ("Point[x: 9, y: 9] ~> .x", Some("Int(9)")),
    ] {
        let rendered = evaluate(&mut environment, &mut repl, source);
        if let Some(expected) = expected {
            assert!(
                rendered.contains(expected),
                "`{source}` gave {rendered}, expected {expected}"
            );
        }
    }

    // The property the deltas exist for: with the modules long linked, a repeated
    // line's updates carry its own code plus a small extension — not the program.
    // (First-line updates linked whole modules and ran to hundreds of KB.)
    let warm_mark = update_sizes.lock().unwrap().len();
    let peak_warmup = *update_sizes.lock().unwrap().iter().max().expect("updates");
    assert!(
        peak_warmup > 100_000,
        "expected module linking to dominate warm-up updates, saw at most {peak_warmup} B"
    );
    assert!(evaluate(&mut environment, &mut repl, "%num.mul [8, 6]").contains("Int(48)"));
    let sizes = update_sizes.lock().unwrap();
    let steady = &sizes[warm_mark..];
    assert!(!steady.is_empty(), "the repeat line must ship an update");
    assert!(
        steady.iter().all(|&bytes| bytes < 20_000),
        "a steady-state update must not scale with the program: {steady:?}"
    );
}
