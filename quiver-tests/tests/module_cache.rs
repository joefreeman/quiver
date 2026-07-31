// Module-cache transparency: reusing a compiled std module from the cache must
// infer exactly the types a fresh compile does. The scenarios pin the shapes that
// have diverged (or been suspected of diverging) before: a module reused on a
// later line of the same session, a module cached as a *nested* import of another
// module, and the shared warm state of the test harness itself.
mod common;
use common::quiver;
use std::rc::Rc;

use quiver::spawn_worker;
use quiver_compiler::PackageResolver;
use quiver_environment::{Environment, Repl, WorkerHandle};
use quiver_io::NativeEffect;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

/// A %parse pipeline whose result type exercises generic instantiation through the
/// cached module (dispatch-table restore) and union folding of members differing
/// only by annotation row — the shapes that regressed under cache reuse before.
const BODY: &str = r#"
    comma = [44, "','"] ~> %parse.byte
    elems = [&%parse.int, &comma] ~> %parse.sep_by
    a1 = [[91, "'['"] ~> %parse.byte, &elems] ~> %parse.right
    a3 = [&a1, [93, "']'"] ~> %parse.byte] ~> %parse.left
    %parse.map [&a3, #{ $ }]
"#;

const EXPECTED: &str = "#P[data: 'bin, pos: 'int, len: 'int, err: (Expected[offset: 'int, message: Str['bin]] | [])] -> ([(Cons['int, μ1] | Nil), P[data: 'bin, pos: 'int, len: 'int, err: (Expected[offset: 'int, message: Str['bin]] | [])]] | [])";

fn eval_line(
    environment: &mut Environment<NativeEffect>,
    repl: &mut Repl<NativeEffect>,
    virtual_time: &Arc<AtomicU64>,
    source: &str,
) -> String {
    let types_request_id = environment.request_process_types().unwrap();
    let process_types = loop {
        environment.step().ok();
        match environment.poll_request(types_request_id) {
            Ok(Some(quiver_environment::RequestResult::ProcessTypes(types))) => break types,
            Ok(None) => continue,
            other => panic!("unexpected process-types result: {:?}", other.is_ok()),
        }
    };

    match repl.evaluate(environment, source, process_types) {
        Ok(Some(request_id)) => {
            let start = std::time::Instant::now();
            loop {
                let did_work = environment.step().unwrap_or(false);
                if !did_work {
                    virtual_time.fetch_add(1, Ordering::Relaxed);
                }
                match environment.poll_request(request_id) {
                    Ok(Some(quiver_environment::RequestResult::Result(Ok(_), _))) => break,
                    Ok(Some(quiver_environment::RequestResult::Result(Err(e), _))) => {
                        panic!("runtime error: {:?}", e)
                    }
                    Ok(None) => {
                        assert!(start.elapsed().as_secs() < 10, "timeout");
                        std::thread::sleep(std::time::Duration::from_micros(10));
                    }
                    other => panic!("unexpected result: {:?}", other.is_ok()),
                }
            }
        }
        Ok(None) => {}
        Err(e) => panic!("compile error: {:?}", e),
    }

    repl.format_type(repl.get_last_result_type())
}

/// One REPL session: evaluate `first_line` (if any), then BODY, and return BODY's
/// inferred type. With a first line, BODY's %parse members come from the module
/// cache; without one, this is the fresh-compile baseline. `session_with_artifacts`
/// carries a warmed artifact store, so imports link instead of compiling.
fn session(debug: bool, first_line: Option<&str>) -> String {
    run_session(debug, first_line, None)
}

fn session_with_artifacts(
    debug: bool,
    first_line: Option<&str>,
    store: &Rc<quiver_compiler::ArtifactStore>,
) -> String {
    run_session(debug, first_line, Some(store.clone()))
}

fn run_session(
    debug: bool,
    first_line: Option<&str>,
    artifacts: Option<Rc<quiver_compiler::ArtifactStore>>,
) -> String {
    let virtual_time_ms = Arc::new(AtomicU64::new(0));

    let mut builtins = quiver_core::builtins::BuiltinRegistry::<NativeEffect>::with_modules(
        &quiver_core::builtins::core_modules(),
    );
    for module in quiver_core::builtins::io_modules() {
        module(&mut builtins);
    }

    let (waker, _wake) = quiver::native_transport::wake_channel();
    let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
    for i in 0..2 {
        let builtins_clone = builtins.clone();
        workers.push(Box::new(spawn_worker(
            quiver::native_transport::SteppedClock::new(virtual_time_ms.clone()),
            builtins_clone,
            false,
            i as u16,
            waker.clone(),
        )));
    }

    let mut environment = Environment::<NativeEffect>::new(workers);
    environment.set_runtime_declarations(builtins.runtime_declarations().clone());
    let resolver = Box::new(PackageResolver::memory(Default::default()));
    let mut repl = Repl::new(&mut environment, resolver, builtins).unwrap();
    if let Some(store) = artifacts {
        repl.set_artifact_store(store);
    }
    if debug {
        repl.set_compile_options(quiver_compiler::compiler::CompileOptions {
            debug: true,
            source_name: "test".to_string(),
        });
    }

    if let Some(first) = first_line {
        eval_line(&mut environment, &mut repl, &virtual_time_ms, first);
    }
    let result = eval_line(&mut environment, &mut repl, &virtual_time_ms, BODY);
    // Every session doubles as an incremental-tables check: the environment extended
    // its compatibility tables once per merge above, and the result must equal a
    // from-scratch recomputation over the final merged program.
    environment.verify_compatibility_tables();
    result
}

#[test]
fn fresh_compile_baseline() {
    assert_eq!(session(false, None), EXPECTED);
}

#[test]
fn reused_from_direct_import() {
    let warm = session(false, Some(r#"p = [44, "','"] ~> %parse.byte; Ok"#));
    assert_eq!(warm, EXPECTED);
}

#[test]
fn reused_from_direct_import_debug() {
    let warm = session(true, Some(r#"p = [44, "','"] ~> %parse.byte; Ok"#));
    assert_eq!(warm, EXPECTED);
}

#[test]
fn reused_from_nested_import_via_list() {
    // %parse cached while compiling %list — the cache entry a nested import builds.
    let warm = session(false, Some(r#"l = %list.new; Ok"#));
    assert_eq!(warm, EXPECTED);
}

#[test]
fn reused_from_nested_import_via_num() {
    let warm = session(false, Some(r#"%num.add [1, 2] ~> =3"#));
    assert_eq!(warm, EXPECTED);
}

#[test]
fn reused_through_shared_store() {
    // The shared-store path: the first evaluation extracts %parse (and its deps)
    // into the harness's artifact store, the second links a fresh REPL from it.
    quiver()
        .debug()
        .evaluate(r#"p = [44, "','"] ~> %parse.byte; Ok"#)
        .expect("Ok");
    quiver().debug().evaluate(BODY).expect_type(EXPECTED);
}

#[test]
fn reused_after_member_reference() {
    // Line 1 binds a *reference* to a module member (no call) — the shape recorded
    // as diverging when this repro was first captured.
    let warm = session(false, Some(r#"p = &%parse.int; Ok"#));
    assert_eq!(warm, EXPECTED);
}

#[test]
fn reused_after_member_reference_debug() {
    let warm = session(true, Some(r#"p = &%parse.int; Ok"#));
    assert_eq!(warm, EXPECTED);
}

fn builtins() -> quiver_core::builtins::BuiltinRegistry<NativeEffect> {
    let mut builtins = quiver_core::builtins::BuiltinRegistry::<NativeEffect>::with_modules(
        &quiver_core::builtins::core_modules(),
    );
    for module in quiver_core::builtins::io_modules() {
        module(&mut builtins);
    }
    builtins
}

fn options(debug: bool) -> quiver_compiler::compiler::CompileOptions {
    quiver_compiler::compiler::CompileOptions {
        debug,
        source_name: "std".to_string(),
    }
}

fn warm_store(debug: bool) -> Rc<quiver_compiler::ArtifactStore> {
    let store = Rc::new(quiver_compiler::ArtifactStore::in_memory());
    quiver_compiler::warm_std_store(&store, &builtins(), options(debug));
    store
}

/// The whole standard library warmed once per compile mode, shared by the linking
/// tests in this binary (memory-only, hermetic from the user cache).
fn build_artifacts(debug: bool) -> Rc<quiver_compiler::ArtifactStore> {
    use std::cell::RefCell;
    use std::collections::HashMap;
    thread_local! {
        // Per-thread: an artifact holds a compile-time `Value`, whose payload is `Rc`, so a
        // store cannot be shared across the test harness's threads. Warming is repeated per
        // thread that asks for it.
        static WARMED: RefCell<HashMap<bool, Rc<quiver_compiler::ArtifactStore>>> =
            RefCell::new(HashMap::new());
    }
    if let Some(store) = WARMED.with(|w| w.borrow().get(&debug).cloned()) {
        return store;
    }
    let store = warm_store(debug);
    WARMED.with(|w| w.borrow_mut().insert(debug, store.clone()));
    store
}

#[test]
fn store_covers_every_std_module() {
    // Every std module must extract an artifact when warmed — a missing one means a
    // module was marked hidden (an undeclared reference surfaced mid-compile, e.g.
    // from a dialect expansion) and would silently compile from source per session.
    let store = build_artifacts(false);
    let names = quiver_compiler::resolver::std_module_names();
    assert!(!names.is_empty());
    let stored: Vec<String> = store
        .entries()
        .iter()
        .map(|(_, artifact)| artifact.id.display())
        .collect();
    let missing: Vec<&String> = names.iter().filter(|n| !stored.contains(n)).collect();
    assert!(
        missing.is_empty(),
        "std modules without artifacts after warming: {:?}",
        missing
    );
}

#[test]
fn warming_is_deterministic() {
    // Two warms must produce identical keys and byte-identical artifacts:
    // reproducible compilation is what makes artifacts content-addressable. A
    // divergence means id assignment somewhere depends on hash-map iteration order,
    // or a hash-ordered collection leaked past the canonical extraction boundary.
    let a = build_artifacts(false);
    let b = warm_store(false);
    let (a, b) = (a.entries(), b.entries());
    assert_eq!(
        a.iter().map(|(key, _)| *key).collect::<Vec<_>>(),
        b.iter().map(|(key, _)| *key).collect::<Vec<_>>(),
        "two std warms must produce the same artifact keys"
    );
    for ((key, first), (_, second)) in a.iter().zip(&b) {
        assert_eq!(
            serde_json::to_vec(&**first).unwrap(),
            serde_json::to_vec(&**second).unwrap(),
            "artifact {key:016x} must serialize identically across warms"
        );
    }
}

#[test]
fn linked_session_matches_cold_inference() {
    // Linking %parse (and its dependency closure) from per-module artifacts must
    // infer exactly what compiling from source does.
    let store = build_artifacts(false);
    assert_eq!(session_with_artifacts(false, None, &store), EXPECTED);
}

#[test]
fn linked_session_matches_cold_inference_debug() {
    let store = build_artifacts(true);
    assert_eq!(session_with_artifacts(true, None, &store), EXPECTED);
}

#[test]
fn linked_receive_type_line_matches_cold_inference() {
    // A top-level select naming a module type makes receive-type extraction resolve
    // that module's namespace before the line's body compiles. With the module also
    // value-imported, the artifact link must still leave the session identical to a
    // from-source compile — imports link before extraction resolves any types, so
    // the namespace is never built from source only for the link to overwrite it.
    let line = "kill = &%proc.kill\n![#'%proc.changed, 0]";
    assert_eq!(session(false, Some(line)), EXPECTED);
    let store = build_artifacts(false);
    assert_eq!(session_with_artifacts(false, Some(line), &store), EXPECTED);
}

#[test]
fn linked_session_is_link_order_independent() {
    // Importing other modules first changes which session ids everything lands on;
    // inference must not notice.
    let store = build_artifacts(false);
    let warm = session_with_artifacts(
        false,
        Some(r#"a = %str.length "hi"; b = %list.new; Ok"#),
        &store,
    );
    assert_eq!(warm, EXPECTED);
}

#[test]
fn warming_is_order_independent() {
    // An artifact must be a pure function of (source, dependencies, compiler build)
    // — the claim its content-address makes. A session importing std in *reverse*
    // order has a completely different compile and interning history; the artifacts
    // it extracts must nonetheless be byte-identical to the canonical warm's, under
    // the same keys. (The function dedup floor is what makes this hold: without it,
    // a module's functions could collapse onto whichever structural twin happened to
    // register first, and attribution — hence artifact content — would vary here.)
    let canonical = build_artifacts(false);
    let reversed = Rc::new(quiver_compiler::ArtifactStore::in_memory());
    let line: String = quiver_compiler::resolver::std_module_names()
        .iter()
        .rev()
        .enumerate()
        .map(|(index, name)| format!("r{} = %{}\n", index, name))
        .collect();
    session_with_artifacts(false, Some(&line), &reversed);
    let (canonical, reversed) = (canonical.entries(), reversed.entries());
    assert_eq!(
        canonical.iter().map(|(key, _)| *key).collect::<Vec<_>>(),
        reversed.iter().map(|(key, _)| *key).collect::<Vec<_>>(),
        "reverse-order warming must produce the same artifact keys"
    );
    for ((key, first), (_, second)) in canonical.iter().zip(&reversed) {
        let (a, b) = (
            serde_json::to_vec(&**first).unwrap(),
            serde_json::to_vec(&**second).unwrap(),
        );
        if a != b {
            let a: serde_json::Value = serde_json::from_slice(&a).unwrap();
            let b: serde_json::Value = serde_json::from_slice(&b).unwrap();
            for (field, value) in a.as_object().unwrap() {
                if b.get(field) != Some(value) {
                    eprintln!(
                        "module {} field {field} differs:\n  canonical: {}\n  reversed:  {}",
                        first.id.display(),
                        serde_json::to_string(value).unwrap(),
                        serde_json::to_string(b.get(field).unwrap()).unwrap(),
                    );
                }
            }
            panic!(
                "artifact {key:016x} ({}) must not depend on session import order",
                first.id.display()
            );
        }
    }
}

#[test]
fn artifacts_round_trip_through_disk() {
    // Persisting and reloading artifacts must lose nothing: a session linking from
    // a store that only has the disk copies behaves identically to one linking the
    // in-memory originals.
    let warmed = build_artifacts(false);
    let dir =
        std::env::temp_dir().join(format!("quiver-artifact-roundtrip-{}", std::process::id()));
    let disk = quiver_compiler::ArtifactStore::at_dir(dir.clone());
    for (key, artifact) in warmed.entries() {
        disk.save(key, (*artifact).clone());
    }
    // A fresh store over the same directory sees only the files.
    let reloaded = Rc::new(quiver_compiler::ArtifactStore::at_dir(dir.clone()));
    for (key, artifact) in warmed.entries() {
        let restored = reloaded.load(key).expect("persisted artifact must load");
        assert_eq!(
            serde_json::to_vec(&*artifact).unwrap(),
            serde_json::to_vec(&*restored).unwrap(),
            "artifact {key:016x} must round-trip identically"
        );
    }
    assert_eq!(session_with_artifacts(false, None, &reloaded), EXPECTED);
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn linked_html_live_encodes_frames() {
    // Regression lock: a linked tuple that is constructed but never referenced as a
    // type must still be runtime-testable. The linker now registers a `Type::Tuple`
    // wrapper entry for every tuple it interns — without one, the compatibility
    // tables can't represent the tuple, every `IsType` rejects it, and %json's
    // encoder (called by %html/live.encode on the wire value) fell through its
    // Array branch into the Object branch, failing with `TypeMismatch { expected:
    // tuple, found: integer }`.
    let store = build_artifacts(false);
    let result = session_with_artifacts(
        false,
        Some(
            r#"view = #[name: Str['bin]] { %html{ <p>{$name}</p> } }
               f1 = view [name: "a"] ~> %html/live.frame
               f2 = view [name: "b"] ~> %html/live.frame
               %html/live.diff [f1, f2] ~> %html/live.encode ["0", ~] ~> ='%str; Ok"#,
        ),
        &store,
    );
    assert_eq!(result, EXPECTED);
}
