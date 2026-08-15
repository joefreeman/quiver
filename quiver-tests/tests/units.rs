//! Compiled units: extraction and linking.
//!
//! A unit is relocatable compiled code with no module identity — a REPL line or a
//! program entry — carrying its own functions plus an import table naming
//! `(module, key, that module's own indices)`. The transparency invariant is that
//! linking one into a session built from artifacts alone produces exactly what running
//! the original compile produces.
//!
//! Nothing in the shipping pipeline consumes units yet; these tests are what holds the
//! extraction and linking contracts still until it does.

use quiver_compiler::compiler::{Bindings, CompileOptions, ModuleCache, SessionTables};
use quiver_compiler::resolver::ModuleId;
use quiver_compiler::{ArtifactStore, PackageResolver, Registration};
use quiver_core::builtins::BuiltinRegistry;
use quiver_core::bytecode::Function;
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_environment::{Environment, Repl, WorkerHandle};
use quiver_io::NativeEffect;
use std::collections::HashMap;
use std::rc::Rc;

const FUEL: u64 = 500_000_000;

fn builtins() -> BuiltinRegistry<NativeEffect> {
    let mut builtins =
        BuiltinRegistry::<NativeEffect>::with_modules(&quiver_core::builtins::core_modules());
    for module in quiver_core::builtins::io_modules() {
        module(&mut builtins);
    }
    builtins
}

/// A store shared by every test in this binary, so std compiles once per thread.
fn store() -> Rc<ArtifactStore> {
    thread_local! {
        static STORE: Rc<ArtifactStore> = Rc::new(ArtifactStore::cache());
    }
    STORE.with(Rc::clone)
}

struct Compiled {
    program: Program,
    module_cache: ModuleCache,
    entry: usize,
    own_floor: usize,
}

/// Compile `source` in a hermetic session, wrapping its instructions in an entry
/// function. `own_floor` is the function table's length before the compile, which is
/// what tells extraction which functions the line owns.
fn compile_with(source: &str, debug: bool, store: Rc<ArtifactStore>) -> Compiled {
    compile_inner(source, debug, store, HashMap::new())
}

fn compile(source: &str, debug: bool) -> Compiled {
    compile_inner(source, debug, store(), HashMap::new())
}

/// Compile against project modules of the test's own, in a store of its own, so two
/// sessions can hold different modules under the same name.
fn compile_project(
    source: &str,
    modules: HashMap<Vec<String>, String>,
    store: Rc<ArtifactStore>,
) -> Compiled {
    compile_inner(source, false, store, modules)
}

fn compile_inner(
    source: &str,
    debug: bool,
    artifact_store: Rc<ArtifactStore>,
    modules: HashMap<Vec<String>, String>,
) -> Compiled {
    let parsed = quiver_compiler::parse(source).expect("parse");
    let resolver = PackageResolver::memory(modules);
    let mut program = Program::new();
    let mut module_cache = ModuleCache::new();
    module_cache.artifact_store = Some(artifact_store);
    let nil = program.register_type(Type::nil());
    let own_floor = program.get_functions().len();
    let compiled = quiver_compiler::Compiler::compile(
        parsed,
        &Bindings::default(),
        SessionTables::default(),
        &mut module_cache,
        &resolver,
        &mut program,
        nil,
        &HashMap::new(),
        &builtins(),
        None,
        CompileOptions {
            debug,
            source_name: "unit-test".to_string(),
            ..Default::default()
        },
    )
    .unwrap_or_else(|e| panic!("compile `{source}`: {:?}", e.error));

    let never = program.never();
    let callable = program.register_type(Type::Callable {
        parameter: nil,
        result: compiled.result_type,
        receive: never,
        states: None,
    });
    let entry = program.register_function(Function {
        instructions: compiled.instructions,
        captures: 0,
        type_id: callable,
    });
    Compiled {
        program,
        module_cache,
        entry,
        own_floor,
    }
}

/// Render in the producing program's own tables, so two sessions with different id
/// assignments are compared by what they mean rather than by raw ids.
fn run(program: &Program, entry: usize) -> String {
    let (value, _) = quiver_core::execute_bytecode_sync_with(
        program.to_bytecode(Some(entry)),
        &builtins(),
        false,
        FUEL,
        None,
    )
    .expect("execution");
    let binaries = quiver_core::format::HeapAndProgramLookup { heap: &[], program };
    quiver_core::format::format_value(&value, program, &binaries)
}

/// Link `id` and everything it depends on into `program`, from stored artifacts alone.
fn link_module_tree(
    id: &ModuleId,
    keys: &HashMap<ModuleId, u64>,
    program: &mut Program,
    module_cache: &mut ModuleCache,
) {
    if module_cache.module_functions.contains_key(id) {
        return;
    }
    let key = *keys
        .get(id)
        .unwrap_or_else(|| panic!("no key for {}", id.display()));
    let artifact = store()
        .load(key)
        .unwrap_or_else(|| panic!("no artifact for {} ({key:016x})", id.display()));
    for dependency in artifact.unit.dependencies() {
        link_module_tree(&dependency, keys, program, module_cache);
    }
    quiver_compiler::link_module(&artifact, id, program, module_cache, &builtins())
        .unwrap_or_else(|e| panic!("link {}: {:?}", id.display(), e));
    module_cache.key_cache.insert(id.clone(), key);
}

/// Compile, extract a unit, then link it into a program built from artifacts alone and
/// run it. Answers `(original, linked)` renderings.
fn round_trip(source: &str, registration: Registration) -> (String, String) {
    let compiled = compile(source, false);
    let unit = quiver_compiler::extract_unit(
        &compiled.program,
        &compiled.module_cache,
        Some(compiled.entry),
        compiled.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    let original = run(&compiled.program, compiled.entry);

    let mut fresh = Program::new();
    let mut fresh_cache = ModuleCache::new();
    fresh_cache.artifact_store = Some(store());
    for (module, key, _) in &unit.imports {
        assert_eq!(
            compiled
                .module_cache
                .content_key(module)
                .expect("an imported module has a stored artifact"),
            *key,
            "import key must match the compiling session's"
        );
        link_module_tree(
            module,
            &compiled.module_cache.key_cache,
            &mut fresh,
            &mut fresh_cache,
        );
    }
    let resolved = quiver_compiler::resolve_imports(&unit, &fresh_cache, "unit");
    let remaps = quiver_compiler::link_unit(
        &unit,
        "unit",
        &mut fresh,
        &resolved,
        &builtins(),
        registration,
    )
    .expect("link unit");
    let entry = remaps.functions[&unit.entry.expect("a line's unit has an entry")];
    (original, run(&fresh, entry))
}

const SOURCES: [(&str, &str); 6] = [
    ("%num.mul [7, 6]", "42"),
    (
        "%json.stringify %json{ {\"a\": [1, 2]} }",
        "\"{\\\"a\\\":[1,2]}\"",
    ),
    (
        "%list{1, 2, 3} ~> %list.map [~, #{ %num.mul [$, 2] }]",
        "Cons[2, Cons[4, Cons[6, Nil]]]",
    ),
    (
        "%list{1, 2, 3} ~> %list.fold [~, init: 0, f: &%num.add]",
        "6",
    ),
    ("%str.from_int 12345", "\"12345\""),
    (
        "%dict{ \"a\" => 1, \"b\" => 2 } ~> %dict.get [~, \"b\"]",
        "2",
    ),
];

#[test]
fn a_linked_unit_computes_what_the_original_did() {
    for (source, expected) in SOURCES {
        let (original, linked) = round_trip(source, Registration::Intern);
        assert_eq!(original, expected, "baseline changed for `{source}`");
        assert_eq!(
            linked, original,
            "linking `{source}` from artifacts diverged"
        );
    }
}

#[test]
fn units_link_under_either_registration_policy() {
    // A unit has no attribution to protect, so its functions may intern structurally;
    // appending must work too, since that is what a module linked beside it will do.
    let source = "%json.stringify %json{ {\"a\": 1} }";
    for registration in [Registration::Intern, Registration::Append] {
        let (original, linked) = round_trip(source, registration);
        assert_eq!(linked, original);
    }
}

/// A one-module project whose `%a.f` adds `n`, so two versions differ in content but not
/// in shape — same export, same function count, same indices.
fn module_a(n: &str) -> HashMap<Vec<String>, String> {
    HashMap::from([(
        vec!["a".to_string()],
        format!("[f: #'int {{ [$, {n}] ~> __integer_add__ }}]"),
    )])
}

#[test]
#[should_panic(expected = "but the session linked")]
fn resolving_imports_against_another_version_of_a_module_is_refused() {
    // Phase 1's deferred check, now that phase 3 has given it a public entry point. The
    // compiler-side resolver finds a dependency by *name*, so a session holding a
    // different `%a` than the unit was built against lines up index-for-index and would
    // mis-link in silence. The key in the import table is what catches it. (The
    // environment-side resolver is keyed outright, so it cannot make this mistake.)
    let unit = {
        let built = compile_project("%a.f 1", module_a("1"), Rc::new(ArtifactStore::in_memory()));
        quiver_compiler::extract_unit(
            &built.program,
            &built.module_cache,
            Some(built.entry),
            built.own_floor,
            quiver_compiler::Imports::Bundle,
        )
    };

    // A fresh session holding the *other* `%a`, linked from its own artifact.
    let other_store = Rc::new(ArtifactStore::in_memory());
    let other = compile_project("%a.f 1", module_a("2"), Rc::clone(&other_store));
    let (id, key) = other
        .module_cache
        .key_cache
        .iter()
        .find(|(id, _)| id.name == vec!["a".to_string()])
        .map(|(id, key)| (id.clone(), *key))
        .expect("%a is keyed");
    assert_ne!(
        other
            .module_cache
            .content_key(&id)
            .expect("%a has a stored artifact"),
        unit.imports[0].1,
        "the two %a versions must key differently, or this proves nothing"
    );

    let mut fresh = Program::new();
    let mut fresh_cache = ModuleCache::new();
    fresh_cache.artifact_store = Some(Rc::clone(&other_store));
    let artifact = other_store.load(key).expect("the other %a's artifact");
    quiver_compiler::link_module(&artifact, &id, &mut fresh, &mut fresh_cache, &builtins())
        .expect("link");
    fresh_cache.key_cache.insert(id, key);

    quiver_compiler::resolve_imports(&unit, &fresh_cache, "unit");
}

#[test]
fn a_units_imports_are_a_sparse_subset_of_its_modules() {
    // The server links whole modules, but the import table names only what the unit
    // reaches — which is what keeps it small.
    let compiled = compile("%num.mul [7, 6]", false);
    let unit = quiver_compiler::extract_unit(
        &compiled.program,
        &compiled.module_cache,
        Some(compiled.entry),
        compiled.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    let (module, key, indices) = unit
        .imports
        .iter()
        .find(|(id, _, _)| id.name == vec!["num".to_string()])
        .expect("imports %num");
    let artifact = store()
        .load(compiled.module_cache.key_cache[module])
        .expect("%num artifact");
    assert_eq!(artifact.id, *module);
    assert_eq!(
        artifact.content_key, *key,
        "the import names the stored unit's content"
    );
    assert!(
        indices.len() < artifact.unit.functions.len(),
        "expected a subset: {} of {}",
        indices.len(),
        artifact.unit.functions.len()
    );
    assert!(
        indices.iter().all(|i| *i < artifact.unit.functions.len()),
        "every index must be a position in the dependency's own functions"
    );
    assert!(
        indices.windows(2).all(|w| w[0] < w[1]),
        "indices are ascending, so the id space is reproducible"
    );
}

#[test]
fn a_compiled_program_is_self_contained() {
    // What a `.qx` holds: an entry unit plus the units of every module it imports,
    // transitively. Serialised, handed to a host that has nothing, it must link and run —
    // no store, no cache, no lookups.
    for (source, expected) in SOURCES {
        let compiled = compile(source, false);
        let program = quiver_compiler::extract_program(
            &compiled.program,
            &compiled.module_cache,
            Some(compiled.entry),
            compiled.own_floor,
            quiver_compiler::Imports::Bundle,
        );

        // The closure is complete and ordered: nothing a module imports is missing, and
        // nothing arrives before what it depends on.
        let mut linked: Vec<quiver_compiler::UnitKey> = Vec::new();
        for (key, unit) in &program.modules {
            for (_, dependency, _) in &unit.imports {
                assert!(
                    linked.contains(dependency),
                    "`{source}`: module {key} precedes its dependency {dependency}"
                );
            }
            linked.push(*key);
        }
        for (_, key, _) in &program.unit.imports {
            assert!(
                linked.contains(key),
                "`{source}`: the entry imports {key}, which is not carried"
            );
        }

        let bytes = serde_json::to_vec(&program).expect("serialize");
        let restored: quiver_compiler::CompiledProgram =
            serde_json::from_slice(&bytes).expect("deserialize");

        let mut fresh = Program::new();
        let entry = quiver_compiler::link_program(&restored, &mut fresh, &builtins())
            .unwrap_or_else(|e| panic!("link `{source}`: {e:?}"))
            .expect("a program compiled from an entry has one");
        assert_eq!(run(&fresh, entry), expected, "`{source}` diverged");
    }
}

#[test]
fn a_program_that_is_not_executable_still_extracts() {
    // `quiv compile` accepts a top level that does not evaluate to a function, so that
    // `quiv inspect` can still show it. The unit form of that is an entry of `None` over
    // everything the compile registered — not an empty unit.
    let compiled = compile("[1, 2] ~> __integer_add__", false);
    let unit = quiver_compiler::extract_unit(
        &compiled.program,
        &compiled.module_cache,
        None,
        compiled.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    assert!(unit.entry.is_none());
    assert!(!unit.constants.is_empty(), "the compile's content is there");

    let mut fresh = Program::new();
    let entry = quiver_compiler::link_program(
        &quiver_compiler::CompiledProgram {
            unit,
            modules: Vec::new(),
        },
        &mut fresh,
        &builtins(),
    )
    .expect("link");
    assert!(entry.is_none(), "nothing to enter at");
}

#[test]
#[ignore = "known gap: linked modules emit ~17% more code than source-compiled ones"]
fn cold_and_warm_module_loading_emit_the_same_code() {
    // Transparency holds for types (module_cache.rs) but NOT for emitted code. A line
    // compiled against `%list` built from source emits 140 instructions; against the
    // same `%list` linked from its artifact, 164.
    //
    // Cause: `emit_value_cse` shares repeated nodes by `Rc` pointer identity, and a
    // module value that has been through an artifact has lost its sharing, so the memo
    // sees distinct nodes where the source-compiled value had one. Semantics are
    // unaffected (every other test here passes either way); the cost is code size in
    // every consumer, on the path a warm cache always takes.
    //
    // Narrowed: source-compiled 140, extracted into an in-memory store 164, serialised
    // and reloaded 164. The loss is in the `Value::remap_ids` walk that moves the value
    // between session and unit space, which rebuilds each node rather than memoising on
    // pointer identity — serialisation adds nothing further, though it would flatten a
    // repaired graph again on its own. So a full fix is both: a memoised remap, and a
    // value *table* in the artifact (root index plus nodes) the way types and tuples
    // already work.
    //
    // Deliberately not fixed here: stage 2 needs exactly that representation anyway, to
    // ship module values to workers with sharing intact, so the three belong together.
    let source = "%list{1, 2, 3} ~> %list.map [~, #{ %num.mul [$, 2] }]";
    let cold = compile_with(source, false, Rc::new(ArtifactStore::in_memory()));
    let warm = compile(source, false);
    let entry_of = |c: &Compiled| c.program.get_functions()[c.entry].instructions.clone();
    assert_eq!(entry_of(&cold), entry_of(&warm));
}

#[test]
fn extraction_is_deterministic() {
    // Units are ephemeral, but the same canonical ordering artifacts use costs nothing
    // and keeps content-addressing them an option.
    // Both compiles must load their modules the same way — see the ignored
    // `cold_and_warm_module_loading_emit_the_same_code` for why that qualifier is needed.
    let source = "%list{1, 2, 3} ~> %list.map [~, #{ %num.mul [$, 2] }]";
    compile(source, false); // warm the store, so neither timed compile is the cold one
    let first = compile(source, false);
    let a = quiver_compiler::extract_unit(
        &first.program,
        &first.module_cache,
        Some(first.entry),
        first.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    let second = compile(source, false);
    let b = quiver_compiler::extract_unit(
        &second.program,
        &second.module_cache,
        Some(second.entry),
        second.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    assert_eq!(
        serde_json::to_vec(&a).expect("serialize"),
        serde_json::to_vec(&b).expect("serialize"),
    );
}

#[test]
fn a_unit_round_trips_through_serialization() {
    let compiled = compile("%str.from_int 99", false);
    let unit = quiver_compiler::extract_unit(
        &compiled.program,
        &compiled.module_cache,
        Some(compiled.entry),
        compiled.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    let bytes = serde_json::to_vec(&unit).expect("serialize");
    let restored: quiver_compiler::CompiledUnit =
        serde_json::from_slice(&bytes).expect("deserialize");
    assert_eq!(bytes, serde_json::to_vec(&restored).expect("re-serialize"));
    // Byte stability is what makes the content key canonical: the receiver's rehash of
    // a deserialized unit must equal the sender's hash of the original.
    assert_eq!(
        quiver_compiler::unit_key(&unit),
        quiver_compiler::unit_key(&restored)
    );

    let mut fresh = Program::new();
    let mut fresh_cache = ModuleCache::new();
    fresh_cache.artifact_store = Some(store());
    for (module, _, _) in &restored.imports {
        link_module_tree(
            module,
            &compiled.module_cache.key_cache,
            &mut fresh,
            &mut fresh_cache,
        );
    }
    let resolved = quiver_compiler::resolve_imports(&restored, &fresh_cache, "unit");
    let remaps = quiver_compiler::link_unit(
        &restored,
        "unit",
        &mut fresh,
        &resolved,
        &builtins(),
        Registration::Intern,
    )
    .expect("link");
    let entry = remaps.functions[&restored.entry.expect("entry")];
    assert_eq!(run(&fresh, entry), "\"99\"");
}

#[test]
fn debug_units_carry_only_the_sites_they_reference() {
    // Extraction walks `Stamp` into the site closure, so a unit ships its own sites
    // rather than the session's accumulated table.
    let compiled = compile("%num.mul [7, 6]", true);
    let unit = quiver_compiler::extract_unit(
        &compiled.program,
        &compiled.module_cache,
        Some(compiled.entry),
        compiled.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    let session_sites = compiled
        .program
        .debug_sites()
        .expect("debug build has a site table")
        .sites
        .len();
    assert!(unit.sites.len() < session_sites);
    assert!(
        unit.sites
            .iter()
            .all(|site| site.module_constant < unit.constants.len()),
        "each site's module name must be in the unit's own constants"
    );
}

/// A bare environment with workers, for the linking API.
fn environment() -> Environment<NativeEffect> {
    let builtins = builtins();
    let (waker, _wake) = quiver::native_transport::wake_channel();
    let workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = (0..2)
        .map(|i| {
            Box::new(quiver::spawn_worker(
                quiver::native_transport::SystemClock,
                builtins.clone(),
                i as u16,
                waker.clone(),
            )) as Box<_>
        })
        .collect();
    let mut environment = Environment::<NativeEffect>::new(workers);
    environment.set_runtime_declarations(builtins.runtime_declarations().clone());
    environment
}

/// Link `id`'s module unit and everything it depends on into `environment`, under its
/// content key. `keys` are the session's artifact (source) keys — the store's index.
fn link_into_environment(
    environment: &mut Environment<NativeEffect>,
    id: &ModuleId,
    keys: &HashMap<ModuleId, u64>,
) {
    let artifact = store().load(keys[id]).expect("artifact");
    if environment.holds_module(artifact.content_key) {
        return;
    }
    for dependency in artifact.unit.dependencies() {
        link_into_environment(environment, &dependency, keys);
    }
    environment
        .link_module_unit(artifact.content_key, &artifact.unit, &builtins())
        .unwrap_or_else(|e| panic!("link {}: {e}", id.display()));
}

#[test]
fn the_environment_links_module_units_from_requests_alone() {
    let compiled = compile("%num.mul [7, 6]", false);
    let unit = quiver_compiler::extract_unit(
        &compiled.program,
        &compiled.module_cache,
        Some(compiled.entry),
        compiled.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    let mut environment = environment();

    // Nothing is linked until a request supplies it: the environment has no store.
    let missing = environment.missing_modules(&unit);
    assert_eq!(missing.len(), unit.imports.len());
    assert!(
        missing
            .iter()
            .all(|(_, key)| !environment.holds_module(*key))
    );

    for (module, key, _) in &unit.imports {
        link_into_environment(&mut environment, module, &compiled.module_cache.key_cache);
        assert!(environment.holds_module(*key));
    }
    assert!(environment.missing_modules(&unit).is_empty());

    // Idempotent: re-linking a held key is a no-op, not a second copy.
    let functions = environment.get_program().get_functions().len();
    for (module, key, _) in &unit.imports {
        let artifact = store()
            .load(compiled.module_cache.key_cache[module])
            .expect("artifact");
        environment
            .link_module_unit(*key, &artifact.unit, &builtins())
            .expect("re-link");
    }
    assert_eq!(functions, environment.get_program().get_functions().len());
}

#[test]
fn a_sweep_evicts_linked_modules_and_a_re_link_revives_them() {
    // Nothing in this environment references the linked code, so a code sweep stubs it.
    // The map entry must go with it — otherwise the next unit importing that key would
    // resolve onto reclaimed stubs — and re-linking must revive the stubbed slots rather
    // than allocate beside them (spike 4, now against a real environment).
    let compiled = compile("%num.mul [7, 6]", false);
    let unit = quiver_compiler::extract_unit(
        &compiled.program,
        &compiled.module_cache,
        Some(compiled.entry),
        compiled.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    let keys = &compiled.module_cache.key_cache;
    let mut environment = environment();
    for (module, _, _) in &unit.imports {
        link_into_environment(&mut environment, module, keys);
    }
    let linked: Vec<quiver_compiler::UnitKey> =
        unit.imports.iter().map(|(_, key, _)| *key).collect();
    let table_before = environment.get_program().get_functions().len();

    loop {
        let started = environment.start_code_collection().expect("start sweep");
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
        while environment.is_collecting() {
            if !environment.step().unwrap_or(false) {
                assert!(std::time::Instant::now() < deadline, "sweep did not finish");
                std::thread::sleep(std::time::Duration::from_micros(10));
            }
        }
        if started {
            break;
        }
    }
    assert!(
        environment.code_reclaimed_totals().0 > 0,
        "the sweep must have stubbed the linked code nothing references"
    );
    assert!(
        linked.iter().all(|key| !environment.holds_module(*key)),
        "a sweep that stubs a module's code must drop its map entry"
    );
    assert_eq!(
        environment.missing_modules(&unit).len(),
        linked.len(),
        "and the next unit naming those keys must be told to send them again"
    );

    for (module, _, _) in &unit.imports {
        link_into_environment(&mut environment, module, keys);
    }
    assert_eq!(
        environment.get_program().get_functions().len(),
        table_before,
        "re-linking identical content must revive the stubbed slots, not add new ones"
    );
    assert!(linked.iter().all(|key| environment.holds_module(*key)));
}

#[test]
fn a_module_unit_that_does_not_match_its_key_is_refused() {
    // The trust boundary: the environment rehashes what it is sent, so neither
    // tampered content under an honest key (which would run as some other module's
    // bytes) nor an honest unit under a wrong key (which would poison the map for
    // every later session naming that key) can link. The check runs before import
    // resolution, so it needs no dependencies in place.
    let compiled = compile("%num.mul [7, 6]", false);
    let unit = quiver_compiler::extract_unit(
        &compiled.program,
        &compiled.module_cache,
        Some(compiled.entry),
        compiled.own_floor,
        quiver_compiler::Imports::Bundle,
    );
    let mut environment = environment();
    let (module, key, _) = &unit.imports[0];
    let artifact = store()
        .load(compiled.module_cache.key_cache[module])
        .expect("artifact");

    let mut tampered = artifact.unit.clone();
    tampered.field_names.push("tampered".to_string());
    let error = environment
        .link_module_unit(*key, &tampered, &builtins())
        .expect_err("tampered content must be refused");
    assert!(
        matches!(
            error,
            quiver_environment::EnvironmentError::InvalidUnit(ref message)
                if message.contains("hashes to")
        ),
        "got {error:?}"
    );
    assert!(!environment.holds_module(*key), "nothing may have linked");

    let wrong = quiver_compiler::unit_key(&tampered);
    let error = environment
        .link_module_unit(wrong, &artifact.unit, &builtins())
        .expect_err("a mismatched key must be refused");
    assert!(
        matches!(error, quiver_environment::EnvironmentError::InvalidUnit(_)),
        "got {error:?}"
    );
    assert!(!environment.holds_module(wrong));

    // The refusals left the environment serviceable: the honest tree still links.
    for (module, key, _) in &unit.imports {
        link_into_environment(&mut environment, module, &compiled.module_cache.key_cache);
        assert!(environment.holds_module(*key));
    }
}

#[test]
fn a_malformed_unit_is_refused_and_the_environment_survives() {
    // Structural validation turns what used to be a panic mid-link into an ordinary
    // error: the bad request is refused, the next one is served.
    let compiled = compile("%num.mul [7, 6]", false);
    let extract = || {
        quiver_compiler::extract_unit(
            &compiled.program,
            &compiled.module_cache,
            Some(compiled.entry),
            compiled.own_floor,
            quiver_compiler::Imports::Bundle,
        )
    };
    let mut environment = environment();

    // An entry outside the unit's own functions.
    let mut bad_entry = extract();
    bad_entry.entry = Some(bad_entry.functions.len());
    let error = environment
        .start_process_unit(&bad_entry, &builtins())
        .expect_err("an out-of-range entry must be refused");
    assert!(
        matches!(error, quiver_environment::EnvironmentError::InvalidUnit(_)),
        "got {error:?}"
    );

    // An import citing a held module at an index past its end — well-formed in
    // isolation, so only the link-time cross-check can catch it.
    for (module, _, _) in &extract().imports {
        link_into_environment(&mut environment, module, &compiled.module_cache.key_cache);
    }
    let mut bad_import = extract();
    bad_import.imports[0].2[0] = 9_999;
    let error = environment
        .start_process_unit(&bad_import, &builtins())
        .expect_err("an out-of-range import index must be refused");
    assert!(
        matches!(
            error,
            quiver_environment::EnvironmentError::InvalidUnit(ref message)
                if message.contains("9999")
        ),
        "got {error:?}"
    );

    // And an honest line still runs.
    let mut repl = repl_on(&mut environment, true);
    assert_eq!(
        evaluate(&mut environment, &mut repl, "%num.mul [7, 6]"),
        "Int(42)"
    );
}

fn repl_on(environment: &mut Environment<NativeEffect>, with_store: bool) -> Repl<NativeEffect> {
    let resolver = Box::new(PackageResolver::memory(HashMap::new()));
    let mut repl = Repl::new(environment, resolver, builtins()).expect("repl");
    if with_store {
        repl.set_artifact_store(store());
    }
    repl.set_compile_options(CompileOptions {
        debug: false,
        source_name: "repl".to_string(),
        ..Default::default()
    });
    repl
}

fn process_types(
    environment: &mut Environment<NativeEffect>,
) -> HashMap<usize, (quiver_core::types::Type, usize)> {
    let id = environment.request_process_types().expect("types");
    loop {
        environment.step().ok();
        if let Ok(Some(quiver_environment::RequestResult::ProcessTypes(t))) =
            environment.poll_request(id)
        {
            break t;
        }
    }
}

/// Run a line through the three-step path, so the payload it produced is observable.
/// Answers `(the payload imports its modules by key, the rendered result)` — a line
/// that inlined a module instead shows an empty import table.
fn evaluate_observing_payload(
    environment: &mut Environment<NativeEffect>,
    repl: &mut Repl<NativeEffect>,
    source: &str,
) -> (bool, String) {
    let types = process_types(environment);
    let prepared = repl
        .prepare(environment, source, types)
        .unwrap_or_else(|e| panic!("prepare `{source}`: {e}"));
    let compiled = repl
        .compile(prepared)
        .unwrap_or_else(|e| panic!("compile `{source}`: {e}"));
    let bundled = compiled
        .payload()
        .is_some_and(|payload| !payload.unit.imports.is_empty());
    let request = repl
        .commit(environment, compiled)
        .unwrap_or_else(|e| panic!("commit `{source}`: {e}"))
        .expect("a line with something to run");
    (bundled, await_result(environment, request, source))
}

fn evaluate(
    environment: &mut Environment<NativeEffect>,
    repl: &mut Repl<NativeEffect>,
    source: &str,
) -> String {
    let types = process_types(environment);
    let request = repl
        .evaluate(environment, source, types)
        .unwrap_or_else(|e| panic!("evaluate `{source}`: {e}"))
        .expect("a line with something to run");
    await_result(environment, request, source)
}

fn await_result(environment: &mut Environment<NativeEffect>, request: u64, source: &str) -> String {
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
fn the_in_process_repl_runs_lines_as_units() {
    // Phase 4's whole point: the driver that the web build and every test harness use
    // takes the unit path, so the extraction and linking contracts are exercised by
    // ordinary evaluation rather than only by these tests.
    let mut environment = environment();
    let mut repl = repl_on(&mut environment, true);

    let prepared = repl
        .prepare(&mut environment, "%num.mul [7, 6]", HashMap::new())
        .expect("prepare");
    let compiled = repl.compile(prepared).expect("compile");
    let payload = compiled.payload().expect("a payload");
    assert!(!payload.unit.imports.is_empty(), "the line imports %num");
    assert!(
        !payload.modules.is_empty(),
        "and must carry the modules for the host to link"
    );
    // Dependencies first: the host links in the order given.
    let mut linked: Vec<quiver_compiler::UnitKey> = Vec::new();
    for (key, artifact) in &payload.modules {
        for (_, dependency, _) in &artifact.unit.imports {
            assert!(
                linked.contains(dependency),
                "module {} sent before its dependency",
                artifact.id.display()
            );
        }
        linked.push(*key);
    }
    drop(compiled); // uncommitted: the session is untouched

    // And the results are the ordinary ones, across a session with bindings — including
    // a line that calls a function an *earlier* line defined, which is the case that
    // would fail if a line's own code ever escaped into another line's unit.
    for (source, expected) in [
        ("%num.mul [7, 6]", Some("Int(42)")),
        ("xs = %list{1, 2, 3}", None),
        (
            "xs ~> %list.fold [~, init: 0, f: &%num.add]",
            Some("Int(6)"),
        ),
        ("double = #'int { %num.mul [$, 2] }", None),
        ("double 21", Some("Int(42)")),
        ("double 21 ~> %num.add [~, 1]", Some("Int(43)")),
        ("%str.from_int 99", None),
    ] {
        // `evaluate` already fails the test on a runtime error, so a `None` expectation
        // asserts "ran cleanly" — which is the whole claim for a binding.
        let rendered = evaluate(&mut environment, &mut repl, source);
        if let Some(expected) = expected {
            assert!(
                rendered.contains(expected),
                "`{source}` gave {rendered}, expected {expected}"
            );
        }
    }
}

#[test]
fn a_repeated_line_keeps_taking_the_unit_path() {
    // The steady state stage 1 exists for: the same line, over and over, shipping only
    // its own code. Its wrapper function is identical every time, so without a dedup
    // floor at the line boundary the third one interns onto the second's id — below the
    // extraction floor, where it reads as an escaped reference. A line reached through
    // a *binding* is the same shape one step removed, so it is covered here too.
    let mut environment = environment();
    let mut repl = repl_on(&mut environment, true);

    evaluate(
        &mut environment,
        &mut repl,
        "double = #'int { %num.mul [$, 2] }",
    );
    for round in 0..4 {
        // The direct reference must keep naming %num by key; the bound call reaches
        // the earlier line's code through a session local, so its unit is legitimately
        // import-free. Either way, a wrapper interned below the extraction floor now
        // panics in `extract_unit`, which is what locks the dedup-floor regression.
        for (source, bundles) in [("%num.mul [7, 6]", true), ("double 21", false)] {
            let (bundled, rendered) =
                evaluate_observing_payload(&mut environment, &mut repl, source);
            assert_eq!(
                bundled, bundles,
                "round {round} of `{source}`: expected bundles={bundles}"
            );
            assert!(rendered.contains("Int(42)"), "`{source}` gave {rendered}");
        }
    }
}

#[test]
fn a_driver_without_artifacts_inlines_its_modules() {
    // No store means no artifacts, so nothing can name a version of the modules the line
    // reaches: extraction claims their functions as the line's own instead, and the
    // payload is a self-contained unit. The line must still run.
    let mut environment = environment();
    let mut repl = repl_on(&mut environment, false);
    let prepared = repl
        .prepare(&mut environment, "%num.mul [7, 6]", HashMap::new())
        .expect("prepare");
    let compiled = repl.compile(prepared).expect("compile");
    let payload = compiled.payload().expect("a payload");
    assert!(
        payload.unit.imports.is_empty(),
        "nothing can be named, so nothing may be imported"
    );
    assert!(payload.modules.is_empty());
    assert!(
        !payload.unit.functions.is_empty(),
        "the module code rides in the unit itself"
    );
    drop(compiled);
    assert_eq!(
        evaluate(&mut environment, &mut repl, "%num.mul [7, 6]"),
        "Int(42)"
    );
}
