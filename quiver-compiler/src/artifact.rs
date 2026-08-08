//! Per-module compiled artifacts, their content-addressed store, and the linker.
//!
//! Every module load goes through one pipeline: resolve the module, compute its
//! **artifact key**, and either **link** the stored artifact for that key into the
//! session or **compile from source** — in which case the compiler records the
//! module's layout and extracts a fresh artifact into the store. The key is a
//! Merkle-style hash over everything that determines compiled output — the compiler
//! build fingerprint, the debug flag, the module's source, and the keys of every
//! module it references (value imports, dialects, *and* type-level references) — so
//! a store hit is valid by construction and invalidation is per-module.
//!
//! A [`ModuleArtifact`] is a compiled module in a self-contained, artifact-local id
//! space. Value-like entities — types, tuples, constants, annotation keys, field
//! names — dedup structurally, so the artifact carries *copies* of everything it
//! references (transitively closed) and the linker re-interns them into a session's
//! program. Only functions and builtins are nominal: the artifact's function id
//! space is its own functions, then its imports — a table naming `(module, that
//! module's own index)`, grouped by module, every named module lying in the
//! artifact's transitive value-import closure and hence covered by its key — and
//! builtins are named strings resolved against the host registry at link time (which
//! is where capability checking happens: a host that doesn't provide a builtin
//! refuses to link a module requiring it).
//!
//! Linking a module re-interns its tables, registers its functions, and installs its
//! cached value, dispatch tables and type namespace — leaving the session exactly as
//! if the module had been compiled from source (the transparency invariant;
//! `quiver-tests/tests/module_cache.rs`). Its dependencies must already be in the
//! session: the import pipeline ensures them (linked or source-compiled, both record
//! the function map) before linking, so a missing map is a pipeline bug and panics.

use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::compiler::{
    Bindings, CachedModule, CompileOptions, Compiler, Error, ModuleCache, ModuleTypeNamespace,
    TypeAliasDef, collect_module_references,
};
use crate::resolver::{ModuleId, ModuleResolver, PackageId, PackageResolver, std_module_names};
use quiver_core::builtins::BuiltinRegistry;
use quiver_core::bytecode::{Function, IdRemaps, Instruction, Opcode, Site};
use quiver_core::effects::Effect;
use quiver_core::program::{Constant, Program};
use quiver_core::types::{TupleTypeInfo, Type};
use quiver_core::value::Value;

/// The hash of every source that determines the compiler's behaviour (this crate and
/// quiver-core) — one component of every artifact key. Emitted by the build script.
/// Standard-library sources are *not* included: std modules are content-addressed
/// individually, like any other module.
pub fn compiler_fingerprint() -> &'static str {
    env!("QUIVER_COMPILER_FINGERPRINT")
}

/// A compiled module in artifact-local id space. Pure canonical data: every
/// collection is a vector in deterministic order, so equal modules serialize to
/// equal bytes.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ModuleArtifact {
    pub id: ModuleId,
    /// Value-like tables. References within them are artifact-local and point
    /// strictly downward (the program's registration order is topological, and
    /// extraction preserves it).
    pub types: Vec<Type>,
    pub tuples: Vec<TupleTypeInfo>,
    pub constants: Vec<Constant>,
    pub annotation_keys: Vec<String>,
    pub field_names: Vec<String>,
    /// `(artifact tuple id, field index)` pairs marked label-omittable.
    pub omittable_labels: Vec<(usize, usize)>,
    /// The functions the module's own compile registered, in registration order —
    /// artifact function ids `0..functions.len()`, the positions other modules'
    /// import entries reference. Identical in every session: the compiler's dedup
    /// floor keeps a module's functions its own even when structurally identical to
    /// another module's.
    pub functions: Vec<Function>,
    /// Imported functions, grouped by module and continuing the id space after
    /// [`Self::functions`]: groups in `ModuleId` order, entries (the *dependency's*
    /// own-function indices) ascending — artifact function ids assigned in that
    /// flattened order.
    pub imports: Vec<(ModuleId, Vec<usize>)>,
    /// Referenced builtins; artifact builtin ids index this list. Nominal — resolved
    /// against the host registry at link time — except for an instantiated
    /// type-consuming builtin, whose type argument is artifact-local and re-interned
    /// with the rest.
    pub builtins: Vec<ArtifactBuiltin>,
    /// Failure-provenance sites (debug artifacts; empty in release ones).
    pub sites: Vec<Site>,
    /// The evaluated module value, in artifact space.
    pub value: Value,
    pub module_type: Type,
    /// Return-type dispatch tables (see `CachedModule`), in artifact space:
    /// function id → `(guard type, result type)` branches.
    pub fn_case_tables: Vec<(usize, Vec<(usize, usize)>)>,
    /// Callable type id → dispatch function id.
    pub case_tables: Vec<(usize, usize)>,
    /// Callable *type* id → declared type-parameter names (for explicit
    /// instantiation of imported generics).
    pub callable_type_params: Vec<(usize, Vec<String>)>,
    /// The module's type namespace (its `'name = …` aliases and nameless default).
    pub namespace: ArtifactNamespace,
}

/// A module's type namespace in artifact form: each alias as its declared type
/// parameters plus an artifact-local type id. `named` is sorted by name.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ArtifactNamespace {
    pub default: Option<AliasEntry>,
    pub named: Vec<(String, AliasEntry)>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AliasEntry {
    pub parameters: Vec<String>,
    pub type_id: usize,
}

/// One referenced builtin: its registry name, plus the artifact-local type argument that
/// distinguishes an instantiated type-consuming builtin (`__data_decode__<'t>`) from the
/// bare one. The pair is what the linker registers, so distinct instantiations stay
/// distinct entries in the session.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ArtifactBuiltin {
    pub name: String,
    pub type_argument: Option<usize>,
}

impl ModuleArtifact {
    /// The modules this artifact's function imports name, in group order.
    pub fn dependencies(&self) -> Vec<ModuleId> {
        self.imports
            .iter()
            .map(|(module, _)| module.clone())
            .collect()
    }

    /// The size of the artifact's function id space (own + imports).
    fn function_space(&self) -> usize {
        self.functions.len()
            + self
                .imports
                .iter()
                .map(|(_, indices)| indices.len())
                .sum::<usize>()
    }
}

/// A content-addressed store of module artifacts: an in-memory map over an optional
/// cache directory, probed lazily per key. There is nothing to "load" up front and
/// no attach ceremony — the store is always ready, and a missing entry simply means
/// the module compiles from source (which then saves its artifact here).
pub struct ArtifactStore {
    dir: Option<PathBuf>,
    /// `RefCell`, not `RwLock`: an artifact holds a compile-time `Value`, whose payload is
    /// `Rc`, so a store never crosses a thread and has nothing to lock against. Sharing
    /// between threads (and between sessions and runs) happens through the content-addressed
    /// cache *directory*, which is what made artifacts shareable in the first place.
    memory: RefCell<HashMap<u64, Rc<ModuleArtifact>>>,
}

impl ArtifactStore {
    /// The user-level store, backed by `$XDG_CACHE_HOME/quiver/artifacts` (fallback
    /// `~/.cache/quiver/artifacts`). Memory-only when no cache directory is usable.
    /// Prunes entries untouched for [`Self::MAX_AGE`] — content-addressed files can't
    /// be invalidated by name, so age is the only signal a key is dead — and sweeps
    /// the legacy bundle caches (`std-image-*` / `std-artifacts-*`) this store
    /// replaced.
    pub fn cache() -> Self {
        let dir = std::env::var_os("XDG_CACHE_HOME")
            .map(PathBuf::from)
            .or_else(|| std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".cache")))
            .map(|base| base.join("quiver").join("artifacts"))
            .filter(|dir| std::fs::create_dir_all(dir).is_ok());
        if let Some(dir) = &dir {
            prune(dir);
        }
        Self {
            dir,
            memory: RefCell::new(HashMap::new()),
        }
    }

    /// A store with no disk backing (tests, wasm hosts without a filesystem).
    pub fn in_memory() -> Self {
        Self {
            dir: None,
            memory: RefCell::new(HashMap::new()),
        }
    }

    /// A store backed by an explicit directory (embedders with their own cache
    /// layout). No pruning: the directory is the caller's to manage.
    pub fn at_dir(dir: PathBuf) -> Self {
        Self {
            dir: Some(dir).filter(|dir| std::fs::create_dir_all(dir).is_ok()),
            memory: RefCell::new(HashMap::new()),
        }
    }

    const MAX_AGE: std::time::Duration = std::time::Duration::from_secs(30 * 24 * 60 * 60);

    pub fn load(&self, key: u64) -> Option<Rc<ModuleArtifact>> {
        if let Some(artifact) = self.memory.borrow().get(&key) {
            return Some(artifact.clone());
        }
        let bytes = std::fs::read(self.path(key)?).ok()?;
        let artifact: ModuleArtifact = serde_json::from_slice(&bytes).ok()?;
        let artifact = Rc::new(artifact);
        self.memory.borrow_mut().insert(key, artifact.clone());
        Some(artifact)
    }

    /// Insert an artifact under its key, persisting it best-effort (write-then-rename
    /// with a per-process temp name, so concurrent writers never tear a file). Equal
    /// keys always carry equal content, so an existing file is left untouched.
    pub fn save(&self, key: u64, artifact: ModuleArtifact) {
        let artifact = Rc::new(artifact);
        self.memory.borrow_mut().insert(key, artifact.clone());
        if let Some(path) = self.path(key)
            && !path.exists()
            && let Ok(bytes) = serde_json::to_vec(&*artifact)
        {
            let tmp = path.with_extension(format!("tmp{}", std::process::id()));
            if std::fs::write(&tmp, bytes).is_ok() {
                let _ = std::fs::rename(&tmp, &path);
            }
        }
    }

    /// Every artifact currently in memory, sorted by key (a test hook; disk entries
    /// appear only once loaded).
    pub fn entries(&self) -> Vec<(u64, Rc<ModuleArtifact>)> {
        let mut entries: Vec<(u64, Rc<ModuleArtifact>)> = self
            .memory
            .borrow()
            .iter()
            .map(|(key, artifact)| (*key, artifact.clone()))
            .collect();
        entries.sort_by_key(|(key, _)| *key);
        entries
    }

    fn path(&self, key: u64) -> Option<PathBuf> {
        self.dir
            .as_ref()
            .map(|dir| dir.join(format!("{key:016x}.json")))
    }
}

fn prune(dir: &Path) {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let stale = entry
                .metadata()
                .and_then(|meta| meta.modified())
                .ok()
                .and_then(|modified| modified.elapsed().ok())
                .is_some_and(|age| age > ArtifactStore::MAX_AGE);
            if stale {
                let _ = std::fs::remove_file(entry.path());
            }
        }
    }
    // The pre-content-addressing bundle caches lived one level up.
    if let Some(parent) = dir.parent()
        && let Ok(entries) = std::fs::read_dir(parent)
    {
        for entry in entries.flatten() {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if (name.starts_with("std-image-") || name.starts_with("std-artifacts-"))
                && name.ends_with(".json")
            {
                let _ = std::fs::remove_file(entry.path());
            }
        }
    }
}

/// The artifact key of the module at `path` (resolved from `from_package`): a
/// Merkle-style hash of the compiler fingerprint, the debug flag, the module's
/// identity and source, and — recursively — the keys of every module it references.
/// `None` when the module or a reference fails to resolve or parse, or the reference
/// graph is cyclic; the compile path then reports the real error.
pub fn module_key(
    resolver: &dyn ModuleResolver,
    module_cache: &mut ModuleCache,
    from_package: &PackageId,
    path: &[String],
    debug: bool,
) -> Option<u64> {
    let resolved = resolver.resolve(from_package, path).ok()?;
    key_for_resolved(resolver, module_cache, &resolved, debug)
}

pub(crate) fn key_for_resolved(
    resolver: &dyn ModuleResolver,
    module_cache: &mut ModuleCache,
    resolved: &crate::resolver::ResolvedModule,
    debug: bool,
) -> Option<u64> {
    if let Some(&key) = module_cache.key_cache.get(&resolved.id) {
        return Some(key);
    }
    if module_cache.key_stack.contains(&resolved.id) {
        return None;
    }
    let parsed = module_cache
        .load_and_cache_ast(&resolved.id, &resolved.source)
        .ok()?;
    let references = collect_module_references(&parsed);

    let mut hasher = DefaultHasher::new();
    compiler_fingerprint().hash(&mut hasher);
    debug.hash(&mut hasher);
    resolved.id.hash(&mut hasher);
    resolved.source.hash(&mut hasher);

    module_cache.key_stack.push(resolved.id.clone());
    let mut resolvable = true;
    for (path, _) in &references {
        match module_key(resolver, module_cache, &resolved.package, path, debug) {
            Some(key) => {
                path.hash(&mut hasher);
                key.hash(&mut hasher);
            }
            None => {
                resolvable = false;
                break;
            }
        }
    }
    module_cache.key_stack.pop();
    if !resolvable {
        return None;
    }

    let key = hasher.finish();
    module_cache.key_cache.insert(resolved.id.clone(), key);
    Some(key)
}

/// Compile (or link) every standard-library module through the ordinary import
/// pipeline against `store`, so the store ends up holding an artifact for each.
/// A cache-warming convenience — background threads, test harnesses — with no
/// session side effects; the scratch session is discarded. Panics on a std compile
/// failure (a build invariant).
pub fn warm_std_store<E: Effect>(
    store: &Rc<ArtifactStore>,
    builtins: &BuiltinRegistry<E>,
    options: CompileOptions,
) {
    let source: String = std_module_names()
        .iter()
        .enumerate()
        .map(|(index, name)| format!("m{} = %{}\n", index, name))
        .collect();
    let parsed = crate::parse(&source).expect("std import line must parse");

    let resolver = PackageResolver::memory(HashMap::new());
    let mut program = Program::new();
    let mut module_cache = ModuleCache::new();
    module_cache.artifact_store = Some(store.clone());
    let nil_type_id = program.register_type(Type::nil());
    Compiler::compile(
        parsed,
        &Bindings::default(),
        Default::default(),
        &mut module_cache,
        &resolver,
        &mut program,
        nil_type_id,
        &HashMap::new(),
        builtins,
        None,
        options,
    )
    .unwrap_or_else(|e| panic!("standard library must compile: {:?}", e.error));
}

/// An insertion-ordered id set. Artifact-local ids are assigned by *first reach* in
/// the canonical traversal, so table order derives from module content alone —
/// session ids (which vary with compile history) never leak into artifact bytes.
#[derive(Default)]
struct OrderedSet {
    order: Vec<usize>,
    seen: HashSet<usize>,
}

impl OrderedSet {
    fn insert(&mut self, id: usize) -> bool {
        if self.seen.insert(id) {
            self.order.push(id);
            true
        } else {
            false
        }
    }

    fn iter(&self) -> std::slice::Iter<'_, usize> {
        self.order.iter()
    }

    fn position(&self, id: usize) -> Option<usize> {
        self.order.iter().position(|&candidate| candidate == id)
    }

    /// The global→local remap: each id to its first-reach position.
    fn index(&self) -> HashMap<usize, usize> {
        self.order
            .iter()
            .enumerate()
            .map(|(local, &global)| (global, local))
            .collect()
    }
}

/// The transitive id closure of one module's artifact, per registry, in canonical
/// first-reach order.
#[derive(Default)]
struct Closure {
    types: OrderedSet,
    tuples: OrderedSet,
    constants: OrderedSet,
    annotation_keys: OrderedSet,
    field_names: OrderedSet,
    functions: OrderedSet,
    builtins: OrderedSet,
    sites: OrderedSet,
}

/// How a session function id relates to the module being extracted.
enum FunctionClass {
    /// A function the module's own compile registered (the dedup floor guarantees
    /// these are attributed to the module even when structurally identical to
    /// another module's — see `Program::register_function`).
    Own,
    /// A function of a module in the transitive value-import closure: `(module, its
    /// own index)`. Closure membership is what makes the entry sound — the owner is
    /// covered by the module's Merkle key (so the index can never go stale) and is
    /// always linked first (so it can never dangle).
    Import(ModuleId, usize),
    /// A function of a module *outside* the closure — reachable only through a
    /// hidden, key-invisible chain. Nothing sound can be written for it (an import
    /// would escape the key; an embedded body copy would go stale the same way), so
    /// the module is uncacheable.
    Foreign,
}

fn classify(
    function_id: usize,
    id: &ModuleId,
    eligible: &HashSet<ModuleId>,
    module_cache: &ModuleCache,
) -> FunctionClass {
    match module_cache.function_owners.get(&function_id) {
        Some((owner, _)) if owner == id => FunctionClass::Own,
        Some((owner, index)) if eligible.contains(owner) => {
            FunctionClass::Import(owner.clone(), *index)
        }
        _ => FunctionClass::Foreign,
    }
}

/// Canonical ordering key for a type appearing as a dispatch-table key: its
/// first-reach position when already traversed, else a structural fingerprint.
type TypeSortKey = (u8, usize, u64);

/// Canonical ordering key for functions appearing as dispatch-table keys, whose map
/// keys are session ids (history-dependent) and so cannot order anything themselves.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum FnSortKey {
    Own(usize),
    Import(ModuleId, usize),
}

/// The module references a function nothing sound can be written for — it is
/// uncacheable (see [`FunctionClass::Foreign`]).
struct Uncacheable;

/// Chase the queued items to a fixpoint (see the traversal in [`extract`]).
fn drain(
    queue: &mut Vec<Item>,
    closure: &mut Closure,
    id: &ModuleId,
    eligible: &HashSet<ModuleId>,
    program: &Program,
    module_cache: &ModuleCache,
) -> Result<(), Uncacheable> {
    while let Some(item) = queue.pop() {
        match item {
            Item::Type(type_id) => {
                let ty = program
                    .get_types()
                    .get(type_id)
                    .expect("type id out of range");
                collect_type_children(ty, closure, queue);
            }
            Item::Tuple(tuple_id) => {
                let info = program
                    .get_tuples()
                    .get(tuple_id)
                    .expect("tuple id out of range");
                for (_, field_type) in &info.fields {
                    add_type(*field_type, closure, queue);
                }
            }
            Item::Function(function_id) => {
                match classify(function_id, id, eligible, module_cache) {
                    FunctionClass::Own => {
                        let function = &program.get_functions()[function_id];
                        add_type(function.type_id, closure, queue);
                        for instruction in &function.instructions {
                            collect_instruction(instruction, closure, queue);
                        }
                    }
                    FunctionClass::Import(..) => {}
                    FunctionClass::Foreign => return Err(Uncacheable),
                }
            }
            Item::Site(site_id) => {
                let site = &program
                    .debug_sites()
                    .expect("stamped function without a site table")
                    .sites[site_id];
                closure.constants.insert(site.module_constant);
            }
            Item::Builtin(builtin_id) => {
                if let Some(type_argument) = program.get_builtins()[builtin_id].type_argument {
                    add_type(type_argument, closure, queue);
                }
            }
        }
    }
    Ok(())
}

/// A structural content hash of a type, independent of session id assignment. Used
/// only to order dispatch-table entries whose key type nothing else reached — the
/// one place with no canonical first-reach position to sort by. Table references
/// form a DAG (recursion is expressed by relative `Cycle` markers), so plain
/// recursion terminates.
fn fingerprint_type(type_id: usize, program: &Program, memo: &mut HashMap<usize, u64>) -> u64 {
    if let Some(&hash) = memo.get(&type_id) {
        return hash;
    }
    let mut hasher = DefaultHasher::new();
    match &program.get_types()[type_id] {
        Type::Integer => 0u8.hash(&mut hasher),
        Type::Binary => 1u8.hash(&mut hasher),
        Type::Reference => 2u8.hash(&mut hasher),
        Type::Cycle(depth) => {
            3u8.hash(&mut hasher);
            depth.hash(&mut hasher);
        }
        Type::Resource(name) => {
            4u8.hash(&mut hasher);
            name.hash(&mut hasher);
        }
        Type::Variable(name) => {
            5u8.hash(&mut hasher);
            name.hash(&mut hasher);
        }
        Type::Tuple(tuple_id) => {
            6u8.hash(&mut hasher);
            let info = &program.get_tuples()[*tuple_id];
            info.name.hash(&mut hasher);
            for (name, field_type) in &info.fields {
                name.hash(&mut hasher);
                fingerprint_type(*field_type, program, memo).hash(&mut hasher);
            }
        }
        Type::Partial { name, fields } => {
            7u8.hash(&mut hasher);
            name.hash(&mut hasher);
            for (field_name, field_type) in fields {
                field_name.hash(&mut hasher);
                fingerprint_type(*field_type, program, memo).hash(&mut hasher);
            }
        }
        Type::Callable {
            parameter,
            result,
            receive,
            states,
        } => {
            8u8.hash(&mut hasher);
            fingerprint_type(*parameter, program, memo).hash(&mut hasher);
            fingerprint_type(*result, program, memo).hash(&mut hasher);
            fingerprint_type(*receive, program, memo).hash(&mut hasher);
            states
                .map(|states| fingerprint_type(states, program, memo))
                .hash(&mut hasher);
        }
        Type::Union(members) => {
            9u8.hash(&mut hasher);
            for member in members {
                fingerprint_type(*member, program, memo).hash(&mut hasher);
            }
        }
        Type::Annotated {
            base,
            exact,
            entries,
        } => {
            10u8.hash(&mut hasher);
            fingerprint_type(*base, program, memo).hash(&mut hasher);
            exact.hash(&mut hasher);
            // Entries are sorted by session key id; re-key by name for stability.
            let mut named: Vec<(&str, u64)> = entries
                .iter()
                .map(|(key, value)| {
                    (
                        program.get_annotation_keys()[*key].as_str(),
                        fingerprint_type(*value, program, memo),
                    )
                })
                .collect();
            named.sort();
            named.hash(&mut hasher);
        }
        Type::Process {
            send,
            receive,
            state,
        } => {
            11u8.hash(&mut hasher);
            for part in [send, receive, state] {
                part.map(|part| fingerprint_type(part, program, memo))
                    .hash(&mut hasher);
            }
        }
    }
    let hash = hasher.finish();
    memo.insert(type_id, hash);
    hash
}

/// Extract the artifact of a module just compiled from source: the transitive
/// closure of its value-like entities, its own functions, an import table over its
/// value-import closure, and its cached value, dispatch tables and type namespace.
/// The module's functions, value closure and namespace must already be recorded in
/// `module_cache`. `None` when the module is uncacheable — it references a function
/// of a module outside its value closure (see [`FunctionClass::Foreign`]).
pub(crate) fn extract(
    id: &ModuleId,
    program: &Program,
    module_cache: &ModuleCache,
) -> Option<ModuleArtifact> {
    let cached = module_cache
        .get_cached_module(id)
        .expect("module must be cached before extraction");
    let namespace = module_cache
        .type_namespace_cache
        .get(id)
        .expect("module namespace must be built before extraction");
    let eligible = module_cache
        .value_closures
        .get(id)
        .expect("module value closure must be recorded before extraction");

    // Seed the closure from the module's roots and chase type/tuple/site/own-
    // function references to a fixpoint. Imported functions and builtins are
    // recorded but never expanded — their contents belong to their own artifacts
    // (or the host). First-reach order is the artifact's table order, so every root
    // group iterates in content-derived order and drains fully before the next:
    // nothing here may key on session ids.
    let mut closure = Closure::default();
    let mut queue: Vec<Item> = Vec::new();

    // 1. Own functions, registration order; each drains before the next so its
    // reachable entities are numbered before a later function's.
    for &function_id in &module_cache.module_functions[id] {
        add_function(function_id, &mut closure, &mut queue);
        drain(
            &mut queue,
            &mut closure,
            id,
            eligible,
            program,
            module_cache,
        )
        .ok()?;
    }
    // 2. The module value, then its type.
    collect_value(&cached.value, &mut closure, &mut queue);
    drain(
        &mut queue,
        &mut closure,
        id,
        eligible,
        program,
        module_cache,
    )
    .ok()?;
    collect_type(&cached.module_type, &mut closure, &mut queue);
    drain(
        &mut queue,
        &mut closure,
        id,
        eligible,
        program,
        module_cache,
    )
    .ok()?;
    // 3. The namespace: default alias, then named aliases by name.
    let mut named_aliases: Vec<(&String, &TypeAliasDef)> = namespace.named.iter().collect();
    named_aliases.sort_by_key(|(name, _)| (*name).clone());
    for def in namespace
        .default
        .iter()
        .chain(named_aliases.iter().map(|(_, def)| *def))
    {
        add_type(def.type_id, &mut closure, &mut queue);
        drain(
            &mut queue,
            &mut closure,
            id,
            eligible,
            program,
            module_cache,
        )
        .ok()?;
    }
    // 4. Dispatch tables. Their map keys are session ids, so entries order by the
    // canonical function classification — and, for the type-keyed tables, by the
    // type's first-reach position (or a structural fingerprint for a type nothing
    // else reached; sort keys snapshot before any entry's traversal mutates state).
    let fn_key = |function_id: usize| -> FnSortKey {
        match classify(function_id, id, eligible, module_cache) {
            FunctionClass::Own => FnSortKey::Own(module_cache.function_owners[&function_id].1),
            FunctionClass::Import(module, index) => FnSortKey::Import(module, index),
            // The delta capture filters foreign-owned dispatch entries.
            FunctionClass::Foreign => {
                panic!("dispatch table references a foreign function")
            }
        }
    };
    let mut fingerprints: HashMap<usize, u64> = HashMap::new();
    let mut type_key = |type_id: usize, closure: &Closure| -> TypeSortKey {
        match closure.types.position(type_id) {
            Some(position) => (0, position, 0),
            None => (1, 0, fingerprint_type(type_id, program, &mut fingerprints)),
        }
    };
    let mut fn_dispatch: Vec<(&usize, &Vec<(usize, usize)>)> =
        cached.fn_case_tables.iter().collect();
    fn_dispatch.sort_by_cached_key(|(function_id, _)| fn_key(**function_id));
    for (function_id, branches) in fn_dispatch {
        add_function(*function_id, &mut closure, &mut queue);
        for (guard, result) in branches {
            add_type(*guard, &mut closure, &mut queue);
            add_type(*result, &mut closure, &mut queue);
        }
        drain(
            &mut queue,
            &mut closure,
            id,
            eligible,
            program,
            module_cache,
        )
        .ok()?;
    }
    type CaseEntry = (usize, usize, FnSortKey, TypeSortKey);
    let mut case_entries: Vec<CaseEntry> = cached
        .case_tables
        .iter()
        .map(|(type_id, function_id)| {
            (
                *type_id,
                *function_id,
                fn_key(*function_id),
                type_key(*type_id, &closure),
            )
        })
        .collect();
    case_entries.sort_by(|a, b| (&a.2, &a.3).cmp(&(&b.2, &b.3)));
    for (type_id, function_id, ..) in case_entries {
        add_type(type_id, &mut closure, &mut queue);
        add_function(function_id, &mut closure, &mut queue);
        drain(
            &mut queue,
            &mut closure,
            id,
            eligible,
            program,
            module_cache,
        )
        .ok()?;
    }
    let mut param_keys: Vec<(usize, TypeSortKey)> = cached
        .callable_type_params
        .keys()
        .map(|type_id| (*type_id, type_key(*type_id, &closure)))
        .collect();
    param_keys.sort_by_key(|(_, key)| *key);
    for (type_id, _) in param_keys {
        add_type(type_id, &mut closure, &mut queue);
        drain(
            &mut queue,
            &mut closure,
            id,
            eligible,
            program,
            module_cache,
        )
        .ok()?;
    }

    // Partition the function closure. Own functions take artifact ids by
    // registration order (the session-invariant `module_functions` positions import
    // entries reference — not first-reach order, since a later own function can be
    // reached from an earlier one's body); imports extend the id space in canonical
    // (module id, dependency index) order.
    let own_ids: Vec<usize> = module_cache.module_functions[id].clone();
    let mut import_entries: Vec<(ModuleId, usize, usize)> = Vec::new();
    for &function_id in closure.functions.iter() {
        match classify(function_id, id, eligible, module_cache) {
            FunctionClass::Own => {}
            FunctionClass::Import(module, index) => {
                import_entries.push((module, index, function_id));
            }
            FunctionClass::Foreign => unreachable!("foreign functions abort the traversal"),
        }
    }
    import_entries.sort();
    let mut imports: Vec<(ModuleId, Vec<usize>)> = Vec::new();
    for (module, dep_index, _) in &import_entries {
        match imports.last_mut() {
            Some((last, indices)) if last == module => indices.push(*dep_index),
            _ => imports.push((module.clone(), vec![*dep_index])),
        }
    }

    // Assign artifact-local ids in ascending global order (preserving topological
    // ordering within each table) and build the global→local remap.
    let mut remaps = IdRemaps {
        types: closure.types.index(),
        tuples: closure.tuples.index(),
        constants: closure.constants.index(),
        annotation_keys: closure.annotation_keys.index(),
        field_names: closure.field_names.index(),
        sites: closure.sites.index(),
        ..IdRemaps::default()
    };
    for (position, &function_id) in own_ids.iter().enumerate() {
        remaps.functions.insert(function_id, position);
    }
    let import_base = own_ids.len();
    for (position, &(_, _, function_id)) in import_entries.iter().enumerate() {
        remaps.functions.insert(function_id, import_base + position);
    }

    let builtin_names: Vec<ArtifactBuiltin> = closure
        .builtins
        .iter()
        .map(|&builtin_id| {
            let info = &program.get_builtins()[builtin_id];
            ArtifactBuiltin {
                name: info.name.clone(),
                type_argument: info.type_argument.map(|id| remaps.types[&id]),
            }
        })
        .collect();
    remaps.builtins = closure.builtins.index();

    let flatten_def = |def: &TypeAliasDef| -> AliasEntry {
        AliasEntry {
            parameters: def.parameters.clone(),
            type_id: *remaps
                .types
                .get(&def.type_id)
                .expect("namespace type in closure"),
        }
    };

    let artifact = ModuleArtifact {
        id: id.clone(),
        types: closure
            .types
            .iter()
            .map(|&type_id| program.get_types()[type_id].remap_ids(&remaps))
            .collect(),
        tuples: closure
            .tuples
            .iter()
            .map(|&tuple_id| program.get_tuples()[tuple_id].remap_ids(&remaps))
            .collect(),
        constants: closure
            .constants
            .iter()
            .map(|&constant_id| program.get_constants()[constant_id].clone())
            .collect(),
        annotation_keys: closure
            .annotation_keys
            .iter()
            .map(|&key| program.get_annotation_keys()[key].clone())
            .collect(),
        field_names: closure
            .field_names
            .iter()
            .map(|&name_id| program.get_field_names()[name_id].clone())
            .collect(),
        omittable_labels: closure
            .tuples
            .iter()
            .flat_map(|&tuple_id| {
                let local = remaps.tuples[&tuple_id];
                let field_count = program.get_tuples()[tuple_id].fields.len();
                (0..field_count)
                    .filter(move |&field| {
                        quiver_core::types::TypeLookup::label_omittable(program, tuple_id, field)
                    })
                    .map(move |field| (local, field))
            })
            .collect(),
        functions: own_ids
            .iter()
            .map(|&function_id| {
                program.get_functions()[function_id]
                    .clone()
                    .remap_ids(&remaps)
            })
            .collect(),
        imports,
        builtins: builtin_names,
        sites: closure
            .sites
            .iter()
            .map(|&site_id| {
                let site = &program.debug_sites().expect("site table").sites[site_id];
                Site {
                    module_constant: remaps.constants[&site.module_constant],
                    ..site.clone()
                }
            })
            .collect(),
        value: cached.value.remap_ids(&remaps),
        module_type: cached.module_type.remap_ids(&remaps),
        fn_case_tables: {
            let mut entries: Vec<(usize, Vec<(usize, usize)>)> = cached
                .fn_case_tables
                .iter()
                .map(|(function_id, branches)| {
                    (
                        remaps.functions[function_id],
                        branches
                            .iter()
                            .map(|(guard, result)| (remaps.types[guard], remaps.types[result]))
                            .collect(),
                    )
                })
                .collect();
            entries.sort_by_key(|(function_id, _)| *function_id);
            entries
        },
        case_tables: {
            let mut entries: Vec<(usize, usize)> = cached
                .case_tables
                .iter()
                .map(|(type_id, function_id)| {
                    (remaps.types[type_id], remaps.functions[function_id])
                })
                .collect();
            entries.sort();
            entries
        },
        callable_type_params: {
            let mut entries: Vec<(usize, Vec<String>)> = cached
                .callable_type_params
                .iter()
                .map(|(type_id, params)| (remaps.types[type_id], params.clone()))
                .collect();
            entries.sort_by_key(|(type_id, _)| *type_id);
            entries
        },
        namespace: ArtifactNamespace {
            default: namespace.default.as_ref().map(&flatten_def),
            named: {
                let mut entries: Vec<(String, AliasEntry)> = namespace
                    .named
                    .iter()
                    .map(|(name, def)| (name.clone(), flatten_def(def)))
                    .collect();
                entries.sort_by(|a, b| a.0.cmp(&b.0));
                entries
            },
        },
    };
    verify(&artifact);
    Some(artifact)
}

enum Item {
    Type(usize),
    Tuple(usize),
    Function(usize),
    Site(usize),
    Builtin(usize),
}

fn add_type(type_id: usize, closure: &mut Closure, queue: &mut Vec<Item>) {
    if closure.types.insert(type_id) {
        queue.push(Item::Type(type_id));
    }
}

/// Record a builtin, and queue it so that an instantiated type-consuming builtin's type
/// argument joins the type closure — it is a type reference the instruction stream does
/// not carry, so nothing else would reach it.
fn add_builtin(builtin_id: usize, closure: &mut Closure, queue: &mut Vec<Item>) {
    if closure.builtins.insert(builtin_id) {
        queue.push(Item::Builtin(builtin_id));
    }
}

fn add_tuple(tuple_id: usize, closure: &mut Closure, queue: &mut Vec<Item>) {
    if closure.tuples.insert(tuple_id) {
        queue.push(Item::Tuple(tuple_id));
    }
}

fn add_function(function_id: usize, closure: &mut Closure, queue: &mut Vec<Item>) {
    if closure.functions.insert(function_id) {
        queue.push(Item::Function(function_id));
    }
}

fn add_site(site_id: usize, closure: &mut Closure, queue: &mut Vec<Item>) {
    if closure.sites.insert(site_id) {
        queue.push(Item::Site(site_id));
    }
}

fn collect_type(ty: &Type, closure: &mut Closure, queue: &mut Vec<Item>) {
    collect_type_children(ty, closure, queue);
}

fn collect_type_children(ty: &Type, closure: &mut Closure, queue: &mut Vec<Item>) {
    match ty {
        Type::Integer
        | Type::Binary
        | Type::Reference
        | Type::Cycle(_)
        | Type::Resource(_)
        | Type::Variable(_) => {}
        Type::Tuple(tuple_id) => add_tuple(*tuple_id, closure, queue),
        Type::Partial { fields, .. } => {
            for (_, type_id) in fields {
                add_type(*type_id, closure, queue);
            }
        }
        Type::Callable {
            parameter,
            result,
            receive,
            states,
        } => {
            add_type(*parameter, closure, queue);
            add_type(*result, closure, queue);
            add_type(*receive, closure, queue);
            if let Some(states) = states {
                add_type(*states, closure, queue);
            }
        }
        Type::Union(members) => {
            for member in members {
                add_type(*member, closure, queue);
            }
        }
        Type::Annotated { base, entries, .. } => {
            add_type(*base, closure, queue);
            for (key, value) in entries {
                closure.annotation_keys.insert(*key);
                add_type(*value, closure, queue);
            }
        }
        Type::Process {
            send,
            receive,
            state,
        } => {
            for type_id in [send, receive, state].into_iter().flatten() {
                add_type(*type_id, closure, queue);
            }
        }
    }
}

fn collect_value(value: &Value, closure: &mut Closure, queue: &mut Vec<Item>) {
    match value {
        Value::Int(_) | Value::BigInt(_) | Value::Reference(_) => {}
        Value::Binary(quiver_core::value::Binary::Constant(constant_id)) => {
            closure.constants.insert(*constant_id);
        }
        Value::Binary(quiver_core::value::Binary::Data(_)) => {}
        Value::Tuple(tuple_id, payload) => {
            add_tuple(*tuple_id, closure, queue);
            collect_payload(payload, closure, queue);
        }
        Value::Function(function_id, payload) => {
            add_function(*function_id, closure, queue);
            collect_payload(payload, closure, queue);
        }
        Value::Builtin(builtin_id, payload) => {
            add_builtin(*builtin_id, closure, queue);
            if let Some(payload) = payload {
                collect_payload(payload, closure, queue);
            }
        }
        Value::Process(..) | Value::Resource(..) => {
            panic!("process/resource values cannot occur in a compile-time module value")
        }
    }
}

fn collect_payload(
    payload: &quiver_core::value::Payload,
    closure: &mut Closure,
    queue: &mut Vec<Item>,
) {
    for value in payload.all_values() {
        collect_value(value, closure, queue);
    }
    for (key, _) in payload.annotations() {
        closure.annotation_keys.insert(*key);
    }
    if let Some(type_id) = payload.type_argument() {
        add_type(type_id, closure, queue);
    }
}

fn collect_instruction(instruction: &Instruction, closure: &mut Closure, queue: &mut Vec<Item>) {
    let id = instruction.operand() as usize;
    match instruction.opcode() {
        Opcode::Constant => {
            closure.constants.insert(id);
        }
        Opcode::Function => add_function(id, closure, queue),
        Opcode::Builtin => add_builtin(id, closure, queue),
        Opcode::Tuple => add_tuple(id, closure, queue),
        Opcode::IsType => add_type(id, closure, queue),
        Opcode::GetNamed => {
            closure.field_names.insert(id);
        }
        Opcode::Annotate | Opcode::GetAnnotation => {
            closure.annotation_keys.insert(id);
        }
        Opcode::Stamp => add_site(id, closure, queue),
        _ => {}
    }
}

/// Fail-fast self-containment check: every reference inside the artifact must land
/// inside its tables (a violation means the extraction closure missed something —
/// linking would silently corrupt a session).
fn verify(artifact: &ModuleArtifact) {
    let function_space = artifact.function_space();
    let check = |what: &str, id: usize, len: usize| {
        assert!(
            id < len,
            "artifact {:?}: {} reference {} outside table (len {})",
            artifact.id,
            what,
            id,
            len
        );
    };
    let check_type_shallow = |ty: &Type| match ty {
        Type::Tuple(tuple_id) => check("tuple", *tuple_id, artifact.tuples.len()),
        Type::Partial { fields, .. } => {
            for (_, type_id) in fields {
                check("type", *type_id, artifact.types.len());
            }
        }
        Type::Callable {
            parameter,
            result,
            receive,
            states,
        } => {
            for type_id in [parameter, result, receive]
                .into_iter()
                .chain(states.iter())
            {
                check("type", *type_id, artifact.types.len());
            }
        }
        Type::Union(members) => {
            for member in members {
                check("type", *member, artifact.types.len());
            }
        }
        Type::Annotated { base, entries, .. } => {
            check("type", *base, artifact.types.len());
            for (key, value) in entries {
                check("annotation key", *key, artifact.annotation_keys.len());
                check("type", *value, artifact.types.len());
            }
        }
        Type::Process {
            send,
            receive,
            state,
        } => {
            for type_id in [send, receive, state].into_iter().flatten() {
                check("type", *type_id, artifact.types.len());
            }
        }
        _ => {}
    };
    for ty in &artifact.types {
        check_type_shallow(ty);
    }
    for info in &artifact.tuples {
        for (_, type_id) in &info.fields {
            check("type", *type_id, artifact.types.len());
        }
    }
    for (position, function) in artifact.functions.iter().enumerate() {
        check("type", function.type_id, artifact.types.len());
        for instruction in &function.instructions {
            let id = instruction.operand() as usize;
            match instruction.opcode() {
                Opcode::Constant => check("constant", id, artifact.constants.len()),
                Opcode::Function => {
                    check("function", id, function_space);
                    // The linked program's function table must reference strictly
                    // backward (the environment merge rewrites single-pass): an own
                    // function may reference earlier own functions or any import —
                    // imports are always linked first.
                    let valid = id < position || id >= artifact.functions.len();
                    assert!(
                        valid,
                        "artifact {:?}: function {} references unregistrable function {}",
                        artifact.id, position, id
                    );
                }
                Opcode::Builtin => check("builtin", id, artifact.builtins.len()),
                Opcode::Tuple => check("tuple", id, artifact.tuples.len()),
                Opcode::IsType => check("type", id, artifact.types.len()),
                Opcode::GetNamed => check("field name", id, artifact.field_names.len()),
                Opcode::Annotate | Opcode::GetAnnotation => {
                    check("annotation key", id, artifact.annotation_keys.len())
                }
                Opcode::Stamp => check("site", id, artifact.sites.len()),
                _ => {}
            }
        }
    }
    for site in &artifact.sites {
        check("constant", site.module_constant, artifact.constants.len());
    }
}

/// Link an artifact into a session, leaving `module_cache` holding the module
/// exactly as a from-source compile would. `expected` is the module the caller
/// resolved and keyed — a mismatch means a key collision or a corrupted store file,
/// caught here rather than silently linking an unrequested module. The module's
/// dependencies must already be cached in the session with their function maps
/// recorded — the import pipeline guarantees this for both linked and
/// source-compiled dependencies, so a missing map is an invariant violation, not a
/// fallback case.
pub(crate) fn link_module<E: Effect>(
    artifact: &ModuleArtifact,
    expected: &ModuleId,
    program: &mut Program,
    module_cache: &mut ModuleCache,
    builtins: &BuiltinRegistry<E>,
) -> Result<(), Error> {
    assert_eq!(
        artifact.id, *expected,
        "artifact key resolved to a different module — key collision or corrupted store"
    );
    // Re-intern the value-like tables, building the artifact-local → session remap.
    // Types and tuples are mutually recursive, so intern on demand with memoisation
    // (references form a DAG — recursion markers are relative, never table cycles).
    let mut remaps = IdRemaps::default();
    for (local, constant) in artifact.constants.iter().enumerate() {
        let session = program.register_constant(constant.clone());
        remaps.constants.insert(local, session);
    }
    for (local, key) in artifact.annotation_keys.iter().enumerate() {
        let session = program.register_annotation_key(key);
        remaps.annotation_keys.insert(local, session);
    }
    for (local, name) in artifact.field_names.iter().enumerate() {
        let session = program.register_field_name(name);
        remaps.field_names.insert(local, session);
    }
    intern_types_and_tuples(artifact, program, &mut remaps);
    // Every linked tuple must also have its `Type::Tuple` wrapper entry: the runtime
    // compatibility tables represent a concrete tuple by that entry, so a tuple
    // without one is invisible to every `IsType` test. A from-source compile
    // registers the wrapper while typing the construction, but the artifact closure
    // only carries types the module's code references by type id — a tuple that is
    // constructed yet never referenced as a type would otherwise arrive untestable.
    for local in 0..artifact.tuples.len() {
        program.register_type(Type::Tuple(remaps.tuples[&local]));
    }
    for (local_tuple, field) in &artifact.omittable_labels {
        program.mark_label_omittable(remaps.tuples[local_tuple], *field);
    }
    for (local, site) in artifact.sites.iter().enumerate() {
        let session = program.register_debug_site(Site {
            module_constant: remaps.constants[&site.module_constant],
            ..site.clone()
        });
        remaps.sites.insert(local, session);
    }

    // Builtins resolve by name against the host registry — the link-time capability
    // check: a host that doesn't provide a builtin refuses the module.
    for (local, builtin) in artifact.builtins.iter().enumerate() {
        if builtins.get_specs(&builtin.name).is_none() {
            return Err(Error::FeatureUnsupported(format!(
                "module {} requires builtin '{}', which this host does not provide",
                artifact.id.display(),
                builtin.name
            )));
        }
        // The type argument re-interns like any other type reference — `remaps.types` is
        // complete by here, and registration dedupes on the instantiated pair.
        let session = program.register_builtin_instantiated(
            builtin.name.clone(),
            builtin.type_argument.map(|local| remaps.types[&local]),
            builtins,
        );
        remaps.builtins.insert(local, session);
    }

    // Imported functions resolve through the dependencies' function maps.
    let own_count = artifact.functions.len();
    let mut import_position = own_count;
    for (module, dep_own_indices) in &artifact.imports {
        let dep_map = module_cache
            .module_functions
            .get(module)
            .unwrap_or_else(|| {
                panic!(
                    "linking {}: dependency {} is cached without a function map — \
                     the import pipeline must record one for every cached module",
                    artifact.id.display(),
                    module.display()
                )
            });
        for dep_own_index in dep_own_indices {
            remaps
                .functions
                .insert(import_position, dep_map[*dep_own_index]);
            import_position += 1;
        }
    }

    // The module's functions register by append (`push_function`) — never collapsing
    // onto other modules' structurally identical ones, so ownership attribution
    // stays session-history-independent; structural collapse is the runtime merge's
    // job. Their references point strictly backward (earlier own functions, or
    // imports linked before this module — `verify` vouched at extraction), keeping
    // the program table single-pass-rewritable for the environment merge.
    let base = program.get_functions().len();
    for own_index in 0..own_count {
        remaps.functions.insert(own_index, base + own_index);
    }
    let mut own_map: Vec<usize> = Vec::with_capacity(own_count);
    for function in &artifact.functions {
        let session = program.push_function(function.clone().remap_ids(&remaps));
        own_map.push(session);
    }
    for (index, &function_id) in own_map.iter().enumerate() {
        module_cache
            .function_owners
            .insert(function_id, (artifact.id.clone(), index));
    }
    module_cache
        .module_functions
        .insert(artifact.id.clone(), own_map);
    // The linked module's value closure mirrors the source-compile record: its
    // import-table modules plus their closures (all cached before it).
    module_cache.record_value_closure(&artifact.id, artifact.dependencies());

    // Install the cached module and namespace, exactly as a from-source compile
    // would have left them.
    let cached = CachedModule {
        value: artifact.value.remap_ids(&remaps),
        module_type: artifact.module_type.remap_ids(&remaps),
        fn_case_tables: artifact
            .fn_case_tables
            .iter()
            .map(|(function_id, branches)| {
                (
                    remaps.functions[function_id],
                    branches
                        .iter()
                        .map(|(guard, result)| (remaps.types[guard], remaps.types[result]))
                        .collect(),
                )
            })
            .collect(),
        case_tables: artifact
            .case_tables
            .iter()
            .map(|(type_id, function_id)| (remaps.types[type_id], remaps.functions[function_id]))
            .collect(),
        callable_type_params: artifact
            .callable_type_params
            .iter()
            .map(|(type_id, params)| (remaps.types[type_id], params.clone()))
            .collect(),
    };
    module_cache.cache_module(artifact.id.clone(), cached);

    let unflatten = |entry: &AliasEntry| TypeAliasDef {
        parameters: entry.parameters.clone(),
        type_id: remaps.types[&entry.type_id],
    };
    module_cache.type_namespace_cache.insert(
        artifact.id.clone(),
        ModuleTypeNamespace {
            default: artifact.namespace.default.as_ref().map(&unflatten),
            named: artifact
                .namespace
                .named
                .iter()
                .map(|(name, entry)| (name.clone(), unflatten(entry)))
                .collect(),
        },
    );

    Ok(())
}

/// Intern the artifact's types and tuples into the session program, on demand with
/// memoisation (the two tables reference each other).
fn intern_types_and_tuples(
    artifact: &ModuleArtifact,
    program: &mut Program,
    remaps: &mut IdRemaps,
) {
    fn intern_type(
        local: usize,
        artifact: &ModuleArtifact,
        program: &mut Program,
        remaps: &mut IdRemaps,
    ) -> usize {
        if let Some(&session) = remaps.types.get(&local) {
            return session;
        }
        let ty = &artifact.types[local];
        // Intern children first so the remap covers every reference.
        match ty {
            Type::Tuple(tuple_id) => {
                intern_tuple(*tuple_id, artifact, program, remaps);
            }
            Type::Partial { fields, .. } => {
                for (_, type_id) in fields {
                    intern_type(*type_id, artifact, program, remaps);
                }
            }
            Type::Callable {
                parameter,
                result,
                receive,
                states,
            } => {
                for &type_id in [parameter, result, receive]
                    .into_iter()
                    .chain(states.iter())
                {
                    intern_type(type_id, artifact, program, remaps);
                }
            }
            Type::Union(members) => {
                for member in members {
                    intern_type(*member, artifact, program, remaps);
                }
            }
            Type::Annotated { base, entries, .. } => {
                intern_type(*base, artifact, program, remaps);
                for (_, value) in entries {
                    intern_type(*value, artifact, program, remaps);
                }
            }
            Type::Process {
                send,
                receive,
                state,
            } => {
                for &type_id in [send, receive, state].into_iter().flatten() {
                    intern_type(type_id, artifact, program, remaps);
                }
            }
            _ => {}
        }
        let session = program.register_type(ty.remap_ids(remaps));
        remaps.types.insert(local, session);
        session
    }

    fn intern_tuple(
        local: usize,
        artifact: &ModuleArtifact,
        program: &mut Program,
        remaps: &mut IdRemaps,
    ) -> usize {
        if let Some(&session) = remaps.tuples.get(&local) {
            return session;
        }
        let info = &artifact.tuples[local];
        for (_, type_id) in &info.fields {
            intern_type(*type_id, artifact, program, remaps);
        }
        let session = program.register_tuple(info.name.clone(), info.remap_ids(remaps).fields);
        remaps.tuples.insert(local, session);
        session
    }

    for local in 0..artifact.types.len() {
        intern_type(local, artifact, program, remaps);
    }
    for local in 0..artifact.tuples.len() {
        intern_tuple(local, artifact, program, remaps);
    }
}
