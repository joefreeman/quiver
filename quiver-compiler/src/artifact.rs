//! Per-module compiled artifacts, their content-addressed store, and the linker.
//!
//! Every module load goes through one pipeline: resolve the module, compute its
//! **artifact key**, and either **link** the stored artifact for that key into the
//! session or **compile from source** — in which case the compiler records the
//! module's layout and extracts a fresh artifact into the store. The key is a
//! Merkle-style hash over everything *within one compiler version* that determines
//! compiled output — the debug flag, the module's source, and the keys of every
//! module it references (value imports, dialects, *and* type-level references) — so
//! a store hit is valid by construction and invalidation is per-module. The compiler
//! version itself is the store's directory layer rather than a key component: each
//! fingerprint gets its own subdirectory, so keys never collide across versions and
//! an old version's cache is one directory to delete.
//!
//! A [`ModuleArtifact`] is a compiled module in a self-contained, artifact-local id
//! space. Value-like entities — types, tuples, constants, annotation keys, field
//! names — dedup structurally, so the artifact carries *copies* of everything it
//! references (transitively closed) and the linker re-interns them into a session's
//! program. Only functions and builtins are nominal: the artifact's function id
//! space is its own functions, then its imports — a table naming `(module, that
//! module's *content* key, that module's own index)`, grouped by module, every named
//! module lying in the artifact's transitive value-import closure and hence covered
//! by its key — and
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
/// quiver-core). Emitted by the build script. It scopes the artifact cache: a
/// disk-backed [`ArtifactStore`] keeps each compiler version's artifacts in their own
/// subdirectory named by this string, which is what keys stay valid against and what
/// makes an old version's cache a single directory to delete. Standard-library sources
/// are *not* included: std modules are content-addressed individually, like any other
/// module.
pub fn compiler_fingerprint() -> &'static str {
    env!("QUIVER_COMPILER_FINGERPRINT")
}

/// The content key of a [`CompiledUnit`]: SHA-256 over its canonical bytes (see
/// [`unit_key`]). This is the identity a unit is named by outside the compiler — on the
/// wire, in a `.qx` file's module list, in import tables, and in a host's linked-module
/// map — and unlike the source-based artifact key it is *checkable*: a host validates a
/// received unit by rehashing it, so a wrong or colliding key is refused at the boundary
/// instead of silently linking one client's code against another's module.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct UnitKey([u8; 32]);

impl std::fmt::Display for UnitKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&hex::encode(self.0))
    }
}

impl std::fmt::Debug for UnitKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UnitKey({})", hex::encode(self.0))
    }
}

impl std::str::FromStr for UnitKey {
    type Err = String;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        let bytes = hex::decode(text).map_err(|e| format!("invalid unit key: {e}"))?;
        let bytes: [u8; 32] = bytes
            .try_into()
            .map_err(|_| "invalid unit key: expected 32 bytes".to_string())?;
        Ok(UnitKey(bytes))
    }
}

impl serde::Serialize for UnitKey {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&hex::encode(self.0))
    }
}

impl<'de> serde::Deserialize<'de> for UnitKey {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let text = String::deserialize(deserializer)?;
        text.parse().map_err(serde::de::Error::custom)
    }
}

/// Compute a unit's content key: SHA-256 over a version prefix plus the unit's
/// compact `serde_json` bytes — the exact representation the wire and a `.qx` carry.
/// This is canonical because a unit is pure vector-shaped data in deterministic order
/// (see [`CompiledUnit`]), so equal units serialize to equal bytes and a deserialized
/// unit re-serializes to the bytes it arrived as; `quiver-tests/tests/units.rs` holds
/// the round-trip. The prefix versions the scheme so a future canonical encoding can
/// change every key at once rather than colliding with the old ones.
pub fn unit_key(unit: &CompiledUnit) -> UnitKey {
    use sha2::Digest;
    let mut hasher = sha2::Sha256::new();
    hasher.update(b"v1\0");
    hasher.update(serde_json::to_vec(unit).expect("a unit always serializes"));
    UnitKey(hasher.finalize().into())
}

/// Relocatable compiled code in unit-local id space: the value-like tables it
/// references, the functions it owns, and an import table naming everything else. Pure
/// canonical data — every collection is a vector in deterministic order, so equal units
/// serialize to equal bytes.
///
/// Both halves of the system produce one: a module compile produces the unit inside its
/// [`ModuleArtifact`], and a REPL line or program entry produces a standalone one. It is
/// the only thing the linker links.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CompiledUnit {
    /// Value-like tables. References within them are unit-local and point strictly
    /// downward (the program's registration order is topological, and extraction
    /// preserves it).
    pub types: Vec<Type>,
    pub tuples: Vec<TupleTypeInfo>,
    pub constants: Vec<Constant>,
    pub annotation_keys: Vec<String>,
    pub field_names: Vec<String>,
    /// The functions this compile registered, in registration order — unit function ids
    /// `0..functions.len()`, the positions other modules' import entries reference.
    /// Identical in every session: the compiler's dedup floor keeps a module's functions
    /// its own even when structurally identical to another module's.
    pub functions: Vec<Function>,
    /// Imported functions, grouped by module and continuing the id space after
    /// [`Self::functions`]: groups in `ModuleId` order, entries (the *dependency's*
    /// own-function indices) ascending — unit function ids assigned in that flattened
    /// order.
    ///
    /// Each group names the dependency's **content key** ([`unit_key`] of its unit) as
    /// well as its id. The id alone would not do for a linker outside the compiler's
    /// import pipeline — it cannot assume one module per name — and a *source-based*
    /// key would not do for a host that validates what it links: only a content key can
    /// be checked against the unit it names. Since these keys are part of the unit's
    /// serialized bytes, a unit's own content key covers its dependencies' recursively —
    /// the Merkle composition the source-based artifact key established, carried over.
    pub imports: Vec<(ModuleId, UnitKey, Vec<usize>)>,
    /// Referenced builtins; unit builtin ids index this list. Nominal — resolved against
    /// the host registry at link time — except for an instantiated type-consuming
    /// builtin, whose type argument is unit-local and re-interned with the rest.
    pub builtins: Vec<ArtifactBuiltin>,
    /// Failure-provenance sites (debug builds; empty in release ones).
    pub sites: Vec<Site>,
    /// The function to run, for a unit that is a program entry or a REPL line. `None`
    /// for a module: its wrapper runs at compile time and is not part of what ships —
    /// the resulting value is, on the artifact.
    pub entry: Option<usize>,
}

/// A compiled module: its [`CompiledUnit`], plus the compile-time metadata only a module
/// has. Stored under a content key (see [`module_key`]) and consumed by the compiler
/// alone — what reaches a runtime is [`Self::unit`].
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ModuleArtifact {
    pub id: ModuleId,
    pub unit: CompiledUnit,
    /// [`unit_key`] of [`Self::unit`], computed once at extraction. Derivable, stored so
    /// that linking a warm session's modules does not re-serialize and re-hash every
    /// unit; a host that receives the unit validates the key by rehashing anyway, so a
    /// corrupt store entry is refused at the boundary rather than trusted.
    pub content_key: UnitKey,
    /// The evaluated module value, in unit space.
    pub value: Value,
    pub module_type: Type,
    /// Return-type dispatch tables (see `CachedModule`), in unit space:
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

impl CompiledUnit {
    /// The modules this unit's function imports name, in group order.
    pub fn dependencies(&self) -> Vec<ModuleId> {
        self.imports
            .iter()
            .map(|(module, _, _)| module.clone())
            .collect()
    }

    /// The size of the unit's function id space (own + imports).
    fn function_space(&self) -> usize {
        self.functions.len()
            + self
                .imports
                .iter()
                .map(|(_, _, indices)| indices.len())
                .sum::<usize>()
    }
}

/// A content-addressed store of module artifacts: an in-memory map over an optional
/// cache directory, probed lazily per key. There is nothing to "load" up front and
/// no attach ceremony — the store is always ready, and a missing entry simply means
/// the module compiles from source (which then saves its artifact here).
///
/// On disk, every store is **scoped by compiler fingerprint**: entries live under
/// `<base>/<fingerprint>/<key>.json`. Artifact keys deliberately do not hash the
/// compiler version, so the directory layer is what keeps versions apart — a key from
/// another build can never resolve here, and an obsolete version's cache is a single
/// directory to remove.
pub struct ArtifactStore {
    /// The fingerprint-scoped directory entries are read from and written to.
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
    /// Opening it cleans the base directory: sibling fingerprint directories — other
    /// compiler versions, garbage the moment this one was built — are removed outright,
    /// as are files from the retired flat layout; within this version's own directory,
    /// entries untouched for [`Self::MAX_AGE`] are pruned, age being the only signal a
    /// content-addressed key is dead.
    pub fn cache() -> Self {
        let base = std::env::var_os("XDG_CACHE_HOME")
            .map(PathBuf::from)
            .or_else(|| std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".cache")))
            .map(|base| base.join("quiver").join("artifacts"));
        if let Some(base) = &base {
            prune(base);
        }
        Self {
            dir: base
                .map(|base| base.join(compiler_fingerprint()))
                .filter(|dir| std::fs::create_dir_all(dir).is_ok()),
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
    /// layout). Entries still live under a fingerprint subdirectory — keys do not
    /// hash the compiler version, so the layer is load-bearing: without it, a stale
    /// artifact from another build would *hit*, not miss. No pruning: the directory
    /// is the caller's to manage.
    pub fn at_dir(dir: PathBuf) -> Self {
        let dir = dir.join(compiler_fingerprint());
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
    /// with a per-process temp name, so concurrent writers never tear a file). The
    /// write replaces any existing file: equal source keys do *not* yet guarantee equal
    /// bytes (a compile against a linked dependency emits differently than against a
    /// source-compiled one), and overwriting converges the directory on the latest
    /// session's coherent set, where skipping would pin a dependent to dependency
    /// content the store no longer resolves — a permanent cache miss.
    pub fn save(&self, key: u64, artifact: ModuleArtifact) {
        let artifact = Rc::new(artifact);
        self.memory.borrow_mut().insert(key, artifact.clone());
        if let Some(path) = self.path(key)
            && let Ok(bytes) = serde_json::to_vec(&*artifact)
        {
            // Re-create the fingerprint directory if it vanished: a newer build's
            // `cache()` removes sibling versions' directories, and this process may be
            // one of those versions, still running. Its saves keep working; the next
            // newer open sweeps the directory again.
            if let Some(parent) = path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }
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

/// Clean the cache base directory on open. Sibling fingerprint directories are other
/// compiler versions' caches — garbage from the moment this compiler was built, since
/// nothing but the version that wrote them can resolve their keys — and are removed
/// whole. Plain files at the base are the retired flat layout (pre fingerprint
/// scoping), unreachable and removed regardless of age. Within the current version's
/// own directory, entries fall to the [`ArtifactStore::MAX_AGE`] prune: a key goes
/// dead when its module's source changes, and age is the only signal of that.
fn prune(base: &Path) {
    if let Ok(entries) = std::fs::read_dir(base) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                if entry.file_name().to_string_lossy() == compiler_fingerprint() {
                    prune_stale_files(&path);
                } else {
                    let _ = std::fs::remove_dir_all(&path);
                }
            } else {
                let _ = std::fs::remove_file(&path);
            }
        }
    }
    // The pre-content-addressing bundle caches lived one level up.
    if let Some(parent) = base.parent()
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

/// Remove files untouched for longer than [`ArtifactStore::MAX_AGE`].
fn prune_stale_files(dir: &Path) {
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
}

/// The artifact key of the module at `path` (resolved from `from_package`): a
/// Merkle-style hash of the debug flag, the module's identity and source, and —
/// recursively — the keys of every module it references. The compiler version is
/// deliberately not hashed: the store scopes its directory by fingerprint instead.
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
    .unwrap_or_else(|e| {
        // Nearly always a registry that is missing a capability group rather than a broken
        // std: the signatures a group registers are what the modules over it type-check
        // against, so an omitted group surfaces here as an ordinary type error in whichever
        // std module used it. A compiling registry wants `universal_modules`.
        panic!(
            "standard library must compile — check the builtin registry covers every \
             capability group ({:?})",
            e.error
        )
    });
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

/// Whose functions an extraction is collecting.
enum Owner<'a> {
    /// A module's: its own functions are the ones it registered, and it may import only
    /// from its transitive value-import closure, which its key covers.
    Module {
        id: &'a ModuleId,
        eligible: &'a HashSet<ModuleId>,
    },
    /// A unit with no module identity — a REPL line, a program entry. It imports from
    /// the modules in `eligible` (the ones whose artifacts can be supplied alongside
    /// it), and owns everything else it reaches: functions no module claims, and —
    /// inlined — the functions of any module outside the set. An empty set is a fully
    /// inlined, self-contained unit.
    Unit {
        eligible: &'a HashSet<ModuleId>,
        /// The entry is always owned, even when interning collapsed it onto a module's
        /// function: an import entry cannot carry the entry point.
        entry: Option<usize>,
    },
}

fn classify(function_id: usize, owner: &Owner, module_cache: &ModuleCache) -> FunctionClass {
    match (owner, module_cache.function_owners.get(&function_id)) {
        (Owner::Module { id, .. }, Some((holder, _))) if holder == *id => FunctionClass::Own,
        (Owner::Module { eligible, .. }, Some((holder, index))) if eligible.contains(holder) => {
            FunctionClass::Import(holder.clone(), *index)
        }
        (Owner::Module { .. }, _) => FunctionClass::Foreign,
        (Owner::Unit { entry, .. }, _) if *entry == Some(function_id) => FunctionClass::Own,
        (Owner::Unit { eligible, .. }, Some((holder, index))) if eligible.contains(holder) => {
            FunctionClass::Import(holder.clone(), *index)
        }
        (Owner::Unit { .. }, _) => FunctionClass::Own,
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
    owner: &Owner,
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
            Item::Constant(constant_id) => {
                let constant = program
                    .get_constant(constant_id)
                    .expect("constant id out of range")
                    .clone();
                for child in constant.children() {
                    add_constant(child, closure, queue);
                }
                // Whatever else the constant names travels with it, exactly as it would if
                // an instruction named it: a constant is a value the unit must be able to
                // rebuild on its own.
                match &constant {
                    Constant::Tuple { id, .. } => add_tuple(*id, closure, queue),
                    Constant::Function { id, .. } => add_function(*id, closure, queue),
                    Constant::Builtin { id } => add_builtin(*id, closure, queue),
                    Constant::Annotated { entries, .. } => {
                        for (key, _) in entries {
                            closure.annotation_keys.insert(*key);
                        }
                    }
                    Constant::Integer(_) | Constant::Binary(_) => {}
                }
            }
            Item::Function(function_id) => match classify(function_id, owner, module_cache) {
                FunctionClass::Own => {
                    let function = &program.get_functions()[function_id];
                    add_type(function.type_id, closure, queue);
                    for instruction in &function.instructions {
                        collect_instruction(instruction, closure, queue);
                    }
                }
                FunctionClass::Import(..) => {}
                FunctionClass::Foreign => return Err(Uncacheable),
            },
            Item::Site(site_id) => {
                let site = &program
                    .debug_sites()
                    .expect("stamped function without a site table")
                    .sites[site_id];
                add_constant(site.module_constant, closure, queue);
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
        Type::Top => 12u8.hash(&mut hasher),
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
            omittable,
        } => {
            8u8.hash(&mut hasher);
            fingerprint_type(*parameter, program, memo).hash(&mut hasher);
            fingerprint_type(*result, program, memo).hash(&mut hasher);
            fingerprint_type(*receive, program, memo).hash(&mut hasher);
            states
                .map(|states| fingerprint_type(states, program, memo))
                .hash(&mut hasher);
            // Part of type identity, so it must be fingerprinted: two units differing only
            // in which labels a parameter lets callers omit are different units, and
            // sharing a cache entry between them would hand one the other's convention.
            omittable.hash(&mut hasher);
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
    let owner = &Owner::Module { id, eligible };

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
        drain(&mut queue, &mut closure, owner, program, module_cache).ok()?;
    }
    // 2. The module value, then its type.
    collect_value(&cached.value, &mut closure, &mut queue);
    drain(&mut queue, &mut closure, owner, program, module_cache).ok()?;
    collect_type(&cached.module_type, &mut closure, &mut queue);
    drain(&mut queue, &mut closure, owner, program, module_cache).ok()?;
    // 3. The namespace: default alias, then named aliases by name.
    let mut named_aliases: Vec<(&String, &TypeAliasDef)> = namespace.named.iter().collect();
    named_aliases.sort_by_key(|(name, _)| (*name).clone());
    for def in namespace
        .default
        .iter()
        .chain(named_aliases.iter().map(|(_, def)| *def))
    {
        add_type(def.type_id, &mut closure, &mut queue);
        drain(&mut queue, &mut closure, owner, program, module_cache).ok()?;
    }
    // 4. Dispatch tables. Their map keys are session ids, so entries order by the
    // canonical function classification — and, for the type-keyed tables, by the
    // type's first-reach position (or a structural fingerprint for a type nothing
    // else reached; sort keys snapshot before any entry's traversal mutates state).
    let fn_key = |function_id: usize| -> FnSortKey {
        match classify(function_id, owner, module_cache) {
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
        drain(&mut queue, &mut closure, owner, program, module_cache).ok()?;
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
        drain(&mut queue, &mut closure, owner, program, module_cache).ok()?;
    }
    let mut param_keys: Vec<(usize, TypeSortKey)> = cached
        .callable_type_params
        .keys()
        .map(|type_id| (*type_id, type_key(*type_id, &closure)))
        .collect();
    param_keys.sort_by_key(|(_, key)| *key);
    for (type_id, _) in param_keys {
        add_type(type_id, &mut closure, &mut queue);
        drain(&mut queue, &mut closure, owner, program, module_cache).ok()?;
    }

    // Partition the function closure. Own functions take artifact ids by
    // registration order (the session-invariant `module_functions` positions import
    // entries reference — not first-reach order, since a later own function can be
    // reached from an earlier one's body); imports extend the id space in canonical
    // (module id, dependency index) order.
    let own_ids: Vec<usize> = module_cache.module_functions[id].clone();
    let mut import_entries: Vec<(ModuleId, usize, usize)> = Vec::new();
    for &function_id in closure.functions.iter() {
        match classify(function_id, owner, module_cache) {
            FunctionClass::Own => {}
            FunctionClass::Import(module, index) => {
                import_entries.push((module, index, function_id));
            }
            FunctionClass::Foreign => unreachable!("foreign functions abort the traversal"),
        }
    }
    import_entries.sort();
    let mut imports: Vec<(ModuleId, UnitKey, Vec<usize>)> = Vec::new();
    for (module, dep_index, _) in &import_entries {
        match imports.last_mut() {
            Some((last, _, indices)) if last == module => indices.push(*dep_index),
            _ => {
                // The dependency's content key comes from its stored artifact. A miss
                // means the dependency itself could not be extracted (unkeyable, hidden,
                // or foreign-referencing), so nothing can name it — extract nothing
                // rather than store an artifact whose imports cannot be checked. This
                // cascades: a module is cacheable only over cacheable dependencies.
                let key = module_cache.content_key(module)?;
                imports.push((module.clone(), key, vec![*dep_index]));
            }
        }
    }
    // Key-only entries for dependencies this module read but reaches no function of:
    // its code was baked against their exact versions all the same, so the import table
    // must name them for link-time version validation. An unkeyable one makes this
    // module uncacheable, like any other dependency — and cannot arise on its own, since
    // a module's key already requires every declared reference to be keyable.
    for read in module_cache.value_reads_of(Some(id)) {
        if imports.iter().any(|(module, _, _)| *module == read) {
            continue;
        }
        let key = module_cache.content_key(&read)?;
        imports.push((read, key, vec![]));
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

    let unit = CompiledUnit {
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
            .map(|&constant_id| {
                program.get_constants()[constant_id]
                    .clone()
                    .remap_ids(&remaps)
            })
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
        // A module's wrapper runs at compile time; the artifact carries its value.
        entry: None,
    };
    verify(&unit, &id.display());
    let content_key = unit_key(&unit);

    let artifact = ModuleArtifact {
        id: id.clone(),
        unit,
        content_key,
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
    Some(artifact)
}

/// Why a unit could not be extracted.
/// How a unit treats the module functions it reaches.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Imports {
    /// Import from every module the session can *supply* — its artifact and the
    /// artifacts of its transitive imports are all stored — and inline the functions of
    /// any module it cannot. With no store attached nothing is suppliable, so this
    /// degenerates to [`Imports::Inline`].
    Bundle,
    /// Import nothing: claim every reachable function as the unit's own and shake it to
    /// the entry. A self-contained unit with an empty import table, for output that
    /// must stand alone.
    Inline,
}

/// The modules whose units the session can supply whole: the module is keyed, its
/// artifact is stored, and so — recursively — is every artifact its import table names.
/// Anything outside this set (unkeyed, hidden, or with a gap in its closure) cannot be
/// named on a wire, so extraction inlines it instead.
fn suppliable_modules(module_cache: &ModuleCache) -> HashSet<ModuleId> {
    let Some(store) = module_cache.artifact_store.as_ref() else {
        return HashSet::new();
    };
    // The store is keyed by *artifact* (source) key, so dependencies resolve by module
    // id through the session's key cache; the import table's content key is asserted
    // against the loaded artifact where the closure is actually assembled
    // ([`module_closure`]).
    fn loadable(
        id: &ModuleId,
        module_cache: &ModuleCache,
        store: &ArtifactStore,
        memo: &mut HashMap<ModuleId, bool>,
    ) -> bool {
        if let Some(&known) = memo.get(id) {
            return known;
        }
        // Seed false so a keying cycle answers unsuppliable rather than recursing.
        memo.insert(id.clone(), false);
        let artifact = module_cache
            .key_cache
            .get(id)
            .and_then(|key| store.load(*key));
        let ok = match artifact {
            Some(artifact) => artifact
                .unit
                .imports
                .iter()
                .all(|(dependency, _, _)| loadable(dependency, module_cache, store, memo)),
            None => false,
        };
        memo.insert(id.clone(), ok);
        ok
    }
    let mut memo = HashMap::new();
    module_cache
        .key_cache
        .keys()
        .filter(|id| loadable(id, module_cache, store, &mut memo))
        .cloned()
        .collect()
}

/// A self-contained compiled program: an entry unit plus the units of every module it
/// imports, transitively, deepest first. Everything a host needs in order to link and run
/// it, with nothing to look up — which is what a `.qx` file holds.
///
/// The closure is **bundled whole, not shaken**. A module's unit is the linkable identity
/// its key names, so trimming it would both falsify the key and cost a host the ability to
/// recognise a module it already holds. Shaking happens at the other boundary: the entry
/// unit carries only what it reaches, and stops at module edges.
///
/// [`Imports::Inline`] instead inlines imports into the entry unit — the extraction walk
/// claiming module functions as its own and shaking them to the entry — which lands here
/// as an empty [`Self::modules`] and an empty import table. No format change: a consumer
/// links what it is given either way.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CompiledProgram {
    pub unit: CompiledUnit,
    /// `(content key, that module's unit)`, dependencies before dependents — the order a
    /// host must link them in.
    pub modules: Vec<(UnitKey, CompiledUnit)>,
}

/// The transitive closure of the modules `unit` imports, deepest first, each under its
/// content key. Artifacts load from the store by the session's *artifact* key (the
/// store's index), and the import table's content key is asserted against what loaded —
/// a mismatch would mean the session's record and its store disagree about a module's
/// bytes. Answers the id of the first module with no stored artifact — nothing could
/// name a version of it, so the closure cannot be assembled.
pub fn module_closure(
    store: &ArtifactStore,
    module_cache: &ModuleCache,
    unit: &CompiledUnit,
) -> Result<Vec<(UnitKey, Rc<ModuleArtifact>)>, ModuleId> {
    fn collect(
        id: &ModuleId,
        content_key: UnitKey,
        store: &ArtifactStore,
        module_cache: &ModuleCache,
        seen: &mut HashSet<UnitKey>,
        out: &mut Vec<(UnitKey, Rc<ModuleArtifact>)>,
    ) -> Result<(), ModuleId> {
        if !seen.insert(content_key) {
            return Ok(());
        }
        let artifact = module_cache
            .key_cache
            .get(id)
            .and_then(|key| store.load(*key))
            .ok_or_else(|| id.clone())?;
        assert_eq!(
            artifact.content_key,
            content_key,
            "module {}: an import names content {content_key} but the store supplies {}",
            id.display(),
            artifact.content_key
        );
        for (dependency, key, _) in &artifact.unit.imports {
            collect(dependency, *key, store, module_cache, seen, out)?;
        }
        out.push((content_key, artifact));
        Ok(())
    }
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for (module, key, _) in &unit.imports {
        collect(module, *key, store, module_cache, &mut seen, &mut out)?;
    }
    Ok(out)
}

/// Extract a self-contained [`CompiledProgram`] from a compile: the unit reachable from
/// `entry`, plus the bundled closure of the modules it imports ([`Imports::Bundle`]) —
/// or nothing beyond the unit itself ([`Imports::Inline`]).
pub fn extract_program(
    program: &Program,
    module_cache: &ModuleCache,
    entry: Option<usize>,
    own_floor: usize,
    imports: Imports,
) -> CompiledProgram {
    let unit = extract_unit(program, module_cache, entry, own_floor, imports);
    let modules = if unit.imports.is_empty() {
        Vec::new()
    } else {
        let store = module_cache
            .artifact_store
            .as_ref()
            .expect("bundled imports require the store that made their modules suppliable");
        module_closure(store, module_cache, &unit)
            .unwrap_or_else(|module| {
                panic!(
                    "no stored artifact for module {} named by a bundled import",
                    module.display()
                )
            })
            .into_iter()
            .map(|(key, artifact)| (key, artifact.unit.clone()))
            .collect()
    };
    let mut program = CompiledProgram { unit, modules };

    // Shaking rewrites a unit's bytes, and a unit is named by the hash of those bytes — with
    // dependencies cited by *their* content keys, so the keys form a Merkle chain. Re-key
    // bottom-up: the closure is deepest-first, so a module's dependencies have already been
    // rewritten and re-keyed when its own citations are updated.
    let mut rekeyed: HashMap<UnitKey, UnitKey> = HashMap::new();
    for (key, unit) in &mut program.modules {
        recite_imports(unit, &rekeyed);
        shake_types(unit);
        let shaken = unit_key(unit);
        rekeyed.insert(*key, shaken);
        *key = shaken;
    }
    recite_imports(&mut program.unit, &rekeyed);
    shake_types(&mut program.unit);
    program
}

/// Point a unit's import citations at the re-keyed versions of the modules they name.
fn recite_imports(unit: &mut CompiledUnit, rekeyed: &HashMap<UnitKey, UnitKey>) {
    for (_, key, _) in &mut unit.imports {
        if let Some(shaken) = rekeyed.get(key) {
            *key = *shaken;
        }
    }
}

/// The type ids a type references directly. `Type::Tuple` is not among them: it names a
/// *tuple* id, and the field types behind it are rewritten with the tuples table, after the
/// map is complete.
fn type_children(ty: &Type) -> Vec<usize> {
    match ty {
        Type::Integer
        | Type::Binary
        | Type::Reference
        | Type::Cycle(_)
        | Type::Resource(_)
        | Type::Variable(_)
        | Type::Top
        | Type::Tuple(_) => Vec::new(),
        Type::Union(members) => members.clone(),
        Type::Partial { fields, .. } => fields.iter().map(|(_, id)| *id).collect(),
        Type::Callable {
            parameter,
            result,
            receive,
            states,
            ..
        } => [Some(*parameter), Some(*result), Some(*receive), *states]
            .into_iter()
            .flatten()
            .collect(),
        Type::Annotated { base, entries, .. } => std::iter::once(*base)
            .chain(entries.iter().map(|(_, id)| *id))
            .collect(),
        Type::Process {
            send,
            receive,
            state,
        } => [*send, *receive, *state].into_iter().flatten().collect(),
    }
}

/// Drop the type information a *program* cannot use, and re-intern what is left.
///
/// A `.qx` is only ever linked and run — never compiled against — and two kinds of type
/// information exist solely for the compiler:
///
/// - **Annotation rows.** `Type::Annotated` records what annotations a value is statically
///   known to carry, so `x:key` can be typed. Everything downstream discards it:
///   `compute_compatible_concrete_types` opens by calling `Type::strip_annotations`, the
///   `%data` and `%json` codecs recurse straight to `base`, and the executor never reads the
///   variant at all. Each row is spliced out in favour of its base.
/// - **Type-variable names.** `check_type_relation` matches a variable with
///   `(Type::Variable(_), _) | (_, Type::Variable(_)) => true` — a wildcard whose name is
///   ignored. Definition-site uniquification mints a distinct name per definition, so the
///   same generic signature written in two modules produces two unrelated graphs. Collapsing
///   every name to one makes them the same row.
///
/// Re-interning is where the second one pays: once names are gone, alpha-equivalent rows are
/// *identical* rows, and dedupe removes them. Together the two take `examples/todo.qv` from
/// 6,391 distinct type rows to 2,966.
///
/// This must not be applied to a unit a compiler will consume. A `ModuleArtifact` carries
/// `module_type`, its type namespaces and `fn_case_tables` in the same id space, and typing a
/// caller against the module needs exactly what is discarded here — the annotation row to
/// type `f:doc`, and variable identity to unify. Only `extract_program` calls this.
fn shake_types(unit: &mut CompiledUnit) {
    // Extraction numbers types by first reach, so a parent's id is *lower* than its
    // children's — the rewrite therefore runs post-order rather than as a forward sweep.
    let mut interned: HashMap<Type, usize> = HashMap::new();
    let mut types: Vec<Type> = Vec::with_capacity(unit.types.len());
    // Only the type table moves. Every other table maps to itself — spelled out rather than
    // left absent, because `IdRemaps::map` treats a missing entry as a caller bug, which is
    // exactly what an unmapped id would be here.
    let mut remap = IdRemaps {
        constants: (0..unit.constants.len()).map(|id| (id, id)).collect(),
        functions: (0..unit.function_space()).map(|id| (id, id)).collect(),
        tuples: (0..unit.tuples.len()).map(|id| (id, id)).collect(),
        builtins: (0..unit.builtins.len()).map(|id| (id, id)).collect(),
        annotation_keys: (0..unit.annotation_keys.len()).map(|id| (id, id)).collect(),
        field_names: (0..unit.field_names.len()).map(|id| (id, id)).collect(),
        sites: (0..unit.sites.len()).map(|id| (id, id)).collect(),
        types: HashMap::new(),
    };

    // `(id, children queued)`. A node is visited once to queue what it references and once
    // to rewrite itself; `Type::Cycle` is a relative marker rather than an edge, so the graph
    // is a DAG and this terminates. Iterative, like every other walk over type structure.
    for root in 0..unit.types.len() {
        let mut stack = vec![(root, false)];
        while let Some((old, queued)) = stack.pop() {
            if remap.types.contains_key(&old) {
                continue;
            }
            if !queued {
                stack.push((old, true));
                stack.extend(
                    type_children(&unit.types[old])
                        .into_iter()
                        .filter(|child| !remap.types.contains_key(child))
                        .map(|child| (child, false)),
                );
                continue;
            }
            // An annotation row *is* its base once the row is gone. Reading the mapping
            // rather than the raw id handles a base that was itself spliced.
            if let Type::Annotated { base, .. } = &unit.types[old] {
                let target = remap.types[base];
                remap.types.insert(old, target);
                continue;
            }
            let rewritten = match unit.types[old].clone().remap_ids(&remap) {
                // One name for every variable, so alpha-equivalent rows intern together.
                Type::Variable(_) => Type::Variable("_".to_string()),
                other => other,
            };
            let new = *interned.entry(rewritten.clone()).or_insert_with(|| {
                types.push(rewritten);
                types.len() - 1
            });
            remap.types.insert(old, new);
        }
    }

    // Tuples keep their rows and their order — a tuple id is a runtime value tag carried by
    // `Opcode::Tuple` and `ConcreteType`, so merging them is a different question — but their
    // field types move with everything else.
    unit.tuples = unit
        .tuples
        .iter()
        .map(|info| info.remap_ids(&remap))
        .collect();
    unit.functions = unit
        .functions
        .drain(..)
        .map(|function| function.remap_ids(&remap))
        .collect();
    for builtin in &mut unit.builtins {
        builtin.type_argument = builtin.type_argument.map(|id| remap.types[&id]);
    }
    unit.types = types;
}

/// Extract the unit reachable from `entry_function`: a compiled fragment with no module
/// identity, in unit-local id space. Never fails: a module reference the session cannot
/// supply ([`Imports::Bundle`]) — or every module reference ([`Imports::Inline`]) — is
/// inlined, the walk claiming the module's functions as the unit's own and shaking them
/// to the entry.
///
/// `own_floor` is the function-table length before this compile. A function *no module
/// owns* below the floor would be an escaped reference — code of an earlier compile,
/// which nothing outside this session can name — and is asserted against: references to
/// earlier compiles' values go through session locals, never the function table, and a
/// caller extracting from a session that outlives one compile holds the program's
/// function dedup floor there (as `LineCompiler::prepare` does), so interning cannot
/// place one of this compile's own functions below it either. Module-owned functions
/// below the floor are ordinary: that is what inlining reaches.
///
/// A `None` entry seeds from every owned function, which is the unit form of a program
/// that does not evaluate to something runnable: nothing to enter at, but still
/// inspectable.
pub fn extract_unit(
    program: &Program,
    module_cache: &ModuleCache,
    entry_function: Option<usize>,
    own_floor: usize,
    imports: Imports,
) -> CompiledUnit {
    let eligible = match imports {
        Imports::Bundle => suppliable_modules(module_cache),
        Imports::Inline => HashSet::new(),
    };
    let owner = &Owner::Unit {
        eligible: &eligible,
        entry: entry_function,
    };
    let mut closure = Closure::default();
    let mut queue: Vec<Item> = Vec::new();
    match entry_function {
        Some(entry) => add_function(entry, &mut closure, &mut queue),
        None => {
            for id in 0..program.get_functions().len() {
                if matches!(classify(id, owner, module_cache), FunctionClass::Own) {
                    add_function(id, &mut closure, &mut queue);
                }
            }
        }
    }
    if drain(&mut queue, &mut closure, owner, program, module_cache).is_err() {
        unreachable!("a unit owns every function no module claims, so nothing is foreign");
    }

    let mut own_ids: Vec<usize> = Vec::new();
    let mut import_entries: Vec<(ModuleId, usize, usize)> = Vec::new();
    for &function_id in closure.functions.iter() {
        match classify(function_id, owner, module_cache) {
            FunctionClass::Own => {
                assert!(
                    function_id >= own_floor
                        || module_cache.function_owners.contains_key(&function_id),
                    "function {function_id} was registered before this compile yet no \
                     module owns it — an escaped reference"
                );
                own_ids.push(function_id)
            }
            FunctionClass::Import(module, index) => {
                import_entries.push((module, index, function_id))
            }
            FunctionClass::Foreign => unreachable!("a unit has no foreign class"),
        }
    }
    // Ascending session id is registration order, which is topological: a function can
    // only reference ones registered before it.
    own_ids.sort_unstable();

    import_entries.sort();
    let mut imports: Vec<(ModuleId, UnitKey, Vec<usize>)> = Vec::new();
    for (module, dep_index, _) in &import_entries {
        match imports.last_mut() {
            Some((last, _, indices)) if last == module => indices.push(*dep_index),
            _ => {
                // Guaranteed for an eligible module: suppliability required its stored
                // artifact, which is where the content key lives.
                let key = module_cache
                    .content_key(module)
                    .expect("a bundled import names a module without a stored artifact");
                imports.push((module.clone(), key, vec![*dep_index]));
            }
        }
    }
    // Key-only entries for dependencies this unit read but reaches no function of (see
    // the module extraction's twin loop): its code was baked against these versions, so
    // validation must still see them. A non-suppliable one is inlined territory — its
    // semantics travel with the unit exactly as inlined functions do — so it needs no
    // entry.
    for read in module_cache.value_reads_of(None) {
        if !eligible.contains(&read) || imports.iter().any(|(module, _, _)| *module == read) {
            continue;
        }
        let key = module_cache
            .content_key(&read)
            .expect("a suppliable module has a stored artifact");
        imports.push((read, key, vec![]));
    }

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
    let builtins: Vec<ArtifactBuiltin> = closure
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

    let entry = entry_function.map(|entry| {
        own_ids
            .iter()
            .position(|&id| id == entry)
            .expect("the entry is always classified as the unit's own")
    });

    let unit = CompiledUnit {
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
            .map(|&id| program.get_constants()[id].clone().remap_ids(&remaps))
            .collect(),
        annotation_keys: closure
            .annotation_keys
            .iter()
            .map(|&id| program.get_annotation_keys()[id].clone())
            .collect(),
        field_names: closure
            .field_names
            .iter()
            .map(|&id| program.get_field_names()[id].clone())
            .collect(),
        functions: own_ids
            .iter()
            .map(|&id| program.get_functions()[id].clone().remap_ids(&remaps))
            .collect(),
        imports,
        builtins,
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
        entry,
    };
    verify(&unit, "unit");
    unit
}

enum Item {
    Type(usize),
    Tuple(usize),
    Constant(usize),
    Function(usize),
    Site(usize),
    Builtin(usize),
}

/// A constant, and — because a composite names its children by index — everything under
/// it. A constant reached only as another's field would otherwise be missing from the unit.
fn add_constant(constant_id: usize, closure: &mut Closure, queue: &mut Vec<Item>) {
    if closure.constants.insert(constant_id) {
        queue.push(Item::Constant(constant_id));
    }
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
        | Type::Variable(_)
        | Type::Top => {}
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
            ..
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
            add_constant(*constant_id, closure, queue);
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
        Opcode::Constant => add_constant(id, closure, queue),
        Opcode::Function => add_function(id, closure, queue),
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

/// Fail-fast self-containment check on extraction output: a violation means the
/// extraction closure missed something, which is a compiler bug.
fn verify(unit: &CompiledUnit, label: &str) {
    if let Err(message) = validate_unit(unit, label) {
        panic!("{message}");
    }
}

/// One unit-local reference a type entry makes.
#[derive(Clone, Copy)]
enum TypeRef {
    Type(usize),
    Tuple(usize),
    AnnotationKey(usize),
}

/// A node of the type graph — the two tables that reference each other.
#[derive(Clone, Copy)]
enum TypeNode {
    Type(usize),
    Tuple(usize),
}

impl std::fmt::Display for TypeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeNode::Type(id) => write!(f, "type {id}"),
            TypeNode::Tuple(id) => write!(f, "tuple {id}"),
        }
    }
}

/// Everything a type entry refers to. One walk drives both the range check and the
/// acyclicity search below, so neither can drift from the other — or from
/// `intern_types_and_tuples`, whose recursion follows exactly these edges.
fn type_refs(ty: &Type) -> Vec<TypeRef> {
    match ty {
        Type::Tuple(tuple_id) => vec![TypeRef::Tuple(*tuple_id)],
        Type::Partial { fields, .. } => fields
            .iter()
            .map(|(_, type_id)| TypeRef::Type(*type_id))
            .collect(),
        Type::Callable {
            parameter,
            result,
            receive,
            states,
            ..
        } => [parameter, result, receive]
            .into_iter()
            .chain(states.iter())
            .map(|type_id| TypeRef::Type(*type_id))
            .collect(),
        Type::Union(members) => members.iter().map(|id| TypeRef::Type(*id)).collect(),
        Type::Annotated { base, entries, .. } => std::iter::once(TypeRef::Type(*base))
            .chain(
                entries
                    .iter()
                    .flat_map(|(key, value)| [TypeRef::AnnotationKey(*key), TypeRef::Type(*value)]),
            )
            .collect(),
        Type::Process {
            send,
            receive,
            state,
        } => [send, receive, state]
            .into_iter()
            .flatten()
            .map(|type_id| TypeRef::Type(*type_id))
            .collect(),
        Type::Integer
        | Type::Binary
        | Type::Reference
        | Type::Cycle(_)
        | Type::Resource(_)
        | Type::Variable(_)
        | Type::Top => Vec::new(),
    }
}

/// The type and tuple tables must form a DAG: a recursive type is expressed with
/// `Type::Cycle`, a marker relative to the enclosing binders, never as a cycle between
/// table entries. [`link_unit`] interns a type's children before memoising it, so a
/// cyclic table recurses until the native stack overflows — an abort, which no host can
/// catch. Ids must already be range-checked.
fn check_types_acyclic(unit: &CompiledUnit, label: &str) -> Result<(), String> {
    // Per node: unvisited, on the stack (an ancestor), or fully explored.
    const UNVISITED: u8 = 0;
    const OPEN: u8 = 1;
    const DONE: u8 = 2;
    let mut type_state = vec![UNVISITED; unit.types.len()];
    let mut tuple_state = vec![UNVISITED; unit.tuples.len()];
    // `(node, expanded)`: an unexpanded entry queues its children above its own expanded
    // marker, which pops once they are all explored.
    let mut stack: Vec<(TypeNode, bool)> = Vec::new();

    let roots = (0..unit.types.len())
        .map(TypeNode::Type)
        .chain((0..unit.tuples.len()).map(TypeNode::Tuple));
    for root in roots {
        stack.push((root, false));
        while let Some((node, expanded)) = stack.pop() {
            let state = match node {
                TypeNode::Type(id) => &mut type_state[id],
                TypeNode::Tuple(id) => &mut tuple_state[id],
            };
            if expanded {
                *state = DONE;
                continue;
            }
            match *state {
                DONE => continue,
                OPEN => {
                    return Err(format!(
                        "{label}: {node} is its own descendant (the type table must be acyclic)"
                    ));
                }
                _ => *state = OPEN,
            }
            stack.push((node, true));
            match node {
                TypeNode::Type(id) => {
                    for reference in type_refs(&unit.types[id]) {
                        match reference {
                            TypeRef::Type(child) => stack.push((TypeNode::Type(child), false)),
                            TypeRef::Tuple(child) => stack.push((TypeNode::Tuple(child), false)),
                            TypeRef::AnnotationKey(_) => {}
                        }
                    }
                }
                TypeNode::Tuple(id) => {
                    for (_, type_id) in &unit.tuples[id].fields {
                        stack.push((TypeNode::Type(*type_id), false));
                    }
                }
            }
        }
    }
    Ok(())
}

/// Structural well-formedness of a unit: every reference inside it must land inside its
/// tables, the type graph must be acyclic, function references must point strictly
/// backward, and the entry (when present) must be one of its own functions. This is the
/// check a *trust boundary* runs on a unit it did not produce — linking a violating unit
/// would panic mid-link, recurse until the native stack overflows, or silently corrupt a
/// session, and a host must refuse it as a bad request instead.
///
/// Scope: link-time soundness, plus the instruction operands whose own decoding is
/// unchecked. Validated bytecode can still misbehave at runtime (stack discipline is not
/// analysed here); who may submit code at all is the host's authentication model.
pub fn validate_unit(unit: &CompiledUnit, label: &str) -> Result<(), String> {
    let function_space = unit.function_space();
    let check = |what: &str, id: usize, len: usize| -> Result<(), String> {
        if id < len {
            Ok(())
        } else {
            Err(format!(
                "{label}: {what} reference {id} outside table (len {len})"
            ))
        }
    };
    for ty in &unit.types {
        // Binders count from 1 — `Cycle(0)` names nothing, and the walks that resolve a
        // marker against the binder stack index past its end.
        if matches!(ty, Type::Cycle(0)) {
            return Err(format!(
                "{label}: a cycle marker names binder 0, but binders count from 1"
            ));
        }
        for reference in type_refs(ty) {
            match reference {
                TypeRef::Type(id) => check("type", id, unit.types.len())?,
                TypeRef::Tuple(id) => check("tuple", id, unit.tuples.len())?,
                TypeRef::AnnotationKey(id) => {
                    check("annotation key", id, unit.annotation_keys.len())?
                }
            }
        }
    }
    for info in &unit.tuples {
        for (_, type_id) in &info.fields {
            check("type", *type_id, unit.types.len())?;
        }
    }
    check_types_acyclic(unit, label)?;
    for builtin in &unit.builtins {
        if let Some(type_argument) = builtin.type_argument {
            check("type", type_argument, unit.types.len())?;
        }
    }
    // A composite constant's references must resolve inside the unit: children into the
    // constants table, and the tuple/function/builtin/key ids it names into theirs.
    for (position, constant) in unit.constants.iter().enumerate() {
        for child in constant.children() {
            check("constant", child, unit.constants.len())
                .map_err(|error| format!("{error} (from constant {position})"))?;
        }
        match constant {
            Constant::Tuple { id, .. } => check("tuple", *id, unit.tuples.len())?,
            Constant::Function { id, .. } => check("function", *id, function_space)?,
            Constant::Builtin { id } => check("builtin", *id, unit.builtins.len())?,
            Constant::Annotated { entries, .. } => {
                for (key, _) in entries {
                    check("annotation key", *key, unit.annotation_keys.len())?;
                }
            }
            Constant::Integer(_) | Constant::Binary(_) => {}
        }
    }

    for (position, function) in unit.functions.iter().enumerate() {
        check("type", function.type_id, unit.types.len())?;
        // Captures become the frame's leading locals, which instructions name through a
        // 24-bit operand: past that they are unaddressable, and the closure build would
        // reserve a nonsense allocation before discovering the stack is short.
        if function.captures > Instruction::OPERAND_MAX {
            return Err(format!(
                "{label}: function {position} declares {} captures, past the {} a local slot can name",
                function.captures,
                Instruction::OPERAND_MAX
            ));
        }
        for instruction in &function.instructions {
            let id = instruction.operand() as usize;
            match instruction.opcode() {
                Opcode::Constant => check("constant", id, unit.constants.len())?,
                Opcode::Function => {
                    check("function", id, function_space)?;
                    // The linked program's function table must reference strictly
                    // backward (the environment merge rewrites single-pass): an own
                    // function may reference earlier own functions or any import —
                    // imports are always linked first.
                    if id >= position && id < unit.functions.len() {
                        return Err(format!(
                            "{label}: function {position} references unregistrable function {id}"
                        ));
                    }
                }
                Opcode::Tuple => check("tuple", id, unit.tuples.len())?,
                Opcode::IsType => check("type", id, unit.types.len())?,
                Opcode::GetNamed => check("field name", id, unit.field_names.len())?,
                Opcode::Annotate | Opcode::GetAnnotation => {
                    check("annotation key", id, unit.annotation_keys.len())?
                }
                Opcode::Stamp => check("site", id, unit.sites.len())?,
                // Not a table id, but not free either: a rotate names the top `id` stack
                // slots, and fewer than two is not an operation — a rotate of nothing
                // reaches one slot past the top of the stack.
                Opcode::Rotate if id < 2 => {
                    return Err(format!(
                        "{label}: function {position} rotates {id} stack slots (a rotate names at least 2)"
                    ));
                }
                _ => {}
            }
        }
    }
    for site in &unit.sites {
        check("constant", site.module_constant, unit.constants.len())?;
    }
    if let Some(entry) = unit.entry {
        check("entry", entry, unit.functions.len())?;
    }
    Ok(())
}

/// How a unit's own functions enter the session's function table.
#[derive(Clone, Copy, PartialEq)]
pub enum Registration {
    /// Append, never collapsing onto structurally identical entries. A module's
    /// functions must stay its own regardless of session history, or its identity —
    /// and every import index pointing at it — would depend on what compiled first.
    Append,
    /// Structural interning. Correct only where there is no attribution to protect:
    /// a unit with no module identity, whose functions nothing else names.
    Intern,
}

/// Link a unit's tables and functions into a session, answering the id remap.
///
/// `resolved_imports` supplies one session function id per entry of [`CompiledUnit::imports`],
/// in the same flattened order — the caller decides how an import resolves, which is what
/// keeps this usable by a runtime that has no `ModuleCache`. [`resolve_imports`] builds it
/// from a compiler session; a host holding its own `key → functions` map builds it from
/// that.
pub fn link_unit<E: Effect>(
    unit: &CompiledUnit,
    label: &str,
    program: &mut Program,
    resolved_imports: &[usize],
    builtins: &BuiltinRegistry<E>,
    registration: Registration,
) -> Result<IdRemaps, Error> {
    // Re-intern the value-like tables, building the artifact-local → session remap.
    // Types and tuples are mutually recursive, so intern on demand with memoisation
    // (references form a DAG — recursion markers are relative, never table cycles).
    //
    // Constants come last of the value-like tables, with the functions: a constant names
    // tuple, builtin and annotation-key ids, so those remaps must be complete before one is
    // rewritten — and constants and functions reference *each other* (a closure constant
    // names a function; a function body names constants), so the two link interleaved in
    // dependency order rather than one table after the other.
    let mut remaps = IdRemaps::default();
    for (local, key) in unit.annotation_keys.iter().enumerate() {
        let session = program.register_annotation_key(key);
        remaps.annotation_keys.insert(local, session);
    }
    for (local, name) in unit.field_names.iter().enumerate() {
        let session = program.register_field_name(name);
        remaps.field_names.insert(local, session);
    }
    intern_types_and_tuples(unit, program, &mut remaps);
    // Every linked tuple must also have its `Type::Tuple` wrapper entry: the runtime
    // compatibility tables represent a concrete tuple by that entry, so a tuple
    // without one is invisible to every `IsType` test. A from-source compile
    // registers the wrapper while typing the construction, but the artifact closure
    // only carries types the module's code references by type id — a tuple that is
    // constructed yet never referenced as a type would otherwise arrive untestable.
    for local in 0..unit.tuples.len() {
        program.register_type(Type::Tuple(remaps.tuples[&local]));
    }
    // Builtins resolve by name against the host registry — the link-time capability
    // check: a host that doesn't provide a builtin refuses the module.
    for (local, builtin) in unit.builtins.iter().enumerate() {
        if builtins.get_specs(&builtin.name).is_none() {
            return Err(Error::FeatureUnsupported(format!(
                "{} requires builtin '{}', which this host does not provide",
                label, builtin.name
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

    // Imports continue the id space after the unit's own functions, in the flattened
    // order `resolved_imports` mirrors.
    let own_count = unit.functions.len();
    assert_eq!(
        resolved_imports.len(),
        unit.function_space() - own_count,
        "{label}: {} resolved imports for {} entries",
        resolved_imports.len(),
        unit.function_space() - own_count
    );
    for (position, &session) in resolved_imports.iter().enumerate() {
        remaps.functions.insert(own_count + position, session);
    }

    // Own functions register in order: each may reference only earlier own functions or
    // imports, so the remap is complete before it is rewritten. Checked explicitly —
    // `IdRemaps::map` falls back to identity, so a forward reference would silently
    // mis-link rather than fail. `verify` vouches for this at extraction; a stored unit
    // could still arrive corrupted.
    //
    // Registering in order (rather than precomputing ids and appending) is what lets
    // `Intern` collapse onto an existing entry, and is sound for both policies because
    // the references point strictly backward.
    // Sites, and the constants they name, link *before* the functions: `Function::remap_ids`
    // rewrites `Stamp` operands through `remaps.sites`, and a missing entry falls back to
    // identity — which would silently point a stamp at whatever site already sits at that
    // index in the session. A site names one constant, the module display name, which is a
    // leaf binary, so linking those ahead of the interleaved pass costs nothing and cannot
    // pull a function in with it. The pass below re-registers them idempotently.
    for site in &unit.sites {
        let constant = unit.constants[site.module_constant].clone();
        debug_assert!(
            constant.children().is_empty() && constant.function().is_none(),
            "{label}: a site's constant must be a leaf"
        );
        let session = program.register_constant(constant.remap_ids(&remaps));
        remaps.constants.insert(site.module_constant, session);
    }
    for (local, site) in unit.sites.iter().enumerate() {
        let session = program.register_debug_site(Site {
            module_constant: remaps.constants[&site.module_constant],
            ..site.clone()
        });
        remaps.sites.insert(local, session);
    }

    for item in link_order(unit, label)? {
        match item {
            LinkItem::Constant(local) => {
                let remapped = unit.constants[local].clone().remap_ids(&remaps);
                let session = program.register_constant(remapped);
                remaps.constants.insert(local, session);
            }
            LinkItem::Function(local) => {
                let function = &unit.functions[local];
                for instruction in &function.instructions {
                    if instruction.opcode() == Opcode::Function {
                        let target = instruction.operand() as usize;
                        assert!(
                            remaps.functions.contains_key(&target),
                            "{label}: function {local} references function {target} before it \
                             is linked — the backward-reference contract does not hold"
                        );
                    }
                }
                let remapped = function.clone().remap_ids(&remaps);
                let session = match registration {
                    Registration::Append => program.push_function(remapped),
                    Registration::Intern => program.register_function(remapped),
                };
                remaps.functions.insert(local, session);
            }
        }
    }

    Ok(remaps)
}

/// One entry of a unit's link order: the constants and functions tables reference each
/// other, so they link as a single interleaved sequence.
#[derive(Clone, Copy, PartialEq, Debug)]
enum LinkItem {
    Constant(usize),
    Function(usize),
}

/// The order in which a unit's constants and own functions can be linked: every entry
/// after everything it references.
///
/// Derived rather than stored. The tables already carry the edges, so computing the order
/// keeps `CompiledUnit` unchanged — no new field to change every unit's content key — and
/// the traversal doubles as the acyclicity check, which is a stronger guarantee than a
/// stored order could be validated against.
///
/// Functions are the first roots, in unit order, so a function still registers before any
/// later one. That is what `Registration::Intern` relies on to collapse onto an existing
/// entry, and what keeps the backward-reference contract meaningful; a constant is pulled in
/// ahead of the first function that needs it.
fn link_order(unit: &CompiledUnit, label: &str) -> Result<Vec<LinkItem>, Error> {
    // Per node: unvisited, on the stack (an ancestor), or emitted.
    const UNVISITED: u8 = 0;
    const OPEN: u8 = 1;
    const EMITTED: u8 = 2;
    let mut constant_state = vec![UNVISITED; unit.constants.len()];
    let mut function_state = vec![UNVISITED; unit.functions.len()];
    let mut order = Vec::with_capacity(unit.constants.len() + unit.functions.len());
    // `(item, expanded)`: an unexpanded entry queues its dependencies above its own
    // expanded marker, so the marker pops once they are all emitted.
    let mut stack: Vec<(LinkItem, bool)> = Vec::new();

    let roots = (0..unit.functions.len())
        .map(LinkItem::Function)
        .chain((0..unit.constants.len()).map(LinkItem::Constant));

    for root in roots {
        stack.push((root, false));
        while let Some((item, expanded)) = stack.pop() {
            let state = match item {
                LinkItem::Constant(index) => &mut constant_state[index],
                LinkItem::Function(index) => &mut function_state[index],
            };
            if expanded {
                *state = EMITTED;
                order.push(item);
                continue;
            }
            match *state {
                EMITTED => continue,
                OPEN => {
                    return Err(Error::FeatureUnsupported(format!(
                        "{label}: constants and functions form a cycle at {item:?}"
                    )));
                }
                _ => *state = OPEN,
            }
            stack.push((item, true));
            match item {
                LinkItem::Constant(index) => {
                    let constant = &unit.constants[index];
                    for child in constant.children() {
                        stack.push((LinkItem::Constant(child), false));
                    }
                    // An imported function is already linked; only an own one is ordered here.
                    if let Some(function) = constant.function()
                        && function < unit.functions.len()
                    {
                        stack.push((LinkItem::Function(function), false));
                    }
                }
                LinkItem::Function(index) => {
                    for instruction in &unit.functions[index].instructions {
                        if instruction.opcode() == Opcode::Constant {
                            let constant = instruction.operand() as usize;
                            stack.push((LinkItem::Constant(constant), false));
                        }
                    }
                }
            }
        }
    }
    Ok(order)
}

/// Why a unit's imports could not be resolved against a host's linked-module map.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportResolveError {
    /// A key the host does not hold: send that module's unit and retry.
    Missing(UnitKey),
    /// An import entry names a function index past the end of the module linked under
    /// its key. The citing unit is malformed: content keys are validated at the
    /// boundary, so no resend could change what the key holds.
    IndexOutOfRange {
        key: UnitKey,
        index: usize,
        len: usize,
    },
}

impl std::fmt::Display for ImportResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportResolveError::Missing(key) => write!(f, "module {key} is not linked"),
            ImportResolveError::IndexOutOfRange { key, index, len } => write!(
                f,
                "module {key} holds {len} functions but an import names index {index}"
            ),
        }
    }
}

/// Resolve a unit's import entries through a `content key → its own functions' session
/// ids` map, as a runtime holding linked modules keeps. Both failures are the caller's
/// to answer: a missing key means "send that module and retry"; an out-of-range index
/// means the citing unit itself is malformed and must be refused.
pub fn resolve_imports_from(
    unit: &CompiledUnit,
    linked: &HashMap<UnitKey, Vec<usize>>,
) -> Result<Vec<usize>, ImportResolveError> {
    let mut resolved = Vec::with_capacity(unit.function_space() - unit.functions.len());
    for (_, key, indices) in &unit.imports {
        let map = linked.get(key).ok_or(ImportResolveError::Missing(*key))?;
        for index in indices {
            resolved.push(*map.get(*index).ok_or(ImportResolveError::IndexOutOfRange {
                key: *key,
                index: *index,
                len: map.len(),
            })?);
        }
    }
    Ok(resolved)
}

/// Link a self-contained [`CompiledProgram`] into `program` — its modules first, in the
/// order given, then the entry unit — answering the session id of the entry, or `None` for
/// a program that has none.
///
/// Everything interns: a program linked this way has no module attribution to protect,
/// and nothing else will name its functions by index.
pub fn link_program<E: Effect>(
    compiled: &CompiledProgram,
    program: &mut Program,
    builtins: &BuiltinRegistry<E>,
) -> Result<Option<usize>, Error> {
    let mut linked: HashMap<UnitKey, Vec<usize>> = HashMap::new();
    for (key, unit) in &compiled.modules {
        let label = format!("module {key}");
        let resolved = resolve_imports_from(unit, &linked).map_err(|error| {
            Error::FeatureUnsupported(format!("{label}: import resolution failed: {error}"))
        })?;
        let remaps = link_unit(
            unit,
            &label,
            program,
            &resolved,
            builtins,
            Registration::Intern,
        )?;
        linked.insert(
            *key,
            (0..unit.functions.len())
                .map(|local| remaps.functions[&local])
                .collect(),
        );
    }
    let resolved = resolve_imports_from(&compiled.unit, &linked).map_err(|error| {
        Error::FeatureUnsupported(format!("the program's import resolution failed: {error}"))
    })?;
    let remaps = link_unit(
        &compiled.unit,
        "program",
        program,
        &resolved,
        builtins,
        Registration::Intern,
    )?;
    Ok(compiled.unit.entry.map(|entry| remaps.functions[&entry]))
}

/// Resolve a unit's imports against a compiler session: every named module must already
/// be linked with its function map recorded, which the import pipeline guarantees for
/// both linked and source-compiled dependencies — so a missing map is an invariant
/// violation, not a fallback case.
pub fn resolve_imports(unit: &CompiledUnit, module_cache: &ModuleCache, label: &str) -> Vec<usize> {
    let mut resolved = Vec::with_capacity(unit.function_space() - unit.functions.len());
    for (module, key, dep_own_indices) in &unit.imports {
        // The name resolves a module; the key resolves a *version* of it. A session may
        // hold two modules with the same name (different projects, or a project and its
        // dependency), so linking the wrong one is a real possibility rather than a
        // theoretical one — and it would corrupt silently, since function indices line up
        // either way.
        if let Some(linked) = module_cache.content_key(module) {
            assert_eq!(
                linked,
                *key,
                "linking {label}: import {} names content {key} but the session linked {linked}",
                module.display()
            );
        }
        let dep_map = module_cache
            .module_functions
            .get(module)
            .unwrap_or_else(|| {
                panic!(
                    "linking {label}: dependency {} is cached without a function map — \
                     the import pipeline must record one for every cached module",
                    module.display()
                )
            });
        resolved.extend(dep_own_indices.iter().map(|index| dep_map[*index]));
    }
    resolved
}

/// Link an artifact into a session, leaving `module_cache` holding the module exactly as
/// a from-source compile would. `expected` is the module the caller resolved and keyed —
/// a mismatch means a key collision or a corrupted store file, caught here rather than
/// silently linking an unrequested module.
pub fn link_module<E: Effect>(
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
    let label = artifact.id.display();
    let resolved = resolve_imports(&artifact.unit, module_cache, &label);
    let remaps = link_unit(
        &artifact.unit,
        &label,
        program,
        &resolved,
        builtins,
        Registration::Append,
    )?;
    let own_map: Vec<usize> = (0..artifact.unit.functions.len())
        .map(|local| remaps.functions[&local])
        .collect();
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
    module_cache.record_value_closure(&artifact.id, artifact.unit.dependencies());

    // Install the cached module and namespace, exactly as a from-source compile would
    // have left them.
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
fn intern_types_and_tuples(unit: &CompiledUnit, program: &mut Program, remaps: &mut IdRemaps) {
    // Driven by an explicit stack rather than by recursion. A unit's type graph is as deep
    // as the values it describes, and a `%list{ … }` literal's is one level per element, so
    // recursing per node would let a long literal exhaust the native stack — here, in the
    // process that links units on behalf of every session.
    //
    // Post-order: a node is expanded, pushing its children above its own registration, so
    // every child is in `remaps` before the parent's ids are rewritten through it.
    // `validate_unit` rejects a cyclic type graph, which is what makes that well-defined;
    // `expanded` only stops a cycle that slipped through from looping here forever, and
    // `IdRemaps::map` then fails loudly on the child that never registered.
    enum Job {
        Expand(TypeRef),
        Register(TypeRef),
    }

    let mut jobs: Vec<Job> = Vec::new();
    let mut expanded_types = vec![false; unit.types.len()];
    let mut expanded_tuples = vec![false; unit.tuples.len()];

    let seeds = (0..unit.types.len())
        .map(TypeRef::Type)
        .chain((0..unit.tuples.len()).map(TypeRef::Tuple));

    for seed in seeds {
        jobs.push(Job::Expand(seed));
        while let Some(job) = jobs.pop() {
            match job {
                Job::Expand(TypeRef::Type(local)) => {
                    if remaps.types.contains_key(&local)
                        || std::mem::replace(&mut expanded_types[local], true)
                    {
                        continue;
                    }
                    jobs.push(Job::Register(TypeRef::Type(local)));
                    // Annotation keys are their own table, interned elsewhere.
                    jobs.extend(
                        type_refs(&unit.types[local])
                            .into_iter()
                            .filter(|node| !matches!(node, TypeRef::AnnotationKey(_)))
                            .map(Job::Expand),
                    );
                }
                Job::Expand(TypeRef::Tuple(local)) => {
                    if remaps.tuples.contains_key(&local)
                        || std::mem::replace(&mut expanded_tuples[local], true)
                    {
                        continue;
                    }
                    jobs.push(Job::Register(TypeRef::Tuple(local)));
                    jobs.extend(
                        unit.tuples[local]
                            .fields
                            .iter()
                            .map(|(_, type_id)| Job::Expand(TypeRef::Type(*type_id))),
                    );
                }
                Job::Register(TypeRef::Type(local)) => {
                    let session = program.register_type(unit.types[local].remap_ids(remaps));
                    remaps.types.insert(local, session);
                }
                Job::Register(TypeRef::Tuple(local)) => {
                    let info = &unit.tuples[local];
                    let session =
                        program.register_tuple(info.name.clone(), info.remap_ids(remaps).fields);
                    remaps.tuples.insert(local, session);
                }
                Job::Expand(TypeRef::AnnotationKey(_))
                | Job::Register(TypeRef::AnnotationKey(_)) => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A scratch directory removed on drop, so a failing assertion cannot strand it.
    struct Scratch(PathBuf);

    impl Scratch {
        fn new(label: &str) -> Self {
            let dir = std::env::temp_dir().join(format!(
                "quiver-artifact-test-{label}-{}",
                std::process::id()
            ));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).expect("scratch dir");
            Scratch(dir)
        }
    }

    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn at_dir_scopes_entries_by_fingerprint() {
        let scratch = Scratch::new("at-dir");
        let _store = ArtifactStore::at_dir(scratch.0.clone());
        assert!(
            scratch.0.join(compiler_fingerprint()).is_dir(),
            "a disk-backed store must root its entries under the compiler fingerprint"
        );
    }

    #[test]
    fn opening_the_cache_cleans_other_versions_and_the_flat_layout() {
        let scratch = Scratch::new("prune");
        let base = scratch.0.join("artifacts");

        // Another compiler version's directory, a flat-layout file, a legacy bundle
        // beside the base — all garbage — and a fresh entry of the current version.
        let sibling = base.join("0123456789abcdef");
        std::fs::create_dir_all(&sibling).unwrap();
        std::fs::write(sibling.join("aaaaaaaaaaaaaaaa.json"), b"{}").unwrap();
        std::fs::write(base.join("bbbbbbbbbbbbbbbb.json"), b"{}").unwrap();
        std::fs::write(scratch.0.join("std-image-cccc.json"), b"{}").unwrap();
        let current = base.join(compiler_fingerprint());
        std::fs::create_dir_all(&current).unwrap();
        std::fs::write(current.join("dddddddddddddddd.json"), b"{}").unwrap();

        prune(&base);

        assert!(!sibling.exists(), "another version's directory is removed");
        assert!(
            !base.join("bbbbbbbbbbbbbbbb.json").exists(),
            "flat-layout files are removed"
        );
        assert!(
            !scratch.0.join("std-image-cccc.json").exists(),
            "legacy bundle caches are removed"
        );
        assert!(
            current.join("dddddddddddddddd.json").exists(),
            "the current version's fresh entries survive"
        );
    }
}
