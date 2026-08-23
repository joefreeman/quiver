use std::collections::HashMap;

use crate::{
    ast, parser,
    resolver::{ModuleId, ModuleResolver, PackageId},
};
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_core::value::Value;

use super::{Error, Scope, ScopeKind, scopes, typing, typing::TypeAliasDef};

/// A cached module value. The value carries its own binary bytes, so nothing has to be
/// extracted alongside it.
#[derive(Clone)]
pub struct CachedModule {
    pub value: Value,
    pub module_type: Type,
    /// Return-type dispatch tables produced while compiling this module (and its nested imports),
    /// keyed by function index. A value-cached module is not recompiled when reused, so these are
    /// restored into the compiler on a cache hit — otherwise a freshly-compiled caller couldn't
    /// specialise the result type of this module's dispatch functions (e.g. `num.add`) and would
    /// fall back to their widened frozen result. Type/function IDs stay valid across REPL lines
    /// because the program grows append-only.
    pub fn_case_tables: HashMap<usize, Vec<(usize, usize)>>,
    /// Companion to [`Self::fn_case_tables`]: maps a callable *type* ID to the dispatch function
    /// to use when a call's callee isn't statically known.
    pub case_tables: HashMap<usize, usize>,
    /// Declared type parameters of the module's generic callables (uniquified variable names,
    /// declaration order), restored like the dispatch tables so a caller can explicitly
    /// instantiate an imported generic (`%list.map<'int>`) without recompiling the module.
    pub callable_type_params: HashMap<usize, Vec<String>>,
}

/// A module-compile recording frame (see [`ModuleCache::recording`]): tracks which
/// modules this compile was *declared* to reference (per [`collect_module_references`],
/// the same set its artifact key hashes) so any other reference discovered mid-compile
/// — an import surfacing only inside a dialect expansion — marks the module hidden:
/// its key would not cover the hidden dependency, so it must not be stored.
#[derive(Clone)]
pub struct RecordingFrame {
    pub id: ModuleId,
    pub declared: std::collections::HashSet<ModuleId>,
    pub hidden: bool,
}

#[derive(Clone)]
pub struct ModuleCache {
    pub ast_cache: HashMap<ModuleId, ast::Sequence>,
    pub import_stack: Vec<ModuleId>,
    /// Cache for module values with their types and extracted binary data.
    /// With capture-by-value, function indices can be reused, making this cache valid.
    pub value_cache: HashMap<ModuleId, CachedModule>,
    /// Cache of resolved type namespaces (default + named types) per module. The type IDs
    /// are valid for the lifetime of the `Program` being compiled.
    pub type_namespace_cache: HashMap<ModuleId, ModuleTypeNamespace>,
    /// Modules whose type namespaces are currently being built, for cyclic-reference detection.
    pub type_namespace_stack: Vec<ModuleId>,
    /// The artifact store this session links modules from and extracts them into (see
    /// `crate::artifact`). None outside artifact-aware embedders.
    pub artifact_store: Option<std::rc::Rc<crate::artifact::ArtifactStore>>,
    /// Per cached module — linked *or* compiled from source — its own-function index →
    /// session function id. The forward map every artifact link resolves its function
    /// imports through; populated at link time and at source-compile time alike, so the
    /// two paths compose. Session-local, never serialized.
    pub module_functions: HashMap<ModuleId, Vec<usize>>,
    /// The reverse of [`Self::module_functions`]: session function id → (owning module,
    /// index in its own-function list). First writer wins, so a function `register_function`
    /// deduplicated across modules stays attributed to its first registrant. Used by
    /// artifact extraction to classify function references.
    pub function_owners: HashMap<usize, (ModuleId, usize)>,
    /// Per cached module: its *transitive value-import closure* — every module whose
    /// functions can be embedded in or referenced from its compiled form. This is the
    /// import-eligibility set for artifact extraction: each member is covered by the
    /// module's Merkle key (directly or through a dependency's key) and guaranteed
    /// linkable first, so an import entry naming it can never go stale or dangle. A
    /// function owned by any module *outside* the closure (reachable only through a
    /// hidden, key-invisible chain) makes the module uncacheable.
    pub value_closures: HashMap<ModuleId, std::collections::HashSet<ModuleId>>,
    /// In-progress module-compile frames, innermost last.
    pub recording: Vec<RecordingFrame>,
    /// Memoised artifact keys (see `crate::artifact::module_key`).
    pub key_cache: HashMap<ModuleId, u64>,
    /// Modules whose keys are currently being computed, for cycle detection.
    pub key_stack: Vec<ModuleId>,
    /// Modules whose *value* a compile read, attributed to the innermost in-progress
    /// module compile — `None` for the entry, which [`Self::begin_entry_reads`] scopes
    /// to one compile.
    ///
    /// Extraction derives import entries from surviving function references, but a read
    /// can bake a module's content into the reader with no reference left to find: an
    /// elided forwarder body, a data member reconstructed inline, a dialect's expansion.
    /// The reader still depends on that exact version, so extraction adds a key-only
    /// entry for every read it did not already name, and version validation covers it.
    ///
    /// Type-namespace loads are deliberately not reads: a unit carries its types by
    /// value, so nothing of the module's *code* is baked in, and the calls that would
    /// notice a drift are validated through their own function-reference entries.
    pub module_reads: HashMap<Option<ModuleId>, std::collections::HashSet<ModuleId>>,
}

impl Default for ModuleCache {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            ast_cache: HashMap::new(),
            import_stack: Vec::new(),
            value_cache: HashMap::new(),
            type_namespace_cache: HashMap::new(),
            type_namespace_stack: Vec::new(),
            artifact_store: None,
            module_functions: HashMap::new(),
            function_owners: HashMap::new(),
            value_closures: HashMap::new(),
            recording: Vec::new(),
            key_cache: HashMap::new(),
            key_stack: Vec::new(),
            module_reads: HashMap::new(),
        }
    }

    /// Record that the innermost module compile read `id`'s value (`None` = the entry).
    /// A module reading itself is not a dependency, so it is not recorded.
    pub fn note_value_read(&mut self, id: &ModuleId) {
        let frame = self.recording.last().map(|frame| frame.id.clone());
        if frame.as_ref() == Some(id) {
            return;
        }
        self.module_reads
            .entry(frame)
            .or_default()
            .insert(id.clone());
    }

    /// Start a fresh entry-compile read set. A cache outlives the compile that fills it
    /// — the REPL clones one per line and commits it back — so without this a line's
    /// unit would name the modules *earlier* lines read, putting session history in an
    /// artifact. Module frames need no such reset: a module is compiled and extracted
    /// once. The function dedup floor scopes a line's own functions the same way.
    pub fn begin_entry_reads(&mut self) {
        self.module_reads.remove(&None);
    }

    /// The modules `owner`'s compile read the value of (`None` = the entry), in
    /// canonical order.
    pub fn value_reads_of(&self, owner: Option<&ModuleId>) -> Vec<ModuleId> {
        let mut reads: Vec<ModuleId> = self
            .module_reads
            .get(&owner.cloned())
            .map(|set| set.iter().cloned().collect())
            .unwrap_or_default();
        reads.sort();
        reads
    }

    /// Record a module's transitive value-import closure: its direct value imports
    /// plus their closures (recorded before it — dependencies cache first).
    pub fn record_value_closure(
        &mut self,
        id: &ModuleId,
        direct: impl IntoIterator<Item = ModuleId>,
    ) {
        let mut closure = std::collections::HashSet::new();
        for dep in direct {
            if let Some(dep_closure) = self.value_closures.get(&dep) {
                closure.extend(dep_closure.iter().cloned());
            }
            closure.insert(dep);
        }
        self.value_closures.insert(id.clone(), closure);
    }

    /// Note that the innermost module compile referenced `id` (a value import, dialect,
    /// or type-namespace load). A reference the module's declared set does not cover
    /// marks it hidden — its artifact key would miss the dependency, so it must not be
    /// stored. No-op outside a module compile.
    pub fn note_module_reference(&mut self, id: &ModuleId) {
        if let Some(frame) = self.recording.last_mut()
            && frame.id != *id
            && !frame.declared.contains(id)
        {
            frame.hidden = true;
        }
    }

    /// Record ownership of the module's functions: every function in
    /// `start..functions_len` not already attributed belongs to `id` (nested module
    /// compiles and links attribute theirs first, so insert-if-absent attributes
    /// exactly the module's own — in registration order). Returns the own list.
    pub fn record_module_functions(
        &mut self,
        id: &ModuleId,
        start: usize,
        functions_len: usize,
    ) -> Vec<usize> {
        let own: Vec<usize> = (start..functions_len)
            .filter(|function_id| !self.function_owners.contains_key(function_id))
            .collect();
        for (index, function_id) in own.iter().enumerate() {
            self.function_owners
                .insert(*function_id, (id.clone(), index));
        }
        self.module_functions.insert(id.clone(), own.clone());
        own
    }

    /// The content key of the module the session holds under `id`: read from its stored
    /// artifact, resolved through the session's key cache. `None` when the module has no
    /// stored artifact — unkeyable, hidden, or uncacheable — in which case nothing can
    /// name it on a wire and callers inline it instead. The store's memory layer makes
    /// this a map lookup after the first load.
    pub fn content_key(&self, id: &ModuleId) -> Option<crate::artifact::UnitKey> {
        let key = self.key_cache.get(id)?;
        let store = self.artifact_store.as_ref()?;
        Some(store.load(*key)?.content_key)
    }

    /// Get cached module value
    pub fn get_cached_module(&self, id: &ModuleId) -> Option<&CachedModule> {
        self.value_cache.get(id)
    }

    /// Cache a module value with its type and extracted binary data
    pub fn cache_module(&mut self, id: ModuleId, cached: CachedModule) {
        self.value_cache.insert(id, cached);
    }

    pub fn load_and_cache_ast(
        &mut self,
        id: &ModuleId,
        source: &str,
    ) -> Result<ast::Sequence, Error> {
        if let Some(cached_ast) = self.ast_cache.get(id).cloned() {
            return Ok(cached_ast);
        }

        let parsed = parser::parse(source).map_err(|e| Error::ModuleParse {
            module: id.display(),
            error: Box::new(e),
        })?;
        // Strip/lift no-op blocks so a module compiles identically whether or not it has been
        // formatted (the formatter strips/keeps the same blocks). See `Compiler::compile`.
        let parsed = crate::simplify::normalize_blocks(
            parsed,
            &crate::simplify::Options {
                keep: &|_| false,
                lift: true,
                group_consequences: false,
            },
        );

        self.ast_cache.insert(id.clone(), parsed.clone());

        Ok(parsed)
    }
}

/// A module's type namespace: its nameless default type (`' = ...`), if any, plus its
/// named type aliases, each resolved to a `TypeAliasDef`. Referenced from other modules
/// as `'%mod` (default) and `'%mod.name` (named).
#[derive(Clone)]
pub struct ModuleTypeNamespace {
    pub default: Option<TypeAliasDef>,
    pub named: HashMap<String, TypeAliasDef>,
}

/// Build (or fetch from cache) the type namespace of a module. Type definitions in the
/// module are resolved against the module's own package (hermetic resolution); references
/// to further modules (`'%other`) inside them resolve recursively. Cyclic references are
/// rejected.
pub fn module_type_namespace(
    module: &[String],
    resolver: &dyn ModuleResolver,
    module_cache: &mut ModuleCache,
    from_package: &PackageId,
    program: &mut Program,
) -> Result<ModuleTypeNamespace, Error> {
    let resolved = resolver
        .resolve(from_package, module)
        .map_err(Error::ModuleLoad)?;
    let id = resolved.id.clone();
    // Before the cache check: cached or not, the innermost module compile depends on
    // this namespace, and an undeclared dependency must mark it hidden.
    module_cache.note_module_reference(&id);

    if let Some(namespace) = module_cache.type_namespace_cache.get(&id) {
        return Ok(namespace.clone());
    }
    if module_cache.type_namespace_stack.contains(&id) {
        return Err(Error::ModuleTypeCycle(module.join("/")));
    }

    let parsed = module_cache.load_and_cache_ast(&id, &resolved.source)?;

    module_cache.type_namespace_stack.push(id.clone());
    let result = build_type_namespace(&parsed, &resolved.package, resolver, module_cache, program);
    module_cache.type_namespace_stack.pop();
    let namespace = result?;

    module_cache
        .type_namespace_cache
        .insert(id, namespace.clone());
    Ok(namespace)
}

/// Resolve every type alias declared in a module into a `ModuleTypeNamespace`. Aliases are
/// resolved in order so that later definitions can reference earlier ones.
fn build_type_namespace(
    parsed: &ast::Sequence,
    package: &PackageId,
    resolver: &dyn ModuleResolver,
    module_cache: &mut ModuleCache,
    program: &mut Program,
) -> Result<ModuleTypeNamespace, Error> {
    // A module-local scope holding the aliases resolved so far, so they can reference
    // each other during resolution.
    let mut module_scope = vec![Scope::new(
        scopes::Bindings::default(),
        None,
        ScopeKind::Root,
    )];
    let mut default: Option<TypeAliasDef> = None;
    let mut named: HashMap<String, TypeAliasDef> = HashMap::new();

    // Only *top-level* aliases form the module's type namespace: one declared inside a block or
    // function body is scoped to it, and so is module-private.
    for step in &parsed.steps {
        let ast::Step::TypeAlias {
            name,
            type_parameters,
            type_definition,
            ..
        } = step
        else {
            continue;
        };

        // Create Type::Variable bindings for type parameters
        let mut bindings = HashMap::new();
        for param in type_parameters {
            let var_type_id = program.register_type(Type::Variable(param.clone()));
            bindings.insert(param.clone(), var_type_id);
        }

        // Resolve the type definition using the module scope built so far
        let mut env = typing::TypeEnv {
            resolver,
            module_cache: &mut *module_cache,
            package,
        };
        let type_id = typing::resolve_ast_type_with_bindings(
            &mut env,
            &module_scope,
            type_definition.clone(),
            program,
            &bindings,
        )?;

        let def = TypeAliasDef {
            parameters: type_parameters.clone(),
            type_id,
        };
        match name {
            Some(name) => {
                named.insert(name.clone(), def.clone());
                scopes::define_type_alias(&mut module_scope, name.clone(), def);
            }
            None => {
                // Bind under the reserved key too, so a bare `'` later in the module resolves.
                default = Some(def.clone());
                scopes::define_type_alias(
                    &mut module_scope,
                    typing::SELF_DEFAULT_KEY.to_string(),
                    def,
                );
            }
        }
    }

    Ok(ModuleTypeNamespace { default, named })
}

/// Collect every module path the unit references by value — bare imports and
/// import-rooted accesses (`%num`, `%num.add [..]`) and dialect invocations
/// (`%mod{…}`) — in source order, deduplicated on first occurrence. Type-level
/// references (`'%mod.t`) are excluded: resolving them builds only the module's
/// type namespace, which registers no functions. This drives link-before-compile:
/// the compiler fully compiles these modules before the unit's own body, so a
/// module's registrations form a contiguous run in the program rather than
/// interleaving with its importers'.
pub fn collect_value_imports(program: &ast::Sequence) -> Vec<(Vec<String>, ast::Spanned)> {
    collect(program, false)
}

/// Collect every module path the unit references at all — value imports and dialects
/// as [`collect_value_imports`], *plus* type-level references (`'%mod`, `'%mod.name`)
/// wherever a type can be written. This is the dependency set an artifact key hashes:
/// a module's compiled form depends on its type-referenced modules' definitions just
/// as on its value imports', so both must invalidate it.
pub fn collect_module_references(program: &ast::Sequence) -> Vec<(Vec<String>, ast::Spanned)> {
    collect(program, true)
}

fn collect(program: &ast::Sequence, types: bool) -> Vec<(Vec<String>, ast::Spanned)> {
    let mut collector = Collector {
        found: Vec::new(),
        types,
    };
    collector.steps(&program.steps);
    let mut seen = std::collections::HashSet::new();
    collector
        .found
        .retain(|(path, _)| seen.insert(path.clone()));
    collector.found
}

struct Collector {
    found: Vec<(Vec<String>, ast::Spanned)>,
    /// Whether to also collect type-level module references (`'%mod.t`). Off for
    /// link-before-compile (namespaces register no functions), on for artifact keys.
    types: bool,
}

impl Collector {
    fn block(&mut self, block: &ast::Block) {
        for annotation in &block.annotations {
            self.chain(&annotation.value);
        }
        for branch in &block.branches {
            self.sequence(&branch.condition);
            if let Some(consequence) = &branch.consequence {
                self.sequence(consequence);
            }
        }
    }

    fn sequence(&mut self, sequence: &ast::Sequence) {
        self.steps(&sequence.steps);
    }

    /// A type alias anywhere — top level or nested in a block — may name a module type, so
    /// its definition is walked for references just like a chain's terms.
    fn steps(&mut self, steps: &[ast::Step]) {
        for step in steps {
            match step {
                ast::Step::Chain(chain) => self.chain(chain),
                ast::Step::TypeAlias {
                    type_definition, ..
                } => self.type_def(type_definition),
            }
        }
    }

    fn chain(&mut self, chain: &ast::Chain) {
        if let Some(pattern) = &chain.binding {
            self.pattern(pattern);
        }
        for term in &chain.terms {
            self.term(term);
        }
    }

    fn term(&mut self, term: &ast::Term) {
        match term {
            // Pin roots are variables and parameters, so a pattern holds no value
            // imports — but its type ascriptions may reference module types.
            ast::Term::Literal(_) | ast::Term::Process(_) | ast::Term::Self_ => {}
            ast::Term::Match(pattern) => self.pattern(pattern),
            ast::Term::Tuple(tuple) => {
                for field in &tuple.fields {
                    match &field.value {
                        ast::FieldValue::Chain(chain) => self.chain(chain),
                        ast::FieldValue::Spread(Some(access)) => self.access(access),
                        ast::FieldValue::Spread(None) => {}
                    }
                }
            }
            ast::Term::String(_, segments) => {
                for segment in segments {
                    if let ast::StrSegment::Hole(block) = segment {
                        self.block(block);
                    }
                }
            }
            ast::Term::Block(block) => self.block(block),
            ast::Term::Function(function) => {
                if self.types {
                    if let Some(parameter) = &function.parameter_type {
                        self.type_def(parameter);
                    }
                    if let Some(result) = &function.return_type {
                        self.type_def(result);
                    }
                }
                if let Some(body) = &function.body {
                    self.block(body);
                }
            }
            ast::Term::Access(access) | ast::Term::State(access, _) => self.access(access),
            ast::Term::Apply(access, argument) => {
                self.access(access);
                self.term(argument);
            }
            ast::Term::Spawn(target, argument, _) => {
                self.term(target);
                if let Some(argument) = argument {
                    self.term(argument);
                }
            }
            ast::Term::Select(sources, _) => {
                for chain in sources.iter().flatten() {
                    self.chain(chain);
                }
            }
            ast::Term::Dialect(dialect) => self.found.push((dialect.path.clone(), dialect.span)),
        }
    }

    fn access(&mut self, access: &ast::Access) {
        if let Some(ast::AccessSource::Import(path)) = &access.source {
            self.found.push((path.clone(), access.base_span));
        }
        if self.types {
            for argument in &access.type_arguments {
                self.type_def(argument);
            }
            for accessor in &access.accessors {
                if let ast::AccessPath::Annotation(_, Some(check)) = accessor {
                    self.type_def(check);
                }
            }
        }
    }

    fn pattern(&mut self, pattern: &ast::Match) {
        match pattern {
            ast::Match::Identifier(..)
            | ast::Match::Literal(_)
            | ast::Match::String(..)
            | ast::Match::Star(_)
            | ast::Match::Placeholder
            | ast::Match::Pin(_) => {}
            ast::Match::Tuple(tuple) => {
                for field in &tuple.fields {
                    self.pattern(&field.pattern);
                }
            }
            ast::Match::Partial(partial) => {
                for field in &partial.fields {
                    if let Some(pattern) = &field.pattern {
                        self.pattern(pattern);
                    }
                }
            }
            ast::Match::Or(alternatives) => {
                for alternative in alternatives {
                    self.pattern(alternative);
                }
            }
            ast::Match::Type(type_def) => self.type_def(type_def),
            ast::Match::As(head, ..) => self.pattern(head),
        }
    }

    fn type_def(&mut self, type_def: &ast::Type) {
        if !self.types {
            return;
        }
        match type_def {
            ast::Type::Primitive(_) | ast::Type::Cycle(_) | ast::Type::Resource(_) => {}
            ast::Type::Tuple(tuple) => {
                for field in &tuple.fields {
                    match field {
                        // A decorator entry carries no type to walk.
                        ast::FieldType::Field {
                            type_def: Some(type_def),
                            ..
                        } => self.type_def(type_def),
                        ast::FieldType::Field { type_def: None, .. } => {}
                        ast::FieldType::Spread { type_arguments, .. } => {
                            for argument in type_arguments {
                                self.type_def(argument);
                            }
                        }
                    }
                }
            }
            ast::Type::Function(function) => {
                self.type_def(&function.input);
                self.type_def(&function.output);
                if let Some(receive) = &function.receive {
                    self.type_def(receive);
                }
                if let Some(states) = &function.states {
                    self.type_def(states);
                }
            }
            ast::Type::Union(union) => {
                for member in &union.types {
                    self.type_def(member);
                }
            }
            ast::Type::Intersection(members) => {
                for member in members {
                    self.type_def(member);
                }
            }
            ast::Type::Identifier { arguments, .. } | ast::Type::SelfDefault { arguments } => {
                for argument in arguments {
                    self.type_def(argument);
                }
            }
            ast::Type::Process(process) => {
                for part in [
                    &process.receive_type,
                    &process.return_type,
                    &process.state_type,
                ]
                .into_iter()
                .flatten()
                {
                    self.type_def(part);
                }
            }
            ast::Type::ModuleType {
                module, arguments, ..
            } => {
                // The span is not tracked for type references; the compiler reports
                // resolution errors at the use site itself.
                self.found.push((module.clone(), ast::Spanned::default()));
                for argument in arguments {
                    self.type_def(argument);
                }
            }
        }
    }
}
