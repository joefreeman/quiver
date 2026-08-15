//! The REPL session, split along the evaluator boundary (see the server plan):
//!
//! - [`LineCompiler`] is the *client-side* half: every piece of compiler-side session
//!   state — the accumulated program, bindings, session tables, module cache, the
//!   last result type — and the prepare/compile/commit-line lifecycle over it. It
//!   never touches an environment; what the host must do comes back as values
//!   (a keep-set to compact, a unit to resume).
//! - [`Repl`] binds a `LineCompiler` to an in-process [`Environment`]: the driver the
//!   web build and the test harnesses use, with the same API it has always had.
//!
//! A remote driver (the CLI talking to `quiv server`) uses `LineCompiler` directly
//! and carries the returned values over its protocol instead.

use crate::environment::{Environment, EnvironmentError};
use quiver_compiler::Compiler;
use quiver_compiler::ModuleResolver;
use quiver_compiler::compiler::{
    Bindings, ModuleCache, Scope, ScopeKind, SessionTables, resolve_type_alias_for_display,
};
use quiver_core::bytecode::Function;
use quiver_core::effects::Effect;
use quiver_core::process::ProcessId;
use quiver_core::program::Program;
use quiver_core::types::{Type, TypeLookup};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum ReplError {
    Parser(Box<quiver_compiler::parser::Error>),
    Compiler(quiver_compiler::compiler::Error),
    Runtime(quiver_core::error::Error),
    Environment(EnvironmentError),
}

impl std::fmt::Display for ReplError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReplError::Parser(e) => write!(f, "Parse error: {}", e),
            ReplError::Compiler(e) => write!(f, "Compile error: {:?}", e),
            ReplError::Runtime(e) => write!(f, "Runtime error: {:?}", e),
            ReplError::Environment(e) => write!(f, "Environment error: {}", e),
        }
    }
}

/// A line staged by [`LineCompiler::prepare`]: parsed, its binding indices re-aligned
/// (the host must apply [`PreparedLine::compact_keep`] to the session process before
/// the line's resume), and carrying clones of everything the compiler mutates.
/// Consumed by [`LineCompiler::compile`].
pub struct PreparedLine {
    epoch: u64,
    parsed: quiver_compiler::ast::Sequence,
    /// The program's function count before this line compiles: everything registered
    /// from here on is the line's own, which is what unit extraction needs to know.
    /// [`LineCompiler::prepare`] sets the program's dedup floor to match, so interning
    /// cannot place one of this line's functions below it.
    own_floor: usize,
    /// The keep-set produced by the binding re-alignment, for the host to apply to
    /// the session process's locals.
    compact_keep: Vec<usize>,
    program: Program,
    module_cache: ModuleCache,
    process_type_ids: HashMap<usize, (usize, usize)>,
    last_result_type_id: usize,
}

impl PreparedLine {
    /// The locals to keep when compacting the session process — the environment-side
    /// half of the re-alignment `prepare` performed on the binding indices.
    pub fn compact_keep(&self) -> &[usize] {
        &self.compact_keep
    }

    /// The staged program, for deep-importing environment-space types before
    /// compiling (see [`PreparedLine::add_process_type`]).
    pub fn program_mut(&mut self) -> &mut Program {
        &mut self.program
    }

    /// Grant the line an `@pid` reference: `type_id` must already be an id in this
    /// line's program (imported via [`PreparedLine::program_mut`]).
    pub fn add_process_type(&mut self, pid: usize, type_id: usize, function_index: usize) {
        self.process_type_ids.insert(pid, (type_id, function_index));
    }
}

/// Build the unit payload for the line's entry. Infallible: a module the session cannot
/// supply is inlined into the unit by the extraction itself.
fn unit_payload(
    program: &Program,
    module_cache: &ModuleCache,
    entry: usize,
    own_floor: usize,
) -> LinePayload {
    let unit = quiver_compiler::extract_unit(
        program,
        module_cache,
        Some(entry),
        own_floor,
        quiver_compiler::Imports::Bundle,
    );
    // The closure is kept as artifacts rather than owned units: in-process the host links
    // straight from them, so nothing is cloned until a driver has to put one on a wire.
    let modules = if unit.imports.is_empty() {
        Vec::new()
    } else {
        let store = module_cache
            .artifact_store
            .as_ref()
            .expect("bundled imports require the store that made their modules suppliable");
        quiver_compiler::module_closure(store, module_cache, &unit).unwrap_or_else(|module| {
            panic!(
                "no stored artifact for module {} named by a bundled import",
                module.display()
            )
        })
    };
    LinePayload { unit, modules }
}

/// What a line hands the host to run: relocatable code, plus the modules it imports in
/// dependency order — each to be linked once per host, then named by key thereafter. A
/// module the compiler cannot supply (no store attached, or nothing stored under its
/// key) is inlined into the unit instead, so the payload always runs.
pub struct LinePayload {
    pub unit: quiver_compiler::CompiledUnit,
    pub modules: Vec<(
        quiver_compiler::UnitKey,
        Rc<quiver_compiler::ModuleArtifact>,
    )>,
}

impl LinePayload {
    /// The payload's serializable form: module *units* under their content keys — the
    /// artifacts stay compiler-side, a host never sees one.
    pub fn to_wire(&self) -> WirePayload {
        WirePayload {
            unit: self.unit.clone(),
            modules: self
                .modules
                .iter()
                .map(|(key, artifact)| (*key, artifact.unit.clone()))
                .collect(),
        }
    }
}

/// The wire form of a [`LinePayload`] — what crosses a process or network boundary to
/// a host that links and runs it. The host validates each module unit against its
/// content key, links the ones it lacks, and skips the rest; this is the payload shape
/// every remote driver shares (the CLI's resume body, the web's compiler-worker
/// output), which is what lets one host implementation serve them all.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct WirePayload {
    pub unit: quiver_compiler::CompiledUnit,
    pub modules: Vec<(quiver_compiler::UnitKey, quiver_compiler::CompiledUnit)>,
}

/// A line compiled by [`LineCompiler::compile`]: the session state it produces,
/// staged but not yet applied, plus what to run (`None` for a line with nothing to
/// execute, such as type definitions alone). Dropping it without
/// [`LineCompiler::commit_line`] leaves the session exactly as it was.
pub struct CompiledLine {
    epoch: u64,
    program: Program,
    bindings: Bindings,
    tables: SessionTables,
    module_cache: ModuleCache,
    last_result_type: Type,
    payload: Option<LinePayload>,
}

impl CompiledLine {
    /// What this line will hand the host, before committing — for drivers that need to
    /// size or inspect it.
    pub fn payload(&self) -> Option<&LinePayload> {
        self.payload.as_ref()
    }
}

/// What a committed line asks of the host: link the payload's modules, resume the
/// session process with its unit, and hand the keep-set to the result request so the
/// line's orphaned locals are released at delivery.
pub struct CommittedLine {
    pub payload: LinePayload,
    pub keep_indices: Vec<usize>,
}

/// The compiler-side half of a REPL session. Owns every piece of cross-line compile
/// state and no environment access; hosts (in-process or remote) apply what its
/// lifecycle methods hand back.
pub struct LineCompiler<E: Effect> {
    /// Guards the prepare → compile → commit sequence: each stamped line must be the
    /// most recently prepared one, so an out-of-order or superseded line fails loudly
    /// instead of silently committing a stale session snapshot.
    line_epoch: u64,
    program: Program, // Accumulated program state across evaluations (needed for module caching)
    bindings: Bindings, // variables and type aliases persisted across sessions
    /// What earlier entries taught the compiler about the values they defined. Persisted for
    /// the same reason `bindings` is: an entry is its own compilation, so without this a
    /// function defined earlier is recompiled against as if nothing were known about it —
    /// losing explicit instantiation (`f<'int>`) and dispatch result specialisation.
    tables: SessionTables,
    module_cache: ModuleCache, // persistent module cache across evaluations
    last_result_type: Type,    // Type of the last evaluated result, for continuations
    resolver: Box<dyn ModuleResolver>,
    builtins: quiver_core::builtins::BuiltinRegistry<E>,
    options: quiver_compiler::compiler::CompileOptions,
}

impl<E: Effect> LineCompiler<E> {
    pub fn new(
        resolver: Box<dyn ModuleResolver>,
        builtins: quiver_core::builtins::BuiltinRegistry<E>,
    ) -> Self {
        Self {
            line_epoch: 0,
            program: Program::new(),
            bindings: Bindings::default(),
            tables: SessionTables::default(),
            module_cache: ModuleCache::new(),
            last_result_type: Type::nil(),
            resolver,
            builtins,
            options: quiver_compiler::compiler::CompileOptions::default(),
        }
    }

    /// The host's builtin registry, which linking a unit needs in order to resolve the
    /// builtins it names.
    pub fn builtins(&self) -> &quiver_core::builtins::BuiltinRegistry<E> {
        &self.builtins
    }

    /// Set the compilation mode for subsequent evaluations (debug builds stamp nil
    /// results with failure provenance).
    pub fn set_compile_options(&mut self, options: quiver_compiler::compiler::CompileOptions) {
        self.options = options;
    }

    /// Swap in a fresh resolver and drop all cached modules, so subsequent evaluations re-read
    /// (and recompile) project modules from disk — picking up edits and `quiver.toml` changes.
    /// Accumulated variables and program state are kept; bindings that already captured values
    /// from a previous version of a module retain those values.
    pub fn reload_modules(&mut self, resolver: Box<dyn ModuleResolver>) {
        self.resolver = resolver;
        self.module_cache = ModuleCache::new();
    }

    /// Attach an artifact store: imports link from stored artifacts when their keys
    /// match, and modules compiled from source are extracted into it (see
    /// `quiver_compiler::artifact`). Safe at any point in a session — the store only
    /// affects future imports.
    pub fn set_artifact_store(&mut self, store: Rc<quiver_compiler::ArtifactStore>) {
        self.module_cache.artifact_store = Some(store);
    }

    /// Parse a line and stage it: re-align the binding indices (the caller applies the
    /// resulting keep-set to the session process before the line's resume) and capture
    /// clones of everything the compiler will mutate — a failed or abandoned line
    /// can't pollute session state.
    pub fn prepare(&mut self, source: &str) -> Result<PreparedLine, ReplError> {
        // Parse the source
        let parsed = quiver_compiler::parse(source).map_err(|e| ReplError::Parser(Box::new(e)))?;

        // Re-align the binding indices before compiling, so each line begins from an
        // aligned state (see `compact_bindings`). This mutates the session state
        // immediately — deliberately: the re-alignment is self-consistent whether or
        // not the prepared line ever compiles or commits, provided the host applies
        // the keep-set before the next resume.
        let compact_keep = self.compact_bindings();

        // Clone the program and module cache for compilation; the compiler mutates these in
        // place, and they reach `self` only when the compiled line is committed.
        let mut program = self.program.clone();
        let module_cache = self.module_cache.clone();

        // Convert last_result_type from Type to type ID for the compiler
        let last_result_type_id = program.register_type(self.last_result_type.clone());

        // Nothing this line registers may collapse onto a function an earlier line
        // registered. Unit extraction identifies the line's own functions as "registered
        // at or above this floor", and structural interning would otherwise hand a line
        // identical to an earlier one that line's ids — leaving it apparently owning
        // nothing it could name. Module compiles raise the floor again for their own
        // (attribution) reasons and restore this one after.
        let own_floor = program.get_functions().len();
        program.set_function_dedup_floor(own_floor);

        self.line_epoch += 1;
        Ok(PreparedLine {
            epoch: self.line_epoch,
            parsed,
            own_floor,
            compact_keep,
            program,
            module_cache,
            process_type_ids: HashMap::new(),
            last_result_type_id,
        })
    }

    /// Compile a prepared line. The slow step: a driver holding any host lock should
    /// release it around this call. Mutates nothing — the session state the line
    /// produces is staged in the returned [`CompiledLine`].
    pub fn compile(&self, line: PreparedLine) -> Result<CompiledLine, ReplError> {
        assert_eq!(
            line.epoch, self.line_epoch,
            "stale line: prepare, compile and commit must run in order, one line at a time"
        );
        let PreparedLine {
            epoch,
            parsed,
            own_floor,
            compact_keep: _,
            mut program,
            mut module_cache,
            process_type_ids,
            last_result_type_id,
        } = line;

        let result = Compiler::compile(
            parsed,
            &self.bindings,
            self.tables.clone(),
            &mut module_cache,
            self.resolver.as_ref(),
            &mut program,
            last_result_type_id, // parameter_type - use previous result type for continuations
            &process_type_ids,
            &self.builtins,
            None, // the REPL doesn't build a semantic index
            self.options.clone(),
        )
        .map_err(|e| ReplError::Compiler(e.error))?;

        let instructions = result.instructions;
        let result_type_id = result.result_type;
        let receive_type_id = result.receive_type;

        // Convert result_type_id to Type for storage
        let result_type = program
            .lookup_type(result_type_id)
            .cloned()
            .unwrap_or_else(Type::nil);

        // Only create a function wrapper if we have instructions to execute (a line of
        // type definitions alone has none)
        let payload = if !instructions.is_empty() {
            // Register the callable type for this REPL wrapper function
            // Use the receive type extracted from the expression (allows REPL to receive messages)
            let callable_type_id = program.register_type(Type::Callable {
                parameter: last_result_type_id,
                result: result_type_id,
                receive: receive_type_id,
                // The REPL wrapper is never spawned; grant nothing.
                states: None,
            });
            let function = Function {
                instructions,
                captures: 0,
                type_id: callable_type_id,
            };
            let function_index = program.register_function(function);
            // The host links each supplied module once and the line ships only its own
            // code; a module the session cannot supply is inlined into the unit. Either
            // way earlier lines' code is already in the host, and values reach it
            // through the heap rather than the payload.
            Some(unit_payload(
                &program,
                &module_cache,
                function_index,
                own_floor,
            ))
        } else {
            None
        };

        Ok(CompiledLine {
            epoch,
            program,
            bindings: result.bindings,
            tables: result.tables,
            module_cache,
            last_result_type: result_type,
            payload,
        })
    }

    /// Apply a compiled line's session state, answering what the host must now do —
    /// link the modules and resume the session process with the unit, keep-set attached
    /// to the result request — or `None` for a line with nothing to execute.
    pub fn commit_line(&mut self, line: CompiledLine) -> Option<CommittedLine> {
        assert_eq!(
            line.epoch, self.line_epoch,
            "stale line: prepare, compile and commit must run in order, one line at a time"
        );
        self.line_epoch += 1;

        self.program = line.program;
        self.bindings = line.bindings;
        self.tables = line.tables;
        self.module_cache = line.module_cache;
        self.last_result_type = line.last_result_type;

        line.payload.map(|payload| CommittedLine {
            payload,
            keep_indices: self.keep_indices(),
        })
    }

    /// Sorted local indices of every currently-bound variable. These are the slots that must
    /// survive compaction; everything else in the process's locals (the line's parameter and
    /// temporaries) is orphaned once the line finishes.
    fn keep_indices(&self) -> Vec<usize> {
        let mut indices: Vec<usize> = self
            .bindings
            .variables
            .values()
            .map(|variable| variable.index)
            .collect();
        indices.sort();
        indices
    }

    /// Re-align the binding indices contiguously, answering the keep-set (the old
    /// indices, in their new order) the host must apply to the session process's
    /// locals. Correctness-critical, not an optimisation: without it the physical
    /// local positions drift from the compiler's binding indices and lookups read
    /// stale slots. (The host separately releases a finished line's orphaned locals
    /// at result delivery; see `keep_indices`.)
    fn compact_bindings(&mut self) -> Vec<usize> {
        let keep_indices = self.keep_indices();

        // Build mapping from old index to new index
        let mut index_mapping = HashMap::new();
        for (new_idx, &old_idx) in keep_indices.iter().enumerate() {
            index_mapping.insert(old_idx, new_idx);
        }

        // Update bindings with new indices
        for variable in self.bindings.variables.values_mut() {
            variable.index = index_mapping
                .get(&variable.index)
                .copied()
                .expect("Invalid variable index");
        }

        keep_indices
    }

    /// The local slot of a bound variable, for host-side value requests.
    pub fn variable_index(&self, name: &str) -> Result<usize, EnvironmentError> {
        self.bindings
            .variables
            .get(name)
            .map(|variable| variable.index)
            .ok_or_else(|| {
                if self.bindings.type_aliases.contains_key(name) {
                    EnvironmentError::VariableNotFound(format!(
                        "'{}' is a type alias, not a variable",
                        name
                    ))
                } else {
                    EnvironmentError::VariableNotFound(name.to_string())
                }
            })
    }

    /// Get all variable names and their formatted types, ordered by local index
    pub fn get_variables(&self) -> Vec<(String, String)> {
        let mut vars: Vec<_> = self
            .bindings
            .variables
            .iter()
            .map(|(name, variable)| {
                // Format the type using the session's own program
                let formatted_type =
                    quiver_core::format::format_type_by_id(&self.program, variable.ty);
                (name.clone(), formatted_type, variable.index)
            })
            .collect();

        // Sort by local index to maintain definition order
        vars.sort_by_key(|(_, _, idx)| *idx);

        // Drop the index from the result
        vars.into_iter().map(|(name, ty, _)| (name, ty)).collect()
    }

    /// Resolve a type alias and return the resolved type ID.
    /// Type parameters are resolved to type variable placeholders.
    /// This is useful for testing and displaying type aliases.
    pub fn resolve_type_alias(&mut self, alias_name: &str) -> Result<usize, String> {
        // Only the type aliases matter for resolution; variables are irrelevant here
        let bindings = Bindings {
            variables: HashMap::new(),
            type_aliases: self.bindings.type_aliases.clone(),
        };
        let scope = Scope::new(bindings, None, ScopeKind::Root);
        let scopes = vec![scope];

        // Use the session's program for resolution (not the environment's)
        // because TypeAliasDef::Resolved type IDs are registered in the session's program
        resolve_type_alias_for_display(&scopes, alias_name).map_err(|e| format!("{:?}", e))
    }

    /// Format a type by its ID using the session's program.
    /// This is needed because type IDs in TypeAliasDef::Resolved are registered
    /// in the session's program, not the Environment's.
    pub fn format_type_by_id(&self, type_id: usize) -> String {
        quiver_core::format::format_type_by_id(&self.program, type_id)
    }

    /// Format a type using the session's program. Compiler-produced types (such as
    /// `get_last_result_type`) carry ids in the session's program space, so they must
    /// be formatted here — an environment's merged program is a different id space.
    pub fn format_type(&self, ty: &Type) -> String {
        quiver_core::format::format_type(&self.program, ty)
    }

    /// Get the type of the last evaluated result
    pub fn get_last_result_type(&self) -> &Type {
        &self.last_result_type
    }
}

/// A [`LineCompiler`] bound to an in-process [`Environment`] — the driver the web
/// build and the test harnesses use.
pub struct Repl<E: Effect> {
    repl_process_id: Option<ProcessId>,
    compiler: LineCompiler<E>,
}

impl<E: Effect> Repl<E> {
    pub fn new(
        env: &mut Environment<E>,
        resolver: Box<dyn ModuleResolver>,
        builtins: quiver_core::builtins::BuiltinRegistry<E>,
    ) -> Result<Self, ReplError> {
        // Create a sleeping process ready for resume
        let pid = env.start_process().map_err(ReplError::Environment)?;

        let compiler = LineCompiler::new(resolver, builtins);
        Ok(Self {
            repl_process_id: Some(pid),
            compiler,
        })
    }

    /// Set the compilation mode for subsequent evaluations (debug builds stamp nil
    /// results with failure provenance).
    pub fn set_compile_options(&mut self, options: quiver_compiler::compiler::CompileOptions) {
        self.compiler.set_compile_options(options);
    }

    /// Get the REPL process ID
    pub fn process_id(&self) -> ProcessId {
        self.repl_process_id.expect("REPL process not initialized")
    }

    /// See [`LineCompiler::reload_modules`].
    pub fn reload_modules(&mut self, resolver: Box<dyn ModuleResolver>) {
        self.compiler.reload_modules(resolver);
    }

    /// See [`LineCompiler::set_artifact_store`].
    pub fn set_artifact_store(&mut self, store: Rc<quiver_compiler::ArtifactStore>) {
        self.compiler.set_artifact_store(store);
    }

    /// Compile and evaluate an expression: [`Repl::prepare`], [`Repl::compile`] and
    /// [`Repl::commit`] in sequence. Drivers that share the environment across threads
    /// should call the three steps directly, releasing the environment around `compile`.
    /// Returns a request ID that can be polled for the result
    /// Returns None if the source only contains type definitions (no executable code)
    ///
    /// Process types must be fetched before calling this method:
    /// - Native: request_process_types() + step()/poll_request() loop
    /// - Web: async request via wasm bindings
    pub fn evaluate(
        &mut self,
        env: &mut Environment<E>,
        source: &str,
        process_types: HashMap<usize, (Type, usize)>,
    ) -> Result<Option<u64>, ReplError> {
        let prepared = self.prepare(env, source, process_types)?;
        let compiled = self.compile(prepared)?;
        self.commit(env, compiled)
    }

    /// Parse a line and stage it against the session: the environment-touching
    /// prologue. Compacts the session process's locals and deep-imports process types
    /// (for `@N` references) from the environment's id space into the line's.
    pub fn prepare(
        &mut self,
        env: &mut Environment<E>,
        source: &str,
        process_types: HashMap<usize, (Type, usize)>,
    ) -> Result<PreparedLine, ReplError> {
        let mut prepared = self.compiler.prepare(source)?;

        // Apply the binding re-alignment to the process's locals (ignore errors —
        // before the first executable line there is nothing to compact).
        if let Some(pid) = self.repl_process_id {
            let _ = env.compact_locals(pid, prepared.compact_keep().to_vec());
        }

        // Deep-import the process types: they are built in the environment's id
        // space, and registering them directly would leave their child ids dangling.
        for (pid, (ty, function_index)) in process_types {
            let local_ty = env.import_type_into(prepared.program_mut(), ty);
            let type_id = prepared.program_mut().register_type(local_ty);
            prepared.add_process_type(pid, type_id, function_index);
        }

        Ok(prepared)
    }

    /// See [`LineCompiler::compile`]. Needs no environment access — a driver holding
    /// a lock on the environment should release it around this call.
    pub fn compile(&self, line: PreparedLine) -> Result<CompiledLine, ReplError> {
        self.compiler.compile(line)
    }

    /// Apply a compiled line's session state and hand its unit to the session
    /// process: the environment-touching epilogue. Returns a request ID that can be
    /// polled for the result, or `None` for a line with nothing to execute.
    pub fn commit(
        &mut self,
        env: &mut Environment<E>,
        line: CompiledLine,
    ) -> Result<Option<u64>, ReplError> {
        let Some(CommittedLine {
            payload,
            keep_indices,
        }) = self.compiler.commit_line(line)
        else {
            return Ok(None);
        };

        // A unit's modules are offered on every line, not only the first: the host holds
        // each under its content key and skips the ones it has, so a line whose module
        // a code sweep reclaimed re-links it here rather than failing.
        for (key, artifact) in &payload.modules {
            env.link_module_unit(*key, &artifact.unit, self.compiler.builtins())
                .map_err(ReplError::Environment)?;
        }

        // Create or resume the REPL process. Resuming pushes the previous result from
        // `process.result` onto the stack, which is the line's input.
        let repl_process_id = match self.repl_process_id {
            Some(pid) => {
                env.resume_process_unit(pid, &payload.unit, self.compiler.builtins())
                    .map_err(ReplError::Environment)?;
                pid
            }
            // The persistent REPL process, created on first evaluation.
            None => {
                let pid = env
                    .start_process_unit(&payload.unit, self.compiler.builtins())
                    .map_err(ReplError::Environment)?;
                self.repl_process_id = Some(pid);
                pid
            }
        };

        // Request the result, handing the worker this line's keep-set so it releases the line's
        // orphaned locals (its parameter and temporaries) the moment the result is delivered. This
        // is the GC early-release; the next line's pre-compile compaction still does the
        // correctness-critical re-indexing, so the two are not redundant.
        let request_id = env
            .request_result(repl_process_id, Some(keep_indices))
            .map_err(ReplError::Environment)?;

        Ok(Some(request_id))
    }

    /// Request a variable value by name
    /// Returns a request ID that can be polled with poll_request()
    /// The result will be RequestResult::Locals containing the variable value
    pub fn request_variable(
        &mut self,
        env: &mut Environment<E>,
        name: &str,
    ) -> Result<u64, EnvironmentError> {
        let local_index = self.compiler.variable_index(name)?;
        let repl_process_id = self
            .repl_process_id
            .ok_or(EnvironmentError::NoReplProcess)?;

        env.request_locals(repl_process_id, vec![local_index])
    }

    /// See [`LineCompiler::get_variables`].
    pub fn get_variables(&self) -> Vec<(String, String)> {
        self.compiler.get_variables()
    }

    /// See [`LineCompiler::resolve_type_alias`].
    pub fn resolve_type_alias(
        &mut self,
        _env: &mut Environment<E>,
        alias_name: &str,
    ) -> Result<usize, String> {
        self.compiler.resolve_type_alias(alias_name)
    }

    /// See [`LineCompiler::format_type_by_id`].
    pub fn format_type_by_id(&self, type_id: usize) -> String {
        self.compiler.format_type_by_id(type_id)
    }

    /// See [`LineCompiler::format_type`].
    pub fn format_type(&self, ty: &Type) -> String {
        self.compiler.format_type(ty)
    }

    /// See [`LineCompiler::get_last_result_type`].
    pub fn get_last_result_type(&self) -> &Type {
        self.compiler.get_last_result_type()
    }
}
