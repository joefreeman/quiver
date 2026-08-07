use crate::environment::{Environment, EnvironmentError};
use quiver_compiler::Compiler;
use quiver_compiler::ModuleResolver;
use quiver_compiler::compiler::{
    Bindings, ModuleCache, Scope, ScopeKind, SessionTables, resolve_type_alias_for_display,
};
use quiver_core::bytecode::{Bytecode, Function};
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

/// A line staged by [`Repl::prepare`]: parsed, aligned with the session process, and
/// carrying clones of everything the compiler mutates. Consumed by [`Repl::compile`],
/// which needs no environment access.
pub struct PreparedLine {
    epoch: u64,
    parsed: quiver_compiler::ast::Sequence,
    program: Program,
    module_cache: ModuleCache,
    process_type_ids: HashMap<usize, (usize, usize)>,
    last_result_type_id: usize,
}

/// A line compiled by [`Repl::compile`]: the session state it produces, staged but not
/// yet applied, plus the bytecode to run (`None` for a line with nothing to execute,
/// such as type definitions alone). Dropping it without [`Repl::commit`] leaves the
/// session exactly as it was.
pub struct CompiledLine {
    epoch: u64,
    program: Program,
    bindings: Bindings,
    tables: SessionTables,
    module_cache: ModuleCache,
    last_result_type: Type,
    bytecode: Option<Bytecode>,
}

pub struct Repl<E: Effect> {
    repl_process_id: Option<ProcessId>,
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

impl<E: Effect> Repl<E> {
    pub fn new(
        env: &mut Environment<E>,
        resolver: Box<dyn ModuleResolver>,
        builtins: quiver_core::builtins::BuiltinRegistry<E>,
    ) -> Result<Self, ReplError> {
        // Create a sleeping process ready for resume
        let pid = env.start_process(None).map_err(ReplError::Environment)?;

        Ok(Self {
            repl_process_id: Some(pid),
            line_epoch: 0,
            program: Program::new(),
            bindings: Bindings::default(),
            tables: SessionTables::default(),
            module_cache: ModuleCache::new(),
            last_result_type: Type::nil(),
            resolver,
            builtins,
            options: quiver_compiler::compiler::CompileOptions::default(),
        })
    }

    /// Set the compilation mode for subsequent evaluations (debug builds stamp nil
    /// results with failure provenance).
    pub fn set_compile_options(&mut self, options: quiver_compiler::compiler::CompileOptions) {
        self.options = options;
    }

    /// Get the REPL process ID
    pub fn process_id(&self) -> ProcessId {
        self.repl_process_id.expect("REPL process not initialized")
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

    /// Parse a line and stage it against the session: the environment-touching prologue.
    /// Re-aligns the session process's locals and imports process types, then captures
    /// clones of everything the compiler will mutate.
    pub fn prepare(
        &mut self,
        env: &mut Environment<E>,
        source: &str,
        process_types: HashMap<usize, (Type, usize)>,
    ) -> Result<PreparedLine, ReplError> {
        // Parse the source
        let parsed = quiver_compiler::parse(source).map_err(|e| ReplError::Parser(Box::new(e)))?;

        // Re-align the persistent process's locals with the binding indices before compiling, so
        // each line begins from an aligned state (see `compact`). This mutates the session
        // immediately — deliberately: the re-alignment is self-consistent whether or not the
        // prepared line ever compiles or commits.
        self.compact(env);

        // Clone the program and module cache for compilation; the compiler mutates these in
        // place, and they reach `self` only when the compiled line is committed (so a failed
        // or abandoned line can't pollute REPL state).
        let mut program = self.program.clone();
        let module_cache = self.module_cache.clone();

        // Convert process types (for `@N` references) from Type to type IDs for the compiler.
        // These types are built in the environment's id space, so deep-import them into this
        // REPL's program first; registering them directly would leave their child ids dangling
        // (referencing the environment's table, not ours) and corrupt the REPL program.
        let process_type_ids: HashMap<usize, (usize, usize)> = process_types
            .into_iter()
            .map(|(pid, (ty, func_idx))| {
                let local_ty = env.import_type_into(&mut program, ty);
                (pid, (program.register_type(local_ty), func_idx))
            })
            .collect();

        // Convert last_result_type from Type to type ID for the compiler
        let last_result_type_id = program.register_type(self.last_result_type.clone());

        self.line_epoch += 1;
        Ok(PreparedLine {
            epoch: self.line_epoch,
            parsed,
            program,
            module_cache,
            process_type_ids,
            last_result_type_id,
        })
    }

    /// Compile a prepared line. Needs no environment access — a driver holding a lock on
    /// the environment should release it around this call, the slow step of the three.
    /// Mutates nothing: the session state the line produces is staged in the returned
    /// [`CompiledLine`] and applied by [`Repl::commit`].
    pub fn compile(&self, line: PreparedLine) -> Result<CompiledLine, ReplError> {
        assert_eq!(
            line.epoch, self.line_epoch,
            "stale line: prepare, compile and commit must run in order, one line at a time"
        );
        let PreparedLine {
            epoch,
            parsed,
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
        let bytecode = if !instructions.is_empty() {
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
            Some(program.to_bytecode(Some(function_index)))
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
            bytecode,
        })
    }

    /// Apply a compiled line's session state and hand its bytecode to the session
    /// process: the environment-touching epilogue. Returns a request ID that can be
    /// polled for the result, or `None` for a line with nothing to execute.
    pub fn commit(
        &mut self,
        env: &mut Environment<E>,
        line: CompiledLine,
    ) -> Result<Option<u64>, ReplError> {
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

        // If no bytecode was produced (type definitions only), we're done
        let Some(bytecode) = line.bytecode else {
            return Ok(None);
        };

        // Create or resume the REPL process
        let repl_process_id = match self.repl_process_id {
            Some(pid) => {
                // Resume existing process with new function
                // resume_process will push the previous result from process.result onto the stack
                env.resume_process(pid, bytecode)
                    .map_err(ReplError::Environment)?;
                pid
            }
            None => {
                // Create the persistent REPL process on first evaluation
                let pid = env
                    .start_process(Some(bytecode))
                    .map_err(ReplError::Environment)?;
                self.repl_process_id = Some(pid);
                pid
            }
        };

        // Request the result, handing the worker this line's keep-set so it releases the line's
        // orphaned locals (its parameter and temporaries) the moment the result is delivered. This
        // is the GC early-release; the next line's pre-compile `compact` still does the
        // correctness-critical re-indexing, so the two are not redundant.
        let request_id = env
            .request_result(repl_process_id, Some(self.keep_indices()))
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
        let local_index = self
            .bindings
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
            })?;

        let repl_process_id = self
            .repl_process_id
            .ok_or(EnvironmentError::NoReplProcess)?;

        env.request_locals(repl_process_id, vec![local_index])
    }

    /// Get all variable names and their formatted types, ordered by local index
    pub fn get_variables(&self) -> Vec<(String, String)> {
        let mut vars: Vec<_> = self
            .bindings
            .variables
            .iter()
            .map(|(name, variable)| {
                // Format the type using the Repl's own program
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

    /// Re-align the process's locals with the binding indices: keep only the bound variables,
    /// re-indexed contiguously, and rewrite the binding map to match. Called by `evaluate` before
    /// compiling each line — without it the physical local positions drift from the compiler's
    /// binding indices and lookups read stale slots, so this is correctness-critical, not an
    /// optimisation. (The worker separately releases a finished line's orphaned locals at result
    /// delivery; see `keep_indices` and `Command::GetResult`.)
    fn compact(&mut self, env: &mut Environment<E>) {
        // Silently ignore if no REPL process exists yet
        let Some(repl_process_id) = self.repl_process_id else {
            return;
        };

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

        // Compact the locals on the worker (ignore errors - this is just an optimization)
        let _ = env.compact_locals(repl_process_id, keep_indices);
    }

    /// Resolve a type alias and return the resolved type ID.
    /// Type parameters are resolved to type variable placeholders.
    /// This is useful for testing and displaying type aliases.
    pub fn resolve_type_alias(
        &mut self,
        _env: &mut Environment<E>,
        alias_name: &str,
    ) -> Result<usize, String> {
        // Only the type aliases matter for resolution; variables are irrelevant here
        let bindings = Bindings {
            variables: HashMap::new(),
            type_aliases: self.bindings.type_aliases.clone(),
        };
        let scope = Scope::new(bindings, None, ScopeKind::Root);
        let scopes = vec![scope];

        // Use the REPL's program for resolution (not the environment's)
        // because TypeAliasDef::Resolved type IDs are registered in the REPL's program
        resolve_type_alias_for_display(&scopes, alias_name).map_err(|e| format!("{:?}", e))
    }

    /// Format a type by its ID using the REPL's program.
    /// This is needed because type IDs in TypeAliasDef::Resolved are registered
    /// in the REPL's program, not the Environment's.
    pub fn format_type_by_id(&self, type_id: usize) -> String {
        quiver_core::format::format_type_by_id(&self.program, type_id)
    }

    /// Format a type using the REPL's program. Compiler-produced types (such as
    /// `get_last_result_type`) carry ids in the REPL's program space, so they must be
    /// formatted here — the environment's merged program is a different id space.
    pub fn format_type(&self, ty: &Type) -> String {
        quiver_core::format::format_type(&self.program, ty)
    }

    /// Get the type of the last evaluated result
    pub fn get_last_result_type(&self) -> &Type {
        &self.last_result_type
    }
}
