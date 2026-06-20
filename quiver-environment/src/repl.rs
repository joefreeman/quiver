use crate::environment::{Environment, EnvironmentError};
use quiver_compiler::Compiler;
use quiver_compiler::compiler::{
    Binding, ModuleCache, Scope, ScopeKind, resolve_type_alias_for_display,
};
use quiver_compiler::modules::ModuleLoader;
use quiver_core::bytecode::Function;
use quiver_core::effects::Effect;
use quiver_core::process::ProcessId;
use quiver_core::program::Program;
use quiver_core::types::{Type, TypeLookup};
use std::collections::HashMap;

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

pub struct Repl<E: Effect> {
    repl_process_id: Option<ProcessId>,
    program: Program, // Accumulated program state across evaluations (needed for module caching)
    bindings: HashMap<String, Binding>, // variables and type aliases persisted across sessions
    module_cache: ModuleCache, // persistent module cache across evaluations
    last_result_type: Type, // Type of the last evaluated result, for continuations
    module_loader: Box<dyn ModuleLoader>,
    builtins: quiver_core::builtins::BuiltinRegistry<E>,
}

impl<E: Effect> Repl<E> {
    pub fn new(
        env: &mut Environment<E>,
        module_loader: Box<dyn ModuleLoader>,
        builtins: quiver_core::builtins::BuiltinRegistry<E>,
    ) -> Result<Self, ReplError> {
        // Create a sleeping process ready for resume
        let pid = env.start_process(None).map_err(ReplError::Environment)?;

        Ok(Self {
            repl_process_id: Some(pid),
            program: Program::new(),
            bindings: HashMap::new(),
            module_cache: ModuleCache::new(),
            last_result_type: Type::nil(),
            module_loader,
            builtins,
        })
    }

    /// Get the REPL process ID
    pub fn process_id(&self) -> ProcessId {
        self.repl_process_id.expect("REPL process not initialized")
    }

    /// Compile and evaluate an expression
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
        // Parse the source
        let parsed = quiver_compiler::parse(source).map_err(|e| ReplError::Parser(Box::new(e)))?;

        // Clone the program and module cache for compilation; the compiler mutates these in
        // place, and we commit them back to `self` only on success (so a failed line can't
        // pollute REPL state).
        let mut program = self.program.clone();
        let mut module_cache = self.module_cache.clone();

        // Convert process types from Type to type IDs for the compiler
        let process_type_ids: HashMap<usize, (usize, usize)> = process_types
            .into_iter()
            .map(|(pid, (ty, func_idx))| (pid, (program.register_type(ty), func_idx)))
            .collect();

        // Convert last_result_type from Type to type ID for the compiler
        let last_result_type_id = program.register_type(self.last_result_type.clone());

        let result = Compiler::compile(
            parsed,
            &self.bindings,
            &mut module_cache,
            self.module_loader.as_ref(),
            &mut program,
            last_result_type_id, // parameter_type - use previous result type for continuations
            &process_type_ids,
            &self.builtins,
            None, // the REPL doesn't build a semantic index
        )
        .map_err(|e| ReplError::Compiler(e.error))?;

        let instructions = result.instructions;
        let result_type_id = result.result_type;
        let receive_type_id = result.receive_type;
        let bindings = result.bindings;

        // Convert result_type_id to Type for storage
        let result_type = program
            .lookup_type(result_type_id)
            .cloned()
            .unwrap_or_else(Type::nil);

        // Update REPL state
        self.bindings = bindings;
        self.module_cache = module_cache;
        self.last_result_type = result_type;

        // Only create function wrapper if we have instructions to execute
        let function_index = if !instructions.is_empty() {
            // Register the callable type for this REPL wrapper function
            // Use the receive type extracted from the expression (allows REPL to receive messages)
            let callable_type_id = program.register_type(Type::Callable {
                parameter: last_result_type_id,
                result: result_type_id,
                receive: receive_type_id,
            });
            let function = Function {
                instructions,
                captures: 0,
                type_id: callable_type_id,
            };
            Some(program.register_function(function))
        } else {
            None
        };

        // Update program state for next iteration
        self.program = program;

        // If no function was created (type definitions only), we're done
        let Some(function_index) = function_index else {
            return Ok(None);
        };

        // Create or resume the REPL process
        let repl_process_id = match self.repl_process_id {
            Some(pid) => {
                // Resume existing process with new function
                // resume_process will push the previous result from process.result onto the stack
                let bytecode = self.program.to_bytecode(Some(function_index));
                env.resume_process(pid, bytecode)
                    .map_err(ReplError::Environment)?;
                pid
            }
            None => {
                // Create the persistent REPL process on first evaluation
                let bytecode = self.program.to_bytecode(Some(function_index));
                let pid = env
                    .start_process(Some(bytecode))
                    .map_err(ReplError::Environment)?;
                self.repl_process_id = Some(pid);
                pid
            }
        };

        // Request the result
        let request_id = env
            .request_result(repl_process_id)
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
        let binding = self
            .bindings
            .get(name)
            .ok_or_else(|| EnvironmentError::VariableNotFound(name.to_string()))?;

        let local_index = match binding {
            Binding::Variable { index, .. } => *index,
            Binding::TypeAlias(_) => {
                return Err(EnvironmentError::VariableNotFound(format!(
                    "'{}' is a type alias, not a variable",
                    name
                )));
            }
        };

        let repl_process_id = self
            .repl_process_id
            .ok_or(EnvironmentError::NoReplProcess)?;

        env.request_locals(repl_process_id, vec![local_index])
    }

    /// Get all variable names and their formatted types, ordered by local index
    pub fn get_variables(&self) -> Vec<(String, String)> {
        let mut vars: Vec<_> = self
            .bindings
            .iter()
            .filter_map(|(name, binding)| {
                if let Binding::Variable { ty, index, .. } = binding {
                    // Format the type using the Repl's own program
                    let formatted_type = quiver_core::format::format_type_by_id(&self.program, *ty);
                    Some((name.clone(), formatted_type, *index))
                } else {
                    None
                }
            })
            .collect();

        // Sort by local index to maintain definition order
        vars.sort_by_key(|(_, _, idx)| *idx);

        // Drop the index from the result
        vars.into_iter().map(|(name, ty, _)| (name, ty)).collect()
    }

    /// Compact locals to remove unused variables (optimization, safe to skip)
    pub fn compact(&mut self, env: &mut Environment<E>) {
        // Silently ignore if no REPL process exists yet
        let Some(repl_process_id) = self.repl_process_id else {
            return;
        };

        // Keep all variables currently in the bindings map
        // Sort indices to ensure consistent ordering
        let mut keep_indices: Vec<usize> = self
            .bindings
            .values()
            .filter_map(|binding| {
                if let Binding::Variable { index, .. } = binding {
                    Some(*index)
                } else {
                    None
                }
            })
            .collect();
        keep_indices.sort();

        // Build mapping from old index to new index
        let mut index_mapping = HashMap::new();
        for (new_idx, &old_idx) in keep_indices.iter().enumerate() {
            index_mapping.insert(old_idx, new_idx);
        }

        // Update bindings with new indices
        for binding in self.bindings.values_mut() {
            if let Binding::Variable { index, .. } = binding {
                *index = index_mapping
                    .get(index)
                    .copied()
                    .expect("Invalid variable index");
            }
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
        // Extract type aliases from bindings
        let type_aliases: HashMap<String, quiver_compiler::compiler::TypeAliasDef> = self
            .bindings
            .iter()
            .filter_map(|(name, binding)| {
                if let Binding::TypeAlias(type_alias) = binding {
                    Some((name.clone(), type_alias.clone()))
                } else {
                    None
                }
            })
            .collect();

        // Convert type_aliases HashMap to a single scope for resolution
        let mut bindings = std::collections::HashMap::new();
        for (name, type_alias) in type_aliases {
            bindings.insert(name, Binding::TypeAlias(type_alias));
        }
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

    /// Get the type of the last evaluated result
    pub fn get_last_result_type(&self) -> &Type {
        &self.last_result_type
    }
}
