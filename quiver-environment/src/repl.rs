use crate::environment::{Environment, EnvironmentError};
use quiver_compiler::Compiler;
use quiver_compiler::compiler::{Binding, ModuleCache};
use quiver_compiler::modules::ModuleLoader;
use quiver_core::bytecode::Function;
use quiver_core::process::ProcessId;
use quiver_core::program::Program;
use quiver_core::types::Type;
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
            ReplError::Parser(e) => write!(f, "Parser error: {:?}", e),
            ReplError::Compiler(e) => write!(f, "Compiler error: {:?}", e),
            ReplError::Runtime(e) => write!(f, "Runtime error: {:?}", e),
            ReplError::Environment(e) => write!(f, "Environment error: {}", e),
        }
    }
}

pub struct Repl {
    repl_process_id: Option<ProcessId>,
    types: Vec<quiver_core::types::TupleTypeInfo>, // Accumulated types across evaluations
    bindings: HashMap<String, Binding>, // variables and type aliases persisted across sessions
    module_cache: ModuleCache,          // persistent module cache across evaluations
    last_result_type: Type,             // Type of the last evaluated result, for continuations
    module_loader: Box<dyn ModuleLoader>,
}

impl Repl {
    pub fn new(program: Program, module_loader: Box<dyn ModuleLoader>) -> Self {
        let types = program.get_tuples().to_vec();

        Self {
            repl_process_id: None,
            types,
            bindings: HashMap::new(),
            module_cache: ModuleCache::new(),
            last_result_type: Type::nil(),
            module_loader,
        }
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
        env: &mut Environment,
        source: &str,
        process_types: HashMap<usize, (Type, usize)>,
    ) -> Result<Option<u64>, ReplError> {
        // Parse the source
        let parsed = quiver_compiler::parse(source).map_err(|e| ReplError::Parser(Box::new(e)))?;

        let result = Compiler::compile(
            parsed,
            &self.bindings,
            self.module_cache.clone(),
            self.module_loader.as_ref(),
            self.types.clone(),
            None,                          // module_path
            self.last_result_type.clone(), // parameter_type - use previous result type for continuations
            &process_types,
        )
        .map_err(ReplError::Compiler)?;

        let instructions = result.instructions;
        let result_type = result.result_type;
        let bindings = result.bindings;
        let module_cache = result.module_cache;
        let mut program = result.program;

        // Update REPL state
        self.bindings = bindings;
        self.module_cache = module_cache;
        self.last_result_type = result_type.clone();

        // Only create function wrapper if we have instructions to execute
        let function_index = if !instructions.is_empty() {
            let function = Function {
                instructions,
                function_type: quiver_core::types::CallableType {
                    parameter: self.last_result_type.clone(),
                    result: result_type.clone(),
                    receive: Type::Union(vec![]), // Bottom type (never)
                },
                captures: vec![],
            };
            Some(program.register_function(function))
        } else {
            None
        };

        // Store types for next iteration
        self.types = program.get_tuples().to_vec();

        // If no function was created (type definitions only), we're done
        let Some(function_index) = function_index else {
            return Ok(None);
        };

        // Create bytecode from the program (without tree-shaking to preserve module imports)
        let mut bytecode = program.to_bytecode(None);
        // Set the entry function
        bytecode.entry = Some(function_index);

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
                    .start_process(bytecode)
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
        env: &mut Environment,
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

    /// Get all variable names and their types, ordered by local index
    pub fn get_variables(&self) -> Vec<(String, Type)> {
        let mut vars: Vec<_> = self
            .bindings
            .iter()
            .filter_map(|(name, binding)| {
                if let Binding::Variable { ty, index } = binding {
                    Some((name.clone(), ty.clone(), *index))
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
    pub fn compact(&mut self, env: &mut Environment) {
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

    /// Resolve a type alias and return the resolved Type.
    /// Type parameters are resolved to Type::Variable placeholders.
    /// This is useful for testing and displaying type aliases.
    pub fn resolve_type_alias(
        &mut self,
        env: &mut Environment,
        alias_name: &str,
    ) -> Result<Type, String> {
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

        env.resolve_type_alias(&type_aliases, alias_name)
    }

    /// Get the type of the last evaluated result
    pub fn get_last_result_type(&self) -> &Type {
        &self.last_result_type
    }
}
