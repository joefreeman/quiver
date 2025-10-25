use crate::WorkerHandle;
use crate::environment::{Environment, EnvironmentError, RequestResult};
use quiver_compiler::Compiler;
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::modules::ModuleLoader;
use quiver_core::bytecode::Function;
use quiver_core::process::ProcessId;
use quiver_core::program::Program;
use quiver_core::types::{ProcessType, Type};
use quiver_core::value::Value;
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
    environment: Environment,
    repl_process_id: Option<ProcessId>,
    program: Program,
    variable_map: HashMap<String, (Type, usize)>, // variable name -> (type, local index)
    type_aliases: HashMap<String, quiver_compiler::compiler::TypeAliasDef>, // type definitions persisted across sessions
    module_cache: ModuleCache, // persistent module cache across evaluations
    last_result_type: Type,    // Type of the last evaluated result, for continuations
    module_loader: Box<dyn ModuleLoader>,
}

impl Repl {
    pub fn new(
        workers: Vec<Box<dyn WorkerHandle>>,
        program: Program,
        module_loader: Box<dyn ModuleLoader>,
    ) -> Result<Self, EnvironmentError> {
        let environment = Environment::new(workers, &program)?;

        Ok(Self {
            environment,
            repl_process_id: None,
            program,
            variable_map: HashMap::new(),
            type_aliases: HashMap::new(),
            module_cache: ModuleCache::new(),
            last_result_type: Type::nil(),
            module_loader,
        })
    }

    /// Compile and evaluate an expression
    /// Returns a request ID that can be polled for the result
    /// Returns None if the source only contains type definitions (no executable code)
    pub fn evaluate(&mut self, source: &str) -> Result<Option<u64>, ReplError> {
        // Parse the source
        let parsed = quiver_compiler::parse(source).map_err(|e| ReplError::Parser(Box::new(e)))?;

        // Compile with existing variables
        let existing_vars = if self.variable_map.is_empty() {
            None
        } else {
            Some(&self.variable_map)
        };

        let result = Compiler::compile(
            parsed,
            self.type_aliases.clone(),
            self.module_cache.clone(),
            self.module_loader.as_ref(),
            &self.program,
            None, // module_path
            existing_vars,
            self.last_result_type.clone(), // parameter_type - use previous result type for continuations
        )
        .map_err(ReplError::Compiler)?;

        let instructions = result.instructions;
        let result_type = result.result_type;
        let variables = result.variables;
        let type_aliases = result.type_aliases;
        let module_cache = result.module_cache;
        let updated_program = result.program;

        // Get only the NEW program data (not already sent)
        let old_constants_len = self.program.get_constants().len();
        let old_functions_len = self.program.get_functions().len();
        let old_types_len = self.program.get_types().len();
        let old_builtins_len = self.program.get_builtins().len();

        self.program = updated_program;
        self.variable_map = variables;
        self.type_aliases = type_aliases;
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
            Some(self.program.register_function(function))
        } else {
            None
        };

        // Send program updates to workers (includes types and optionally the function wrapper)
        let all_constants = self.program.get_constants();
        let all_functions = self.program.get_functions();
        let all_types = self.program.get_types();
        let all_builtins = self.program.get_builtins();

        let new_constants = all_constants[old_constants_len..].to_vec();
        let new_functions = all_functions[old_functions_len..].to_vec();
        let new_types = all_types[old_types_len..].to_vec();
        let new_builtins = all_builtins[old_builtins_len..].to_vec();

        self.environment
            .update_program(new_constants, new_functions, new_types, new_builtins)
            .map_err(ReplError::Environment)?;

        // If no function was created (type definitions only), we're done
        let Some(function_index) = function_index else {
            return Ok(None);
        };

        // Create or resume the REPL process
        let repl_process_id = match self.repl_process_id {
            Some(pid) => {
                // Resume existing process with new function
                // resume_process will push the previous result from process.result onto the stack
                self.environment
                    .resume_process(pid, function_index)
                    .map_err(ReplError::Environment)?;
                pid
            }
            None => {
                // Create the persistent REPL process on first evaluation
                let pid = self
                    .environment
                    .start_process(function_index, true)
                    .map_err(ReplError::Environment)?;
                self.repl_process_id = Some(pid);
                pid
            }
        };

        // Request the result
        let request_id = self
            .environment
            .request_result(repl_process_id)
            .map_err(ReplError::Environment)?;

        Ok(Some(request_id))
    }

    /// Poll for a request result (non-blocking)
    /// Returns None if not ready, Some(Ok(...)) on success, Some(Err(...)) on error
    /// Handles all request types (evaluation results, statuses, info, locals, etc.)
    pub fn poll_request(
        &mut self,
        request_id: u64,
    ) -> Result<Option<RequestResult>, EnvironmentError> {
        match self.environment.poll_request(request_id)? {
            None => Ok(None),
            Some(RequestResult::Result(Ok((value, heap)))) => {
                self.compact()?;
                Ok(Some(RequestResult::Result(Ok((value, heap)))))
            }
            Some(RequestResult::Result(Err(error))) => {
                // Got a runtime error - reset REPL state
                self.repl_process_id = None; // Next eval will create new process
                self.variable_map.clear();
                self.type_aliases.clear();
                self.module_cache = ModuleCache::new();
                self.last_result_type = Type::nil();
                Ok(Some(RequestResult::Result(Err(error))))
            }
            result => Ok(result),
        }
    }

    /// Request a variable value by name
    /// Returns a request ID that can be polled with poll_request()
    /// The result will be RequestResult::Locals containing the variable value
    pub fn request_variable(&mut self, name: &str) -> Result<u64, EnvironmentError> {
        let (_, local_index) = self
            .variable_map
            .get(name)
            .ok_or_else(|| EnvironmentError::VariableNotFound(name.to_string()))?;

        let repl_process_id = self
            .repl_process_id
            .ok_or(EnvironmentError::NoReplProcess)?;

        self.environment
            .request_locals(repl_process_id, vec![*local_index])
    }

    /// Get all variable names and their types, ordered by local index
    pub fn get_variables(&self) -> Vec<(String, Type)> {
        let mut vars: Vec<_> = self
            .variable_map
            .iter()
            .map(|(name, (ty, idx))| (name.clone(), ty.clone(), *idx))
            .collect();

        // Sort by local index to maintain definition order
        vars.sort_by_key(|(_, _, idx)| *idx);

        // Drop the index from the result
        vars.into_iter().map(|(name, ty, _)| (name, ty)).collect()
    }

    /// Compact locals to remove unused variables (called automatically after evaluation)
    fn compact(&mut self) -> Result<(), EnvironmentError> {
        let repl_process_id = self
            .repl_process_id
            .ok_or(EnvironmentError::NoReplProcess)?;

        // Keep all variables currently in the variable map
        // Sort indices to ensure consistent ordering
        let mut keep_indices: Vec<usize> =
            self.variable_map.values().map(|(_, idx)| *idx).collect();
        keep_indices.sort();

        // Build mapping from old index to new index
        let mut index_mapping = HashMap::new();
        for (new_idx, &old_idx) in keep_indices.iter().enumerate() {
            index_mapping.insert(old_idx, new_idx);
        }

        // Update variable_map with new indices
        for (_, idx) in self.variable_map.values_mut() {
            *idx = *index_mapping
                .get(idx)
                .ok_or(EnvironmentError::InvalidVariableIndex(*idx))?;
        }

        // Compact the locals on the worker
        self.environment
            .compact_locals(repl_process_id, keep_indices)?;

        Ok(())
    }

    /// Step the environment (process events)
    pub fn step(&mut self) -> Result<bool, EnvironmentError> {
        self.environment.step()
    }

    /// Request all process statuses across all workers
    /// Returns a single request ID that will aggregate results from all workers
    /// The result will be RequestResult::Statuses containing all processes
    pub fn request_process_statuses(&mut self) -> Result<u64, EnvironmentError> {
        self.environment.request_statuses()
    }

    /// Request process info for a specific process
    /// Returns a request ID that can be polled with poll_request()
    /// The result will be RequestResult::Info
    pub fn request_process_info(&mut self, pid: ProcessId) -> Result<u64, EnvironmentError> {
        self.environment.request_process_info(pid)
    }

    /// Format a value for display
    pub fn format_value(&self, value: &Value, heap: &[Vec<u8>]) -> String {
        quiver_core::format::format_value(value, heap, &self.program)
    }

    /// Format a type for display
    pub fn format_type(&self, ty: &Type) -> String {
        quiver_core::format::format_type(&self.program, ty)
    }

    /// Get the formatted type for a process given its function index
    pub fn format_process_type(&self, function_index: usize) -> Option<String> {
        let function = self.program.get_function(function_index)?;
        let process_type = Type::Process(Box::new(ProcessType {
            receive: Some(Box::new(function.function_type.receive.clone())),
            returns: Some(Box::new(function.function_type.result.clone())),
        }));
        Some(self.format_type(&process_type))
    }

    /// Convert a runtime Value to its Type representation
    pub fn value_to_type(&self, value: &Value) -> Type {
        match value {
            Value::Integer(_) => Type::Integer,
            Value::Binary(_) => Type::Binary,
            Value::Tuple(type_id, _) => Type::Tuple(*type_id),
            Value::Function(func_idx, _) => {
                let func_type = &self
                    .program
                    .get_function(*func_idx)
                    .expect("Function should exist")
                    .function_type;
                Type::Callable(Box::new(func_type.clone()))
            }
            Value::Builtin(name) => {
                let builtin_info = self
                    .program
                    .get_builtins()
                    .iter()
                    .find(|b| &b.name == name)
                    .expect("Builtin should be registered");
                Type::Callable(Box::new(quiver_core::types::CallableType {
                    parameter: builtin_info.parameter_type.clone(),
                    result: builtin_info.result_type.clone(),
                    receive: Type::Union(vec![]),
                }))
            }
            Value::Process(_, function_idx) => {
                let func_type = &self
                    .program
                    .get_function(*function_idx)
                    .expect("Function should exist")
                    .function_type;
                Type::Process(Box::new(quiver_core::types::ProcessType {
                    receive: Some(Box::new(func_type.receive.clone())),
                    returns: Some(Box::new(func_type.result.clone())),
                }))
            }
        }
    }

    /// Resolve a type alias and return the resolved Type.
    /// Type parameters are resolved to Type::Variable placeholders.
    /// This is useful for testing and displaying type aliases.
    pub fn resolve_type_alias(&mut self, alias_name: &str) -> Result<Type, String> {
        quiver_compiler::compiler::resolve_type_alias_for_display(
            &self.type_aliases,
            alias_name,
            &mut self.program,
        )
        .map_err(|e| format!("{:?}", e))
    }
}
