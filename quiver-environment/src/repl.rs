use crate::WorkerHandle;
use crate::environment::{Environment, RequestResult};
use quiver_compiler::modules::ModuleLoader;
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_core::value::Value;
use std::collections::HashMap;

#[derive(Debug)]
pub enum ReplError {
    ParseError(Box<quiver_compiler::parser::Error>),
    CompileError(quiver_compiler::compiler::Error),
    RuntimeError(quiver_core::error::Error),
    Other(String),
}

impl std::fmt::Display for ReplError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReplError::ParseError(e) => write!(f, "Parse error: {:?}", e),
            ReplError::CompileError(e) => write!(f, "Compile error: {:?}", e),
            ReplError::RuntimeError(e) => write!(f, "Runtime error: {:?}", e),
            ReplError::Other(msg) => write!(f, "{}", msg),
        }
    }
}

pub struct Repl {
    environment: Environment,
    repl_process_id: Option<ProcessId>,
    program: Program,
    variable_map: HashMap<String, (Type, usize)>, // variable name -> (type, local index)
    last_result_type: Type, // Type of the last evaluated result, for continuations
    module_loader: Box<dyn ModuleLoader>,
}

#[derive(Clone, Copy)]
pub struct EvaluateRequest {
    process_id: ProcessId,
    result_request_id: Option<u64>, // Track the request ID for getting the result
}

impl Repl {
    pub fn new(
        workers: Vec<Box<dyn WorkerHandle>>,
        program: Program,
        module_loader: Box<dyn ModuleLoader>,
    ) -> Result<Self, String> {
        let environment = Environment::new(workers, &program)?;

        Ok(Self {
            environment,
            repl_process_id: None,
            program,
            variable_map: HashMap::new(),
            last_result_type: Type::nil(),
            module_loader,
        })
    }

    /// Compile and evaluate an expression
    /// Returns a request that can be polled for the result
    pub fn evaluate(&mut self, source: &str) -> Result<EvaluateRequest, ReplError> {
        use quiver_compiler::Compiler;
        use quiver_compiler::compiler::ModuleCache;

        // Parse the source
        let parsed = quiver_compiler::parse(source).map_err(|e| ReplError::ParseError(Box::new(e)))?;

        // Compile with existing variables
        let existing_vars = if self.variable_map.is_empty() {
            None
        } else {
            Some(&self.variable_map)
        };

        let (instructions, result_type, variables, updated_program, _type_aliases, _module_cache) =
            Compiler::compile(
                parsed,
                HashMap::new(), // type_aliases
                ModuleCache::new(),
                self.module_loader.as_ref(),
                &self.program,
                None, // module_path
                existing_vars,
                self.last_result_type.clone(), // parameter_type - use previous result type for continuations
            )
            .map_err(|e| ReplError::CompileError(e))?;

        // Update variable map with new variables
        self.variable_map = variables;

        // Update last result type for next evaluation (supports continuations)
        self.last_result_type = result_type;

        // Get only the NEW program data (not already sent)
        let old_constants_len = self.program.get_constants().len();
        let old_functions_len = self.program.get_functions().len();
        let old_builtins_len = self.program.get_builtins().len();

        let all_constants = updated_program.get_constants();
        let all_functions = updated_program.get_functions();
        let all_builtins = updated_program.get_builtins();

        let new_constants = all_constants[old_constants_len..].to_vec();
        let new_functions = all_functions[old_functions_len..].to_vec();
        let new_types = updated_program.get_types(); // Types are always sent (they're a HashMap)
        let new_builtins = all_builtins[old_builtins_len..].to_vec();

        // Update our local program reference
        self.program = updated_program;

        // Send UpdateProgram to all workers (additive - only new items)
        self.environment
            .update_program(new_constants, new_functions, new_types, new_builtins)
            .map_err(|e| ReplError::Other(e))?;

        // Remove the final Return instruction for REPL execution
        // In a REPL, we want to leave the result on the stack without returning
        let mut repl_instructions = instructions;
        if let Some(last) = repl_instructions.last() {
            if matches!(last, quiver_core::bytecode::Instruction::Return) {
                repl_instructions.pop();
            }
        }

        // Wrap instructions in a function and register with program
        use quiver_core::bytecode::Function;
        let function = Function {
            instructions: repl_instructions,
            function_type: None,
            captures: vec![],
        };
        let function_index = self.program.register_function(function);

        // Send the new function to all workers
        let new_functions = vec![self.program.get_functions()[function_index].clone()];
        self.environment.update_program(
            vec![],
            new_functions,
            std::collections::HashMap::new(),
            vec![],
        ).map_err(|e| ReplError::Other(e))?;

        // Create or wake the REPL process
        let repl_process_id = match self.repl_process_id {
            Some(pid) => {
                // Wake existing process with new function
                self.environment.wake_process(pid, function_index).map_err(|e| ReplError::Other(e))?;
                pid
            }
            None => {
                // Create the persistent REPL process on first evaluation
                let pid = self.environment.start_process(function_index, true).map_err(|e| ReplError::Other(e))?;
                self.repl_process_id = Some(pid);
                pid
            }
        };

        // Return immediately - poll_evaluate() will handle waiting for completion
        Ok(EvaluateRequest {
            process_id: repl_process_id,
            result_request_id: None, // Will be set on first poll
        })
    }

    /// Poll for evaluation result (non-blocking)
    pub fn poll_evaluate(
        &mut self,
        request: &mut EvaluateRequest,
    ) -> Option<Result<(Value, Vec<Vec<u8>>), ReplError>> {
        // Step the environment to process events from workers
        let _ = self.environment.step();

        // Make request only on first poll, reuse thereafter
        let req_id = match request.result_request_id {
            Some(id) => id,
            None => {
                // First poll - make the request
                let id = match self.environment.request_result(request.process_id) {
                    Ok(id) => id,
                    Err(e) => return Some(Err(ReplError::Other(format!("Failed to request result: {}", e)))),
                };
                request.result_request_id = Some(id);
                id
            }
        };

        // Step again to process the result request/response
        let _ = self.environment.step();

        // Poll for the result
        match self.environment.poll_request(req_id) {
            Some(RequestResult::Result(Some((value, heap)))) => {
                // Got a result! Clear the request ID for next evaluation
                request.result_request_id = None;

                // Compact locals to remove unused variables
                if let Err(e) = self.compact() {
                    return Some(Err(ReplError::Other(format!("Failed to compact locals: {}", e))));
                }

                Some(Ok((value, heap)))
            }
            Some(RequestResult::Result(None)) => {
                // Process not done yet, make a new request on next poll
                request.result_request_id = None;
                None
            }
            Some(RequestResult::RuntimeError(error)) => {
                // Got a runtime error
                request.result_request_id = None;
                Some(Err(ReplError::RuntimeError(error)))
            }
            Some(_) => {
                request.result_request_id = None;
                Some(Err(ReplError::Other("Unexpected result type".to_string())))
            }
            None => {
                // Request not ready yet - keep polling with same request ID
                None
            }
        }
    }

    /// Wait for evaluation result (blocking)
    pub fn wait_evaluate(&mut self, mut request: EvaluateRequest) -> Result<(Value, Vec<Vec<u8>>), ReplError> {
        let start = std::time::Instant::now();
        let timeout = std::time::Duration::from_secs(5);

        loop {
            if let Some(result) = self.poll_evaluate(&mut request) {
                return result;
            }

            if start.elapsed() > timeout {
                return Err(ReplError::Other("Evaluation timed out after 5 seconds".to_string()));
            }

            // Brief sleep to avoid spinning
            std::thread::sleep(std::time::Duration::from_micros(10));
        }
    }

    /// Get variable value by name
    pub fn get_variable(&mut self, name: &str) -> Result<Value, String> {
        let (_, local_index) = self
            .variable_map
            .get(name)
            .ok_or_else(|| format!("Variable '{}' not found", name))?;

        let repl_process_id = self
            .repl_process_id
            .ok_or_else(|| "No REPL process started yet".to_string())?;

        let request_id = self
            .environment
            .request_locals(repl_process_id, vec![*local_index])?;

        let result = self.environment.wait_for_request(request_id)?;
        match result {
            RequestResult::Locals(mut locals) => {
                let (value, _heap) = locals.pop().ok_or_else(|| "No local found".to_string())?;
                Ok(value)
            }
            _ => Err("Unexpected result type".to_string()),
        }
    }

    /// Get all variable names and their types
    pub fn get_variables(&self) -> Vec<(String, Type)> {
        self.variable_map
            .iter()
            .map(|(name, (ty, _))| (name.clone(), ty.clone()))
            .collect()
    }

    /// Compact locals to remove unused variables (called automatically after evaluation)
    fn compact(&mut self) -> Result<(), String> {
        let repl_process_id = self
            .repl_process_id
            .ok_or_else(|| "No REPL process started yet".to_string())?;

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
                .ok_or_else(|| "Variable index not found in mapping".to_string())?;
        }

        // Compact the locals on the worker
        self.environment
            .compact_locals(repl_process_id, keep_indices)?;

        Ok(())
    }

    /// Step the environment (process events)
    pub fn step(&mut self) -> Result<bool, String> {
        self.environment.step()
    }

    /// Get all process statuses across all workers
    pub fn get_process_statuses(&mut self) -> Result<HashMap<ProcessId, ProcessStatus>, String> {
        let request_ids = self.environment.request_statuses()?;

        // Collect results from all workers
        let mut all_statuses = HashMap::new();
        for request_id in request_ids {
            let result = self.environment.wait_for_request(request_id)?;
            if let RequestResult::Statuses(statuses) = result {
                all_statuses.extend(statuses);
            }
        }
        Ok(all_statuses)
    }

    /// Get process info for a specific process
    pub fn get_process_info(&mut self, pid: ProcessId) -> Result<Option<ProcessInfo>, String> {
        let request_id = self.environment.request_process_info(pid)?;
        let result = self.environment.wait_for_request(request_id)?;

        match result {
            RequestResult::Info(info) => Ok(info),
            _ => Err("Unexpected result type".to_string()),
        }
    }

    /// Format a value for display
    pub fn format_value(&self, value: &Value, heap: &[Vec<u8>]) -> String {
        quiver_core::format::format_value(value, heap, &self.program)
    }

    /// Format a type for display
    pub fn format_type(&self, ty: &Type) -> String {
        quiver_core::format::format_type(&self.program, ty)
    }
}
