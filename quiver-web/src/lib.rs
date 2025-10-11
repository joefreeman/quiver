use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{Compiler, InMemoryModuleLoader, parse};
use quiver_core::format;
use quiver_core::Executor;
use quiver_core::process::{Action, ProcessId, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_core::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

macro_rules! load_stdlib_modules {
    ($($name:literal),* $(,)?) => {{
        let mut modules = HashMap::new();
        $(
            modules.insert(
                $name.to_string(),
                include_str!(concat!("../../std/", $name, ".qv")).to_string()
            );
        )*
        modules
    }};
}

#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub struct QuiverRuntime {
    executor: Executor,
    next_process_id: usize,
    program: Program,
    type_aliases: HashMap<String, Type>,
    module_cache: ModuleCache,
}

#[derive(Serialize, Deserialize)]
pub struct CompileResult {
    success: bool,
    error: Option<String>,
    entry_function: Option<usize>,
}

#[derive(Serialize, Deserialize)]
pub struct ProcessInfo {
    pub id: u64,
    pub status: String,
    pub stack_size: usize,
}

#[derive(Serialize, Deserialize)]
pub struct StepResultInfo {
    pub status: String,
    pub processes: Vec<ProcessInfo>,
}

#[wasm_bindgen]
impl QuiverRuntime {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Result<QuiverRuntime, JsValue> {
        let program = Program::new();
        let executor = Executor::new(&program);

        Ok(QuiverRuntime {
            executor,
            next_process_id: 0,
            program,
            type_aliases: HashMap::new(),
            module_cache: ModuleCache::new(),
        })
    }

    /// Load standard library modules
    fn load_stdlib() -> HashMap<String, String> {
        load_stdlib_modules!("math", "list")
    }

    #[wasm_bindgen]
    pub fn compile(&mut self, source: &str, modules: JsValue) -> Result<JsValue, JsValue> {
        let user_modules: Option<HashMap<String, String>> = if modules.is_null()
            || modules.is_undefined()
        {
            None
        } else {
            Some(
                serde_wasm_bindgen::from_value(modules)
                    .map_err(|e| JsValue::from_str(&format!("Failed to parse modules: {}", e)))?,
            )
        };

        let ast_program =
            parse(source).map_err(|e| JsValue::from_str(&format!("Parse error: {:?}", e)))?;

        // Merge stdlib with user-provided modules (user modules take precedence)
        let mut all_modules = Self::load_stdlib();
        if let Some(user_modules) = user_modules {
            all_modules.extend(user_modules);
        }

        let module_loader = InMemoryModuleLoader::new(all_modules);

        match Compiler::compile(
            ast_program,
            self.type_aliases.clone(),
            self.module_cache.clone(),
            &module_loader,
            &self.program,
            None,
            None,
            Type::nil(),
        ) {
            Ok((
                instructions,
                _result_type,
                _variables,
                new_program,
                new_type_aliases,
                new_module_cache,
            )) => {
                self.program = new_program;
                self.type_aliases = new_type_aliases;
                self.module_cache = new_module_cache;

                // Execute the compiled instructions to get the main function
                let (result_value, new_executor) =
                    quiver_core::execute_instructions_sync(&self.program, instructions)
                        .map_err(|e| JsValue::from_str(&format!("Execution error: {:?}", e)))?;

                self.executor = new_executor;

                // Extract the entry function index from the result, injecting captures if needed
                let entry_function = match result_value {
                    Some(Value::Function(func_idx, captures)) => {
                        let final_func_idx = if !captures.is_empty() {
                            self.program.inject_function_captures(
                                func_idx,
                                captures,
                                &self.executor,
                            )
                        } else {
                            func_idx
                        };
                        Some(final_func_idx)
                    }
                    _ => None,
                };

                // Recreate executor after injecting captures (which modifies self.program)
                self.executor = Executor::new(&self.program);

                let result = CompileResult {
                    success: true,
                    error: None,
                    entry_function,
                };
                serde_wasm_bindgen::to_value(&result)
                    .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
            }
            Err(e) => {
                let result = CompileResult {
                    success: false,
                    error: Some(format!("{:?}", e)),
                    entry_function: None,
                };
                serde_wasm_bindgen::to_value(&result)
                    .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
            }
        }
    }

    #[wasm_bindgen]
    pub fn spawn(&mut self, entry_function: usize, persistent: bool) -> u64 {
        let process_id = ProcessId(self.next_process_id);
        self.next_process_id += 1;
        self.executor.spawn_process(process_id, persistent);

        // Initialize the process with the entry function
        if let Some(process) = self.executor.get_process_mut(process_id) {
            process.stack.push(Value::nil());
            if let Some(function) = self.program.get_function(entry_function) {
                let frame = quiver_core::process::Frame::new(function.instructions.clone(), 0, 0);
                process.frames.push(frame);
            }
        }

        process_id.0 as u64
    }

    #[wasm_bindgen]
    pub fn step(&mut self, max_steps: usize) -> Result<JsValue, JsValue> {
        let result = self
            .executor
            .step(max_steps)
            .map_err(|e| JsValue::from_str(&format!("Execution error: {:?}", e)))?;

        // Handle routing requests from the step
        match result {
            None => {
                // No routing needed
            }
            Some(Action::Spawn {
                caller,
                function_index,
                captures,
            }) => {
                // Allocate a new process ID for the spawned process
                let new_pid = ProcessId(self.next_process_id);
                self.next_process_id += 1;

                // Spawn the new process
                self.executor.spawn_process(new_pid, false);

                // Set up the process with the function and captures
                if let Some(process) = self.executor.get_process_mut(new_pid) {
                    process.stack.push(Value::nil());
                    process
                        .stack
                        .push(Value::Function(function_index, captures));
                    process.frames.push(quiver_core::process::Frame::new(
                        vec![quiver_core::bytecode::Instruction::Call],
                        0,
                        0,
                    ));
                }

                // Add the new process to the queue so it can run
                self.executor.add_to_queue(new_pid);

                // Notify the caller with the new PID
                self.executor.notify_spawn(caller, Value::Pid(new_pid));
            }
            Some(Action::Deliver { target, value }) => {
                // Deliver message to target process
                self.executor.notify_message(target, value);
            }
            Some(Action::AwaitResult { target, caller }) => {
                // Check if target process has a result available
                let result = self
                    .executor
                    .get_process(target)
                    .and_then(|p| p.result.clone());
                if let Some(result) = result {
                    // Result is available, notify the caller immediately
                    self.executor.notify_result(caller, result);
                }
                // If result not available, the caller will remain in waiting state
            }
        }

        // Collect process information
        let processes: Vec<ProcessInfo> = self
            .executor
            .get_process_statuses()
            .into_keys()
            .filter_map(|pid| {
                self.executor.get_process_info(pid).map(|info| ProcessInfo {
                    id: info.id.0 as u64,
                    status: match info.status {
                        ProcessStatus::Active => "active".to_string(),
                        ProcessStatus::Waiting => "waiting".to_string(),
                        ProcessStatus::Sleeping => "sleeping".to_string(),
                        ProcessStatus::Terminated => "terminated".to_string(),
                    },
                    stack_size: info.stack_size,
                })
            })
            .collect();

        // Determine overall status: idle if no active processes, running otherwise
        let has_active_processes = self
            .executor
            .get_process_statuses()
            .values()
            .any(|status| !matches!(status, ProcessStatus::Terminated));

        let status_str = if has_active_processes {
            "running"
        } else {
            "idle"
        };

        let step_result = StepResultInfo {
            status: status_str.to_string(),
            processes,
        };

        serde_wasm_bindgen::to_value(&step_result)
            .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
    }

    #[wasm_bindgen]
    pub fn get_result(&self, process_id: u64) -> Result<JsValue, JsValue> {
        let pid = ProcessId(process_id as usize);

        if let Some(process) = self.executor.get_process(pid) {
            let constants = self.program.get_constants();
            if let Some(value) = &process.result {
                // For web runtime, heap data is directly accessible via the executor
                // We pass empty heap_data since all binaries should be constants
                let formatted = format::format_value(value, &[], constants, &self.program);
                Ok(JsValue::from_str(&formatted))
            } else if let Some(value) = process.stack.last() {
                let formatted = format::format_value(value, &[], constants, &self.program);
                Ok(JsValue::from_str(&formatted))
            } else {
                Ok(JsValue::NULL)
            }
        } else {
            Err(JsValue::from_str("Process not found"))
        }
    }
}
