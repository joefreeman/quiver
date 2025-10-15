mod repl;
mod runtime;
mod worker_executor;

use quiver_environment::Environment;
use repl::Repl;
use runtime::WebRuntime;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;
pub use worker_executor::WorkerExecutor;

#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

#[derive(Serialize, Deserialize)]
pub struct EvaluateResult {
    pub success: bool,
    pub error: Option<String>,
    pub value: Option<String>,
    pub type_str: Option<String>,
}

/// Common state needed during REPL initialization
struct InitializingData {
    environment: Environment<WebRuntime>,
    type_aliases: HashMap<String, quiver_core::types::Type>,
    module_cache: quiver_compiler::compiler::ModuleCache,
    module_loader: Box<dyn quiver_compiler::ModuleLoader>,
}

enum ReplState {
    /// Waiting for workers to be ready before starting REPL process
    WaitingForWorker {
        data: InitializingData,
        poll_count: u32,
    },
    /// Waiting for the REPL process to be created
    Initializing {
        data: InitializingData,
        repl_process_id_cell:
            std::rc::Rc<std::cell::RefCell<Option<quiver_core::process::ProcessId>>>,
    },
    /// REPL is ready to evaluate expressions
    Ready(Repl),
    /// Currently evaluating an expression
    Evaluating {
        repl: Repl,
        eval_state: EvaluationState,
    },
    /// An error occurred
    Error(String),
}

type AsyncResult<T> = std::rc::Rc<std::cell::RefCell<Option<Result<T, quiver_core::error::Error>>>>;

pub struct EvaluationState {
    pub result_type: quiver_core::types::Type,
    pub new_variables: HashMap<String, (quiver_core::types::Type, usize)>,
    pub wake_result: AsyncResult<()>,
    pub value_result: AsyncResult<(quiver_core::value::Value, Vec<Vec<u8>>)>,
    pub compact_result: Option<AsyncResult<()>>,
    pub stage: EvaluationStage,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EvaluationStage {
    WaitingForWake,
    WaitingForResult,
    WaitingForCompact,
    Done,
}

/// Web-based REPL wrapper with polling API for async operations
#[wasm_bindgen]
pub struct QuiverRepl {
    state: ReplState,
    variables: HashMap<String, (quiver_core::types::Type, usize)>,
}

#[wasm_bindgen]
impl QuiverRepl {
    #[wasm_bindgen(constructor)]
    pub fn new() -> QuiverRepl {
        let program = quiver_core::program::Program::new();
        let runtime = WebRuntime::new();
        let environment = Environment::new(runtime, program, 4);

        QuiverRepl {
            state: ReplState::WaitingForWorker {
                data: InitializingData {
                    environment,
                    type_aliases: HashMap::new(),
                    module_cache: quiver_compiler::compiler::ModuleCache::new(),
                    module_loader: Box::new(quiver_compiler::InMemoryModuleLoader::new(
                        Self::load_stdlib(),
                    )),
                },
                poll_count: 0,
            },
            variables: HashMap::new(),
        }
    }

    /// Poll initialization status. Returns true when ready, false when still initializing.
    /// Call this repeatedly (e.g., with setTimeout) until it returns true.
    #[wasm_bindgen]
    pub fn poll_init(&mut self) -> bool {
        match &mut self.state {
            ReplState::WaitingForWorker { data, poll_count } => {
                // Poll for events (worker should send empty array when ready)
                data.environment.process_pending_events();
                *poll_count += 1;

                // Wait for at least 5 polls (~50ms) to give worker time to initialize
                if *poll_count >= 5 {
                    // Transition to Initializing state and send Execute command
                    if let ReplState::WaitingForWorker { mut data, .. } = std::mem::replace(
                        &mut self.state,
                        ReplState::Error("Transitioning".to_string()),
                    ) {
                        let repl_process_id = std::rc::Rc::new(std::cell::RefCell::new(None));
                        let repl_process_id_clone = repl_process_id.clone();

                        data.environment.execute(vec![], true, move |result| {
                            match result {
                                Ok(pid) => *repl_process_id_clone.borrow_mut() = Some(pid),
                                Err(_) => {} // Will be caught in next poll
                            }
                        });

                        self.state = ReplState::Initializing {
                            data,
                            repl_process_id_cell: repl_process_id,
                        };
                    }
                }
                false
            }
            ReplState::Initializing {
                data,
                repl_process_id_cell,
            } => {
                // Poll for Execute response
                data.environment.process_pending_events();
                let pid_opt = *repl_process_id_cell.borrow();

                // If we have a PID, transition to Ready state
                if let Some(pid) = pid_opt {
                    if let ReplState::Initializing { data, .. } = std::mem::replace(
                        &mut self.state,
                        ReplState::Error("Transitioning".to_string()),
                    ) {
                        let repl = Repl::from_parts(
                            data.environment,
                            pid,
                            data.type_aliases,
                            data.module_cache,
                            data.module_loader,
                        );
                        self.state = ReplState::Ready(repl);
                        return true;
                    }
                }
                false
            }
            ReplState::Ready(_) => true,
            ReplState::Evaluating { .. } => true,
            ReplState::Error(_) => true,
        }
    }

    /// Load standard library modules
    fn load_stdlib() -> HashMap<String, String> {
        let mut modules = HashMap::new();
        modules.insert(
            "math".to_string(),
            include_str!("../../std/math.qv").to_string(),
        );
        modules.insert(
            "list".to_string(),
            include_str!("../../std/list.qv").to_string(),
        );
        modules
    }

    /// Start evaluating source code. Returns immediately.
    /// Call poll_evaluate() repeatedly until it returns a result.
    #[wasm_bindgen]
    pub fn evaluate(&mut self, source: &str) -> Result<(), JsValue> {
        let repl = match std::mem::replace(
            &mut self.state,
            ReplState::Error("Transitioning".to_string()),
        ) {
            ReplState::Ready(repl) => repl,
            ReplState::WaitingForWorker { .. } | ReplState::Initializing { .. } => {
                return Err(JsValue::from_str(
                    "REPL not initialized yet - call poll_init() until ready",
                ));
            }
            ReplState::Evaluating { .. } => {
                return Err(JsValue::from_str(
                    "Already evaluating - call poll_evaluate() to check status",
                ));
            }
            ReplState::Error(e) => {
                return Err(JsValue::from_str(&format!("REPL in error state: {}", e)));
            }
        };

        let module_path = None;
        let parameter_type = None;

        match repl.start_evaluate(source, module_path, Some(&self.variables), parameter_type) {
            Ok((repl, eval_state)) => {
                self.state = ReplState::Evaluating { repl, eval_state };
                Ok(())
            }
            Err((repl, err)) => {
                self.state = ReplState::Ready(repl);
                Err(JsValue::from_str(&err))
            }
        }
    }

    /// Poll the evaluation status. Returns null if still evaluating, or the result when done.
    #[wasm_bindgen]
    pub fn poll_evaluate(&mut self) -> Result<JsValue, JsValue> {
        match &mut self.state {
            ReplState::Evaluating { repl, eval_state } => {
                repl.poll_evaluate(eval_state);

                if eval_state.stage == EvaluationStage::Done {
                    self.finalize_evaluation()
                } else {
                    Ok(JsValue::NULL)
                }
            }
            ReplState::Ready(_) => Err(JsValue::from_str("Not evaluating - call evaluate() first")),
            ReplState::WaitingForWorker { .. } | ReplState::Initializing { .. } => {
                Err(JsValue::from_str("REPL not initialized yet"))
            }
            ReplState::Error(e) => Err(JsValue::from_str(&format!("REPL in error state: {}", e))),
        }
    }

    fn finalize_evaluation(&mut self) -> Result<JsValue, JsValue> {
        if let ReplState::Evaluating { repl, eval_state } = std::mem::replace(
            &mut self.state,
            ReplState::Error("Transitioning".to_string()),
        ) {
            match repl.finish_evaluate(eval_state) {
                Ok((repl, result, result_type, new_variables, heap_data)) => {
                    self.variables = new_variables;
                    let eval_result = Self::format_success(&repl, result, result_type, &heap_data);
                    self.state = ReplState::Ready(repl);
                    Self::serialize_result(&eval_result)
                }
                Err((repl, err)) => {
                    let eval_result = Self::format_error(err);
                    self.state = ReplState::Ready(repl);
                    Self::serialize_result(&eval_result)
                }
            }
        } else {
            Err(JsValue::from_str("Internal state error"))
        }
    }

    fn format_success(
        repl: &Repl,
        result: Option<quiver_core::value::Value>,
        result_type: quiver_core::types::Type,
        heap_data: &[Vec<u8>],
    ) -> EvaluateResult {
        let (value, type_str) = if let Some(v) = result {
            (
                Some(repl.format_value(&v, heap_data)),
                Some(repl.format_type(&result_type)),
            )
        } else {
            (None, None)
        };

        EvaluateResult {
            success: true,
            error: None,
            value,
            type_str,
        }
    }

    fn format_error(err: String) -> EvaluateResult {
        EvaluateResult {
            success: false,
            error: Some(err),
            value: None,
            type_str: None,
        }
    }

    fn serialize_result(result: &EvaluateResult) -> Result<JsValue, JsValue> {
        serde_wasm_bindgen::to_value(result)
            .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
    }

    #[wasm_bindgen]
    pub fn get_variables(&mut self) -> Result<JsValue, JsValue> {
        let repl = match &mut self.state {
            ReplState::Ready(repl) => repl,
            ReplState::Evaluating { repl, .. } => repl,
            ReplState::WaitingForWorker { .. } | ReplState::Initializing { .. } => {
                return Err(JsValue::from_str("REPL not initialized yet"));
            }
            ReplState::Error(e) => {
                return Err(JsValue::from_str(&format!("REPL in error state: {}", e)));
            }
        };

        let vars = repl.get_variables(&self.variables);

        let formatted_vars: Vec<(String, String, String)> = vars
            .into_iter()
            .map(|(name, value)| {
                let (var_type, _) = &self.variables[&name];
                (
                    name,
                    repl.format_value(&value, &[]),
                    repl.format_type(var_type),
                )
            })
            .collect();

        serde_wasm_bindgen::to_value(&formatted_vars)
            .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
    }
}
