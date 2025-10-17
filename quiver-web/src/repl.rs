use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{EvaluationStage, EvaluationState};
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{Compiler, ModuleLoader, parse};
use quiver_core::ProcessId;
use quiver_core::format;
use quiver_core::types::Type;
use quiver_core::value::Value;
use quiver_environment::Environment;

/// Web-specific REPL
pub struct Repl {
    environment: Rc<RefCell<Environment<crate::runtime::WebCommandSender>>>,
    repl_process_id: ProcessId,
    type_aliases: HashMap<String, Type>,
    module_cache: ModuleCache,
    module_loader: Box<dyn ModuleLoader>,
}

impl Repl {
    pub fn from_parts(
        environment: Rc<RefCell<Environment<crate::runtime::WebCommandSender>>>,
        repl_process_id: ProcessId,
        type_aliases: HashMap<String, Type>,
        module_cache: ModuleCache,
        module_loader: Box<dyn ModuleLoader>,
    ) -> Self {
        Self {
            environment,
            repl_process_id,
            type_aliases,
            module_cache,
            module_loader,
        }
    }

    /// Start an evaluation. Returns (self, eval_state) on success, or (self, error) on failure.
    pub fn start_evaluate(
        mut self,
        source: &str,
        module_path: Option<std::path::PathBuf>,
        variables: Option<&HashMap<String, (Type, usize)>>,
        parameter_type: Option<Type>,
    ) -> Result<(Self, EvaluationState), (Self, String)> {
        let ast_program = match parse(source) {
            Ok(p) => p,
            Err(e) => return Err((self, format!("Parse error: {:?}", e))),
        };

        let old_program = self.environment.borrow().program().clone();
        let parameter_type = parameter_type.unwrap_or_else(Type::nil);

        let (
            instructions,
            result_type,
            new_variables,
            new_program,
            new_type_aliases,
            new_module_cache,
        ) = match Compiler::compile(
            ast_program,
            self.type_aliases.clone(),
            self.module_cache.clone(),
            self.module_loader.as_ref(),
            &old_program,
            module_path,
            variables,
            parameter_type,
        ) {
            Ok(compiled) => compiled,
            Err(e) => return Err((self, format!("Compile error: {:?}", e))),
        };

        self.environment.borrow_mut().update_program(new_program);
        self.type_aliases = new_type_aliases;
        self.module_cache = new_module_cache;

        let wake_result = Rc::new(RefCell::new(None));
        let wake_result_clone = wake_result.clone();
        self.environment
            .borrow_mut()
            .wake(self.repl_process_id, instructions, move |result| {
                *wake_result_clone.borrow_mut() = Some(result);
            });

        let value_result = Rc::new(RefCell::new(None));

        let eval_state = EvaluationState {
            result_type,
            new_variables,
            wake_result,
            value_result,
            compact_result: None,
            stage: EvaluationStage::WaitingForWake,
        };

        Ok((self, eval_state))
    }

    /// Poll the evaluation state. Call repeatedly until eval_state.stage == Done.
    pub fn poll_evaluate(&mut self, eval_state: &mut EvaluationState) {
        // No need to poll for events - callbacks are invoked immediately on web

        match eval_state.stage {
            EvaluationStage::WaitingForWake => {
                if eval_state.wake_result.borrow().is_some() {
                    // Wake completed, start getting the result
                    let value_result_clone = eval_state.value_result.clone();
                    self.environment
                        .borrow_mut()
                        .get_result(self.repl_process_id, move |result| {
                            *value_result_clone.borrow_mut() = Some(result);
                        });
                    eval_state.stage = EvaluationStage::WaitingForResult;
                }
            }
            EvaluationStage::WaitingForResult => {
                if eval_state.value_result.borrow().is_some() {
                    // Result obtained, check if we need to compact locals
                    if !eval_state.new_variables.is_empty() {
                        let mut sorted_vars: Vec<_> = eval_state.new_variables.iter().collect();
                        sorted_vars.sort_by_key(|(_, (_, idx))| *idx);
                        let referenced_indices: Vec<usize> =
                            sorted_vars.iter().map(|(_, (_, idx))| *idx).collect();

                        let compact_result = Rc::new(RefCell::new(None));
                        let compact_result_clone = compact_result.clone();
                        self.environment.borrow_mut().compact_locals(
                            self.repl_process_id,
                            &referenced_indices,
                            move |result| {
                                *compact_result_clone.borrow_mut() = Some(result);
                            },
                        );

                        eval_state.compact_result = Some(compact_result);
                        eval_state.stage = EvaluationStage::WaitingForCompact;
                    } else {
                        eval_state.stage = EvaluationStage::Done;
                    }
                }
            }
            EvaluationStage::WaitingForCompact => {
                if let Some(ref compact_result) = eval_state.compact_result {
                    if compact_result.borrow().is_some() {
                        eval_state.stage = EvaluationStage::Done;
                    }
                }
            }
            EvaluationStage::Done => {}
        }
    }

    /// Finish the evaluation and extract the result.
    pub fn finish_evaluate(
        self,
        eval_state: EvaluationState,
    ) -> Result<
        (
            Self,
            Option<Value>,
            Type,
            HashMap<String, (Type, usize)>,
            Vec<Vec<u8>>,
        ),
        (Self, String),
    > {
        // Check wake result
        let wake_result = eval_state.wake_result.borrow();
        if let Some(Err(ref e)) = *wake_result {
            return Err((self, format!("Runtime error: {:?}", e)));
        }

        // Get the value result
        let value_result = eval_state.value_result.borrow();
        let (value, heap_data) = match value_result.as_ref() {
            Some(Ok((v, h))) => (v.clone(), h.clone()),
            Some(Err(e)) => return Err((self, format!("Runtime error: {:?}", e))),
            None => return Err((self, "Value result not available".to_string())),
        };

        // Check compact result if present
        if let Some(ref compact_result) = eval_state.compact_result {
            let compact_res = compact_result.borrow();
            if let Some(Err(ref e)) = *compact_res {
                return Err((self, format!("Runtime error: {:?}", e)));
            }

            // Remap variables
            let mut sorted_vars: Vec<_> = eval_state.new_variables.iter().collect();
            sorted_vars.sort_by_key(|(_, (_, idx))| *idx);
            let mut compacted_variables = HashMap::new();
            for (new_idx, (name, (typ, _))) in sorted_vars.into_iter().enumerate() {
                compacted_variables.insert(name.clone(), (typ.clone(), new_idx));
            }

            Ok((
                self,
                Some(value),
                eval_state.result_type,
                compacted_variables,
                heap_data,
            ))
        } else {
            Ok((
                self,
                Some(value),
                eval_state.result_type,
                eval_state.new_variables,
                heap_data,
            ))
        }
    }

    /// Start getting variables asynchronously. Returns (self, result_cell, var_list).
    /// The caller should poll process_pending_events() until result_cell is populated.
    pub fn start_get_variables(
        self,
        variables: &HashMap<String, (Type, usize)>,
    ) -> (
        Self,
        Rc<RefCell<Option<Result<Vec<Value>, quiver_core::error::Error>>>>,
        Vec<(String, usize)>,
    ) {
        let mut var_list: Vec<(String, usize)> = variables
            .iter()
            .map(|(name, (_, index))| (name.clone(), *index))
            .collect();
        var_list.sort_by_key(|(_, index)| *index);

        let indices: Vec<usize> = var_list.iter().map(|(_, index)| *index).collect();

        let locals_result = Rc::new(RefCell::new(None));
        let locals_result_clone = locals_result.clone();
        self.environment
            .borrow_mut()
            .get_locals(self.repl_process_id, &indices, move |result| {
                *locals_result_clone.borrow_mut() = Some(result);
            });

        (self, locals_result, var_list)
    }

    pub fn format_value(&self, value: &Value, heap: &[Vec<u8>]) -> String {
        let env = self.environment.borrow();
        let constants = env.program().get_constants();
        format::format_value(value, heap, constants, env.program())
    }

    pub fn format_type(&self, type_def: &Type) -> String {
        format::format_type(self.environment.borrow().program(), type_def)
    }
}
