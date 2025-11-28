use crate::effects::WebEffect;
use quiver_core::bytecode::Constant;
use quiver_core::executor::Executor;
use quiver_core::program::Program;
use quiver_core::value::Binary;
use serde::{Deserialize, Serialize};
use tsify::Tsify;

// Re-export core types with Tsify annotations for TypeScript
// These mirror quiver_core types but with TypeScript bindings

#[derive(Serialize, Deserialize, Tsify, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum Value {
    Integer {
        value: i64,
    },
    Binary {
        // Hex-encoded binary data for JSON serialization
        hex: String,
    },
    Tuple {
        #[serde(rename = "typeId")]
        type_id: usize,
        values: Vec<Value>,
    },
    Function {
        index: usize,
        captures: Vec<Value>,
    },
    Builtin {
        name: String,
    },
    Process {
        pid: usize,
        #[serde(rename = "functionIndex")]
        function_index: usize,
    },
    Resource {
        id: usize,
        #[serde(rename = "processId")]
        process_id: usize,
    },
}

impl Value {
    /// Convert web value to core value for formatting purposes
    /// Extracts hex-encoded binaries and adds them to the heap
    /// Returns (core_value, extended_heap)
    pub fn to_core_for_formatting(
        &self,
        heap: &[Vec<u8>],
    ) -> (quiver_core::value::Value, Vec<Vec<u8>>) {
        let mut extended_heap = heap.to_vec();
        let core_value = self.to_core_recursive(&mut extended_heap);
        (core_value, extended_heap)
    }

    fn to_core_recursive(&self, heap: &mut Vec<Vec<u8>>) -> quiver_core::value::Value {
        match self {
            Value::Integer { value } => quiver_core::value::Value::Integer(*value),
            Value::Binary { hex } => {
                // Decode hex and add to heap
                let bytes = hex::decode(hex).unwrap_or_default();
                let heap_idx = heap.len();
                heap.push(bytes);
                quiver_core::value::Value::Binary(Binary::Heap(heap_idx))
            }
            Value::Tuple { type_id, values } => quiver_core::value::Value::Tuple(
                *type_id,
                values.iter().map(|v| v.to_core_recursive(heap)).collect(),
            ),
            Value::Function { index, captures } => quiver_core::value::Value::Function(
                *index,
                captures.iter().map(|v| v.to_core_recursive(heap)).collect(),
            ),
            Value::Builtin { name: _ } => {
                // Web Value uses name, but core Value uses builtin_id
                // Use 0 as placeholder - this is only for formatting purposes
                quiver_core::value::Value::Builtin(0)
            }
            Value::Process {
                pid,
                function_index,
            } => quiver_core::value::Value::Process(*pid, *function_index),
            Value::Resource { id, process_id: _ } => {
                // Resource type_id is stored differently in web vs core
                // Use 0 as placeholder - resource_type_id is for type checking only
                quiver_core::value::Value::Resource(*id, 0)
            }
        }
    }

    /// Convert from core value to web value
    /// Requires heap data and program to resolve binary references
    pub fn from_core_value(
        value: &quiver_core::value::Value,
        heap_data: &[Vec<u8>],
        program: &Program,
    ) -> Self {
        match value {
            quiver_core::value::Value::Integer(i) => Value::Integer { value: *i },
            quiver_core::value::Value::Binary(binary) => {
                // Resolve binary reference to actual bytes
                let bytes = match binary {
                    Binary::Constant(idx) => program
                        .get_constant(*idx)
                        .and_then(|c| {
                            if let Constant::Binary(b) = c {
                                Some(b.as_slice())
                            } else {
                                None
                            }
                        })
                        .unwrap_or(&[]),
                    Binary::Heap(idx) => heap_data.get(*idx).map(|v| v.as_slice()).unwrap_or(&[]),
                };

                Value::Binary {
                    hex: hex::encode(bytes),
                }
            }
            quiver_core::value::Value::Tuple(type_id, values) => Value::Tuple {
                type_id: *type_id,
                values: values
                    .iter()
                    .map(|v| Value::from_core_value(v, heap_data, program))
                    .collect(),
            },
            quiver_core::value::Value::Function(index, captures) => Value::Function {
                index: *index,
                captures: captures
                    .iter()
                    .map(|v| Value::from_core_value(v, heap_data, program))
                    .collect(),
            },
            quiver_core::value::Value::Builtin(builtin_id) => {
                // Look up builtin name from program
                let name = program
                    .get_builtins()
                    .get(*builtin_id)
                    .map(|b| b.name.clone())
                    .unwrap_or_else(|| format!("builtin#{}", builtin_id));
                Value::Builtin { name }
            }
            quiver_core::value::Value::Process(pid, function_index) => Value::Process {
                pid: *pid,
                function_index: *function_index,
            },
            quiver_core::value::Value::Resource(id, _) => Value::Resource {
                id: *id,
                process_id: 0, // Resources are now tracked by Environment, not processes
            },
        }
    }

    /// Convert from web value to core value
    /// Requires mutable executor to allocate binaries to heap
    pub fn to_core_value(
        self,
        executor: &mut Executor<WebEffect>,
    ) -> std::result::Result<quiver_core::value::Value, String> {
        match self {
            Value::Integer { value } => Ok(quiver_core::value::Value::Integer(value)),
            Value::Binary { hex } => {
                // Decode hex string and allocate to executor heap
                let bytes = hex::decode(&hex).map_err(|e| format!("Invalid hex: {}", e))?;
                let binary = executor
                    .allocate_binary(bytes)
                    .map_err(|e| format!("Failed to allocate binary: {:?}", e))?;
                Ok(quiver_core::value::Value::Binary(binary))
            }
            Value::Tuple { type_id, values } => {
                let core_values: std::result::Result<Vec<_>, _> = values
                    .into_iter()
                    .map(|v| v.to_core_value(executor))
                    .collect();
                Ok(quiver_core::value::Value::Tuple(type_id, core_values?))
            }
            Value::Function { index, captures } => {
                let core_captures: std::result::Result<Vec<_>, _> = captures
                    .into_iter()
                    .map(|v| v.to_core_value(executor))
                    .collect();
                Ok(quiver_core::value::Value::Function(index, core_captures?))
            }
            Value::Builtin { name: _ } => {
                // Web Value stores name, but core Value needs builtin_id
                // We can't look up the ID without program context, use 0 as placeholder
                Ok(quiver_core::value::Value::Builtin(0))
            }
            Value::Process {
                pid,
                function_index,
            } => Ok(quiver_core::value::Value::Process(pid, function_index)),
            Value::Resource { id, process_id: _ } => {
                // Resource type_id is for type checking only, use 0 as placeholder
                Ok(quiver_core::value::Value::Resource(id, 0))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
#[serde(tag = "type")]
pub enum Result<T> {
    #[serde(rename = "ok")]
    Ok { value: T },
    #[serde(rename = "error")]
    Err { error: String },
}

impl<T> Result<T> {
    pub fn ok(value: T) -> Self {
        Result::Ok { value }
    }

    pub fn err(error: impl ToString) -> Self {
        Result::Err {
            error: error.to_string(),
        }
    }
}

/// Variable with name and formatted type
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct Variable {
    pub name: String,
    #[serde(rename = "type")]
    pub var_type: String,
}

/// Process status
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub enum ProcessStatus {
    Active,
    Waiting,
    Sleeping,
    Failed,
    Completed,
}

impl From<quiver_core::process::ProcessStatus> for ProcessStatus {
    fn from(status: quiver_core::process::ProcessStatus) -> Self {
        match status {
            quiver_core::process::ProcessStatus::Active => ProcessStatus::Active,
            quiver_core::process::ProcessStatus::Waiting => ProcessStatus::Waiting,
            quiver_core::process::ProcessStatus::Sleeping => ProcessStatus::Sleeping,
            quiver_core::process::ProcessStatus::Failed => ProcessStatus::Failed,
            quiver_core::process::ProcessStatus::Completed => ProcessStatus::Completed,
        }
    }
}

/// Process info for inspection
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct ProcessInfo {
    pub id: usize,
    pub status: ProcessStatus,
    #[serde(rename = "type")]
    pub process_type: Option<String>,
    #[serde(rename = "stackSize")]
    pub stack_size: usize,
    #[serde(rename = "localsCount")]
    pub locals_count: usize,
    #[serde(rename = "framesCount")]
    pub frames_count: usize,
    #[serde(rename = "mailboxSize")]
    pub mailbox_size: usize,
    pub persistent: bool,
    pub result: Option<Result<EvaluationResult>>,
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct Process {
    pub id: usize,
    pub status: ProcessStatus,
}

/// Evaluation result with value and heap
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct EvaluationResult {
    pub value: Value,
    pub heap: Vec<Vec<u8>>,
}
