use serde::{Deserialize, Serialize};
use tsify::Tsify;

// Re-export core types with Tsify annotations for TypeScript
// These mirror quiver_core types but with TypeScript bindings

#[derive(Serialize, Deserialize, Tsify, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum QuiverValue {
    Integer {
        value: i64,
    },
    BinaryConstant {
        index: usize,
    },
    BinaryHeap {
        index: usize,
    },
    Tuple {
        type_id: usize,
        values: Vec<QuiverValue>,
    },
    Function {
        index: usize,
        captures: Vec<QuiverValue>,
    },
    Builtin {
        name: String,
    },
    Process {
        pid: usize,
        function_index: usize,
    },
}

impl From<quiver_core::value::Value> for QuiverValue {
    fn from(value: quiver_core::value::Value) -> Self {
        match value {
            quiver_core::value::Value::Integer(i) => QuiverValue::Integer { value: i },
            quiver_core::value::Value::Binary(b) => match b {
                quiver_core::value::Binary::Constant(index) => {
                    QuiverValue::BinaryConstant { index }
                }
                quiver_core::value::Binary::Heap(index) => QuiverValue::BinaryHeap { index },
            },
            quiver_core::value::Value::Tuple(type_id, values) => QuiverValue::Tuple {
                type_id: type_id.0,
                values: values.into_iter().map(Into::into).collect(),
            },
            quiver_core::value::Value::Function(index, captures) => QuiverValue::Function {
                index,
                captures: captures.into_iter().map(Into::into).collect(),
            },
            quiver_core::value::Value::Builtin(name) => QuiverValue::Builtin { name },
            quiver_core::value::Value::Process(pid, function_index) => QuiverValue::Process {
                pid,
                function_index,
            },
        }
    }
}

impl From<QuiverValue> for quiver_core::value::Value {
    fn from(value: QuiverValue) -> Self {
        match value {
            QuiverValue::Integer { value } => quiver_core::value::Value::Integer(value),
            QuiverValue::BinaryConstant { index } => {
                quiver_core::value::Value::Binary(quiver_core::value::Binary::Constant(index))
            }
            QuiverValue::BinaryHeap { index } => {
                quiver_core::value::Value::Binary(quiver_core::value::Binary::Heap(index))
            }
            QuiverValue::Tuple { type_id, values } => quiver_core::value::Value::Tuple(
                quiver_core::bytecode::TypeId(type_id),
                values.into_iter().map(Into::into).collect(),
            ),
            QuiverValue::Function { index, captures } => quiver_core::value::Value::Function(
                index,
                captures.into_iter().map(Into::into).collect(),
            ),
            QuiverValue::Builtin { name } => quiver_core::value::Value::Builtin(name),
            QuiverValue::Process {
                pid,
                function_index,
            } => quiver_core::value::Value::Process(pid, function_index),
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
#[serde(tag = "type")]
pub enum JsResult<T> {
    #[serde(rename = "ok")]
    Ok { value: T },
    #[serde(rename = "error")]
    Err { error: String },
}

impl<T> JsResult<T> {
    pub fn ok(value: T) -> Self {
        JsResult::Ok { value }
    }

    pub fn err(error: impl ToString) -> Self {
        JsResult::Err {
            error: error.to_string(),
        }
    }
}

/// Variable with name and formatted type
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsVariable {
    pub name: String,
    #[serde(rename = "type")]
    pub var_type: String,
}

/// Process status
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
#[serde(rename_all = "snake_case")]
pub enum JsProcessStatus {
    Active,
    Waiting,
    Sleeping,
    Failed,
    Completed,
}

impl From<quiver_core::process::ProcessStatus> for JsProcessStatus {
    fn from(status: quiver_core::process::ProcessStatus) -> Self {
        match status {
            quiver_core::process::ProcessStatus::Active => JsProcessStatus::Active,
            quiver_core::process::ProcessStatus::Waiting => JsProcessStatus::Waiting,
            quiver_core::process::ProcessStatus::Sleeping => JsProcessStatus::Sleeping,
            quiver_core::process::ProcessStatus::Failed => JsProcessStatus::Failed,
            quiver_core::process::ProcessStatus::Completed => JsProcessStatus::Completed,
        }
    }
}

/// Process info for inspection
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsProcessInfo {
    pub id: usize,
    pub status: JsProcessStatus,
    #[serde(rename = "type")]
    pub process_type: Option<String>,
    pub stack_size: usize,
    pub locals_size: usize,
    pub frames_count: usize,
    pub mailbox_size: usize,
    pub persistent: bool,
    pub result: Option<JsResult<JsEvaluationResult>>,
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsProcess {
    pub id: usize,
    pub status: JsProcessStatus,
}

/// Evaluation result with value and heap
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsEvaluationResult {
    pub value: QuiverValue,
    pub heap: Vec<Vec<u8>>,
}
