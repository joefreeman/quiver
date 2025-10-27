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
    BinaryConstant {
        index: usize,
    },
    BinaryHeap {
        index: usize,
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
}

impl From<quiver_core::value::Value> for Value {
    fn from(value: quiver_core::value::Value) -> Self {
        match value {
            quiver_core::value::Value::Integer(i) => Value::Integer { value: i },
            quiver_core::value::Value::Binary(b) => match b {
                quiver_core::value::Binary::Constant(index) => Value::BinaryConstant { index },
                quiver_core::value::Binary::Heap(index) => Value::BinaryHeap { index },
            },
            quiver_core::value::Value::Tuple(type_id, values) => Value::Tuple {
                type_id: type_id.0,
                values: values.into_iter().map(Into::into).collect(),
            },
            quiver_core::value::Value::Function(index, captures) => Value::Function {
                index,
                captures: captures.into_iter().map(Into::into).collect(),
            },
            quiver_core::value::Value::Builtin(name) => Value::Builtin { name },
            quiver_core::value::Value::Process(pid, function_index) => Value::Process {
                pid,
                function_index,
            },
        }
    }
}

impl From<Value> for quiver_core::value::Value {
    fn from(value: Value) -> Self {
        match value {
            Value::Integer { value } => quiver_core::value::Value::Integer(value),
            Value::BinaryConstant { index } => {
                quiver_core::value::Value::Binary(quiver_core::value::Binary::Constant(index))
            }
            Value::BinaryHeap { index } => {
                quiver_core::value::Value::Binary(quiver_core::value::Binary::Heap(index))
            }
            Value::Tuple { type_id, values } => quiver_core::value::Value::Tuple(
                quiver_core::bytecode::TypeId(type_id),
                values.into_iter().map(Into::into).collect(),
            ),
            Value::Function { index, captures } => quiver_core::value::Value::Function(
                index,
                captures.into_iter().map(Into::into).collect(),
            ),
            Value::Builtin { name } => quiver_core::value::Value::Builtin(name),
            Value::Process {
                pid,
                function_index,
            } => quiver_core::value::Value::Process(pid, function_index),
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
