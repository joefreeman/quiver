use crate::effects::WebEffect;
use quiver_core::builtins::{bigint_from_i64, bigint_from_str};
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
        // Arbitrary-precision: encoded as a decimal string so values beyond JS's safe
        // integer range (or i64) survive the bridge intact (`value: string` in TypeScript).
        value: String,
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
    Ref {
        value: String, // Hex-encoded ref value for JSON serialization
    },
}

impl Value {
    /// Convert web value to core value for formatting purposes
    /// Extracts hex-encoded binaries and adds them to the heap
    /// Returns (core_value, extended_heap)
    /// Convert web value to core value for formatting purposes. The heap pair the JS bridge
    /// speaks is vestigial — a binary carries its own bytes — so the returned heap is empty.
    pub fn to_core_for_formatting(
        &self,
        _heap: &[Vec<u8>],
    ) -> (quiver_core::value::Value, Vec<Vec<u8>>) {
        (self.to_core_recursive(), Vec::new())
    }

    fn to_core_recursive(&self) -> quiver_core::value::Value {
        match self {
            Value::Integer { value } => quiver_core::value::Value::integer(
                // The string is produced by `from_core_value` (always a valid decimal); fall
                // back to 0 only if a malformed value somehow reaches this formatting path.
                bigint_from_str(value).unwrap_or_else(|_| bigint_from_i64(0)),
            ),
            Value::Binary { hex } => {
                // A binary owns its bytes, so the value carries them directly.
                let bytes = hex::decode(hex).unwrap_or_default();
                quiver_core::value::Value::Binary(Binary::Data(std::rc::Rc::new(
                    quiver_core::binary::BinaryData::new(bytes),
                )))
            }
            Value::Tuple { type_id, values } => quiver_core::value::Value::tuple(
                *type_id,
                values.iter().map(|v| v.to_core_recursive()).collect(),
            ),
            Value::Function { index, captures } => quiver_core::value::Value::function(
                *index,
                captures.iter().map(|v| v.to_core_recursive()).collect(),
            ),
            Value::Builtin { name: _ } => {
                // Web Value uses name, but core Value uses builtin_id
                // Use 0 as placeholder - this is only for formatting purposes
                quiver_core::value::Value::builtin(0)
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
            Value::Ref { value } => {
                // Decode hex ref value
                let ref_value = u64::from_str_radix(value, 16).unwrap_or(0);
                quiver_core::value::Value::Reference(ref_value)
            }
        }
    }

    /// Convert from core value to web value
    /// Requires heap data and program to resolve binary references
    pub fn from_core_value(value: &quiver_core::value::Value, program: &Program) -> Self {
        match value {
            quiver_core::value::Value::Int(n) => Value::Integer {
                value: n.to_string(),
            },
            quiver_core::value::Value::BigInt(n) => Value::Integer {
                // Decimal string keeps arbitrary-precision integers exact across the bridge,
                // instead of the old `i64` field that coerced out-of-range values to 0.
                value: n.to_string(),
            },
            quiver_core::value::Value::Binary(binary) => {
                // Resolve binary reference to actual bytes
                let bytes: Vec<u8> = match binary {
                    Binary::Constant(idx) => program
                        .get_constant(*idx)
                        .and_then(|c| match c {
                            Constant::Binary(b) => Some(b.clone()),
                            _ => None,
                        })
                        .unwrap_or_default(),
                    Binary::Data(data) => data.to_vec(),
                };

                Value::Binary {
                    hex: hex::encode(bytes),
                }
            }
            quiver_core::value::Value::Tuple(type_id, values) => Value::Tuple {
                type_id: *type_id,
                values: values
                    .iter()
                    .map(|v| Value::from_core_value(v, program))
                    .collect(),
            },
            quiver_core::value::Value::Function(index, captures) => Value::Function {
                index: *index,
                captures: captures
                    .iter()
                    .map(|v| Value::from_core_value(v, program))
                    .collect(),
            },
            quiver_core::value::Value::Builtin(builtin_id, _) => {
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
            quiver_core::value::Value::Reference(r) => Value::Ref {
                value: format!("{:x}", r),
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
            Value::Integer { value } => Ok(quiver_core::value::Value::integer(
                bigint_from_str(&value).map_err(|e| format!("{:?}", e))?,
            )),
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
                Ok(quiver_core::value::Value::tuple(type_id, core_values?))
            }
            Value::Function { index, captures } => {
                let core_captures: std::result::Result<Vec<_>, _> = captures
                    .into_iter()
                    .map(|v| v.to_core_value(executor))
                    .collect();
                Ok(quiver_core::value::Value::function(index, core_captures?))
            }
            Value::Builtin { name: _ } => {
                // Web Value stores name, but core Value needs builtin_id
                // We can't look up the ID without program context, use 0 as placeholder
                Ok(quiver_core::value::Value::builtin(0))
            }
            Value::Process {
                pid,
                function_index,
            } => Ok(quiver_core::value::Value::Process(pid, function_index)),
            Value::Resource { id, process_id: _ } => {
                // Resource type_id is for type checking only, use 0 as placeholder
                Ok(quiver_core::value::Value::Resource(id, 0))
            }
            Value::Ref { value } => {
                // Decode hex ref value
                let ref_value = u64::from_str_radix(&value, 16)
                    .map_err(|e| format!("Invalid ref hex: {}", e))?;
                Ok(quiver_core::value::Value::Reference(ref_value))
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

/// Distinct binary buffers (and their total bytes) reachable from some root.
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct HeapUsage {
    pub binaries: usize,
    pub bytes: usize,
}

impl From<quiver_core::process::HeapUsage> for HeapUsage {
    fn from(usage: quiver_core::process::HeapUsage) -> Self {
        Self {
            binaries: usage.binaries,
            bytes: usage.bytes,
        }
    }
}

/// A process's binary-heap footprint, broken down by root. `total` is distinct across all roots, so
/// the per-root figures may overlap and need not sum to it (see the core `ProcessHeapUsage`).
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct ProcessHeapUsage {
    pub stack: HeapUsage,
    pub locals: HeapUsage,
    pub mailbox: HeapUsage,
    pub total: HeapUsage,
}

impl From<quiver_core::process::ProcessHeapUsage> for ProcessHeapUsage {
    fn from(usage: quiver_core::process::ProcessHeapUsage) -> Self {
        Self {
            stack: usage.stack.into(),
            locals: usage.locals.into(),
            mailbox: usage.mailbox.into(),
            total: usage.total.into(),
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
    pub heap: ProcessHeapUsage,
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct Process {
    pub id: usize,
    pub status: ProcessStatus,
}

/// A worker's executor snapshot (heap/memory stats and owned process ids), for the Workers view.
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct WorkerInfo {
    pub worker_id: usize,
    pub process_ids: Vec<usize>,
    pub live_binaries: usize,
    pub live_bytes: usize,
    pub rope_binaries: usize,
    pub max_rope_depth: usize,
    pub shared_bytes: usize,
    pub constant_binaries: usize,
    pub constant_bytes: usize,
}

impl From<quiver_core::process::WorkerInfo> for WorkerInfo {
    fn from(w: quiver_core::process::WorkerInfo) -> Self {
        Self {
            worker_id: w.worker_id as usize,
            process_ids: w.process_ids,
            live_binaries: w.live_binaries,
            live_bytes: w.live_bytes,
            rope_binaries: w.rope_binaries,
            max_rope_depth: w.max_rope_depth,
            shared_bytes: w.shared_bytes,
            constant_binaries: w.constant_binaries,
            constant_bytes: w.constant_bytes,
        }
    }
}

/// Evaluation result with value and heap
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct EvaluationResult {
    pub value: Value,
    pub heap: Vec<Vec<u8>>,
    /// The *static* type the compiler inferred, not one derived from the value. The two
    /// differ exactly where it matters: a call that can fail is `T | []` however the one
    /// value in hand happens to have turned out, and showing the narrower derived type is
    /// what makes the next line's `r.status` a surprise. `None` for a result with no
    /// compiled expression behind it (a process's own result, say).
    #[serde(rename = "type")]
    pub result_type: Option<String>,
    /// A nil result's failure provenance ("match failed at repl:1:3"), read from the
    /// debug-build origin stamp the nil carries. `None` for a non-nil result, an
    /// unstamped nil (nil used as data), or a release-mode compile.
    pub origin: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_round_trips_beyond_i64() {
        // i64::MAX * 1000 — far outside i64 range, which the old `i64` bridge coerced to 0.
        let s = "9223372036854775807000";
        let core = quiver_core::value::Value::integer(bigint_from_str(s).unwrap());

        // core -> web: preserved exactly as a decimal string (not 0).
        let program = Program::new();
        let web = Value::from_core_value(&core, &program);
        let Value::Integer { value } = &web else {
            panic!("expected an Integer web value");
        };
        assert_eq!(value, s);

        // web -> core: parses back to the same arbitrary-precision integer.
        let quiver_core::value::Value::BigInt(back) = web.to_core_recursive() else {
            panic!("expected a big-integer core value");
        };
        assert_eq!(back.to_string(), s);
    }
}
