use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Error {
    // Stack operation errors
    StackUnderflow,

    // Function and call errors
    CallInvalid,
    FunctionUndefined(usize),
    BuiltinUndefined(usize),
    FrameUnderflow,

    // Variable and constant access errors
    VariableUndefined(String),
    ConstantUndefined(usize),

    // Data access errors
    FieldAccessInvalid(usize),

    // Type system errors
    TypeMismatch { expected: String, found: String },
    ArityMismatch { expected: usize, found: usize },
    InvalidArgument(String),

    // Tuple and structure errors
    TupleEmpty,

    // Operation restrictions
    OperationNotAllowed { operation: String, context: String },

    // An explicit, unrecoverable abort (`__panic__`) — e.g. a debug-mode `:pre`/`:post`
    // contract whose verdict was nil, or an `assert`/`unreachable` helper.
    Panic(String),

    // Terminated from outside: containment teardown of a terminated parent's subtree
    // (and, later, an explicit `%proc.kill` or link propagation). Reified for awaiters
    // as the `Killed` crash kind (see docs/process-state.md).
    Killed,

    // Scope management errors
    ScopeCountInvalid { expected: usize, found: usize },
    ScopeUnderflow,
}

impl Error {
    /// A human-readable message for crash delivery (the `message` field of a `'crash`
    /// value — see docs/process-state.md). Internal-invariant errors (stack/scope/table
    /// misuse — compiler bugs, not user-reachable) all read as internal errors.
    pub fn crash_message(&self) -> String {
        match self {
            Error::Panic(message) => message.clone(),
            Error::Killed => "killed".to_string(),
            Error::TypeMismatch { expected, found } => {
                format!("type mismatch: expected {expected}, found {found}")
            }
            Error::ArityMismatch { expected, found } => {
                format!("arity mismatch: expected {expected}, found {found}")
            }
            Error::InvalidArgument(message) => message.clone(),
            Error::OperationNotAllowed { operation, context } => {
                format!("{operation} is not allowed in {context}")
            }
            Error::VariableUndefined(name) => format!("undefined variable: {name}"),
            other => format!("internal error: {other:?}"),
        }
    }
}
