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

    // Scope management errors
    ScopeCountInvalid { expected: usize, found: usize },
    ScopeUnderflow,
}
