use crate::types;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Constant {
    #[serde(rename = "int")]
    Integer(i64),
    #[serde(rename = "bin")]
    Binary(Vec<u8>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    #[serde(rename = "type")]
    pub function_type: types::CallableType,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub captures: Vec<usize>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct BuiltinInfo {
    pub name: String,
    pub parameter_type: types::Type,
    pub result_type: types::Type,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bytecode {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub builtins: Vec<BuiltinInfo>,
    pub entry: Option<usize>,
    pub tuples: Vec<types::TupleTypeInfo>,
    pub types: Vec<types::Type>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Constant(usize),
    Pop,
    Duplicate,
    Pick(usize),
    Rotate(usize),
    Allocate(usize),
    Clear(usize),
    Load(usize),
    Store(usize),
    Tuple(usize),
    Get(usize),
    IsType(usize),
    Jump(isize),
    JumpIf(isize),
    Call,
    TailCall(bool),
    Function(usize),
    Builtin(usize),
    Equal(usize),
    Not,
    Spawn,
    Send,
    Self_,
    Select(usize),
    Process(usize, usize), // (process_id, function_index)
}
