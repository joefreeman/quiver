use crate::types;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Constant {
    Integer(i64),
    Binary(Vec<u8>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct Function {
    pub captures: Vec<String>,
    pub instructions: Vec<Instruction>,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub function_type: Option<types::FunctionType>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Bytecode {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub entry: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub types: Option<HashMap<TypeId, (Option<String>, Vec<(Option<String>, types::Type)>)>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeId(pub usize);

impl TypeId {
    pub const NIL: TypeId = TypeId(0);
    pub const OK: TypeId = TypeId(1);
}

impl Bytecode {
    pub fn without_debug_info(&self) -> Self {
        Self {
            constants: self.constants.clone(),
            functions: self
                .functions
                .iter()
                .map(|f| Function {
                    captures: f.captures.clone(),
                    instructions: f.instructions.clone(),
                    function_type: None,
                })
                .collect(),
            entry: self.entry,
            types: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Constant(usize),
    Pop,
    Duplicate,
    Swap,
    Add(usize),
    Subtract(usize),
    Multiply(usize),
    Divide(usize),
    Modulo(usize),
    Equal(usize),
    NotEqual(usize),
    Less(usize),
    LessEqual(usize),
    Greater(usize),
    GreaterEqual(usize),
    Load(String),
    Store(String),
    Tuple(TypeId, usize),
    Get(usize),
    IsInteger,
    IsBinary,
    IsTuple(TypeId),
    Jump(isize),
    JumpIfNil(isize),
    JumpIfNotNil(isize),
    Call,
    TailCall(bool),
    Return,
    Parameter,
    Function(usize),
    Enter,
    Exit,
    Reset,
}
