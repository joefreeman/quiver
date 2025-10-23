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
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub function_type: Option<types::CallableType>,
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
    pub types: Vec<types::TupleTypeInfo>,
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
                    instructions: f.instructions.clone(),
                    function_type: None,
                    captures: f.captures.clone(),
                })
                .collect(),
            builtins: self.builtins.clone(),
            entry: self.entry,
            types: self.types.clone(),
        }
    }
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
    Tuple(TypeId),
    Get(usize),
    IsInteger,
    IsBinary,
    IsTuple(TypeId),
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
}
