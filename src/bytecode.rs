use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Constant {
    Integer(i64),
    Binary(Vec<u8>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Function {
    // TODO: type_id?
    pub captures: Vec<String>,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Bytecode {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub entry: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeId(pub usize);

impl TypeId {
    pub const NIL: TypeId = TypeId(0);
    pub const OK: TypeId = TypeId(1);
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
    Parameter, // TODO: remove? (get from stack..?)
    Function(usize),
    Enter,
    Exit,
}
