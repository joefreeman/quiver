#[derive(Debug, PartialEq)]
pub enum Constant {
    Integer(i64),
    Binary(Vec<u8>),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    // TODO: type_id?
    pub captures: Vec<String>,
    pub instructions: Vec<Instruction>,
}

pub struct Bytecode {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub entry: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeId(pub usize);

impl TypeId {
    pub const NIL: TypeId = TypeId(0);
    pub const OK: TypeId = TypeId(1);
}

#[derive(Debug, Clone, PartialEq)]
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
    Is(TypeId),
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
}
