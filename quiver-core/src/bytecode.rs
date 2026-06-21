use crate::types::{BuiltinInfo, TupleTypeInfo, Type, TypeLookup};
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Constant {
    #[serde(rename = "int")]
    Integer(BigInt),
    #[serde(rename = "bin")]
    Binary(Vec<u8>),
}

/// A concrete type that uniquely identifies a runtime value's type.
/// This is used for O(1) type compatibility checking at runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ConcreteType {
    Integer,
    Binary,
    Reference,
    Tuple(usize),    // tuple_id
    Function(usize), // func_id
    Builtin(usize),  // builtin_id
    Process(usize),  // func_id (spawning function)
    Resource(usize), // resource_type_id
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub captures: usize,
    /// Type ID referencing this function's callable type in the types vec
    pub type_id: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bytecode {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    /// Builtin information with type_ids
    pub builtins: Vec<BuiltinInfo>,
    pub entry: Option<usize>,
    /// Tuple type information with type_ids for fields
    pub tuples: Vec<TupleTypeInfo>,
    /// Types (referenced by type_ids throughout bytecode)
    pub types: Vec<Type>,
    /// Resource type names (index is resource_id, used for effect dispatch)
    pub resources: Vec<String>,
}

impl TypeLookup for Bytecode {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        self.types.get(type_id)
    }

    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo> {
        self.tuples.get(tuple_id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Constant(usize),
    Pop,
    Duplicate,
    Pick(usize),
    Rotate(usize),
    Reset(usize),
    Load(usize),
    Store,
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
    Select,
    Reference,
    Process(usize, usize), // (process_id, function_index)
}
