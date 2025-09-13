use serde::{Deserialize, Serialize};

use crate::bytecode::TypeId;

/// Type alias for tuple field information: (optional name, field type)
pub type TupleField = (Option<String>, Type);

/// Type alias for tuple type information: (optional tuple name, field definitions)
pub type TupleTypeInfo = (Option<String>, Vec<TupleField>);

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    pub parameter: Vec<Type>,
    pub result: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Type {
    #[serde(rename = "int")]
    Integer,
    #[serde(rename = "bin")]
    Binary,
    #[serde(rename = "tuple")]
    Tuple(TypeId),
    #[serde(rename = "fn")]
    Function(Box<FunctionType>),
    #[serde(rename = "cycle")]
    Cycle(usize),
}
