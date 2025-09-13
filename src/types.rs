use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
}

#[derive(Debug, Clone)]
pub struct TypeRegistry {
    types: HashMap<TypeId, TupleTypeInfo>,
    next_id: usize,
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            types: HashMap::new(),
            next_id: 0,
        };

        let nil_type_id = registry.register_type(None, vec![]);
        assert_eq!(nil_type_id, TypeId::NIL);

        let nil_type_id = registry.register_type(Some("Ok".to_string()), vec![]);
        assert_eq!(nil_type_id, TypeId::OK);

        registry
    }

    pub fn register_type(
        &mut self,
        name: Option<String>,
        fields: Vec<(Option<String>, Type)>,
    ) -> TypeId {
        for (&existing_id, existing_type) in &self.types {
            if existing_type.0 == name && existing_type.1 == fields {
                return existing_id;
            }
        }

        let type_id = TypeId(self.next_id);
        self.next_id += 1;

        self.types.insert(type_id, (name, fields));
        type_id
    }

    pub fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.types.get(type_id)
    }

    pub fn get_types(&self) -> &HashMap<TypeId, TupleTypeInfo> {
        &self.types
    }

    pub fn find_type(
        &self,
        name: Option<String>,
        fields: &[(Option<String>, Type)],
    ) -> Option<TypeId> {
        for (&existing_id, existing_type) in &self.types {
            if existing_type.0 == name && existing_type.1 == fields {
                return Some(existing_id);
            }
        }
        None
    }

    pub fn get_tuple_name(&self, type_id: &TypeId) -> String {
        if let Some((name, _)) = self.lookup_type(type_id) {
            let default_name = format!("Type{}", type_id.0);
            name.as_deref().unwrap_or(&default_name).to_string()
        } else {
            format!("Type{}", type_id.0)
        }
    }
}
