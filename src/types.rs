use std::collections::HashMap;
use serde::{Deserialize, Serialize};

use crate::bytecode::TypeId;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Type {
    Integer,
    Binary,
    Tuple(TypeId),
    Function(Box<Type>, Box<Type>),
}

#[derive(Debug, Clone)]
pub struct TypeRegistry {
    types: HashMap<TypeId, (Option<String>, Vec<(Option<String>, Type)>)>,
    next_id: usize,
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

    pub fn lookup_type(
        &self,
        type_id: &TypeId,
    ) -> Option<&(Option<String>, Vec<(Option<String>, Type)>)> {
        self.types.get(type_id)
    }

    pub fn get_types(&self) -> &HashMap<TypeId, (Option<String>, Vec<(Option<String>, Type)>)> {
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
}
