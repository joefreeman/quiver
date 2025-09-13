use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use crate::bytecode::TypeId;

/// Type alias for tuple field information: (optional name, field type)
pub type TupleField = (Option<String>, Type);

/// Type alias for tuple type information: (optional tuple name, field definitions)
pub type TupleTypeInfo = (Option<String>, Vec<TupleField>);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    pub parameter: Vec<Type>,
    pub result: Vec<Type>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
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

impl Type {
    /// Check if a concrete type is compatible with a type that may contain cycles.
    /// This uses coinductive reasoning to handle recursive types.
    pub fn is_compatible_with<F>(
        &self,
        pattern: &Type,
        lookup_type: &F,
        assumptions: &mut HashSet<(Type, Type)>,
    ) -> bool
    where
        F: Fn(&TypeId) -> Option<TupleTypeInfo>,
    {
        // Check if we've already assumed these types are compatible (coinductive hypothesis)
        let key = (self.clone(), pattern.clone());
        if assumptions.contains(&key) {
            return true;
        }

        // Add assumption for coinductive reasoning
        assumptions.insert(key);

        match (self, pattern) {
            // Basic types must match exactly
            (Type::Integer, Type::Integer) => true,
            (Type::Binary, Type::Binary) => true,

            // For tuples, check structural compatibility
            (Type::Tuple(id1), Type::Tuple(id2)) => {
                if id1 == id2 {
                    return true;
                }

                // Look up both tuple types
                let tuple1 = lookup_type(id1);
                let tuple2 = lookup_type(id2);

                match (tuple1, tuple2) {
                    (Some((name1, fields1)), Some((name2, fields2))) => {
                        // Names must match (or both be None)
                        if name1 != name2 {
                            return false;
                        }

                        // Same number of fields
                        if fields1.len() != fields2.len() {
                            return false;
                        }

                        // Check each field recursively
                        fields1.iter().zip(fields2.iter()).all(
                            |((fname1, ftype1), (fname2, ftype2))| {
                                fname1 == fname2
                                    && ftype1.is_compatible_with(ftype2, lookup_type, assumptions)
                            },
                        )
                    }
                    _ => false,
                }
            }

            // A concrete tuple can match a Cycle
            // The VM will handle structural checking at runtime
            (Type::Tuple(_), Type::Cycle(_)) => true,

            // Function types
            (Type::Function(f1), Type::Function(f2)) => {
                // For functions, parameters are contravariant and results are covariant
                // But for simplicity, we'll just check exact match for now
                f1 == f2
            }

            _ => false,
        }
    }
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

        let ok_type_id = registry.register_type(Some("Ok".to_string()), vec![]);
        assert_eq!(ok_type_id, TypeId::OK);

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

    // Load types with specific IDs (for bytecode loading)
    pub fn load_types(&mut self, types: HashMap<TypeId, TupleTypeInfo>) {
        // Update next_id to be higher than all loaded IDs
        self.next_id = types.keys().map(|id| id.0).max().unwrap_or(0) + 1;
        self.types = types;
    }
}
