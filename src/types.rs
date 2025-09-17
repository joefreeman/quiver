use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use crate::bytecode::TypeId;

/// Type alias for tuple field information: (optional name, field type)
pub type TupleField = (Option<String>, Type);

/// Type alias for tuple type information: (optional tuple name, field definitions)
pub type TupleTypeInfo = (Option<String>, Vec<TupleField>);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    pub parameter: Type,
    pub result: Type,
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
    #[serde(rename = "union")]
    Union(Vec<Type>),
}

impl Type {
    /// Create a NIL tuple type
    pub fn nil() -> Self {
        Type::Tuple(TypeId::NIL)
    }

    /// Create an OK tuple type
    pub fn ok() -> Self {
        Type::Tuple(TypeId::OK)
    }

    /// Check if this type is concrete (not a union or cycle)
    pub fn is_concrete(&self) -> bool {
        !matches!(self, Type::Union(_) | Type::Cycle(_))
    }

    /// Get the concrete type if this is not a union
    pub fn as_concrete(&self) -> Option<&Type> {
        match self {
            Type::Union(types) if types.len() == 1 => types.first(),
            Type::Union(_) => None,
            t => Some(t),
        }
    }

    /// Create a Type from a vector of types
    /// Returns a single type if the vector has one element, otherwise a Union
    pub fn from_types(mut types: Vec<Type>) -> Type {
        // Remove duplicates
        types.sort_by_key(|t| format!("{:?}", t));
        types.dedup();

        if types.is_empty() {
            // This shouldn't happen in normal operation, but return NIL as a safe default
            Type::nil()
        } else if types.len() == 1 {
            types.into_iter().next().unwrap()
        } else {
            Type::Union(types)
        }
    }

    /// Extract all tuple TypeIds from this Type, filtering out non-tuple types
    pub fn extract_tuple_types(&self) -> Vec<TypeId> {
        match self {
            Type::Union(types) => types
                .iter()
                .filter_map(|t| match t {
                    Type::Tuple(id) => Some(*id),
                    _ => None,
                })
                .collect(),
            Type::Tuple(id) => vec![*id],
            _ => vec![],
        }
    }

    /// Simple compatibility check with a type registry
    pub fn is_compatible(&self, pattern: &Type, registry: &TypeRegistry) -> bool {
        let mut assumptions = HashSet::new();
        let mut type_stack = Vec::new();
        self.is_compatible_with_impl(pattern, registry, &mut assumptions, &mut type_stack)
    }

    /// Internal implementation of type compatibility checking.
    /// This uses coinductive reasoning to handle recursive types.
    fn is_compatible_with_impl(
        &self,
        pattern: &Type,
        registry: &TypeRegistry,
        assumptions: &mut HashSet<(Type, Type)>,
        type_stack: &mut Vec<(Type, Type)>,
    ) -> bool {
        // Check if we've already assumed these types are compatible (coinductive hypothesis)
        let key = (self.clone(), pattern.clone());
        if assumptions.contains(&key) {
            return true;
        }

        // Add assumption for coinductive reasoning
        assumptions.insert(key);

        // Push current types onto stack for cycle resolution
        type_stack.push((self.clone(), pattern.clone()));

        let result = match (self, pattern) {
            // Basic types must match exactly
            (Type::Integer, Type::Integer) => true,
            (Type::Binary, Type::Binary) => true,

            // For tuples, check structural compatibility
            (Type::Tuple(id1), Type::Tuple(id2)) => {
                if id1 == id2 {
                    return true;
                }

                // Look up both tuple types
                let tuple1 = registry.lookup_type(id1);
                let tuple2 = registry.lookup_type(id2);

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
                                    && ftype1.is_compatible_with_impl(
                                        ftype2,
                                        registry,
                                        assumptions,
                                        type_stack,
                                    )
                            },
                        )
                    }
                    _ => false,
                }
            }

            // Handle cycles by looking up the type in the stack
            (_, Type::Cycle(depth)) => {
                // Cycle(n) refers to the pattern type n positions from the end of the stack
                if let Some((_, pattern_type)) =
                    type_stack.get(type_stack.len().saturating_sub(*depth))
                {
                    let pattern_type = pattern_type.clone();
                    let mut new_assumptions = assumptions.clone();
                    self.is_compatible_with_impl(
                        &pattern_type,
                        registry,
                        &mut new_assumptions,
                        type_stack,
                    )
                } else {
                    // Invalid cycle depth - shouldn't happen with well-formed types
                    false
                }
            }

            // Function types
            (Type::Function(f1), Type::Function(f2)) => {
                // For functions, parameters are contravariant and results are covariant
                // But for simplicity, we'll just check exact match for now
                f1 == f2
            }

            // When self is concrete and pattern is a union, check if self matches any variant
            (concrete_type, Type::Union(variants)) => variants.iter().any(|variant| {
                let mut new_assumptions = assumptions.clone();
                if concrete_type == variant {
                    true
                } else {
                    concrete_type.is_compatible_with_impl(
                        variant,
                        registry,
                        &mut new_assumptions,
                        type_stack,
                    )
                }
            }),

            // When self is a union and pattern is concrete, check if any variant matches pattern
            (Type::Union(variants), pattern_type) => variants.iter().any(|variant| {
                let mut new_assumptions = assumptions.clone();
                if variant == pattern_type {
                    true
                } else {
                    variant.is_compatible_with_impl(
                        pattern_type,
                        registry,
                        &mut new_assumptions,
                        type_stack,
                    )
                }
            }),

            _ => false,
        };

        type_stack.pop();
        result
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
