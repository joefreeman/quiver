use serde::{Deserialize, Serialize};
use std::collections::HashSet;

use crate::bytecode::TypeId;

/// Trait for types that can look up type information
pub trait TypeLookup {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo>;
}

/// Type alias for tuple field information: (optional name, field type)
pub type TupleField = (Option<String>, Type);

/// Type alias for tuple type information: (optional tuple name, field definitions)
pub type TupleTypeInfo = (Option<String>, Vec<TupleField>);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub struct CallableType {
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
    Callable(Box<CallableType>),
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

    /// Simple compatibility check using a type lookup provider
    pub fn is_compatible<T: TypeLookup>(&self, pattern: &Type, type_lookup: &T) -> bool {
        let mut assumptions = HashSet::new();
        let mut type_stack = Vec::new();
        let result =
            self.is_compatible_with_impl(pattern, type_lookup, &mut assumptions, &mut type_stack);
        result
    }

    /// Internal implementation of type compatibility checking.
    /// This uses coinductive reasoning to handle recursive types.
    fn is_compatible_with_impl<T: TypeLookup>(
        &self,
        pattern: &Type,
        type_lookup: &T,
        assumptions: &mut HashSet<(Type, Type)>,
        type_stack: &mut Vec<Type>,
    ) -> bool {
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
                // Fast path: same ID means definitely compatible
                if id1 == id2 {
                    return true;
                }

                // Look up both tuple types
                let Some((name1, fields1)) = type_lookup.lookup_type(id1) else {
                    return false;
                };
                let Some((name2, fields2)) = type_lookup.lookup_type(id2) else {
                    return false;
                };

                // Names must match and same number of fields required
                name1 == name2
                    && fields1.len() == fields2.len()
                    && fields1.iter().zip(fields2.iter()).all(
                        |((fname1, ftype1), (fname2, ftype2))| {
                            fname1 == fname2
                                && ftype1.is_compatible_with_impl(
                                    ftype2,
                                    type_lookup,
                                    assumptions,
                                    type_stack,
                                )
                        },
                    )
            }

            // When both are cycles with same depth, they refer to the same recursive type
            (Type::Cycle(d1), Type::Cycle(d2)) if d1 == d2 => true,

            // Handle cycles in self by looking up the type in the stack
            (Type::Cycle(depth), _) => {
                // Check if stack has enough context
                if type_stack.len() < *depth {
                    return true; // Use coinductive reasoning
                }

                let lookup_index = type_stack.len() - *depth;
                if let Some(Type::Union(variants)) = type_stack.get(lookup_index) {
                    // Resolve the cycle to the union and check compatibility
                    let variants = variants.clone();
                    variants.iter().any(|variant| {
                        variant.is_compatible_with_impl(
                            pattern,
                            type_lookup,
                            assumptions,
                            type_stack,
                        )
                    })
                } else {
                    // Cycles should only refer to unions, but use coinductive reasoning as fallback
                    true
                }
            }

            // Handle cycles in pattern by looking up the type in the stack
            (_, Type::Cycle(depth)) => {
                // Cycle(n) refers to the pattern type n positions from the end of the stack
                // Cycles always refer to union types (enforced during type resolution)

                // Check if stack has enough context
                if type_stack.len() < *depth {
                    // Stack doesn't have enough context - this happens when comparing
                    // a concrete type against a variant containing a Cycle outside
                    // the context of the enclosing Union. Use coinductive reasoning:
                    // assume compatibility and let the assumptions set prevent infinite loops.
                    return true;
                }

                let lookup_index = type_stack.len() - *depth;
                if let Some(Type::Union(variants)) = type_stack.get(lookup_index) {
                    // Check if self matches any variant WITHOUT pushing the union
                    // onto the stack again (it's already there!)
                    let variants = variants.clone();
                    variants.iter().any(|variant| {
                        self.is_compatible_with_impl(variant, type_lookup, assumptions, type_stack)
                    })
                } else {
                    // This shouldn't happen - cycles should only refer to unions
                    // But use coinductive reasoning as a fallback
                    true
                }
            }

            // Function types
            (Type::Callable(f1), Type::Callable(f2)) => {
                // For functions, parameters are contravariant and results are covariant
                // But for simplicity, we'll just check exact match for now
                f1 == f2
            }

            // When self is concrete and pattern is a union, check if self matches any variant
            // Push the union pattern onto stack since cycles may reference it
            (concrete_type, Type::Union(variants)) => {
                type_stack.push(pattern.clone());
                let result = variants.iter().any(|variant| {
                    concrete_type.is_compatible_with_impl(
                        variant,
                        type_lookup,
                        assumptions,
                        type_stack,
                    )
                });
                type_stack.pop();
                result
            }

            // When self is a union and pattern is concrete, check if any variant matches pattern
            (Type::Union(variants), pattern_type) => variants.iter().any(|variant| {
                variant.is_compatible_with_impl(pattern_type, type_lookup, assumptions, type_stack)
            }),

            _ => false,
        }
    }
}
