use serde::{Deserialize, Serialize};
use std::collections::HashSet;

use crate::bytecode::TypeId;

/// Trait for looking up type information.
/// This trait exists in types.rs to break circular dependencies - it allows
/// Type methods to look up type info without depending on Program.
pub trait TypeLookup {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo>;
}

/// Type alias for tuple field information: (optional name, field type)
pub type TupleField = (Option<String>, Type);

/// Tuple type information: name, field definitions, and whether it's a partial type
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TupleTypeInfo {
    pub name: Option<String>,
    pub fields: Vec<TupleField>,
    pub is_partial: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub struct CallableType {
    pub parameter: Type,
    pub result: Type,
    pub receive: Type,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub struct ProcessType {
    pub receive: Option<Box<Type>>,
    pub returns: Option<Box<Type>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub enum Type {
    #[serde(rename = "int")]
    Integer,
    #[serde(rename = "bin")]
    Binary,
    #[serde(rename = "tuple")]
    Tuple(TypeId),
    #[serde(rename = "partial")]
    Partial(TypeId),
    #[serde(rename = "fn")]
    Callable(Box<CallableType>),
    #[serde(rename = "cycle")]
    Cycle(usize),
    #[serde(rename = "union")]
    Union(Vec<Type>),
    #[serde(rename = "process")]
    Process(Box<ProcessType>),
    #[serde(rename = "var")]
    Variable(String),
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

    /// Create a never type (empty union - bottom type)
    pub fn never() -> Self {
        Type::Union(vec![])
    }

    /// Check if this type is NIL
    pub fn is_nil(&self) -> bool {
        matches!(self, Type::Tuple(TypeId::NIL))
    }

    /// Check if this type is OK
    pub fn is_ok(&self) -> bool {
        matches!(self, Type::Tuple(TypeId::OK))
    }

    /// Create a Type from a vector of types
    /// Returns a single type if the vector has one element, otherwise a Union
    pub fn from_types(mut types: Vec<Type>) -> Type {
        // Remove duplicates
        types.sort_by_key(|t| format!("{:?}", t));
        types.dedup();

        if types.len() == 1 {
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
                    Type::Tuple(id) | Type::Partial(id) => Some(*id),
                    _ => None,
                })
                .collect(),
            Type::Tuple(id) | Type::Partial(id) => vec![*id],
            _ => vec![],
        }
    }

    /// Simple compatibility check using a type lookup provider
    pub fn is_compatible<T: TypeLookup>(&self, pattern: &Type, type_lookup: &T) -> bool {
        let mut assumptions = HashSet::new();
        let mut type_stack = Vec::new();
        self.is_compatible_with_impl(pattern, type_lookup, &mut assumptions, &mut type_stack)
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
            // Empty union (never type) is bottom type - compatible with anything
            (Type::Union(variants), _) if variants.is_empty() => true,

            // Basic types must match exactly
            (Type::Integer, Type::Integer) => true,
            (Type::Binary, Type::Binary) => true,

            // Process types are compatible if their receive and return types are compatible
            (Type::Process(proc1), Type::Process(proc2)) => {
                // Check receive type compatibility
                let receive_compatible = match (&proc1.receive, &proc2.receive) {
                    (Some(r1), Some(r2)) => {
                        r1.is_compatible_with_impl(r2, type_lookup, assumptions, type_stack)
                    }
                    (None, _) | (_, None) => true, // Compatible if either has no receive type
                };

                // Check return type compatibility
                let return_compatible = match (&proc1.returns, &proc2.returns) {
                    (Some(ret1), Some(ret2)) => {
                        ret1.is_compatible_with_impl(ret2, type_lookup, assumptions, type_stack)
                    }
                    (None, _) | (_, None) => true, // Compatible if either has no return type
                };

                receive_compatible && return_compatible
            }

            // For tuples, check structural compatibility
            (Type::Tuple(id1), Type::Tuple(id2)) => {
                // Fast path: same ID means definitely compatible
                if id1 == id2 {
                    return true;
                }

                // Look up both tuple types
                let Some(info1) = type_lookup.lookup_type(id1) else {
                    return false;
                };
                let Some(info2) = type_lookup.lookup_type(id2) else {
                    return false;
                };

                // Names must match and same number of fields required
                info1.name == info2.name
                    && info1.fields.len() == info2.fields.len()
                    && info1.fields.iter().zip(info2.fields.iter()).all(
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

            (Type::Callable(f1), Type::Callable(f2)) => {
                // For functions:
                // - Parameters are contravariant (pattern's param must be compatible with self's param)
                // - Results are covariant (self's result must be compatible with pattern's result)
                // - Receive types are contravariant (pattern's receive must be compatible with self's receive)
                f2.parameter.is_compatible_with_impl(
                    &f1.parameter,
                    type_lookup,
                    assumptions,
                    type_stack,
                ) && f1.result.is_compatible_with_impl(
                    &f2.result,
                    type_lookup,
                    assumptions,
                    type_stack,
                ) && f2.receive.is_compatible_with_impl(
                    &f1.receive,
                    type_lookup,
                    assumptions,
                    type_stack,
                )
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

            // Concrete tuple vs partial type: check if concrete satisfies partial constraint
            (Type::Tuple(concrete_id), Type::Partial(partial_id)) => {
                let Some(concrete_info) = type_lookup.lookup_type(concrete_id) else {
                    return false;
                };
                let Some(partial_info) = type_lookup.lookup_type(partial_id) else {
                    return false;
                };

                // If partial has a name, concrete must match it
                if let Some(partial_name) = &partial_info.name
                    && concrete_info.name.as_ref() != Some(partial_name)
                {
                    return false;
                }

                // Check that all partial fields exist in concrete with compatible types
                partial_info
                    .fields
                    .iter()
                    .all(|(partial_fname, partial_ftype)| {
                        // Partial types must have all fields named (enforced during type resolution)
                        let Some(partial_fname) = partial_fname else {
                            return false;
                        };

                        // Find matching field in concrete type
                        concrete_info
                            .fields
                            .iter()
                            .any(|(concrete_fname, concrete_ftype)| {
                                concrete_fname.as_ref() == Some(partial_fname)
                                    && concrete_ftype.is_compatible_with_impl(
                                        partial_ftype,
                                        type_lookup,
                                        assumptions,
                                        type_stack,
                                    )
                            })
                    })
            }

            // Type variables are compatible with anything (they're placeholders)
            (Type::Variable(_), _) | (_, Type::Variable(_)) => true,

            _ => false,
        }
    }
}
