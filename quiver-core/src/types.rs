use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Index of the NIL tuple type (always at index 0)
pub const NIL: usize = 0;

/// Index of the OK tuple type (always at index 1)
pub const OK: usize = 1;

/// Trait for looking up type information.
/// This trait exists in types.rs to break circular dependencies - it allows
/// Type methods to look up type info without depending on Program.
pub trait TypeLookup {
    fn lookup_type(&self, type_id: usize) -> Option<&Type>;
    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo>;
}

/// The unified type representation used throughout compiler, runtime, and bytecode.
/// All nested type references use IDs into a type registry.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub enum Type {
    #[serde(rename = "int")]
    Integer,
    #[serde(rename = "bin")]
    Binary,
    #[serde(rename = "tuple")]
    Tuple(usize),
    #[serde(rename = "partial")]
    Partial {
        name: Option<String>,
        fields: Vec<(String, usize)>, // (field_name, type_id) - all fields must be named
    },
    #[serde(rename = "fn")]
    Callable {
        parameter: usize,
        result: usize,
        receive: usize,
    },
    #[serde(rename = "cycle")]
    Cycle(usize),
    #[serde(rename = "union")]
    Union(Vec<usize>),
    #[serde(rename = "process")]
    Process {
        send: Option<usize>,
        receive: Option<usize>,
    },
    #[serde(rename = "resource")]
    Resource(String),
    #[serde(rename = "var")]
    Variable(String),
}

/// Type alias for tuple field information: (optional name, type_id)
pub type TupleField = (Option<String>, usize);

/// Tuple type information: name and field definitions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TupleTypeInfo {
    pub name: Option<String>,
    pub fields: Vec<TupleField>,
}

/// Builtin function information with type IDs
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BuiltinInfo {
    pub name: String,
    pub param_type: usize,
    pub result_type: usize,
}

impl Type {
    /// Create a NIL tuple type
    pub fn nil() -> Self {
        Type::Tuple(NIL)
    }

    /// Create an OK tuple type
    pub fn ok() -> Self {
        Type::Tuple(OK)
    }

    /// Create a never type (empty union - bottom type)
    pub fn never() -> Self {
        Type::Union(vec![])
    }

    /// Check if this is the never type (empty union)
    pub fn is_never(&self) -> bool {
        matches!(self, Type::Union(types) if types.is_empty())
    }

    /// Check if this type is NIL
    pub fn is_nil(&self) -> bool {
        matches!(self, Type::Tuple(id) if *id == NIL)
    }

    /// Check if this type is OK
    pub fn is_ok(&self) -> bool {
        matches!(self, Type::Tuple(id) if *id == OK)
    }

    /// Extract tuple IDs with type lookup
    pub fn extract_tuples_with_lookup<T: TypeLookup>(&self, lookup: &T) -> Vec<usize> {
        match self {
            Type::Union(type_ids) => type_ids
                .iter()
                .filter_map(|&type_id| {
                    lookup.lookup_type(type_id).and_then(|t| match t {
                        Type::Tuple(id) => Some(*id),
                        _ => None,
                    })
                })
                .collect(),
            Type::Tuple(id) => vec![*id],
            _ => vec![],
        }
    }

    /// Get variants for union types, or wrap single type
    /// Returns type IDs for unions, or a single-element vec with the type ID for non-unions
    pub fn variants<T: TypeLookup>(&self, self_id: usize, _lookup: &T) -> Vec<usize> {
        match self {
            Type::Union(type_ids) => type_ids.clone(),
            _ => vec![self_id],
        }
    }

    /// Check if this type contains NIL (either is NIL or contains NIL in union)
    pub fn contains_nil<T: TypeLookup>(&self, lookup: &T) -> bool {
        match self {
            Type::Tuple(id) if *id == NIL => true,
            Type::Union(type_ids) => type_ids
                .iter()
                .any(|&id| lookup.lookup_type(id).map(|t| t.is_nil()).unwrap_or(false)),
            _ => false,
        }
    }

    /// Return a type without NIL variants
    pub fn without_nil<T: TypeLookup>(&self, lookup: &T) -> Type {
        match self {
            Type::Tuple(id) if *id == NIL => Type::never(),
            Type::Union(type_ids) => {
                let filtered: Vec<usize> = type_ids
                    .iter()
                    .filter(|&&id| !lookup.lookup_type(id).map(|t| t.is_nil()).unwrap_or(false))
                    .copied()
                    .collect();
                if filtered.len() == 1 {
                    // Could unwrap to single type, but keep as union for now
                    Type::Union(filtered)
                } else {
                    Type::Union(filtered)
                }
            }
            _ => self.clone(),
        }
    }

    /// Check compatibility with lookup context
    pub fn is_compatible_with<T: TypeLookup>(
        &self,
        self_id: usize,
        pattern_id: usize,
        lookup: &T,
    ) -> bool {
        is_compatible(self_id, pattern_id, lookup)
    }
}

/// Create a Type from a list of type IDs (creates a Union or simplifies to single type)
pub fn from_type_ids(type_ids: Vec<usize>) -> Type {
    match type_ids.len() {
        0 => Type::never(),
        1 => {
            // For a single type, we still wrap in Union for consistency
            // The caller can unwrap if needed
            Type::Union(type_ids)
        }
        _ => Type::Union(type_ids),
    }
}

/// Check if two type IDs are compatible.
/// This is the main compatibility check used throughout the system.
pub fn is_compatible<T: TypeLookup>(self_id: usize, pattern_id: usize, lookup: &T) -> bool {
    let mut assumptions = HashSet::new();
    let mut type_stack = Vec::new();
    is_compatible_impl(
        self_id,
        pattern_id,
        lookup,
        &mut assumptions,
        &mut type_stack,
    )
}

/// Internal implementation of type compatibility checking.
/// This uses coinductive reasoning to handle recursive types.
fn is_compatible_impl<T: TypeLookup>(
    self_id: usize,
    pattern_id: usize,
    lookup: &T,
    assumptions: &mut std::collections::HashSet<(usize, usize)>,
    type_stack: &mut Vec<usize>,
) -> bool {
    // Fast path: same ID means definitely compatible
    if self_id == pattern_id {
        return true;
    }

    // Check if we've already assumed these types are compatible (coinductive hypothesis)
    let key = (self_id, pattern_id);
    if assumptions.contains(&key) {
        return true;
    }

    // Add assumption for coinductive reasoning
    assumptions.insert(key);

    let Some(self_type) = lookup.lookup_type(self_id) else {
        return false;
    };
    let Some(pattern_type) = lookup.lookup_type(pattern_id) else {
        return false;
    };

    match (self_type, pattern_type) {
        // Empty union (never type) is bottom type - compatible with anything
        (Type::Union(variants), _) if variants.is_empty() => true,

        // Basic types must match exactly
        (Type::Integer, Type::Integer) => true,
        (Type::Binary, Type::Binary) => true,

        // Process types are compatible if their send and receive types are compatible
        (
            Type::Process {
                send: send1,
                receive: receive1,
            },
            Type::Process {
                send: send2,
                receive: receive2,
            },
        ) => {
            let send_compatible = match (send1, send2) {
                (Some(s1), Some(s2)) => {
                    is_compatible_impl(*s1, *s2, lookup, assumptions, type_stack)
                }
                (None, _) | (_, None) => true,
            };

            let receive_compatible = match (receive1, receive2) {
                (Some(r1), Some(r2)) => {
                    is_compatible_impl(*r1, *r2, lookup, assumptions, type_stack)
                }
                (None, _) | (_, None) => true,
            };

            send_compatible && receive_compatible
        }

        // Resource types must have matching identifiers
        (Type::Resource(r1), Type::Resource(r2)) => r1 == r2,

        // For tuples, check structural compatibility
        (Type::Tuple(id1), Type::Tuple(id2)) => {
            if id1 == id2 {
                return true;
            }

            let Some(info1) = lookup.lookup_tuple(*id1) else {
                return false;
            };
            let Some(info2) = lookup.lookup_tuple(*id2) else {
                return false;
            };

            info1.name == info2.name
                && info1.fields.len() == info2.fields.len()
                && info1.fields.iter().zip(info2.fields.iter()).all(
                    |((fname1, ftype1), (fname2, ftype2))| {
                        fname1 == fname2
                            && is_compatible_impl(*ftype1, *ftype2, lookup, assumptions, type_stack)
                    },
                )
        }

        // When both are cycles with same depth, they refer to the same recursive type
        (Type::Cycle(d1), Type::Cycle(d2)) if d1 == d2 => true,

        // Handle cycles by looking up the type in the stack
        (Type::Cycle(depth), _) => {
            if type_stack.len() < *depth {
                return true; // Coinductive reasoning
            }
            let lookup_index = type_stack.len() - *depth;
            if let Some(&stack_type_id) = type_stack.get(lookup_index) {
                is_compatible_impl(stack_type_id, pattern_id, lookup, assumptions, type_stack)
            } else {
                true
            }
        }

        (_, Type::Cycle(depth)) => {
            if type_stack.len() < *depth {
                return true;
            }
            let lookup_index = type_stack.len() - *depth;
            if let Some(&stack_type_id) = type_stack.get(lookup_index) {
                is_compatible_impl(self_id, stack_type_id, lookup, assumptions, type_stack)
            } else {
                true
            }
        }

        // Callable types
        (
            Type::Callable {
                parameter: param1,
                result: result1,
                receive: receive1,
            },
            Type::Callable {
                parameter: param2,
                result: result2,
                receive: receive2,
            },
        ) => {
            // Only push if not already on stack - prevents stack growth during recursive traversal
            let already_on_stack = type_stack.contains(&pattern_id);
            if !already_on_stack {
                type_stack.push(pattern_id);
            }

            // Parameters are contravariant, results are covariant, receive is contravariant
            let result = is_compatible_impl(*param2, *param1, lookup, assumptions, type_stack)
                && is_compatible_impl(*result1, *result2, lookup, assumptions, type_stack)
                && is_compatible_impl(*receive2, *receive1, lookup, assumptions, type_stack);

            if !already_on_stack {
                type_stack.pop();
            }
            result
        }

        // When self is a union, ALL variants must be compatible with the pattern
        (Type::Union(variants), _) => variants.iter().all(|&variant_id| {
            is_compatible_impl(variant_id, pattern_id, lookup, assumptions, type_stack)
        }),

        // When pattern is a union, self must match ANY variant
        (_, Type::Union(variants)) => {
            // Only push if not already on stack - prevents stack growth during recursive traversal
            // Cycle depths are relative to type definition structure, not traversal depth
            let already_on_stack = type_stack.contains(&pattern_id);
            if !already_on_stack {
                type_stack.push(pattern_id);
            }
            let result = variants.iter().any(|&variant_id| {
                is_compatible_impl(self_id, variant_id, lookup, assumptions, type_stack)
            });
            if !already_on_stack {
                type_stack.pop();
            }
            result
        }

        // Concrete tuple vs partial type
        (
            Type::Tuple(concrete_id),
            Type::Partial {
                name: partial_name,
                fields: partial_fields,
            },
        ) => {
            let Some(concrete_info) = lookup.lookup_tuple(*concrete_id) else {
                return false;
            };

            // If partial has a name, concrete must match it
            if let Some(pname) = partial_name
                && concrete_info.name.as_ref() != Some(pname)
            {
                return false;
            }

            // Check that all partial fields exist in concrete with compatible types
            partial_fields.iter().all(|(partial_fname, partial_ftype)| {
                concrete_info
                    .fields
                    .iter()
                    .any(|(concrete_fname, concrete_ftype)| {
                        concrete_fname.as_ref() == Some(partial_fname)
                            && is_compatible_impl(
                                *concrete_ftype,
                                *partial_ftype,
                                lookup,
                                assumptions,
                                type_stack,
                            )
                    })
            })
        }

        // Partial vs partial - check structural compatibility
        (
            Type::Partial {
                name: name1,
                fields: fields1,
            },
            Type::Partial {
                name: name2,
                fields: fields2,
            },
        ) => {
            // Names must match if both have names
            if name1.is_some() && name2.is_some() && name1 != name2 {
                return false;
            }

            // All fields in pattern (fields2) must exist in self (fields1) with compatible types
            fields2.iter().all(|(fname2, ftype2)| {
                fields1.iter().any(|(fname1, ftype1)| {
                    fname1 == fname2
                        && is_compatible_impl(*ftype1, *ftype2, lookup, assumptions, type_stack)
                })
            })
        }

        // Type variables are compatible with anything
        (Type::Variable(_), _) | (_, Type::Variable(_)) => true,

        _ => false,
    }
}
