//! Provenance tracking for type narrowing.
//!
//! Provenance tracks where a value originated, enabling narrowing to affect
//! the original binding when type checks succeed.

use std::collections::HashMap;

/// Tracks where a value originated, enabling type narrowing.
///
/// When a type check succeeds on a value with known provenance, we can narrow
/// the type of the original binding (variable or parameter).
#[derive(Debug, Clone, PartialEq)]
pub enum Provenance {
    /// Value came from a variable.
    Variable(String),

    /// Value came from a field of another provenance.
    /// The usize is the field index.
    Field(Box<Provenance>, usize),

    /// Value is the block or function parameter.
    Parameter,

    /// Value is a tuple where each field has its own provenance.
    Tuple(Vec<Provenance>),

    /// Value has no trackable provenance (e.g., literals, function results).
    Unknown,
}

impl Provenance {
    /// Resolve tuple field access to its underlying provenance.
    ///
    /// If this provenance is a `Tuple`, returns the provenance at the given index.
    /// Otherwise, returns `Field(self, index)` to track nested field access.
    pub fn field(&self, index: usize) -> Provenance {
        match self {
            Provenance::Tuple(fields) => fields.get(index).cloned().unwrap_or(Provenance::Unknown),
            Provenance::Unknown => Provenance::Unknown,
            other => Provenance::Field(Box::new(other.clone()), index),
        }
    }
}

/// Type narrowings in effect within a scope.
///
/// Narrowings overlay the original bindings, providing refined types based on
/// runtime checks that have succeeded on the current control flow path.
/// All type references are type IDs into the Program's type registry.
#[derive(Debug, Clone, Default)]
pub struct Narrowings {
    /// Narrowed types for variables, keyed by variable name (values are type IDs).
    pub variables: HashMap<String, usize>,

    /// Narrowed type for the block/function parameter (if any) - type ID.
    pub parameter: Option<usize>,

    /// Field narrowings for any provenance, stored as (parent_provenance, field_index, narrowed_type_id).
    /// Used for tuple pattern complement narrowing where a specific field
    /// has been constrained but the parent type hasn't changed.
    pub fields: Vec<(Provenance, usize, usize)>,
}
