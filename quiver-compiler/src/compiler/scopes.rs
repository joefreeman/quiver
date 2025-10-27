use crate::ast;
use quiver_core::types::Type;
use std::collections::HashMap;

use super::{Error, helpers, typing::TypeAliasDef};

/// A binding can be either a variable or a type alias
#[derive(Debug, Clone)]
pub enum Binding {
    Variable { ty: Type, index: usize },
    TypeAlias(TypeAliasDef),
}

/// Represents a scope in the compiler's variable environment
/// Each scope tracks bindings (variables and type aliases) and an optional function parameter
pub struct Scope {
    pub bindings: HashMap<String, Binding>,
    pub parameter: Option<(Type, usize)>,
}

impl Scope {
    pub fn new(bindings: HashMap<String, Binding>, parameter: Option<(Type, usize)>) -> Self {
        Self {
            bindings,
            parameter,
        }
    }

    /// Create a new empty scope
    pub fn empty() -> Self {
        Self {
            bindings: HashMap::new(),
            parameter: None,
        }
    }
}

/// Define a new variable in the current scope
/// Returns the allocated local index for the variable
pub fn define_variable(
    scopes: &mut [Scope],
    local_count: &mut usize,
    name: &str,
    accessors: &[ast::AccessPath],
    var_type: Type,
) -> Result<usize, Error> {
    // Only check base name (not captured field paths like "foo.x")
    if accessors.is_empty() && helpers::is_reserved_name(name) {
        return Err(Error::TypeUnresolved(format!(
            "Cannot use reserved primitive type '{}' as a variable name",
            name
        )));
    }

    let full_name = helpers::make_capture_name(name, accessors);
    let index = *local_count;
    *local_count += 1;
    if let Some(scope) = scopes.last_mut() {
        scope.bindings.insert(
            full_name,
            Binding::Variable {
                ty: var_type,
                index,
            },
        );
    }
    Ok(index)
}

/// Define a new type alias in the current scope
pub fn define_type_alias(scopes: &mut [Scope], name: String, type_alias: TypeAliasDef) {
    if let Some(scope) = scopes.last_mut() {
        scope.bindings.insert(name, Binding::TypeAlias(type_alias));
    }
}

/// Look up a variable in the scope stack
/// Searches from innermost to outermost scope
pub fn lookup_variable(
    scopes: &[Scope],
    name: &str,
    accessors: &[ast::AccessPath],
) -> Option<(Type, usize)> {
    let full_name = helpers::make_capture_name(name, accessors);
    for scope in scopes.iter().rev() {
        if let Some(Binding::Variable { ty, index }) = scope.bindings.get(&full_name) {
            return Some((ty.clone(), *index));
        }
    }
    None
}

/// Look up a type alias in the scope stack
/// Searches from innermost to outermost scope
pub fn lookup_type_alias(scopes: &[Scope], name: &str) -> Option<TypeAliasDef> {
    for scope in scopes.iter().rev() {
        if let Some(Binding::TypeAlias(type_alias)) = scope.bindings.get(name) {
            return Some(type_alias.clone());
        }
    }
    None
}

/// Get the parameter from the current (innermost) scope
pub fn get_parameter(scopes: &[Scope]) -> Result<(Type, usize), Error> {
    scopes
        .last()
        .and_then(|scope| scope.parameter.clone())
        .ok_or_else(|| Error::InternalError {
            message: "No parameter in current scope".to_string(),
        })
}
