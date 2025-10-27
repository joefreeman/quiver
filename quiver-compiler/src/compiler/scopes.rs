use crate::ast;
use quiver_core::types::Type;
use std::collections::HashMap;

use super::{Error, helpers};

/// Represents a scope in the compiler's variable environment
/// Each scope tracks local variables and an optional function parameter
pub struct Scope {
    pub variables: HashMap<String, (Type, usize)>,
    pub parameter: Option<(Type, usize)>,
}

impl Scope {
    pub fn new(
        variables: HashMap<String, (Type, usize)>,
        parameter: Option<(Type, usize)>,
    ) -> Self {
        Self {
            variables,
            parameter,
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
) -> usize {
    let full_name = helpers::make_capture_name(name, accessors);
    let index = *local_count;
    *local_count += 1;
    if let Some(scope) = scopes.last_mut() {
        scope.variables.insert(full_name, (var_type, index));
    }
    index
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
        if let Some(&(ref variable_type, index)) = scope.variables.get(&full_name) {
            return Some((variable_type.clone(), index));
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
