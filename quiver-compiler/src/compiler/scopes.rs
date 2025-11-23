use crate::{
    ast,
    compiler::{Error, Narrowings, Provenance, TypeAliasDef, helpers},
};
use quiver_core::types::Type;
use std::collections::HashMap;

/// A binding can be either a variable or a type alias
#[derive(Debug, Clone)]
pub enum Binding {
    Variable {
        ty: Type,
        index: usize,
        /// Provenance of the value stored in this variable (for tuple field resolution).
        provenance: super::Provenance,
    },
    TypeAlias(TypeAliasDef),
}

/// The kind of scope, used to distinguish function scopes from block scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Root scope (top-level program)
    Root,
    /// Function scope - its parameter IS the function parameter (accessible via $)
    Function,
    /// Block scope - its parameter is local to the block (accessible via ~>)
    Block,
}

/// Block/function parameter with provenance tracking.
///
/// Stores the parameter type, its stack index, and where its value originated
/// (for propagating narrowings back to the source).
#[derive(Debug, Clone)]
pub struct Parameter {
    /// The parameter's type.
    pub ty: Type,
    /// Stack index for the parameter.
    pub index: usize,
    /// Where this parameter's value came from (for narrowing propagation).
    pub provenance: Provenance,
}

/// Represents a scope in the compiler's variable environment.
///
/// Each scope tracks bindings (variables and type aliases), an optional parameter,
/// and any type narrowings in effect for this scope.
pub struct Scope {
    /// Variable and type alias bindings.
    pub bindings: HashMap<String, Binding>,
    /// Type narrowings that overlay bindings (from type checks).
    pub narrowings: Narrowings,
    /// Block/function parameter.
    pub parameter: Option<Parameter>,
    /// Kind of scope (root, function, or block).
    pub kind: ScopeKind,
}

impl Scope {
    /// Create a new scope with the given bindings, parameter, and kind.
    pub fn new(
        bindings: HashMap<String, Binding>,
        parameter: Option<Parameter>,
        kind: ScopeKind,
    ) -> Self {
        Self {
            bindings,
            narrowings: Narrowings::default(),
            parameter,
            kind,
        }
    }

    /// Create a new empty scope.
    pub fn empty() -> Self {
        Self {
            bindings: HashMap::new(),
            narrowings: Narrowings::default(),
            parameter: None,
            kind: ScopeKind::Root,
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
    provenance: super::Provenance,
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
                provenance,
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

/// Look up a variable in the scope stack, checking for narrowings.
///
/// Searches from innermost to outermost scope for the binding.
/// If found, checks for any narrowing from current scope back to the binding's scope.
/// Returns the narrowed type if one exists, otherwise the original type.
pub fn lookup_variable(
    scopes: &[Scope],
    name: &str,
    accessors: &[ast::AccessPath],
) -> Option<(Type, usize)> {
    let full_name = helpers::make_capture_name(name, accessors);

    // Find the scope containing the binding
    let (binding_scope_idx, binding) = scopes
        .iter()
        .enumerate()
        .rev()
        .find_map(|(i, s)| s.bindings.get(&full_name).map(|b| (i, b)))?;

    let Binding::Variable { ty, index, .. } = binding else {
        return None;
    };

    // Check for narrowings from current scope back to binding scope
    // (innermost narrowing takes precedence)
    for scope in scopes[binding_scope_idx..].iter().rev() {
        if let Some(narrowed) = scope.narrowings.variables.get(&full_name) {
            return Some((narrowed.clone(), *index));
        }
    }

    // No narrowing, return original type
    Some((ty.clone(), *index))
}

/// Look up the provenance stored for a variable.
/// Returns the provenance that was stored when the variable was defined.
pub fn lookup_variable_provenance(scopes: &[Scope], name: &str) -> Option<super::Provenance> {
    let full_name = helpers::make_capture_name(name, &[]);

    for scope in scopes.iter().rev() {
        if let Some(Binding::Variable { provenance, .. }) = scope.bindings.get(&full_name) {
            return Some(provenance.clone());
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

/// Get the parameter from the current (innermost) scope.
///
/// Returns the (possibly narrowed) parameter type and its stack index.
/// If a narrowing exists for the parameter in the current scope, returns the narrowed type.
pub fn get_parameter(scopes: &[Scope]) -> Result<(Type, usize), Error> {
    let scope = scopes.last().ok_or_else(|| Error::InternalError {
        message: "No scope available".to_string(),
    })?;

    let param = scope
        .parameter
        .as_ref()
        .ok_or_else(|| Error::InternalError {
            message: "No parameter in current scope".to_string(),
        })?;

    // Check for parameter narrowing in current scope
    let ty = scope
        .narrowings
        .parameter
        .clone()
        .unwrap_or_else(|| param.ty.clone());

    Ok((ty, param.index))
}

/// Get the function parameter (for $ operator).
///
/// Walks up scopes to find the nearest Function scope's parameter.
/// Returns the (possibly narrowed) parameter type and its stack index.
pub fn get_function_parameter(scopes: &[Scope]) -> Result<(Type, usize), Error> {
    for scope in scopes.iter().rev() {
        if scope.kind == ScopeKind::Function
            && let Some(param) = &scope.parameter
        {
            // Check for parameter narrowing
            let ty = scope
                .narrowings
                .parameter
                .clone()
                .unwrap_or_else(|| param.ty.clone());
            return Ok((ty, param.index));
        }
    }
    Err(Error::InternalError {
        message: "No function parameter available ($ used outside function)".to_string(),
    })
}
