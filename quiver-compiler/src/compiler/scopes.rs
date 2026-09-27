use crate::{
    ast,
    compiler::{Error, Narrowings, Provenance, TypeAliasDef, helpers},
};
use std::collections::HashMap;

/// A variable binding: its type, stack slot, and value provenance.
#[derive(Debug, Clone)]
pub struct Variable {
    /// Type ID referencing the Program's type registry.
    pub ty: usize,
    /// Stack index for the variable.
    pub index: usize,
    /// Provenance of the value stored in this variable (for tuple field resolution).
    pub provenance: super::Provenance,
}

/// Named bindings in a scope. Variables and type aliases are separate namespaces —
/// `name` (a value) and `'name` (a type) may coexist — so they live in separate maps.
#[derive(Debug, Clone, Default)]
pub struct Bindings {
    pub variables: HashMap<String, Variable>,
    pub type_aliases: HashMap<String, TypeAliasDef>,
}

impl Bindings {
    /// Remove all variables and type aliases.
    pub fn clear(&mut self) {
        self.variables.clear();
        self.type_aliases.clear();
    }
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
/// Stores the parameter type ID, its stack index, and where its value originated
/// (for propagating narrowings back to the source).
#[derive(Debug, Clone)]
pub struct Parameter {
    /// The parameter's type ID (referencing the Program's type registry).
    pub ty: usize,
    /// Stack index for the parameter.
    pub index: usize,
    /// Where this parameter's value came from (for narrowing propagation).
    pub provenance: Provenance,
}

/// A lexical scope: its bindings, its narrowings, and its parameter.
#[derive(Debug, Clone)]
pub struct Scope {
    /// Variable and type alias bindings.
    pub bindings: Bindings,
    /// Type narrowings that overlay bindings (from type checks). Values are type IDs.
    pub narrowings: Narrowings,
    /// Block/function parameter.
    pub parameter: Option<Parameter>,
    /// Kind of scope (root, function, or block).
    pub kind: ScopeKind,
}

impl Scope {
    /// Create a new scope with the given bindings, parameter, and kind.
    pub fn new(bindings: Bindings, parameter: Option<Parameter>, kind: ScopeKind) -> Self {
        Self {
            bindings,
            narrowings: Narrowings::default(),
            parameter,
            kind,
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
    var_type_id: usize,
    provenance: super::Provenance,
) -> Result<usize, Error> {
    let full_name = helpers::make_capture_name(name, accessors);
    let index = *local_count;
    *local_count += 1;
    if let Some(scope) = scopes.last_mut() {
        scope.bindings.variables.insert(
            full_name,
            Variable {
                ty: var_type_id,
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
        scope.bindings.type_aliases.insert(name, type_alias);
    }
}

/// Look up a variable in the scope stack, checking for narrowings.
///
/// Searches from innermost to outermost scope for the binding.
/// If found, checks for any narrowing from current scope back to the binding's scope.
/// Returns the narrowed type ID if one exists, otherwise the original type ID.
pub fn lookup_variable(
    scopes: &[Scope],
    name: &str,
    accessors: &[ast::AccessPath],
) -> Option<(usize, usize)> {
    let full_name = helpers::make_capture_name(name, accessors);

    // Find the scope containing the binding
    let (binding_scope_idx, variable) = scopes
        .iter()
        .enumerate()
        .rev()
        .find_map(|(i, s)| s.bindings.variables.get(&full_name).map(|v| (i, v)))?;

    // Check for narrowings from current scope back to binding scope
    // (innermost narrowing takes precedence)
    for scope in scopes[binding_scope_idx..].iter().rev() {
        if let Some(narrowed) = scope.narrowings.variables.get(&full_name) {
            return Some((*narrowed, variable.index));
        }
    }

    // No narrowing, return original type
    Some((variable.ty, variable.index))
}

/// Look up a variable's *declared* type, ignoring any runtime narrowings.
/// Unlike `lookup_variable`, this returns the type the binding was defined with.
pub fn lookup_declared_variable_type(scopes: &[Scope], name: &str) -> Option<usize> {
    let full_name = helpers::make_capture_name(name, &[]);
    scopes
        .iter()
        .rev()
        .find_map(|s| s.bindings.variables.get(&full_name).map(|v| v.ty))
}

/// Look up the provenance stored for a variable.
/// Returns the provenance that was stored when the variable was defined.
pub fn lookup_variable_provenance(scopes: &[Scope], name: &str) -> Option<super::Provenance> {
    let full_name = helpers::make_capture_name(name, &[]);
    scopes
        .iter()
        .rev()
        .find_map(|s| s.bindings.variables.get(&full_name))
        .map(|v| v.provenance.clone())
}

/// Look up a type alias in the scope stack
/// Searches from innermost to outermost scope
pub fn lookup_type_alias(scopes: &[Scope], name: &str) -> Option<TypeAliasDef> {
    scopes
        .iter()
        .rev()
        .find_map(|s| s.bindings.type_aliases.get(name).cloned())
}

/// The (possibly narrowed) type of the parameter of the scope at `level`: the innermost
/// narrowing of it in effect, or its declared type. `None` when that scope has no parameter.
pub fn parameter_type(scopes: &[Scope], level: usize) -> Option<usize> {
    let declared = scopes.get(level)?.parameter.as_ref()?.ty;
    Some(
        scopes[level..]
            .iter()
            .rev()
            .find_map(|scope| scope.narrowings.parameters.get(&level).copied())
            .unwrap_or(declared),
    )
}

/// The level of the current (innermost) scope, whose parameter is the block's input.
pub fn current_level(scopes: &[Scope]) -> usize {
    scopes.len() - 1
}

/// The level of the nearest enclosing function scope, whose parameter `$` names.
pub fn function_level(scopes: &[Scope]) -> Option<usize> {
    scopes
        .iter()
        .rposition(|scope| scope.kind == ScopeKind::Function && scope.parameter.is_some())
}

/// Get the parameter from the current (innermost) scope.
///
/// Returns the (possibly narrowed) parameter type ID and its stack index.
pub fn get_parameter(scopes: &[Scope]) -> Result<(usize, usize), Error> {
    let level = current_level(scopes);
    let param = scopes[level]
        .parameter
        .as_ref()
        .ok_or_else(|| Error::InternalError {
            message: "No parameter in current scope".to_string(),
        })?;
    let ty = parameter_type(scopes, level).expect("the scope has a parameter");
    Ok((ty, param.index))
}

/// Get the function parameter (for $ operator).
///
/// Finds the nearest Function scope's parameter.
/// Returns the (possibly narrowed) parameter type ID and its stack index.
pub fn get_function_parameter(scopes: &[Scope]) -> Result<(usize, usize), Error> {
    let level = function_level(scopes).ok_or_else(|| Error::InternalError {
        message: "No function parameter available ($ used outside function)".to_string(),
    })?;
    let param = scopes[level]
        .parameter
        .as_ref()
        .expect("found by its parameter");
    let ty = parameter_type(scopes, level).expect("the scope has a parameter");
    Ok((ty, param.index))
}

/// Get the enclosing function's *declared* parameter type, ignoring any narrowing
/// applied by the current branch's patterns. A self tail call (`^`) re-enters the whole
/// function — every branch re-dispatches — so its argument is checked against the
/// declared parameter, not the branch's narrowed view of it.
pub fn get_function_parameter_declared(scopes: &[Scope]) -> Result<(usize, usize), Error> {
    for scope in scopes.iter().rev() {
        if scope.kind == ScopeKind::Function
            && let Some(param) = &scope.parameter
        {
            return Ok((param.ty, param.index));
        }
    }
    Err(Error::InternalError {
        message: "No function parameter available (^ used outside function)".to_string(),
    })
}
