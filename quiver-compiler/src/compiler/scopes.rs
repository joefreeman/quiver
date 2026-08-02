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

/// The kind of value behind a reconstruction-CSE slot. Together with the table id it
/// guards the (never-observed) case of two values of different shapes sharing one
/// payload allocation, so the payload pointer alone is never trusted as identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CseKind {
    Tuple,
    Function,
    Builtin,
}

/// Identity of a compile-time value for reconstruction CSE (`Compiler::emit_value_cse`):
/// payload identity is the sharing witness — every clone of a cached module value shares
/// its payload `Rc`. The key holds the `Rc` itself, not just its address: most emitted
/// values live in the module cache, but an *instantiated* builtin member's payload is
/// synthesized fresh per use site (`instantiate_builtin_member`) and would otherwise be
/// freed with its address up for reuse while the slot still pointed there — a
/// false-sharing miscompile. Owning the `Rc` pins the allocation for the slot's
/// lifetime, so equality by pointer ([`Rc::ptr_eq`]) is sound. Never serialized or
/// hashed into anything that outlives the compile.
#[derive(Debug, Clone)]
pub struct CseKey {
    pub kind: CseKind,
    /// The value's table id (tuple id, function index, or builtin id).
    pub id: usize,
    pub payload: std::rc::Rc<quiver_core::value::Payload>,
}

impl PartialEq for CseKey {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
            && self.id == other.id
            && std::rc::Rc::ptr_eq(&self.payload, &other.payload)
    }
}

impl Eq for CseKey {}

impl std::hash::Hash for CseKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.id.hash(state);
        std::rc::Rc::as_ptr(&self.payload).hash(state);
    }
}

/// Represents a scope in the compiler's variable environment.
///
/// Each scope tracks bindings (variables and type aliases), an optional parameter,
/// and any type narrowings in effect for this scope.
pub struct Scope {
    /// Variable and type alias bindings.
    pub bindings: Bindings,
    /// Type narrowings that overlay bindings (from type checks). Values are type IDs.
    pub narrowings: Narrowings,
    /// Block/function parameter.
    pub parameter: Option<Parameter>,
    /// Kind of scope (root, function, or block).
    pub kind: ScopeKind,
    /// Reconstruction-CSE slots: values this scope has already emitted, as
    /// `key → (type id, local index)`. A side table rather than named bindings, so the
    /// slots are invisible to everything that consumes `bindings` (exports, the REPL,
    /// tooling); living on the scope gives them the one property that matters — a slot
    /// dies with its scope, exactly when the compiler `Reset`s the scope's locals, so a
    /// later occurrence can never load a slot the runtime has discarded.
    pub cse_slots: HashMap<CseKey, (usize, usize)>,
}

impl Scope {
    /// Create a new scope with the given bindings, parameter, and kind.
    pub fn new(bindings: Bindings, parameter: Option<Parameter>, kind: ScopeKind) -> Self {
        Self {
            bindings,
            narrowings: Narrowings::default(),
            parameter,
            kind,
            cse_slots: HashMap::new(),
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

/// Look up a reconstruction-CSE slot, innermost scope first, yielding
/// `(type id, local index)`. A slot registered in a since-popped scope is simply
/// unreachable — the lifetime guarantee is the scope stack itself.
pub fn lookup_cse_slot(scopes: &[Scope], key: &CseKey) -> Option<(usize, usize)> {
    scopes
        .iter()
        .rev()
        .find_map(|scope| scope.cse_slots.get(key).copied())
}

/// Register a reconstruction-CSE slot in the innermost scope, allocating its local
/// index (the caller emits the `Store` that fills it, in allocation order).
pub fn define_cse_slot(
    scopes: &mut [Scope],
    local_count: &mut usize,
    key: CseKey,
    type_id: usize,
) -> usize {
    let index = *local_count;
    *local_count += 1;
    if let Some(scope) = scopes.last_mut() {
        scope.cse_slots.insert(key, (type_id, index));
    }
    index
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

/// Get the parameter from the current (innermost) scope.
///
/// Returns the (possibly narrowed) parameter type ID and its stack index.
/// If a narrowing exists for the parameter in the current scope, returns the narrowed type ID.
pub fn get_parameter(scopes: &[Scope]) -> Result<(usize, usize), Error> {
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
    let ty = scope.narrowings.parameter.unwrap_or(param.ty);

    Ok((ty, param.index))
}

/// Get the function parameter (for $ operator).
///
/// Walks up scopes to find the nearest Function scope's parameter.
/// Returns the (possibly narrowed) parameter type ID and its stack index.
pub fn get_function_parameter(scopes: &[Scope]) -> Result<(usize, usize), Error> {
    for scope in scopes.iter().rev() {
        if scope.kind == ScopeKind::Function
            && let Some(param) = &scope.parameter
        {
            // Check for parameter narrowing
            let ty = scope.narrowings.parameter.unwrap_or(param.ty);
            return Ok((ty, param.index));
        }
    }
    Err(Error::InternalError {
        message: "No function parameter available ($ used outside function)".to_string(),
    })
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
