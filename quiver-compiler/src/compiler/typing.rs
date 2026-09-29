use std::collections::{HashMap, HashSet};

use crate::ast;
use crate::resolver::{ModuleResolver, PackageId};
use quiver_core::{
    binders::{BinderStack, close_against, has_free_cycles, is_binder, rewrite_free_cycles},
    program::Program,
    types::{Type, TypeLookup, is_subsumed_by},
};

use super::modules::{self, ModuleCache};
use super::{Error, Scope, scopes};

/// Capabilities for resolving `'%mod` / `'%mod.name` module-type references: the package
/// to resolve module paths against, plus the resolver and module cache used to load and
/// build the target module's type namespace. Threaded through type resolution because a
/// module type can appear anywhere a type can.
pub struct TypeEnv<'a> {
    pub resolver: &'a dyn ModuleResolver,
    pub module_cache: &'a mut ModuleCache,
    pub package: &'a PackageId,
}

#[derive(Debug, Clone)]
pub enum TupleAccessor {
    Field(String),
    Position(usize),
}

/// Create a union type from a list of type IDs, flattening any unions among them.
///
/// A union that already holds every resulting member is the result itself. Otherwise each
/// union is spliced with its references to its own root closed (`splice_union_members`), so a
/// recursive member keeps meaning the union it was written in.
pub fn union_type_ids(program: &mut Program, type_ids: Vec<usize>) -> usize {
    // The top type already holds every other member.
    let is_top = |id: usize| matches!(program.lookup_type(id), Some(Type::Top));
    if type_ids.iter().any(|&id| {
        is_top(id)
            || matches!(program.lookup_type(id), Some(Type::Union(members)) if members.iter().any(|&m| is_top(m)))
    }) {
        return program.register_type(Type::Top);
    }
    let is_union = |id: &usize| matches!(program.lookup_type(*id), Some(Type::Union(_)));
    if type_ids.iter().any(is_union) {
        let plain = distinct_members(
            program,
            type_ids
                .iter()
                .flat_map(|&id| match program.lookup_type(id) {
                    Some(Type::Union(members)) => members.clone(),
                    _ => vec![id],
                }),
        );
        if plain.len() <= 1 {
            return plain.first().copied().unwrap_or_else(|| program.never());
        }
        if let Some(&whole) = type_ids.iter().find(|&&id| match program.lookup_type(id) {
            Some(Type::Union(members)) => {
                members.len() >= plain.len() && plain.iter().all(|m| members.contains(m))
            }
            _ => false,
        }) {
            return whole;
        }
    }

    let mut spliced = Vec::new();
    for type_id in type_ids {
        if matches!(program.lookup_type(type_id), Some(Type::Union(_))) {
            spliced.extend(splice_union_members(type_id, 1, program));
        } else {
            spliced.push(type_id);
        }
    }
    let unique = distinct_members(program, spliced);
    match unique.len() {
        0 => program.never(),
        1 => unique[0],
        _ => refold(&unique, program).unwrap_or_else(|| program.register_type(Type::Union(unique))),
    }
}

/// The recursive union these members are exactly one unrolling of, if any: `Nil | Cons['t,
/// 'l<'t>]` is `'l<'t>` spliced, and is `'l<'t>` — so a union that lost and regained members
/// (`'l | []` less `[]`) reads as the alias again, not as an equal but distinct twin. The
/// candidates are the unions a member's fields name.
fn refold(members: &[usize], program: &mut Program) -> Option<usize> {
    let mut candidates = Vec::new();
    for &member in members {
        if let Some(Type::Tuple(tuple_id)) = program.lookup_type(member)
            && let Some(info) = program.lookup_tuple(*tuple_id)
        {
            for &(_, field) in &info.fields {
                let is_union = matches!(program.lookup_type(field), Some(Type::Union(u)) if u.len() == members.len());
                if is_union && !candidates.contains(&field) {
                    candidates.push(field);
                }
            }
        }
    }
    candidates.into_iter().find(|&candidate| {
        let unrolled = splice_union_members(candidate, 1, program);
        unrolled.len() == members.len() && unrolled.iter().all(|m| members.contains(m))
    })
}

/// The members that are not subsumed by another, in order.
///
/// A member whose every value belongs to another member adds nothing to the union, so it is
/// dropped (`is_subsumed_by`). That folds members differing only by annotation rows, at any
/// depth — a freshly built `Nil` with its exact-empty row inside the open one — and members
/// structurally inside another, like `[Nil, 't]` beside `[Nil | Cons[…], 't]`. Retrieval on
/// a union is already governed by its weakest member, so a row dropped with its member loses
/// nothing. Of two members subsuming each other, the earlier is kept; a member subsuming
/// earlier ones takes the place of the first of them.
fn distinct_members(program: &Program, members: impl IntoIterator<Item = usize>) -> Vec<usize> {
    let subsumed_by = |member: usize, by: usize| {
        may_relate(program, member, by) && is_subsumed_by(member, by, program)
    };
    let mut kept: Vec<usize> = Vec::new();
    for member in members {
        if kept.iter().any(|&k| k == member || subsumed_by(member, k)) {
            continue;
        }
        let mut retained = 0;
        let mut slot = None;
        kept.retain(|&k| {
            let drop = subsumed_by(k, member);
            if drop {
                slot.get_or_insert(retained);
            } else {
                retained += 1;
            }
            !drop
        });
        kept.insert(slot.unwrap_or(kept.len()), member);
    }
    kept
}

/// A cheap pre-check for `is_subsumed_by` between two distinct members: whether their shapes
/// could relate at all. Only carriers can — identical primitives, variables and resources
/// share an id — and then only a tuple with a tuple of its name and labels or a partial it
/// may satisfy, or two partials, callables or processes. An intersection lies inside each of
/// its members, and is rare enough to always check.
fn may_relate(program: &Program, member: usize, by: usize) -> bool {
    match (program.lookup_base(member), program.lookup_base(by)) {
        (Some(Type::Intersection(_)), _) | (_, Some(Type::Intersection(_))) => true,
        (Some(Type::Tuple(t1)), Some(Type::Tuple(t2))) => {
            match (program.lookup_tuple(*t1), program.lookup_tuple(*t2)) {
                (Some(i1), Some(i2)) => {
                    i1.name == i2.name
                        && i1.fields.len() == i2.fields.len()
                        && i1
                            .fields
                            .iter()
                            .zip(&i2.fields)
                            .all(|((l1, _), (l2, _))| l1 == l2)
                }
                _ => false,
            }
        }
        (Some(Type::Tuple(t)), Some(Type::Partial { name, .. })) => {
            name.is_none()
                || program
                    .lookup_tuple(*t)
                    .is_some_and(|info| info.name == *name)
        }
        (Some(Type::Partial { .. }), Some(Type::Partial { .. }))
        | (Some(Type::Callable { .. }), Some(Type::Callable { .. }))
        | (Some(Type::Process { .. }), Some(Type::Process { .. })) => true,
        _ => false,
    }
}

/// Type alias definition - a pre-resolved type ID with type parameters.
/// The type_id may contain Type::Variable for generic parameters.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TypeAliasDef {
    pub parameters: Vec<String>,
    /// Each parameter's upper bound (`_` when unbounded), parallel to `parameters`: an
    /// argument applying the alias must fit it.
    pub bounds: Vec<usize>,
    pub type_id: usize,
}

/// Scope key under which a module's nameless default type (`' = ...`) is stored, so that a
/// bare `'` (`ast::Type::SelfDefault`) can resolve to it. A lone `'` is never a valid user
/// type-alias name, so this key cannot collide with one.
pub const SELF_DEFAULT_KEY: &str = "'";

fn instantiate_generic_type(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    name: &str,
    arguments: Vec<ast::Type>,
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<usize, Error> {
    // Look up type alias definition
    let type_def = scopes::lookup_type_alias(scopes_ref, name)
        .ok_or_else(|| Error::TypeAliasMissing(name.to_string()))?;

    instantiate_alias_def(
        recursion_depth,
        env,
        scopes_ref,
        name,
        &type_def,
        arguments,
        program,
        type_bindings,
    )
}

/// Instantiate a (possibly parameterised) type alias definition with the given type
/// arguments, substituting its `Type::Variable` placeholders. Shared between named alias
/// references and module-type references.
#[allow(clippy::too_many_arguments)]
fn instantiate_alias_def(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    name: &str,
    type_def: &TypeAliasDef,
    arguments: Vec<ast::Type>,
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<usize, Error> {
    // Resolve all type arguments
    let resolved_args: Vec<usize> = arguments
        .into_iter()
        .map(|arg| {
            resolve_ast_type_impl(
                recursion_depth,
                env,
                scopes_ref,
                arg,
                program,
                type_bindings,
            )
        })
        .collect::<Result<_, _>>()?;

    // Validate argument count
    if resolved_args.len() != type_def.parameters.len() {
        return Err(Error::TypeUnresolved(format!(
            "Generic type '{}' expects {} type argument(s), got {}",
            name,
            type_def.parameters.len(),
            resolved_args.len()
        )));
    }

    // Build type bindings for substitution
    let mut new_bindings = HashMap::new();
    for (param, arg) in type_def.parameters.iter().zip(resolved_args.iter()) {
        new_bindings.insert(param.clone(), *arg);
    }
    check_type_parameter_bounds(
        type_def
            .parameters
            .iter()
            .zip(type_def.bounds.iter().copied()),
        &new_bindings,
        program,
    )?;

    // Substitute Type::Variable placeholders with concrete types
    Ok(substitute(type_def.type_id, &new_bindings, program))
}

/// Validate that every union reachable from `root` is *productive* — a finite value can
/// be constructed for it. This is the semantic version of "recursion needs a base case":
/// a union whose variants all mention `^` is still fine when the cycle's target has base
/// cases of its own, e.g. the field-element union in
/// `Tuple[fields: (Nil | Cons[(^ | Labeled[label: Str['bin], value: ^]), ^1])]`.
///
/// Productivity is a least fixpoint: unions start unproductive, and the reachable graph
/// is re-evaluated until no verdict changes. A union is productive if any variant is; a
/// tuple or partial requires all fields; callables and processes are guarded (a closure
/// is a finite value however recursive its type — matching resolution, which lets `^`
/// cross function boundaries freely); a `Cycle` is productive iff the union it targets
/// is. A `Cycle` reaching above the walked fragment targets an enclosing function
/// boundary (parameter self-types start at depth 1) and is guarded, mirroring
/// `check_type_relation`'s optimistic underflow. An *empty* union (`never`, e.g. from
/// `'int & 'bin`) is unproductive but deliberate, so it propagates without being flagged.
///
/// Verdicts are keyed by type id. An interned union id shared between contexts that
/// resolve its cycles differently could in principle be over-approved — accepted, since
/// this check is a diagnostic against unconstructible definitions, not a soundness gate,
/// and erring towards acceptance is the safe direction. The walk skips memoization (a
/// shared node's verdict is context-dependent); definitions are small, and each pass
/// visits the whole reachable graph so every union gets a verdict.
fn validate_productive(root: usize, program: &Program) -> Result<(), Error> {
    let mut productive = std::collections::HashSet::new();
    loop {
        let mut changed = false;
        let mut all_productive = true;
        productivity_pass(
            root,
            program,
            &mut BinderStack::default(),
            &mut productive,
            &mut changed,
            &mut all_productive,
        );
        if !changed {
            // Fixpoint reached: this pass's verdicts are final.
            return if all_productive {
                Ok(())
            } else {
                Err(Error::TypeUnresolved(
                    "Union must have a constructible base case".to_string(),
                ))
            };
        }
    }
}

/// One evaluation pass for [`validate_productive`]: returns whether `type_id` is
/// productive under the current `productive` assumptions, growing them (setting
/// `changed`) as unions are proven, and clearing `all_productive` for any non-empty
/// union that remains unproven. Deliberately avoids short-circuiting so the whole
/// reachable graph is visited every pass.
fn productivity_pass(
    type_id: usize,
    program: &Program,
    stack: &mut BinderStack,
    productive: &mut std::collections::HashSet<usize>,
    changed: &mut bool,
    all_productive: &mut bool,
) -> bool {
    match program.lookup_type(type_id) {
        // Unresolvable ids are not this check's concern.
        None => true,
        Some(
            Type::Integer
            | Type::Binary
            | Type::Reference
            | Type::Resource(_)
            | Type::Variable(_)
            | Type::Intersection(_)
            | Type::Top,
        ) => true,
        // Guarded: closures and process ids are finite values however recursive their types.
        Some(Type::Callable { .. } | Type::Process { .. }) => true,
        Some(Type::Annotated { base, .. }) => {
            productivity_pass(*base, program, stack, productive, changed, all_productive)
        }
        Some(Type::Tuple(tuple_id)) => match program.lookup_tuple(*tuple_id) {
            None => true,
            Some(info) => info.fields.iter().fold(true, |acc, (_, field_id)| {
                productivity_pass(
                    *field_id,
                    program,
                    stack,
                    productive,
                    changed,
                    all_productive,
                ) && acc
            }),
        },
        Some(Type::Partial { fields, .. }) => fields.iter().fold(true, |acc, (_, field_id)| {
            productivity_pass(
                *field_id,
                program,
                stack,
                productive,
                changed,
                all_productive,
            ) && acc
        }),
        // Empty union: `never` — unproductive by nature, but deliberate; don't flag it.
        Some(Type::Union(variants)) if variants.is_empty() => false,
        Some(Type::Union(variants)) => {
            if stack.as_slice().contains(&type_id) {
                // In progress: answer with the current assumption (least fixpoint).
                return productive.contains(&type_id);
            }
            stack.enter(type_id);
            let mut any = false;
            for &variant_id in variants {
                if productivity_pass(
                    variant_id,
                    program,
                    stack,
                    productive,
                    changed,
                    all_productive,
                ) {
                    any = true;
                }
            }
            stack.leave(type_id);
            if any {
                if productive.insert(type_id) {
                    *changed = true;
                }
            } else if !productive.contains(&type_id) {
                *all_productive = false;
            }
            any || productive.contains(&type_id)
        }
        // A reference past the walked fragment names an enclosing function (a literal's
        // parameter is resolved inside it), which is guarded.
        Some(Type::Cycle(depth)) => stack
            .resolve(*depth)
            .is_none_or(|binder| productive.contains(&binder)),
    }
}

pub fn resolve_ast_type(
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    ast_type: ast::Type,
    program: &mut Program,
) -> Result<usize, Error> {
    let bindings = HashMap::new();
    let mut recursion_depth = 0;
    let type_id = resolve_ast_type_impl(
        &mut recursion_depth,
        env,
        scopes_ref,
        ast_type,
        program,
        &bindings,
    )?;
    validate_productive(type_id, program)?;
    Ok(type_id)
}

/// Resolve an AST type with pre-defined type variable bindings.
/// Used when importing types from modules where type parameters should be
/// resolved as Type::Variable.
pub fn resolve_ast_type_with_bindings(
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    ast_type: ast::Type,
    program: &mut Program,
    bindings: &HashMap<String, usize>,
) -> Result<usize, Error> {
    let mut recursion_depth = 0;
    let type_id = resolve_ast_type_impl(
        &mut recursion_depth,
        env,
        scopes_ref,
        ast_type,
        program,
        bindings,
    )?;
    validate_productive(type_id, program)?;
    Ok(type_id)
}

/// A declaration's type parameters, resolved (see [`resolve_type_parameters`]).
#[derive(Debug, Clone, Default)]
pub struct DeclaredTypeParameters {
    /// Source name (`t`) → the parameter's variable type id, for resolving the
    /// declaration's written types.
    pub bindings: HashMap<String, usize>,
    /// Each parameter's variable name and upper bound (`_` when unbounded), in declaration
    /// order.
    pub bounds: Vec<(String, usize)>,
}

impl DeclaredTypeParameters {
    /// The variable names, in declaration order.
    pub fn names(&self) -> Vec<String> {
        self.bounds.iter().map(|(name, _)| name.clone()).collect()
    }

    /// The rigid set in force with these parameters added to `outer` (see
    /// `TypeLookup::rigid_bound`).
    pub fn rigid_over(&self, outer: &HashMap<String, usize>) -> HashMap<String, usize> {
        let mut rigid = outer.clone();
        rigid.extend(self.bounds.iter().cloned());
        rigid
    }
}

/// Resolve a declaration's type parameters: register each one's variable, named by
/// `variable_name` from its source name, and resolve its bound. A bound may name the
/// parameters declared before it, which are rigid while it resolves — so a bounded alias it
/// applies to one of them checks that parameter's own bound — but not itself or later ones.
///
/// A parameter whose variable is already rigid is a nested literal re-declaring its
/// enclosing function's parameter (the names are uniquified per definition, so the two are
/// one variable): it keeps the enclosing bound, and may not state one of its own.
pub fn resolve_type_parameters(
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    parameters: &[ast::TypeParameter],
    variable_name: impl Fn(&str) -> String,
    program: &mut Program,
) -> Result<DeclaredTypeParameters, Error> {
    let outer = program.rigid_variables().clone();
    let mut declared = DeclaredTypeParameters::default();
    let result = (|| {
        for parameter in parameters {
            let variable = variable_name(&parameter.name);
            let enclosing = outer.get(&variable).copied();
            let bound = match (&parameter.bound, enclosing) {
                (None, Some(enclosing)) => enclosing,
                (Some(_), Some(_)) => {
                    return Err(Error::TypeParameterRedeclaredWithBound {
                        parameter: format!("'{}", parameter.name),
                    });
                }
                (Some(bound), None) => {
                    program.set_rigid_variables(declared.rigid_over(&outer));
                    resolve_ast_type_with_bindings(
                        env,
                        scopes_ref,
                        bound.clone(),
                        program,
                        &declared.bindings,
                    )?
                }
                (None, None) => program.register_type(Type::Top),
            };
            let variable_id = program.register_type(Type::Variable(variable.clone()));
            declared
                .bindings
                .insert(parameter.name.clone(), variable_id);
            declared.bounds.push((variable, bound));
        }
        Ok(())
    })();
    program.set_rigid_variables(outer);
    result.map(|()| declared)
}

/// Resolve a type alias's definition. Its parameters are variables named as written (an
/// alias is substituted by name wherever it is applied), rigid within the definition, so a
/// bounded alias the definition applies to one of them checks that parameter's own bound.
pub fn resolve_alias_definition(
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    type_parameters: &[ast::TypeParameter],
    type_definition: ast::Type,
    program: &mut Program,
) -> Result<TypeAliasDef, Error> {
    let declared =
        resolve_type_parameters(env, scopes_ref, type_parameters, str::to_string, program)?;
    let outer = program.set_rigid_variables(HashMap::new());
    program.set_rigid_variables(declared.rigid_over(&outer));
    let type_id = resolve_ast_type_with_bindings(
        env,
        scopes_ref,
        type_definition,
        program,
        &declared.bindings,
    );
    program.set_rigid_variables(outer);
    Ok(TypeAliasDef {
        parameters: declared.names(),
        bounds: declared.bounds.iter().map(|&(_, bound)| bound).collect(),
        type_id: type_id?,
    })
}

/// Check type arguments against the bounds of the parameters they instantiate: each bound,
/// with the instantiation substituted (a bound may name earlier parameters), must hold the
/// argument. `bindings` maps a parameter's variable name to its argument; a parameter it
/// leaves out is not checked.
pub fn check_type_parameter_bounds<'a>(
    bounds: impl IntoIterator<Item = (&'a String, usize)>,
    bindings: &HashMap<String, usize>,
    program: &mut Program,
) -> Result<(), Error> {
    for (name, bound) in bounds {
        let Some(&argument) = bindings.get(name) else {
            continue;
        };
        if matches!(program.lookup_type(bound), Some(Type::Top)) {
            continue;
        }
        let bound = substitute(bound, bindings, program);
        if !quiver_core::types::is_compatible(argument, bound, &*program) {
            return Err(Error::TypeParameterBound(Box::new(super::BoundMismatch {
                parameter: format!("'{}", name.split('#').next().unwrap_or(name)),
                bound: quiver_core::format::format_type_by_id(&*program, bound),
                found: quiver_core::format::format_type_by_id(&*program, argument),
            })));
        }
    }
    Ok(())
}

/// Resolve a function's written parameter or result type, its declared type parameters
/// bound (see [`resolve_type_parameters`]). A parameter's variable *name* is uniquified per
/// definition (`t#42`): variables are name-keyed, so two generic functions both declaring
/// `'t` would otherwise share one variable, and a call from one's body into the other would
/// unify "a variable with itself" and silently fail to pin it. Display strips the suffix
/// (see quiver-core's format).
pub fn resolve_function_parameter_type(
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    ast_type: ast::Type,
    bindings: &HashMap<String, usize>,
    program: &mut Program,
) -> Result<(usize, Vec<usize>), Error> {
    // Start at depth 1 since this is a function parameter (the function creates a recursion boundary)
    // This allows the parameter type to use & to refer to the enclosing function
    let mut recursion_depth = 1;
    let (type_id, omittable) = resolve_parameter_type(
        &mut recursion_depth,
        env,
        scopes_ref,
        ast_type,
        program,
        bindings,
    )?;
    validate_productive(type_id, program)?;
    Ok((type_id, omittable))
}

/// Resolve a type alias for display purposes (e.g., in tests or REPL).
/// Returns the type ID which already has Type::Variable placeholders for type parameters.
pub fn resolve_type_alias_for_display(
    scopes_ref: &[Scope],
    alias_name: &str,
) -> Result<usize, Error> {
    let type_def = scopes::lookup_type_alias(scopes_ref, alias_name)
        .ok_or_else(|| Error::TypeAliasMissing(alias_name.to_string()))?;

    Ok(type_def.type_id)
}

/// Resolve tuple name - distinguishes between literal names (capitalized) and type aliases (lowercase)
fn resolve_tuple_name(
    name: Option<String>,
    scopes_ref: &[Scope],
    program: &Program,
) -> Result<Option<String>, Error> {
    match name {
        None => Ok(None),
        Some(s) if s.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
            // Capitalized: literal tuple name like "Point"
            Ok(Some(s))
        }
        Some(identifier) => {
            // Lowercase: type alias reference like "event[..., x: int]"
            let type_alias = scopes::lookup_type_alias(scopes_ref, &identifier)
                .ok_or_else(|| Error::TypeAliasMissing(identifier.to_string()))?;

            // Type parameters must be empty for name inheritance (for now)
            if !type_alias.parameters.is_empty() {
                return Err(Error::FeatureUnsupported(format!(
                    "Type alias '{}' has type parameters - name inheritance from parameterized types requires explicit instantiation",
                    identifier
                )));
            }

            // Look up the resolved type to get the tuple name
            match program.lookup_type(type_alias.type_id) {
                Some(Type::Tuple(tuple_id)) => {
                    if let Some(info) = program.lookup_tuple(*tuple_id) {
                        Ok(info.name.clone())
                    } else {
                        Ok(None)
                    }
                }
                Some(Type::Union(_)) => {
                    // For unions, don't try to extract a single name
                    Ok(None)
                }
                _ => Err(Error::TypeUnresolved(format!(
                    "Type alias '{}' must resolve to a tuple or union type for identifier spread syntax",
                    identifier
                ))),
            }
        }
    }
}

type FieldSet = Vec<(Option<String>, usize, bool)>; // (field_name, type_id, omittable label)
type NamedFieldVariants = Vec<(Option<String>, FieldSet)>; // (tuple_name, fields)

/// Register a resolved (non-partial) tuple type, returning its id together with the
/// indices of the fields whose label was written omittable (`[(foo): 'int]`).
///
/// The marks are *returned*, never recorded against the tuple. Tuple types are interned
/// structurally, so a mark stored against a tuple id would be shared by every
/// structurally identical spelling in the program — including tuples that never asked for
/// it. A mark is a calling convention, so it belongs to the function type whose parameter
/// this spelling is, and only a parameter position keeps it (`resolve_parameter_type`).
/// Indices are into the *resolved* field list, so fields introduced by a spread keep their
/// positions.
fn register_resolved_tuple(
    program: &mut Program,
    name: Option<String>,
    fields: FieldSet,
) -> (usize, Vec<usize>) {
    let omittable: Vec<usize> = fields
        .iter()
        .enumerate()
        .filter_map(|(index, (_, _, omittable))| omittable.then_some(index))
        .collect();
    let tuple_id = program.register_tuple(
        name,
        fields.into_iter().map(|(name, ty, _)| (name, ty)).collect(),
    );
    (tuple_id, omittable)
}

/// Reject `(name):` outside a function's parameter tuple. A mark is a calling convention,
/// and only a function has callers — anywhere else it could never fire, so it is an error
/// rather than a silent no-op. Runs after the partial check, which has a sharper message
/// for the one shape people reach for by mistake.
fn check_marks_in_parameter(fields: &FieldSet, parameter_position: bool) -> Result<(), Error> {
    if !parameter_position && fields.iter().any(|(_, _, omittable)| *omittable) {
        return Err(Error::TypeUnresolved(
            "An optional field label (`(name): ...`) is only allowed in a function's \
             parameter type, where it tells callers they may omit the label"
                .to_string(),
        ));
    }
    Ok(())
}

/// Reject `(name):` markers in a partial type: partials match by name, so an omittable
/// label has nothing to mean there.
fn check_no_omittable_in_partial(fields: &FieldSet) -> Result<(), Error> {
    if fields.iter().any(|(_, _, omittable)| *omittable) {
        return Err(Error::TypeUnresolved(
            "Optional field labels (`(name): ...`) apply to tuple types, not partial types"
                .to_string(),
        ));
    }
    Ok(())
}

/// Resolve tuple fields that contain spreads
/// Returns a vector of (name, field_set) pairs - multiple pairs if spreading creates union variants
fn resolve_tuple_fields_with_spread(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    fields: &[ast::FieldType],
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<NamedFieldVariants, Error> {
    // Track all possible field combinations with their names (for union distribution)
    // Each variant is (tuple_name, fields)
    let mut variants: NamedFieldVariants = vec![(None, Vec::new())];

    // Process fields left to right
    for field in fields {
        match field {
            ast::FieldType::Field {
                name,
                omittable,
                type_def,
                default,
                ..
            } => {
                // A default belongs to a function, not to a type. `compile_function` strips
                // the ones it consumes from its parameter spelling before resolving it, so
                // one surviving to here was written where it could never fire.
                if default.is_some() {
                    return Err(Error::TypeUnresolved(
                        "A field default is only allowed in a function literal's parameter \
                         type, where it attaches to that function"
                            .to_string(),
                    ));
                }
                // A decorator states no type: it names a field an earlier entry — normally
                // a spread — already brought in, and adjusts only its label, leaving the
                // type and, crucially, the *position* alone. Moving it would silently
                // reorder positional calls, which is the very thing labels exist to enable.
                let Some(type_def) = type_def else {
                    let Some(field_name) = name else {
                        return Err(Error::TypeUnresolved(
                            "A field entry with no type must name the field it decorates"
                                .to_string(),
                        ));
                    };
                    for (_tuple_name, variant_fields) in &mut variants {
                        let Some(position) = variant_fields
                            .iter()
                            .position(|(existing, _, _)| existing.as_ref() == Some(field_name))
                        else {
                            return Err(Error::TypeUnresolved(format!(
                                "'{field_name}' states no type, so it decorates a field \
                                 brought in earlier — but nothing here has one by that name"
                            )));
                        };
                        variant_fields[position].2 = *omittable;
                    }
                    continue;
                };

                // Resolve the field type
                let field_type_id = resolve_ast_type_impl(
                    recursion_depth,
                    env,
                    scopes_ref,
                    type_def.clone(),
                    program,
                    type_bindings,
                )?;

                // Add this field to all variants
                for (_tuple_name, variant_fields) in &mut variants {
                    // Check if we should replace an existing field
                    if let Some(pos) = name.as_ref().and_then(|n| {
                        variant_fields
                            .iter()
                            .position(|(field_name, _, _)| field_name.as_ref() == Some(n))
                    }) {
                        variant_fields[pos].1 = field_type_id;
                        variant_fields[pos].2 = *omittable;
                        continue;
                    }
                    variant_fields.push((name.clone(), field_type_id, *omittable));
                }
            }
            ast::FieldType::Spread {
                identifier,
                type_arguments,
                ..
            } => {
                // Identifier must be specified (parser transforms `...` in `identifier[...]` to `...identifier`)
                let spread_id = identifier.as_ref().ok_or_else(|| {
                    Error::TypeUnresolved(
                        "Spread without identifier is only allowed in `identifier[..., fields]` syntax"
                            .to_string(),
                    )
                })?;

                // Look up the spread type
                let type_alias = scopes::lookup_type_alias(scopes_ref, spread_id)
                    .ok_or_else(|| Error::TypeAliasMissing(spread_id.clone()))?;

                // Check type parameter count matches
                if type_alias.parameters.len() != type_arguments.len() {
                    return Err(Error::TypeUnresolved(format!(
                        "Type alias '{}' expects {} type parameters, got {}",
                        spread_id,
                        type_alias.parameters.len(),
                        type_arguments.len()
                    )));
                }

                // Create type bindings for the spread type
                let mut spread_bindings = HashMap::new();
                for (param, arg) in type_alias.parameters.iter().zip(type_arguments.iter()) {
                    let arg_type_id = resolve_ast_type_impl(
                        recursion_depth,
                        env,
                        scopes_ref,
                        arg.clone(),
                        program,
                        type_bindings,
                    )?;
                    spread_bindings.insert(param.clone(), arg_type_id);
                }

                // Substitute type variables in the resolved type
                let spread_type_id = substitute(type_alias.type_id, &spread_bindings, program);

                // Extract tuple types with names from the spread (handling unions)
                let spread_named_types =
                    extract_tuples_from_type_with_names(spread_type_id, program)?;

                // For each existing variant, create new variants for each spread type
                let mut new_variants = Vec::new();
                for (existing_name, existing_fields) in &variants {
                    for (spread_name, spread_fields) in &spread_named_types {
                        let mut new_variant_fields = existing_fields.clone();
                        // Use spread name if existing name is None, otherwise keep existing
                        let new_name = existing_name.clone().or_else(|| spread_name.clone());

                        // Merge spread fields into variant fields
                        for (spread_field_name, spread_field_type_id, spread_omittable) in
                            spread_fields
                        {
                            // Check if we should replace an existing field
                            if let Some(pos) = spread_field_name.as_ref().and_then(|n| {
                                new_variant_fields
                                    .iter()
                                    .position(|(field_name, _, _)| field_name.as_ref() == Some(n))
                            }) {
                                new_variant_fields[pos].1 = *spread_field_type_id;
                                new_variant_fields[pos].2 = *spread_omittable;
                                continue;
                            }
                            new_variant_fields.push((
                                spread_field_name.clone(),
                                *spread_field_type_id,
                                *spread_omittable,
                            ));
                        }

                        new_variants.push((new_name, new_variant_fields));
                    }
                }
                variants = new_variants;
            }
        }
    }

    // Return all variants - caller will decide if this should be a union
    Ok(variants)
}

/// Extract tuple field definitions with names from a type, handling unions
fn extract_tuples_from_type_with_names(
    type_id: usize,
    program: &Program,
) -> Result<NamedFieldVariants, Error> {
    let Some(typ) = program.lookup_type(type_id) else {
        return Err(Error::TypeUnresolved("Type not found".to_string()));
    };

    match typ {
        Type::Tuple(tuple_id) => {
            let tuple_id = *tuple_id;
            let tuple_info = program
                .lookup_tuple(tuple_id)
                .ok_or(Error::TupleNotInRegistry { tuple_id })?;
            let fields: FieldSet = tuple_info
                .fields
                .iter()
                .map(|(name, ty)| (name.clone(), *ty, false))
                .collect();
            Ok(vec![(tuple_info.name.clone(), fields)])
        }
        Type::Partial { rest: Some(_), .. } => Err(Error::TypeUnresolved(
            "A partial type with a rest type (`*'t`) cannot be spread: the fields it speaks \
             for are not listed"
                .to_string(),
        )),
        Type::Partial {
            name,
            fields,
            rest: None,
        } => {
            // Convert partial fields (all named) to tuple field format
            let tuple_fields: FieldSet = fields
                .iter()
                .map(|(fname, ftype)| (Some(fname.clone()), *ftype, false))
                .collect();
            Ok(vec![(name.clone(), tuple_fields)])
        }
        Type::Union(variants) => {
            let variants = variants.clone();
            let mut all_named_fields = Vec::new();
            for variant_id in variants {
                let variant_named_fields =
                    extract_tuples_from_type_with_names(variant_id, program)?;
                all_named_fields.extend(variant_named_fields);
            }
            Ok(all_named_fields)
        }
        _ => Err(Error::TypeMismatch {
            expected: "tuple or partial".to_string(),
            found: format!("{:?}", typ),
        }),
    }
}

/// Resolve a tuple type spelling, returning its type id together with the indices of the
/// fields whose label was written omittable (`[(foo): 'int]`). Only a parameter position
/// keeps the marks — see `resolve_parameter_type`; every other position discards them.
fn resolve_tuple_ast(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    tuple: ast::TupleType,
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
    // True only for a function type's own parameter tuple, the one place a `(name):` mark
    // can mean anything. A nested tuple is resolved through `resolve_ast_type_impl`, which
    // passes false, so adoption stays top-level.
    parameter_position: bool,
) -> Result<(usize, Vec<usize>), Error> {
    // A partial's rest type. `(*_)` constrains nothing, so it is spelled as `()` is.
    let rest = tuple
        .rest
        .map(|rest| {
            resolve_ast_type_impl(
                recursion_depth,
                env,
                scopes_ref,
                *rest,
                program,
                type_bindings,
            )
        })
        .transpose()?
        .filter(|&rest| !matches!(program.lookup_type(rest), Some(Type::Top)));

    // Resolve field types without distributing unions
    // Check if there are any spreads
    let has_spread = tuple
        .fields
        .iter()
        .any(|f| matches!(f, ast::FieldType::Spread { .. }));

    if has_spread {
        // Handle spreads - may create multiple variants
        let field_variants = resolve_tuple_fields_with_spread(
            recursion_depth,
            env,
            scopes_ref,
            &tuple.fields,
            program,
            type_bindings,
        )?;

        // Determine if this should be a partial type based solely on the AST syntax
        // [...] produces a tuple, (...) produces a partial
        let is_partial = tuple.is_partial;

        // Process all variants (may be 1 or many)
        let mut variant_type_ids = Vec::new();
        let mut variant_omittable: Vec<Vec<usize>> = Vec::new();
        for (variant_name, fields) in field_variants {
            // Validate partial types
            if is_partial {
                check_no_omittable_in_partial(&fields)?;
                for (field_name, _, _) in &fields {
                    if field_name.is_none() {
                        return Err(Error::TypeUnresolved(
                            "All fields in a partial type must be named".to_string(),
                        ));
                    }
                }
            }
            check_marks_in_parameter(&fields, parameter_position)?;

            // Determine the final tuple name:
            // - None or capitalized name -> resolve directly
            // - Lowercase name (type alias) -> use variant name from spread
            let is_type_alias = tuple
                .name
                .as_ref()
                .is_some_and(|n| n.chars().next().is_some_and(|c| c.is_ascii_lowercase()));
            let final_name = if is_type_alias {
                // Use variant name from spread (inherits from source)
                variant_name
            } else {
                resolve_tuple_name(tuple.name.clone(), scopes_ref, program)?
            };

            let (type_id, omittable) = if is_partial {
                // Create inline partial type (not in tuples registry)
                let partial_fields: Vec<(String, usize)> = fields
                    .into_iter()
                    .map(|(name, type_id, _)| {
                        (name.expect("Partial fields must be named"), type_id)
                    })
                    .collect();
                let type_id = program.register_type(Type::Partial {
                    name: final_name,
                    fields: partial_fields,
                    rest,
                });
                // A partial never carries marks - `check_no_omittable_in_partial` above
                // has already rejected them.
                (type_id, Vec::new())
            } else {
                let (tuple_id, omittable) = register_resolved_tuple(program, final_name, fields);
                (program.register_type(Type::Tuple(tuple_id)), omittable)
            };
            variant_type_ids.push(type_id);
            variant_omittable.push(omittable);
        }

        // A mark names a field position, so it belongs to one tuple. A spread that
        // distributed over a union produced several, and the result is a union with no
        // positions of its own - so there is nothing to attribute the marks to.
        let omittable = if variant_omittable.len() == 1 {
            variant_omittable.remove(0)
        } else {
            Vec::new()
        };

        // Return single type or union based on variant count
        return Ok((union_type_ids(program, variant_type_ids), omittable));
    }

    // No spreads - process fields normally
    let mut fields: FieldSet = Vec::new();
    for field in tuple.fields {
        match field {
            ast::FieldType::Field {
                name,
                omittable,
                type_def,
                default,
                ..
            } => {
                // A default belongs to a function, not to a type. `compile_function`
                // strips the ones it consumes from its parameter spelling before
                // resolving it, so one surviving here could never fire.
                if default.is_some() {
                    return Err(Error::TypeUnresolved(
                        "A field default is only allowed in a function literal's \
                         parameter type, where it attaches to that function"
                            .to_string(),
                    ));
                }
                // Every entry here must state a type. A decorator adjusts a field brought
                // in by a spread, and this tuple has none — so there is nothing to adjust.
                let Some(type_def) = type_def else {
                    let described = name
                        .as_deref()
                        .map(|name| format!("'{name}'"))
                        .unwrap_or_else(|| "A field entry".to_string());
                    return Err(Error::TypeUnresolved(format!(
                        "{described} states no type, so it decorates a field brought in by \
                         a spread — but this tuple has no spread"
                    )));
                };
                let field_type_id = resolve_ast_type_impl(
                    recursion_depth,
                    env,
                    scopes_ref,
                    type_def,
                    program,
                    type_bindings,
                )?;
                fields.push((name, field_type_id, omittable));
            }
            ast::FieldType::Spread { .. } => unreachable!(),
        }
    }

    // Validate partial types
    if tuple.is_partial {
        check_no_omittable_in_partial(&fields)?;
        // All fields must be named
        for (field_name, _, _) in &fields {
            if field_name.is_none() {
                return Err(Error::TypeUnresolved(
                    "All fields in a partial type must be named".to_string(),
                ));
            }
        }
    }
    check_marks_in_parameter(&fields, parameter_position)?;

    // Resolve tuple name (may inherit from identifier spread)
    let resolved_name = resolve_tuple_name(tuple.name, scopes_ref, program)?;

    // Return Type::Partial for partial types, Type::Tuple for concrete types
    if tuple.is_partial {
        // Create inline partial type (not in tuples registry)
        let partial_fields: Vec<(String, usize)> = fields
            .into_iter()
            .map(|(name, type_id, _)| (name.expect("Partial fields must be named"), type_id))
            .collect();
        // A partial never carries marks - see the spread path above.
        Ok((
            program.register_type(Type::Partial {
                name: resolved_name,
                fields: partial_fields,
                rest,
            }),
            Vec::new(),
        ))
    } else {
        let (tuple_id, omittable) = register_resolved_tuple(program, resolved_name, fields);
        Ok((program.register_type(Type::Tuple(tuple_id)), omittable))
    }
}

/// Resolve a function parameter's spelling, returning its type id and the indices of the
/// fields whose label the caller may omit. Marks are kept only for a parameter written
/// directly as a tuple type: an alias reference (`#'point`) carries none, because a mark
/// in an alias is rejected — a bare tuple type has no caller to grant anything to.
pub fn resolve_parameter_type(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    parameter: ast::Type,
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<(usize, Vec<usize>), Error> {
    match parameter {
        ast::Type::Tuple(tuple) if !tuple.is_partial => resolve_tuple_ast(
            recursion_depth,
            env,
            scopes_ref,
            tuple,
            program,
            type_bindings,
            true,
        ),
        other => Ok((
            resolve_ast_type_impl(
                recursion_depth,
                env,
                scopes_ref,
                other,
                program,
                type_bindings,
            )?,
            Vec::new(),
        )),
    }
}

fn resolve_ast_type_impl(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    ast_type: ast::Type,
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<usize, Error> {
    match ast_type {
        ast::Type::Primitive(ast::PrimitiveType::Int) => Ok(program.register_type(Type::Integer)),
        ast::Type::Primitive(ast::PrimitiveType::Bin) => Ok(program.register_type(Type::Binary)),
        ast::Type::Primitive(ast::PrimitiveType::Ref) => Ok(program.register_type(Type::Reference)),
        ast::Type::Resource(name) => Ok(program.register_type(Type::Resource(name))),
        ast::Type::Top => Ok(program.register_type(Type::Top)),
        // Not a parameter position: a mark written here could never grant anything.
        ast::Type::Tuple(tuple) => Ok(resolve_tuple_ast(
            recursion_depth,
            env,
            scopes_ref,
            tuple,
            program,
            type_bindings,
            false,
        )?
        .0),
        ast::Type::Function(function) => {
            // Increment recursion depth for function boundary
            *recursion_depth += 1;

            let (input_type_id, omittable) = resolve_parameter_type(
                recursion_depth,
                env,
                scopes_ref,
                *function.input,
                program,
                type_bindings,
            )?;
            let output_type_id = resolve_ast_type_impl(
                recursion_depth,
                env,
                scopes_ref,
                *function.output,
                program,
                type_bindings,
            )?;
            // The `!'c` clause: what running the function receives. Omitted = receives
            // nothing (never), per "a declared type grants what it spells".
            let receive_type_id = match function.receive {
                Some(receive) => resolve_ast_type_impl(
                    recursion_depth,
                    env,
                    scopes_ref,
                    *receive,
                    program,
                    type_bindings,
                )?,
                None => program.never(),
            };
            // The `?'d` clause: states beyond the parameter (which is included
            // implicitly: states = input | d). Omitted = sampling not granted (None).
            let states_id = match function.states {
                Some(states) => {
                    let extension = resolve_ast_type_impl(
                        recursion_depth,
                        env,
                        scopes_ref,
                        *states,
                        program,
                        type_bindings,
                    )?;
                    Some(union_type_ids(program, vec![input_type_id, extension]))
                }
                None => None,
            };

            // Decrement recursion depth
            *recursion_depth -= 1;

            // Create function type without distributing unions
            Ok(program.register_type(Type::Callable {
                parameter: input_type_id,
                result: output_type_id,
                receive: receive_type_id,
                states: states_id,
                omittable,
            }))
        }
        ast::Type::Union(union) => {
            if union.types.is_empty() {
                return Err(Error::TypeUnresolved("Empty union type".to_string()));
            }
            // Productivity (base-case) validation happens on the resolved type, at the
            // top-level resolve entry points — see `validate_productive`.

            // Increment recursion depth for union boundary
            *recursion_depth += 1;

            // A member that is the nearest reference names this very union, which adds no
            // values: the author meant a boundary further out.
            if union
                .types
                .iter()
                .any(|member| matches!(member, ast::Type::Cycle(None | Some(0))))
            {
                return Err(Error::TypeUnresolved(
                    "`^` as a union member names that union itself, which adds nothing: count \
                     outward to the type enclosing it, starting from `^1`"
                        .to_string(),
                ));
            }

            // Resolve all union member types
            let mut resolved_type_ids = Vec::new();
            for member_type in union.types {
                let member_type_id = resolve_ast_type_impl(
                    recursion_depth,
                    env,
                    scopes_ref,
                    member_type,
                    program,
                    type_bindings,
                )?;
                // Flatten nested unions, written inside the one being built (so it gains no
                // binder): an alias-typed `'l` member keeps meaning `'l` in its own tails, and
                // a parenthesised `(B[^] | C)` member's reach through both unions shortens by
                // the one flattening removes. Non-union members keep their references to the
                // union being built.
                if matches!(program.lookup_type(member_type_id), Some(Type::Union(_))) {
                    resolved_type_ids.extend(splice_union_members(member_type_id, 0, program));
                } else {
                    resolved_type_ids.push(member_type_id);
                }
            }

            // Decrement recursion depth
            *recursion_depth -= 1;

            Ok(union_type_ids(program, resolved_type_ids))
        }
        ast::Type::Intersection(members) => {
            if members.is_empty() {
                return Err(Error::TypeUnresolved("Empty intersection type".to_string()));
            }
            // Resolve each member and fold with `intersect_types`: the result is the most specific
            // type satisfying all members (`never` when they're disjoint, e.g. `'int & 'bin`).
            let mut resolved: Option<usize> = None;
            for member_type in members {
                let member_id = resolve_ast_type_impl(
                    recursion_depth,
                    env,
                    scopes_ref,
                    member_type,
                    program,
                    type_bindings,
                )?;
                resolved = Some(match resolved {
                    None => member_id,
                    Some(acc) => super::narrowing::intersect_types(acc, member_id, program),
                });
            }
            Ok(resolved.expect("intersection has at least one member"))
        }
        ast::Type::Cycle(target_depth) => {
            // ^ or ^N syntax for cycle references
            let target_depth = target_depth.unwrap_or(0);

            // Validate: cycles require enclosing union or function
            if *recursion_depth == 0 {
                return Err(Error::TypeUnresolved(
                    "Cycle reference '^' requires enclosing union or function type".to_string(),
                ));
            }

            // `^N` counts N boundaries outward from the reference (`^` is `^0`, the nearest), so
            // it needs N + 1 of them around it.
            if target_depth >= *recursion_depth {
                return Err(Error::TypeUnresolved(format!(
                    "Invalid cycle reference ^{}: only {} enclosing union or function type(s)",
                    target_depth, *recursion_depth
                )));
            }

            // `Cycle(n)` is the n-th enclosing binder, counting from 1.
            Ok(program.register_type(Type::Cycle(target_depth + 1)))
        }
        ast::Type::Process(process) => {
            let receive_id = process
                .receive_type
                .map(|receive_type| {
                    resolve_ast_type_impl(
                        recursion_depth,
                        env,
                        scopes_ref,
                        *receive_type,
                        program,
                        type_bindings,
                    )
                })
                .transpose()?;
            let returns_id = process
                .return_type
                .map(|return_type| {
                    resolve_ast_type_impl(
                        recursion_depth,
                        env,
                        scopes_ref,
                        *return_type,
                        program,
                        type_bindings,
                    )
                })
                .transpose()?;
            let state_id = process
                .state_type
                .map(|state_type| {
                    resolve_ast_type_impl(
                        recursion_depth,
                        env,
                        scopes_ref,
                        *state_type,
                        program,
                        type_bindings,
                    )
                })
                .transpose()?;
            Ok(program.register_type(Type::Process {
                send: receive_id,
                receive: returns_id,
                state: state_id,
            }))
        }
        ast::Type::Identifier { name, arguments } => {
            // For bare identifiers (no arguments), check type parameter bindings first
            if arguments.is_empty() {
                // Check type parameter bindings first (highest priority)
                if let Some(&bound_type_id) = type_bindings.get(&name) {
                    return Ok(bound_type_id);
                }
            }

            // Instantiate the type (works for both bare identifiers and parameterized types)
            instantiate_generic_type(
                recursion_depth,
                env,
                scopes_ref,
                &name,
                arguments,
                program,
                type_bindings,
            )
        }
        ast::Type::ModuleType {
            module,
            member,
            arguments,
        } => {
            // Build the target module's type namespace and select the default type
            // (`'%mod`) or a named one (`'%mod.name`).
            let namespace = modules::module_type_namespace(
                &module,
                env.resolver,
                env.module_cache,
                env.package,
                program,
            )?;
            let display = display_module_type(&module, member.as_deref());
            let type_def =
                match &member {
                    None => namespace.default.ok_or_else(|| Error::ModuleTypeMissing {
                        type_name: display.clone(),
                        module: module.join("/"),
                    })?,
                    Some(name) => namespace.named.get(name).cloned().ok_or_else(|| {
                        Error::ModuleTypeMissing {
                            type_name: name.clone(),
                            module: module.join("/"),
                        }
                    })?,
                };

            instantiate_alias_def(
                recursion_depth,
                env,
                scopes_ref,
                &display,
                &type_def,
                arguments,
                program,
                type_bindings,
            )
        }
        ast::Type::SelfDefault { arguments } => {
            // Bare `'`: the enclosing module's own default type, stored in scope under the
            // reserved key when its `' = ...` marker was compiled.
            let type_def =
                scopes::lookup_type_alias(scopes_ref, SELF_DEFAULT_KEY).ok_or_else(|| {
                    Error::TypeUnresolved(
                        "`'` refers to the module's default type, but this module defines none"
                            .to_string(),
                    )
                })?;
            instantiate_alias_def(
                recursion_depth,
                env,
                scopes_ref,
                "'",
                &type_def,
                arguments,
                program,
                type_bindings,
            )
        }
    }
}

/// Human-readable form of a module type reference, e.g. `'%mathx/vec` or `'%shapes.circle`.
fn display_module_type(module: &[String], member: Option<&str>) -> String {
    let path = module.join("/");
    match member {
        Some(name) => format!("'%{path}.{name}"),
        None => format!("'%{path}"),
    }
}

/// Require that a runtime type test can decide `type_id`: one mentioning a type variable
/// has no runtime meaning, as the code does not know what its caller chose.
pub fn require_testable(type_id: usize, lookup: &impl TypeLookup) -> Result<(), Error> {
    if contains_variables(type_id, lookup) {
        return Err(Error::TypeTestNotConcrete {
            tested: quiver_core::format::format_type_by_id(lookup, type_id),
        });
    }
    Ok(())
}

/// `type_id` with every type variable replaced by the top type: what a runtime test of it
/// can actually check, knowing nothing of what the variables were instantiated to.
pub fn erase_type_variables(type_id: usize, program: &mut Program) -> usize {
    let mut names = Vec::new();
    collect_type_variables(type_id, &*program, &mut names);
    let top = program.register_type(Type::Top);
    let bindings = names.into_iter().map(|name| (name, top)).collect();
    substitute(type_id, &bindings, program)
}

/// Whether no value has the type: `never`, or a union, tuple or partial that can't be built
/// (`'int & 'bin`, `[x: never]`). Recursive references aren't followed, so a type is only
/// reported uninhabited where that shows without unrolling it.
pub fn is_uninhabited(type_id: usize, program: &Program) -> bool {
    match program.lookup_type(type_id) {
        Some(Type::Union(members)) => members
            .iter()
            .all(|&member| is_uninhabited(member, program)),
        Some(Type::Tuple(tuple_id)) => program
            .lookup_tuple(*tuple_id)
            .is_some_and(|info| info.fields.iter().any(|&(_, t)| is_uninhabited(t, program))),
        Some(Type::Partial { fields, .. }) => {
            fields.iter().any(|&(_, t)| is_uninhabited(t, program))
        }
        Some(Type::Annotated { base, .. }) => is_uninhabited(*base, program),
        _ => false,
    }
}

/// Whether a type mentions a type variable that isn't rigid here: one nothing in scope binds,
/// such as a generic function value's own.
pub fn contains_free_variables(type_id: usize, program: &Program) -> bool {
    let mut names = Vec::new();
    collect_type_variables(type_id, program, &mut names);
    names.iter().any(|name| program.rigid_bound(name).is_none())
}

/// Whether a type mentions the type variable `name`.
pub fn mentions_variable(type_id: usize, name: &str, program: &Program) -> bool {
    let mut names = Vec::new();
    collect_type_variables(type_id, program, &mut names);
    names.iter().any(|mentioned| mentioned == name)
}

/// Check if a type contains any unbound type variables
pub fn contains_variables(type_id: usize, lookup: &impl TypeLookup) -> bool {
    let Some(typ) = lookup.lookup_type(type_id) else {
        return false;
    };

    match typ {
        Type::Variable(_) => true,
        Type::Union(variants) | Type::Intersection(variants) => {
            let variants = variants.clone();
            variants.iter().any(|&v| contains_variables(v, lookup))
        }
        Type::Callable {
            parameter,
            result,
            receive,
            states,
            ..
        } => {
            contains_variables(*parameter, lookup)
                || contains_variables(*result, lookup)
                || contains_variables(*receive, lookup)
                || states.is_some_and(|s| contains_variables(s, lookup))
        }
        Type::Process {
            send,
            receive,
            state,
        } => {
            send.is_some_and(|s| contains_variables(s, lookup))
                || receive.is_some_and(|r| contains_variables(r, lookup))
                || state.is_some_and(|s| contains_variables(s, lookup))
        }
        Type::Tuple(tuple_id) => {
            // Check if any field contains variables
            if let Some(type_info) = lookup.lookup_tuple(*tuple_id) {
                type_info
                    .fields
                    .iter()
                    .any(|(_, field_type_id)| contains_variables(*field_type_id, lookup))
            } else {
                // If we can't look up the type, conservatively assume it might have variables
                false
            }
        }
        Type::Partial { fields, rest, .. } => {
            // Check if any field in the partial, or its rest type, contains variables
            fields
                .iter()
                .map(|(_, field_type_id)| field_type_id)
                .chain(rest)
                .any(|field_type_id| contains_variables(*field_type_id, lookup))
        }
        Type::Annotated { base, entries, .. } => {
            contains_variables(*base, lookup)
                || entries
                    .iter()
                    .any(|(_, value_type)| contains_variables(*value_type, lookup))
        }
        Type::Integer
        | Type::Binary
        | Type::Reference
        | Type::Cycle(_)
        | Type::Resource(_)
        | Type::Top => false,
    }
}

/// Collect the distinct type-variable names in a type, in first-occurrence order (the
/// traversal mirrors `contains_variables`; recursive types terminate through `Cycle`
/// nodes). Used to order a builtin signature's type parameters for explicit
/// instantiation — a source definition's declaration order is recorded instead.
pub fn collect_type_variables(type_id: usize, lookup: &impl TypeLookup, names: &mut Vec<String>) {
    let Some(typ) = lookup.lookup_type(type_id) else {
        return;
    };

    match typ {
        Type::Variable(name) => {
            if !names.contains(name) {
                names.push(name.clone());
            }
        }
        Type::Union(variants) | Type::Intersection(variants) => {
            for v in variants.clone() {
                collect_type_variables(v, lookup, names);
            }
        }
        Type::Callable {
            parameter,
            result,
            receive,
            states,
            ..
        } => {
            let (parameter, result, receive, states) = (*parameter, *result, *receive, *states);
            collect_type_variables(parameter, lookup, names);
            collect_type_variables(result, lookup, names);
            collect_type_variables(receive, lookup, names);
            if let Some(s) = states {
                collect_type_variables(s, lookup, names);
            }
        }
        Type::Process {
            send,
            receive,
            state,
        } => {
            for t in [*send, *receive, *state].into_iter().flatten() {
                collect_type_variables(t, lookup, names);
            }
        }
        Type::Tuple(tuple_id) => {
            if let Some(type_info) = lookup.lookup_tuple(*tuple_id) {
                for (_, field_type_id) in type_info.fields.clone() {
                    collect_type_variables(field_type_id, lookup, names);
                }
            }
        }
        Type::Partial { fields, rest, .. } => {
            let rest = *rest;
            for (_, field_type_id) in fields.clone() {
                collect_type_variables(field_type_id, lookup, names);
            }
            if let Some(rest) = rest {
                collect_type_variables(rest, lookup, names);
            }
        }
        Type::Annotated { base, entries, .. } => {
            let (base, entries) = (*base, entries.clone());
            collect_type_variables(base, lookup, names);
            for (_, value_type) in entries {
                collect_type_variables(value_type, lookup, names);
            }
        }
        Type::Integer
        | Type::Binary
        | Type::Reference
        | Type::Cycle(_)
        | Type::Resource(_)
        | Type::Top => {}
    }
}

/// Substitute type variables in a type with their bindings
/// Collect the names of type variables occurring in `type_id`, split by variance
/// relative to the root: a callable's parameter and a process's send clause flip the
/// polarity; everything else preserves it.
fn collect_variables_by_variance(
    type_id: usize,
    program: &Program,
    covariant: bool,
    co: &mut std::collections::HashSet<String>,
    contra: &mut std::collections::HashSet<String>,
) {
    let Some(typ) = program.lookup_type(type_id) else {
        return;
    };
    match typ {
        Type::Variable(name) => {
            if covariant {
                co.insert(name.clone());
            } else {
                contra.insert(name.clone());
            }
        }
        Type::Union(members) | Type::Intersection(members) => {
            for &member in members.clone().iter() {
                collect_variables_by_variance(member, program, covariant, co, contra);
            }
        }
        Type::Tuple(id) => {
            if let Some(info) = program.lookup_tuple(*id) {
                for (_, field) in info.fields.clone() {
                    collect_variables_by_variance(field, program, covariant, co, contra);
                }
            }
        }
        Type::Partial { fields, rest, .. } => {
            let rest = *rest;
            for (_, field) in fields.clone() {
                collect_variables_by_variance(field, program, covariant, co, contra);
            }
            if let Some(rest) = rest {
                collect_variables_by_variance(rest, program, covariant, co, contra);
            }
        }
        Type::Callable {
            parameter,
            result,
            receive,
            states,
            ..
        } => {
            let (parameter, result, receive, states) = (*parameter, *result, *receive, *states);
            collect_variables_by_variance(parameter, program, !covariant, co, contra);
            collect_variables_by_variance(result, program, covariant, co, contra);
            collect_variables_by_variance(receive, program, covariant, co, contra);
            if let Some(states) = states {
                collect_variables_by_variance(states, program, covariant, co, contra);
            }
        }
        Type::Process {
            send,
            receive,
            state,
        } => {
            let (send, receive, state) = (*send, *receive, *state);
            if let Some(send) = send {
                collect_variables_by_variance(send, program, !covariant, co, contra);
            }
            if let Some(receive) = receive {
                collect_variables_by_variance(receive, program, covariant, co, contra);
            }
            if let Some(state) = state {
                collect_variables_by_variance(state, program, covariant, co, contra);
            }
        }
        Type::Annotated { base, entries, .. } => {
            let base = *base;
            let entries = entries.clone();
            collect_variables_by_variance(base, program, covariant, co, contra);
            for (_, value) in entries {
                collect_variables_by_variance(value, program, covariant, co, contra);
            }
        }
        _ => {}
    }
}

/// Close a call result's unpinned type parameters: any variable the unification left
/// unbound, occurring only covariantly in the result, is bound to the empty union — no
/// value of that type was supplied, so the result provably can't produce one
/// (`child "x"` yields a tree that carries no events). Left open instead: a variable
/// with a contravariant occurrence (a returned function's own parameter must stay
/// callable), and a *rigid* variable (see `TypeLookup::rigid_bound`) — one of the
/// enclosing generic's own parameters, not the callee's to instantiate (calling a
/// `'p<'t>`-typed parameter inside a generic body must keep `'t` in the result).
pub fn close_unpinned_result(
    result_id: usize,
    bindings: &mut HashMap<String, usize>,
    program: &mut Program,
) {
    let mut co = std::collections::HashSet::new();
    let mut contra = std::collections::HashSet::new();
    collect_variables_by_variance(result_id, program, true, &mut co, &mut contra);
    let unpinned: Vec<String> = co
        .into_iter()
        .filter(|name| {
            !contra.contains(name)
                && !bindings.contains_key(name)
                && program.rigid_bound(name).is_none()
        })
        .collect();
    if unpinned.is_empty() {
        return;
    }
    let never = program.never();
    for name in unpinned {
        bindings.insert(name, never);
    }
}

/// Shift every *free* `Cycle` in `type_id` by `shift` binder levels. A `Cycle(k)` at a
/// position `cutoff` binders deep within the walked fragment is free iff `k > cutoff` —
/// it reaches above the fragment's root — and becomes `Cycle(k + shift)`; bound cycles
/// (self-contained recursion) are position-independent and travel unchanged. Binders are
/// unions and callables, mirroring written-type resolution's `recursion_depth`.
///
/// This is the hygiene step for splicing a type under binders: `substitute` lengthens a
/// type argument's outward references by the depth of the `Variable` it replaces.
fn shift_free_cycles_at(
    type_id: usize,
    shift: isize,
    cutoff: usize,
    program: &mut Program,
) -> usize {
    if shift == 0 {
        return type_id;
    }
    rewrite_free_cycles(type_id, cutoff, program, &mut |depth, _, program| {
        let shifted = usize::try_from(depth as isize + shift)
            .expect("cycle shift must keep the reference within its binders");
        program.register_type(Type::Cycle(shifted))
    })
}

/// The members of `union_id`, rewritten to stand directly in a union that replaces it —
/// the flattening step, which removes `union_id` as a binder (see `close_root_references`).
/// `gained` is 1 when the new union takes `union_id`'s place (a union of types at the same
/// position), and 0 when `union_id` was already written inside it (a parenthesised member of
/// a written union).
pub fn splice_union_members(union_id: usize, gained: usize, program: &mut Program) -> Vec<usize> {
    let Some(Type::Union(members)) = program.lookup_type(union_id).cloned() else {
        panic!("splice_union_members on a non-union type");
    };
    members
        .into_iter()
        .map(|member| close_root_references(member, union_id, gained, program))
        .collect()
}

/// Rewrite a part of the binder `root` — a union's member, or a function type's parameter or
/// result — to stand outside it, `gained` binders (0 or 1) now enclosing it where `root` did.
///
/// The part's references to `root` itself would otherwise be captured by whatever binder
/// encloses it next and mean *that* (`'l | []` spliced as `Nil | Cons['int, ^] | []` would
/// admit a `[]` tail), so they are closed: replaced by `root`, with `root`'s own outward
/// references lengthened for the deeper position. References reaching past `root` are
/// adjusted by the binders the move adds and removes.
pub fn close_root_references(
    member: usize,
    root: usize,
    gained: usize,
    program: &mut Program,
) -> usize {
    rewrite_free_cycles(member, 0, program, &mut |depth, cutoff, program| {
        if depth == cutoff + 1 {
            shift_free_cycles_at(root, (gained + cutoff) as isize, 0, program)
        } else {
            program.register_type(Type::Cycle(depth + gained - 1))
        }
    })
}

/// `type_id` with any union among its members spliced in, so that none is itself a union.
/// Closing a reference that stood as a member (`(^1 | [])`, closed with the union it named)
/// leaves one there. Each spliced union was written inside this one, and its members'
/// references to it are closed so they keep their meaning (`splice_union_members`). Their
/// fields may hold such a union again, a level down, flattened when that level is read.
pub fn flatten_union_members(type_id: usize, program: &mut Program) -> usize {
    let is_union =
        |id: usize, program: &Program| matches!(program.lookup_type(id), Some(Type::Union(_)));
    let Some(Type::Union(members)) = program.lookup_type(type_id).cloned() else {
        return type_id;
    };
    if !members.iter().any(|&m| is_union(m, program)) {
        return type_id;
    }
    let mut flat = Vec::with_capacity(members.len());
    for member in members {
        let spliced = if is_union(member, program) {
            splice_union_members(member, 0, program)
        } else {
            vec![member]
        };
        for piece in spliced {
            if !flat.contains(&piece) {
                flat.push(piece);
            }
        }
    }
    // Structurally: this union stays the binder its other members' references count.
    let flattened = program.register_type(Type::Union(flat));
    flatten_union_members(flattened, program)
}

/// The parts of a function type, each taken out of it to stand on its own (see
/// `open_callable`).
#[derive(Clone, Copy, Debug)]
pub struct CallableParts {
    pub parameter: usize,
    pub result: usize,
    pub receive: usize,
    pub states: Option<usize>,
}

/// The parts of the function type `type_id` (under any annotation rows), for use outside it: a
/// call's argument check, the parameter a body sees, a received message. A function type is the
/// binder of every part, so a `^` naming it (`#[(self): ^, …] -> 'r`) is closed with the
/// function type itself. `None` when `type_id` is not a function type.
pub fn open_callable(type_id: usize, program: &mut Program) -> Option<CallableParts> {
    let callable = Type::strip_annotations(type_id, &*program);
    let Some(Type::Callable {
        parameter,
        result,
        receive,
        states,
        ..
    }) = program.lookup_type(callable).cloned()
    else {
        return None;
    };
    let mut open = |part| close_root_references(part, callable, 0, program);
    Some(CallableParts {
        parameter: open(parameter),
        result: open(result),
        receive: open(receive),
        states: states.map(open),
    })
}

/// `type_id` with every annotation row removed, at any depth: the data shape alone.
pub fn erase_rows(type_id: usize, program: &mut Program) -> usize {
    fn erase(type_id: usize, program: &mut Program, memo: &mut HashMap<usize, usize>) -> usize {
        if let Some(&erased) = memo.get(&type_id) {
            return erased;
        }
        let Some(typ) = program.lookup_type(type_id).cloned() else {
            return type_id;
        };
        let erased = match typ {
            Type::Annotated { base, .. } => erase(base, program, memo),
            _ => {
                let parts: Vec<usize> = typ
                    .parts(&*program)
                    .into_iter()
                    .map(|part| erase(part, program, memo))
                    .collect();
                program.with_parts(type_id, &parts)
            }
        };
        memo.insert(type_id, erased);
        erased
    }
    erase(type_id, program, &mut HashMap::new())
}

/// Widen a type that grew by embedding its previous self — `new` holding `old` inside its
/// members, as a folder's accumulator does when each round wraps the last (`Nil` grows to
/// `Nil | Cons['int, Nil]`, then to `… | Cons['int, Nil | Cons['int, Nil]]`) — into the recursive
/// type that growth converges to: each embedded `old` becomes a reference to `new`'s root.
/// Members that were only an earlier round's (cycle-free, and inside the rest) are dropped,
/// which leaves the set of values unchanged. `None` when `new` does not embed `old`.
pub fn generalize_growth(new: usize, old: usize, program: &mut Program) -> Option<usize> {
    fn embed(
        type_id: usize,
        old: usize,
        depth: usize,
        program: &mut Program,
        found: &mut bool,
    ) -> usize {
        if type_id == old {
            *found = true;
            return program.register_type(Type::Cycle(depth));
        }
        let Some(typ) = program.lookup_type(type_id).cloned() else {
            return type_id;
        };
        let inner = depth + usize::from(is_binder(&typ));
        let parts: Vec<usize> = typ
            .parts(&*program)
            .into_iter()
            .map(|part| embed(part, old, inner, program, found))
            .collect();
        program.with_parts(type_id, &parts)
    }

    let Some(Type::Union(members)) = program.lookup_type(new).cloned() else {
        return None;
    };
    if has_free_cycles(old, program) {
        return None;
    }
    let mut found = false;
    // The members themselves stay: `old`'s own members are values `new` still holds.
    let members: Vec<usize> = members
        .iter()
        .map(|&m| {
            if m == old {
                m
            } else {
                embed(m, old, 1, program, &mut found)
            }
        })
        .collect();
    if !found {
        return None;
    }
    let mut generalized = program.register_type(Type::Union(members.clone()));
    // A cycle-free member the rest already covers adds no values: without it, the root still
    // means the same set, so the references keep their meaning.
    for member in members {
        let Some(Type::Union(current)) = program.lookup_type(generalized).cloned() else {
            break;
        };
        if current.len() < 2 || has_free_cycles(member, program) || !current.contains(&member) {
            continue;
        }
        let rest: Vec<usize> = current.iter().copied().filter(|&m| m != member).collect();
        let candidate = match rest[..] {
            [only] => only,
            _ => program.register_type(Type::Union(rest)),
        };
        if quiver_core::types::is_compatible(member, candidate, &*program)
            && matches!(program.lookup_type(candidate), Some(Type::Union(_)))
        {
            generalized = candidate;
        }
    }
    Some(generalized)
}

pub fn substitute(
    type_id: usize,
    bindings: &HashMap<String, usize>,
    program: &mut Program,
) -> usize {
    substitute_at(type_id, bindings, 0, program)
}

/// `substitute`, tracking how many binders (unions/callables) the walk has descended
/// through, so that a spliced argument's free `Cycle`s can be lengthened to keep
/// pointing at the binders they were written under (see `shift_free_cycles_at`).
fn substitute_at(
    type_id: usize,
    bindings: &HashMap<String, usize>,
    depth: usize,
    program: &mut Program,
) -> usize {
    let Some(typ) = program.lookup_type(type_id).cloned() else {
        return type_id;
    };

    match typ {
        Type::Variable(name) => match bindings.get(&name).copied() {
            // The argument was resolved at the alias-reference position; spliced `depth`
            // binders below it, its outward references must reach that much further.
            Some(bound) => shift_free_cycles_at(bound, depth as isize, 0, program),
            None => type_id,
        },
        Type::Union(variants) => {
            let new_variants: Vec<usize> = variants
                .iter()
                .map(|&v| {
                    let substituted = substitute_at(v, bindings, depth + 1, program);
                    // `union_type_ids` flattens a variant that is itself a union,
                    // stripping one binder from the path of any cycle inside it that
                    // reaches through its root — compensate. Only substitution
                    // introduces union-shaped variants (resolution already flattens),
                    // so gate on the variant having become one.
                    let was_union = matches!(program.lookup_type(v), Some(Type::Union(_)));
                    let is_union = matches!(program.lookup_type(substituted), Some(Type::Union(_)));
                    if is_union && !was_union {
                        shift_free_cycles_at(substituted, -1, 0, program)
                    } else {
                        substituted
                    }
                })
                .collect();
            union_type_ids(program, new_variants)
        }
        // Its variables known, an intersection is computed: `'t & []` is `[]` for a `'t` that
        // holds nil, and nothing for one that does not.
        Type::Intersection(members) => {
            let substituted: Vec<usize> = members
                .iter()
                .map(|&member| substitute_at(member, bindings, depth, program))
                .collect();
            if substituted == members {
                type_id
            } else {
                super::narrowing::meet(substituted, program)
            }
        }
        Type::Annotated {
            base,
            exact,
            entries,
        } => {
            let new_base = substitute_at(base, bindings, depth, program);
            let mut any_changed = new_base != base;
            let new_entries: Vec<(usize, usize)> = entries
                .iter()
                .map(|(key, value_type)| {
                    let new_value = substitute_at(*value_type, bindings, depth, program);
                    any_changed = any_changed || new_value != *value_type;
                    (*key, new_value)
                })
                .collect();
            if any_changed {
                program.annotate_type(new_base, exact, new_entries)
            } else {
                type_id
            }
        }
        // The rest only carry the substitution into their parts, a binder's one deeper.
        _ => {
            let inner = depth + usize::from(is_binder(&typ));
            let parts: Vec<usize> = typ
                .parts(&*program)
                .into_iter()
                .map(|part| substitute_at(part, bindings, inner, program))
                .collect();
            program.with_parts(type_id, &parts)
        }
    }
}

/// A tuple's compact one-line description for mismatch messages: its name and field
/// count (`Ev[…1]`, `[…4]`), not its full field types.
fn describe_tuple(name: &Option<String>, fields: usize) -> String {
    match name {
        Some(n) if fields == 0 => n.clone(),
        Some(n) => format!("{n}[…{fields}]"),
        None if fields == 0 => "[]".to_string(),
        None => format!("[…{fields}]"),
    }
}

/// A compact shape description of a type for mismatch messages: tuples by name and
/// arity, unions as their joined members. Used where a *detail* already locates the
/// mismatch, so a full format would only repeat it at length.
fn describe_shape(program: &Program, type_id: usize) -> String {
    match program.lookup_type(type_id) {
        Some(Type::Tuple(id)) => match program.lookup_tuple(*id) {
            Some(info) => describe_tuple(&info.name, info.fields.len()),
            None => quiver_core::format::format_type_by_id(program, type_id),
        },
        Some(Type::Union(members)) => members
            .clone()
            .iter()
            .map(|&member| describe_shape(program, member))
            .collect::<Vec<_>>()
            .join(" | "),
        Some(Type::Annotated { base, .. }) => {
            let base = *base;
            describe_shape(program, base)
        }
        _ => quiver_core::format::format_type_by_id(program, type_id),
    }
}

/// A tuple's near-miss signature: its name and field labels. A union variant sharing a
/// concrete tuple's signature is the member the author meant, so its inner failure is
/// the message worth surfacing.
fn tuple_signature(
    program: &Program,
    type_id: usize,
) -> Option<(Option<String>, Vec<Option<String>>)> {
    match program.lookup_type(type_id) {
        Some(Type::Tuple(id)) => program.lookup_tuple(*id).map(|info| {
            (
                info.name.clone(),
                info.fields.iter().map(|(label, _)| label.clone()).collect(),
            )
        }),
        _ => None,
    }
}

/// `type_id` followed through the bindings while it is a bound variable.
fn resolve_bound_variable(
    bindings: &HashMap<String, usize>,
    mut type_id: usize,
    program: &Program,
) -> usize {
    let mut steps = 0;
    while let Some(Type::Variable(name)) = program.lookup_type(type_id)
        && let Some(&bound) = bindings.get(name)
        && steps <= bindings.len()
    {
        type_id = bound;
        steps += 1;
    }
    type_id
}

/// Follow `name`'s binding `bound` through variables bound in turn, answering the last
/// variable on the way and what it holds.
fn chain_end(
    bindings: &HashMap<String, usize>,
    name: &str,
    mut bound: usize,
    program: &Program,
) -> (String, usize) {
    let mut holder = name.to_string();
    let mut steps = 0;
    while let Some(Type::Variable(next)) = program.lookup_type(bound)
        && let Some(&next_bound) = bindings.get(next)
        && steps <= bindings.len()
    {
        holder = next.clone();
        bound = next_bound;
        steps += 1;
    }
    (holder, bound)
}

/// The name of `type_id` when it is a free (not rigid) type variable nothing has bound yet.
fn unbound_free_variable(
    bindings: &HashMap<String, usize>,
    type_id: usize,
    program: &Program,
) -> Option<String> {
    match program.lookup_type(type_id) {
        Some(Type::Variable(name))
            if !bindings.contains_key(name) && program.rigid_bound(name).is_none() =>
        {
            Some(name.clone())
        }
        _ => None,
    }
}

/// Whether `type_id` mentions a free variable nothing has bound yet.
fn has_unbound_free_variables(
    bindings: &HashMap<String, usize>,
    type_id: usize,
    program: &Program,
) -> bool {
    let mut names = Vec::new();
    collect_type_variables(type_id, program, &mut names);
    names
        .iter()
        .any(|name| !bindings.contains_key(name) && program.rigid_bound(name).is_none())
}

/// Separates a variable instantiated afresh from the variable it instantiates.
const FRESH_SEPARATOR: char = '@';

/// The declared variable a (possibly freshly instantiated) variable name stands for.
pub fn declared_variable(name: &str) -> &str {
    name.split(FRESH_SEPARATOR).next().unwrap_or(name)
}

/// `type_id` with each free variable nothing has bound yet renamed to a fresh one.
fn instantiate_afresh(
    bindings: &HashMap<String, usize>,
    type_id: usize,
    program: &mut Program,
) -> usize {
    static NEXT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let mut names = Vec::new();
    collect_type_variables(type_id, &*program, &mut names);
    names.retain(|name| !bindings.contains_key(name) && program.rigid_bound(name).is_none());
    let renaming: HashMap<String, usize> = names
        .into_iter()
        .map(|name| {
            let n = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            let declared = declared_variable(&name);
            let fresh =
                program.register_type(Type::Variable(format!("{declared}{FRESH_SEPARATOR}{n}")));
            (name, fresh)
        })
        .collect();
    substitute(type_id, &renaming, program)
}

/// Resolve bindings that mention other bound variables (`'b := 't`, `'t := 'bin`), so that
/// substituting them leaves no variable that was solved — a binding to an unknown the
/// argument supplied must not survive as that unknown once it is known.
pub fn resolve_bindings(bindings: &mut HashMap<String, usize>, program: &mut Program) {
    for _ in 0..bindings.len() {
        let mut changed = false;
        let names: Vec<String> = bindings.keys().cloned().collect();
        for name in names {
            // Without its own entry, so a binding that mentions itself stays as it is.
            let mut others = bindings.clone();
            let value = others.remove(&name).expect("a key of the map");
            let resolved = substitute(value, &others, program);
            if resolved != value {
                bindings.insert(name, resolved);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

/// Unify a pattern type (containing Type::Variable) with a concrete type.
/// Builds up a mapping from type variable names to concrete type IDs.
/// Returns an error if there's a conflict (e.g., variable bound to two different types).
/// Combine two sets of type-variable bindings drawn from different variants of the same
/// concrete union. A variable the two disagree on is bound to the *union* of what each
/// found: both variants really can arrive, so both belong in its type. Overwriting (or
/// dropping one side) would narrow the variable to a single variant.
fn merge_bindings(
    left: HashMap<String, usize>,
    right: HashMap<String, usize>,
    program: &mut Program,
) -> HashMap<String, usize> {
    let mut merged = left;
    for (name, id) in right {
        match merged.get(&name).copied() {
            Some(existing) if existing != id => {
                let widened = union_type_ids(program, vec![existing, id]);
                merged.insert(name, widened);
            }
            _ => {
                merged.insert(name, id);
            }
        }
    }
    merged
}

pub fn unify(
    bindings: &mut HashMap<String, usize>,
    pattern_id: usize,
    concrete_id: usize,
    program: &mut Program,
) -> Result<(), Error> {
    unify_bounded(
        bindings,
        &mut Unifier::default(),
        false,
        pattern_id,
        concrete_id,
        program,
    )
}

/// Unify, carrying the set of variables whose binding is an **upper bound** rather than
/// a lower one — those bound from a contravariant position, where the concrete type
/// says what the value *accepts* rather than what it *is*.
///
/// Ordinary unification widens: two occurrences of `'t` meeting different types make
/// `'t` their union, which is how an element type is inferred from several places. An
/// upper-bound occurrence cannot widen, because widening would manufacture a capability
/// the value does not have — `%proc.send_after`'s `[(to): @'m, (message): 'm]` binds `'m`
/// to what the target accepts, so a message that is not already covered is a mismatch, not a reason to
/// grow `'m`.
/// Unification state threaded through one `unify`: the variables bound as upper bounds (see
/// `unify_bounded`), the pattern-side binders (unions and callables) entered, innermost last, which a
/// pattern `Cycle(n)` counts back through, and the (pattern union, concrete) pairs already being
/// unified, which a recursive type revisits.
#[derive(Default)]
struct Unifier {
    upper: HashSet<String>,
    binders: BinderStack,
    visiting: HashSet<(usize, usize)>,
    /// How many concrete-side function types the walk is inside: a generic function value's
    /// variables are instantiated afresh where the outermost one is met.
    in_concrete_callable: usize,
}

impl Unifier {
    /// A trial unification's state: no upper bounds of its own, but the same binders.
    fn fresh(&self) -> Self {
        Unifier {
            upper: HashSet::new(),
            binders: self.binders.clone(),
            visiting: self.visiting.clone(),
            in_concrete_callable: self.in_concrete_callable,
        }
    }

    /// A trial of one of the pattern union `pattern_id`'s members: `fresh`, with the union
    /// entered.
    fn member_trial(&self, pattern_id: usize) -> Self {
        let mut trial = self.fresh();
        trial.binders.enter(pattern_id);
        trial
    }
}

fn unify_bounded(
    bindings: &mut HashMap<String, usize>,
    ctx: &mut Unifier,
    contra: bool,
    pattern_id: usize,
    concrete_id: usize,
    program: &mut Program,
) -> Result<(), Error> {
    let pattern = program.lookup_type(pattern_id).cloned();
    let concrete = program.lookup_type(concrete_id).cloned();

    let (Some(pattern), Some(concrete)) = (pattern, concrete) else {
        return Ok(()); // If we can't look up either type, assume compatible
    };

    // A type unifies with itself without constraint: a generic function passed where its own
    // type is expected (`map [map, …]`, for a `(self): ^` parameter) fits every instance. A
    // bare variable still binds, so a result it pins stays pinned.
    if pattern_id == concrete_id && !matches!(pattern, Type::Variable(_)) {
        return Ok(());
    }

    match (&pattern, &concrete) {
        // A rigid variable (a type parameter of the body being compiled) is not the call's to
        // solve: it is one opaque type, which the value must already belong to.
        (Type::Variable(name), _) if program.rigid_bound(name).is_some() => {
            if quiver_core::types::is_compatible(concrete_id, pattern_id, &*program) {
                Ok(())
            } else {
                Err(Error::TypeUnresolved(format!(
                    "{} is not {}",
                    quiver_core::format::format_type_by_id(&*program, concrete_id),
                    quiver_core::format::format_type_by_id(&*program, pattern_id),
                )))
            }
        }

        // When pattern is a variable, bind it or check consistency
        (Type::Variable(name), _) => {
            let resolved_concrete_id = resolve_bound_variable(bindings, concrete_id, program);

            // An unbound free variable on the concrete side is the argument's own unknown: a
            // generic function value passed where a function type is expected (`apply [f: id,
            // x: <01>]`, `id`'s `'t`). It is solved here like the pattern's own, so meeting
            // it binds rather than fits whatever it meets.
            if let Some(unknown) = unbound_free_variable(bindings, resolved_concrete_id, program) {
                if unknown == *name {
                    return Ok(());
                }
                match bindings.get(name).copied() {
                    Some(existing_id) => {
                        let (_, existing_id) = chain_end(bindings, name, existing_id, program);
                        if unbound_free_variable(bindings, existing_id, program).as_ref()
                            != Some(&unknown)
                        {
                            bindings.insert(unknown, existing_id);
                        }
                    }
                    None => {
                        bindings.insert(name.clone(), resolved_concrete_id);
                        if contra {
                            ctx.upper.insert(name.clone());
                        }
                    }
                }
                return Ok(());
            }

            if let Some(existing_id) = bindings.get(name).copied() {
                // Bound through the argument's unknowns (`'a := 't` from `f: id`), the binding
                // ends at the variable holding the value: widening happens there, and an
                // unknown still unsolved is solved by this occurrence.
                let (holder, existing_id) = chain_end(bindings, name, existing_id, program);
                if let Some(unknown) = unbound_free_variable(bindings, existing_id, program) {
                    bindings.insert(unknown, resolved_concrete_id);
                    return Ok(());
                }
                // A binding holding unknowns inside (`'a := ['q, 'q]` from `f: first`) is
                // solved by this occurrence. Widening past them instead would leave them
                // unsolved, and the value — which must fit the function that supplied them —
                // unchecked against it.
                if existing_id != resolved_concrete_id
                    && has_unbound_free_variables(bindings, existing_id, program)
                {
                    return unify_bounded(
                        bindings,
                        &mut ctx.fresh(),
                        contra,
                        existing_id,
                        resolved_concrete_id,
                        program,
                    );
                }
                if existing_id != resolved_concrete_id {
                    if ctx.upper.contains(name) || ctx.upper.contains(&holder) {
                        // The binding is an upper bound (see `unify_bounded`): this
                        // occurrence must fit inside it. Widening would hand the
                        // caller a capability the value never had.
                        if !quiver_core::types::is_compatible(
                            resolved_concrete_id,
                            existing_id,
                            &*program,
                        ) {
                            return Err(Error::TypeUnresolved(format!(
                                "{} does not fit {}",
                                quiver_core::format::format_type_by_id(
                                    &*program,
                                    resolved_concrete_id
                                ),
                                quiver_core::format::format_type_by_id(&*program, existing_id),
                            )));
                        }
                    } else {
                        // Widen the type variable to a union
                        let widened =
                            union_type_ids(program, vec![existing_id, resolved_concrete_id]);
                        bindings.insert(holder, widened);
                    }
                }
            } else {
                bindings.insert(name.clone(), resolved_concrete_id);
                if contra {
                    ctx.upper.insert(name.clone());
                }
            }
            Ok(())
        }

        // Everything fits the top type, and it has no variables to bind.
        (Type::Top, _) => Ok(()),

        // An intersection expected: the value must fit every member, and each binds.
        (Type::Intersection(members), _) => members.iter().try_for_each(|&member| {
            unify_bounded(bindings, ctx, contra, member, concrete_id, program)
        }),
        // An intersection given: its values lie in every member, so fitting through any one
        // suffices. Known types are tried before variables, which (unbound, from an enclosing
        // generic) fit only a variable; the first that fits is replayed for real.
        (_, Type::Intersection(members)) => {
            let (variables, known): (Vec<usize>, Vec<usize>) =
                members.iter().partition(|&&member| {
                    matches!(program.lookup_type(member), Some(Type::Variable(_)))
                });
            let mut first_error = None;
            for member in known.into_iter().chain(variables) {
                let mut trial = bindings.clone();
                match unify_bounded(
                    &mut trial,
                    &mut ctx.fresh(),
                    contra,
                    pattern_id,
                    member,
                    program,
                ) {
                    Ok(()) => {
                        return unify_bounded(bindings, ctx, contra, pattern_id, member, program);
                    }
                    Err(error) => {
                        first_error.get_or_insert(error);
                    }
                }
            }
            Err(first_error.expect("an intersection has members"))
        }

        // Annotation rows are transparent to structural unification: a `'t` pattern binds
        // the whole annotated type (the Variable arm above fires first), but a structural
        // pattern (tuple/callable/...) unifies against the row's base. A row in pattern
        // position is likewise peeled.
        (_, Type::Annotated { base, .. }) => {
            unify_bounded(bindings, ctx, contra, pattern_id, *base, program)
        }
        (Type::Annotated { base, .. }, _) => {
            unify_bounded(bindings, ctx, contra, *base, concrete_id, program)
        }

        // When concrete is a variable, resolve it and try unifying with the resolved type
        (_, Type::Variable(name)) => {
            if let Some(&resolved_id) = bindings.get(name) {
                // Concrete variable is bound - unify with its binding
                unify_bounded(bindings, ctx, contra, pattern_id, resolved_id, program)
            } else if program.rigid_bound(name).is_none() {
                // The argument's own unknown (see the variable-pattern arm) meeting a known
                // shape takes it — closed against the pattern's binders, so a recursive
                // reference in it keeps its meaning out of place.
                let shape = close_against(pattern_id, ctx.binders.as_slice(), program);
                bindings.insert(name.clone(), shape);
                Ok(())
            } else {
                // An unbound concrete-side variable is a *rigid* variable from an
                // enclosing generic context (e.g. a captured value whose type mentions
                // the enclosing function's parameter). Treat it as an opaque type: it
                // satisfies a variable member of a union pattern (bound against it, so
                // the result keeps it), and otherwise whatever its bound satisfies —
                // generic bodies are checked once, so accepting more here would let
                // ill-typed calls through generics compile and fail at runtime.
                if let Type::Union(members) = &pattern {
                    for member in members.clone() {
                        if let Some(Type::Variable(_)) = program.lookup_type(member) {
                            return unify_bounded(
                                bindings,
                                ctx,
                                contra,
                                member,
                                concrete_id,
                                program,
                            );
                        }
                    }
                }
                if let Some(bound) = program.rigid_bound(name)
                    && !matches!(program.lookup_type(bound), Some(Type::Top))
                {
                    return unify_bounded(bindings, ctx, contra, pattern_id, bound, program);
                }
                Err(Error::TypeUnresolved(format!(
                    "Cannot unify rigid type variable {} with expected type {}",
                    quiver_core::format::format_type_by_id(&*program, concrete_id),
                    quiver_core::format::format_type_by_id(&*program, pattern_id),
                )))
            }
        }

        // Both are basic types - must match
        (Type::Integer, Type::Integer) => Ok(()),
        (Type::Binary, Type::Binary) => Ok(()),
        (Type::Reference, Type::Reference) => Ok(()),
        (Type::Resource(a), Type::Resource(b)) if a == b => Ok(()),

        // Process types must match in structure
        (
            Type::Process {
                send: send1,
                receive: receive1,
                state: state1,
            },
            Type::Process {
                send: send2,
                receive: receive2,
                state: state2,
            },
        ) => {
            // Unify state types when both are stated; a missing side imposes no
            // constraint here (the strict direction is subtyping's job, not unification's)
            if let (Some(st1), Some(st2)) = (state1, state2) {
                unify_bounded(bindings, ctx, contra, *st1, *st2, program)?;
            }
            // Send and receive likewise: unify only when both are stated. A generic
            // param like `@!'r` accepts any pid whose receive pins 'r — the
            // send grant is simply dropped, exactly as the covariant subtype allows
            // (a declared clause grants a capability; omitting one never demands
            // the value lack it). Send unifies pattern-first like every other position
            // — variance is `is_compatible`'s job and unification's is to bind, so a
            // `@'m` parameter must see its variable in pattern position or it reads as
            // rigid — but the binding it makes is an *upper* bound: `'m` becomes what
            // this target accepts, and a later occurrence must fit inside it rather
            // than widen it.
            if let (Some(s1), Some(s2)) = (send1, send2) {
                unify_bounded(bindings, ctx, true, *s1, *s2, program)?;
            }
            if let (Some(ret1), Some(ret2)) = (receive1, receive2) {
                unify_bounded(bindings, ctx, contra, *ret1, *ret2, program)?;
            }
            Ok(())
        }

        // Tuple types must match structurally
        (Type::Tuple(id1), Type::Tuple(id2)) => {
            if id1 == id2 {
                return Ok(());
            }

            let info1 = program
                .lookup_tuple(*id1)
                .ok_or(Error::TupleNotInRegistry { tuple_id: *id1 })?;
            let info2 = program
                .lookup_tuple(*id2)
                .ok_or(Error::TupleNotInRegistry { tuple_id: *id2 })?;

            // Names must match
            if info1.name != info2.name {
                return Err(Error::TypeUnresolved(format!(
                    "{} is not {}",
                    describe_tuple(&info2.name, info2.fields.len()),
                    describe_tuple(&info1.name, info1.fields.len()),
                )));
            }

            // Same number of fields
            if info1.fields.len() != info2.fields.len() {
                return Err(Error::TypeUnresolved(format!(
                    "{} is not {}",
                    describe_tuple(&info2.name, info2.fields.len()),
                    describe_tuple(&info1.name, info1.fields.len()),
                )));
            }

            // Unify each field, wrapping a failure with the field's name — nested
            // mismatches then read as a breadcrumb path to the offending leaf.
            let fields1 = info1.fields.clone();
            let fields2 = info2.fields.clone();
            for (index, ((fname1, ftype1_id), (fname2, ftype2_id))) in
                fields1.iter().zip(fields2.iter()).enumerate()
            {
                if fname1 != fname2 {
                    let describe = |name: &Option<String>| match name {
                        Some(name) => format!("field `{name}`"),
                        None => format!("positional field {index}"),
                    };
                    // A parameter's labels are adopted by a written call's positional
                    // argument, not by a value passed on from elsewhere.
                    let hint = if fname1.is_some() && fname2.is_none() {
                        " (positional fields take a parameter's labels only in a direct call)"
                    } else {
                        ""
                    };
                    return Err(Error::TypeUnresolved(format!(
                        "expected {} here, found {}{hint}",
                        describe(fname1),
                        describe(fname2),
                    )));
                }
                unify_bounded(bindings, ctx, contra, *ftype1_id, *ftype2_id, program).map_err(
                    |e| match e {
                        Error::TypeUnresolved(message) => {
                            let field = fname1.clone().unwrap_or_else(|| index.to_string());
                            Error::TypeUnresolved(format!("in `{field}`: {message}"))
                        }
                        other => other,
                    },
                )?;
            }

            Ok(())
        }

        // Partial type vs concrete tuple - check pattern fields exist in concrete
        (
            Type::Partial {
                name: pattern_name,
                fields: pattern_fields,
                rest: pattern_rest,
            },
            Type::Tuple(concrete_tuple_id),
        ) => {
            let concrete_info =
                program
                    .lookup_tuple(*concrete_tuple_id)
                    .ok_or(Error::TupleNotInRegistry {
                        tuple_id: *concrete_tuple_id,
                    })?;

            // If partial has a name, concrete must match
            if let Some(pname) = pattern_name
                && concrete_info.name.as_ref() != Some(pname)
            {
                return Err(Error::TypeUnresolved(format!(
                    "Partial type expects name {:?}, got {:?}",
                    pname, concrete_info.name
                )));
            }

            // Check all pattern fields exist in concrete with compatible types
            let concrete_fields = concrete_info.fields.clone();
            for (pattern_fname, pattern_ftype_id) in pattern_fields {
                let Some((_, concrete_ftype_id)) = concrete_fields
                    .iter()
                    .find(|(fname, _)| fname.as_ref() == Some(pattern_fname))
                else {
                    return Err(Error::TypeUnresolved(format!(
                        "Concrete type missing field: {}",
                        pattern_fname
                    )));
                };

                unify_bounded(
                    bindings,
                    ctx,
                    contra,
                    *pattern_ftype_id,
                    *concrete_ftype_id,
                    program,
                )?;
            }

            // The fields the partial does not list are its rest type's.
            if let Some(pattern_rest) = pattern_rest {
                for (fname, concrete_ftype_id) in &concrete_fields {
                    let listed = fname
                        .as_ref()
                        .is_some_and(|fname| pattern_fields.iter().any(|(name, _)| name == fname));
                    if !listed {
                        unify_bounded(
                            bindings,
                            ctx,
                            contra,
                            *pattern_rest,
                            *concrete_ftype_id,
                            program,
                        )?;
                    }
                }
            }

            Ok(())
        }

        // Partial type vs partial type - check pattern fields exist in concrete partial
        (
            Type::Partial {
                name: pattern_name,
                fields: pattern_fields,
                rest: pattern_rest,
            },
            Type::Partial {
                name: concrete_name,
                fields: concrete_fields,
                rest: concrete_rest,
            },
        ) => {
            // If partial has a name, concrete must match
            if let Some(pname) = pattern_name
                && concrete_name.as_ref() != Some(pname)
            {
                return Err(Error::TypeUnresolved(format!(
                    "Partial type expects name {:?}, got {:?}",
                    pname, concrete_name
                )));
            }

            // Check all pattern fields exist in concrete with compatible types
            for (pattern_fname, pattern_ftype_id) in pattern_fields {
                let Some((_, concrete_ftype_id)) = concrete_fields
                    .iter()
                    .find(|(fname, _)| fname == pattern_fname)
                else {
                    return Err(Error::TypeUnresolved(format!(
                        "Concrete partial missing field: {}",
                        pattern_fname
                    )));
                };

                unify_bounded(
                    bindings,
                    ctx,
                    contra,
                    *pattern_ftype_id,
                    *concrete_ftype_id,
                    program,
                )?;
            }

            // The pattern's rest type holds the concrete partial's other listed fields, and
            // its rest type, which it must therefore state.
            if let Some(pattern_rest) = pattern_rest {
                let Some(concrete_rest) = concrete_rest else {
                    return Err(Error::TypeUnresolved(
                        "Concrete partial states no rest type".to_string(),
                    ));
                };
                let unlisted = concrete_fields
                    .iter()
                    .filter(|(fname, _)| !pattern_fields.iter().any(|(name, _)| name == fname))
                    .map(|&(_, field)| field)
                    .chain(Some(*concrete_rest));
                for concrete_ftype_id in unlisted.collect::<Vec<_>>() {
                    unify_bounded(
                        bindings,
                        ctx,
                        contra,
                        *pattern_rest,
                        concrete_ftype_id,
                        program,
                    )?;
                }
            }

            Ok(())
        }

        // Function types - parameters are contravariant, results are covariant
        (
            Type::Callable {
                parameter: param1,
                result: result1,
                receive: receive1,
                states: states1,
                ..
            },
            Type::Callable {
                parameter: param2,
                result: result2,
                receive: receive2,
                states: states2,
                ..
            },
        ) => {
            // A generic function value is instantiated afresh wherever it is passed: two
            // uses of `id` in one argument are two instantiations, not one `'t` for both.
            let (param2, result2, receive2, states2) = if ctx.in_concrete_callable == 0
                && has_unbound_free_variables(bindings, concrete_id, program)
            {
                let fresh = instantiate_afresh(bindings, concrete_id, program);
                let Some(Type::Callable {
                    parameter,
                    result,
                    receive,
                    states,
                    ..
                }) = program.lookup_type(fresh).cloned()
                else {
                    unreachable!("renaming variables keeps a function type")
                };
                (parameter, result, receive, states)
            } else {
                (*param2, *result2, *receive2, *states2)
            };
            // A function type is a binder over every part.
            ctx.binders.enter(pattern_id);
            ctx.in_concrete_callable += 1;
            let parts = [
                Some((*param1, param2)),
                Some((*result1, result2)),
                Some((*receive1, receive2)),
                // States unify only when both are known: a `#'t -> 'u` parameter (states
                // unknown) accepts any literal without constraining its states.
                (*states1).zip(states2),
            ];
            let result = parts
                .into_iter()
                .flatten()
                .try_for_each(|(pattern_part, part)| {
                    unify_bounded(bindings, ctx, contra, pattern_part, part, program)
                });
            ctx.in_concrete_callable -= 1;
            ctx.binders.leave(pattern_id);
            result
        }

        // Handle cycles on either side - for recursive types like list<t>.
        // A `Cycle` is a back-reference to an enclosing μ-binder, i.e. a recursive
        // occurrence of the surrounding type. The two sides can legitimately be unrolled to
        // different depths (one shows the expanded union, the other still holds the cycle),
        // so a cycle unifies with whatever stands at the same position on the other side.
        // This is sound because the recursive structure is checked at every non-cyclic
        // position; the back-edge carries no additional constraint to verify here.
        //
        // A pattern's reference meeting a concrete type is the exception: it names a pattern
        // binder whose variables the concrete type must still bind (`Cons['t, ^]` against
        // `Cons[A, '%list<B>]` makes `'t` `A | B`, not `A`), so it unifies against that binder —
        // once per pair, which is what ends the recursion.
        (Type::Cycle(depth), _) if !matches!(concrete, Type::Cycle(_)) => {
            // Every binder a reference can name is on the stack: a function type's parts are
            // opened before they reach unification (`open_callable`), so none arrives free.
            let Some(root) = ctx.binders.resolve(*depth) else {
                return Err(Error::InternalError {
                    message: format!(
                        "unification met a free recursive reference in {}",
                        quiver_core::format::format_type_by_id(&*program, pattern_id)
                    ),
                });
            };
            if !ctx.visiting.insert((root, concrete_id)) {
                return Ok(());
            }
            let (root, cut) = ctx
                .binders
                .follow(*depth)
                .expect("the binder was just resolved");
            let result = unify_bounded(bindings, ctx, contra, root, concrete_id, program);
            ctx.binders.restore(cut);
            result
        }
        (Type::Cycle(_depth), _) | (_, Type::Cycle(_depth)) => Ok(()),

        // Never type (empty union) unifies with anything
        (Type::Union(variants), _) if variants.is_empty() => Ok(()),
        (_, Type::Union(variants)) if variants.is_empty() => Ok(()),

        // Union types - the argument's type (`concrete`) must be a subtype of the parameter's
        // type (`pattern`): every concrete variant has to unify with at least one pattern
        // variant, binding the pattern's type variables. The reverse — requiring every pattern
        // variant to appear in the argument — would wrongly reject a narrower argument (e.g. a
        // list builder that only ever returns `Cons` flowing where `Cons | Nil` is expected).
        (Type::Union(pattern_variants), Type::Union(concrete_variants)) => {
            if concrete_variants.is_empty() {
                return Ok(()); // the empty union (NEVER) is a subtype of anything
            }

            // Structural variants first, variable variants last: a bound variable member
            // accepts any concrete variant by widening, so trying it before the union's
            // own structural members would absorb variants they match exactly (the nil of
            // a fallible result `'t | []` widening `'t` to `T | []`). A variable member
            // only takes what no structural member matched.
            let (variable_variants, structural_variants): (Vec<usize>, Vec<usize>) =
                pattern_variants
                    .iter()
                    .copied()
                    .partition(|&v| matches!(program.lookup_type(v), Some(Type::Variable(_))));
            let pattern_variants: Vec<usize> = structural_variants
                .into_iter()
                .chain(variable_variants)
                .collect();
            let concrete_variants = concrete_variants.clone();

            for &concrete_variant in &concrete_variants {
                let mut found_match = false;
                for &pattern_variant in &pattern_variants {
                    let mut temp_bindings = bindings.clone();
                    if unify_bounded(
                        &mut temp_bindings,
                        &mut ctx.member_trial(pattern_id),
                        false,
                        pattern_variant,
                        concrete_variant,
                        program,
                    )
                    .is_ok()
                    {
                        *bindings =
                            merge_bindings(std::mem::take(bindings), temp_bindings, program);
                        found_match = true;
                        break;
                    }
                }
                if !found_match {
                    // Re-run the lone variant against the whole pattern union: the
                    // single-variant arm below diagnoses the failure (near-miss detail,
                    // compact shapes) far better than a canned line.
                    return unify_bounded(
                        bindings,
                        ctx,
                        contra,
                        pattern_id,
                        concrete_variant,
                        program,
                    );
                }
            }
            Ok(())
        }

        // Pattern union with concrete non-union - try each variant (structural members
        // before variable members, for the same reason as the union-union arm above)
        (Type::Union(variants), _) => {
            let (variable_variants, structural_variants): (Vec<usize>, Vec<usize>) = variants
                .iter()
                .copied()
                .partition(|&v| matches!(program.lookup_type(v), Some(Type::Variable(_))));
            let variants: Vec<usize> = structural_variants
                .into_iter()
                .chain(variable_variants)
                .collect();
            // Try to unify with at least one variant, remembering the *near miss* — the
            // variant the author plainly meant, whose inner failure explains the
            // mismatch far better than an every-variant dump (`Ev[Bogus]` against an
            // event union should say why the `Ev` member refused it). A variant sharing
            // the concrete tuple's name *and* field labels is the best witness; one
            // sharing just the name is kept as a fallback.
            let mut signature_miss: Option<Error> = None;
            let mut name_miss: Option<Error> = None;
            let concrete_signature = tuple_signature(program, concrete_id);
            for &variant in variants.iter() {
                let mut temp_bindings = bindings.clone();
                match unify_bounded(
                    &mut temp_bindings,
                    &mut ctx.member_trial(pattern_id),
                    false,
                    variant,
                    concrete_id,
                    program,
                ) {
                    Ok(()) => {
                        *bindings = temp_bindings;
                        return Ok(());
                    }
                    Err(e) => {
                        if let Some(signature) = &concrete_signature {
                            let variant_signature = tuple_signature(program, variant);
                            if signature_miss.is_none()
                                && variant_signature.as_ref() == Some(signature)
                            {
                                signature_miss = Some(e);
                            } else if name_miss.is_none()
                                && signature.0.is_some()
                                && variant_signature.is_some_and(|(name, _)| name == signature.0)
                            {
                                name_miss = Some(e);
                            }
                        }
                    }
                }
            }
            let near_miss = signature_miss.or(name_miss);
            // With a near miss the detail already locates the mismatch, so compact
            // shape descriptions suffice; without one the full formats are the message.
            let (found, expected) = if near_miss.is_some() {
                (
                    describe_shape(program, concrete_id),
                    describe_shape(program, pattern_id),
                )
            } else {
                (
                    quiver_core::format::format_type_by_id(&*program, concrete_id),
                    quiver_core::format::format_type_by_id(&*program, pattern_id),
                )
            };
            let detail = match near_miss {
                // Unwrap rather than Debug-format, so nested failures read as one
                // plain-text chain instead of escaping at every level.
                Some(Error::TypeUnresolved(message)) => format!(": {message}"),
                Some(other) => format!(": {other:?}"),
                None => String::new(),
            };
            Err(Error::TypeUnresolved(format!(
                "{found} does not fit {expected}{detail}"
            )))
        }

        // Concrete union with pattern non-union - pattern must match one variant
        // Every variant of the concrete union contributes to the pattern's type variables,
        // not just the first that fits: `['t, 'int]` unified against
        // `[A, 'int] | [B, 'int]` binds `'t` to `A | B`. Stopping at the first match bound
        // `'t` to one arm and dropped the rest, leaving an inferred type that excludes
        // values the expression demonstrably produces — and a match against one of the
        // dropped members then compiles to the wrong answer rather than to an error.
        //
        // A variant that does not unify is not itself a failure: a pattern may legitimately
        // describe only part of the union (`Some['t]` against `Some['int] | None`). Only a
        // union no variant of which fits is unresolvable.
        (_, Type::Union(variants)) => {
            let variants = variants.clone();
            let mut merged: Option<HashMap<String, usize>> = None;
            // A union unifies if *some* variant does: the pattern is being solved
            // for, and the variants that fit say what it is — which is how a `Cons['t, ^]`
            // parameter reads a `Nil | Cons['int, ^]` argument.
            //
            // A process pattern is the exception, because it is a *requirement* rather
            // than a shape to solve for: the value may turn out to be any variant, so
            // every one must meet it, and they are threaded through the same bindings
            // so that each narrows what the next may grant. That is what checks a send
            // to a union of pids against every member, and rejects one to a union that
            // is not all pids.
            if let Type::Process { .. } = pattern {
                for &variant in &variants {
                    unify_bounded(bindings, ctx, contra, pattern_id, variant, program)?;
                }
                return Ok(());
            }
            for &variant in &variants {
                let mut temp_bindings = bindings.clone();
                let mut temp_ctx = Unifier {
                    upper: ctx.upper.clone(),
                    ..ctx.fresh()
                };
                if unify_bounded(
                    &mut temp_bindings,
                    &mut temp_ctx,
                    contra,
                    pattern_id,
                    variant,
                    program,
                )
                .is_ok()
                {
                    ctx.upper = temp_ctx.upper;
                    merged = Some(match merged {
                        None => temp_bindings,
                        Some(accumulated) => merge_bindings(accumulated, temp_bindings, program),
                    });
                }
            }
            if let Some(merged) = merged {
                *bindings = merged;
                return Ok(());
            }
            Err(Error::TypeUnresolved(format!(
                "{} is not {}",
                quiver_core::format::format_type_by_id(&*program, concrete_id),
                quiver_core::format::format_type_by_id(&*program, pattern_id),
            )))
        }

        // All other combinations are incompatible
        _ => Err(Error::TypeUnresolved(format!(
            "{} is not {}",
            quiver_core::format::format_type_by_id(&*program, concrete_id),
            quiver_core::format::format_type_by_id(&*program, pattern_id),
        ))),
    }
}
