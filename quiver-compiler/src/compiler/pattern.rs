use std::collections::HashMap;

use crate::ast;
use quiver_core::{
    bytecode::{Constant, Instruction},
    program::Program,
    types::{Type, TypeLookup},
};

use super::{
    Error, codegen::InstructionBuilder, narrowing::intersect_types, type_queries,
    typing::union_type_ids,
};

// Type aliases for complex pattern matching types (using type IDs)
/// (bindings, binding sets, result type, success-narrowed type). The result type is the
/// narrowed type widened with nil when the match can fail — the match *term's* value domain.
/// The success-narrowed type is what the scrutinee is known to be when the pattern matched;
/// success-path narrowing and complement recording must use it, not the widened result — a
/// fallible `='int` covers `'int`, not `'int | []`, and recording the widened type would
/// subtract nil from subsequent branches when the branch fails.
type PatternAnalysisResult = (Vec<(String, usize)>, Vec<BindingSet>, usize, usize);
type TupleMatchResult = Vec<(usize, Vec<(usize, usize)>)>;
// Field info plus how to rebuild a variant's narrowed type: the variant's fields, the matched
// field indices, and the optional tuple name (`None` for a partial match, which keeps the input type).
type VariantFieldInfo<'a> = (
    Vec<(Option<String>, usize)>,
    &'a Vec<usize>,
    Option<Option<String>>,
);

/// Helper for managing identifiers across variants
/// Clones identifiers when processing multiple variants to avoid cross-contamination
enum IdentifierScope<'a> {
    /// Borrowed reference to parent identifiers (single variant case)
    Borrowed(&'a mut HashMap<String, Identifier>),
    /// Owned clone (multiple variants case)
    Owned(HashMap<String, Identifier>),
}

impl<'a> IdentifierScope<'a> {
    /// Create an appropriate scope based on whether we have multiple variants
    fn new(identifiers: &'a mut HashMap<String, Identifier>, has_multiple_variants: bool) -> Self {
        if has_multiple_variants {
            Self::Owned(identifiers.clone())
        } else {
            Self::Borrowed(identifiers)
        }
    }

    /// Get mutable access to the identifier map
    fn get_mut(&mut self) -> &mut HashMap<String, Identifier> {
        match self {
            Self::Borrowed(map) => map,
            Self::Owned(map) => map,
        }
    }
}

/// Represents a check that must be performed at runtime
#[derive(Debug, Clone)]
enum RuntimeCheck {
    TypeId(usize), // Type ID to check against
    Literal(ast::Literal),
    /// A pin (`&name`, `&name.field`, `&$x`): load the referenced value and compare. The
    /// access steps are resolved against the root's static type at analysis; the root itself
    /// is looked up again at codegen, like every variable reference.
    Pin {
        load: PinLoad,
        steps: Vec<Access>,
    },
    Path(AccessPath),
}

/// How a pin's root value is loaded at codegen.
#[derive(Debug, Clone)]
enum PinLoad {
    /// A variable, with the accessors it resolved under — non-empty exactly when the whole
    /// path resolved as a single pre-evaluated capture (in which case `steps` is empty).
    Variable(String, Vec<ast::AccessPath>),
    /// The enclosing function's parameter (`$`).
    Parameter,
}

/// A requirement that must be satisfied for a pattern to match
#[derive(Debug, Clone)]
struct Requirement {
    path: AccessPath,
    check: RuntimeCheck,
}

/// One step of an access path: a fixed position, or an interned field name resolved
/// against the value's own tuple id at runtime. Named steps arise from partial sources,
/// whose declared field order says nothing about the runtime layout.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Access {
    Position(usize),
    Named(usize),
}

/// Represents a path to access a value within a data structure
/// Empty vector means root, otherwise it's a sequence of field accesses
type AccessPath = Vec<Access>;

/// Emit the instruction for one access-path step.
fn emit_access(codegen: &mut InstructionBuilder, access: Access) {
    codegen.add_instruction(match access {
        Access::Position(index) => Instruction::get_positional(index),
        Access::Named(name) => Instruction::get_named(name),
    });
}

/// Resolve a pin target's accessors against the root's static type: how each step locates its
/// field at runtime, and the type of the accessed value (which narrows the scrutinee).
fn resolve_pin_steps(
    program: &mut Program,
    root_type_id: usize,
    accessors: &[ast::AccessPath],
    target_name: &str,
) -> Result<(Vec<Access>, usize), Error> {
    let mut current_type_id = root_type_id;
    let mut steps = Vec::with_capacity(accessors.len());
    for accessor in accessors {
        let (access, field_types) = match accessor {
            ast::AccessPath::Field(field_name) => {
                let (access, field_types) = type_queries::get_field_by_name(
                    program,
                    current_type_id,
                    field_name,
                    target_name,
                )?;
                let access = match access {
                    type_queries::FieldAccess::Position(index) => Access::Position(index),
                    type_queries::FieldAccess::Named { name, .. } => Access::Named(name),
                };
                (access, field_types)
            }
            ast::AccessPath::Index(index) => {
                let field_types = type_queries::get_field_at_index(
                    program,
                    current_type_id,
                    *index,
                    target_name,
                )?;
                (Access::Position(*index), field_types)
            }
            ast::AccessPath::Annotation(..) => unreachable!("pin targets have no annotation steps"),
        };
        steps.push(access);
        current_type_id = union_type_ids(program, field_types);
    }
    Ok((steps, current_type_id))
}

/// Tracks information about identifiers encountered during pattern analysis
#[derive(Debug, Clone)]
struct Identifier {
    first_path: AccessPath,
    is_repeated: bool,
}

/// Information about a variable binding
#[derive(Debug, Clone)]
struct Binding {
    name: String,
    path: AccessPath,
    var_type_id: usize,
}

/// A set of bindings that can be created if certain requirements are met
#[derive(Debug, Clone)]
pub struct BindingSet {
    requirements: Vec<Requirement>, // Requirements that must be satisfied
    bindings: Vec<Binding>,         // Variable bindings to create if requirements are met
}

/// Check if any binding set has requirements that prevent complement narrowing.
///
/// Returns true if the pattern cannot use complement narrowing because:
/// 1. It has value-based requirements (literals, variable pins, path equality) — their negation
///    is not a type, so the structural complement cannot represent it.
/// 2. It has a partial type check — a partial like `(x: A)` may fail because a field value
///    doesn't match, not because the tuple type is wrong.
///
/// Concrete type checks at *any* depth are fine: `compute_complement` is structural over tuple
/// fields, so a failed inner check soundly refines the outer type. (Constraints on *recursive*
/// fields are a separate concern — the narrowed type can't capture them — and are handled by
/// `narrowing::pattern_constrains_recursive_field` at the call site.)
///
/// For example:
/// - `=Node[x, y]`, `=Node[Leaf[x], _]` - safe, concrete type checks
/// - `=5` - NOT safe, literal check
/// - `=(x: A)` - NOT safe, partial type check (field value could fail)
pub fn prevents_complement_narrowing(binding_sets: &[BindingSet], program: &Program) -> bool {
    binding_sets.iter().any(|bs| {
        bs.requirements.iter().any(|req| {
            match &req.check {
                // Value-based checks prevent complement narrowing
                RuntimeCheck::Literal(_) | RuntimeCheck::Pin { .. } | RuntimeCheck::Path(_) => true,
                // Concrete type checks — at any depth — are fine: `compute_complement` is
                // structural over tuple fields (and sound on recursive types), so a failed inner
                // check soundly refines the outer type. Partial checks remain an exception, as
                // they constrain field compatibility rather than concrete identity.
                RuntimeCheck::TypeId(type_id) => {
                    matches!(program.lookup_type(*type_id), Some(Type::Partial { .. }))
                }
            }
        })
    })
}

/// Analyze pattern without generating code
/// Whether some binding set matches unconditionally (no runtime requirements) — an
/// irrefutable pattern, e.g. a bare binder.
pub fn is_irrefutable(binding_sets: &[BindingSet]) -> bool {
    binding_sets.iter().any(|set| set.requirements.is_empty())
}

pub fn analyze_pattern(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    pattern: &ast::Match,
    value_type_id: usize,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<PatternAnalysisResult, Error> {
    let mut identifiers = HashMap::new();
    let (binding_sets, narrowed_type_id) = analyze_match_pattern(
        env,
        program,
        pattern,
        value_type_id,
        vec![],
        &mut identifiers,
        scopes,
        value_provenance,
    )?;

    if binding_sets.is_empty() {
        // Won't match - return never type (empty union)
        let never = program.never();
        return Ok((Vec::new(), Vec::new(), never, never));
    }

    // Check if all binding sets have requirements (might match) or some have none (will match)
    let will_match = binding_sets.iter().any(|bs| bs.requirements.is_empty());

    // Collect all bindings and union their types across all binding sets
    let mut bindings_map: HashMap<String, Vec<usize>> = HashMap::new();
    for binding_set in &binding_sets {
        for binding in &binding_set.bindings {
            bindings_map
                .entry(binding.name.clone())
                .or_default()
                .push(binding.var_type_id);
        }
    }

    // Sort by name to ensure consistent ordering (must match generate_pattern_code) —
    // and sort BEFORE registering the union types: `union_type_ids` interns into the
    // program, so mapping during HashMap iteration would make type-id assignment
    // depend on hash order, breaking reproducible compilation.
    let mut all_bindings: Vec<(String, Vec<usize>)> = bindings_map.into_iter().collect();
    all_bindings.sort_by(|a, b| a.0.cmp(&b.0));
    let all_bindings: Vec<(String, usize)> = all_bindings
        .into_iter()
        .map(|(name, types)| (name, union_type_ids(program, types)))
        .collect();

    // Include [] in the result type if there are runtime requirements (might match)
    let result_type_id = if will_match {
        narrowed_type_id
    } else {
        let nil_id = program.register_type(Type::nil());
        union_type_ids(program, vec![nil_id, narrowed_type_id])
    };

    Ok((all_bindings, binding_sets, result_type_id, narrowed_type_id))
}

/// Generate bytecode for pattern matching
pub fn generate_pattern_code(
    codegen: &mut InstructionBuilder,
    program: &mut Program,
    scopes: &[super::scopes::Scope],
    binding_sets: &[BindingSet],
    fail_addr: usize,
) -> Result<(), Error> {
    let mut end_jumps = Vec::new();
    let mut next_set_jumps = Vec::new();

    for (i, binding_set) in binding_sets.iter().enumerate() {
        // Patch jumps from previous iteration that should skip to this binding set
        for jump in next_set_jumps.drain(..) {
            codegen.patch_jump_to_here(jump);
        }

        let is_last = i == binding_sets.len() - 1;

        // Check all requirements for this binding set
        for requirement in &binding_set.requirements {
            match &requirement.check {
                RuntimeCheck::Path(other_path) => {
                    codegen.add_instruction(Instruction::duplicate());
                    for &access in &requirement.path {
                        emit_access(codegen, access);
                    }
                    codegen.add_instruction(Instruction::pick(1));
                    for &access in other_path {
                        emit_access(codegen, access);
                    }
                    codegen.add_instruction(Instruction::equal());
                }
                RuntimeCheck::TypeId(type_id) => {
                    generate_value_access(codegen, &requirement.path);
                    codegen.add_instruction(Instruction::is_type(*type_id));
                }
                RuntimeCheck::Literal(literal) => {
                    generate_value_access(codegen, &requirement.path);
                    match literal {
                        ast::Literal::Integer(val) => {
                            let idx = program.register_constant(Constant::Integer(val.clone()));
                            codegen.add_instruction(Instruction::constant(idx));
                        }
                        ast::Literal::Binary(binary) => {
                            let idx = program
                                .register_constant(Constant::Binary(binary.bytes().to_vec()));
                            codegen.add_instruction(Instruction::constant(idx));
                        }
                    }
                    codegen.add_instruction(Instruction::equal());
                }
                RuntimeCheck::Pin { load, steps } => {
                    generate_value_access(codegen, &requirement.path);
                    let index = match load {
                        PinLoad::Variable(name, accessors) => {
                            super::scopes::lookup_variable(scopes, name, accessors)
                                .ok_or_else(|| Error::InternalError {
                                    message: format!("Pin variable '{}' not found in scope", name),
                                })?
                                .1
                        }
                        PinLoad::Parameter => super::scopes::get_function_parameter(scopes)?.1,
                    };
                    codegen.add_instruction(Instruction::load(index));
                    for &step in steps {
                        emit_access(codegen, step);
                    }
                    codegen.add_instruction(Instruction::equal());
                }
            }

            codegen.add_instruction(Instruction::not());
            if is_last {
                codegen.emit_jump_if_to_addr(fail_addr);
            } else {
                let skip = codegen.emit_jump_if_placeholder();
                next_set_jumps.push(skip);
            }
        }

        // If we get here, all checks passed - extract bindings
        // Sort by name to ensure consistent ordering across binding sets (important for unions
        // where different variants may have bindings in different field orders)
        let mut sorted_bindings: Vec<_> = binding_set.bindings.iter().collect();
        sorted_bindings.sort_by(|a, b| a.name.cmp(&b.name));
        for binding in sorted_bindings {
            generate_value_access(codegen, &binding.path);
            codegen.add_instruction(Instruction::store());
        }

        // Jump to end (unless this is the last set)
        if !is_last {
            let end_jump = codegen.emit_jump_placeholder();
            end_jumps.push(end_jump);
        }
    }

    // Patch any remaining next_set_jumps to fail
    for jump in next_set_jumps {
        codegen.patch_jump_to_addr(jump, fail_addr);
    }

    // Patch all end jumps to here
    for end_jump in end_jumps {
        codegen.patch_jump_to_here(end_jump);
    }

    Ok(())
}

fn generate_value_access(codegen: &mut InstructionBuilder, path: &AccessPath) {
    codegen.add_instruction(Instruction::duplicate());
    for &access in path {
        emit_access(codegen, access);
    }
}

#[allow(clippy::too_many_arguments)]
fn analyze_match_pattern(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    pattern: &ast::Match,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<(Vec<BindingSet>, usize), Error> {
    match pattern {
        ast::Match::Identifier(name, _) => {
            analyze_identifier_pattern(name.clone(), value_type_id, path, identifiers)
        }
        ast::Match::Literal(literal) => {
            analyze_literal_pattern(literal.clone(), path, value_type_id, program)
        }
        ast::Match::String(_, bytes) => {
            // A string pattern matches the desugared `Str[<bin>]` value; the delimiter style is
            // irrelevant to matching. Reuse the tuple-pattern machinery.
            let tuple = ast::MatchTuple {
                name: Some("Str".to_string()),
                fields: vec![ast::MatchField {
                    name: None,
                    pattern: ast::Match::Literal(ast::Literal::Binary(
                        ast::BinaryLiteral::ungrouped(bytes.clone()),
                    )),
                }],
            };
            analyze_match_tuple_pattern(
                env,
                program,
                &tuple,
                value_type_id,
                path,
                identifiers,
                scopes,
                value_provenance,
            )
        }
        ast::Match::Tuple(tuple) => analyze_match_tuple_pattern(
            env,
            program,
            tuple,
            value_type_id,
            path,
            identifiers,
            scopes,
            value_provenance,
        ),
        ast::Match::Partial(partial) => analyze_partial_pattern(
            env,
            program,
            partial,
            value_type_id,
            path,
            identifiers,
            scopes,
            value_provenance,
        ),
        ast::Match::Or(alternatives) => analyze_or_pattern(
            env,
            program,
            alternatives,
            value_type_id,
            path,
            identifiers,
            scopes,
            value_provenance,
        ),
        ast::Match::As(head, name, _) => {
            // Ascribed binding `(P)x`: match the head, then bind `name` to the whole value at the
            // type the head narrowed it to — so `('int)x` binds `x: 'int` and `(0 | 1)x` binds `x`
            // at the two literals' union. The head is analysed like any other pattern, so an
            // alternation contributes one binding set per alternative and the binder joins each.
            let (mut sets, narrowed_type_id) = analyze_match_pattern(
                env,
                program,
                head,
                value_type_id,
                path.clone(),
                identifiers,
                scopes,
                value_provenance,
            )?;
            match identifiers.get_mut(name) {
                // A repeated binder (`=[('int)x, x]`) becomes a runtime equality check against the
                // first occurrence, exactly like a repeated plain identifier.
                Some(info) => {
                    info.is_repeated = true;
                    let requirement = Requirement {
                        path: info.first_path.clone(),
                        check: RuntimeCheck::Path(path.clone()),
                    };
                    for set in &mut sets {
                        set.requirements.push(requirement.clone());
                    }
                }
                None => {
                    identifiers.insert(
                        name.clone(),
                        Identifier {
                            first_path: path.clone(),
                            is_repeated: false,
                        },
                    );
                    for set in &mut sets {
                        set.bindings.push(Binding {
                            name: name.clone(),
                            path: path.clone(),
                            var_type_id: narrowed_type_id,
                        });
                    }
                }
            }
            Ok((sets, narrowed_type_id))
        }
        ast::Match::Star(name) => {
            analyze_star_pattern(program, name.as_ref(), value_type_id, path, identifiers)
        }
        ast::Match::Placeholder => Ok((
            vec![BindingSet {
                requirements: vec![],
                bindings: vec![],
            }],
            value_type_id,
        )),
        ast::Match::Pin(target) => {
            // Pin pattern `&name` / `&name.field` / `&$x`: check the value equals the referenced
            // value at runtime. A variable root must reference a binding already in scope — if it
            // isn't found it's undefined, e.g. a name bound by a *sibling* sub-pattern of the same
            // compound pattern (`=[x, &x]`), which isn't visible yet.
            let accessors = &target.accessors;
            let (load, steps, pinned_type_id) = match &target.root {
                ast::PinRoot::Variable(name) => {
                    // A captured access path materialises as a single pre-evaluated local;
                    // prefer it, exactly as expression accesses do (`compile_member_access`).
                    if !accessors.is_empty()
                        && let Some((capture_type_id, _)) =
                            super::scopes::lookup_variable(scopes, name, accessors)
                    {
                        (
                            PinLoad::Variable(name.clone(), accessors.clone()),
                            vec![],
                            capture_type_id,
                        )
                    } else {
                        let Some((base_type_id, _)) =
                            super::scopes::lookup_variable(scopes, name, &[])
                        else {
                            return Err(Error::VariableUndefined(name.clone()));
                        };
                        let (steps, accessed_type_id) =
                            resolve_pin_steps(program, base_type_id, accessors, name)?;
                        (
                            PinLoad::Variable(name.clone(), vec![]),
                            steps,
                            accessed_type_id,
                        )
                    }
                }
                ast::PinRoot::Parameter { depth: 0 } => {
                    let (param_type_id, _) = super::scopes::get_function_parameter(scopes)?;
                    let (steps, accessed_type_id) =
                        resolve_pin_steps(program, param_type_id, accessors, "$")?;
                    (PinLoad::Parameter, steps, accessed_type_id)
                }
                ast::PinRoot::Parameter { depth } => {
                    // `&$$x`: an outer parameter is this function's capture — a local named
                    // by the sigil run — so the pin resolves like a variable-rooted one.
                    let name = super::variables::CaptureSource::OuterParameter(*depth).scope_name();
                    if let Some((capture_type_id, _)) =
                        super::scopes::lookup_variable(scopes, &name, accessors)
                    {
                        (
                            PinLoad::Variable(name, accessors.clone()),
                            vec![],
                            capture_type_id,
                        )
                    } else if let Some((base_type_id, _)) =
                        super::scopes::lookup_variable(scopes, &name, &[])
                    {
                        let (steps, accessed_type_id) =
                            resolve_pin_steps(program, base_type_id, accessors, &name)?;
                        (PinLoad::Variable(name, vec![]), steps, accessed_type_id)
                    } else {
                        // Only reachable outside any enclosing literal (module top level):
                        // within one, the collector recorded the capture or errored earlier.
                        return Err(Error::ParameterDepthExceeded {
                            written: ast::parameter_sigils(*depth),
                        });
                    }
                }
            };

            let requirements = vec![Requirement {
                path,
                check: RuntimeCheck::Pin { load, steps },
            }];

            // Narrow the type by intersecting with the pinned value's type.
            let narrowed_type_id = intersect_types(value_type_id, pinned_type_id, program);

            Ok((
                vec![BindingSet {
                    requirements,
                    bindings: vec![],
                }],
                narrowed_type_id,
            ))
        }
        ast::Match::Type(ast_type) => {
            // A type assertion, e.g. `='int` or an intersection `=('t & 'u)`. Each intersection
            // member is checked separately (see `type_check_requirements`); the narrowed type
            // folds them all in.
            let (requirements, narrowed_type_id) =
                type_check_requirements(env, program, scopes, ast_type, value_type_id, &path)?;
            Ok((
                vec![BindingSet {
                    requirements,
                    bindings: vec![],
                }],
                narrowed_type_id,
            ))
        }
    }
}

/// Compute the runtime type-check requirements and narrowed type for a `Type` match — including a
/// `&`-intersection. Each intersection member is checked separately (an exact `IsType` per
/// member), so partial-type constraints compose soundly: `intersect_types` widens for partials, so
/// a single folded `IsType` would be unsound. The narrowed type folds every member in. A member
/// already implied by the (accumulated) value type adds no runtime check.
fn type_check_requirements(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    scopes: &[super::scopes::Scope],
    ast_type: &ast::Type,
    value_type_id: usize,
    path: &AccessPath,
) -> Result<(Vec<Requirement>, usize), Error> {
    let members: Vec<&ast::Type> = match ast_type {
        ast::Type::Intersection(members) => members.iter().collect(),
        other => vec![other],
    };
    let mut requirements = Vec::new();
    let mut narrowed = value_type_id;
    for member in members {
        let resolved = super::typing::resolve_ast_type(env, scopes, member.clone(), program)?;
        let next = intersect_types(narrowed, resolved, program);
        // Elide the runtime check only when the scrutinee *provably* fits: identical
        // ids always do, and otherwise `is_compatible` can vouch only for cycle-free
        // operands — it traverses a `Cycle` optimistically, so trusting it on a
        // recursive scrutinee elided load-bearing checks (an `=(I['int])s` ascription
        // on a `(^ | Lb['int])`-typed binding matched an `Lb`). The emitted `IsType`'s
        // runtime set is computed with the full cycle-aware machinery, so keeping the
        // check is exact, merely occasionally redundant.
        let provable = narrowed == resolved
            || (!super::narrowing::has_cycles(narrowed, program)
                && !super::narrowing::has_cycles(resolved, program)
                && is_compatible(narrowed, resolved, program)
                && next == narrowed);
        if !provable {
            requirements.push(Requirement {
                path: path.clone(),
                check: RuntimeCheck::TypeId(resolved),
            });
        }
        narrowed = next;
    }
    Ok((requirements, narrowed))
}

/// Analyze an alternation pattern `(p | q | …)`. Each alternative is analyzed independently and
/// its binding sets are pooled: at runtime `generate_pattern_code` tries each in turn, so the
/// pattern matches if any alternative does. The narrowed type is the union of the alternatives'.
///
/// Every alternative must bind the same set of variables, so the body sees them whichever one
/// matched. Each alternative is analyzed in its own identifier scope, so a name bound in two
/// alternatives is one binding (the alternatives are mutually exclusive) rather than a repeated
/// identifier (an equality check).
#[allow(clippy::too_many_arguments)]
fn analyze_or_pattern(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    alternatives: &[ast::Match],
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<(Vec<BindingSet>, usize), Error> {
    let mut pooled_sets = vec![];
    let mut narrowed_ids = vec![];
    let mut expected_names: Option<Vec<String>> = None;

    for alternative in alternatives {
        let mut alt_identifiers = identifiers.clone();
        let (sets, narrowed) = analyze_match_pattern(
            env,
            program,
            alternative,
            value_type_id,
            path.clone(),
            &mut alt_identifiers,
            scopes,
            value_provenance,
        )?;

        // An alternative that produces no binding sets is statically dead for this value type
        // (e.g. `A[x]` against a `B` value): it can never match at runtime, so it neither
        // contributes bindings nor participates in the same-variables check.
        if sets.is_empty() {
            continue;
        }

        let mut names: Vec<String> = sets
            .iter()
            .flat_map(|set| set.bindings.iter().map(|binding| binding.name.clone()))
            .collect();
        names.sort();
        names.dedup();
        match &expected_names {
            None => expected_names = Some(names),
            Some(expected) if *expected != names => {
                return Err(Error::OrPatternBindingMismatch {
                    expected: expected.clone(),
                    found: names,
                });
            }
            Some(_) => {}
        }

        pooled_sets.extend(sets);
        narrowed_ids.push(narrowed);
    }

    let narrowed_type_id = union_type_ids(program, narrowed_ids);
    Ok((pooled_sets, narrowed_type_id))
}

#[allow(clippy::too_many_arguments)]
fn analyze_match_tuple_pattern(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    tuple: &ast::MatchTuple,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<(Vec<BindingSet>, usize), Error> {
    let mut binding_sets = vec![];
    let mut successful_tuple_ids = vec![];

    // Find matching tuple types
    let matching_types = find_matching_match_tuples(program, tuple, value_type_id)?;

    // Each member's runtime tuple test, when one is needed: the *declared* scrutinee's
    // same-shaped member where one exists, since a complement-narrowed member's field types
    // would wrongly reject a value whose tuple id carries the declared (wider) field type (see
    // `declared_shape_witness`), and the member itself otherwise.
    let needs_tuple_check = is_union(value_type_id, program) || matching_types.len() > 1;
    let mut witnesses = Vec::new();
    for (tuple_id, _) in &matching_types {
        let witness = if needs_tuple_check {
            super::narrowing::declared_shape_witness(scopes, value_provenance, *tuple_id, program)
                .unwrap_or(*tuple_id)
        } else {
            *tuple_id
        };
        witnesses.push(witness);
    }

    // For each matching type, create binding sets
    for (member, (tuple_id, field_mappings)) in matching_types.iter().enumerate() {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        // For a single variant, use the parent's identifiers directly
        let mut variant_identifiers_scope =
            IdentifierScope::new(identifiers, matching_types.len() > 1);
        let variant_identifiers = variant_identifiers_scope.get_mut();

        // Tuple name and field defs upfront. `narrowed_fields` starts as the static defs and is
        // refined per field as sub-patterns narrow them, so the reconstructed narrowed type
        // carries field-level precision (e.g. `[True, True]`) instead of the opaque tuple — which
        // is what lets `compute_complement` reason about field combinations.
        let (tuple_name, mut narrowed_fields): (Option<String>, Vec<(Option<String>, usize)>) = {
            let tuple_info = program
                .lookup_tuple(*tuple_id)
                .ok_or(Error::TupleNotInRegistry {
                    tuple_id: *tuple_id,
                })?;
            (tuple_info.name.clone(), tuple_info.fields.clone())
        };
        let tuple_fields: Vec<usize> = narrowed_fields
            .iter()
            .map(|(_, type_id)| *type_id)
            .collect();

        // Start with a binding set for this type
        let mut base_requirements = vec![];
        // The field types the sub-patterns are checked against. A test against the declared
        // shape passes a value of *any* member sharing that shape — `[E, P | E]` and
        // `[P | E, E]`, the complement of `[P, P]`, are indistinguishable by it — so each field
        // must be checked as that whole group types it, not as this member alone does.
        let check_tuple_id = witnesses[member];
        let siblings: Vec<usize> = matching_types
            .iter()
            .zip(&witnesses)
            .filter(|((other, _), witness)| **witness == check_tuple_id && other != tuple_id)
            .map(|((other, _), _)| *other)
            .collect();
        let check_fields: Option<Vec<usize>> = if siblings.is_empty() {
            None
        } else {
            let mut fields = tuple_fields.clone();
            for sibling in siblings {
                let sibling_fields: Vec<usize> = program
                    .lookup_tuple(sibling)
                    .map(|info| info.fields.iter().map(|(_, t)| *t).collect())
                    .unwrap_or_default();
                for (field, other) in fields.iter_mut().zip(sibling_fields) {
                    *field = union_type_ids(program, vec![*field, other]);
                }
            }
            Some(fields)
        };
        // Add runtime check if needed
        // We need a runtime check if value_type is a union (even if it contains only one tuple type)
        // because the value could be a non-tuple type (like int or bin)
        if needs_tuple_check {
            let tuple_type_id = program.register_type(Type::Tuple(check_tuple_id));
            base_requirements.push(Requirement {
                path: path.clone(),
                check: RuntimeCheck::TypeId(tuple_type_id),
            });
        }

        let mut current_binding_sets = vec![BindingSet {
            requirements: base_requirements,
            bindings: vec![],
        }];

        // Process each field pattern
        for (pattern_idx, actual_idx) in field_mappings {
            let field = &tuple.fields[*pattern_idx];
            let member_field_type_id = tuple_fields[*actual_idx];
            let raw_field_type_id = check_fields
                .as_ref()
                .map_or(member_field_type_id, |fields| fields[*actual_idx]);

            // Close the field type's `Cycle` references against the scrutinee boundary,
            // so a binding (or sub-pattern) taken from a recursive position carries a
            // self-contained type: `Cycle(1)` is the scrutinee's own union, and deeper
            // references *through* a nested union field (e.g. a list element's `^`
            // reaching the enclosing definition's root) close one boundary further out.
            // A dangling cycle would make every later match on the binding statically
            // dead — the recorded inner-recursive-union bug. Cycles beyond the known
            // context (enclosing function boundaries) are kept as-is.
            //
            // Resolve against the scrutinee's *declared* type rather than `value_type_id`: the
            // field's type is the recursion boundary fixed by the type definition, but
            // `value_type_id` may have been complement-narrowed (dropping sibling variants such as
            // `Nil` after a `=Nil` branch). Using the narrowed view would dangle this cycle and
            // wrongly exclude those variants from the field — e.g. a `Cons` tail losing `Nil`, so
            // `=Cons[h, Nil]` could never match. Fall back to `value_type_id` when the declared
            // type is unavailable (e.g. unknown provenance).
            // Only the top-level scrutinee (`path` empty) can be complement-narrowed; at nested
            // depths `value_type_id` is the already-resolved field type, which is the correct
            // boundary. So consult the declared type only at the top level.
            let boundary = if path.is_empty() {
                super::narrowing::get_declared_type_for_provenance(
                    scopes,
                    value_provenance,
                    program,
                )
                .unwrap_or(value_type_id)
            } else {
                value_type_id
            };
            let mut field_type_id =
                super::narrowing::close_cycles(raw_field_type_id, boundary, program);

            let mut field_path = path.clone();
            field_path.push(Access::Position(*actual_idx));

            // Check for narrowed field type from complement narrowing.
            // This enables patterns like `=[Cons[...], ys]` in the second branch to know
            // that field 0 has been narrowed to Cons (from previous branch's `=[Nil, ys]`).
            // Only applies when path is empty (we're at the root).
            if path.is_empty()
                && let Some(narrowed_id) =
                    super::narrowing::get_field_narrowing(scopes, value_provenance, *actual_idx)
            {
                // Intersect with the narrowed type (which re-roots a recursive field's kept
                // variants, so `Nil | Cons['t, ^]` narrowed to `Cons` still ends in `Nil`).
                field_type_id = intersect_types(field_type_id, narrowed_id, program);
            }

            // Recursively analyze the field pattern
            let field_provenance = value_provenance.field(*actual_idx);
            let (field_binding_sets, field_narrowed_type_id) = analyze_match_pattern(
                env,
                program,
                &field.pattern,
                field_type_id,
                field_path,
                variant_identifiers,
                scopes,
                &field_provenance,
            )?;

            if field_binding_sets.is_empty() {
                // This type variant can't match, skip it entirely
                current_binding_sets.clear();
                break;
            }

            // Record the field's narrowed type for the reconstructed tuple. Cycle-bearing
            // fields keep their original reference rather than the resolved (closed)
            // type: the reconstructed type feeds coverage/complement computation, which
            // must recognize the branch as covering the original member — a closed
            // field would read as a *different* recursive type and break exhaustiveness
            // (and a direct `Cycle(1)` would materialize an infinite type). A narrowing that
            // leaves no cycle behind (`('int)a` against a union with a recursive member) is
            // exact on its own, and is kept.
            if !super::narrowing::has_cycles(raw_field_type_id, program)
                || !super::narrowing::has_cycles(field_narrowed_type_id, program)
            {
                narrowed_fields[*actual_idx].1 = if check_fields.is_some() {
                    // Checked against the declared shape; this member's own field type is
                    // still what a value reaching this binding set as *this* member holds.
                    intersect_types(field_narrowed_type_id, member_field_type_id, program)
                } else {
                    field_narrowed_type_id
                };
            }

            // Combine field binding sets with current binding sets (cartesian product)
            let mut new_binding_sets = vec![];
            for current_set in &current_binding_sets {
                for field_set in &field_binding_sets {
                    let mut combined_requirements = current_set.requirements.clone();
                    combined_requirements.extend(field_set.requirements.clone());

                    let mut combined_bindings = current_set.bindings.clone();
                    combined_bindings.extend(field_set.bindings.clone());

                    new_binding_sets.push(BindingSet {
                        requirements: combined_requirements,
                        bindings: combined_bindings,
                    });
                }
            }
            current_binding_sets = new_binding_sets;
        }

        // Track this tuple as successful if it produced binding sets, reconstructing it with the
        // narrowed field types so the narrowed result carries field-level precision.
        if !current_binding_sets.is_empty() {
            let narrowed_tuple_id = program.register_tuple(tuple_name, narrowed_fields);
            successful_tuple_ids.push(program.register_type(Type::Tuple(narrowed_tuple_id)));
            binding_sets.extend(current_binding_sets);
        }
    }

    if is_never(value_type_id, program) {
        return Err(Error::InternalError {
            message: format!(
                "analyze_assignment_tuple_pattern received empty type for tuple: {:?}",
                tuple
            ),
        });
    }

    // `successful_tuple_ids` already holds reconstructed `Type::Tuple` ids (narrowed per field).
    let narrowed_type_id = if successful_tuple_ids.is_empty() {
        program.never()
    } else {
        union_type_ids(program, successful_tuple_ids)
    };

    Ok((binding_sets, narrowed_type_id))
}

fn find_matching_match_tuples(
    program: &mut Program,
    tuple: &ast::MatchTuple,
    value_type_id: usize,
) -> Result<TupleMatchResult, Error> {
    let mut matching_types = Vec::new();

    let tuple_ids = extract_tuple_ids(program, value_type_id);

    for tuple_id in tuple_ids {
        if let Some(field_mappings) = check_match_tuple_match(program, tuple, tuple_id)? {
            matching_types.push((tuple_id, field_mappings));
        }
    }

    Ok(matching_types)
}

fn check_match_tuple_match(
    program: &mut Program,
    tuple: &ast::MatchTuple,
    tuple_id: usize,
) -> Result<Option<Vec<(usize, usize)>>, Error> {
    let tuple_info = program
        .lookup_tuple(tuple_id)
        .ok_or(Error::TupleNotInRegistry { tuple_id })?;

    // Names must correspond exactly: a stated tuple name requires that name, and an
    // unnamed pattern requires an unnamed value — destructuring a named tuple without
    // stating its name is a partial or star pattern's job (`(x, y)`, `*`).
    let name_compatible = match &tuple.name {
        Some(name) => tuple_info.name.as_ref() == Some(name),
        None => tuple_info.name.is_none(),
    };
    if !name_compatible || tuple.fields.len() != tuple_info.fields.len() {
        return Ok(None);
    }

    let mut field_mappings = Vec::new();
    for (pattern_idx, field) in tuple.fields.iter().enumerate() {
        let tuple_field = &tuple_info.fields[pattern_idx];
        // Labels must correspond exactly: an unlabeled pattern field matches only an
        // unlabeled value field, and a stated label only the same label.
        if field.name.as_ref() != tuple_field.0.as_ref() {
            return Ok(None);
        }

        field_mappings.push((pattern_idx, pattern_idx));
    }

    Ok(Some(field_mappings))
}

fn analyze_literal_pattern(
    literal: ast::Literal,
    path: AccessPath,
    value_type_id: usize,
    program: &mut Program,
) -> Result<(Vec<BindingSet>, usize), Error> {
    // Determine the type of the literal
    let literal_type_id = match &literal {
        ast::Literal::Integer(_) => program.register_type(Type::Integer),
        ast::Literal::Binary(_) => program.register_type(Type::Binary),
    };

    // Narrow the type by filtering compatible variants
    let narrowed_type_id = intersect_types(value_type_id, literal_type_id, program);

    Ok((
        vec![BindingSet {
            requirements: vec![Requirement {
                path,
                check: RuntimeCheck::Literal(literal),
            }],
            bindings: vec![],
        }],
        narrowed_type_id,
    ))
}

fn analyze_identifier_pattern(
    name: String,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
) -> Result<(Vec<BindingSet>, usize), Error> {
    // Check if we've seen this identifier before
    if let Some(info) = identifiers.get_mut(&name) {
        // Second or later occurrence - mark as repeated and create Path requirement
        // No type narrowing for repeated identifiers (equality check)
        info.is_repeated = true;
        Ok((
            vec![BindingSet {
                requirements: vec![Requirement {
                    path: info.first_path.clone(),
                    check: RuntimeCheck::Path(path),
                }],
                bindings: vec![],
            }],
            value_type_id,
        ))
    } else {
        // First occurrence - record it and create binding
        identifiers.insert(
            name.clone(),
            Identifier {
                first_path: path.clone(),
                is_repeated: false,
            },
        );

        // Binding gets the full value type, including nil if present.
        // The binding executes regardless of whether the value is nil.
        let var_type_id = value_type_id;
        let narrowed_type_id = var_type_id;

        // No runtime requirements for identifier bindings - they match any value
        Ok((
            vec![BindingSet {
                requirements: vec![],
                bindings: vec![Binding {
                    name,
                    path,
                    var_type_id,
                }],
            }],
            narrowed_type_id,
        ))
    }
}

#[allow(clippy::too_many_arguments)]
fn analyze_partial_pattern(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    partial_pattern: &ast::PartialPattern,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<(Vec<BindingSet>, usize), Error> {
    let mut binding_sets = vec![];

    // Extract field names from the partial pattern fields
    let field_names: Vec<String> = partial_pattern
        .fields
        .iter()
        .map(|f| f.name.clone())
        .collect();

    // Find types that have all required fields (and optionally matching type name)
    let matching_types = find_types_with_fields_and_name(
        program,
        &field_names,
        partial_pattern.name.as_ref(),
        value_type_id,
    )?;

    // Check if the original value type could be multiple types (for adding type checks).
    // Members that contribute no field source at all (primitives, callables — and they
    // can reach here through a union) still make the check necessary: without it the
    // field fetch below would run unguarded against them.
    let value_type_sources = extract_field_sources(program, value_type_id);
    let needs_type_check =
        value_type_sources.len() > 1 || has_sourceless_members(program, value_type_id);

    // Narrowed type accumulated per matchable variant, reconstructed with field-level precision so
    // a later branch's complement reflects the field check (e.g. `mode: R | A` after `=(mode: W)`).
    let mut narrowed_type_ids: Vec<usize> = Vec::new();

    // Create a binding set for each matching type
    for field_match in &matching_types {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        let mut variant_identifiers_scope =
            IdentifierScope::new(identifiers, matching_types.len() > 1);
        let variant_identifiers = variant_identifiers_scope.get_mut();

        let mut requirements = vec![];

        // Field info plus how to rebuild this variant's narrowed type once its fields are refined:
        // a concrete tuple match reconstructs `Type::Tuple` with the narrowed fields; a partial
        // match (`Some` name vs `None`) keeps the input type.
        let (mut fields, field_indices, narrowed_tuple_name): VariantFieldInfo = match field_match {
            FieldMatch::Tuple {
                tuple_id,
                field_indices,
            } => {
                // Add type check if the value could be multiple types. As in the tuple
                // pattern above, a complement-narrowed member's id would wrongly reject
                // values carrying the declared field types, so the declared same-shaped
                // member is the test's witness where one exists.
                if needs_type_check {
                    let check_tuple_id = super::narrowing::declared_shape_witness(
                        scopes,
                        value_provenance,
                        *tuple_id,
                        program,
                    )
                    .unwrap_or(*tuple_id);
                    let tuple_type_id = program.register_type(Type::Tuple(check_tuple_id));
                    requirements.push(Requirement {
                        path: path.clone(),
                        check: RuntimeCheck::TypeId(tuple_type_id),
                    });
                }

                let tuple_info =
                    program
                        .lookup_tuple(*tuple_id)
                        .ok_or(Error::TupleNotInRegistry {
                            tuple_id: *tuple_id,
                        })?;
                (
                    tuple_info.fields.clone(),
                    field_indices,
                    Some(tuple_info.name.clone()),
                )
            }
            FieldMatch::Partial {
                name,
                fields,
                field_indices,
            } => {
                // A partial source is a structural constraint, not a concrete type — but
                // when the value could also be *other* members (e.g. the nil of a
                // `shape | []` union), the constraint itself is the runtime test;
                // without it the field fetch below would run unguarded against them.
                if needs_type_check {
                    let partial_type_id = program.register_type(Type::Partial {
                        name: name.clone(),
                        fields: fields.clone(),
                    });
                    requirements.push(Requirement {
                        path: path.clone(),
                        check: RuntimeCheck::TypeId(partial_type_id),
                    });
                }
                // Convert partial fields to (Option<String>, usize) format
                let converted: Vec<(Option<String>, usize)> = fields
                    .iter()
                    .map(|(name, type_id)| (Some(name.clone()), *type_id))
                    .collect();
                (converted, field_indices, None)
            }
        };

        // Seed with the base requirements (the type check, if any). Each field then refines these
        // sets: a field whose nested pattern yields several binding sets (e.g. an alternation) fans
        // out via a cartesian product, and a field whose nested pattern can never match collapses
        // the variant to no binding sets. The latter is what makes `=(mode: W)` correctly fail
        // against a field typed `A`, rather than silently matching because the field check was
        // dropped on the floor.
        let mut current_binding_sets = vec![BindingSet {
            requirements,
            bindings: vec![],
        }];
        let mut unmatchable = false;

        for (i, partial_field) in partial_pattern.fields.iter().enumerate() {
            let field_name = &partial_field.name;
            let nested_pattern = &partial_field.pattern;
            let idx = field_indices[i];
            let field_type_id = fields[idx].1;
            let mut field_path = path.clone();
            // A concrete variant pins the field's position (gated by its type check); a
            // partial source's layout is unknown, so the field is fetched by name.
            field_path.push(match narrowed_tuple_name {
                Some(_) => Access::Position(idx),
                None => Access::Named(program.register_field_name(field_name)),
            });

            // If the field has a nested pattern, analyze it recursively and combine.
            if let Some(nested_pattern) = nested_pattern {
                let field_provenance = value_provenance.field(idx);
                let (field_binding_sets, field_narrowed_type_id) = analyze_match_pattern(
                    env,
                    program,
                    nested_pattern,
                    field_type_id,
                    field_path,
                    variant_identifiers,
                    scopes,
                    &field_provenance,
                )?;

                if field_binding_sets.is_empty() {
                    // The nested pattern can never match this field's type, so this variant can't
                    // match at all.
                    current_binding_sets.clear();
                    unmatchable = true;
                    break;
                }

                // Record the field's narrowed type for the reconstructed variant. Leave
                // cycle-bearing fields alone: the reconstruction feeds coverage/
                // complement computation, which must recognize the original member
                // (and a direct `Cycle(1)` would materialize an infinite type).
                if !super::narrowing::has_cycles(field_type_id, program) {
                    fields[idx].1 = field_narrowed_type_id;
                }

                let mut combined = Vec::new();
                for current in &current_binding_sets {
                    for field_set in &field_binding_sets {
                        let mut requirements = current.requirements.clone();
                        requirements.extend(field_set.requirements.clone());
                        let mut bindings = current.bindings.clone();
                        bindings.extend(field_set.bindings.clone());
                        combined.push(BindingSet {
                            requirements,
                            bindings,
                        });
                    }
                }
                current_binding_sets = combined;
            } else {
                // No nested pattern - bind the field by name (or, if repeated, equality-check it).
                let (extra_requirement, extra_binding) =
                    if let Some(info) = variant_identifiers.get_mut(field_name) {
                        // Repeated identifier - mark as repeated and add a Path equality check.
                        info.is_repeated = true;
                        (
                            Some(Requirement {
                                path: info.first_path.clone(),
                                check: RuntimeCheck::Path(field_path),
                            }),
                            None,
                        )
                    } else {
                        // First occurrence - record it and create a binding. Strip [] because if
                        // the binding executes, the value is not [].
                        variant_identifiers.insert(
                            field_name.clone(),
                            Identifier {
                                first_path: field_path.clone(),
                                is_repeated: false,
                            },
                        );
                        let var_type_id = without_nil(field_type_id, program);
                        (
                            None,
                            Some(Binding {
                                name: field_name.clone(),
                                path: field_path,
                                var_type_id,
                            }),
                        )
                    };

                for set in &mut current_binding_sets {
                    if let Some(requirement) = &extra_requirement {
                        set.requirements.push(requirement.clone());
                    }
                    if let Some(binding) = &extra_binding {
                        set.bindings.push(binding.clone());
                    }
                }
            }
        }

        if unmatchable {
            continue;
        }

        binding_sets.extend(current_binding_sets);

        // Reconstruct this variant's narrowed type from its (possibly refined) fields.
        match narrowed_tuple_name {
            Some(name) => {
                let narrowed_tuple_id = program.register_tuple(name, fields);
                narrowed_type_ids.push(program.register_type(Type::Tuple(narrowed_tuple_id)));
            }
            None => narrowed_type_ids.push(value_type_id),
        }
    }

    // For tuples, narrow to the reconstructed (field-refined) tuple types; for partials, the input
    // type. No matchable variant means the pattern can't match (never).
    let narrowed_type_id = if narrowed_type_ids.is_empty() {
        program.never()
    } else {
        union_type_ids(program, narrowed_type_ids)
    };

    Ok((binding_sets, narrowed_type_id))
}

fn analyze_star_pattern(
    program: &mut Program,
    name: Option<&String>,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
) -> Result<(Vec<BindingSet>, usize), Error> {
    // Collect all field sources (tuples and partials)
    let all_sources = extract_field_sources(program, value_type_id);

    // When a name is given, keep only the variants carrying that tuple name (`Config*`).
    let field_sources: Vec<FieldSource> = match name {
        None => all_sources.clone(),
        Some(expected) => all_sources
            .iter()
            .filter(|source| field_source_name(program, source).as_ref() == Some(expected))
            .cloned()
            .collect(),
    };

    // If the value could be one of several variants at runtime, a type check is needed to
    // discriminate the matching variant (and, for a named star, to enforce the name).
    let needs_type_check = all_sources.len() > 1;

    // Create a binding set for each matching field source
    let mut binding_sets = vec![];
    let mut narrowed_type_ids: Vec<usize> = vec![];
    for source in &field_sources {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        let mut variant_identifiers_scope =
            IdentifierScope::new(identifiers, field_sources.len() > 1);
        let variant_identifiers = variant_identifiers_scope.get_mut();

        // Get fields from the source
        let (fields, type_check): (Vec<(Option<String>, usize)>, Option<usize>) = match source {
            FieldSource::Tuple(tuple_id) => {
                let tuple_fields = program
                    .lookup_tuple(*tuple_id)
                    .ok_or(Error::TupleNotInRegistry {
                        tuple_id: *tuple_id,
                    })?
                    .fields
                    .clone();
                let tuple_type_id = program.register_type(Type::Tuple(*tuple_id));
                narrowed_type_ids.push(tuple_type_id);
                let type_check = needs_type_check.then_some(tuple_type_id);
                (tuple_fields, type_check)
            }
            FieldSource::Partial { fields, .. } => {
                // Partial fields are all named, convert to (Option<String>, usize) format
                let converted: Vec<(Option<String>, usize)> = fields
                    .iter()
                    .map(|(name, type_id)| (Some(name.clone()), *type_id))
                    .collect();
                // No type check needed for partials since they're type constraints
                narrowed_type_ids.push(value_type_id);
                (converted, None)
            }
        };

        // Add type check if needed
        let mut requirements = vec![];
        if let Some(type_id) = type_check {
            requirements.push(Requirement {
                path: path.clone(),
                check: RuntimeCheck::TypeId(type_id),
            });
        }

        let mut bindings = Vec::new();
        for (idx, (name, field_type_id)) in fields.iter().enumerate() {
            if let Some(field_name) = name {
                let mut field_path = path.clone();
                // As in analyze_partial_pattern: a partial source's fields are only
                // addressable by name.
                field_path.push(match source {
                    FieldSource::Tuple(_) => Access::Position(idx),
                    FieldSource::Partial { .. } => {
                        Access::Named(program.register_field_name(field_name))
                    }
                });

                // Check if we've seen this identifier before
                if let Some(info) = variant_identifiers.get_mut(field_name.as_str()) {
                    // Repeated identifier - mark as repeated and add Path requirement
                    info.is_repeated = true;
                    requirements.push(Requirement {
                        path: info.first_path.clone(),
                        check: RuntimeCheck::Path(field_path),
                    });
                } else {
                    // First occurrence - record it
                    variant_identifiers.insert(
                        field_name.clone(),
                        Identifier {
                            first_path: field_path.clone(),
                            is_repeated: false,
                        },
                    );

                    // Create a binding for this identifier
                    // Strip [] from binding type because if the binding executes, the value is not []
                    let var_type_id = without_nil(*field_type_id, program);
                    bindings.push(Binding {
                        name: field_name.clone(),
                        path: field_path,
                        var_type_id,
                    });
                }
            }
        }

        binding_sets.push(BindingSet {
            requirements,
            bindings,
        });
    }

    // An unnamed star matches every variant, so the type is unchanged. A named star narrows
    // to the matching variants (or never, if none carry the name).
    let narrowed_type_id = match name {
        None => value_type_id,
        Some(_) if narrowed_type_ids.is_empty() => program.never(),
        Some(_) => union_type_ids(program, narrowed_type_ids),
    };
    Ok((binding_sets, narrowed_type_id))
}

/// The tuple name carried by a field source, if any.
fn field_source_name(program: &Program, source: &FieldSource) -> Option<String> {
    match source {
        FieldSource::Tuple(tuple_id) => {
            program.lookup_tuple(*tuple_id).and_then(|t| t.name.clone())
        }
        FieldSource::Partial { name, .. } => name.clone(),
    }
}

/// Represents a match against either a concrete tuple or a partial type
enum FieldMatch {
    Tuple {
        tuple_id: usize,
        field_indices: Vec<usize>,
    },
    Partial {
        name: Option<String>,
        fields: Vec<(String, usize)>, // (field_name, type_id)
        field_indices: Vec<usize>,
    },
}

fn find_types_with_fields_and_name(
    program: &mut Program,
    field_names: &[String],
    type_name: Option<&String>,
    value_type_id: usize,
) -> Result<Vec<FieldMatch>, Error> {
    let mut matches = Vec::new();

    let field_sources = extract_field_sources(program, value_type_id);

    for source in field_sources {
        match source {
            FieldSource::Tuple(tuple_id) => {
                let tuple_info = program
                    .lookup_tuple(tuple_id)
                    .ok_or(Error::TupleNotInRegistry { tuple_id })?;

                // Check if tuple name matches (if specified)
                if let Some(expected_name) = type_name
                    && tuple_info.name.as_ref() != Some(expected_name)
                {
                    continue; // Skip this type if name doesn't match
                }

                if let Some(indices) = find_field_indices(field_names, &tuple_info.fields) {
                    matches.push(FieldMatch::Tuple {
                        tuple_id,
                        field_indices: indices,
                    });
                }
            }
            FieldSource::Partial { name, fields } => {
                // Check if partial name matches (if specified)
                if let Some(expected_name) = type_name
                    && name.as_ref() != Some(expected_name)
                {
                    continue; // Skip this partial if name doesn't match
                }

                // Convert partial fields to (Option<String>, usize) format for find_field_indices
                let converted_fields: Vec<(Option<String>, usize)> = fields
                    .iter()
                    .map(|(name, type_id)| (Some(name.clone()), *type_id))
                    .collect();

                if let Some(indices) = find_field_indices(field_names, &converted_fields) {
                    matches.push(FieldMatch::Partial {
                        name,
                        fields,
                        field_indices: indices,
                    });
                }
            }
        }
    }

    Ok(matches)
}

/// Find indices of specified fields in a tuple type
fn find_field_indices(
    field_names: &[String],
    tuple_fields: &[(Option<String>, usize)],
) -> Option<Vec<usize>> {
    let mut indices = Vec::new();

    for field_name in field_names {
        match tuple_fields
            .iter()
            .position(|(name, _)| name.as_deref() == Some(field_name))
        {
            Some(idx) => indices.push(idx),
            None => return None,
        }
    }

    Some(indices)
}

// ============================================================================
// Helper functions for ID-based type operations
// ============================================================================

/// Represents field information from either a tuple or partial type
#[derive(Clone)]
enum FieldSource {
    Tuple(usize), // tuple_id
    Partial {
        name: Option<String>,
        fields: Vec<(String, usize)>, // (field_name, type_id) - all partial fields are named
    },
}

/// Whether the type has members that contribute no field source at all (primitives,
/// callables, processes, variables): a field pattern can never match them, so their
/// presence forces a runtime type check on the members it *can* match.
fn has_sourceless_members(program: &Program, type_id: usize) -> bool {
    let Some(ty) = program.lookup_type(type_id) else {
        return true;
    };
    match ty {
        Type::Annotated { base, .. } => has_sourceless_members(program, *base),
        Type::Tuple(_) | Type::Partial { .. } => false,
        Type::Union(type_ids) => type_ids
            .iter()
            .any(|&tid| has_sourceless_members(program, tid)),
        _ => true,
    }
}

/// Extract field sources from a type (tuples and partials)
fn extract_field_sources(program: &Program, type_id: usize) -> Vec<FieldSource> {
    let Some(ty) = program.lookup_type(type_id) else {
        return vec![];
    };
    match ty {
        Type::Annotated { base, .. } => extract_field_sources(program, *base),
        Type::Tuple(id) => vec![FieldSource::Tuple(*id)],
        Type::Partial { name, fields } => vec![FieldSource::Partial {
            name: name.clone(),
            fields: fields.clone(),
        }],
        Type::Union(type_ids) => type_ids
            .iter()
            .flat_map(|&tid| extract_field_sources(program, tid))
            .collect(),
        _ => vec![],
    }
}

/// Extract tuple IDs from a type (only concrete tuples, not partials)
fn extract_tuple_ids(program: &Program, type_id: usize) -> Vec<usize> {
    let Some(ty) = program.lookup_type(type_id) else {
        return vec![];
    };
    match ty {
        Type::Annotated { base, .. } => extract_tuple_ids(program, *base),
        Type::Tuple(id) => vec![*id],
        Type::Union(type_ids) => type_ids
            .iter()
            .filter_map(|&tid| {
                program.lookup_type(tid).and_then(|t| match t {
                    Type::Tuple(id) => Some(*id),
                    Type::Annotated { base, .. } => match program.lookup_type(*base) {
                        Some(Type::Tuple(id)) => Some(*id),
                        _ => None,
                    },
                    _ => None,
                })
            })
            .collect(),
        _ => vec![],
    }
}

/// Check if a type is a union
fn is_union(type_id: usize, program: &Program) -> bool {
    matches!(program.lookup_type(type_id), Some(Type::Union(_)))
}

/// Check if a type is the never type (empty union)
fn is_never(type_id: usize, program: &Program) -> bool {
    matches!(program.lookup_type(type_id), Some(Type::Union(ids)) if ids.is_empty())
}

/// Check if type a is compatible with type b (simplified version for pattern matching)
fn is_compatible(a_id: usize, b_id: usize, program: &Program) -> bool {
    if a_id == b_id {
        return true;
    }

    // Pattern matching is row-transparent: peel annotation rows before comparing.
    let (a_id, b_id) = (
        Type::strip_annotations(a_id, program),
        Type::strip_annotations(b_id, program),
    );
    if a_id == b_id {
        return true;
    }

    let (Some(a), Some(b)) = (program.lookup_type(a_id), program.lookup_type(b_id)) else {
        return false;
    };

    match (a, b) {
        (Type::Integer, Type::Integer) => true,
        (Type::Binary, Type::Binary) => true,
        (Type::Tuple(id1), Type::Tuple(id2)) => id1 == id2,
        (Type::Union(ids), _) => ids.iter().all(|&id| is_compatible(id, b_id, program)),
        (_, Type::Union(ids)) => ids.iter().any(|&id| is_compatible(a_id, id, program)),
        // For partial compatibility, use the full is_compatible from types module
        _ => quiver_core::types::is_compatible(a_id, b_id, program),
    }
}

/// Remove nil from a type (for bindings that strip nil)
fn without_nil(type_id: usize, program: &mut Program) -> usize {
    let Some(ty) = program.lookup_type(type_id) else {
        return type_id;
    };

    match ty {
        Type::Union(ids) => {
            let ids = ids.clone();
            let filtered: Vec<usize> = ids
                .into_iter()
                .filter(|&id| {
                    let id = Type::strip_annotations(id, program);
                    if let Some(Type::Tuple(tuple_id)) = program.lookup_type(id) {
                        // Check if this is the nil tuple (empty tuple with no name)
                        if let Some(info) = program.lookup_tuple(*tuple_id) {
                            !(info.fields.is_empty() && info.name.is_none())
                        } else {
                            true
                        }
                    } else {
                        true
                    }
                })
                .collect();
            union_type_ids(program, filtered)
        }
        Type::Annotated { .. } if ty.is_nil_deep(program) => program.never(),
        Type::Tuple(tuple_id) => {
            // Check if this is nil
            if let Some(info) = program.lookup_tuple(*tuple_id)
                && info.fields.is_empty()
                && info.name.is_none()
            {
                return program.never();
            }
            type_id
        }
        _ => type_id,
    }
}
