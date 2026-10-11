//! Type-consuming functions: generic functions whose body needs a type-consuming builtin
//! (`%codec.convert<'t>`, `%data.decode<'t>`) instantiated at their own type parameters.
//!
//! A body is compiled once, and checked once with its type parameters rigid: a type-consuming
//! builtin is typed parametrically, so only its *instantiation* — its plan, its label tables —
//! depends on the types. Each instantiation the body needs is a **requirement**: the builtin and
//! its type arguments, as templates over the type parameters in scope. The body reads it from a
//! **hidden capture** (`Hidden`), held by its closure past its ordinary captures.
//!
//! A requirement over an enclosing function's type parameters is filled where the enclosing
//! body makes the closure, from its own hidden captures or with a constant. One over the
//! function's own type parameters makes the function type-consuming: it is filled where the
//! function is **named** at types that determine them — its type arguments written, or inferred
//! from the argument it is applied to — by substituting them into the templates. A module
//! member named at concrete types is a constant closure; anything else is extended where it is
//! named (`Extend`). Requirements are only ever builtin instantiations: naming a type-consuming
//! function inside another substitutes into its requirements, which become the outer one's.
//!
//! The requirements are part of the function's type (`Type::Callable::consumes`), as is an
//! uninstantiated type-consuming builtin's, so they travel wherever its type does — through a
//! binding, a destructured module, a tuple field — and substitution reaches them as it does the
//! rest of the type. Naming it with its type arguments answers the plain function type. Until
//! then it fits no other type than its own: not a plain function's, so calling it or passing it on
//! is a type error, and not the top type or a type variable either (`holds_consuming`), which
//! would forget what it needs. Only its own type ever tracks an unnamed one, so the runtime,
//! which judges a closure by its function, judges every closure as named, and a type test on a
//! value that may hold an unnamed one is made against what its type says it can be.

use std::collections::HashMap;

use quiver_core::bytecode::{Constant, Instruction, Opcode};
use quiver_core::types::{Consumes, Requirement, Type, TypeLookup};

use super::{Compiler, Error, typing};
use crate::ast;

/// The requirements found while compiling one function literal, in the order its body first
/// needs them (the order its `Hidden` operands count until the literal is finished).
pub(super) struct RequirementFrame {
    /// The type parameters the literal declares itself (not an enclosing one's, re-declared).
    pub own: Vec<String>,
    pub found: Vec<Requirement>,
}

impl<E: quiver_core::effects::Effect> Compiler<'_, E> {
    /// Whether every free type variable in `type_id` is a type parameter of a function being
    /// compiled: a type a requirement can be a template over. (A generic function's own
    /// parameters, quantified inside its type, are not free.)
    pub(super) fn only_open_variables(&self, type_id: usize) -> bool {
        self.free_variables(type_id).iter().all(|name| {
            self.requirement_frames
                .iter()
                .any(|frame| frame.own.contains(name))
        })
    }

    /// Whether `type_id` mentions a type parameter of a function being compiled.
    fn mentions_open_variable(&self, type_id: usize) -> bool {
        let mut names = Vec::new();
        typing::collect_type_variables(type_id, &*self.program, &mut names);
        names.iter().any(|name| {
            self.requirement_frames
                .iter()
                .any(|frame| frame.own.contains(name))
        })
    }

    /// Push the instantiation of `builtin` at `type_arguments`: a constant when they mention no
    /// type parameter of a function being compiled; otherwise a requirement of the innermost
    /// one, read from its hidden captures. Answers the constant, when there is one. (Any other
    /// variable they mention is quantified inside them — a generic edge's own — as checked where
    /// they were first written.)
    pub(super) fn emit_instance(
        &mut self,
        builtin: &str,
        type_arguments: Vec<usize>,
    ) -> Result<Option<usize>, Error> {
        if !type_arguments
            .iter()
            .any(|&argument| self.mentions_open_variable(argument))
        {
            let builtin_index = self.program.register_builtin_instantiated(
                builtin.to_string(),
                type_arguments,
                self.builtins,
            );
            super::check_conversion_plan(self.program, builtin_index)?;
            let constant = self
                .program
                .register_constant(Constant::Builtin { id: builtin_index });
            self.codegen.add_instruction(Instruction::push(constant));
            return Ok(Some(constant));
        }
        let requirement = Requirement {
            builtin: builtin.to_string(),
            type_arguments,
        };
        let frame = self
            .requirement_frames
            .last_mut()
            .expect("an open type parameter belongs to a function being compiled");
        let slot = match frame.found.iter().position(|found| *found == requirement) {
            Some(slot) => slot,
            None => {
                frame.found.push(requirement);
                frame.found.len() - 1
            }
        };
        self.codegen.add_instruction(Instruction::hidden(slot));
        Ok(None)
    }

    /// Finish a function literal's requirements: split them into those over enclosing type
    /// parameters, filled where the closure is made, and those over its own, filled where it is
    /// named, and renumber the body's `Hidden` operands to match (the first kind first, so the
    /// closure's hidden captures can be appended in two steps). Answers the two kinds.
    pub(super) fn finish_requirements(
        &mut self,
        frame: RequirementFrame,
        body: &mut [Instruction],
    ) -> (Vec<Requirement>, Vec<Requirement>) {
        let program = &*self.program;
        let owns = |requirement: &Requirement| {
            requirement.type_arguments.iter().any(|&argument| {
                frame
                    .own
                    .iter()
                    .any(|name| typing::mentions_variable(argument, name, program))
            })
        };
        let (named, made): (Vec<_>, Vec<_>) = frame
            .found
            .iter()
            .enumerate()
            .partition(|(_, requirement)| owns(requirement));
        let mut slots = vec![0; frame.found.len()];
        for (slot, &(found, _)) in made.iter().chain(&named).enumerate() {
            slots[found] = slot;
        }
        for instruction in body.iter_mut() {
            if instruction.opcode() == Opcode::Hidden {
                *instruction = Instruction::hidden(slots[instruction.operand() as usize]);
            }
        }
        let clone = |kind: Vec<(usize, &Requirement)>| {
            kind.into_iter()
                .map(|(_, requirement)| requirement.clone())
                .collect()
        };
        (clone(made), clone(named))
    }

    /// Append the instantiations `requirements` need, at `bindings`, to the closure on top of
    /// the stack. When the closure is a constant and every instantiation is too, the result is a
    /// constant closure in place of the push.
    pub(super) fn extend_closure(
        &mut self,
        requirements: &[Requirement],
        bindings: &HashMap<String, usize>,
    ) -> Result<(), Error> {
        if requirements.is_empty() {
            return Ok(());
        }
        let closure_at = self.codegen.instructions.len().checked_sub(1);
        let mut constants = Vec::with_capacity(requirements.len());
        for requirement in requirements {
            let type_arguments = requirement
                .type_arguments
                .iter()
                .map(|&template| typing::substitute(template, bindings, self.program))
                .collect();
            if let Some(constant) = self.emit_instance(&requirement.builtin, type_arguments)? {
                constants.push(constant);
            }
        }
        let folded = closure_at.and_then(|at| {
            let push = self.codegen.instructions[at];
            if push.opcode() != Opcode::Push || constants.len() != requirements.len() {
                return None;
            }
            match self.program.get_constant(push.operand() as usize) {
                Some(Constant::Function { id, captures }) => Some((at, *id, captures.clone())),
                _ => None,
            }
        });
        match folded {
            Some((at, id, mut captures)) => {
                captures.extend(constants);
                let constant = self
                    .program
                    .register_constant(Constant::Function { id, captures });
                self.codegen.instructions.truncate(at);
                self.codegen.add_instruction(Instruction::push(constant));
            }
            None => self
                .codegen
                .add_instruction(Instruction::extend(requirements.len())),
        }
        Ok(())
    }

    /// Where a value of type `accessed_type` — on top of the stack — is named: when it is
    /// type-consuming and named at types that determine its type parameters (written in
    /// `type_arguments`, or inferred from the `argument` it is `applied` to), give it the
    /// instantiations it needs and answer its plain, instantiated type. `None` for any other
    /// value, and for one merely named (a binding, a tuple field), which keeps its type.
    fn instantiate_consuming(
        &mut self,
        accessed_type: usize,
        type_arguments: &[ast::Type],
        applied: bool,
        argument: Option<usize>,
    ) -> Result<Option<usize>, Error> {
        let base = Type::strip_annotations(accessed_type, &*self.program);
        let Some(Type::Callable {
            consumes: Some(consumes),
            ..
        }) = self.program.lookup_type(base).cloned()
        else {
            return Ok(None);
        };
        if type_arguments.is_empty() && !applied {
            return Ok(None);
        }
        let plain = self.without_consumes(accessed_type);
        let (parameters, requirements) = match consumes {
            Consumes::Builtin(builtin) => {
                // Its instantiation is made here, in its place.
                self.codegen.add_instruction(Instruction::pop());
                let instance = self.resolve_builtin_type_arguments(
                    &builtin,
                    type_arguments,
                    applied,
                    argument,
                )?;
                self.emit_instance(&builtin, instance)?;
                return self
                    .instantiate_type_arguments_of(plain, Some(&builtin), type_arguments)
                    .map(Some);
            }
            Consumes::Captures {
                parameters,
                requirements,
            } => (parameters, requirements),
        };

        if type_arguments.len() > parameters.len() {
            return Err(Error::TypeArgumentsTooMany {
                declared: parameters.len(),
                given: type_arguments.len(),
            });
        }
        let mut bindings = HashMap::new();
        for (parameter, written) in parameters.iter().zip(type_arguments) {
            let mut env = typing::TypeEnv {
                resolver: self.resolver,
                module_cache: &mut *self.module_cache,
                package: &self.current_package,
            };
            let resolved =
                typing::resolve_ast_type(&mut env, &self.scopes, written.clone(), self.program)?;
            bindings.insert(parameter.clone(), resolved);
        }
        if let (true, Some(argument)) = (applied, argument) {
            let parameter = typing::open_callable(base, self.program)
                .expect("a consuming type is callable")
                .parameter;
            typing::unify(&mut bindings, parameter, argument, self.program)?;
            typing::resolve_bindings(&mut bindings, self.program);
        }
        // Each parameter the templates mention must now be known.
        if let Some(missing) = parameters.iter().find(|name| {
            !bindings.contains_key(*name)
                && requirements
                    .iter()
                    .flat_map(|requirement| &requirement.type_arguments)
                    .any(|&template| typing::mentions_variable(template, name, &*self.program))
        }) {
            return Err(Error::TypeArgumentUndetermined {
                function: quiver_core::format::format_type_by_id(&*self.program, base),
                parameter: quiver_core::types::declared_variable_name(missing).to_string(),
            });
        }
        self.check_bindings_fit_bounds(&bindings)?;
        self.extend_closure(&requirements, &bindings)?;
        Ok(Some(typing::substitute(plain, &bindings, self.program)))
    }

    /// `type_id` without what its function consumes: the plain function type it has once
    /// named with its type arguments. An annotation row rides through.
    pub(super) fn without_consumes(&mut self, type_id: usize) -> usize {
        match self.program.lookup_type(type_id).cloned() {
            Some(Type::Annotated {
                base,
                exact,
                entries,
            }) => {
                let base = self.without_consumes(base);
                self.program.register_type(Type::Annotated {
                    base,
                    exact,
                    entries,
                })
            }
            Some(Type::Callable {
                parameter,
                result,
                receive,
                states,
                omittable,
                consumes: Some(_),
            }) => self.program.register_type(Type::Callable {
                parameter,
                result,
                receive,
                states,
                omittable,
                consumes: None,
            }),
            _ => type_id,
        }
    }

    /// Where a value on top of the stack, of type `accessed_type`, is named: instantiate it if
    /// it is type-consuming and named at types that determine it, and otherwise apply the
    /// written type arguments to its type, as for any generic.
    pub(super) fn instantiate_named(
        &mut self,
        accessed_type: usize,
        type_arguments: &[ast::Type],
        applied: bool,
        argument: Option<usize>,
    ) -> Result<usize, Error> {
        let argument = applied.then_some(argument).flatten();
        if let Some(instantiated) =
            self.instantiate_consuming(accessed_type, type_arguments, applied, argument)?
        {
            return Ok(instantiated);
        }
        self.instantiate_type_arguments(accessed_type, type_arguments)
    }

    /// That a value of type `type_id` holds no type-consuming function not yet named (see
    /// `holds_consuming`).
    pub(super) fn check_complete(&self, type_id: usize) -> Result<(), Error> {
        match quiver_core::types::find_consuming(type_id, &*self.program) {
            Some(function) => Err(Error::ConsumingUninstantiated {
                function: quiver_core::format::format_type_by_id(&*self.program, function),
            }),
            None => Ok(()),
        }
    }

    /// That a callable about to be called, spawned or tail-called has been named with the type
    /// arguments it consumes.
    pub(super) fn check_named(&self, callee_type: usize) -> Result<(), Error> {
        match self.program.lookup_base(callee_type) {
            Some(Type::Callable {
                consumes: Some(_), ..
            }) => Err(Error::ConsumingUninstantiated {
                function: quiver_core::format::format_type_by_id(&*self.program, callee_type),
            }),
            _ => Ok(()),
        }
    }
}
