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

use super::{Compiler, Error, pattern, typing};

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
    pub(super) fn mentions_open_variable(&self, type_id: usize) -> bool {
        let mut names = Vec::new();
        typing::collect_type_variables(type_id, &*self.program, &mut names);
        names.iter().any(|name| {
            self.requirement_frames
                .iter()
                .any(|frame| frame.own.contains(name))
        })
    }

    /// Push the instantiation of `builtin` at `type_arguments` (see [`Self::require`]).
    /// Answers the constant, when it is one.
    pub(super) fn emit_instance(
        &mut self,
        builtin: &str,
        type_arguments: Vec<usize>,
    ) -> Result<Option<usize>, Error> {
        let (push, constant) = self.require(Requirement::Instance {
            builtin: builtin.to_string(),
            type_arguments,
        })?;
        self.codegen.add_instruction(push);
        Ok(constant)
    }

    /// The instruction pushing what `requirement` holds, and the constant it pushes when it is
    /// one: a constant when it mentions no type parameter of a function being compiled;
    /// otherwise a requirement of the innermost one, read from its hidden captures. (Any other
    /// variable it mentions is quantified inside it — a generic edge's own — as checked where it
    /// was first written.)
    fn require(&mut self, requirement: Requirement) -> Result<(Instruction, Option<usize>), Error> {
        if !requirement
            .templates()
            .iter()
            .any(|&template| self.mentions_open_variable(template))
        {
            let constant = match requirement {
                Requirement::Instance {
                    builtin,
                    type_arguments,
                } => {
                    let builtin_index = self.program.register_builtin_instantiated(
                        builtin,
                        type_arguments,
                        self.builtins,
                    );
                    super::check_conversion_plan(self.program, builtin_index)?;
                    Constant::Builtin { id: builtin_index }
                }
                Requirement::Type(id) => Constant::Type { id },
            };
            let constant = self.program.register_constant(constant);
            return Ok((Instruction::push(constant), Some(constant)));
        }
        Ok((
            Instruction::hidden(self.requirement_slot(requirement)),
            None,
        ))
    }

    /// The hidden capture of the innermost function being compiled that holds `requirement`.
    pub(super) fn requirement_slot(&mut self, requirement: Requirement) -> usize {
        let frame = self
            .requirement_frames
            .last_mut()
            .expect("an open type parameter belongs to a function being compiled");
        match frame.found.iter().position(|found| *found == requirement) {
            Some(slot) => slot,
            None => {
                frame.found.push(requirement);
                frame.found.len() - 1
            }
        }
    }

    /// The instruction testing a value against `tested`: `TestType` when it is concrete, and
    /// otherwise `TestHidden`, against the type the function being compiled holds from where it
    /// is named. Only such a function can be given one, so `tested` may mention no other free
    /// variable.
    pub(super) fn type_test(&mut self, tested: usize) -> Result<Instruction, Error> {
        if !self.only_open_variables(tested) {
            return Err(Error::TypeTestNotConcrete {
                tested: quiver_core::format::format_type_by_id(&*self.program, tested),
            });
        }
        if !self.mentions_open_variable(tested) {
            return Ok(Instruction::test_type(tested));
        }
        Ok(Instruction::test_hidden(
            self.requirement_slot(Requirement::Type(tested)),
        ))
    }

    /// The type tests a pattern makes against types its function is named at (see
    /// [`pattern::witness_types`]).
    pub(super) fn witnesses(
        &mut self,
        binding_sets: &[pattern::BindingSet],
    ) -> Result<pattern::Witnesses, Error> {
        pattern::witness_types(binding_sets)
            .into_iter()
            .map(|tested| Ok((tested, self.type_test(tested)?)))
            .collect()
    }

    /// Finish a function literal's requirements: split them into those over enclosing type
    /// parameters, filled where the closure is made, and those over its own, filled where it is
    /// named, and renumber the body's `Hidden` and `TestHidden` operands to match (the first kind
    /// first, so the closure's hidden captures can be appended in two steps). Answers the two
    /// kinds, and each requirement's hidden capture by the order it was found in.
    pub(super) fn finish_requirements(
        &mut self,
        frame: RequirementFrame,
        body: &mut [Instruction],
    ) -> (Vec<Requirement>, Vec<Requirement>, Vec<usize>) {
        let program = &*self.program;
        let owns = |requirement: &Requirement| {
            requirement.templates().iter().any(|&argument| {
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
            if matches!(instruction.opcode(), Opcode::Hidden | Opcode::TestHidden) {
                *instruction = instruction.with_operand(slots[instruction.operand() as usize]);
            }
        }
        let clone = |kind: Vec<(usize, &Requirement)>| {
            kind.into_iter()
                .map(|(_, requirement)| requirement.clone())
                .collect()
        };
        (clone(made), clone(named), slots)
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
            let requirement = requirement
                .with_templates(|template| typing::substitute(template, bindings, self.program));
            let (push, constant) = self.require(requirement)?;
            self.codegen.add_instruction(push);
            constants.extend(constant);
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
                // Its instantiation is made here, in its place, keeping the annotations (a
                // module's `:doc`) of a builtin known where it is pushed.
                let annotations = self.take_pushed_builtin();
                let instance = self.resolve_builtin_type_arguments(
                    &builtin,
                    type_arguments,
                    applied,
                    argument,
                )?;
                let constant = self.emit_instance(&builtin, instance)?;
                if let (Some(constant), Some(entries)) = (constant, annotations) {
                    let annotated = self.program.register_constant(Constant::Annotated {
                        value: constant,
                        entries,
                    });
                    *self
                        .codegen
                        .instructions
                        .last_mut()
                        .expect("the instance was pushed") = Instruction::push(annotated);
                }
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
        // Those not written are inferred from the argument, against the parameter at the written
        // ones: an explicit type argument is a checked assertion, which the argument must fit
        // (as the call then checks), not a first guess it can widen.
        if let (true, Some(argument)) = (applied, argument)
            && bindings.len() < parameters.len()
        {
            let parameter = typing::open_callable(base, self.program)
                .expect("a consuming type is callable")
                .parameter;
            let parameter = typing::substitute(parameter, &bindings, self.program);
            let mut inferred = HashMap::new();
            typing::unify(&mut inferred, parameter, argument, self.program)?;
            typing::resolve_bindings(&mut inferred, self.program);
            // One the parameter holds values of, but the argument holds none of (`race [Nil]`),
            // is determined too: as the type of no value, as a call's result closes one.
            typing::close_unpinned_result(parameter, &mut inferred, self.program);
            for name in &parameters {
                if let Some(&inferred) = inferred.get(name) {
                    bindings.entry(name.clone()).or_insert(inferred);
                }
            }
        }
        // Each parameter the templates mention must now be known.
        if let Some(missing) = parameters.iter().find(|name| {
            !bindings.contains_key(*name)
                && requirements
                    .iter()
                    .flat_map(|requirement| requirement.templates())
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

    /// Take back the uninstantiated builtin on top of the stack, which its instantiation
    /// replaces: the push of a constant is undone, answering its annotation entries when it has
    /// any, and any other value is popped.
    fn take_pushed_builtin(&mut self) -> Option<Vec<(usize, usize)>> {
        let pushed = self
            .codegen
            .instructions
            .last()
            .filter(|instruction| instruction.opcode() == Opcode::Push)
            .map(|push| self.program.get_constant(push.operand() as usize).cloned());
        match pushed {
            Some(Some(constant @ (Constant::Builtin { .. } | Constant::Annotated { .. }))) => {
                self.codegen.instructions.pop();
                match constant {
                    Constant::Annotated { entries, .. } => Some(entries),
                    _ => None,
                }
            }
            _ => {
                self.codegen.add_instruction(Instruction::pop());
                None
            }
        }
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

    /// Where a value on top of the stack, of type `accessed_type` and with `provenance`, is
    /// named: instantiate it if it is type-consuming and named at types that determine it, and
    /// otherwise apply the written type arguments to its type, as for any generic. Answers its
    /// type as named, and its provenance — none, once instantiated, as the closure is a new one:
    /// the plain type it now has must not narrow what it was named from.
    pub(super) fn instantiate_named(
        &mut self,
        accessed_type: usize,
        type_arguments: &[ast::Type],
        applied: bool,
        argument: Option<usize>,
        provenance: super::Provenance,
    ) -> Result<(usize, super::Provenance), Error> {
        let argument = applied.then_some(argument).flatten();
        if let Some(instantiated) =
            self.instantiate_consuming(accessed_type, type_arguments, applied, argument)?
        {
            return Ok((instantiated, super::Provenance::Unknown));
        }
        Ok((
            self.instantiate_type_arguments(accessed_type, type_arguments)?,
            provenance,
        ))
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
