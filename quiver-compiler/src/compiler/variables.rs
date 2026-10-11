//! Variable resolution across function boundaries.
//!
//! A function literal's body is compiled in scopes of its own. A name it reads that those
//! scopes don't bind is resolved where the literal was written, and becomes one of its
//! **captures** the first time it is read: a value the closure holds, evaluated where the
//! closure is made. A name the enclosing function doesn't bind either is captured by it in
//! turn, so a chain of closures relays the value inward. Because captures are found by the same
//! scope walk as any other read, a body's own bindings shadow them exactly as they shadow outer
//! bindings anywhere else.

use quiver_core::bytecode::Instruction;

use super::scopes::{self, Scope, Slot, Variable};
use super::{Compiler, Error, Provenance};
use crate::ast;

/// Where a captured value comes from in the creation scope.
#[derive(Debug, Clone, PartialEq)]
pub enum CaptureSource {
    /// A variable in the enclosing scope, by name.
    Variable(String),
    /// A function parameter `levels` above the capturing function: 1 is its parent's own
    /// parameter; deeper levels resolve to the parent's own outer-parameter capture, one
    /// level shallower, so a chain of closures relays the value inward.
    OuterParameter(usize),
}

impl CaptureSource {
    /// The base name this capture registers and resolves under in the string-keyed scope
    /// map. A private encoding — produced only here, never parsed back. An outer parameter
    /// borrows the written sigil spelling purely because identifiers can never start with
    /// `$`, so the key cannot collide with a variable capture.
    pub fn scope_name(&self) -> String {
        match self {
            CaptureSource::Variable(name) => name.clone(),
            CaptureSource::OuterParameter(levels) => ast::parameter_sigils(*levels),
        }
    }

    /// The error for reading this source where nothing binds it.
    pub fn undefined(&self) -> Error {
        match self {
            CaptureSource::Variable(name) => Error::VariableUndefined(name.clone()),
            CaptureSource::OuterParameter(levels) => Error::ParameterDepthExceeded {
                written: ast::parameter_sigils(*levels),
            },
        }
    }

    /// What this source is to the function enclosing the one reading it: the same variable,
    /// or an outer parameter one level nearer.
    fn relay(&self) -> CaptureSource {
        match self {
            CaptureSource::Variable(name) => CaptureSource::Variable(name.clone()),
            CaptureSource::OuterParameter(levels) => CaptureSource::OuterParameter(levels - 1),
        }
    }
}

/// One capture of a function literal: a source in the scope the literal was written in,
/// optionally narrowed to a path within it, so a closure reading only `o.t` holds only that.
#[derive(Debug, Clone, PartialEq)]
pub struct Capture {
    pub source: CaptureSource,
    pub accessors: Vec<ast::AccessPath>,
}

/// A function literal being compiled inside another: the scopes in effect where it was
/// written, and the captures its body has needed so far, in the order of its closure's slots.
pub struct Enclosing {
    pub scopes: Vec<Scope>,
    pub captures: Vec<Capture>,
}

impl<E: quiver_core::effects::Effect> Compiler<'_, E> {
    /// Resolve a variable, or a captured path of one, for reading: its type and where it is
    /// read from. A name the function being compiled doesn't bind is captured.
    ///
    /// Answers `None` for a path when the base is bound but the path isn't, so the caller reads
    /// the base and walks the accessors.
    pub(super) fn lookup_variable(
        &mut self,
        name: &str,
        accessors: &[ast::AccessPath],
    ) -> Option<(usize, Slot)> {
        self.resolve(CaptureSource::Variable(name.to_string()), accessors)
    }

    /// Resolve `source` at `accessors`, as [`Self::lookup_variable`] does a variable. An outer
    /// parameter (`$$`, `$$$x`) is always a capture, as it belongs to an enclosing function.
    pub(super) fn resolve(
        &mut self,
        source: CaptureSource,
        accessors: &[ast::AccessPath],
    ) -> Option<(usize, Slot)> {
        let key = source.scope_name();
        if let Some(found) = scopes::lookup_variable(&self.scopes, &key, accessors) {
            return Some(found);
        }
        if !accessors.is_empty() && scopes::binds(&self.scopes, &key) {
            return None;
        }
        let level = self.enclosing.len();
        (level > 0).then(|| self.capture(level, source, accessors))?
    }

    /// The type a read of `source` at `accessors` answers, without emitting it: the captured
    /// path's when there is one, else the base's with the accessors walked.
    pub(super) fn peek_resolved_type(
        &mut self,
        source: &CaptureSource,
        accessors: &[ast::AccessPath],
    ) -> Option<usize> {
        if let Some((ty, _)) = self.resolve(source.clone(), accessors) {
            return Some(ty);
        }
        let (base, _) = self.resolve(source.clone(), &[])?;
        self.peek_accessor_type(base, accessors, &source.scope_name())
            .ok()
    }

    /// The type a read of variable `name` at `accessors` would answer, from wherever it is
    /// bound — in the scopes being compiled or an enclosing function's — without reading it:
    /// nothing is marked used or captured. For passes over a body ahead of compiling it.
    pub(super) fn peek_variable_type(
        &mut self,
        name: &str,
        accessors: &[ast::AccessPath],
    ) -> Option<usize> {
        // The nearest function binding the path whole, or else its base, answers it.
        let (ty, whole) = (0..=self.enclosing.len()).rev().find_map(|level| {
            let scopes = self.scopes_at(level);
            scopes::peek_variable(scopes, name, accessors)
                .map(|ty| (ty, true))
                .or_else(|| scopes::peek_variable(scopes, name, &[]).map(|base| (base, false)))
        })?;
        if whole {
            Some(ty)
        } else {
            self.peek_accessor_type(ty, accessors, name).ok()
        }
    }

    /// The scopes of the function at `level`: 0 is where the outermost literal being compiled
    /// was written, and `self.enclosing.len()` is the body being compiled now.
    fn scopes_at(&self, level: usize) -> &[Scope] {
        if level == self.enclosing.len() {
            &self.scopes
        } else {
            &self.enclosing[level].scopes
        }
    }

    fn scopes_at_mut(&mut self, level: usize) -> &mut [Scope] {
        if level == self.enclosing.len() {
            &mut self.scopes
        } else {
            &mut self.enclosing[level].scopes
        }
    }

    /// Capture `source` (at `accessors`) into the literal at `level` (at least 1), from the
    /// scopes it was written in — capturing it into the literal enclosing it first, when those
    /// don't bind it either. Answers its type and slot there, or `None` when nothing binds it.
    fn capture(
        &mut self,
        level: usize,
        source: CaptureSource,
        accessors: &[ast::AccessPath],
    ) -> Option<(usize, Slot)> {
        let creation = level - 1;
        let key = source.scope_name();
        let ty = match &source {
            CaptureSource::OuterParameter(1) => {
                let (ty, _) = scopes::get_function_parameter(self.scopes_at(creation)).ok()?;
                self.peek_accessor_type(ty, accessors, &key).ok()?
            }
            _ => {
                let outer = source.relay();
                let outer_key = outer.scope_name();
                let read = |accessors| {
                    scopes::lookup_variable(self.scopes_at(creation), &outer_key, accessors)
                        .map(|(ty, _)| ty)
                };
                match (read(accessors), read(&[])) {
                    (Some(ty), _) => ty,
                    (None, Some(base)) => {
                        self.peek_accessor_type(base, accessors, &outer_key).ok()?
                    }
                    (None, None) if creation > 0 => self.capture(creation, outer, accessors)?.0,
                    (None, None) => return None,
                }
            }
        };
        let captures = &mut self.enclosing[level - 1].captures;
        let slot = Slot::Capture(captures.len());
        captures.push(Capture {
            source,
            accessors: accessors.to_vec(),
        });
        // A function's captures live in its root scope, beneath every scope its body opens.
        self.scopes_at_mut(level)[0].bindings.variables.insert(
            super::helpers::make_capture_name(&key, accessors),
            Variable {
                ty,
                slot,
                provenance: Provenance::Unknown,
                usage: scopes::Usage::default(),
            },
        );
        Some((ty, slot))
    }

    /// Push each of a literal's captures, read from the scope it is made in, for `Enclose`.
    pub(super) fn emit_captures(&mut self, captures: Vec<Capture>) -> Result<(), Error> {
        for Capture { source, accessors } in captures {
            if source == CaptureSource::OuterParameter(1) {
                let (ty, index) = scopes::get_function_parameter(&self.scopes)?;
                self.codegen.add_instruction(Instruction::load(index));
                self.compile_accessor(ty, accessors, "$", Provenance::Unknown)?;
                continue;
            }
            let outer = source.relay();
            let key = outer.scope_name();
            match self.resolve(outer.clone(), &accessors) {
                Some((_, slot)) => self.codegen.add_instruction(slot.load()),
                None => {
                    let (ty, slot) =
                        self.resolve(outer, &[])
                            .ok_or_else(|| Error::InternalError {
                                message: format!("capture of `{key}` unresolved where it is made"),
                            })?;
                    self.codegen.add_instruction(slot.load());
                    self.compile_accessor(ty, accessors, &key, Provenance::Unknown)?;
                }
            }
        }
        Ok(())
    }
}
