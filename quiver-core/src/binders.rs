//! How a walk over types resolves their recursive references.
//!
//! A recursive reference is stored as `Type::Cycle(n)`: the n-th *binder* enclosing it,
//! counting outward from 1. The binders are unions and function types (a function type is one
//! binder over all of its parts). What a reference means therefore depends on the binders a walk
//! passed through to reach it, so a walk records them as it enters each one, innermost last.

use std::collections::HashSet;

use crate::program::Program;
use crate::types::{Type, TypeLookup};

/// Whether `ty` is a binder: a type whose parts a `Cycle` counts through.
pub fn is_binder(ty: &Type) -> bool {
    matches!(ty, Type::Union(_) | Type::Callable { .. })
}

/// The binders a walk has entered, innermost last.
#[derive(Clone, Debug, Default)]
pub struct BinderStack(Vec<usize>);

/// The binders `BinderStack::follow` cut off, for `BinderStack::restore`.
#[must_use = "a followed reference's cut must be restored"]
pub struct Cut(Vec<usize>);

impl BinderStack {
    /// Record `binder` as entered, for a walk into its parts. Every entry is recorded, even of
    /// a binder already on the stack: re-entering a union (as following one of its own
    /// references does) is a binder deeper, and the stack mirrors the path taken.
    pub fn enter(&mut self, binder: usize) {
        self.0.push(binder);
    }

    /// Undo the matching `enter`.
    pub fn leave(&mut self, binder: usize) {
        let left = self.0.pop();
        assert_eq!(
            left,
            Some(binder),
            "left a binder that was not entered last"
        );
    }

    /// The binder `Cycle(depth)` names here, if the walk entered it.
    pub fn resolve(&self, depth: usize) -> Option<usize> {
        let index = self.0.len().checked_sub(depth)?;
        self.0.get(index).copied()
    }

    /// Follow `Cycle(depth)` to the binder it names, cutting the stack back to the binders that
    /// enclose that one — so a walk into it from here counts its own references as they were
    /// written. `None` when the walk did not enter it.
    pub fn follow(&mut self, depth: usize) -> Option<(usize, Cut)> {
        let binder = self.resolve(depth)?;
        let cut = self.0.split_off(self.0.len() - depth);
        Some((binder, Cut(cut)))
    }

    /// Put back what `follow` cut off.
    pub fn restore(&mut self, cut: Cut) {
        self.0.extend(cut.0);
    }

    /// The binders entered, outermost first.
    pub fn as_slice(&self) -> &[usize] {
        &self.0
    }
}

/// One stack per side of a walk relating two types: each side's references name binders of the
/// type they came from, never the other's.
#[derive(Clone, Debug, Default)]
pub struct BinderPair {
    pub left: BinderStack,
    pub right: BinderStack,
}

impl BinderPair {
    /// Exchange the sides, as a walk does where the relation's direction flips (a function's
    /// parameter is contravariant).
    pub fn swap(&mut self) {
        std::mem::swap(&mut self.left, &mut self.right);
    }

    /// `f` with the sides exchanged.
    pub fn swapped<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.swap();
        let result = f(self);
        self.swap();
        result
    }
}

/// `type_id`, standing inside the binders `enclosing` (outermost first), with every free
/// reference replaced by the binder it names, itself closed against the binders enclosing it.
/// The result means the same wherever it is used. A reference reaching past the outermost
/// binder has nothing to be closed with, and is kept.
pub fn close_against(type_id: usize, enclosing: &[usize], program: &mut Program) -> usize {
    rewrite_free_cycles(
        type_id,
        0,
        program,
        &mut |depth, cutoff, program| match enclosing.len().checked_sub(depth - cutoff) {
            Some(index) => close_against(enclosing[index], &enclosing[..index], program),
            None => program.register_type(Type::Cycle(depth)),
        },
    )
}

/// Whether `type_id` has a `Cycle` reaching above its own root — one whose meaning depends on
/// where the type sits.
pub fn has_free_cycles(type_id: usize, program: &Program) -> bool {
    fn walk(
        type_id: usize,
        cutoff: usize,
        program: &Program,
        seen: &mut HashSet<(usize, usize)>,
    ) -> bool {
        if !seen.insert((type_id, cutoff)) {
            return false;
        }
        let Some(typ) = program.lookup_type(type_id) else {
            return false;
        };
        if let Type::Cycle(depth) = typ {
            return *depth > cutoff;
        }
        let inner = cutoff + usize::from(is_binder(typ));
        typ.parts(program)
            .into_iter()
            .any(|part| walk(part, inner, program, seen))
    }
    walk(type_id, 0, program, &mut HashSet::new())
}

/// Rebuild `type_id` with each *free* `Cycle` — one reaching above the walked fragment's
/// root, `Cycle(k)` with `k > cutoff` at `cutoff` binders deep — replaced by
/// `rewrite(k, cutoff)`.
pub fn rewrite_free_cycles(
    type_id: usize,
    cutoff: usize,
    program: &mut Program,
    rewrite: &mut dyn FnMut(usize, usize, &mut Program) -> usize,
) -> usize {
    let Some(typ) = program.lookup_type(type_id).cloned() else {
        return type_id;
    };
    if let Type::Cycle(depth) = typ {
        return if depth > cutoff {
            rewrite(depth, cutoff, program)
        } else {
            type_id
        };
    }
    let inner = cutoff + usize::from(is_binder(&typ));
    let parts: Vec<usize> = typ
        .parts(&*program)
        .into_iter()
        .map(|part| rewrite_free_cycles(part, inner, program, rewrite))
        .collect();
    program.with_parts(type_id, &parts)
}
