//! Honest labels for the tuples a type-directed builtin builds.
//!
//! A tuple value's id is its label: a whole-type test reads the value as what the label's
//! field types say, never looking inside. The compiler builds every tuple with closed field
//! types, but a builtin that builds values by walking a type (`%data.decode<'t>`) only has
//! that type's own members to hand, and a member of a recursive type is a fragment:
//! `Cons['int, ^]`, whose `^` names a binder the fragment no longer has. Such a label would fit
//! anything. The honest label is the fragment closed against the binders the walk entered
//! to reach it (`Cons['int, 'l]`).
//!
//! The runtime can neither create types nor find them by shape, so the labels are computed
//! when an instantiation is registered, by the same walk the builtin makes, and carried on
//! its table row. At runtime the builtin looks each tuple it builds up by its position.

use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

use crate::binders::{BinderStack, close_against};
use crate::error::Error;
use crate::program::Program;
use crate::types::{Type, TypeLookup};

/// The label for a tuple reached by a type walk: the member `tuple`, reached inside
/// `binders` (outermost first, as `BinderStack::as_slice` gives them), is built as `label`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValueLabel {
    pub tuple: usize,
    pub binders: Vec<usize>,
    pub label: usize,
}

/// The label of every tuple a walk from each of `roots` reaches, registering the closed
/// labels in `program`.
///
/// The walk is the one a type-directed builtin makes: it peels annotations, enters each union
/// on a binder stack, follows a reference to the binder it names, and descends into a tuple's
/// fields; it goes no further into anything else. Every tuple gets an entry, a closed one
/// labelling itself, so that a lookup that misses is a bug rather than a closed tuple. Following
/// a reference cuts the stack back to where its binder was written, so the `(type, stack)`
/// pairs, and the entries, are bounded by the positions written in the roots.
pub fn value_labels(
    roots: impl IntoIterator<Item = usize>,
    program: &mut Program,
) -> Vec<ValueLabel> {
    fn walk(
        type_id: usize,
        stack: &mut BinderStack,
        program: &mut Program,
        seen: &mut HashSet<(usize, Vec<usize>)>,
        labels: &mut Vec<ValueLabel>,
    ) {
        if !seen.insert((type_id, stack.as_slice().to_vec())) {
            return;
        }
        let Some(typ) = program.lookup_type(type_id).cloned() else {
            return;
        };
        match typ {
            Type::Annotated { base, .. } => walk(base, stack, program, seen, labels),
            Type::Union(members) => {
                stack.enter(type_id);
                for member in members {
                    walk(member, stack, program, seen, labels);
                }
                stack.leave(type_id);
            }
            Type::Cycle(depth) => {
                if let Some((target, cut)) = stack.follow(depth) {
                    walk(target, stack, program, seen, labels);
                    stack.restore(cut);
                }
            }
            Type::Tuple(tuple) => {
                let fragment = program.register_type(Type::Tuple(tuple));
                let closed = close_against(fragment, stack.as_slice(), program);
                let Some(Type::Tuple(label)) = program.lookup_type(closed).cloned() else {
                    unreachable!("closing a tuple type answers a tuple type");
                };
                labels.push(ValueLabel {
                    tuple,
                    binders: stack.as_slice().to_vec(),
                    label,
                });
                let fields = program
                    .lookup_tuple(tuple)
                    .expect("a tuple type's tuple is in the table")
                    .fields
                    .clone();
                for (_, field) in fields {
                    walk(field, stack, program, seen, labels);
                }
            }
            Type::Integer
            | Type::Binary
            | Type::Partial { .. }
            | Type::Callable { .. }
            | Type::Process { .. }
            | Type::Resource(_)
            | Type::Reference
            | Type::Variable(_)
            | Type::Top => {}
        }
    }

    let mut seen = HashSet::new();
    let mut labels = Vec::new();
    for root in roots {
        walk(
            root,
            &mut BinderStack::default(),
            program,
            &mut seen,
            &mut labels,
        );
    }
    labels
}

/// An instantiation's labels, indexed for the builtin's lookups: per tuple, the stacks it is
/// reached inside (usually one) and its label at each.
#[derive(Debug, Default)]
pub struct LabelTable(HashMap<usize, Vec<(Vec<usize>, usize)>>);

impl LabelTable {
    pub fn new(labels: &[ValueLabel]) -> Self {
        let mut table: HashMap<usize, Vec<(Vec<usize>, usize)>> = HashMap::new();
        for entry in labels {
            table
                .entry(entry.tuple)
                .or_default()
                .push((entry.binders.clone(), entry.label));
        }
        Self(table)
    }

    /// The label to build `tuple` with, reached inside `stack`.
    pub fn label(&self, tuple: usize, stack: &BinderStack) -> Result<usize, Error> {
        self.0
            .get(&tuple)
            .and_then(|positions| {
                positions
                    .iter()
                    .find(|(binders, _)| binders == stack.as_slice())
            })
            .map(|(_, label)| *label)
            .ok_or(Error::LabelUndefined { tuple })
    }
}
