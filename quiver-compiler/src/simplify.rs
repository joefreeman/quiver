//! Redundant-block removal, shared by the compiler and the formatter.
//!
//! A `{ … }` is *redundant* — semantically a complete no-op — when it is a single branchless branch
//! wrapping a single non-empty chain that introduces no bindings. Such a block only scopes variables
//! (none here) and acts as a type-checker narrowing barrier; dropping it and splicing its terms into
//! the surrounding chain preserves behaviour (verified against the VM: it removes only wasted
//! `Store`/`Load`/`Reset` bytecode and preserves tail-call TCO).
//!
//! The compiler strips every redundant block so block presence never affects compiled output; the
//! formatter strips them for readability but passes a `keep` predicate to retain any block carrying
//! comments or blank lines, so that trivia is not lost.

use crate::ast::*;

/// How [`normalize_blocks`] treats a block.
pub struct Options<'a> {
    /// Consulted for every redundant single-chain block (given its body chain); return `true` to
    /// retain it anyway (the formatter keeps blocks that carry comments/blank lines).
    pub keep: &'a dyn Fn(&Chain) -> bool,
    /// Also **lift** a multi-step no-binding block — a sole-term block forming a sequence step — by
    /// splicing its chains into the enclosing sequence. Such a block is a runtime no-op (a single
    /// branch with no bindings needs no frame, whatever its step count), but cannot be spliced into
    /// a *chain* as `keep`-style stripping does. The compiler enables this to drop the frame; the
    /// formatter does not — it keeps the braces as grouping context.
    pub lift: bool,
    /// **Add** grouping braces: wrap a multi-step, frame-free branch consequence in a block, so a
    /// compound consequence reads as a delimited body (`=> { a, b }`) instead of a bare `=> a, b`
    /// whose later steps look like outer steps. The formatter enables this; the compiler does not
    /// (it would only lift the block straight back out). Frame-free keeps it bytecode-neutral.
    pub group_consequences: bool,
}

/// Normalize the blocks throughout `sequence` — a whole program, or any nested branch body:
/// remove the presentational ones (splice a single-chain redundant block's terms into its chain,
/// and with `options.lift` lift a multi-step no-binding block's chains into its sequence) and,
/// with `options.group_consequences`, add grouping braces around compound branch consequences.
pub fn normalize_blocks(sequence: Sequence, options: &Options) -> Sequence {
    let mut steps = Vec::with_capacity(sequence.steps.len());
    for step in sequence.steps {
        // A type alias has no value terms to simplify.
        let chain = match step {
            Step::Chain(chain) => chain,
            type_alias => {
                steps.push(type_alias);
                continue;
            }
        };
        let chain = strip_chain(chain, options);
        // Lift a multi-step no-binding block that is a sequence step's sole term, splicing its
        // (already-simplified) steps into this sequence so the compiler emits no frame for it. A
        // single-step such block was already spliced into the chain by `strip_chain`.
        if options.lift
            && chain.binding.is_none()
            && matches!(chain.terms.as_slice(), [term] if is_liftable_block(term))
        {
            let Some(Term::Block(mut block)) = chain.terms.into_iter().next() else {
                unreachable!("matched a sole block term")
            };
            steps.extend(block.branches.remove(0).condition.steps);
        } else {
            steps.push(Step::Chain(chain));
        }
    }
    Sequence { steps }
}

fn strip_chain(chain: Chain, options: &Options) -> Chain {
    let Chain {
        binding,
        binding_span,
        span,
        terms,
    } = chain;
    let last_index = terms.len().saturating_sub(1);
    let mut simplified = Vec::with_capacity(terms.len());
    for (index, term) in terms.into_iter().enumerate() {
        // Simplify each term bottom-up first, so nested redundant blocks collapse before the parent
        // is tested; then splice a redundant block's body terms in place of the block.
        let term = strip_term(term, options);
        let strip = is_redundant_block(&term) && {
            let Term::Block(block) = &term else {
                unreachable!("redundant implies a block")
            };
            let body = block.branches[0]
                .condition
                .single_chain()
                .expect("redundant implies a single chain step");
            // A body ending in a tail call may only be spliced when the block is the chain's last
            // term, so the `^` stays final rather than gaining dead code after it.
            let ends_in_tail_call = body.terms.last().is_some_and(is_tail_call);
            !(options.keep)(body) && (!ends_in_tail_call || index == last_index)
        };
        if strip {
            let Term::Block(mut block) = term else {
                unreachable!("redundant implies a block")
            };
            let mut condition = block.branches.remove(0).condition;
            let Step::Chain(body) = condition.steps.remove(0) else {
                unreachable!("redundant implies a single chain step")
            };
            simplified.extend(body.terms);
        } else {
            simplified.push(term);
        }
    }
    Chain {
        binding,
        binding_span,
        span,
        terms: simplified,
    }
}

fn strip_term(term: Term, options: &Options) -> Term {
    match term {
        Term::Tuple(mut tuple) => {
            tuple.fields = tuple
                .fields
                .into_iter()
                .map(|mut field| {
                    if let FieldValue::Chain(chain) = field.value {
                        field.value = FieldValue::Chain(strip_chain(chain, options));
                    }
                    field
                })
                .collect();
            Term::Tuple(tuple)
        }
        Term::Block(block) => Term::Block(strip_block(block, options)),
        Term::String(style, segments) => Term::String(
            style,
            segments
                .into_iter()
                .map(|segment| match segment {
                    StrSegment::Hole(block) => StrSegment::Hole(strip_block(block, options)),
                    text => text,
                })
                .collect(),
        ),
        Term::Function(mut function) => {
            function.body = function.body.map(|body| strip_block(body, options));
            Term::Function(function)
        }
        Term::Spawn(inner, arg, span) => Term::Spawn(
            Box::new(strip_term(*inner, options)),
            arg.map(|arg| Box::new(strip_term(*arg, options))),
            span,
        ),
        Term::Apply(access, arg) => Term::Apply(access, Box::new(strip_term(*arg, options))),
        Term::Select(Some(chains), span) => Term::Select(
            Some(
                chains
                    .into_iter()
                    .map(|chain| strip_chain(chain, options))
                    .collect(),
            ),
            span,
        ),
        other => other,
    }
}

fn strip_block(block: Block, options: &Options) -> Block {
    Block {
        annotations: block
            .annotations
            .into_iter()
            .map(|annotation| Annotation {
                value: strip_chain(annotation.value, options),
                ..annotation
            })
            .collect(),
        branches: block
            .branches
            .into_iter()
            .map(|branch| Branch {
                condition: normalize_blocks(branch.condition, options),
                consequence: branch.consequence.map(|consequence| {
                    let consequence = normalize_blocks(consequence, options);
                    if options.group_consequences {
                        group_consequence(consequence)
                    } else {
                        consequence
                    }
                }),
            })
            .collect(),
    }
}

/// Wrap a multi-step, frame-free consequence sequence in a block — `a, b` → `{ a, b }`. A single
/// step (or a consequence that already wraps the steps in a block) is one chain and left untouched;
/// a binding/matching consequence is not frame-free and keeps its own markers.
fn group_consequence(consequence: Sequence) -> Sequence {
    let all_chains = consequence
        .steps
        .iter()
        .all(|step| step.as_chain().is_some_and(is_frame_free_chain));
    if consequence.steps.len() > 1 && all_chains {
        let block = Term::Block(Block {
            annotations: vec![],
            branches: vec![Branch {
                condition: consequence,
                consequence: None,
            }],
        });
        Sequence::from_chains([Chain {
            binding: None,
            binding_span: Spanned::default(),
            span: Spanned::default(),
            terms: vec![block],
        }])
    } else {
        consequence
    }
}

/// Whether `term` is a redundant block whose braces can be dropped: one branch, no `=>`, and a
/// single non-empty chain that is safe to inline (see [`is_inlinable_chain`]). Whether it is *also*
/// safe given its surrounding position (tail call as the last term) is checked by `strip_chain`.
///
/// `single_chain` also excludes a block that declares a type alias: dropping those braces would
/// widen the alias's scope to the enclosing one, where it could shadow an outer alias of the same
/// name.
pub fn is_redundant_block(term: &Term) -> bool {
    matches!(term, Term::Block(block)
    if block.annotations.is_empty()
        && block.branches.len() == 1
        && block.branches[0].consequence.is_none()
        && block.branches[0].condition.single_chain().is_some_and(is_inlinable_chain))
}

/// A chain whose terms can be spliced out of a redundant block: non-empty, frame-free, and with no
/// tail call before its final term (that would become visibly unreachable mid-chain once spliced).
fn is_inlinable_chain(chain: &Chain) -> bool {
    !chain.terms.is_empty() && is_frame_free_chain(chain) && !has_nonfinal_tail_call(&chain.terms)
}

/// A chain that needs no block frame of its own: it binds nothing and contains no in-chain match. A
/// match is excluded because a block disables complement narrowing (see `compiler.rs`), so a block
/// wrapping a match can be a deliberate narrowing barrier; keeping it avoids changing type-checking.
fn is_frame_free_chain(chain: &Chain) -> bool {
    chain.binding.is_none() && !chain.terms.iter().any(contains_match)
}

/// Whether `term` is a multi-step block whose runtime frame is unnecessary, so its steps may be
/// lifted into the enclosing sequence: no annotations (they attach to the block's result, so the
/// braces are load-bearing), one branch, no `=>`, and every step a frame-free chain. (A single
/// branch with no bindings needs no frame regardless of step count; a tail call is fine, since
/// lifting into a sequence keeps it in tail position rather than mid-chain.) Requiring *chain*
/// steps also keeps a type alias in the scope that declares it — lifting would widen it.
fn is_liftable_block(term: &Term) -> bool {
    matches!(term, Term::Block(block)
        if block.annotations.is_empty()
            && block.branches.len() == 1
            && block.branches[0].consequence.is_none()
            && !block.branches[0].condition.steps.is_empty()
            && block.branches[0]
                .condition
                .steps
                .iter()
                .all(|step| step.as_chain().is_some_and(is_frame_free_chain)))
}

/// Whether a term contains an in-chain match or binding in its own (enclosing) scope. Recurses
/// through tuples and select sources (same scope) but stops at blocks and functions (their own
/// scopes — their matches/bindings do not leak when an outer block is removed).
fn contains_match(term: &Term) -> bool {
    match term {
        Term::Match(_) => true,
        Term::Tuple(tuple) => tuple.fields.iter().any(|field| match &field.value {
            FieldValue::Chain(chain) => {
                chain.binding.is_some() || chain.terms.iter().any(contains_match)
            }
            FieldValue::Spread(_) => false,
        }),
        Term::Select(Some(chains), _) => chains
            .iter()
            .any(|chain| chain.binding.is_some() || chain.terms.iter().any(contains_match)),
        _ => false,
    }
}

/// Whether a term is a tail call (`^`, `^f`, `^~`).
fn is_tail_call(term: &Term) -> bool {
    matches!(
        term,
        Term::Access(Access {
            source: Some(AccessSource::TailCall(_) | AccessSource::TailCallRipple),
            ..
        })
    )
}

/// Whether `terms` has a tail call anywhere but its last position — already-dead code that, spliced
/// into a longer chain, would sit confusingly mid-chain.
fn has_nonfinal_tail_call(terms: &[Term]) -> bool {
    terms.len() > 1 && terms[..terms.len() - 1].iter().any(is_tail_call)
}
