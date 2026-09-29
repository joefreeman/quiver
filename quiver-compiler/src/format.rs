//! An AST pretty-printer that renders a parsed [`Sequence`] back to canonical Quiver source.
//!
//! The AST is rendered to a [`crate::pretty`] document and laid out against a fixed target
//![`WIDTH`]: a construct stays on one line if it fits, otherwise it breaks using its own
//! reparse-safe separator — chains continue with `~>`, blocks lead each branch with `|`, tuples
//! and multi-chain sequences put one item per line.
//!
//! Two layout choices are the author's rather than the width's, read back from the source text:
//! steps written on separate lines stay on separate lines (only `;`-joined steps are joined), and a
//! bracketed list written with a trailing comma stays one item per line.
//!
//! The formatter changes only whitespace and rendering choices that re-parse to the identical AST
//! (`$0` sugar, parenthesised unions, string literals), plus block *normalization*: it drops
//! redundant blocks (like redundant parentheses, via [`crate::simplify`]) and adds grouping braces
//! around a branch body that would otherwise sprawl — a compound consequence, or a single chain that
//! breaks into a `~>` pipeline (`wrap_breaking_body`). Every block it adds or removes is one the
//! compiler treats as a runtime no-op (it strips/lifts them), so formatting never changes compiled
//! output — it is bytecode-preserving (`compile(parse(format(src))) == compile(parse(src))`).
//!
//! Atomic constructs that never break — accesses, literals, most patterns — are rendered to plain
//! strings and wrapped as [`pretty::text`]; only the breakable layers (including types, and
//! tuple patterns) build structured docs.

use crate::ast::*;
use crate::pretty::{self, Doc};
use std::collections::HashMap;

/// Target line width: groups that would exceed this many columns are broken.
const WIDTH: usize = 100;

/// A chain whose single-line form exceeds this many columns is split onto `~>` continuation lines
/// even when it would fit within [`WIDTH`] — a long pipeline reads better as one step per line. Well
/// below [`WIDTH`], since the point is to stack a pipeline that has become hard to scan rather than
/// to avoid an overflow; but not so low that an everyday two- or three-call chain is taken apart,
/// which costs more than the stacking gains. (Group B, tunable.)
const CHAIN_SOFT_WIDTH: usize = 70;

/// A multi-branch block whose single-line form exceeds this many columns is split one branch per
/// line; shorter blocks stay inline. (Group B, tunable.)
const SHORT_BLOCK_WIDTH: usize = 40;

/// A `cond => …` guard whose single-line form exceeds this many columns is laid out one step per
/// line. Measured on the guard alone: what follows it on that line — the ` => `, and however much of
/// the consequence lands before the consequence's own first break — is not known here, so this is
/// the budget left for it rather than a column the guard may reach. (Group B, tunable.)
const GUARD_SOFT_WIDTH: usize = 60;

/// Render a program to canonical source. `source` is the program's original text, from which
/// comments and blank lines (trivia) are recovered and re-attached, since the parser discards them.
pub fn format_program(program: &Sequence, source: &str) -> String {
    let trivia = Trivia::collect(program, source);
    // Drop redundant blocks for readability, keeping any that carry comments/blank lines so their
    // trivia is not lost. Trivia is collected first, from the original AST, so its source offsets
    // still resolve against the surviving nodes. The compiler strips the same blocks
    // (unconditionally) before codegen, so this never changes the compiled output.
    let program = crate::simplify::normalize_blocks(
        program.clone(),
        &crate::simplify::Options {
            keep: &|chain| trivia.has_trivia(chain.span),
            // The formatter keeps multi-step no-binding blocks as grouping context (only the
            // compiler lifts them, where they are a runtime no-op), and adds such braces around a
            // bare compound consequence.
            lift: false,
            group_consequences: true,
            // Calls take their preferred spelling, except across a `~>` carrying comments.
            restyle_calls: Some(&|gap| !trivia.has_trivia(gap.end) && !trivia.has_trivia(gap.pipe)),
        },
    );
    // The program is one sequence, rendered exactly as a block body's is.
    let mut docs = vec![sequence_doc(
        &trivia,
        &Sequence {
            steps: program.steps,
        },
        false,
        0,
        true,
    )];
    // Comments after the last step have nowhere to attach, so emit them at the end.
    if !trivia.dangling.is_empty() {
        docs.push(trivia_doc(&trivia.dangling));
    }
    let doc = pretty::join(pretty::hardline(), docs);
    collapse_blanks(&pretty::print(&doc, WIDTH))
}

/// A type-alias declaration step. `break_parent` keeps a sequence that declares a type broken —
/// an alias reads as its own line, never run together with the steps around it.
fn type_alias_doc(
    trivia: &Trivia,
    name: &Option<String>,
    type_parameters: &[TypeParameter],
    type_definition: &Type,
) -> Doc {
    let mut lhs = String::from("'");
    if let Some(name) = name {
        lhs.push_str(name);
    }
    lhs.push_str(&render_type_parameters(type_parameters));
    lhs.push_str(" =");
    // A union right-hand side breaks with a leading `|` per member; everything else stays
    // inline (its `=` already carries the trailing space the union form omits).
    let body = match type_definition {
        Type::Union(union_type) => {
            pretty::concat(vec![pretty::text(lhs), union_alias_doc(trivia, union_type)])
        }
        other => pretty::concat(vec![
            pretty::text(format!("{} ", lhs)),
            type_doc(trivia, other),
        ]),
    };
    pretty::concat(vec![body, pretty::break_parent()])
}

/// The right-hand side of a union type alias: `= A | B | C` flat, or each member on its own line
/// led by `|` when broken, indented under the alias name.
fn union_alias_doc(trivia: &Trivia, union_type: &UnionType) -> Doc {
    let mut parts = Vec::new();
    for (index, member) in union_type.types.iter().enumerate() {
        parts.push(pretty::line());
        parts.push(leading_bar(index == 0));
        // Indent the member itself, so a member that breaks puts its fields under its own name
        // and its closing delimiter in line with it, rather than back in the `|` gutter.
        parts.push(pretty::nest(2, union_member_doc(trivia, member)));
    }
    pretty::group(pretty::nest(2, pretty::concat(parts)))
}

/// The `| ` that precedes each branch/union member, following the separating [`pretty::line`]. When
/// broken, every item is led by `| `; when flat, the first has none (`{ a | b }`) and later ones the
/// usual ` | ` (the leading space comes from the preceding `line`).
fn leading_bar(first: bool) -> Doc {
    if first {
        pretty::if_break(pretty::text("| "), pretty::nil())
    } else {
        pretty::text("| ")
    }
}

/// `<'a, 'b: bound>` for a non-empty parameter list, otherwise empty. Type parameters are stored
/// without their `'` prefix, so it is re-added here.
fn render_type_parameters(params: &[TypeParameter]) -> String {
    if params.is_empty() {
        return String::new();
    }
    format!(
        "<{}>",
        params
            .iter()
            .map(|param| match &param.bound {
                Some(bound) => format!("'{}: {}", param.name, render_type(bound)),
                None => format!("'{}", param.name),
            })
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// A sequence of steps, each preceded by any leading comments and blank lines it carries. Steps
/// keep the lines the author gave them; see [`sequence_parts`].
///
/// `skip_first_leading` drops the leading trivia of the first chain — used when a caller (a
/// multi-branch block) has already emitted it elsewhere (before the branch's `|`).
///
/// `continuation_nest` indents every line the sequence breaks onto by that many spaces — its later
/// steps and the first step's own wrapped lines alike, so a step's `~>` continuations stay in line
/// with the steps around it. The first step itself opens on the caller's line, so it is not moved.
/// An arm under a `| ` uses 2 so its lines align under the content past the bar (see
/// [`arm_body_doc`]).
///
/// `set_off_tall` allows blank lines around a step that breaks across lines
/// ([`breaks_into_pipeline`]). A branch *condition* clears it: its steps are one gating unit — a
/// match and the guards that qualify it — and a blank line between them would read as a break in the
/// program's flow that isn't there.
fn sequence_doc(
    trivia: &Trivia,
    sequence: &Sequence,
    skip_first_leading: bool,
    continuation_nest: usize,
    set_off_tall: bool,
) -> Doc {
    pretty::group(sequence_parts(
        trivia,
        sequence,
        skip_first_leading,
        continuation_nest,
        set_off_tall,
    ))
}

/// [`sequence_doc`]'s content without the enclosing group, for a caller that must compose its own
/// break decision with the steps' — a branch condition, which leans on [`break_if_wider_than`] and
/// then has to know the verdict in order to indent to match.
fn sequence_parts(
    trivia: &Trivia,
    sequence: &Sequence,
    skip_first_leading: bool,
    continuation_nest: usize,
    set_off_tall: bool,
) -> Doc {
    // The author decides where steps break. Steps written on one line, `;`-joined, form a *run*
    // that stays joined while it fits and otherwise breaks one step per line — a run is all or
    // nothing, so a step that breaks across lines never leaves `; next` dangling after it. Steps
    // written on separate lines stay on separate lines. Semicolon and newline are synonymous
    // step separators, so a broken separator is a bare newline (the lighter form) and only the
    // inline form needs the semicolon.
    let joined = || {
        pretty::concat(vec![
            pretty::if_break(pretty::nil(), pretty::text(";")),
            pretty::line(),
        ])
    };
    // Between runs: a line break forced by `break_parent` rather than a hard line, so a
    // flattened rendering (an interpolation hole) still reads back as `; `.
    let fresh_line = || pretty::concat(vec![joined(), pretty::break_parent()]);
    let mut runs: Vec<Vec<Doc>> = Vec::new();
    let mut gaps: Vec<Doc> = Vec::new();
    let mut prev_tall = false;
    for (index, step) in sequence.steps.iter().enumerate() {
        let span = step.span();
        let leading = if index == 0 && skip_first_leading {
            pretty::nil()
        } else {
            trivia.leading_doc(span)
        };
        let (body, tall) = match step {
            Step::Chain(chain) => {
                let body = chain_doc(trivia, chain);
                // A step laid out as `~>` continuation lines has nothing delimiting it, so it is
                // set off from its neighbours with a blank line.
                let tall = set_off_tall && breaks_into_pipeline(trivia, chain);
                // The assertions ending the step's last line ride after the chain; they do not
                // make a step "tall". A trailing assertion is glued to the step's line; an
                // own-line one (a leading `//=`) starts a fresh line at the step's indent,
                // keeping any comments written above it. Each is an anchor (see `visit_chain`),
                // so a prose note written after one — an ordinary trailing comment — follows it
                // rather than the step. Assertions written above a `~>` continuation sit inside
                // the chain, and `chain_terms_doc` has already placed them on the line whose
                // value they observe.
                // A comment trailing the step belongs to the line the chain ends on — the line a
                // trailing assertion finishes. It is a line suffix, so emitting it here, ahead
                // of the assertions, still renders it after them, while keeping it off any
                // own-line assertions stacked below.
                let mut parts = vec![body, trivia.trailing_doc(span)];
                let final_assertions = chain
                    .assertions
                    .iter()
                    .filter(|assertion| assertion.after == chain.terms.len());
                for (index, assertion) in final_assertions.enumerate() {
                    // The first assertion of an assertion-only step opens the step itself, so
                    // the step-level trivia above already covers it — both the leading comments
                    // and, sharing the step's offset, its own trailing note.
                    let opens_step = index == 0 && chain.terms.is_empty();
                    let own_line = assertion.own_line && !opens_step;
                    if own_line {
                        parts.push(pretty::hardline());
                        parts.push(trivia.leading_doc(assertion.span));
                    }
                    parts.push(assertion_doc(assertion, own_line || opens_step));
                    if !opens_step {
                        parts.push(trivia.trailing_doc(assertion.span));
                    }
                }
                (pretty::concat(parts), tall)
            }
            Step::TypeAlias {
                name,
                type_parameters,
                type_definition,
                ..
            } => (
                pretty::concat(vec![
                    type_alias_doc(trivia, name, type_parameters, type_definition),
                    trivia.trailing_doc(span),
                ]),
                false,
            ),
        };
        let item = pretty::concat(vec![leading, body]);
        if index == 0 {
            runs.push(vec![item]);
        } else if prev_tall || tall {
            // Set a tall step off from its neighbours with a blank line (two newlines). `collapse_
            // blanks` caps a run at one, so this composes with any blank the author already left.
            gaps.push(pretty::concat(vec![pretty::hardline(), pretty::hardline()]));
            runs.push(vec![item]);
        } else if trivia.starts_line(span) {
            gaps.push(fresh_line());
            runs.push(vec![item]);
        } else {
            let run = runs.last_mut().expect("the first step opens a run");
            run.push(joined());
            run.push(item);
        }
        prev_tall = tall;
    }
    // A lone run is left to the caller's group, which may compose its own break decision with it
    // (a branch condition's soft width). Several runs are each their own group, fitting or
    // breaking independently of the lines around them.
    let single = runs.len() == 1;
    let mut parts = Vec::new();
    for (index, run) in runs.into_iter().enumerate() {
        if index > 0 {
            parts.push(gaps[index - 1].clone());
        }
        let run = pretty::concat(run);
        parts.push(if single { run } else { pretty::group(run) });
    }
    pretty::nest(continuation_nest, pretty::concat(parts))
}

/// A braced expression `{ … }`. A single branch lays its body out directly; multiple branches each
/// get a leading `|` when broken. The body indents two spaces from the line carrying the `{`.
fn block_doc(trivia: &Trivia, block: &Block) -> Doc {
    let branches = &block.branches;
    // The annotation prefix (`:key value` entries). With a body following, each annotation ends
    // in a hardline — the semicolon/newline separator is what ends an annotation's value chain, so
    // a flat space-joined rendering would re-parse differently. An annotation-only block may
    // stay flat (`{ :error X }`).
    let annotation_parts: Vec<Doc> = block
        .annotations
        .iter()
        .enumerate()
        .map(|(index, annotation)| {
            pretty::concat(vec![
                if !branches.is_empty() {
                    pretty::hardline()
                } else if index == 0 {
                    pretty::line()
                } else {
                    // Annotations are steps: a flat layout needs the semicolon separator
                    // (`{ :a X; :b Y }`) — space-joined, the second `:b` re-parses as
                    // part of the first annotation's value chain. Like sequence steps,
                    // a broken layout uses the bare newline.
                    pretty::concat(vec![
                        pretty::if_break(pretty::nil(), pretty::text(";")),
                        pretty::line(),
                    ])
                },
                trivia.leading_doc(annotation.span),
                pretty::text(format!(":{} ", annotation.name)),
                chain_doc(trivia, &annotation.value),
                trivia.trailing_doc(annotation.span),
            ])
        })
        .collect();
    if branches.is_empty() {
        return pretty::group(pretty::concat(vec![
            pretty::text("{"),
            pretty::nest(2, pretty::concat(annotation_parts)),
            pretty::line(),
            pretty::text("}"),
        ]));
    }
    let annotations = pretty::concat(annotation_parts);
    let inner = if branches.len() == 1 {
        pretty::concat(vec![
            annotations,
            pretty::line(),
            branch_doc(trivia, &branches[0], false),
        ])
    } else {
        let mut parts = Vec::new();
        for (index, branch) in branches.iter().enumerate() {
            // A branch's leading comments go *before* its `|` (a comment after the bar would
            // re-parse as a trailing comment of the bar), so they are hoisted out of `branch_doc`.
            let leading = branch
                .condition
                .steps
                .first()
                .map_or_else(pretty::nil, |step| trivia.leading_doc(step.span()));
            parts.push(pretty::line());
            parts.push(leading);
            parts.push(leading_bar(index == 0));
            // `branch_doc` does its own indenting: it aligns a guard's wrapped lines under the
            // content (past the `| `), but lets a consequence block's `}` align with the bar line.
            parts.push(branch_doc(trivia, branch, true));
        }
        // Lean a multi-branch block toward one-branch-per-line unless it is very short.
        pretty::concat(vec![
            annotations,
            break_if_wider_than(pretty::concat(parts), SHORT_BLOCK_WIDTH),
        ])
    };
    pretty::group(pretty::concat(vec![
        pretty::text("{"),
        pretty::nest(2, inner),
        pretty::line(),
        pretty::text("}"),
    ]))
}

/// `multi_branch` is set when this branch sits under a broken multi-branch block (led by `| `): its
/// continuation lines then indent two spaces to align under the content past the bar, and the first
/// chain's leading trivia is dropped (the block already emitted it before the `|`).
fn branch_doc(trivia: &Trivia, branch: &Branch, multi_branch: bool) -> Doc {
    let nest = if multi_branch { 2 } else { 0 };
    match &branch.consequence {
        None => {
            // A bare body under a lone branch is the block's whole body, laid out as a function
            // body is; under a `| ` it is one arm among several, which a blank line would split.
            // A single-chain arm keeps its wrapped lines at the bar, so a tuple it ends in closes
            // under the `|`; only a compound arm's steps move past it, to line up with each other.
            let arm_nest = if branch.condition.single_chain().is_some() {
                0
            } else {
                nest
            };
            let (body, _) = arm_body_doc(
                trivia,
                &branch.condition,
                multi_branch,
                multi_branch,
                arm_nest,
                !multi_branch,
            );
            body
        }
        Some(consequence) => {
            // The guard's own wrapped lines are indented by the `nest` below rather than by
            // `continuation_nest`, so a broken chain's `~>` lines and a later step land at the same
            // column — one is the continuation of the other's line, and staggering them would read
            // as structure the guard does not have.
            // A guard past [`GUARD_SOFT_WIDTH`] is laid out one step per line. Below it the guard is
            // flattened onto one line instead, so that a consequence too wide for what is left of
            // the line breaks *itself* rather than dragging the guard apart with it — a guard reads
            // as one thing, and splitting it to make room for a tuple would be the wrong trade.
            let condition = break_if_wider_than(
                sequence_parts(trivia, &branch.condition, multi_branch, 0, false),
                GUARD_SOFT_WIDTH,
            );
            // A consequence opens mid-line, after the ` => `, and is placed under the arm's content
            // by the `nest` below. A single chain breaks there, so a closing delimiter lines up with
            // the line that opened it; the steps of a compound one go a level deeper, reading as the
            // consequence rather than as more of the guard. They are never set off with blank
            // lines, which would detach the later steps from the arm they belong to.
            let steps_nest = if consequence.steps.len() > 1 { 2 } else { 0 };
            let (body, delimited) =
                arm_body_doc(trivia, consequence, false, multi_branch, steps_nest, false);
            // The threshold decides by width, but a guard carrying a comment or a breaking pipeline
            // forces a break of its own, and flattening that would comment out / collapse the rest of
            // the line. Either way the verdict is `forces_break`, so the two agree.
            if pretty::forces_break(&condition) {
                let content =
                    pretty::concat(vec![pretty::group(condition), pretty::text(" => "), body]);
                // A broken guard indents its wrapped lines under the head (past the `| `), *and* the
                // consequence that follows the last one on the same line — so the whole
                // `cond => consequence` is nested together, keeping the consequence aligned with the
                // line it opens on.
                pretty::nest(nest, content)
            } else {
                let content = pretty::concat(vec![
                    pretty::text(pretty::flatten(&condition)),
                    pretty::text(" => "),
                    body,
                ]);
                // A delimited consequence closes at the bar, in line with the `|`. Anything else
                // that breaks indents under the arm's content, so its closing delimiter lines up
                // with the term that opened it instead of landing in the bar's gutter.
                if delimited {
                    content
                } else {
                    pretty::nest(nest, content)
                }
            }
        }
    }
}

/// A branch arm's body — a branch without `=>`, or a consequence — and whether it is *delimited*:
/// a single chain that carries the arm's own shape, its closing `}` landing at the bar in line with
/// the `|`. That is a chain ending in a body it opens itself, or a pipeline that
/// [`wrap_breaking_body`] puts in braces.
///
/// Anything else lays every line it breaks onto at `nest` — its later steps and a step's own wrapped
/// lines alike, so the arm's steps line up with each other and a step's `~>` continuations with its
/// neighbours.
fn arm_body_doc(
    trivia: &Trivia,
    sequence: &Sequence,
    skip_first_leading: bool,
    multi_branch: bool,
    nest: usize,
    set_off_tall: bool,
) -> (Doc, bool) {
    let delimited = sequence.single_chain().is_some_and(|chain| {
        chain.terms.last().is_some_and(opens_a_body)
            || (multi_branch && breaks_into_pipeline(trivia, chain))
    });
    let nest = if delimited { 0 } else { nest };
    let body = sequence_doc(trivia, sequence, skip_first_leading, nest, set_off_tall);
    (
        wrap_breaking_body(trivia, sequence, body, multi_branch),
        delimited,
    )
}

/// Wrap a branch body under a `| ` bar in grouping braces when it is a single chain that will break
/// across lines as a `~>` pipeline: its continuation would otherwise dangle at the bar indent,
/// reading like a new step. The brace block is a frame-free single chain, which both the compiler
/// and the formatter's own strip pass remove — so this render-time wrap is bytecode-neutral and
/// idempotent. `body` is the already-rendered doc.
fn wrap_breaking_body(trivia: &Trivia, sequence: &Sequence, body: Doc, multi_branch: bool) -> Doc {
    let pipeline = sequence
        .single_chain()
        .is_some_and(|chain| breaks_into_pipeline(trivia, chain));
    if multi_branch && pipeline {
        pretty::concat(vec![
            pretty::text("{"),
            pretty::nest(2, pretty::concat(vec![pretty::hardline(), body])),
            pretty::hardline(),
            pretty::text("}"),
        ])
    } else {
        body
    }
}

/// Whether a chain lays out as `~>` continuation lines — a run of terms one per line, with nothing
/// delimiting it. This is the shape every caller here reacts to: a branch body wraps it in braces, a
/// binding breaks after its `=` and indents it, and a sequence sets it off with blank lines.
///
/// It asks of exactly the part [`chain_doc`] lets break at its gaps what [`break_if_wider_than`]
/// asks: a chain ending in a container breaks only in the head that precedes it (the container
/// delimits its own contents), so the head is what is measured; any other chain is measured whole.
/// Like that threshold it cannot see the column the chain will start at, so a chain that fits the
/// soft width but overflows where it lands still reads as one that does not break.
fn breaks_into_pipeline(trivia: &Trivia, chain: &Chain) -> bool {
    let terms = &chain.terms;
    if terms.len() < 2 {
        return false;
    }
    let breakable =
        if is_breakable_container(&terms[terms.len() - 1]) && !has_interior_trivia(trivia, chain) {
            terms.len() - 1
        } else {
            terms.len()
        };
    let doc = chain_terms_doc(trivia, chain, breakable);
    pretty::flat_width(&doc, CHAIN_SOFT_WIDTH).is_none()
}

/// Render a `//= P` assertion: just the canonical pattern, since a prose note after one is an
/// ordinary trailing comment that the caller emits from the trivia attached to the assertion.
/// An assertion terminates its line like a comment, so every form forces the enclosing construct
/// to break — a closing `}` must never land after one. An assertion rendered at the start of its
/// line (`bare`) needs no separating space: the line is its own.
fn assertion_doc(assertion: &Assertion, bare: bool) -> Doc {
    let text = format!("//= {}", render_match(&assertion.pattern));
    let text = if bare { text } else { format!(" {text}") };
    pretty::concat(vec![pretty::text(text), pretty::break_parent()])
}

/// A chain: an optional `pattern = ` binding followed by `~>`-joined terms. When the terms do not
/// fit, they break with a leading `~>` per continuation line. A trailing breakable container (a
/// block, tuple, or function) is kept attached to the preceding terms and allowed to break
/// internally, rather than forcing the whole chain onto `~>` lines.
fn chain_doc(trivia: &Trivia, chain: &Chain) -> Doc {
    let terms = &chain.terms;
    let value = if terms.len() > 1
        && is_breakable_container(&terms[terms.len() - 1])
        && !has_interior_trivia(trivia, chain)
    {
        // A chain ending in a container (`head { … }`, `head [ … ]`) lets the container break
        // internally rather than pushing the whole chain onto `~>` lines. The head is its own group,
        // measured against the line the container *opens* on — `fits` reaches the container in break
        // mode and stops at its first break opportunity — so the head stays flat while it fits there
        // and breaks at its own `~>` gaps once it does not.
        let head = pretty::group(break_if_wider_than(
            chain_terms_doc(trivia, chain, terms.len() - 1),
            CHAIN_SOFT_WIDTH,
        ));
        pretty::concat(vec![head, term_doc(trivia, &terms[terms.len() - 1])])
    } else {
        pretty::group(break_if_wider_than(
            chain_terms_doc(trivia, chain, terms.len()),
            CHAIN_SOFT_WIDTH,
        ))
    };
    let Some(pattern) = &chain.binding else {
        return value;
    };
    // A binding whose value lays out as a `~>` pipeline breaks *after* the `=` and indents the whole
    // value, so the continuations sit under it rather than dangling at the binding's own indent —
    // where they read as steps of the enclosing sequence rather than as part of this one. The break
    // and the pipeline are the same decision, so a value that stays on the `=` line is one that also
    // stays on a single line, and a value that leaves it puts every term on its own line. A value
    // that only breaks *inside* a trailing container (`xs = [ … ]`, `y = head [ … ]`) is not a
    // pipeline: the container opens on the `=` line and delimits itself.
    if breaks_into_pipeline(trivia, chain) {
        return pretty::group(pretty::concat(vec![
            match_doc(trivia, pattern),
            pretty::text(" ="),
            pretty::nest(2, pretty::concat(vec![pretty::line(), value])),
        ]));
    }
    pretty::concat(vec![match_doc(trivia, pattern), pretty::text(" = "), value])
}

/// Lay out the first `count` terms of a chain, joined by an explicit `~>` between every pair of
/// adjacent terms. Every gap is a break point, rendered as a leading-`~>` continuation line, and
/// they all share one group — so a chain that does not fit puts *every* term on its own line rather
/// than breaking at some gaps and running several terms together at others.
///
/// Stopping short of the last term (`count < terms.len()`, the trailing-container layout) still
/// emits the gap that follows, so the container joins the break: the caller's group holds the whole
/// `head ~> ` run, and the container either sits after it on one line or starts a line of its own.
///
/// A gap also carries whatever the author wrote at the end of the line it continues — a comment,
/// a `//= P` assertion — and those run to the end of that line, so the gap must then break
/// whatever the width says, or the `~>` would land inside the comment.
fn chain_terms_doc(trivia: &Trivia, chain: &Chain, count: usize) -> Doc {
    let terms = &chain.terms[..count];
    let mut parts = Vec::new();
    for (index, term) in terms.iter().enumerate() {
        let doc = term_doc(trivia, term);
        // A term that opens a *body* and will lay it out vertically — a block, a function literal —
        // provides its own structure below the line it is written on. Giving it a line of its own
        // would leave the term before it stranded on one, so the gap ahead of it does not break. A
        // call's argument list is data, not a body: it may break too, but there the break is the
        // chain's, and every term takes a line together.
        let glued = opens_a_body(term) && pretty::forces_break(&doc);
        if index > 0 {
            let gap = chain
                .continuations
                .get(index - 1)
                .cloned()
                .unwrap_or_default();
            let assertions: Vec<_> = chain
                .assertions
                .iter()
                .filter(|assertion| assertion.after == index)
                .collect();
            // A comment written after the previous term trails the gap's start; one on its own
            // line leads the `~>`. (Two anchors, because trivia attaches directionally.)
            parts.push(trivia.trailing_doc(gap.end));
            for assertion in &assertions {
                if assertion.own_line {
                    parts.push(pretty::hardline());
                    parts.push(trivia.leading_doc(assertion.span));
                }
                parts.push(assertion_doc(assertion, assertion.own_line));
                parts.push(trivia.trailing_doc(assertion.span));
            }
            let breaks =
                !assertions.is_empty() || trivia.has_trivia(gap.end) || trivia.has_trivia(gap.pipe);
            if breaks {
                parts.push(pretty::hardline());
                parts.push(trivia.leading_doc(gap.pipe));
                parts.push(pretty::text("~> "));
            } else if glued {
                parts.push(pretty::text(" ~> "));
            } else {
                parts.push(pretty::line());
                parts.push(pretty::text("~> "));
            }
        }
        parts.push(doc);
    }
    if count < chain.terms.len() {
        parts.push(pretty::line());
        parts.push(pretty::text("~> "));
    }
    pretty::concat(parts)
}

/// Whether a chain carries anything *between* its terms — a `//= P` assertion or a comment —
/// which pins the layout to the lines the author wrote and rules out any flattened rendering.
fn has_interior_trivia(trivia: &Trivia, chain: &Chain) -> bool {
    chain
        .assertions
        .iter()
        .any(|assertion| assertion.after < chain.terms.len())
        || chain
            .continuations
            .iter()
            .any(|gap| trivia.has_trivia(gap.end) || trivia.has_trivia(gap.pipe))
}

/// Force `inner` to break when its single-line form exceeds `threshold` columns (or already contains
/// a break) — a softer split target than [`WIDTH`], used to lean chains and multi-branch blocks
/// toward breaking. Measures the flat width directly, with early exit, rather than rendering it.
fn break_if_wider_than(inner: Doc, threshold: usize) -> Doc {
    if pretty::flat_width(&inner, threshold).is_some() {
        inner
    } else {
        pretty::concat(vec![inner, pretty::break_parent()])
    }
}

/// Whether a term opens a braced body — a block, a function literal, a spawned one, or a call whose
/// argument is one. [`is_breakable_container`] minus the tuple case: a tuple breaks into fields,
/// which is data laid out vertically, not a body written below the line that opens it.
fn opens_a_body(term: &Term) -> bool {
    match term {
        Term::Block(_) => true,
        Term::Function(function) => function.body.is_some(),
        Term::Spawn(inner, argument, _) => {
            argument.is_none() && matches!(inner.as_ref(), Term::Function(_))
        }
        Term::Apply(_, argument) => opens_a_body(argument),
        _ => false,
    }
}

/// Whether a term is a container that lays out vertically, so it can absorb a chain's overflow by
/// breaking internally instead of pushing the chain onto `~>` lines.
fn is_breakable_container(term: &Term) -> bool {
    match term {
        Term::Block(_) => true,
        Term::Tuple(tuple) => !tuple.fields.is_empty(),
        Term::Function(function) => function.body.is_some(),
        Term::Spawn(inner, argument, _) => {
            argument.is_none() && matches!(inner.as_ref(), Term::Function(_))
        }
        // An application whose argument is a container (`f [ … ]`, `f { … }`) can absorb
        // overflow by breaking inside the argument.
        Term::Apply(_, argument) => is_breakable_container(argument),
        _ => false,
    }
}

/// Render a single chain term. Breakable containers build structured docs; atomic terms render to a
/// string wrapped as text.
fn term_doc(trivia: &Trivia, term: &Term) -> Doc {
    match term {
        Term::Tuple(tuple) => tuple_doc(trivia, tuple),
        // A binary written across lines keeps its rows; a single-line one is atomic text.
        Term::Literal(Literal::Binary(binary)) if binary.row_count() > 1 => binary_doc(binary),
        Term::String(style, segments) => string_term_doc(trivia, *style, segments),
        Term::Block(block) => block_doc(trivia, block),
        Term::Function(function) => function_doc(trivia, function),
        Term::Spawn(inner, argument, _) => spawn_doc(trivia, inner, argument.as_deref()),
        Term::Select(sources, _) => select_doc(trivia, sources),
        Term::Dialect(dialect) => dialect_doc(dialect),
        Term::Match(pattern) => pretty::concat(vec![pretty::text("="), match_doc(trivia, pattern)]),
        // A juxtaposed application `head arg`: the head is always atomic (an access), the
        // argument may be a container.
        Term::Apply(access, argument) => pretty::concat(vec![
            pretty::text(render_access(access)),
            pretty::text(" "),
            term_doc(trivia, argument),
        ]),
        atom => pretty::text(render_term_atom(atom)),
    }
}

/// Render a dialect term. The content is preserved verbatim — re-indenting it would change what
/// the dialect function receives — so multi-line content becomes text segments joined by literal
/// lines (newlines without indentation), which the width engine counts as breaks, forcing the
/// enclosing groups open instead of separator-joining around the embedded newlines.
fn dialect_doc(dialect: &Dialect) -> Doc {
    let rendered = format!("%{}{{{}}}", dialect.path.join("/"), dialect.raw);
    if !rendered.contains('\n') {
        return pretty::text(rendered);
    }
    let segments = rendered.split('\n').map(pretty::text).collect();
    pretty::join(pretty::literalline(), segments)
}

/// Render an atomic (never-breaking) term to a string. Container terms are handled by [`term_doc`]
/// and never reach here.
fn render_term_atom(term: &Term) -> String {
    match term {
        Term::Literal(literal) => render_literal(literal),
        Term::Match(pattern) => format!("={}", render_match(pattern)),
        Term::Access(access) => render_access(access),
        Term::Process(index) => format!("@{}", index),
        Term::State(access, _) => format!("?{}", render_access(access)),
        // Dialect content is opaque raw text and is preserved verbatim (including newlines):
        // re-indenting it would change what the dialect function receives.
        Term::Dialect(dialect) => format!("%{}{{{}}}", dialect.path.join("/"), dialect.raw),
        Term::Tuple(_)
        | Term::String(..)
        | Term::Block(_)
        | Term::Function(_)
        | Term::Spawn(..)
        | Term::Select(..)
        | Term::Apply(..) => unreachable!("container terms are rendered by term_doc"),
    }
}

/// Render a string-literal term in its original delimiter style: a single-line `"…"` (with
/// interpolation holes), or a multi-line `"""…"""` block (text only).
fn string_term_doc(trivia: &Trivia, style: StringStyle, segments: &[StrSegment]) -> Doc {
    match style {
        StringStyle::Single => single_line_string_doc(trivia, segments),
        StringStyle::Multi => multiline_string_doc(trivia, segments),
    }
}

/// Render a single-line string `"…"`: literal-text segments are re-escaped, and each interpolation
/// hole is rendered flat (single-line strings stay on one line) as a tightly-braced expression.
fn single_line_string_doc(trivia: &Trivia, segments: &[StrSegment]) -> Doc {
    let mut out = String::from("\"");
    for segment in segments {
        match segment {
            StrSegment::Text(bytes) => {
                let text = std::str::from_utf8(bytes).expect("string text is UTF-8");
                out.push_str(&escape_single_line_text(text));
            }
            // A hole parses like a block body. Render its branches flat (single-line strings stay
            // on one line) and wrap them tightly in braces — `{name}`, not `{ name }`.
            StrSegment::Hole(block) => {
                let body = block
                    .branches
                    .iter()
                    .map(|branch| pretty::flatten(&branch_doc(trivia, branch, false)))
                    .collect::<Vec<_>>()
                    .join(" | ");
                out.push('{');
                out.push_str(&body);
                out.push('}');
            }
        }
    }
    out.push('"');
    pretty::text(out)
}

/// Escape literal text of a single-line string. As well as the basic escapes, `{` is escaped (a bare
/// `{` would open an interpolation hole); `}` needs no escape.
fn escape_single_line_text(text: &str) -> String {
    let mut out = String::new();
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '{' => out.push_str("\\{"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out
}

fn tuple_doc(trivia: &Trivia, tuple: &Tuple) -> Doc {
    // A sourced spread-update re-parses via its source head (`a[..., y]`, `$conn[..., y]`),
    // with the head's own spread elided back to `...`. The `~`-headed form remains for
    // flowing-value updates — and whenever a later bare `...` sits among the fields, which
    // only the `~` head leaves un-rewritten on re-parse.
    let mut elide_first_spread = false;
    let name = match &tuple.name {
        TupleName::Anonymous => String::new(),
        TupleName::Named(name) if tuple.fields.is_empty() => return pretty::text(name.clone()),
        TupleName::Named(name) => name.clone(),
        TupleName::Inherit => match tuple.fields.first() {
            Some(TupleField {
                value: FieldValue::Spread(Some(access)),
                ..
            }) if !tuple.fields[1..]
                .iter()
                .any(|f| matches!(f.value, FieldValue::Spread(None))) =>
            {
                elide_first_spread = true;
                render_access(access)
            }
            _ => "~".to_string(),
        },
    };
    if tuple.fields.is_empty() {
        return pretty::text(format!("{}[]", name));
    }
    // A punned tuple renders back to the spelling it was written as — `(a, p.x)` rather than the
    // `[a: a, x: p.x]` the parser desugared it to. Entries carry their own trivia, so comments
    // and blank lines inside the parens survive as they do in any field list.
    let sticky = tuple
        .fields
        .last()
        .is_some_and(|field| trivia.trailing_comma(field.span));
    if tuple.punned {
        return bracketed(
            format!("{}(", name),
            ")",
            tuple
                .fields
                .iter()
                .map(|field| pun_doc(trivia, field))
                .collect(),
            true,
            sticky,
        );
    }
    // Tuple field lists accept a trailing comma, so add one when broken.
    bracketed(
        format!("{}[", name),
        "]",
        tuple
            .fields
            .iter()
            .enumerate()
            .map(|(index, field)| {
                if index == 0 && elide_first_spread {
                    pretty::text("...")
                } else {
                    field_doc(trivia, field)
                }
            })
            .collect(),
        true,
        sticky,
    )
}

/// Render a multi-line string as a triple-quoted block. Each line is emitted as a `hardline` so the
/// renderer indents it to the ambient column — that indentation becomes the closing delimiter's
/// *margin*, which the parser strips back off, so the value round-trips at any nesting depth.
/// Interpolation holes sit inline on their line as `{…}`; a newline inside a text segment starts a
/// new line.
fn multiline_string_doc(trivia: &Trivia, segments: &[StrSegment]) -> Doc {
    // Build the content line by line, escaping text and rendering holes inline.
    let mut lines = vec![String::new()];
    for segment in segments {
        match segment {
            StrSegment::Text(bytes) => {
                let text = std::str::from_utf8(bytes).expect("string text is UTF-8");
                let mut parts = text.split('\n');
                if let Some(first) = parts.next() {
                    lines
                        .last_mut()
                        .unwrap()
                        .push_str(&escape_multiline_text(first));
                }
                for part in parts {
                    lines.push(escape_multiline_text(part));
                }
            }
            StrSegment::Hole(block) => {
                let body = block
                    .branches
                    .iter()
                    .map(|branch| pretty::flatten(&branch_doc(trivia, branch, false)))
                    .collect::<Vec<_>>()
                    .join(" | ");
                let line = lines.last_mut().unwrap();
                line.push('{');
                line.push_str(&body);
                line.push('}');
            }
        }
    }

    let mut docs = vec![pretty::text("\"\"\"")];
    for line in lines {
        docs.push(pretty::hardline());
        docs.push(pretty::text(protect_trailing_spaces(line)));
    }
    docs.push(pretty::hardline());
    docs.push(pretty::text("\"\"\""));
    pretty::concat(docs)
}

/// Escape one text fragment of a multi-line string (no trailing-space handling — that is applied per
/// line). Tabs and carriage returns are escaped (a literal `\r` would be normalised to `\n`, a
/// trailing tab would be stripped), every `"` is escaped so a run can't form a closing `"""`, and a
/// literal `{` becomes `\{` (a bare `{` opens an interpolation hole).
fn escape_multiline_text(text: &str) -> String {
    let mut out = String::new();
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '{' => out.push_str("\\{"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out
}

/// Convert a rendered line's trailing spaces to `\s` so they survive the trailing-whitespace
/// stripping done by both the renderer and the parser.
fn protect_trailing_spaces(line: String) -> String {
    let trimmed_len = line.trim_end_matches(' ').len();
    let trailing = line.len() - trimmed_len;
    if trailing == 0 {
        return line;
    }
    format!("{}{}", &line[..trimmed_len], "\\s".repeat(trailing))
}

/// Render a punned entry back to the bare access path it was written as — the inverse of the
/// parser's desugaring, whose shape (`name: path`) is what makes the match exhaustive.
fn pun_doc(trivia: &Trivia, field: &TupleField) -> Doc {
    let FieldValue::Chain(chain) = &field.value else {
        unreachable!("a punned entry is a chain")
    };
    let [Term::Access(path)] = chain.terms.as_slice() else {
        unreachable!("a punned entry is a lone reference")
    };
    pretty::concat(vec![
        trivia.leading_doc(field.span),
        pretty::text(render_access(path)),
        trivia.trailing_doc(field.span),
    ])
}

fn field_doc(trivia: &Trivia, field: &TupleField) -> Doc {
    let value = match &field.value {
        FieldValue::Spread(None) => pretty::text("..."),
        FieldValue::Spread(Some(access)) => pretty::text(format!("...{}", render_access(access))),
        FieldValue::Chain(chain) => match &field.name {
            Some(name) => pretty::concat(vec![
                pretty::text(format!("{}: ", name)),
                chain_doc(trivia, chain),
            ]),
            None => chain_doc(trivia, chain),
        },
    };
    pretty::concat(vec![
        trivia.leading_doc(field.span),
        value,
        trivia.trailing_doc(field.span),
    ])
}

/// A comma-separated, bracket-delimited list (a tuple literal or a select source list): inline when
/// it fits, otherwise one item per line. A trailing comma is added on break only when `trailing` is
/// set — tuple field lists allow one, but select source lists do not.
///
/// `sticky` holds the list one item per line whatever the width: set when the author wrote a
/// trailing comma ([`Trivia::trailing_comma`]). The broken layout prints one, so a list that once
/// breaks stays broken — the author removes the comma to let it join again.
fn bracketed(open: String, close: &str, items: Vec<Doc>, trailing: bool, sticky: bool) -> Doc {
    let separator = pretty::concat(vec![pretty::text(","), pretty::line()]);
    let trailing = if trailing {
        pretty::if_break(pretty::text(","), pretty::nil())
    } else {
        pretty::nil()
    };
    let sticky = if sticky {
        pretty::break_parent()
    } else {
        pretty::nil()
    };
    pretty::group(pretty::concat(vec![
        pretty::text(open),
        pretty::nest(
            2,
            pretty::concat(vec![
                pretty::softline(),
                pretty::join(separator, items),
                trailing,
            ]),
        ),
        pretty::softline(),
        pretty::text(close.to_string()),
        sticky,
    ]))
}

fn function_doc(trivia: &Trivia, function: &Function) -> Doc {
    let mut parts = vec![pretty::text(format!(
        "#{}",
        render_type_parameters(&function.type_parameters)
    ))];
    let bare = function.parameter_type.is_none() && function.return_type.is_none();
    if let Some(parameter_type) = &function.parameter_type {
        // The parameter type sits in a `function_input_type` position, which does not accept a bare
        // union/intersection/function — wrap those in parentheses.
        parts.push(type_atom_doc(trivia, parameter_type));
    }
    if let Some(return_type) = &function.return_type {
        parts.push(pretty::text(" -> "));
        parts.push(type_atom_doc(trivia, return_type));
    }
    let signature = pretty::concat(parts);
    match &function.body {
        None => signature,
        // A bare `#` (no signature — the inferred-parameter form) abuts its block as `#{ … }`;
        // a typed head takes a space.
        Some(body) if bare && function.type_parameters.is_empty() => {
            pretty::concat(vec![signature, block_doc(trivia, body)])
        }
        Some(body) => pretty::concat(vec![signature, pretty::text(" "), block_doc(trivia, body)]),
    }
}

/// Whether a tuple type renders in a form that can stand bare in a sugar head: its name and
/// brackets are its own delimiters (`Done`, `Reply['ref, 'bin]`, `['int, 'int]`). False for
/// partials (parens, handled per head) and for the alias-named spread form (`'v1[...]`, a
/// lowercase name), which has no bare rendering.
fn bare_tuple(tuple_type: &TupleType) -> bool {
    !tuple_type.is_partial
        && tuple_type
            .name
            .as_deref()
            .is_none_or(|name| name.starts_with(char::is_uppercase))
}

/// The self-delimiting form of a type in a sugar head position (a select receive or a spawn
/// parameter), or `None` when it has one only via the head's own bracketing: named and module
/// types are bare (`'int`, `'%proc.changed`), unions render carrying their own parentheses
/// (`('int | 'bin)`), and a named tuple type is its own delimiter (`Done`, `Reply['ref, 'bin]`).
fn sugar_type(type_def: &Type) -> Option<String> {
    match type_def {
        Type::Primitive(_) | Type::Identifier { .. } | Type::ModuleType { .. } | Type::Union(_) => {
            Some(render_type(type_def))
        }
        Type::Tuple(tuple_type) if tuple_type.name.is_some() && bare_tuple(tuple_type) => {
            Some(render_type(type_def))
        }
        _ => None,
    }
}

/// Render a spawn (`@f`, `@~`, `@[] { … }`, `@'int { … }`, `@<'t>'t { … }`). An inline
/// spawned function is normalised onto the `@`-sugar forms: the tight `@'type { body }` where
/// the type allows it, or `@(type) { body }` (the parenthesised arm accepts any type). The
/// sugar has no return type and requires a parameter type and a body, so a literal lacking
/// any of those keeps its `#`.
fn spawn_doc(trivia: &Trivia, func: &Term, argument: Option<&Term>) -> Doc {
    let head = match func {
        Term::Function(Function {
            type_parameters,
            parameter_type: Some(parameter_type),
            return_type: None,
            body: Some(body),
            ..
        }) => {
            // The spawn grammar also takes the unnamed tuple form (`@['int, 'int] { … }`
            // — there is no `@[sources]` to collide with). A partial has NO bare spawn
            // form: its parens read as the grouping arm, whose content must be a full
            // type, so it double-wraps (`@((x: 'int)) { … }`).
            let sugar = sugar_type(parameter_type)
                .or_else(|| match parameter_type {
                    Type::Tuple(tuple_type) if bare_tuple(tuple_type) => {
                        Some(render_type(parameter_type))
                    }
                    _ => None,
                })
                .unwrap_or_else(|| format!("({})", render_type(parameter_type)));
            pretty::concat(vec![
                pretty::text(format!(
                    "@{}{} ",
                    render_type_parameters(type_parameters),
                    sugar
                )),
                block_doc(trivia, body),
            ])
        }
        Term::Function(function) => {
            pretty::concat(vec![pretty::text("@"), function_doc(trivia, function)])
        }
        other => pretty::text(format!("@{}", render_term_atom(other))),
    };
    match argument {
        // A juxtaposed init argument (`@f x`).
        Some(argument) => pretty::concat(vec![head, pretty::text(" "), term_doc(trivia, argument)]),
        None => head,
    }
}

fn select_doc(trivia: &Trivia, sources: &Option<Vec<Chain>>) -> Doc {
    let Some(chains) = sources else {
        return pretty::text("!");
    };
    // A single source that has a tight shorthand keeps it (`!p`, `!#'int`, …); the general
    // `![…]` form is reserved for genuine multi-source selects.
    if let Some(shorthand) = select_shorthand(chains) {
        return pretty::text(shorthand);
    }
    // A single receive function *with* a body — a filter — keeps the tight shorthand too,
    // its body rendering as an ordinary block: `!'int { =42 => Ok }`.
    if let [chain] = chains.as_slice()
        && chain.binding.is_none()
        && let [Term::Function(function)] = chain.terms.as_slice()
        && function.type_parameters.is_empty()
        && function.return_type.is_none()
        && let Some(body) = &function.body
        && let Some(parameter_type) = &function.parameter_type
    {
        return pretty::concat(vec![
            pretty::text(format!("{} ", render_select_head(parameter_type))),
            block_doc(trivia, body),
        ]);
    }
    bracketed(
        "![".to_string(),
        "]",
        chains
            .iter()
            .map(|chain| chain_doc(trivia, chain))
            .collect(),
        false,
        false,
    )
}

/// The tightest select head for an identity receive's parameter type, mirroring what the select
/// grammar admits without the `#`: named, module, and named tuple types keep the bare sugar
/// (`!'int`, `!'%proc.changed`, `!Done`, `!Reply['ref, 'bin]`); unions and partial types the
/// parenthesised form (`!('int | 'bin)`, `!(x: 'int)` — their rendering carries its own
/// parentheses); anything else the `#` form (`!#['int, 'int]` — an unnamed tuple type has no
/// bare form, since `![…]` is the general source list).
fn render_select_head(parameter_type: &Type) -> String {
    let bare = sugar_type(parameter_type).or_else(|| match parameter_type {
        Type::Tuple(tuple_type) if tuple_type.is_partial => Some(render_type(parameter_type)),
        _ => None,
    });
    match bare {
        Some(sugar) => format!("!{}", sugar),
        None => format!(
            "!#{}",
            pretty::flatten(&type_atom_doc(&Trivia::default(), parameter_type))
        ),
    }
}

/// The tight single-source shorthand for a select, when the source list is a single source whose AST
/// has one (`!p`/`!f`, `!'int`, `!1000`). Returns `None` for the general form — several sources, or
/// a filter (a receive function *with* a body, which only the general `![…]` form can express).
fn select_shorthand(chains: &[Chain]) -> Option<String> {
    let [chain] = chains else { return None };
    if chain.binding.is_some() {
        return None;
    }
    let [term] = chain.terms.as_slice() else {
        return None;
    };
    match term {
        // `!f`, `!p`, `!$stream`, `!%mod.recv` — a named source.
        Term::Access(access)
            if matches!(
                access.source,
                Some(
                    AccessSource::Identifier(_)
                        | AccessSource::Parameter { .. }
                        | AccessSource::Import(_)
                )
            ) =>
        {
            Some(format!("!{}", render_access(access)))
        }
        // `!'int`, `!#Reply[...]` — a body-less identity receive.
        Term::Function(function)
            if function.type_parameters.is_empty()
                && function.return_type.is_none()
                && function.body.is_none() =>
        {
            function.parameter_type.as_ref().map(render_select_head)
        }
        // `!1000` — a timeout literal.
        Term::Literal(literal) => Some(format!("!{}", render_literal(literal))),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Trivia (comments and blank lines)
// ---------------------------------------------------------------------------

/// A comment or blank line — whitespace the parser discards but the formatter must preserve.
#[derive(Clone)]
enum TriviaItem {
    /// A line comment, stored verbatim from `//` to end of line.
    Comment(String),
    /// A blank (whitespace-only) line.
    Blank,
}

/// One piece of trivia found by [`scan_trivia`], with its source offset.
enum Scanned {
    Blank(usize),
    /// A line comment. `trailing` is set when code precedes it on the same line, in which case it
    /// belongs *after* that code rather than before the next node.
    Comment {
        offset: usize,
        text: String,
        trailing: bool,
    },
}

/// The start (`start`) and end (`end`) source offsets of a node trivia can attach to.
struct Anchor {
    start: usize,
    end: usize,
}

/// Comments and blank lines recovered from the source. `leading` and `trailing` are keyed by the
/// start offset of the AST node (type-alias step, chain, or tuple field) they attach to;
/// `dangling` holds anything after the last node.
///
/// `source` is kept for the layout decisions the author makes in the text itself: where steps
/// start a new line ([`Trivia::starts_line`]) and which bracketed lists end in a trailing comma
/// ([`Trivia::trailing_comma`]). An empty source (the `Default`) answers no to both, which is
/// what a flat rendering wants.
#[derive(Default)]
struct Trivia {
    leading: HashMap<usize, Vec<TriviaItem>>,
    trailing: HashMap<usize, Vec<String>>,
    dangling: Vec<TriviaItem>,
    source: String,
}

impl Trivia {
    /// Recover trivia from `source` and attach each item to an AST node: a leading comment/blank to
    /// the nearest following node, a trailing comment to the node whose text it follows.
    fn collect(program: &Sequence, source: &str) -> Trivia {
        let mut collected = Collected::default();
        collect_anchors(program, &mut collected);
        let Collected {
            mut anchors,
            mut dialect_content,
        } = collected;
        dialect_content.sort_unstable();
        // Index the anchors for O(log n) lookups per trivium: by start (to find the nearest node
        // *after* a leading comment) and by end (the nearest node *before* a trailing comment).
        anchors.sort_unstable_by_key(|anchor| anchor.start);
        let mut by_end: Vec<(usize, usize)> = anchors.iter().map(|a| (a.end, a.start)).collect();
        by_end.sort_unstable();

        let mut leading: HashMap<usize, Vec<TriviaItem>> = HashMap::new();
        let mut trailing: HashMap<usize, Vec<String>> = HashMap::new();
        let mut dangling = Vec::new();
        // The node a leading item precedes: the nearest anchor starting after it.
        let following = |offset: usize| {
            let index = anchors.partition_point(|anchor| anchor.start <= offset);
            anchors.get(index).map(|anchor| anchor.start)
        };
        // The node a trailing comment follows: the anchor ending nearest before it.
        let preceding = |offset: usize| {
            let index = by_end.partition_point(|&(end, _)| end <= offset);
            index.checked_sub(1).map(|i| by_end[i].1)
        };
        for item in scan_trivia(source, &dialect_content) {
            match item {
                Scanned::Blank(offset) => match following(offset) {
                    Some(anchor) => leading.entry(anchor).or_default().push(TriviaItem::Blank),
                    None => dangling.push(TriviaItem::Blank),
                },
                Scanned::Comment {
                    offset,
                    text,
                    trailing: true,
                } => match preceding(offset) {
                    Some(anchor) => trailing.entry(anchor).or_default().push(text),
                    None => dangling.push(TriviaItem::Comment(text)),
                },
                Scanned::Comment {
                    offset,
                    text,
                    trailing: false,
                } => match following(offset) {
                    Some(anchor) => leading
                        .entry(anchor)
                        .or_default()
                        .push(TriviaItem::Comment(text)),
                    None => dangling.push(TriviaItem::Comment(text)),
                },
            }
        }
        Trivia {
            leading,
            trailing,
            dangling,
            source: source.to_string(),
        }
    }

    /// Whether the step starting at `span` was written on a line of its own, rather than after a
    /// `;` on the line of the step before it. Reads back from the step's start over the separator
    /// run between the two — spaces and `;`s — to the newline that ends the previous line, or to
    /// the previous step's last character. (A comment in the run is always followed by the
    /// newline that ends it, which is met first.) A span without a source position — a step the
    /// parser or [`crate::simplify`] synthesised — reads as joined.
    fn starts_line(&self, span: Spanned) -> bool {
        let Some(before) = span.get().and_then(|span| self.source.get(..span.offset)) else {
            return false;
        };
        before
            .chars()
            .rev()
            .find(|c| !matches!(c, ' ' | '\t' | ';'))
            .is_some_and(|c| matches!(c, '\n' | '\r'))
    }

    /// Whether the bracketed list whose last entry is at `last` was written with a comma after
    /// that entry — the author's request to keep the list one entry per line. Skips whitespace
    /// and comments between the entry and the comma.
    fn trailing_comma(&self, last: Spanned) -> bool {
        let Some(mut rest) = last
            .get()
            .and_then(|span| self.source.get(span.offset + span.length..))
        else {
            return false;
        };
        loop {
            rest = rest.trim_start();
            if !rest.starts_with("//") {
                return rest.starts_with(',');
            }
            rest = rest.find('\n').map_or("", |index| &rest[index..]);
        }
    }

    /// The doc for trivia leading the node starting at `span`: each comment/blank on its own line,
    /// terminated by a hard line so the node starts fresh. `nil` when there is none.
    fn leading_doc(&self, span: Spanned) -> Doc {
        span.get()
            .and_then(|span| self.leading.get(&span.offset))
            .map_or_else(pretty::nil, |items| trivia_doc(items))
    }

    /// The doc for comments trailing the node starting at `span`: each is deferred to the end of the
    /// node's last line (a line suffix) and forces the surrounding construct to break so following
    /// code is not commented out. `nil` when there is none.
    fn trailing_doc(&self, span: Spanned) -> Doc {
        let Some(comments) = span.get().and_then(|span| self.trailing.get(&span.offset)) else {
            return pretty::nil();
        };
        let parts = comments
            .iter()
            .flat_map(|text| {
                [
                    pretty::line_suffix(pretty::text(format!(" {}", text))),
                    pretty::break_parent(),
                ]
            })
            .collect();
        pretty::concat(parts)
    }

    /// Whether the node starting at `span` carries any leading or trailing trivia.
    fn has_trivia(&self, span: Spanned) -> bool {
        span.get().is_some_and(|span| {
            self.leading.contains_key(&span.offset) || self.trailing.contains_key(&span.offset)
        })
    }
}

/// Render a run of leading trivia: each comment on its own line, each blank as an extra hard line.
/// Hard lines force the enclosing construct to break, so a commented node never stays inline.
fn trivia_doc(items: &[TriviaItem]) -> Doc {
    let mut parts = Vec::new();
    for item in items {
        match item {
            TriviaItem::Blank => parts.push(pretty::hardline()),
            TriviaItem::Comment(text) => {
                parts.push(pretty::text(text.clone()));
                parts.push(pretty::hardline());
            }
        }
    }
    pretty::concat(parts)
}

/// Scan `source` for line comments and blank lines in source order, marking a comment as `trailing`
/// when code precedes it on its line. String-aware so a `//` or blank line inside a `"…"` literal is
/// not mistaken for trivia; `skip` holds the (sorted, disjoint) byte ranges of dialect content,
/// which is raw text the scanner must likewise not read trivia out of.
/// The `//=` marker that opens an assertion.
const MARKER: &str = "//=";

/// How far a `//=` assertion's text runs, measured from just after the marker: to the `//` of a
/// prose note, or to the end of the line. String-aware, so a `//` inside a pattern's string
/// literal is not mistaken for the note's marker.
fn assertion_end(rest: &str) -> usize {
    let mut quoted = false;
    let mut chars = rest.char_indices();
    while let Some((index, c)) = chars.next() {
        match c {
            '\n' => return index,
            '\\' if quoted => {
                chars.next();
            }
            '"' => quoted = !quoted,
            '/' if !quoted && rest[index..].starts_with("//") => return index,
            _ => {}
        }
    }
    rest.len()
}

fn scan_trivia(source: &str, skip: &[(usize, usize)]) -> Vec<Scanned> {
    let mut out = Vec::new();
    let mut chars = source.char_indices().peekable();
    let mut in_string = false;
    let mut escaped = false;
    let mut line_start = 0usize;
    let mut line_blank = true;
    let mut skip = skip.iter().copied().peekable();
    while let Some((index, c)) = chars.next() {
        while skip.peek().is_some_and(|&(_, end)| end <= index) {
            skip.next();
        }
        if skip.peek().is_some_and(|&(start, _)| start <= index) {
            // Dialect content: nothing in it is trivia, and (as inside a string) its newlines
            // reset line tracking without producing `Blank` entries.
            if c == '\n' {
                line_start = index + 1;
                line_blank = true;
            } else {
                line_blank = false;
            }
            continue;
        }
        if in_string {
            match c {
                _ if escaped => escaped = false,
                '\\' => escaped = true,
                '"' => in_string = false,
                _ => {}
            }
            if c == '\n' {
                line_start = index + 1;
                line_blank = true;
            } else {
                line_blank = false;
            }
            continue;
        }
        match c {
            '\n' => {
                if line_blank {
                    out.push(Scanned::Blank(line_start));
                }
                line_start = index + 1;
                line_blank = true;
            }
            '/' if matches!(chars.peek(), Some((_, '/'))) => {
                // A `//=` assertion is AST, not trivia: consume it so its pattern text isn't
                // scanned, but record nothing — the formatter re-emits it from the
                // `Chain.assertions` node. It ends at the `//` of a prose note, which the loop
                // then reads as the ordinary trailing comment it is.
                let is_assertion = source[index..].starts_with("//=");
                let end = if is_assertion {
                    index + MARKER.len() + assertion_end(&source[index + MARKER.len()..])
                } else {
                    source[index..]
                        .find('\n')
                        .map_or(source.len(), |offset| index + offset)
                };
                while chars.peek().is_some_and(|&(offset, _)| offset < end) {
                    chars.next();
                }
                if !is_assertion {
                    out.push(Scanned::Comment {
                        offset: index,
                        text: source[index..end].trim_end().to_string(),
                        trailing: !line_blank,
                    });
                }
                line_blank = false;
            }
            '"' => {
                in_string = true;
                line_blank = false;
            }
            c if !c.is_whitespace() => line_blank = false,
            _ => {}
        }
    }
    out
}

/// Everything the trivia pass reads off the AST: the anchors trivia can attach to, and the byte
/// ranges of dialect content (raw text [`scan_trivia`] must not read trivia out of).
#[derive(Default)]
struct Collected {
    anchors: Vec<Anchor>,
    dialect_content: Vec<(usize, usize)>,
}

/// Collect every node trivia can attach to: the steps of a sequence (chains and type-alias
/// declarations alike) and tuple fields. Mirrors where [`sequence_doc`]/[`field_doc`] emit
/// trivia, so every attached item has exactly one emission site. Also records each dialect term's
/// content range along the way.
fn collect_anchors(program: &Sequence, out: &mut Collected) {
    visit_steps(&program.steps, out);
}

fn push_anchor(span: Spanned, out: &mut Collected) {
    if let Some(span) = span.get() {
        out.anchors.push(Anchor {
            start: span.offset,
            end: span.offset + span.length,
        });
    }
}

fn visit_sequence(sequence: &Sequence, out: &mut Collected) {
    visit_steps(&sequence.steps, out);
}

fn visit_steps(steps: &[Step], out: &mut Collected) {
    for step in steps {
        push_anchor(step.span(), out);
        match step {
            Step::Chain(chain) => visit_chain(chain, out),
            Step::TypeAlias {
                type_definition, ..
            } => visit_alias_type(type_definition, out),
        }
    }
}

/// Anchor the field entries of a type that [`type_alias_doc`] lays out as a document: the alias's
/// own tuple, or the members of a union it distributes over.
fn visit_alias_type(type_def: &Type, out: &mut Collected) {
    match type_def {
        Type::Union(union_type) => {
            for member in &union_type.types {
                visit_type_fields(member, out);
            }
        }
        other => visit_type_fields(other, out),
    }
}

/// Anchor a tuple type's field entries — only those of the outermost tuple type of a signature or
/// alias. A nested type may be reached where it is rendered flat (a spawn or select head), onto a
/// line that has no room for a comment, so its fields are deliberately left unanchored: an item
/// inside one keeps attaching outward, to the nearest node that *can* emit it.
fn visit_type_fields(type_def: &Type, out: &mut Collected) {
    if let Type::Tuple(tuple_type) = type_def {
        for field in &tuple_type.fields {
            push_anchor(field_type_span(field), out);
        }
    }
}

/// A function literal's own anchors. `signature` is set where [`function_doc`] renders the
/// parameter and return types as documents, and cleared where a caller renders them flat — a
/// spawn head, which spells its parameter type into its own `@…` text.
fn visit_function(function: &Function, signature: bool, out: &mut Collected) {
    if signature {
        for type_def in [&function.parameter_type, &function.return_type]
            .into_iter()
            .flatten()
        {
            visit_type_fields(type_def, out);
        }
    }
    if let Some(body) = &function.body {
        visit_block(body, out);
    }
}

/// Recurse into a chain's terms without making the chain itself an anchor (used for select sources
/// and tuple-field chains, which are not emitted by `sequence_doc`), anchoring what sits *between*
/// the terms: each `~>` separator, and each own-line assertion.
fn visit_chain(chain: &Chain, out: &mut Collected) {
    // A gap contributes two anchors because trivia attaches directionally: a comment ending the
    // previous term's line needs one that *ends* before it, and an own-line comment one that
    // *starts* after it. Mirrors the emission in `chain_terms_doc`.
    for gap in &chain.continuations {
        push_anchor(gap.end, out);
        push_anchor(gap.pipe, out);
    }
    // An own-line assertion is its own anchor, so comments written above it keep their place and
    // a prose note written after it trails it. A *trailing* assertion is deliberately not one: a
    // note after it attaches to the term or gap before it instead, and still renders in the right
    // place, since a trailing comment is a `line_suffix` and so is deferred past the assertion to
    // the end of the line either way. Anchoring it would capture comments written above it — a
    // dangling one inside a container the step ends with — that nothing then emits.
    // The first assertion of an assertion-only step shares the step's offset and is covered by
    // the step's anchor.
    for (index, assertion) in chain.assertions.iter().enumerate() {
        if assertion.own_line && !(index == 0 && chain.terms.is_empty()) {
            push_anchor(assertion.span, out);
        }
    }
    for term in &chain.terms {
        visit_term(term, out);
    }
}

fn visit_term(term: &Term, out: &mut Collected) {
    match term {
        Term::Tuple(tuple) => {
            for field in &tuple.fields {
                push_anchor(field.span, out);
                if let FieldValue::Chain(chain) = &field.value {
                    visit_chain(chain, out);
                }
            }
        }
        Term::Block(block) => visit_block(block, out),
        Term::Function(function) => visit_function(function, true, out),
        Term::Spawn(inner, argument, _) => {
            match inner.as_ref() {
                Term::Function(function) => visit_function(function, false, out),
                other => visit_term(other, out),
            }
            if let Some(argument) = argument {
                visit_term(argument, out);
            }
        }
        Term::Apply(_, argument) => visit_term(argument, out),
        Term::Select(Some(chains), _) => {
            for chain in chains {
                visit_chain(chain, out);
            }
        }
        Term::Dialect(dialect) => {
            if let Some(span) = dialect.content_span.get() {
                out.dialect_content
                    .push((span.offset, span.offset + span.length));
            }
        }
        _ => {}
    }
}

fn visit_block(block: &Block, out: &mut Collected) {
    for annotation in &block.annotations {
        push_anchor(annotation.span, out);
        visit_chain(&annotation.value, out);
    }
    for branch in &block.branches {
        visit_sequence(&branch.condition, out);
        if let Some(consequence) = &branch.consequence {
            visit_sequence(consequence, out);
        }
    }
}

/// Collapse the laid-out text so at most one blank line separates anything, with no leading or
/// trailing blank lines, and exactly one final newline. (Blank-line trivia and hard-line joins can
/// otherwise stack up.)
fn collapse_blanks(text: &str) -> String {
    let mut lines: Vec<&str> = Vec::new();
    let mut prev_blank = true; // seeded true so leading blank lines are dropped
    for line in text.lines() {
        let blank = line.trim().is_empty();
        if blank && prev_blank {
            continue;
        }
        lines.push(if blank { "" } else { line });
        prev_blank = blank;
    }
    while lines.last().is_some_and(|line| line.is_empty()) {
        lines.pop();
    }
    let mut out = lines.join("\n");
    out.push('\n');
    out
}

/// Render a literal to one line. A binary written across rows collapses onto that line, so
/// this is only the whole story where a term can't break — patterns and select sources.
/// [`binary_doc`] renders the rows where they can be kept.
fn render_literal(literal: &Literal) -> String {
    match literal {
        Literal::Integer(value) => value.to_string(),
        // The author's grouping is presentation the formatter keeps, like a string's
        // delimiter style: separator runs and whitespace against a bracket normalise to one
        // space, but where the groups divide is left alone (`<550e8400 e29b 41d4>` means
        // something the formatter has no business regrouping).
        Literal::Binary(binary) => {
            let groups: Vec<String> = binary.groups().map(hex::encode).collect();
            format!("<{}>", groups.join(" "))
        }
    }
}

/// Render a binary literal written across lines: one row per line, indented inside the
/// brackets, with the closing `>` back at the enclosing indentation — the same shape as a
/// `"""` string, and, as there, the line breaks are the author's rather than the width
/// engine's. A table of round constants laid out to match its spec stays laid out that way.
fn binary_doc(binary: &BinaryLiteral) -> Doc {
    let mut parts = vec![pretty::text("<")];
    for row in binary.rows() {
        parts.push(pretty::hardline());
        parts.push(pretty::text(
            row.iter().map(hex::encode).collect::<Vec<_>>().join(" "),
        ));
    }
    pretty::concat(vec![
        pretty::nest(2, pretty::concat(parts)),
        pretty::hardline(),
        pretty::text(">"),
    ])
}

fn render_access(access: &Access) -> String {
    let mut out = String::new();
    match &access.source {
        None => {}
        Some(AccessSource::Identifier(name)) => out.push_str(name),
        Some(AccessSource::Parameter { depth }) => out.push_str(&parameter_sigils(*depth)),
        Some(AccessSource::Ripple) => out.push('~'),
        Some(AccessSource::Import(parts)) => {
            out.push('%');
            out.push_str(&parts.join("/"));
        }
        Some(AccessSource::Builtin(name)) => {
            out.push_str("__");
            out.push_str(name);
            out.push_str("__");
        }
        Some(AccessSource::TailCall(None)) => out.push('^'),
        Some(AccessSource::TailCall(Some(name))) => {
            out.push('^');
            out.push_str(name);
        }
        Some(AccessSource::TailCallRipple) => out.push_str("^~"),
        Some(AccessSource::Self_) => out.push('@'),
    }
    // A single field/index directly after `$` is sugar for `$.x`/`$.0`, so the first accessor on a
    // parameter is written without its dot (`$0`, `$foo`); the rest keep their dots.
    let dotless_first = matches!(access.source, Some(AccessSource::Parameter { .. }));
    for (index, accessor) in access.accessors.iter().enumerate() {
        // An annotation accessor carries its own `:` sigil; fields/indexes get their `.`
        // (except a first accessor on `$`, which is written dotless: `$0`, `$foo`).
        match accessor {
            AccessPath::Field(field) => {
                if !(index == 0 && dotless_first) {
                    out.push('.');
                }
                out.push_str(field);
            }
            AccessPath::Index(value) => {
                if !(index == 0 && dotless_first) {
                    out.push('.');
                }
                out.push_str(&value.to_string());
            }
            AccessPath::Annotation(name, expected) => {
                out.push(':');
                out.push_str(name);
                // The checked form's shape is a glued type argument (`:key<'t>`).
                if let Some(ast_type) = expected {
                    out.push_str(&render_type_arguments(std::slice::from_ref(ast_type)));
                }
            }
        }
    }
    // Explicit type arguments (`f<'int>`) — always last, glued like every access suffix.
    out.push_str(&render_type_arguments(&access.type_arguments));
    out
}

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

/// The flat, single-line rendering of a pattern, for a position with no vertical form — an
/// assertion's comment text, a nested pattern inside one. Patterns carry no trivia, so they never
/// force a break and this is always legal.
pub(crate) fn render_match(pattern: &Match) -> String {
    pretty::flatten(&match_doc(&Trivia::default(), pattern))
}

/// A pattern as a document. Only a tuple's field list breaks — a destructuring wide enough to
/// matter is a wide field list — along with any type a pattern tests, and everything else is text.
fn match_doc(trivia: &Trivia, pattern: &Match) -> Doc {
    match pattern {
        Match::Tuple(tuple) => match_tuple_doc(trivia, tuple),
        Match::Partial(partial) => partial_pattern_doc(trivia, partial),
        Match::Not(inner) => pretty::concat(vec![pretty::text("\\"), match_doc(trivia, inner)]),
        Match::Type(type_def) => match_type_doc(trivia, type_def),
        other => pretty::text(render_match_flat(other)),
    }
}

fn render_match_flat(pattern: &Match) -> String {
    match pattern {
        Match::Identifier(name, _) => name.clone(),
        Match::Literal(literal) => render_literal(literal),
        // A string pattern renders as a single-line `"…"` regardless of how it was written: the
        // match formatter is string-based and can't lay out a multi-line block, so a `"""…"""`
        // pattern (rare) collapses to single-line, with newlines escaped.
        Match::String(_, bytes) => {
            let text = std::str::from_utf8(bytes).expect("string pattern text is UTF-8");
            format!("\"{}\"", escape_single_line_text(text))
        }
        Match::Tuple(_) | Match::Partial(_) | Match::Type(_) | Match::Not(_) => {
            unreachable!("rendered as a document by match_doc")
        }
        Match::Star(None) => "*".to_string(),
        Match::Star(Some(name)) => format!("{}*", name),
        Match::Placeholder => "_".to_string(),
        Match::Ripple => "~".to_string(),
        Match::Pin(target) => {
            let mut out = String::from("^");
            // As in `render_access`, a first accessor on `$` is written dotless (`^$x`, `^$0`).
            let dotless_first = matches!(target.root, PinRoot::Parameter { .. });
            match &target.root {
                PinRoot::Variable(name) => out.push_str(name),
                PinRoot::Parameter { depth } => out.push_str(&parameter_sigils(*depth)),
            }
            for (index, accessor) in target.accessors.iter().enumerate() {
                if !(index == 0 && dotless_first) {
                    out.push('.');
                }
                match accessor {
                    AccessPath::Field(field) => out.push_str(field),
                    AccessPath::Index(value) => out.push_str(&value.to_string()),
                    AccessPath::Annotation(..) => {
                        unreachable!("pin targets have no annotation steps")
                    }
                }
            }
            out
        }
        Match::Or(alternatives) => format!(
            "({})",
            alternatives
                .iter()
                .map(render_match)
                .collect::<Vec<_>>()
                .join(" | ")
        ),
        // Conjuncts render as themselves: `&` binds tighter than `|`, and an alternation or union
        // conjunct renders its own parens (`((0 | 1) & v)`). A conjunct may also be a type the
        // intersection grammar admits bare where a lone pattern could not (`(+File & fd)`).
        Match::And(conjuncts) => format!(
            "({})",
            conjuncts
                .iter()
                .map(|conjunct| match conjunct {
                    Match::Type(type_def) if is_bare_conjunct_type(type_def) => {
                        render_type(type_def)
                    }
                    other => render_match(other),
                })
                .collect::<Vec<_>>()
                .join(" & ")
        ),
    }
}

/// A tuple pattern, breaking one field per line when the list does not fit, or when the author
/// ended it with a comma. Pattern field lists take a trailing comma, as tuple values and tuple
/// types do.
fn match_tuple_doc(trivia: &Trivia, tuple: &MatchTuple) -> Doc {
    let name = tuple.name.clone().unwrap_or_default();
    if tuple.fields.is_empty() {
        return pretty::text(if tuple.name.is_some() {
            name
        } else {
            "[]".to_string()
        });
    }
    let sticky = tuple
        .fields
        .last()
        .is_some_and(|field| trivia.trailing_comma(field.span));
    let fields = tuple
        .fields
        .iter()
        .map(|field| match &field.name {
            Some(label) => pretty::concat(vec![
                pretty::text(format!("{}: ", label)),
                match_doc(trivia, &field.pattern),
            ]),
            None => match_doc(trivia, &field.pattern),
        })
        .collect();
    bracketed(format!("{}[", name), "]", fields, true, sticky)
}

fn partial_pattern_doc(trivia: &Trivia, partial: &PartialPattern) -> Doc {
    let name = partial.name.clone().unwrap_or_default();
    if partial.fields.is_empty() {
        return pretty::text(format!("{}()", name));
    }
    let sticky = partial
        .fields
        .last()
        .is_some_and(|field| trivia.trailing_comma(field.span));
    let fields = partial
        .fields
        .iter()
        .map(|field| match &field.pattern {
            None => pretty::text(field.name.clone()),
            Some(pattern) => pretty::concat(vec![
                pretty::text(format!("{}: ", field.name)),
                match_doc(trivia, pattern),
            ]),
        })
        .collect();
    bracketed(format!("{}(", name), ")", fields, true, sticky)
}

/// Whether a type conjunct of a conjunction pattern renders without parentheses: a resource, or a
/// process type without clauses. A clause would take the conjunction's `&` as part of its type.
fn is_bare_conjunct_type(type_def: &Type) -> bool {
    match type_def {
        Type::Resource(_) => true,
        Type::Process(process) => process.return_type.is_none() && process.state_type.is_none(),
        _ => false,
    }
}

/// A type used as a pattern. A pattern's type position only accepts the bare forms recognised by
/// `inline_type_expression` (a type name/module/self-default reference or a partial type);
/// anything else (unions, intersections, functions, non-partial tuples, cycles, …) must be wrapped
/// in parentheses so it re-parses as a `Match::Type` rather than, say, a structural tuple pattern.
fn match_type_doc(trivia: &Trivia, type_def: &Type) -> Doc {
    let bare = match type_def {
        Type::Primitive(_)
        | Type::Identifier { .. }
        | Type::ModuleType { .. }
        | Type::SelfDefault { .. } => true,
        Type::Tuple(tuple_type) => tuple_type.is_partial,
        // A union renders its own parentheses, so it needs no extra wrapping here.
        Type::Union(_) => true,
        _ => false,
    };
    if bare {
        type_doc(trivia, type_def)
    } else {
        parenthesised(type_doc(trivia, type_def))
    }
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// The flat, single-line rendering of a type, for a position with no vertical form to fall back on
/// — a select or spawn sugar head, an access's type arguments, a pattern inside an assertion.
/// Types carry no trivia there, so they never force a break and this is always legal.
fn render_type(type_def: &Type) -> String {
    pretty::flatten(&type_doc(&Trivia::default(), type_def))
}

/// `(inner)`, for a type in a position that needs it delimited.
fn parenthesised(inner: Doc) -> Doc {
    pretty::concat(vec![pretty::text("("), inner, pretty::text(")")])
}

/// A type as a document. Unions and tuple field lists break where they would overflow — a union
/// one member per line behind a leading `|`, a tuple one field per line with a trailing comma —
/// and every other type is laid out around the types inside it, so a function type or a type
/// application wraps by way of its parts.
fn type_doc(trivia: &Trivia, type_def: &Type) -> Doc {
    match type_def {
        Type::Primitive(PrimitiveType::Int) => pretty::text("'int"),
        Type::Primitive(PrimitiveType::Bin) => pretty::text("'bin"),
        Type::Primitive(PrimitiveType::Ref) => pretty::text("'ref"),
        Type::Identifier { name, arguments } => pretty::concat(vec![
            pretty::text(format!("'{}", name)),
            type_arguments_doc(trivia, arguments),
        ]),
        Type::Tuple(tuple_type) => tuple_type_doc(trivia, tuple_type),
        Type::Function(function_type) => {
            let mut parts = vec![
                pretty::text("#"),
                type_atom_doc(trivia, &function_type.input),
                pretty::text(" -> "),
                type_atom_doc(trivia, &function_type.output),
            ];
            if let Some(receive) = &function_type.receive {
                parts.push(pretty::text(" !"));
                parts.push(type_atom_doc(trivia, receive));
            }
            if let Some(states) = &function_type.states {
                parts.push(pretty::text(" ?"));
                parts.push(type_atom_doc(trivia, states));
            }
            pretty::concat(parts)
        }
        // A union is parenthesised everywhere it is rendered inline; only a top-level type-alias
        // right-hand side (handled by `union_alias_doc`) is left bare.
        Type::Union(union_type) => union_doc(trivia, union_type),
        Type::Intersection(types) => pretty::join(
            pretty::text(" & "),
            types
                .iter()
                .map(|member| type_atom_doc(trivia, member))
                .collect(),
        ),
        Type::Cycle(None) => pretty::text("^"),
        Type::Cycle(Some(level)) => pretty::text(format!("^{}", level)),
        Type::Process(process_type) => process_type_doc(trivia, process_type),
        Type::Resource(name) => pretty::text(format!("+{}", name)),
        Type::Top => pretty::text("_"),
        Type::ModuleType {
            module,
            member,
            arguments,
        } => {
            let mut head = format!("'%{}", module.join("/"));
            if let Some(member) = member {
                head.push('.');
                head.push_str(member);
            }
            pretty::concat(vec![
                pretty::text(head),
                type_arguments_doc(trivia, arguments),
            ])
        }
        Type::SelfDefault { arguments } => pretty::concat(vec![
            pretty::text("'"),
            type_arguments_doc(trivia, arguments),
        ]),
    }
}

/// An inline union, `(A | B)` flat, or broken one member per line, each led by `|` and indented
/// inside the parentheses — the shape an alias's union takes, with the parentheses as its frame.
fn union_doc(trivia: &Trivia, union_type: &UnionType) -> Doc {
    let mut parts = vec![pretty::softline()];
    for (index, member) in union_type.types.iter().enumerate() {
        if index > 0 {
            parts.push(pretty::line());
        }
        parts.push(leading_bar(index == 0));
        // As in `union_alias_doc`, a member that breaks lays its fields out under its own name.
        parts.push(pretty::nest(2, union_member_doc(trivia, member)));
    }
    pretty::group(pretty::concat(vec![
        pretty::text("("),
        pretty::nest(2, pretty::concat(parts)),
        pretty::softline(),
        pretty::text(")"),
    ]))
}

/// A type where the grammar expects a `base_type`/atom (intersection members, process heads and
/// clauses, function input/output): wrap an intersection or function in parentheses, and a
/// clause-bearing process type — nested, its clauses would bind to the wrong head (and a function
/// output's trailing clauses are the function's). A union already carries its parentheses.
fn type_atom_doc(trivia: &Trivia, type_def: &Type) -> Doc {
    match type_def {
        Type::Intersection(_) | Type::Function(_) => parenthesised(type_doc(trivia, type_def)),
        Type::Process(process_type)
            if process_type.return_type.is_some() || process_type.state_type.is_some() =>
        {
            parenthesised(type_doc(trivia, type_def))
        }
        _ => type_doc(trivia, type_def),
    }
}

/// A union member is an intersection-level type, so it never needs wrapping except for a function
/// type (which only appears as a member when originally parenthesised).
fn union_member_doc(trivia: &Trivia, type_def: &Type) -> Doc {
    match type_def {
        Type::Function(_) => parenthesised(type_doc(trivia, type_def)),
        _ => type_doc(trivia, type_def),
    }
}

/// `<'a, 'b>` for a non-empty argument list, otherwise nothing. The grammar allows no whitespace
/// against the angle brackets, so the list itself never breaks; an argument may, inside itself.
fn type_arguments_doc(trivia: &Trivia, arguments: &[Type]) -> Doc {
    if arguments.is_empty() {
        return pretty::nil();
    }
    pretty::concat(vec![
        pretty::text("<"),
        pretty::join(
            pretty::text(", "),
            arguments
                .iter()
                .map(|argument| type_doc(trivia, argument))
                .collect(),
        ),
        pretty::text(">"),
    ])
}

fn render_type_arguments(arguments: &[Type]) -> String {
    pretty::flatten(&type_arguments_doc(&Trivia::default(), arguments))
}

/// A tuple type, breaking one field per line when the list does not fit, or when the author ended
/// it with a comma. Field lists take a trailing comma, so the broken form gets one, exactly as a
/// value tuple's does.
fn tuple_type_doc(trivia: &Trivia, tuple_type: &TupleType) -> Doc {
    // A lowercase name is the alias-applied spread form (`'v1[..., id: 'bin]`): the parser
    // stores the alias as the name and rewrites its bare spreads to `...v1`. Render it back
    // in source form — the name takes its `'` prefix, and a spread of the alias itself
    // (without type arguments) collapses to a bare `...`.
    let alias = tuple_type
        .name
        .as_deref()
        .filter(|name| name.starts_with(char::is_lowercase));
    let name = match alias {
        Some(alias) => format!("'{}", alias),
        None => tuple_type.name.clone().unwrap_or_default(),
    };
    if tuple_type.fields.is_empty() && tuple_type.rest.is_none() {
        return pretty::text(if tuple_type.is_partial {
            format!("{}()", name)
        } else if tuple_type.name.is_some() {
            name
        } else {
            "[]".to_string()
        });
    }
    // A rest entry carries no span of its own, so only a field's trailing comma is kept.
    let sticky = tuple_type.rest.is_none()
        && tuple_type
            .fields
            .last()
            .is_some_and(|field| trivia.trailing_comma(field_type_span(field)));
    let mut fields: Vec<Doc> = tuple_type
        .fields
        .iter()
        .map(|field| {
            let rendered = match (alias, field) {
                (
                    Some(alias),
                    FieldType::Spread {
                        identifier: Some(identifier),
                        type_arguments,
                        ..
                    },
                ) if identifier == alias && type_arguments.is_empty() => pretty::text("..."),
                _ => field_type_doc(trivia, field),
            };
            pretty::concat(vec![
                trivia.leading_doc(field_type_span(field)),
                rendered,
                trivia.trailing_doc(field_type_span(field)),
            ])
        })
        .collect();
    if let Some(rest) = &tuple_type.rest {
        fields.push(pretty::concat(vec![
            pretty::text("*"),
            type_doc(trivia, rest),
        ]));
    }
    let (open, close) = if tuple_type.is_partial {
        ("(", ")")
    } else {
        ("[", "]")
    };
    bracketed(format!("{}{}", name, open), close, fields, true, sticky)
}

/// Where a type's field entry starts, for trivia attachment.
fn field_type_span(field: &FieldType) -> Spanned {
    match field {
        FieldType::Field { span, .. } | FieldType::Spread { span, .. } => *span,
    }
}

/// A field's ` = <value>` default. A default is a short value chain, so it renders flat.
fn render_field_default(default: &Option<Box<Chain>>) -> String {
    match default {
        Some(chain) => format!(
            " = {}",
            crate::pretty::flatten(&chain_doc(&Trivia::default(), chain))
        ),
        None => String::new(),
    }
}

fn field_type_doc(trivia: &Trivia, field_type: &FieldType) -> Doc {
    match field_type {
        FieldType::Field {
            name,
            omittable,
            type_def,
            default,
            ..
        } => {
            let label = match name {
                Some(name) if *omittable => format!("({})", name),
                Some(name) => name.clone(),
                None => String::new(),
            };
            // A decorator states no type — it names a field the spread already brought in
            // and adjusts only its label and default — so it renders as the label alone.
            let body = match (type_def, name) {
                (Some(type_def), Some(_)) => {
                    pretty::concat(vec![pretty::text(": "), type_doc(trivia, type_def)])
                }
                (Some(type_def), None) => type_doc(trivia, type_def),
                (None, _) => pretty::nil(),
            };
            pretty::concat(vec![
                pretty::text(label),
                body,
                pretty::text(render_field_default(default)),
            ])
        }
        FieldType::Spread {
            identifier: None, ..
        } => pretty::text("..."),
        FieldType::Spread {
            identifier: Some(identifier),
            type_arguments,
            ..
        } => pretty::concat(vec![
            pretty::text(format!("...'{}", identifier)),
            type_arguments_doc(trivia, type_arguments),
        ]),
    }
}

fn process_type_doc(trivia: &Trivia, process_type: &ProcessType) -> Doc {
    let mut parts = vec![pretty::text("@")];
    if let Some(receive) = &process_type.receive_type {
        parts.push(type_atom_doc(trivia, receive));
    }
    // Clause sigils glue to a bare `@` and take a space after a head or earlier clause.
    if let Some(ret) = &process_type.return_type {
        parts.push(pretty::text(if process_type.receive_type.is_some() {
            " !"
        } else {
            "!"
        }));
        parts.push(type_atom_doc(trivia, ret));
    }
    if let Some(state) = &process_type.state_type {
        parts.push(pretty::text(
            if process_type.receive_type.is_some() || process_type.return_type.is_some() {
                " ?"
            } else {
                "?"
            },
        ));
        parts.push(type_atom_doc(trivia, state));
    }
    pretty::concat(parts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use std::path::{Path, PathBuf};

    /// Reduce a program to the compiler's canonical, block-free form: every no-op block stripped or
    /// lifted (`Compiler::compile` does the same before codegen), and every call in its preferred
    /// spelling (each spelling compiles alike). Two programs equal here compile identically.
    fn canonical(program: Sequence) -> Sequence {
        crate::simplify::normalize_blocks(
            program,
            &crate::simplify::Options {
                keep: &|_| false,
                lift: true,
                group_consequences: false,
                restyle_calls: Some(&|_| true),
            },
        )
    }

    /// Assert that `source` parses, formatting is an idempotent fixpoint (the second print equals the
    /// first and re-parses cleanly), and formatting preserves the *compiled* program — it only adds
    /// or removes blocks the compiler treats as no-ops (plus whitespace), so both sides reduce to the
    /// same canonical form.
    fn assert_idempotent(source: &str, label: &str) {
        let ast = parse(source).unwrap_or_else(|e| panic!("{label}: source must parse: {e:?}"));
        let printed = format_program(&ast, source);
        let reparsed = parse(&printed).unwrap_or_else(|e| {
            panic!("{label}: formatted output must parse: {e:?}\n--- output ---\n{printed}")
        });
        let printed2 = format_program(&reparsed, &printed);
        assert_eq!(printed, printed2, "{label}: format must be idempotent");
        assert_eq!(
            canonical(ast),
            canonical(reparsed),
            "{label}: format must preserve the compiled program"
        );
    }

    /// Assert that formatting `source` produces exactly `expected`.
    fn assert_formats(source: &str, expected: &str) {
        let ast = parse(source).expect("source must parse");
        let printed = format_program(&ast, source);
        assert_eq!(printed, expected, "\n--- got ---\n{printed}");
    }

    #[test]
    fn spawned_function_literal_takes_the_shorthand_only_when_it_can() {
        // Type parameters move onto the `@`; a return type or a missing body has no
        // shorthand, so the `#` stays.
        assert_formats("@#'int { $ } 1\n", "@'int { $ } 1\n");
        assert_formats("@#<'t>'t { $ } 1\n", "@<'t>'t { $ } 1\n");
        assert_formats("@#<'t: 'int>'t { $ } 1\n", "@<'t: 'int>'t { $ } 1\n");
        assert_formats("@#<'t>('t | []) { $ } 1\n", "@<'t>('t | []) { $ } 1\n");
        assert_formats(
            "@#'int -> ('int | 'bin) { $ } 1\n",
            "@#'int -> ('int | 'bin) { $ } 1\n",
        );
        assert_formats("@#'int 1\n", "@#'int 1\n");
    }

    #[test]
    fn conjunction_type_conjuncts_render_bare() {
        // The intersection grammar admits a resource or clause-free process type bare as a
        // conjunct; a clause would read the conjunction's `&` into its type, so it keeps parens.
        assert_formats("x ~> =(+File & fd)\n", "x ~> =(+File & fd)\n");
        assert_formats("x ~> =(@'int & p)\n", "x ~> =(@'int & p)\n");
        assert_formats("x ~> =((@'int !'bin) & p)\n", "x ~> =((@'int !'bin) & p)\n");
    }

    #[test]
    fn single_named_select_sources_keep_the_shorthand() {
        assert_formats("![p]\n", "!p\n");
        assert_formats("![$stream]\n", "!$stream\n");
        assert_formats("![%proc.changed]\n", "!%proc.changed\n");
        assert_formats("![p, 1000]\n", "![p, 1000]\n");
        assert_formats("x ~> ![~]\n", "x ~> ![~]\n");
    }

    #[test]
    fn ripple_patterns_render_as_written() {
        assert_formats("x ~> =[~, [~, _]]\n", "x ~> =[~, [~, _]]\n");
        assert_formats("x ~> =(Ok[~] | ~)\n", "x ~> =(Ok[~] | ~)\n");
        assert_formats("x ~> =(y: ~) ~> f\n", "x ~> =(y: ~) ~> f\n");
    }

    #[test]
    fn assertion_round_trips_and_canonicalizes() {
        // Marker spacing and the pattern normalize; the value side formats as usual.
        assert_formats("5  //=  Ok\n", "5 //= Ok\n");
        assert_formats("x = 5 //= 5", "x = 5 //= 5\n");
        assert_formats(
            "Point[1,2] //= Point[1, 2]",
            "Point[1, 2] //= Point[1, 2]\n",
        );
    }

    #[test]
    fn own_line_assertions_keep_their_lines() {
        // A leading `//=` continues the step above; the formatter keeps it on its own line,
        // at the step's indent, with any comments and blanks between held in place.
        assert_idempotent("1 ~> f\n//= 2\n", "own-line assertion");
        assert_idempotent("5 //= 'int\n//= 5 // note\n", "trailing then own-line");
        assert_idempotent("5 ~> f\n\n// why\n//= 10\n", "trivia above an assertion");
        assert_idempotent(
            "x = {\n  5 ~> f\n  //= 10\n}\nx\n",
            "own-line assertion in a block",
        );
        // A `;`-separated trailing assertion is the trailing form: the separator drops.
        assert_formats("5; //= 5\n", "5 //= 5\n");
    }

    #[test]
    fn assertion_only_steps_render_bare() {
        // An opening assertion is a step with no chain: nothing precedes the marker. The
        // assertion terminates its line, so the enclosing block never flattens around it.
        assert_idempotent(
            "f = #'int {\n  //= 5\n  $ ~> g\n}\nf 5\n",
            "opening assertion",
        );
        assert_formats(
            "f = #'int {\n  //= 5 // the note\n  $\n}\n",
            "f = #'int {\n  //= 5 // the note\n  $\n}\n",
        );
    }

    #[test]
    fn assertion_note_and_trailing_comment_survive() {
        // A note after an assertion is an ordinary trailing comment, and rides after it.
        assert_idempotent("x = 5 //= 5\nx //= 5 // the note\n", "assertion note");
        assert_idempotent(
            "f = #'int {\n  $ //= 'int\n}\nf 3 //= 3\n",
            "assertion in body",
        );
        // A redundant block whose body asserts is kept, not spliced.
        assert_idempotent("{\n  5 //= 6\n}\n", "assertion keeps its block");
    }

    #[test]
    fn assertion_notes_attach_to_their_assertion() {
        // A note follows the assertion it was written after, at every emission site: a
        // step-final assertion, an own-line one, and one in a `~>` continuation gap.
        assert_idempotent("5 //= 5 // trailing\n", "note on a step-final assertion");
        assert_idempotent(
            "1 ~> f\n//= 2 // own-line\n",
            "note on an own-line assertion",
        );
        assert_idempotent(
            "1 //= 1 // the head\n~> f //= 2 // the result\n",
            "notes on chain-line assertions",
        );
        // A comment trailing the term and a note on the assertion above it are distinct
        // attachments, and keep their order.
        assert_idempotent(
            "1 // the term\n//= 1 // the assertion\n~> f\n",
            "term comment then assertion note",
        );
        // A note on a trailing assertion stays on its line, rather than being carried down to
        // the own-line assertions stacked below it.
        assert_idempotent(
            "1 ~> f //= 2 // the result\n//= 'int // the type\n",
            "note above stacked own-line assertions",
        );
        // A comment dangling inside the container a step ends with keeps its place, and is not
        // captured by the assertion that follows the container.
        assert_formats(
            "f [\n  1\n  // why\n] //= 2\ng\n",
            "f [1] //= 2\n// why\ng\n",
        );
        // Note spacing normalizes to a single space, as any trailing comment does.
        assert_formats("5 //= 5    // spaced out\n", "5 //= 5 // spaced out\n");
        // A `//` inside the pattern's own string is not the note's marker.
        assert_idempotent("x //= \"http://a\" // a url\n", "url in an asserted string");
    }

    #[test]
    fn code_may_not_follow_an_assertion() {
        // Only a comment may follow the pattern; anything else is the error that keeps
        // `//= Point [x: 1]` from silently weakening to `//= Point` plus prose.
        assert!(parse("5 //= Point [x: 1]\n").is_err());
        assert!(parse("5 //= 5 6\n").is_err());
        // A second `//=` would otherwise be swallowed as the first one's note.
        assert!(parse("5 //= 5 //= 6\n").is_err());
    }

    #[test]
    fn chain_lines_keep_their_assertions_and_comments() {
        // Each line of a spread chain may end in an assertion or a comment, and both pin the
        // break: the `~>` must not be pulled up into what runs to the end of a line.
        assert_idempotent(
            "1 //= 1\n~> %num.add [~, 2] //= 3\n~> %num.mul [~, 3] //= 9\n",
            "assertion per chain line",
        );
        assert_idempotent(
            "1 // the head\n// above the continuation\n~> double\n",
            "comments in a continuation gap",
        );
        assert_idempotent(
            "1\n//= 'int\n//= 1\n~> double\n",
            "stacked own-line assertions mid-chain",
        );
        // The head of a chain ending in a container is normally flattened onto one line; an
        // assertion in the gap rules that out.
        assert_formats(
            "1 //= 1\n~> %list.map [~, double]\n",
            "1 //= 1\n~> %list.map [~, double]\n",
        );
    }

    #[test]
    fn preserves_leading_comments_and_blank_lines() {
        // Comments and a single blank line survive; a doubled blank collapses to one.
        let source = "// header\nx = 1\n\n\n// note\ny = 2";
        assert_formats(source, "// header\nx = 1\n\n// note\ny = 2\n");
    }

    #[test]
    fn preserves_comments_before_tuple_fields() {
        assert_formats(
            "r = [\n  // first\n  a: 1,\n  b: 2,\n]",
            "r = [\n  // first\n  a: 1,\n  b: 2,\n]\n",
        );
    }

    #[test]
    fn preserves_comment_before_branch() {
        // A comment on a branch must survive and not mangle the block (it forces it to break).
        let source = "f = #'int {\n  // guard\n  =0 => a\n  | b\n}";
        let printed = format_program(&parse(source).unwrap(), source);
        assert!(printed.contains("// guard"), "comment lost: {printed}");
        // ...and the result is a fixpoint.
        assert_eq!(printed, format_program(&parse(&printed).unwrap(), &printed));
    }

    #[test]
    fn comment_markers_inside_strings_are_not_trivia() {
        // The `//` lives inside a string literal: it round-trips as string content, not a comment.
        assert_formats("f = #{ \"x // y\" }", "f = #{ \"x // y\" }\n");
    }

    #[test]
    fn dialect_content_is_not_trivia() {
        // `//` text and blank lines inside `%mod{…}` raw content are content, not trivia: nothing
        // is harvested and re-emitted outside the dialect, and the content bytes stay verbatim.
        let source = "x = %json{ [1,\n  2] // not a comment\n}\nx";
        assert_formats(source, "x = %json{ [1,\n  2] // not a comment\n}\nx\n");
        assert_idempotent(source, "dialect comment");
        // A blank line inside the content doesn't leak a leading blank onto the next step.
        let source = "x = %json{ [1,\n\n  2] }\nx";
        assert_formats(source, "x = %json{ [1,\n\n  2] }\nx\n");
        assert_idempotent(source, "dialect blank line");
    }

    #[test]
    fn multiline_dialect_breaks_enclosing_layout() {
        // The embedded newlines count as breaks (literal lines): the enclosing block must not be
        // separator-joined around them, and the content is not re-indented.
        let source = "f = #{\n  d = %dict{ \"a\" => 1,\n    \"b\" => 2 }\n  d\n}\nf";
        assert_formats(
            source,
            "f = #{\n  d = %dict{ \"a\" => 1,\n    \"b\" => 2 }\n  d\n}\nf\n",
        );
        assert_idempotent(source, "multiline dialect");
    }

    #[test]
    fn drops_redundant_blocks() {
        // A single branchless, binding-free block is spliced into the surrounding chain.
        assert_formats(
            "bit = { f [a, b] ~> g [1, ~] ~> h }",
            "bit = f [a, b] ~> g [1, ~] ~> h\n",
        );
        // Nested redundant blocks collapse fully.
        assert_formats("x = { { 5 } }", "x = 5\n");
        // A block in a consequence position is also unwrapped.
        assert_formats(
            "f = #'int { =0 => { g [~, 1] ~> k } | h }",
            "f = #'int { =0 => g [~, 1] ~> k | h }\n",
        );
    }

    #[test]
    fn consequence_block_aligns_with_its_bar_line() {
        // A consequence block's `}` aligns with the bar (`|`) of the line carrying its `{`, indented
        // from the bar rather than from the content past the `| `.
        assert_formats(
            "f = #'t { =B[h, t] => { idx ~> =0 => h | %num.sub [idx, 1] ~> ^ [t, ~] } | other_branch }",
            "f = #'t {\n  | =B[h, t] => {\n    | idx ~> =0 => h\n    | %num.sub [idx, 1] ~> ^ [t, ~]\n  }\n  | other_branch\n}\n",
        );
        // The same when the block ends a branch's *condition* chain (`| lst { … }`): the body is one
        // chain, so the block stays at the bar indent and its `}` aligns with the `|`.
        assert_formats(
            "f = #'t { lst ~> { =Nil => empty_result | =Cons[h, t] => process [h, t] } | fallback }",
            "f = #'t {\n  | lst ~> {\n    | =Nil => empty_result\n    | =Cons[h, t] => process [h, t]\n  }\n  | fallback\n}\n",
        );
    }

    #[test]
    fn tall_steps_get_surrounding_blank_lines() {
        // A `~>` pipeline step is set off from its short neighbours with a blank line on each side…
        assert_formats(
            "#{ first_step; %bin.slice [target, %num.sub [target_len, suffix_len], target_len] ~> =^suffix; last_step }",
            "#{\n  first_step\n\n  %bin.slice [target, %num.sub [target_len, suffix_len], target_len]\n  ~> =^suffix\n\n  last_step\n}\n",
        );
        // …but a body of only short steps stays packed (no imposed blanks).
        assert_formats("#{ aa; bb; cc }", "#{ aa; bb; cc }\n");
    }

    #[test]
    fn steps_on_separate_lines_stay_separate() {
        // Short enough to join, but written a line apiece, so kept that way.
        let source = "f = #'int {\n  a = 1\n  b = 2\n  [a, b]\n}\n";
        assert_formats(source, source);
        assert_idempotent(source, "separate steps");
        assert_formats("x = 1\ny = 2\n", "x = 1\ny = 2\n");
        // A guard's steps follow the same rule.
        let source = "f = #[] {\n  | =A\n    g => 1\n  | 2\n}\n";
        assert_formats(source, source);
    }

    #[test]
    fn steps_joined_on_one_line_stay_joined() {
        assert_formats(
            "g = #'int { a = 1; b = 2; [a, b] }\n",
            "g = #'int { a = 1; b = 2; [a, b] }\n",
        );
        assert_formats(
            "h = %list.filter [bs, #{ $label ~> =\"CERTIFICATE\"; $ }]\n",
            "h = %list.filter [bs, #{ $label ~> =\"CERTIFICATE\"; $ }]\n",
        );
        // Runs are independent: a joined pair keeps its line among separate ones.
        let source = "m = #[] {\n  x = 1; y = 2\n  [x, y]\n}\n";
        assert_formats(source, source);
        assert_idempotent(source, "mixed runs");
    }

    #[test]
    fn a_joined_run_breaks_whole() {
        // Too wide for the line, the run breaks one step per line.
        assert_formats(
            "k = #[] { alpha_value = compute_something [1, 2, 3]; beta_value = compute_other_thing [alpha_value, 4, 5]; gamma }\n",
            "k = #[] {\n  alpha_value = compute_something [1, 2, 3]\n  beta_value = compute_other_thing [alpha_value, 4, 5]\n  gamma\n}\n",
        );
        // A step that breaks across lines takes the run with it, rather than leaving `; y`
        // dangling after its last line.
        assert_formats("#{ x = [1,]; y }\n", "#{\n  x = [\n    1,\n  ]\n  y\n}\n");
        // A run broken by width reads back as separate lines, so the layout is stable.
        assert_idempotent(
            "k = #[] { alpha_value = compute_something [1, 2, 3]; beta_value = compute_other_thing [alpha_value, 4, 5]; gamma }\n",
            "broken run",
        );
    }

    #[test]
    fn steps_in_an_interpolation_hole_stay_on_its_line() {
        // A hole has no lines to give: steps written apart in one rejoin with `;`.
        assert_formats(
            "s = \"\"\"\n  pick {a = 1\n  a} done\n  \"\"\"\n",
            "s = \"\"\"\npick {a = 1; a} done\n\"\"\"\n",
        );
    }

    #[test]
    fn a_trailing_comma_holds_a_list_open() {
        // Values, puns, patterns, partial patterns and types alike.
        assert_formats("x = [1, 2,]\n", "x = [\n  1,\n  2,\n]\n");
        assert_formats("x = (a, b,)\n", "x = (\n  a,\n  b,\n)\n");
        assert_formats("[a, b,] = x\n", "[\n  a,\n  b,\n] = x\n");
        assert_formats("(a, b,) = x\n", "(\n  a,\n  b,\n) = x\n");
        // A pattern is not a container a chain opens onto, so the chain breaks at its gap first.
        assert_formats("x ~> =P[a, b,]\n", "x\n~> =P[\n  a,\n  b,\n]\n");
        assert_formats(
            "f = #[a: 'int, b: 'int,] { $a }\n",
            "f = #[\n  a: 'int,\n  b: 'int,\n] { $a }\n",
        );
        assert_formats("'p = P[x: 'int,]\n", "'p = P[\n  x: 'int,\n]\n");
        // A comment between the last entry and the comma does not hide it.
        assert_formats("x = [1 // one\n,]\n", "x = [\n  1, // one\n]\n");
        // Without one, the list lays out by width: a list broken in the source may rejoin.
        assert_formats("x = [1, 2]\n", "x = [1, 2]\n");
        assert_formats("x = [\n  1,\n  2\n]\n", "x = [1, 2]\n");
        assert_formats("f = #[\n  a: 'int\n] { $a }\n", "f = #[a: 'int] { $a }\n");
        // The broken layout writes the comma, so it sticks.
        assert_idempotent("x = [1, 2,]\n", "sticky tuple");
        assert_idempotent("[a, b,] = x\n", "sticky pattern");
    }

    #[test]
    fn a_wide_union_head_wraps() {
        let source = "truncate = #(['instant, 'day_unit] | ['duration, 'clock_unit] | ['time, 'clock_unit] | ['datetime, ('date_unit | 'day_unit)] | ['date, 'date_unit] | ['zoned, ('date_unit | 'day_unit)]) {\n  a = 1\n  a\n}\n";
        assert_formats(
            source,
            "truncate = #(\n  | ['instant, 'day_unit]\n  | ['duration, 'clock_unit]\n  | ['time, 'clock_unit]\n  | ['datetime, ('date_unit | 'day_unit)]\n  | ['date, 'date_unit]\n  | ['zoned, ('date_unit | 'day_unit)]\n) {\n  a = 1\n  a\n}\n",
        );
        assert_idempotent(source, "wrapped union head");
        // A union that fits stays inline.
        assert_formats("f = #('int | 'bin) { $ }\n", "f = #('int | 'bin) { $ }\n");
        // A union nested in a field wraps inside the field once the list has broken.
        let source = "f = #[mode: (Alpha_mode_value | Beta_mode_value | Gamma_mode_value | Delta_mode_value | Epsilon_mode_value), n: 'int] { $n }\n";
        assert_formats(
            source,
            "f = #[\n  mode: (\n    | Alpha_mode_value\n    | Beta_mode_value\n    | Gamma_mode_value\n    | Delta_mode_value\n    | Epsilon_mode_value\n  ),\n  n: 'int,\n] { $n }\n",
        );
        assert_idempotent(source, "wrapped union field");
        // A result type wraps too.
        let source = "f = #'int -> (Alpha_result_value | Beta_result_value | Gamma_result_value | Delta_result_value | Epsilon) { A }\n";
        assert_formats(
            source,
            "f = #'int -> (\n  | Alpha_result_value\n  | Beta_result_value\n  | Gamma_result_value\n  | Delta_result_value\n  | Epsilon\n) { A }\n",
        );
        assert_idempotent(source, "wrapped result union");
    }

    #[test]
    fn a_union_alias_keeps_its_layout() {
        let source = "'zoning = ['instant, 'zone] | ['datetime, 'zone] | ['datetime_long_name_here, 'zone] | ['other_thing, 'zone_x]\n";
        assert_formats(
            source,
            "'zoning =\n  | ['instant, 'zone]\n  | ['datetime, 'zone]\n  | ['datetime_long_name_here, 'zone]\n  | ['other_thing, 'zone_x]\n",
        );
        assert_formats("'bool = True | False\n", "'bool = True | False\n");
    }

    #[test]
    fn a_chain_ending_in_a_container_breaks_at_its_gaps_before_opening_it() {
        // The trailing container absorbs the overflow while the head still fits on the line it
        // opens on — the head is measured against that line, not against the container's full width.
        assert_formats(
            "#{ value ~> %mod.call [aaaaaaaaaa, bbbbbbbbbb, cccccccccc, dddddddddd, eeeeeeeeee, ffffffffff, gggggggggg, hhhhhhhhhh, iiiiiiiiii] }",
            "#{\n  value ~> %mod.call [\n    aaaaaaaaaa,\n    bbbbbbbbbb,\n    cccccccccc,\n    dddddddddd,\n    eeeeeeeeee,\n    ffffffffff,\n    gggggggggg,\n    hhhhhhhhhh,\n    iiiiiiiiii,\n  ]\n}\n",
        );
        // Once the head no longer fits there, the chain breaks at its own `~>` gaps — every term on
        // its own line, the container back on one. Taking the container apart instead would leave
        // the head running past the width with an argument list dangling under it.
        assert_formats(
            "#{ %aa.one [~, 1] ~> %bb.two [~, 2] ~> %cc.three [~, 3] ~> %dd.four [~, 4] ~> %ee.five [~, 5] }",
            "#{\n  %aa.one [~, 1]\n  ~> %bb.two [~, 2]\n  ~> %cc.three [~, 3]\n  ~> %dd.four [~, 4]\n  ~> %ee.five [~, 5]\n}\n",
        );
    }

    #[test]
    fn binding_pipeline_breaks_after_equals() {
        // A binding whose value breaks into a `~>` pipeline breaks after the `=` and indents the
        // pipeline, rather than leaving the head on the `=` line and dangling the continuations.
        assert_formats(
            "#{ char_end = byte_position ~> advance_one ~> skip_continuation_bytes ~> clamp_to_length ~> check_boundary }",
            "#{\n  char_end =\n    byte_position\n    ~> advance_one\n    ~> skip_continuation_bytes\n    ~> clamp_to_length\n    ~> check_boundary\n}\n",
        );
        // A value ending in a call is a pipeline too: the `=` break tracks whether the chain breaks
        // at a `~>` gap, not what its last term happens to be.
        assert_formats(
            "#{ char_end = byte_pos ~> %num.add [~, 1] ~> skip_continuation [data, ~, data_len] ~> %num.mul [~, 2] }",
            "#{\n  char_end =\n    byte_pos\n    ~> %num.add [~, 1]\n    ~> skip_continuation [data, ~, data_len]\n    ~> %num.mul [~, 2]\n}\n",
        );
        // …but a value that only breaks *inside* a trailing container is not: the container opens on
        // the `=` line and delimits itself, so moving the value down would gain nothing.
        assert_formats(
            "#{ y = value ~> %mod.call [aaaaaaaaaa, bbbbbbbbbb, cccccccccc, dddddddddd, eeeeeeeeee, ffffffffff, gg] }",
            "#{\n  y = value ~> %mod.call [\n    aaaaaaaaaa,\n    bbbbbbbbbb,\n    cccccccccc,\n    dddddddddd,\n    eeeeeeeeee,\n    ffffffffff,\n    gg,\n  ]\n}\n",
        );
        // A short binding stays inline…
        assert_formats(
            "#{ x = byte_pos ~> %num.add [~, 1] }",
            "#{ x = byte_pos ~> %num.add [~, 1] }\n",
        );
        // …and a value ending in a self-breaking container stays on the `=` line (the container
        // opens there and breaks internally).
        assert_formats(
            "#{ xs = [aaaaaaaaaa, bbbbbbbbbb, cccccccccc, dddddddddd, eeeeeeeeee, ffffffffff, gggggggggg, hhhhhhhhhh] }",
            "#{\n  xs = [\n    aaaaaaaaaa,\n    bbbbbbbbbb,\n    cccccccccc,\n    dddddddddd,\n    eeeeeeeeee,\n    ffffffffff,\n    gggggggggg,\n    hhhhhhhhhh,\n  ]\n}\n",
        );
    }

    #[test]
    fn guard_pipeline_indents_continuations() {
        // A `cond => …` guard that is a single `~>` pipeline indents its continuations under the
        // head (past the `| `), so they read as part of the condition rather than dangling at the bar.
        assert_formats(
            "#{ x ~> { haystack_length ~> subtract_needle_length ~> find_index_in_haystack ~> check_found_index ~> confirm_boundary => Ok | other } }",
            "#{\n  x ~> {\n    | haystack_length\n      ~> subtract_needle_length\n      ~> find_index_in_haystack\n      ~> check_found_index\n      ~> confirm_boundary => Ok\n    | other\n  }\n}\n",
        );
        // A consequence that follows the last continuation on the same line lays out relative to
        // that deeper indent too — a breaking tuple's fields indent under its `[`, `]` aligned.
        assert_formats(
            "#{ x ~> { find_index_from [haystack, delimiter, start_offset, end_offset] ~> =('int & index) => [%bin.slice [haystack, start_offset, index] ~> Str[~], %num.add [index, delim_len], another_field, one_more_field] | other } }",
            "#{\n  x ~> {\n    | find_index_from [haystack, delimiter, start_offset, end_offset]\n      ~> =('int & index) => [\n        %bin.slice [haystack, start_offset, index] ~> Str[~],\n        %num.add [index, delim_len],\n        another_field,\n        one_more_field,\n      ]\n    | other\n  }\n}\n",
        );
    }

    #[test]
    fn a_body_opening_term_keeps_the_term_before_it_on_its_line() {
        // The block writes itself out below the line it opens on, so it stays attached to what
        // feeds it; the rest of the chain still takes a line per term.
        assert_formats(
            "#{ %bin.get_byte [$data, p1] ~> { =40 => OpenParenthesis | =41 => CloseParenthesis | Other } ~> =('token & t) }",
            "#{\n  %bin.get_byte [$data, p1] ~> {\n    | =40 => OpenParenthesis\n    | =41 => CloseParenthesis\n    | Other\n  }\n  ~> =('token & t)\n}\n",
        );
        // A call's argument list is not a body: it may break, but the chain breaks with it rather
        // than gluing the call to what precedes it.
        assert_formats(
            "#{ $acc ~> %bin.append [~, %int.divide [b0, 4] ~> b64_digit, 1] ~> %bin.append [~, %int.modulo [b0, 4] ~> b64_digit, 1] ~> =('bin & out) }",
            "#{\n  $acc\n  ~> %bin.append [~, %int.divide [b0, 4] ~> b64_digit, 1]\n  ~> %bin.append [~, %int.modulo [b0, 4] ~> b64_digit, 1]\n  ~> =('bin & out)\n}\n",
        );
    }

    #[test]
    fn comments_inside_a_type_stay_in_it() {
        // A parameter list is laid out as a document, so its entries anchor trivia like a value
        // tuple's fields do — before this the comment had no anchor inside the type and drifted
        // down to the first step of the body.
        assert_formats(
            "f = #[\n  // the mode we run in\n  (mode): 'vmode,\n  (state): 's, // carried through\n  (frame): 'frame,\n] { $mode }",
            "f = #[\n  // the mode we run in\n  (mode): 'vmode,\n  (state): 's, // carried through\n  (frame): 'frame,\n] { $mode }\n",
        );
        // A type reached in a flat-rendered position has no line to carry a comment, so an item
        // there still attaches outward — to the next entry of the list that *is* laid out.
        assert_formats(
            "'wrap = [outer: #[\n  // inner note\n  a: 'int\n] -> 'int, other: 'bin]",
            "'wrap = [\n  outer: #[a: 'int] -> 'int,\n  // inner note\n  other: 'bin,\n]\n",
        );
    }

    #[test]
    fn a_wide_type_breaks_its_field_list() {
        // A parameter list too wide for the line breaks one field per line, with the trailing comma
        // its grammar accepts — the head is the only place a type has room to give.
        assert_formats(
            "f = #<'k, 'v>[(self): #^ -> '<'k, 'v>, (h1): 'int, (k1): 'k, (v1): 'v, (h2): 'int, (k2): 'k, (shift): 'int] { $h1 }",
            "f = #<'k, 'v>[\n  (self): #^ -> '<'k, 'v>,\n  (h1): 'int,\n  (k1): 'k,\n  (v1): 'v,\n  (h2): 'int,\n  (k2): 'k,\n  (shift): 'int,\n] { $h1 }\n",
        );
        // A union member breaks the same way, indented under its own name so the closing bracket
        // lines up with it rather than landing in the `|` gutter.
        assert_formats(
            "'shape = Circle[radius: 'int] | Rectangle[width_in_pixels: 'int, height_in_pixels: 'int, origin_x: 'int, origin_y: 'int, fill: 'bin]",
            "'shape =\n  | Circle[radius: 'int]\n  | Rectangle[\n      width_in_pixels: 'int,\n      height_in_pixels: 'int,\n      origin_x: 'int,\n      origin_y: 'int,\n      fill: 'bin,\n    ]\n",
        );
    }

    #[test]
    fn a_wide_pattern_breaks_its_field_list() {
        // Breaking the pattern is the last resort — a chain gap goes first — so this one has no
        // gap to give.
        assert_formats(
            "#{ Something[alpha: aaaaaaaaaa, beta: bbbbbbbbbb, gamma: gggggggggg, delta: dddddddddd, epsilon: eeeeeeeeee] = source }",
            "#{\n  Something[\n    alpha: aaaaaaaaaa,\n    beta: bbbbbbbbbb,\n    gamma: gggggggggg,\n    delta: dddddddddd,\n    epsilon: eeeeeeeeee,\n  ] = source\n}\n",
        );
    }

    #[test]
    fn a_wide_guard_lays_out_one_step_per_line() {
        // Past the threshold the guard's steps each take a line, and the consequence follows the
        // last of them — so the arm fits without the consequence having to blow its argument list
        // apart to make room, which is all it could do while the guard was flattened.
        assert_formats(
            "#{ x ~> { __integer_compare__ [$, 48] ~> =(0 | 1); __integer_compare__ [$, 57] ~> =(-1 | 0) => __integer_subtract__ [$, 48] | other } }",
            "#{\n  x ~> {\n    | __integer_compare__ [$, 48] ~> =(0 | 1)\n      __integer_compare__ [$, 57] ~> =(-1 | 0) => __integer_subtract__ [$, 48]\n    | other\n  }\n}\n",
        );
        // A guard that fits stays on one line even when the consequence does not: the consequence
        // breaks itself rather than dragging the guard apart, indenting under the arm's content so
        // its `]` lines up with the term that opened it.
        assert_formats(
            "#{ x ~> { =Cons[k, t] => [alpha_value, beta_value, gamma_value, delta_value, epsilon_value, zeta_value, eta_value] | other } }",
            "#{\n  x ~> {\n    | =Cons[k, t] => [\n        alpha_value,\n        beta_value,\n        gamma_value,\n        delta_value,\n        epsilon_value,\n        zeta_value,\n        eta_value,\n      ]\n    | other\n  }\n}\n",
        );
    }

    #[test]
    fn wraps_breaking_consequence_pipeline() {
        // A consequence that is a single chain breaking into a `~>` pipeline is wrapped in grouping
        // braces, so the continuation reads as a delimited body instead of dangling at the bar.
        assert_formats(
            "f = #'t { =Cons[k, t] => dict ~> put_entry_into ~> normalise_keys ~> rebalance_tree ~> recount_nodes ~> settle ~> finish | =Nil => dict }",
            "f = #'t {\n  | =Cons[k, t] => {\n    dict\n    ~> put_entry_into\n    ~> normalise_keys\n    ~> rebalance_tree\n    ~> recount_nodes\n    ~> settle\n    ~> finish\n  }\n  | =Nil => dict\n}\n",
        );
        // A short consequence stays bare (it does not break).
        assert_formats(
            "f = #'t { =A => a ~> b ~> c | =B => d }",
            "f = #'t { =A => a ~> b ~> c | =B => d }\n",
        );
    }

    #[test]
    fn groups_compound_consequences() {
        // A bare multi-step frame-free consequence is wrapped in grouping braces…
        assert_formats(
            "x = 5 ~> { =0 => a; b | c }",
            "x = 5 ~> { =0 => { a; b } | c }\n",
        );
        // …a single-step consequence (one chain, many terms) stays bare…
        assert_formats(
            "x = 5 ~> { =0 => a ~> b ~> c | d }",
            "x = 5 ~> { =0 => a ~> b ~> c | d }\n",
        );
        // …a binding consequence keeps its own markers and stays bare…
        assert_formats(
            "x = 5 ~> { =0 => y = 1; g [y, 2] | c }",
            "x = 5 ~> { =0 => y = 1; g [y, 2] | c }\n",
        );
        // …and an already-braced consequence is not double-wrapped.
        assert_formats(
            "x = 5 ~> { =0 => { a; b } | c }",
            "x = 5 ~> { =0 => { a; b } | c }\n",
        );
    }

    #[test]
    fn compound_consequence_steps_line_up() {
        // A consequence's later steps and its first step's `~>` continuations share one indent, a
        // level past the arm's content, with no blank line imposed between them.
        let source = "f = #[] { | cond => fixed [data, iadd [e2, 1], 2, \"two-digit seconds are here to make this long\"] ~> =[sec, e3]; [[m, sec], e3] | 0 }";
        assert_formats(
            source,
            "f = #[] {\n  | cond => fixed [data, iadd [e2, 1], 2, \"two-digit seconds are here to make this long\"]\n      ~> =[sec, e3]\n      [[m, sec], e3]\n  | 0\n}\n",
        );
        assert_idempotent(source, "compound consequence with a pipeline step");
        // Steps that do not break internally sit at the same indent.
        assert_formats(
            "f = #[] { | =A => x = fetch_the_value [alpha, beta]; y = combine [x, gamma, delta]; finish [x, y, epsilon, zeta] | 0 }",
            "f = #[] {\n  | =A => x = fetch_the_value [alpha, beta]\n      y = combine [x, gamma, delta]\n      finish [x, y, epsilon, zeta]\n  | 0\n}\n",
        );
        // After a broken guard, the consequence's steps go a level past the guard's.
        assert_formats(
            "f = #[] { | first_guard_step [alpha, beta, gamma]; second_guard_step [delta, epsilon] ~> =Ok => x = fetch [a]; finish [x, y, alpha, beta, gamma, delta, epsilon, zeta] | 0 }",
            "f = #[] {\n  | first_guard_step [alpha, beta, gamma]\n    second_guard_step [delta, epsilon] ~> =Ok => x = fetch [a]\n      finish [x, y, alpha, beta, gamma, delta, epsilon, zeta]\n  | 0\n}\n",
        );
    }

    #[test]
    fn compound_branch_steps_line_up_past_the_bar() {
        // A multi-step branch without `=>` lays its first step's `~>` continuations and its later
        // steps under the content past the `| `, not in the bar's gutter.
        let source = "f = #[] { | fixed [data, iadd [e2, 1], 2, \"two-digit seconds are here to make this long\"] ~> =[sec, e3]; [[m, sec], e3] | 0 }";
        assert_formats(
            source,
            "f = #[] {\n  | fixed [data, iadd [e2, 1], 2, \"two-digit seconds are here to make this long\"]\n    ~> =[sec, e3]\n    [[m, sec], e3]\n  | 0\n}\n",
        );
        assert_idempotent(source, "compound branch with a pipeline step");
        // A single chain ending in data keeps its wrapped lines at the bar, closing its delimiter
        // under the `|`.
        assert_formats(
            "f = #[] { | =A => a | build [alpha_value, beta_value, gamma_value, delta_value, epsilon_value, zeta_value, eta_value, theta] }",
            "f = #[] {\n  | =A => a\n  | build [\n    alpha_value,\n    beta_value,\n    gamma_value,\n    delta_value,\n    epsilon_value,\n    zeta_value,\n    eta_value,\n    theta,\n  ]\n}\n",
        );
    }

    #[test]
    fn flatten_paths_do_not_corrupt_comments() {
        // Regression: flattening a chain head or guard condition that carries a comment must not
        // inline the comment (which would comment out the rest of the line). The output must reparse
        // and be a fixpoint — the corrupt output of the old bug did neither.
        assert_idempotent(
            "#{\n  // c\n  foo\n} ~> g ~> [x, y]",
            "commented chain head",
        );
        assert_idempotent(
            "5 ~> {\n  [ z: ~, // c\n  ] ~> foo? => x | y\n}",
            "comment on a guard field",
        );
    }

    #[test]
    fn calls_take_their_preferred_spelling() {
        // A value piped into a call, and nothing more, is the juxtaposition.
        assert_formats("x ~> f\n", "f x\n");
        assert_formats("x ~> f ~\n", "f x\n");
        assert_formats("y = [1, 2] ~> %num.add\n", "y = %num.add [1, 2]\n");
        assert_formats("[a, b] ~> ^\n", "^ [a, b]\n");
        assert_formats("5 ~> @w ~\n", "@w 5\n");
        assert_formats("7 ~> @\n", "@ 7\n");
        // In a longer chain a call drops its `~`, and a tuple or string argument moves into it.
        assert_formats("x ~> f ~ ~> g ~\n", "x ~> f ~> g\n");
        assert_formats(
            "x ~> [~, 2] ~> %num.add ~> d\n",
            "x ~> %num.add [~, 2] ~> d\n",
        );
        assert_formats("[a, b] ~> f ~> g\n", "f [a, b] ~> g\n");
        assert_formats("x ~> \"a{~}\" ~> d\n", "x ~> d \"a{~}\"\n");
        // A block, a match or a tail call heading the chain stays put.
        assert_formats("x = 5 ~> { ^ ~> foo }\n", "x = 5 ~> { ^ ~> foo }\n");
        assert_formats("x ~> { =1 => a | b } ~> g\n", "x ~> { =1 => a | b } ~> g\n");
        // Braces around a bare name keep it a value, so they are not dropped.
        assert_formats("5 ~> { d }\n", "5 ~> { d }\n");
        // A gap carrying an assertion or a comment is not merged away.
        assert_formats("x //= 1\n~> d\n", "x //= 1\n~> d\n");
        assert_formats("x // why\n~> d\n", "x // why\n~> d\n");
    }

    #[test]
    fn keeps_narrowing_barrier_and_unsafe_tail_blocks() {
        // A block wrapping a match can be a deliberate narrowing barrier — never strip it.
        assert_formats(
            "x = 5 ~> { { =A } => 1 | 2 }",
            "x = 5 ~> { { =A } => 1 | 2 }\n",
        );
        // A tail call that is not the chain's last term keeps its block (no mid-chain dead code)…
        assert_formats("x = { ^ [a] } ~> f", "x = { ^ [a] } ~> f\n");
        // …a non-final tail call inside the body keeps the block too…
        assert_formats("x = 5 ~> { ^ ~> foo }", "x = 5 ~> { ^ ~> foo }\n");
        // …but a block whose tail call ends up last after splicing is dropped cleanly.
        assert_formats("x = y ~> { ^ [a] }", "x = y ~> ^ [a]\n");
    }

    #[test]
    fn keeps_meaningful_blocks() {
        // A binding inside the block would leak if inlined, so the block stays.
        assert_formats("x = { 5 ~> =y; y }", "x = { 5 ~> =y; y }\n");
        // Multiple branches are not redundant.
        assert_formats("x = 5 ~> { =0 => a | b }", "x = 5 ~> { =0 => a | b }\n");
        // A comment inside the block keeps it (so the comment is not lost).
        assert_formats("x = {\n  // note\n  f 5\n}", "x = {\n  // note\n  f 5\n}\n");
    }

    #[test]
    fn renders_group_a_sugar() {
        // Strings, `$`-access sugar, and parenthesised unions.
        assert_formats("x = \"hi\"", "x = \"hi\"\n");
        assert_formats("f = #'pt { $.x }", "f = #'pt { $x }\n");
        assert_formats("f = #['int, 'int] { $.0 }", "f = #['int, 'int] { $0 }\n");
        assert_formats("'t = [f: 'int | 'bin]", "'t = [f: ('int | 'bin)]\n");
        // A top-level union alias stays bare.
        assert_formats("'bool = True | False", "'bool = True | False\n");
    }

    #[test]
    fn string_escapes_round_trip() {
        // Guard the encode table in `string_literal` against drifting from the parser's decoder: each
        // formatted string must re-parse to the identical bytes (a fixpoint).
        for source in [
            r#""plain""#,
            r#""a\nb""#,
            r#""tab\tend""#,
            r#""back\\slash""#,
            r#""cr\rlf""#,
            // An embedded quote now round-trips as `\"` rather than falling back to `Str[<…>]`.
            r#""say \"hi\"""#,
        ] {
            assert_idempotent(source, source);
        }
    }

    #[test]
    fn interpolation_round_trips() {
        // Holes render tightly (`{name}`), literal braces escape as `\{`, and nested strings,
        // adjacent holes, and multi-branch holes all survive a format round-trip unchanged.
        for source in [
            "x = \"hello {name}!\"",
            "x = \"{a}{b}\"",
            "x = \"a \\{ b\"",
            "x = \"pair: {[p, q] ~> %str.concat}\"",
            "x = \"v: {flag ~> { =Ok => \"yes\" | \"no\" }}\"",
        ] {
            assert_idempotent(source, source);
        }
    }

    #[test]
    fn interpolation_formats_tightly() {
        // A simple hole renders with no inner padding.
        assert_formats("x = \"hi {name}\"", "x = \"hi {name}\"\n");
    }

    #[test]
    fn string_style_is_preserved() {
        // A single-line string keeps its style even when its value contains a newline (escaped as
        // `\n`), rather than being rewritten as a `"""` block.
        assert_formats("x = \"a\\nb\"", "x = \"a\\nb\"\n");
        // A multi-line string keeps its style even when its value has no newline.
        assert_formats("x = \"\"\"\n  hi\n  \"\"\"", "x = \"\"\"\nhi\n\"\"\"\n");
    }

    #[test]
    fn string_pattern_renders_as_string() {
        // A string pattern reconstructs as `"…"`, not the desugared `Str[<…>]`.
        assert_formats(
            "f = #{ role ~> =\"admin\" }",
            "f = #{ role ~> =\"admin\" }\n",
        );
    }

    #[test]
    fn literal_str_tuple_is_not_canonicalised() {
        // A hand-written `Str[<…>]` tuple stays a tuple — only string *literals* render as `"…"`.
        assert_formats("x = Str[<68>]", "x = Str[<68>]\n");
    }

    #[test]
    fn binary_rows_are_kept_and_indentation_normalises() {
        // The rows are the author's — a table laid out to match a spec is not reflowed —
        // while the indentation is the formatter's, and follows the enclosing context.
        assert_formats(
            "k = <\n0a1b 2c3d\n4e5f 6071\n>",
            "k = <\n  0a1b 2c3d\n  4e5f 6071\n>\n",
        );
        assert_formats(
            "f = #{ <\n0a1b\n2c3d\n> }",
            "f = #{\n  <\n    0a1b\n    2c3d\n  >\n}\n",
        );
        // Blank lines carry no bytes, so they collapse rather than becoming empty rows.
        assert_formats("k = <\n0a1b\n\n\n2c3d\n>", "k = <\n  0a1b\n  2c3d\n>\n");
    }

    #[test]
    fn binary_pattern_collapses_onto_one_line() {
        // A pattern renders on one line, so a binary written across rows flattens there —
        // exactly as a `"""` string in pattern position renders as a single-line `"…"`.
        assert_formats(
            "f = #{ $ ~> =<\n0a1b\n2c3d\n> }",
            "f = #{ $ ~> =<0a1b 2c3d> }\n",
        );
    }

    #[test]
    fn binary_grouping_is_kept_but_separators_normalise() {
        // Where the groups divide is the author's, so it survives; how wide the gap is is not.
        assert_formats("k = <6a09e667   bb67ae85>", "k = <6a09e667 bb67ae85>\n");
        // Regrouping would destroy meaning (this is a UUID's 4-2-2-2-6 division), so it is
        // left exactly as written even though the bytes are one run.
        assert_formats(
            "id = <550e8400 e29b 41d4 a716 446655440000>",
            "id = <550e8400 e29b 41d4 a716 446655440000>\n",
        );
    }

    #[test]
    fn multiline_strings_round_trip() {
        // Each multi-line source must format to an identical fixpoint that preserves the value. The
        // cases exercise margin tracking at depth, leading/trailing whitespace, blank lines, an
        // embedded triple-quote, and a value that is a single bare newline.
        for source in [
            "msg = \"\"\"\n    hello\n      indented\n    \"\"\"",
            // Nested inside a tuple, so the margin is driven by the ambient indentation.
            "r = [\n  a: \"\"\"\n    one\n    two\n    \"\"\",\n]",
            // A trailing space (encoded as `\s`) and an interior blank line.
            "msg = \"\"\"\n    keep \n\n    end\n    \"\"\"",
            // A value containing `"""` (written `\"""`) must re-encode so it can't close early.
            "msg = \"\"\"\n    a \\\"\"\" b\n    \"\"\"",
            // A value that is exactly one newline.
            "msg = \"\"\"\n\n    \"\"\"",
        ] {
            let ast = parse(source).unwrap_or_else(|e| panic!("source must parse: {e:?}"));
            let printed = format_program(&ast, source);
            assert_idempotent(&printed, &printed);
        }
    }

    #[test]
    fn multiline_interpolation_round_trips() {
        // Holes in a multi-line string: inline on a line, the whole of a line, with surrounding
        // text/spaces, a nested-string hole, an escaped literal brace, and a hole that itself spans
        // lines. Each must format to a value-preserving fixpoint.
        for source in [
            "s = \"\"\"\n    hi {name}\n    {x}\n    a \\{ b {[p, q] ~> %str.concat} c\n    \"\"\"",
            // A hole whose expression spans multiple lines stays a single hole.
            "s = \"\"\"\n    pick {flag ~> {\n      =Ok => \"y\"\n      | \"n\"\n    }} done\n    \"\"\"",
        ] {
            let ast = parse(source).unwrap_or_else(|e| panic!("source must parse: {e:?}"));
            let printed = format_program(&ast, source);
            assert_idempotent(&printed, &printed);
        }
    }

    #[test]
    fn dangling_comment_after_last_node_is_kept() {
        assert_formats("x = 1\n// tail", "x = 1\n// tail\n");
    }

    #[test]
    fn trailing_comment_stays_on_its_node_line() {
        // A same-line comment trails the step it follows, and pushes the next step onto a new line.
        assert_formats("x = 1 // note\ny = 2", "x = 1 // note\ny = 2\n");
    }

    #[test]
    fn trailing_comment_on_tuple_field() {
        assert_formats(
            "r = [\n  a: 1, // one\n  b: 2,\n]",
            "r = [\n  a: 1, // one\n  b: 2,\n]\n",
        );
    }

    fn std_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("compiler crate has a parent (repo root)")
            .join("std")
    }

    /// Every `.qv` under `dir`, recursively — the standard library's largest modules live in
    /// subdirectories (`html/live.qv`, `http/server.qv`), and they exercise the printer hardest.
    fn source_files(dir: &Path, files: &mut Vec<PathBuf>) {
        let entries =
            std::fs::read_dir(dir).unwrap_or_else(|e| panic!("read std dir {dir:?}: {e:?}"));
        for entry in entries {
            let path = entry.unwrap().path();
            if path.is_dir() {
                source_files(&path, files);
            } else if path.extension().is_some_and(|ext| ext == "qv") {
                files.push(path);
            }
        }
    }

    #[test]
    fn idempotent_over_std() {
        let dir = std_dir();
        let mut files = Vec::new();
        source_files(&dir, &mut files);
        files.sort();
        assert!(!files.is_empty(), "expected std/*.qv files in {dir:?}");
        for path in files {
            let source = std::fs::read_to_string(&path).unwrap();
            let label = path
                .strip_prefix(&dir)
                .unwrap_or(&path)
                .to_string_lossy()
                .to_string();
            assert_idempotent(&source, &label);
        }
    }

    #[test]
    fn idempotent_over_corpus() {
        // A snippet corpus exercising every AST variant the printer must handle. Each must parse
        // and reach a print fixpoint.
        let corpus: &[&str] = &[
            // --- step-final assertions ---
            "5 //= 5",
            "x = f y //= Ok // the note runs to end of line",
            // --- argument-first application (chain terms joined by `~>`) ---
            "[3, 4] ~> add ~ ~> [~, 2] ~> mul ~",
            "x ~> f ~",
            "[[x] ~> g ~, y] ~> f ~",
            "[3, 4] ~> __integer_add__ ~",
            "g ~> f ~",
            "5 ~> f",
            // --- bare ripple (flowing-value) terms: no juxtaposed argument ---
            "5 ~> ~",
            "num ~> ~.add",
            "g ~> ^~ []",
            "g ~> @~ []",
            // --- argument-first tail calls ---
            "[a, b] ~> ^ ~",
            "[x] ~> ^foo ~",
            // --- spawn (the block is part of the spawn, so it stays a single term) ---
            "@f []",
            "@",
            "@ 5",
            "@'int { $ }",
            "@('int | 'bin) { $ }",
            "x ~> @counter ~",
            // --- select / process / references ---
            "!'int",
            "!'int ~> { =0 => Ok | [] }",
            "!'%proc.changed",
            "!'%list<'int>",
            "!'int { =42 => Ok | [] }",
            "!('int | 'bin)",
            "!(x: 'int)",
            "!Done",
            "!Reply['ref, 'bin]",
            "!Reply['ref, 'bin] { =Reply[^id, _]; Ok }",
            "!#['int, 'int]",
            "@'%proc.changed { $ }",
            "@Done { $ }",
            "@Reply['int] { $ }",
            "@['int, 'int] { $ }",
            "@[] { $ }",
            "@((x: 'int)) { $ }",
            "@<'t>'t { $ }",
            "@<'a, 'b>['a, 'b] { $ }",
            "@#'int -> ('int | 'bin) { $ }",
            "@#'int 1",
            "![p, 1000]",
            "!p",
            "![]",
            "f",
            "__integer_add__",
            // --- binary literals: the author's digit grouping is preserved as written ---
            "<0a1b>",
            "<>",
            "<6a09e667 bb67ae85>",
            "<550e8400 e29b 41d4 a716 446655440000>",
            "42 ~> pid ~",
            "@3",
            // --- ripple / spread values ---
            "5 ~> [~, 1]",
            "0 ~> Point[x: ~, y: ~]",
            "a[..., y: 3]",
            "~[..., y: 3]",
            "A[x: 1] ~> B[...]",
            "[...a, ...b]",
            "[w: 0, ...a]",
            // --- field access / self / operators ---
            "point.x ~> .name",
            "$ ~> $.x ~> $.0",
            "[] ~> =[]",
            "42 ~> @ ~",
            // --- match forms ---
            "=Point[x, y]",
            "=(x: 'int)",
            "=Config*",
            "=*",
            "=_",
            "=^y",
            "=('int & n)",
            "=(('int | 'bin) & v)",
            "=((x: 'int) & p)",
            "=(Point(x: 'int) & p)",
            // A resource type renders bare, so the binder supplies the only pair.
            "=(+File & fd)",
            "('bin & ip) = f x; ip",
            "=([a] | [b])",
            // Negation glues to what it negates, and a lone negation heads a binder.
            "=\\[]",
            "=A[b: \\'int, c: \\^y]",
            "=\\(0 | 1)",
            "=(\\[] & n)",
            // A ripple marks what the match yields; `~>` after a pattern stays a continuation.
            "=[~]",
            "=[~, [~, 'int], _] ~> f ~",
            "=(Ok[~] | Err[_] & ~)",
            "[~] = x",
            "(\\[] & n) = f x; n",
            // An alternation head renders its own pair, and takes a binder like a type head.
            "=((32 | 9 | 10 | 13) & b)",
            "=(([a, _] | [_, a]) & whole)",
            "='int",
            "=Circle[radius: r]",
            "=\"hello\"",
            "=42",
            "=<0a1b>",
            "=<0a1b 2c3d>",
            "Point[x, y] = p",
            "x = 5",
            "(a, b) = p",
            "[x: a, y: b] = p",
            "Config(host, port) = c",
            // --- blocks / branches / consequence ---
            "v ~> { =0 => \"zero\" | \"neg\" }",
            "item ~> { is_valid? ~> process | [] ~> show_error }",
            "{ =Square[x] ~> [x, 10] ~> num.gt? => \"large\" | \"small\" }",
            // --- functions (the function body block is part of the term) ---
            "xs ~> [~, #{ $0 }, Nil] ~> map",
            "#['int, 'int] { =[a, b] => [b, a] }",
            "#<'t>'t { $ }",
            "#<'t: (x: 'int)>'t { $x }",
            "#<'k: 'int | 'bin, 'v>['k, 'v] { $0 }",
            "#<'f: #'int -> 'int>'f { $ }",
            "#'int",
            "#'int -> 'bin { $ }",
            // --- multi-chain sequences & control flow ---
            "tag = %ref; [tag, 42] ~> =[^tag, x]; x",
            "[]; 5",
            // --- type aliases: unions, intersections, partials, modules, recursion ---
            "'bool = True | False",
            "'shape = Circle[radius: 'int] | Rectangle[width: 'int, height: 'int]",
            "'rw = 'readable & 'writable",
            "'inter = 'a & 'b | 'c",
            "'list<'t> = Nil | Cons['t, ^]",
            "'v2 = 'v1[..., id: 'bin]",
            "'v3 = 'v1[...'other, id: 'bin]",
            "'post = Post[...'entity, title: 'int, ...'updateable]",
            "'pairs = 'wrap[...'pair<'int>, tag: 'bin]",
            "'tree<'t> = Leaf['t] | Node[^, ^]",
            "'json = Null | 'bool | 'int | Str['bin] | Array[(Nil | Cons[^, ^1])]",
            "'pair<'a, 'b> = Pair[first: 'a, second: 'b]",
            "'keyed<'k: 'int | 'bin, 'v> = ['k, 'v]",
            "'<'t: (x: 'int)> = Nil | Cons['t, ^]",
            "'adder = #'int -> 'int",
            "'writer = (write: (#'bin -> Ok))",
            "'np = Point(x: 'int)",
            "'ep = ()",
            "'enp = Point()",
            "'nil = []",
            "'mt = '%list<'int>",
            "'mn = '%shapes.circle",
            "'recv = @'int",
            "'both = @'int !'bin",
            "'ret = @!'bin",
            "'all = @'int !'bin ?'int",
            "'watch = @?'int",
            "'punion = 'int | @'int ?'int",
            "'pout = #'int -> (@'int !'bin)",
            "'res = +File",
            "'post = Post[...'entity, title: Str['bin], ...'updateable]",
            "' = Str['bin]",
            "'<'t> = Nil | Cons['t, ^]",
            "'selfapp = '<'int>",
            "'fnfield = [f: (#'int -> 'bin) | Nil]",
        ];
        for snippet in corpus {
            assert_idempotent(snippet, snippet);
        }
    }
}
