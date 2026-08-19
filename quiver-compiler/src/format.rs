//! An AST pretty-printer that renders a parsed [`Sequence`] back to canonical Quiver source.
//!
//! The AST is rendered to a [`crate::pretty`] document and laid out against a fixed target
//![`WIDTH`]: a construct stays on one line if it fits, otherwise it breaks using its own
//! reparse-safe separator — chains continue with `~>`, blocks lead each branch with `|`, tuples
//! and multi-chain sequences put one item per line.
//!
//! The formatter changes only whitespace and rendering choices that re-parse to the identical AST
//! (`$0` sugar, parenthesised unions, string literals), plus block *normalization*: it drops
//! redundant blocks (like redundant parentheses, via [`crate::simplify`]) and adds grouping braces
//! around a branch body that would otherwise sprawl — a compound consequence, or a single chain that
//! breaks into a `~>` pipeline (`wrap_breaking_body`). Every block it adds or removes is one the
//! compiler treats as a runtime no-op (it strips/lifts them), so formatting never changes compiled
//! output — it is bytecode-preserving (`compile(parse(format(src))) == compile(parse(src))`).
//!
//! Atomic constructs that never break — types, patterns, accesses, literals — are rendered to
//! plain strings and wrapped as [`pretty::text`]; only the breakable layers build structured docs.

use crate::ast::*;
use crate::pretty::{self, Doc};
use std::collections::HashMap;

/// Target line width: groups that would exceed this many columns are broken.
const WIDTH: usize = 100;

/// A chain whose single-line form exceeds this many columns is split onto `~>` continuation lines
/// even when it would fit within [`WIDTH`] — long pipelines read better broken. (Group B, tunable.)
const CHAIN_SOFT_WIDTH: usize = 50;

/// A multi-branch block whose single-line form exceeds this many columns is split one branch per
/// line; shorter blocks stay inline. (Group B, tunable.)
const SHORT_BLOCK_WIDTH: usize = 40;

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
    name: &Option<String>,
    type_parameters: &[String],
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
            pretty::concat(vec![pretty::text(lhs), union_alias_doc(union_type)])
        }
        other => pretty::text(format!("{} {}", lhs, render_type(other))),
    };
    pretty::concat(vec![body, pretty::break_parent()])
}

/// The right-hand side of a union type alias: `= A | B | C` flat, or each member on its own line
/// led by `|` when broken, indented under the alias name.
fn union_alias_doc(union_type: &UnionType) -> Doc {
    let mut parts = Vec::new();
    for (index, member) in union_type.types.iter().enumerate() {
        parts.push(pretty::line());
        parts.push(leading_bar(index == 0));
        parts.push(pretty::text(render_union_member(member)));
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

/// `<'a, 'b>` for a non-empty parameter list, otherwise empty. Type parameters are stored without
/// their `'` prefix, so it is re-added here.
fn render_type_parameters(params: &[String]) -> String {
    if params.is_empty() {
        return String::new();
    }
    format!(
        "<{}>",
        params
            .iter()
            .map(|p| format!("'{}", p))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// A sequence of `,`-separated chains, one per line when broken, each preceded by any leading
/// comments and blank lines it carries.
///
/// `skip_first_leading` drops the leading trivia of the first chain — used when a caller (a
/// multi-branch block) has already emitted it elsewhere (before the branch's `|`).
///
/// `continuation_nest` indents the *continuation* chains (the 2nd onward) by that many spaces while
/// the first chain stays at the sequence's indent. A branch body uses 2 so its wrapped steps align
/// under the content past the `| `, while a body that is a single chain ending in a block keeps that
/// block at the bar indent — so the block's `}` lines up with the branch's `|`.
fn sequence_doc(
    trivia: &Trivia,
    sequence: &Sequence,
    skip_first_leading: bool,
    continuation_nest: usize,
) -> Doc {
    // Semicolon and newline are synonymous step separators, so a broken sequence uses a bare
    // newline (the lighter form) and only the inline form needs the semicolon.
    let separator = pretty::concat(vec![
        pretty::if_break(pretty::nil(), pretty::text(";")),
        pretty::line(),
    ]);
    let mut first = pretty::nil();
    let mut rest = Vec::new();
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
                let tall = is_tall_step(chain, &body);
                // The assertions ending the step's last line ride after the chain, before any
                // trailing comment; they do not make a step "tall". A trailing assertion is
                // glued to the step's line; an own-line one (a leading `//=>`) starts a fresh
                // line at the step's indent, keeping any comments written above it (it is an
                // anchor — see `visit_chain`). Assertions written above a `~>` continuation
                // sit inside the chain, and `chain_terms_doc` has already placed them on the
                // line whose value they observe.
                let mut parts = vec![body];
                let final_assertions = chain
                    .assertions
                    .iter()
                    .filter(|assertion| assertion.after == chain.terms.len());
                for (index, assertion) in final_assertions.enumerate() {
                    // The first assertion of an assertion-only step opens the step itself,
                    // so the step-level leading trivia above already covers it.
                    let opens_step = index == 0 && chain.terms.is_empty();
                    let own_line = assertion.own_line && !opens_step;
                    if own_line {
                        parts.push(pretty::hardline());
                        parts.push(trivia.leading_doc(assertion.span));
                    }
                    parts.push(assertion_doc(assertion, own_line || opens_step));
                }
                (pretty::concat(parts), tall)
            }
            Step::TypeAlias {
                name,
                type_parameters,
                type_definition,
                ..
            } => (
                type_alias_doc(name, type_parameters, type_definition),
                false,
            ),
        };
        let item = pretty::concat(vec![leading, body, trivia.trailing_doc(span)]);
        if index == 0 {
            first = item;
        } else {
            // Set a tall step off from its neighbours with a blank line (two newlines). `collapse_
            // blanks` caps a run at one, so this composes with any blank the author already left.
            if prev_tall || tall {
                rest.push(pretty::hardline());
                rest.push(pretty::hardline());
            } else {
                rest.push(separator.clone());
            }
            rest.push(item);
        }
        prev_tall = tall;
    }
    pretty::group(pretty::concat(vec![
        first,
        pretty::nest(continuation_nest, pretty::concat(rest)),
    ]))
}

/// Whether a sequence step renders across several lines as an *undelimited* pipeline, so it is set
/// off from its neighbours with a blank line. That happens when the chain actually breaks at this
/// width (`forces_break`) and either has a `~>` call-unit boundary to break at, or is a binding whose
/// multi-term value breaks after the `=`. A step delimited by its own container/block absorbs its
/// own overflow and is not set off; nor is a single-line step that merely carries a comment (whose
/// forced break lives in the trivia, not in `body`).
fn is_tall_step(chain: &Chain, body: &Doc) -> bool {
    let terms = &chain.terms;
    if terms.len() < 2 || terms.last().is_some_and(is_breakable_container) {
        return false;
    }
    let pipeline = terms[..terms.len() - 1].iter().any(is_call_ender);
    (pipeline || chain.binding.is_some()) && pretty::forces_break(body)
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
            let body = sequence_doc(trivia, &branch.condition, multi_branch, nest);
            wrap_breaking_body(&branch.condition, body, multi_branch)
        }
        Some(consequence) => {
            let condition = sequence_doc(trivia, &branch.condition, multi_branch, nest);
            let body = sequence_doc(trivia, consequence, false, nest);
            let body = wrap_breaking_body(consequence, body, multi_branch);
            // A guard is normally flattened onto one line so a long consequence does not push it onto
            // `~>` lines — but not when it carries a comment or is itself a breaking pipeline (which
            // forces a break; flattening would comment out / collapse the rest of the line).
            if pretty::forces_break(&condition) {
                let content = pretty::concat(vec![condition, pretty::text(" => "), body]);
                // A single-chain `~>` guard indents its continuations under the head (past the `| `),
                // *and* the consequence that follows the last one on the same line — so the whole
                // `cond => consequence` is nested together, keeping the consequence aligned with the
                // line it opens on. A multi-chain guard already indents its steps via
                // `continuation_nest`, so it is left as-is.
                if branch.condition.single_chain().is_some() {
                    pretty::nest(2, content)
                } else {
                    content
                }
            } else {
                pretty::concat(vec![
                    pretty::text(pretty::flatten(&condition)),
                    pretty::text(" => "),
                    body,
                ])
            }
        }
    }
}

/// Wrap a branch body under a `| ` bar in grouping braces when it is a single chain that will break
/// across lines as a `~>` pipeline (rather than one ending in a block, which delimits itself): its
/// continuation would otherwise dangle at the bar indent, reading like a new step. The brace block is
/// a frame-free single chain, which both the compiler and the formatter's own strip pass remove —
/// so this render-time wrap is bytecode-neutral and idempotent. `body` is the already-rendered doc.
fn wrap_breaking_body(sequence: &Sequence, body: Doc, multi_branch: bool) -> Doc {
    let breaking_pipeline = sequence.single_chain().is_some_and(|chain| {
        chain
            .terms
            .last()
            .is_some_and(|term| !is_breakable_container(term))
    });
    if multi_branch && breaking_pipeline && pretty::forces_break(&body) {
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

/// A chain: an optional `pattern = ` binding followed by `~>`-joined terms. When the terms do not
/// fit, they break with a leading `~>` per continuation line. A trailing breakable container (a
/// block, tuple, or function) is kept attached to the preceding terms and allowed to break
/// internally, rather than forcing the whole chain onto `~>` lines.
/// Render a step-final `//=> P` assertion: the canonical pattern, plus any prose note three
/// spaces off. An assertion terminates its line like a comment, so every form forces the
/// enclosing construct to break — a closing `}` must never land after one. A trailing
/// assertion with a note is additionally deferred to the line's end via `line_suffix`, like
/// the trailing comment it resembles; an assertion rendered at the start of its line (`bare`)
/// keeps its text in place — the line is its own.
fn assertion_doc(assertion: &Assertion, bare: bool) -> Doc {
    let text = match &assertion.note {
        Some(note) => format!("//=> {}   {}", render_match(&assertion.pattern), note),
        None => format!("//=> {}", render_match(&assertion.pattern)),
    };
    let text = if bare { text } else { format!(" {text}") };
    let doc = match (&assertion.note, bare) {
        (Some(_), false) => pretty::line_suffix(pretty::text(text)),
        _ => pretty::text(text),
    };
    pretty::concat(vec![doc, pretty::break_parent()])
}

fn chain_doc(trivia: &Trivia, chain: &Chain) -> Doc {
    let prefix = match &chain.binding {
        Some(pattern) => pretty::text(format!("{} = ", render_match(pattern))),
        None => pretty::nil(),
    };
    let terms = &chain.terms;
    if terms.len() > 1
        && is_breakable_container(&terms[terms.len() - 1])
        && !has_interior_trivia(trivia, chain)
    {
        // A chain ending in a container (`head { … }`, `head [ … ]`) keeps its head on one line and
        // lets the container break internally. The head is rendered fully flat so neither it nor its
        // inner groups break: a `fits` check on a grouped head would count the (large) trailing
        // container against the head's line and break it spuriously onto `~>` lines.
        let (head, tail) = terms.split_at(terms.len() - 1);
        let head_docs: Vec<Doc> = head.iter().map(|term| term_doc(trivia, term)).collect();
        // …unless a head term carries a comment (it forces a break): flattening it would comment out
        // the rest of the line, so fall back to the ordinary grouped layout, which breaks safely.
        if !head_docs.iter().any(pretty::forces_break) {
            let head_flat = head_docs
                .iter()
                .map(pretty::flatten)
                .collect::<Vec<_>>()
                .join(" ~> ");
            return pretty::concat(vec![
                prefix,
                pretty::text(head_flat),
                pretty::text(" ~> "),
                term_doc(trivia, &tail[0]),
            ]);
        }
    }
    let inner = break_if_wider_than(chain_terms_doc(trivia, chain), CHAIN_SOFT_WIDTH);
    // A binding whose value is a multi-term `~>` pipeline breaks *after* the `=` and indents the
    // pipeline, so the continuations sit under the value rather than dangling at the binding's own
    // indent. A single-term value, or one ending in a self-breaking container (`x = head { … }`),
    // stays on the `=` line.
    if let Some(pattern) = &chain.binding
        && terms.len() > 1
        && !is_breakable_container(&terms[terms.len() - 1])
    {
        return pretty::group(pretty::concat(vec![
            pretty::text(format!("{} =", render_match(pattern))),
            pretty::nest(2, pretty::concat(vec![pretty::line(), inner])),
        ]));
    }
    pretty::concat(vec![prefix, pretty::group(inner)])
}

/// Lay out a chain's terms, joined by an explicit `~>` between every pair of adjacent terms, and
/// breaking only at *call-unit* boundaries: a break point sits after a term that consumes the
/// flowing value (an `[args] ~> callable` unit just completed), rendered as a leading-`~>`
/// continuation line. The separator within a unit is a hard ` ~> ` (never breaks), so an argument
/// tuple stays on the same line as its callable.
///
/// A gap also carries whatever the author wrote at the end of the line it continues — a comment,
/// a `//=> P` assertion — and those run to the end of that line, so the gap must then break
/// whatever the width says, or the `~>` would land inside the comment.
fn chain_terms_doc(trivia: &Trivia, chain: &Chain) -> Doc {
    let terms = &chain.terms;
    let mut parts = Vec::new();
    for (index, term) in terms.iter().enumerate() {
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
            }
            let breaks =
                !assertions.is_empty() || trivia.has_trivia(gap.end) || trivia.has_trivia(gap.pipe);
            if breaks {
                parts.push(pretty::hardline());
                parts.push(trivia.leading_doc(gap.pipe));
                parts.push(pretty::text("~> "));
            } else if is_call_ender(&terms[index - 1]) {
                parts.push(pretty::line());
                parts.push(pretty::text("~> "));
            } else {
                parts.push(pretty::text(" ~> "));
            }
        }
        parts.push(term_doc(trivia, term));
    }
    pretty::concat(parts)
}

/// Whether a chain carries anything *between* its terms — a `//=> P` assertion or a comment —
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

/// Whether a term completes a call unit: the flowing value is consumed here (a callable applied, a
/// field access, or an operator), so a breaking chain breaks *after* it.
fn is_call_ender(term: &Term) -> bool {
    matches!(term, Term::Access(_) | Term::Self_)
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
        Term::Self_ => ".".to_string(),
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
    // `[a: &a, x: &p.x]` the parser desugared it to. Entries carry their own trivia, so comments
    // and blank lines inside the parens survive as they do in any field list.
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
/// parser's desugaring, whose shape (`name: &path`) is what makes the match exhaustive.
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
fn bracketed(open: String, close: &str, items: Vec<Doc>, trailing: bool) -> Doc {
    let separator = pretty::concat(vec![pretty::text(","), pretty::line()]);
    let trailing = if trailing {
        pretty::if_break(pretty::text(","), pretty::nil())
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
    ]))
}

fn function_doc(trivia: &Trivia, function: &Function) -> Doc {
    let mut signature = String::from("#");
    signature.push_str(&render_type_parameters(&function.type_parameters));
    if let Some(parameter_type) = &function.parameter_type {
        // The parameter type sits in a `function_input_type` position, which does not accept a bare
        // union/intersection/function — wrap those in parentheses.
        signature.push_str(&render_type_atom(parameter_type));
    }
    if let Some(return_type) = &function.return_type {
        signature.push_str(" -> ");
        signature.push_str(&render_type_atom(return_type));
    }
    match &function.body {
        None => pretty::text(signature),
        // A bare `#` (nilary, no signature) abuts its block as `#{ … }`; a typed head takes a space.
        Some(body) if signature == "#" => {
            pretty::concat(vec![pretty::text(signature), block_doc(trivia, body)])
        }
        Some(body) => pretty::concat(vec![
            pretty::text(signature),
            pretty::text(" "),
            block_doc(trivia, body),
        ]),
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

/// Render a spawn (`@f`, `@~`, `@{ … }`, `@'int { … }`). An inline spawned function must use the
/// `@`-sugar forms — the parser does not accept `@#…` — so a function head is emitted as `@{ body }`,
/// the tight `@'type { body }` where the type allows it, or `@(type) { body }` (the parenthesised
/// arm accepts any type).
fn spawn_doc(trivia: &Trivia, func: &Term, argument: Option<&Term>) -> Doc {
    let head = match func {
        Term::Function(function) => {
            let head = match &function.parameter_type {
                None => "@".to_string(),
                Some(parameter_type) => {
                    // The spawn grammar also takes the unnamed tuple form (`@['int, 'int] { … }`
                    // — there is no `@[sources]` to collide with). A partial has NO bare spawn
                    // form: its parens read as the grouping arm, whose content must be a full
                    // type, so it double-wraps (`@((x: 'int)) { … }`).
                    let bare = sugar_type(parameter_type).or_else(|| match parameter_type {
                        Type::Tuple(tuple_type) if bare_tuple(tuple_type) => {
                            Some(render_type(parameter_type))
                        }
                        _ => None,
                    });
                    match bare {
                        Some(sugar) => format!("@{} ", sugar),
                        None => format!("@({}) ", render_type(parameter_type)),
                    }
                }
            };
            match &function.body {
                None => pretty::text(head),
                Some(body) => pretty::concat(vec![pretty::text(head), block_doc(trivia, body)]),
            }
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
        None => format!("!#{}", render_type_atom(parameter_type)),
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
        // `!f`, `!p`, `!%mod.recv` — a named source.
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
#[derive(Default)]
struct Trivia {
    leading: HashMap<usize, Vec<TriviaItem>>,
    trailing: HashMap<usize, Vec<String>>,
    dangling: Vec<TriviaItem>,
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
                // A `//=>` assertion is AST, not trivia: consume it like a comment so its
                // pattern text isn't scanned, but record nothing — the formatter re-emits it
                // from the `Chain.assertion` node.
                let is_assertion = source[index..].starts_with("//=>");
                let mut end = source.len();
                while let Some(&(j, next)) = chars.peek() {
                    if next == '\n' {
                        end = j;
                        break;
                    }
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
        if let Step::Chain(chain) = step {
            visit_chain(chain, out);
        }
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
    // An own-line assertion is its own anchor, so comments written above it keep their place.
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
        Term::Function(function) => {
            if let Some(body) = &function.body {
                visit_block(body, out);
            }
        }
        Term::Spawn(inner, argument, _) => {
            visit_term(inner, out);
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
        Some(AccessSource::Self_) => out.push('.'),
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
                if let Some(ast_type) = expected {
                    // The checked form's shape is parenthesised. Unions and unnamed
                    // partials render fully wrapped in their own parens, so don't double
                    // up — but decide by variant, not by rendered prefix: an intersection
                    // like `(a: 't) & (b: 't)` *starts* with `(` without being enclosed.
                    let self_parenthesised = matches!(ast_type, Type::Union(_))
                        || matches!(ast_type, Type::Tuple(tuple) if tuple.is_partial && tuple.name.is_none());
                    let rendered = render_type(ast_type);
                    if self_parenthesised {
                        out.push_str(&rendered);
                    } else {
                        out.push('(');
                        out.push_str(&rendered);
                        out.push(')');
                    }
                }
                out.push_str(name);
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

pub(crate) fn render_match(pattern: &Match) -> String {
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
        Match::Tuple(tuple) => render_match_tuple(tuple),
        Match::Partial(partial) => render_partial_pattern(partial),
        Match::Star(None) => "*".to_string(),
        Match::Star(Some(name)) => format!("{}*", name),
        Match::Placeholder => "_".to_string(),
        Match::Pin(target) => {
            let mut out = String::from("&");
            // As in `render_access`, a first accessor on `$` is written dotless (`&$x`, `&$0`).
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
        Match::Type(type_def) => render_match_type(type_def),
        Match::Or(alternatives) => format!(
            "({})",
            alternatives
                .iter()
                .map(render_match)
                .collect::<Vec<_>>()
                .join(" | ")
        ),
        // A type-ascribed binding always parenthesises its type: the parser requires `('(' type ')'`
        // immediately followed by the binder. A self-parenthesised rendering — a union or an
        // unnamed partial — already provides that pair (`('int | 'bin)v`, `(x: 'int)p`); a named
        // partial's parens don't lead, so it still takes the explicit pair (`(Point(x))p`).
        Match::As(type_def, name, _) => {
            let self_parenthesised = matches!(type_def, Type::Union(_))
                || matches!(type_def, Type::Tuple(t) if t.is_partial && t.name.is_none());
            if self_parenthesised {
                format!("{}{}", render_type(type_def), name)
            } else {
                format!("({}){}", render_type(type_def), name)
            }
        }
    }
}

fn render_match_tuple(tuple: &MatchTuple) -> String {
    let fields = tuple
        .fields
        .iter()
        .map(render_match_field)
        .collect::<Vec<_>>()
        .join(", ");
    match &tuple.name {
        Some(name) if tuple.fields.is_empty() => name.clone(),
        Some(name) => format!("{}[{}]", name, fields),
        None => format!("[{}]", fields),
    }
}

fn render_match_field(field: &MatchField) -> String {
    match &field.name {
        Some(name) => format!("{}: {}", name, render_match(&field.pattern)),
        None => render_match(&field.pattern),
    }
}

fn render_partial_pattern(partial: &PartialPattern) -> String {
    let fields = partial
        .fields
        .iter()
        .map(|field| match &field.pattern {
            None => field.name.clone(),
            Some(pattern) => format!("{}: {}", field.name, render_match(pattern)),
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("{}({})", partial.name.clone().unwrap_or_default(), fields)
}

/// Render a type used as a pattern. A pattern's type position only accepts the bare forms recognised
/// by `inline_type_expression` (a type name/module/self-default reference or a partial type);
/// anything else (unions, intersections, functions, non-partial tuples, cycles, …) must be wrapped
/// in parentheses so it re-parses as a `Match::Type` rather than, say, a structural tuple pattern.
fn render_match_type(type_def: &Type) -> String {
    let bare = match type_def {
        Type::Primitive(_)
        | Type::Identifier { .. }
        | Type::ModuleType { .. }
        | Type::SelfDefault { .. } => true,
        Type::Tuple(tuple_type) => tuple_type.is_partial,
        // `render_type` already parenthesises a union, so it needs no extra wrapping here.
        Type::Union(_) => true,
        _ => false,
    };
    if bare {
        render_type(type_def)
    } else {
        format!("({})", render_type(type_def))
    }
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

fn render_type(type_def: &Type) -> String {
    match type_def {
        Type::Primitive(PrimitiveType::Int) => "'int".to_string(),
        Type::Primitive(PrimitiveType::Bin) => "'bin".to_string(),
        Type::Primitive(PrimitiveType::Ref) => "'ref".to_string(),
        Type::Identifier { name, arguments } => {
            format!("'{}{}", name, render_type_arguments(arguments))
        }
        Type::Tuple(tuple_type) => render_tuple_type(tuple_type),
        Type::Function(function_type) => {
            let mut out = format!(
                "#{} -> {}",
                render_type_atom(&function_type.input),
                render_type_atom(&function_type.output)
            );
            if let Some(receive) = &function_type.receive {
                out.push_str(" !");
                out.push_str(&render_type_atom(receive));
            }
            if let Some(states) = &function_type.states {
                out.push_str(" ?");
                out.push_str(&render_type_atom(states));
            }
            out
        }
        // A union is parenthesised everywhere it is rendered inline; only a top-level type-alias
        // right-hand side (handled by `union_alias_doc`) is left bare.
        Type::Union(union_type) => format!(
            "({})",
            union_type
                .types
                .iter()
                .map(render_union_member)
                .collect::<Vec<_>>()
                .join(" | ")
        ),
        Type::Intersection(types) => types
            .iter()
            .map(render_type_atom)
            .collect::<Vec<_>>()
            .join(" & "),
        Type::Cycle(None) => "^".to_string(),
        Type::Cycle(Some(level)) => format!("^{}", level),
        Type::Process(process_type) => render_process_type(process_type),
        Type::Resource(name) => format!("\\{}", name),
        Type::ModuleType {
            module,
            member,
            arguments,
        } => {
            let mut out = format!("'%{}", module.join("/"));
            if let Some(member) = member {
                out.push('.');
                out.push_str(member);
            }
            out.push_str(&render_type_arguments(arguments));
            out
        }
        Type::SelfDefault { arguments } => format!("'{}", render_type_arguments(arguments)),
    }
}

/// Render a type where the grammar expects a `base_type`/atom (intersection members, process
/// heads and clauses, function input/output): wrap an intersection or function in parentheses,
/// and a clause-bearing process type — nested, its clauses would bind to the wrong head (and a
/// function output's trailing clauses are the function's). A union is already parenthesised by
/// `render_type`.
fn render_type_atom(type_def: &Type) -> String {
    match type_def {
        Type::Intersection(_) | Type::Function(_) => {
            format!("({})", render_type(type_def))
        }
        Type::Process(process_type)
            if process_type.return_type.is_some() || process_type.state_type.is_some() =>
        {
            format!("({})", render_type(type_def))
        }
        _ => render_type(type_def),
    }
}

/// A union member is an intersection-level type, so it never needs wrapping except for a function
/// type (which only appears as a member when originally parenthesised).
fn render_union_member(type_def: &Type) -> String {
    match type_def {
        Type::Function(_) => format!("({})", render_type(type_def)),
        _ => render_type(type_def),
    }
}

fn render_type_arguments(arguments: &[Type]) -> String {
    if arguments.is_empty() {
        return String::new();
    }
    format!(
        "<{}>",
        arguments
            .iter()
            .map(render_type)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn render_tuple_type(tuple_type: &TupleType) -> String {
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
    if tuple_type.fields.is_empty() {
        return if tuple_type.is_partial {
            format!("{}()", name)
        } else if tuple_type.name.is_some() {
            name
        } else {
            "[]".to_string()
        };
    }
    let fields = tuple_type
        .fields
        .iter()
        .map(|field| match (alias, field) {
            (
                Some(alias),
                FieldType::Spread {
                    identifier: Some(identifier),
                    type_arguments,
                },
            ) if identifier == alias && type_arguments.is_empty() => "...".to_string(),
            _ => render_field_type(field),
        })
        .collect::<Vec<_>>()
        .join(", ");
    let (open, close) = if tuple_type.is_partial {
        ("(", ")")
    } else {
        ("[", "]")
    };
    format!("{}{}{}{}", name, open, fields, close)
}

/// A field's ` = <value>` default. Types render flat, and a default is a short value
/// chain, so it flattens with them.
fn render_field_default(default: &Option<Box<Chain>>) -> String {
    match default {
        Some(chain) => format!(
            " = {}",
            crate::pretty::flatten(&chain_doc(&Trivia::default(), chain))
        ),
        None => String::new(),
    }
}

fn render_field_type(field_type: &FieldType) -> String {
    match field_type {
        FieldType::Field {
            name: Some(name),
            omittable,
            type_def,
            default,
        } => {
            let label = if *omittable {
                format!("({})", name)
            } else {
                name.clone()
            };
            format!(
                "{}: {}{}",
                label,
                render_type(type_def),
                render_field_default(default)
            )
        }
        FieldType::Field {
            name: None,
            type_def,
            default,
            ..
        } => format!("{}{}", render_type(type_def), render_field_default(default)),
        FieldType::Spread {
            identifier: None, ..
        } => "...".to_string(),
        FieldType::Spread {
            identifier: Some(identifier),
            type_arguments,
        } => format!(
            "...'{}{}",
            identifier,
            render_type_arguments(type_arguments)
        ),
    }
}

fn render_process_type(process_type: &ProcessType) -> String {
    let mut body = "@".to_string();
    if let Some(receive) = &process_type.receive_type {
        body.push_str(&render_type_atom(receive));
    }
    // Clause sigils glue to a bare `@` and take a space after a head or earlier clause.
    if let Some(ret) = &process_type.return_type {
        if process_type.receive_type.is_some() {
            body.push(' ');
        }
        body.push('!');
        body.push_str(&render_type_atom(ret));
    }
    if let Some(state) = &process_type.state_type {
        if process_type.receive_type.is_some() || process_type.return_type.is_some() {
            body.push(' ');
        }
        body.push('?');
        body.push_str(&render_type_atom(state));
    }
    body
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use std::path::PathBuf;

    /// Reduce a program to the compiler's canonical, block-free form: every no-op block stripped or
    /// lifted (`Compiler::compile` does the same before codegen). Two programs equal here compile
    /// identically.
    fn canonical(program: Sequence) -> Sequence {
        crate::simplify::normalize_blocks(
            program,
            &crate::simplify::Options {
                keep: &|_| false,
                lift: true,
                group_consequences: false,
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
    fn assertion_round_trips_and_canonicalizes() {
        // Marker spacing and the pattern normalize; the value side formats as usual.
        assert_formats("5  //=>  Ok\n", "5 //=> Ok\n");
        assert_formats("x = 5 //=> 5", "x = 5 //=> 5\n");
        assert_formats(
            "Point[1,2] //=> Point[1, 2]",
            "Point[1, 2] //=> Point[1, 2]\n",
        );
    }

    #[test]
    fn own_line_assertions_keep_their_lines() {
        // A leading `//=>` continues the step above; the formatter keeps it on its own line,
        // at the step's indent, with any comments and blanks between held in place.
        assert_idempotent("1 ~> f\n//=> 2\n", "own-line assertion");
        assert_idempotent("5 //=> 'int\n//=> 5   note\n", "trailing then own-line");
        assert_idempotent("5 ~> f\n\n// why\n//=> 10\n", "trivia above an assertion");
        assert_idempotent(
            "x = {\n  5 ~> f\n  //=> 10\n}\nx\n",
            "own-line assertion in a block",
        );
        // A `;`-separated trailing assertion is the trailing form: the separator drops.
        assert_formats("5; //=> 5\n", "5 //=> 5\n");
    }

    #[test]
    fn assertion_only_steps_render_bare() {
        // An opening assertion is a step with no chain: nothing precedes the marker. The
        // assertion terminates its line, so the enclosing block never flattens around it.
        assert_idempotent(
            "f = #'int {\n  //=> 5\n  $ ~> g\n}\nf 5\n",
            "opening assertion",
        );
        assert_formats(
            "f = #'int {\n  //=> 5   the note\n  $\n}\n",
            "f = #'int {\n  //=> 5   the note\n  $\n}\n",
        );
    }

    #[test]
    fn assertion_note_and_trailing_comment_survive() {
        // The note is preserved three spaces off.
        assert_idempotent("x = 5 //=> 5\nx //=> 5   the note\n", "assertion note");
        assert_idempotent(
            "f = #'int {\n  $ //=> 'int\n}\nf 3 //=> 3\n",
            "assertion in body",
        );
        // A redundant block whose body asserts is kept, not spliced.
        assert_idempotent("{\n  5 //=> 6\n}\n", "assertion keeps its block");
    }

    #[test]
    fn chain_lines_keep_their_assertions_and_comments() {
        // Each line of a spread chain may end in an assertion or a comment, and both pin the
        // break: the `~>` must not be pulled up into what runs to the end of a line.
        assert_idempotent(
            "1 //=> 1\n~> %num.add [~, 2] //=> 3\n~> %num.mul [~, 3] //=> 9\n",
            "assertion per chain line",
        );
        assert_idempotent(
            "1 // the head\n// above the continuation\n~> double\n",
            "comments in a continuation gap",
        );
        assert_idempotent(
            "1\n//=> 'int\n//=> 1\n~> double\n",
            "stacked own-line assertions mid-chain",
        );
        // The head of a chain ending in a container is normally flattened onto one line; an
        // assertion in the gap rules that out.
        assert_formats(
            "1 //=> 1\n~> %list.map [~, double]\n",
            "1 //=> 1\n~> %list.map [~, double]\n",
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
            "bit = { [a, b] ~> f ~> [1, ~] ~> g }",
            "bit = [a, b] ~> f ~> [1, ~] ~> g\n",
        );
        // Nested redundant blocks collapse fully.
        assert_formats("x = { { 5 } }", "x = 5\n");
        // A block in a consequence position is also unwrapped.
        assert_formats(
            "f = #'int { =0 => { [~, 1] ~> g } | h }",
            "f = #'int { =0 => [~, 1] ~> g | h }\n",
        );
    }

    #[test]
    fn consequence_block_aligns_with_its_bar_line() {
        // A consequence block's `}` aligns with the bar (`|`) of the line carrying its `{`, indented
        // from the bar rather than from the content past the `| `.
        assert_formats(
            "f = #'t { =B[h, t] => { idx ~> =0 => h | [idx, 1] ~> %num.sub ~> [t, ~] ~> ^ } | other_branch }",
            "f = #'t {\n  | =B[h, t] => {\n    | idx ~> =0 => h\n    | [idx, 1] ~> %num.sub ~> [t, ~] ~> ^\n  }\n  | other_branch\n}\n",
        );
        // The same when the block ends a branch's *condition* chain (`| lst { … }`): the body is one
        // chain, so the block stays at the bar indent and its `}` aligns with the `|`.
        assert_formats(
            "f = #'t { lst ~> { =Nil => empty_result | =Cons[h, t] => [h, t] ~> process } | fallback }",
            "f = #'t {\n  | lst ~> {\n    | =Nil => empty_result\n    | =Cons[h, t] => [h, t] ~> process\n  }\n  | fallback\n}\n",
        );
    }

    #[test]
    fn tall_steps_get_surrounding_blank_lines() {
        // A `~>` pipeline step is set off from its short neighbours with a blank line on each side…
        assert_formats(
            "#{ first_step; target_len ~> [~, suffix_len] ~> %num.sub ~> [target, ~, target_len] ~> %bin.slice ~> =&suffix; last_step }",
            "#{\n  first_step\n\n  target_len\n  ~> [~, suffix_len] ~> %num.sub\n  ~> [target, ~, target_len] ~> %bin.slice\n  ~> =&suffix\n\n  last_step\n}\n",
        );
        // …but a body of only short steps stays packed (no imposed blanks).
        assert_formats("#{ aa; bb; cc }", "#{ aa; bb; cc }\n");
    }

    #[test]
    fn binding_pipeline_breaks_after_equals() {
        // A binding whose value breaks into a `~>` pipeline breaks after the `=` and indents the
        // pipeline, rather than leaving the head on the `=` line and dangling the continuations.
        assert_formats(
            "#{ char_end = byte_pos ~> [~, 1] ~> %num.add ~> [data, ~, data_len] ~> skip_continuation }",
            "#{\n  char_end =\n    byte_pos\n    ~> [~, 1] ~> %num.add\n    ~> [data, ~, data_len] ~> skip_continuation\n}\n",
        );
        // A short binding stays inline…
        assert_formats(
            "#{ x = byte_pos ~> [~, 1] ~> %num.add }",
            "#{ x = byte_pos ~> [~, 1] ~> %num.add }\n",
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
            "#{ x ~> { haystack_len ~> [~, needle_len] ~> %num.sub ~> [haystack, needle, 0, ~] ~> find_index => Ok | other } }",
            "#{\n  x ~> {\n    | haystack_len\n      ~> [~, needle_len] ~> %num.sub\n      ~> [haystack, needle, 0, ~] ~> find_index => Ok\n    | other\n  }\n}\n",
        );
        // A consequence that follows the last continuation on the same line lays out relative to
        // that deeper indent too — a breaking tuple's fields indent under its `[`, `]` aligned.
        assert_formats(
            "#{ x ~> { [haystack, delim, start, end] ~> find_index ~> =('int)index => [haystack ~> [~, start, index] ~> %bin.slice ~> Str[~], [index, delim_len] ~> %num.add, another_field, one_more_field] | other } }",
            "#{\n  x ~> {\n    | [haystack, delim, start, end] ~> find_index\n      ~> =('int)index => [\n        haystack ~> [~, start, index] ~> %bin.slice ~> Str[~],\n        [index, delim_len] ~> %num.add,\n        another_field,\n        one_more_field,\n      ]\n    | other\n  }\n}\n",
        );
    }

    #[test]
    fn wraps_breaking_consequence_pipeline() {
        // A consequence that is a single chain breaking into a `~>` pipeline is wrapped in grouping
        // braces, so the continuation reads as a delimited body instead of dangling at the bar.
        assert_formats(
            "f = #'t { =Cons[k, t] => dict ~> put_entry ~ ~> normalise ~ ~> rebalance ~ ~> recount ~ ~> settle ~ ~> finish ~ | =Nil => dict }",
            "f = #'t {\n  | =Cons[k, t] => {\n    dict\n    ~> put_entry ~ ~> normalise ~ ~> rebalance ~ ~> recount ~ ~> settle ~ ~> finish ~\n  }\n  | =Nil => dict\n}\n",
        );
        // A short consequence stays bare (it does not break).
        assert_formats(
            "f = #'t { =A => a ~> b | =B => c }",
            "f = #'t { =A => a ~> b | =B => c }\n",
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
            "x = 5 ~> { =0 => a ~> b | c }",
            "x = 5 ~> { =0 => a ~> b | c }\n",
        );
        // …a binding consequence keeps its own markers and stays bare…
        assert_formats(
            "x = 5 ~> { =0 => y = 1; [y, 2] ~> g | c }",
            "x = 5 ~> { =0 => y = 1; [y, 2] ~> g | c }\n",
        );
        // …and an already-braced consequence is not double-wrapped.
        assert_formats(
            "x = 5 ~> { =0 => { a; b } | c }",
            "x = 5 ~> { =0 => { a; b } | c }\n",
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
    fn keeps_narrowing_barrier_and_unsafe_tail_blocks() {
        // A block wrapping a match can be a deliberate narrowing barrier — never strip it.
        assert_formats(
            "x = 5 ~> { { =A } => 1 | 2 }",
            "x = 5 ~> { { =A } => 1 | 2 }\n",
        );
        // A tail call that is not the chain's last term keeps its block (no mid-chain dead code)…
        assert_formats("x = { [a] ~> ^ } ~> f", "x = { [a] ~> ^ } ~> f\n");
        // …a non-final tail call inside the body keeps the block too…
        assert_formats("x = 5 ~> { ^ ~> foo }", "x = 5 ~> { ^ ~> foo }\n");
        // …but a block whose tail call ends up last after splicing is dropped cleanly.
        assert_formats("x = y ~> { [a] ~> ^ }", "x = y ~> [a] ~> ^\n");
    }

    #[test]
    fn keeps_meaningful_blocks() {
        // A binding inside the block would leak if inlined, so the block stays.
        assert_formats("x = { 5 ~> =y; y }", "x = { 5 ~> =y; y }\n");
        // Multiple branches are not redundant.
        assert_formats("x = 5 ~> { =0 => a | b }", "x = 5 ~> { =0 => a | b }\n");
        // A comment inside the block keeps it (so the comment is not lost).
        assert_formats(
            "x = {\n  // note\n  5 ~> f\n}",
            "x = {\n  // note\n  5 ~> f\n}\n",
        );
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

    #[test]
    fn idempotent_over_std() {
        let dir = std_dir();
        let mut files: Vec<_> = std::fs::read_dir(&dir)
            .unwrap_or_else(|e| panic!("read std dir {dir:?}: {e:?}"))
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().is_some_and(|ext| ext == "qv"))
            .collect();
        files.sort();
        assert!(!files.is_empty(), "expected std/*.qv files in {dir:?}");
        for path in files {
            let source = std::fs::read_to_string(&path).unwrap();
            let label = path.file_name().unwrap().to_string_lossy().to_string();
            assert_idempotent(&source, &label);
        }
    }

    #[test]
    fn idempotent_over_corpus() {
        // A snippet corpus exercising every AST variant the printer must handle. Each must parse
        // and reach a print fixpoint.
        let corpus: &[&str] = &[
            // --- step-final assertions ---
            "5 //=> 5",
            "x = f y //=> Ok   the note runs to end of line",
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
            "@{ 5 } []",
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
            "!Reply['ref, 'bin] { =Reply[&id, _]; Ok }",
            "!#['int, 'int]",
            "@'%proc.changed { $ }",
            "@Done { $ }",
            "@Reply['int] { $ }",
            "@['int, 'int] { $ }",
            "@[] { $ }",
            "@((x: 'int)) { $ }",
            "![p, 1000]",
            "!p",
            "![]",
            "f",
            ".",
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
            "42 ~> .",
            // --- match forms ---
            "=Point[x, y]",
            "=(x: 'int)",
            "=Config*",
            "=*",
            "=_",
            "=&y",
            "=('int)n",
            "=('int | 'bin)v",
            "=(x: 'int)p",
            "=(Point(x: 'int))p",
            "('bin)ip = f x; ip",
            "=([a] | [b])",
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
            "#'int",
            "#'int -> 'bin { $ }",
            // --- multi-chain sequences & control flow ---
            "tag = %ref; [tag, 42] ~> =[&tag, x]; x",
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
            "'res = \\File",
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
