//! An AST pretty-printer that renders a parsed [`Program`] back to canonical Quiver source.
//!
//! The AST is rendered to a [`crate::pretty`] document and laid out against a fixed target
//! [`WIDTH`]: a construct stays on one line if it fits, otherwise it breaks using its own
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
pub fn format_program(program: &Program, source: &str) -> String {
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
    let mut docs: Vec<Doc> = program
        .statements
        .iter()
        .map(|statement| statement_doc(&trivia, statement))
        .collect();
    // Comments after the last statement have nowhere to attach, so emit them at the end.
    if !trivia.dangling.is_empty() {
        docs.push(trivia_doc(&trivia.dangling));
    }
    let doc = pretty::join(pretty::hardline(), docs);
    collapse_blanks(&pretty::print(&doc, WIDTH))
}

fn statement_doc(trivia: &Trivia, statement: &Statement) -> Doc {
    match statement {
        Statement::TypeAlias {
            name,
            name_span,
            type_parameters,
            type_definition,
        } => {
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
            pretty::concat(vec![
                trivia.leading_doc(*name_span),
                body,
                trivia.trailing_doc(*name_span),
            ])
        }
        Statement::Expression(sequence) => sequence_doc(trivia, sequence, false, 0),
    }
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
    // Comma and newline are synonymous step separators, so a broken sequence uses a bare newline
    // (the lighter form) and only the inline form needs the comma.
    let separator = pretty::concat(vec![
        pretty::if_break(pretty::nil(), pretty::text(",")),
        pretty::line(),
    ]);
    let mut first = pretty::nil();
    let mut rest = Vec::new();
    let mut prev_tall = false;
    for (index, chain) in sequence.chains.iter().enumerate() {
        let leading = if index == 0 && skip_first_leading {
            pretty::nil()
        } else {
            trivia.leading_doc(chain.span)
        };
        let body = chain_doc(trivia, chain);
        let tall = is_tall_step(chain, &body);
        let item = pretty::concat(vec![leading, body, trivia.trailing_doc(chain.span)]);
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
    (pipeline || chain.match_pattern.is_some()) && pretty::forces_break(body)
}

/// A braced expression `{ … }`. A single branch lays its body out directly; multiple branches each
/// get a leading `|` when broken. The body indents two spaces from the line carrying the `{`.
fn block_doc(trivia: &Trivia, expression: &Expression) -> Doc {
    let branches = &expression.branches;
    let inner = if branches.len() == 1 {
        pretty::concat(vec![
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
                .chains
                .first()
                .map_or_else(pretty::nil, |chain| trivia.leading_doc(chain.span));
            parts.push(pretty::line());
            parts.push(leading);
            parts.push(leading_bar(index == 0));
            // `branch_doc` does its own indenting: it aligns a guard's wrapped lines under the
            // content (past the `| `), but lets a consequence block's `}` align with the bar line.
            parts.push(branch_doc(trivia, branch, true));
        }
        // Lean a multi-branch block toward one-branch-per-line unless it is very short.
        break_if_wider_than(pretty::concat(parts), SHORT_BLOCK_WIDTH)
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
                if branch.condition.chains.len() == 1 {
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
    let breaking_pipeline = matches!(sequence.chains.as_slice(), [chain]
        if chain.terms.last().is_some_and(|term| !is_breakable_container(term)));
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

/// A chain: an optional `pattern = ` binding followed by space-joined terms. When the terms do not
/// fit, they break with a leading `~>` per continuation line. A trailing breakable container (a
/// block, tuple, or function) is kept attached to the preceding terms and allowed to break
/// internally, rather than forcing the whole chain onto `~>` lines.
fn chain_doc(trivia: &Trivia, chain: &Chain) -> Doc {
    let prefix = match &chain.match_pattern {
        Some(pattern) => pretty::text(format!("{} = ", render_match(pattern))),
        None => pretty::nil(),
    };
    let terms = &chain.terms;
    if terms.len() > 1 && is_breakable_container(&terms[terms.len() - 1]) {
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
                .join(" ");
            return pretty::concat(vec![
                prefix,
                pretty::text(head_flat),
                pretty::text(" "),
                term_doc(trivia, &tail[0]),
            ]);
        }
    }
    let inner = break_if_wider_than(chain_terms_doc(trivia, terms), CHAIN_SOFT_WIDTH);
    // A binding whose value is a multi-term `~>` pipeline breaks *after* the `=` and indents the
    // pipeline, so the continuations sit under the value rather than dangling at the binding's own
    // indent. A single-term value, or one ending in a self-breaking container (`x = head { … }`),
    // stays on the `=` line.
    if let Some(pattern) = &chain.match_pattern
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

/// Lay out a chain's terms, breaking only at *call-unit* boundaries: a break point sits after a term
/// that consumes the flowing value (an `[args] callable` unit just completed), rendered as a `~>`
/// continuation. Terms within a unit stay space-joined, so an argument tuple stays on the same line
/// as its callable.
fn chain_terms_doc(trivia: &Trivia, terms: &[Term]) -> Doc {
    let mut parts = Vec::new();
    for (index, term) in terms.iter().enumerate() {
        if index > 0 {
            if is_call_ender(&terms[index - 1]) {
                parts.push(pretty::line());
                parts.push(pretty::if_break(pretty::text("~> "), pretty::nil()));
            } else {
                parts.push(pretty::text(" "));
            }
        }
        parts.push(term_doc(trivia, term));
    }
    pretty::concat(parts)
}

/// Whether a term completes a call unit: the flowing value is consumed here (a callable applied, a
/// field access, or an operator), so a breaking chain breaks *after* it.
fn is_call_ender(term: &Term) -> bool {
    matches!(
        term,
        Term::Access(_) | Term::Equality | Term::Not | Term::Self_
    )
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
        Term::Spawn(inner, _) => matches!(inner.as_ref(), Term::Function(_)),
        _ => false,
    }
}

/// Render a single chain term. Breakable containers build structured docs; atomic terms render to a
/// string wrapped as text.
fn term_doc(trivia: &Trivia, term: &Term) -> Doc {
    match term {
        Term::Tuple(tuple) => tuple_doc(trivia, tuple),
        Term::Block(expression) => block_doc(trivia, expression),
        Term::Function(function) => function_doc(trivia, function),
        Term::Spawn(inner, _) => spawn_doc(trivia, inner),
        Term::Select(sources, _) => select_doc(trivia, sources),
        atom => pretty::text(render_term_atom(atom)),
    }
}

/// Render an atomic (never-breaking) term to a string. Container terms are handled by [`term_doc`]
/// and never reach here.
fn render_term_atom(term: &Term) -> String {
    match term {
        Term::Literal(literal) => render_literal(literal),
        Term::Match(pattern) => format!("={}", render_match(pattern)),
        Term::Access(access) => render_access(access),
        Term::Equality => "==".to_string(),
        Term::Not => "<>".to_string(),
        Term::Self_ => ".".to_string(),
        Term::Process(index) => format!("@{}", index),
        Term::Reference(access) => format!("&{}", render_access(access)),
        Term::Tuple(_)
        | Term::Block(_)
        | Term::Function(_)
        | Term::Spawn(..)
        | Term::Select(..) => unreachable!("container terms are rendered by term_doc"),
    }
}

fn tuple_doc(trivia: &Trivia, tuple: &Tuple) -> Doc {
    // A `Str[<bytes>]` tuple is the desugared form of a string literal; render it back as `"…"`.
    if let Some(doc) = string_doc(tuple) {
        return doc;
    }
    let name = match &tuple.name {
        TupleName::Anonymous => String::new(),
        TupleName::Named(name) if tuple.fields.is_empty() => return pretty::text(name.clone()),
        TupleName::Named(name) => name.clone(),
        // An inherited spread-update always re-parses via the `~`-headed form.
        TupleName::Inherit => "~".to_string(),
    };
    if tuple.fields.is_empty() {
        return pretty::text(format!("{}[]", name));
    }
    // Tuple field lists accept a trailing comma, so add one when broken.
    bracketed(
        format!("{}[", name),
        "]",
        tuple
            .fields
            .iter()
            .map(|field| field_doc(trivia, field))
            .collect(),
        true,
    )
}

/// If `tuple` is a string literal in desugared form — `Str` wrapping a single unnamed binary whose
/// bytes are valid UTF-8 — render it back as a quoted string: the single-line `"…"` form, or the
/// triple-quoted multi-line form when the content spans lines. Returns `None` (keeping the literal
/// `Str[0x…]` form) only when the bytes can't be a string — non-UTF-8, or a control character with
/// no escape (any control other than `\n`, `\r`, `\t`).
fn string_doc(tuple: &Tuple) -> Option<Doc> {
    let TupleName::Named(name) = &tuple.name else {
        return None;
    };
    if name != "Str" {
        return None;
    }
    let [field] = tuple.fields.as_slice() else {
        return None;
    };
    if field.name.is_some() {
        return None;
    }
    let FieldValue::Chain(chain) = &field.value else {
        return None;
    };
    let [Term::Literal(Literal::Binary(bytes))] = chain.terms.as_slice() else {
        return None;
    };
    let text = std::str::from_utf8(bytes).ok()?;
    if text
        .chars()
        .any(|c| (c as u32) < 0x20 && !matches!(c, '\n' | '\r' | '\t'))
    {
        return None;
    }
    // A newline reads best as a multi-line literal; everything else stays on one line.
    if text.contains('\n') {
        Some(multiline_string_doc(text))
    } else {
        Some(pretty::text(single_line_string_source(text)))
    }
}

/// Render `text` (free of newlines) as a single-line `"…"` literal, escaping the characters the
/// lexer decodes.
fn single_line_string_source(text: &str) -> String {
    let mut out = String::from("\"");
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Render `text` as a triple-quoted multi-line literal. Each line is emitted as a `hardline` so the
/// renderer indents it to the ambient column — that indentation becomes the closing delimiter's
/// *margin*, which the parser strips back off, so the value round-trips at any nesting depth.
fn multiline_string_doc(text: &str) -> Doc {
    let mut docs = vec![pretty::text("\"\"\"")];
    for line in text.split('\n') {
        docs.push(pretty::hardline());
        docs.push(pretty::text(encode_multiline_line(line)));
    }
    docs.push(pretty::hardline());
    docs.push(pretty::text("\"\"\""));
    pretty::concat(docs)
}

/// Encode one content line (free of newlines) of a multi-line string. Tabs and carriage returns are
/// escaped (a literal `\r` would be normalised to `\n`, a trailing tab would be stripped), every `"`
/// is escaped so a run can't form a closing `"""`, and trailing spaces become `\s` so they survive
/// the trailing-whitespace stripping done by both the renderer and the parser.
fn encode_multiline_line(line: &str) -> String {
    let trailing_spaces = line.len() - line.trim_end_matches(' ').len();
    let mut out = String::new();
    for ch in line[..line.len() - trailing_spaces].chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    for _ in 0..trailing_spaces {
        out.push_str("\\s");
    }
    out
}

fn field_doc(trivia: &Trivia, field: &TupleField) -> Doc {
    let value = match &field.value {
        FieldValue::Spread(None) => pretty::text("..."),
        FieldValue::Spread(Some(name)) => pretty::text(format!("...{}", name)),
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

/// Render a spawn (`@f`, `@~`, `@{ … }`, `@('int) { … }`). An inline spawned function must use the
/// `@`-sugar forms — the parser does not accept `@#…` — so a function head is emitted as `@{ body }`
/// or `@(type) { body }` (the parenthesised arm accepts any type).
fn spawn_doc(trivia: &Trivia, func: &Term) -> Doc {
    match func {
        Term::Function(function) => {
            let head = match &function.parameter_type {
                None => "@".to_string(),
                Some(parameter_type) => format!("@({}) ", render_type(parameter_type)),
            };
            match &function.body {
                None => pretty::text(head),
                Some(body) => pretty::concat(vec![pretty::text(head), block_doc(trivia, body)]),
            }
        }
        other => pretty::text(format!("@{}", render_term_atom(other))),
    }
}

fn select_doc(trivia: &Trivia, sources: &Option<Vec<Chain>>) -> Doc {
    let Some(chains) = sources else {
        return pretty::text("!");
    };
    // A single source that has a tight shorthand keeps it (`!p`, `!#'int`, …): the general
    // `! [#'int]` form is *not* always equivalent — a following `{ … }` handler binds differently —
    // and a select source list takes no trailing comma, so it is reserved for genuine multi-source
    // and filter selects.
    if let Some(shorthand) = select_shorthand(chains) {
        return pretty::text(shorthand);
    }
    bracketed(
        "! [".to_string(),
        "]",
        chains
            .iter()
            .map(|chain| chain_doc(trivia, chain))
            .collect(),
        false,
    )
}

/// The tight single-source shorthand for a select, when the source list is a single source whose AST
/// has one (`!p`/`!f`, `!#'int`, `!1000`). Returns `None` for the general form — several sources, or
/// a filter (a receive function *with* a body, which only the general `! […]` form can express).
fn select_shorthand(chains: &[Chain]) -> Option<String> {
    let [chain] = chains else { return None };
    if chain.match_pattern.is_some() {
        return None;
    }
    let [term] = chain.terms.as_slice() else {
        return None;
    };
    match term {
        // `!f`, `!p`, `!%mod.recv`, `!.` — the `&` is part of the sugar.
        Term::Reference(access) => Some(format!("!{}", render_access(access))),
        // `!#'int`, `!#Reply[...]` — a body-less identity receive.
        Term::Function(function)
            if function.type_parameters.is_empty()
                && function.return_type.is_none()
                && function.body.is_none() =>
        {
            function
                .parameter_type
                .as_ref()
                .map(|parameter_type| format!("!#{}", render_type_atom(parameter_type)))
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
/// start offset of the AST node (type-alias statement, chain, or tuple field) they attach to;
/// `dangling` holds anything after the last node.
struct Trivia {
    leading: HashMap<usize, Vec<TriviaItem>>,
    trailing: HashMap<usize, Vec<String>>,
    dangling: Vec<TriviaItem>,
}

impl Trivia {
    /// Recover trivia from `source` and attach each item to an AST node: a leading comment/blank to
    /// the nearest following node, a trailing comment to the node whose text it follows.
    fn collect(program: &Program, source: &str) -> Trivia {
        let mut anchors = Vec::new();
        collect_anchors(program, &mut anchors);
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
        for item in scan_trivia(source) {
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
/// not mistaken for trivia.
fn scan_trivia(source: &str) -> Vec<Scanned> {
    let mut out = Vec::new();
    let mut chars = source.char_indices().peekable();
    let mut in_string = false;
    let mut escaped = false;
    let mut line_start = 0usize;
    let mut line_blank = true;
    while let Some((index, c)) = chars.next() {
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
                let mut end = source.len();
                while let Some(&(j, next)) = chars.peek() {
                    if next == '\n' {
                        end = j;
                        break;
                    }
                    chars.next();
                }
                out.push(Scanned::Comment {
                    offset: index,
                    text: source[index..end].trim_end().to_string(),
                    trailing: !line_blank,
                });
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

/// Collect every node trivia can attach to: type-alias statements, the chains of a sequence, and
/// tuple fields. Mirrors where [`sequence_doc`]/[`field_doc`]/[`statement_doc`] emit trivia, so
/// every attached item has exactly one emission site.
fn collect_anchors(program: &Program, out: &mut Vec<Anchor>) {
    for statement in &program.statements {
        match statement {
            Statement::TypeAlias { name_span, .. } => push_anchor(*name_span, out),
            Statement::Expression(sequence) => visit_sequence(sequence, out),
        }
    }
}

fn push_anchor(span: Spanned, out: &mut Vec<Anchor>) {
    if let Some(span) = span.get() {
        out.push(Anchor {
            start: span.offset,
            end: span.offset + span.length,
        });
    }
}

fn visit_sequence(sequence: &Sequence, out: &mut Vec<Anchor>) {
    for chain in &sequence.chains {
        push_anchor(chain.span, out);
        visit_chain(chain, out);
    }
}

/// Recurse into a chain's terms without making the chain itself an anchor (used for select sources
/// and tuple-field chains, which are not emitted by `sequence_doc`).
fn visit_chain(chain: &Chain, out: &mut Vec<Anchor>) {
    for term in &chain.terms {
        visit_term(term, out);
    }
}

fn visit_term(term: &Term, out: &mut Vec<Anchor>) {
    match term {
        Term::Tuple(tuple) => {
            for field in &tuple.fields {
                push_anchor(field.span, out);
                if let FieldValue::Chain(chain) = &field.value {
                    visit_chain(chain, out);
                }
            }
        }
        Term::Block(expression) => visit_expression(expression, out),
        Term::Function(function) => {
            if let Some(body) = &function.body {
                visit_expression(body, out);
            }
        }
        Term::Spawn(inner, _) => visit_term(inner, out),
        Term::Select(Some(chains), _) => {
            for chain in chains {
                visit_chain(chain, out);
            }
        }
        _ => {}
    }
}

fn visit_expression(expression: &Expression, out: &mut Vec<Anchor>) {
    for branch in &expression.branches {
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

fn render_literal(literal: &Literal) -> String {
    match literal {
        Literal::Integer(value) => value.to_string(),
        Literal::Binary(bytes) => format!("0x{}", hex::encode(bytes)),
    }
}

fn render_access(access: &Access) -> String {
    let mut out = String::new();
    match &access.source {
        None => {}
        Some(AccessSource::Identifier(name)) => out.push_str(name),
        Some(AccessSource::Parameter) => out.push('$'),
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
    let dotless_first = matches!(access.source, Some(AccessSource::Parameter));
    for (index, accessor) in access.accessors.iter().enumerate() {
        if !(index == 0 && dotless_first) {
            out.push('.');
        }
        match accessor {
            AccessPath::Field(field) => out.push_str(field),
            AccessPath::Index(value) => out.push_str(&value.to_string()),
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

fn render_match(pattern: &Match) -> String {
    match pattern {
        Match::Identifier(name, _) => name.clone(),
        Match::Literal(literal) => render_literal(literal),
        Match::Tuple(tuple) => render_match_tuple(tuple),
        Match::Partial(partial) => render_partial_pattern(partial),
        Match::Star(None) => "*".to_string(),
        Match::Star(Some(name)) => format!("{}*", name),
        Match::Placeholder => "_".to_string(),
        Match::Reference(name, _) => format!("&{}", name),
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
        // immediately followed by the binder.
        Match::As(type_def, name, _) => format!("({}){}", render_type(type_def), name),
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
        Type::Function(function_type) => format!(
            "#{} -> {}",
            render_type_atom(&function_type.input),
            render_type_atom(&function_type.output)
        ),
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
/// receive/return, function input/output): wrap an intersection or function in parentheses. A union
/// is already parenthesised by `render_type`.
fn render_type_atom(type_def: &Type) -> String {
    match type_def {
        Type::Intersection(_) | Type::Function(_) => {
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
    let name = tuple_type.name.clone().unwrap_or_default();
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
        .map(render_field_type)
        .collect::<Vec<_>>()
        .join(", ");
    let (open, close) = if tuple_type.is_partial {
        ("(", ")")
    } else {
        ("[", "]")
    };
    format!("{}{}{}{}", name, open, fields, close)
}

fn render_field_type(field_type: &FieldType) -> String {
    match field_type {
        FieldType::Field {
            name: Some(name),
            type_def,
        } => format!("{}: {}", name, render_type(type_def)),
        FieldType::Field {
            name: None,
            type_def,
        } => render_type(type_def),
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
    match (&process_type.receive_type, &process_type.return_type) {
        (Some(receive), None) => format!("@{}", render_type_atom(receive)),
        (None, None) => "@".to_string(),
        (None, Some(ret)) => format!("(@-> {})", render_type_atom(ret)),
        (Some(receive), Some(ret)) => {
            format!(
                "(@{} -> {})",
                render_type_atom(receive),
                render_type_atom(ret)
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use std::path::PathBuf;

    /// Reduce a program to the compiler's canonical, block-free form: every no-op block stripped or
    /// lifted (`Compiler::compile` does the same before codegen). Two programs equal here compile
    /// identically.
    fn canonical(program: Program) -> Program {
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
    fn drops_redundant_blocks() {
        // A single branchless, binding-free block is spliced into the surrounding chain.
        assert_formats("bit = { [a, b] f [1, ~] g }", "bit = [a, b] f [1, ~] g\n");
        // Nested redundant blocks collapse fully.
        assert_formats("x = { { 5 } }", "x = 5\n");
        // A block in a consequence position is also unwrapped.
        assert_formats(
            "f = #'int { =0 => { [~, 1] g } | h }",
            "f = #'int { =0 => [~, 1] g | h }\n",
        );
    }

    #[test]
    fn consequence_block_aligns_with_its_bar_line() {
        // A consequence block's `}` aligns with the bar (`|`) of the line carrying its `{`, indented
        // from the bar rather than from the content past the `| `.
        assert_formats(
            "f = #'t { =B[h, t] => { idx =0 => h | [idx, 1] %num.sub [t, ~] ^ } | other_branch }",
            "f = #'t {\n  | =B[h, t] => {\n    | idx =0 => h\n    | [idx, 1] %num.sub [t, ~] ^\n  }\n  | other_branch\n}\n",
        );
        // The same when the block ends a branch's *condition* chain (`| lst { … }`): the body is one
        // chain, so the block stays at the bar indent and its `}` aligns with the `|`.
        assert_formats(
            "f = #'t { lst { =Nil => empty_result | =Cons[h, t] => [h, t] process } | fallback }",
            "f = #'t {\n  | lst {\n    | =Nil => empty_result\n    | =Cons[h, t] => [h, t] process\n  }\n  | fallback\n}\n",
        );
    }

    #[test]
    fn tall_steps_get_surrounding_blank_lines() {
        // A `~>` pipeline step is set off from its short neighbours with a blank line on each side…
        assert_formats(
            "#{ first_step, target_len [~, suffix_len] %num.sub [target, ~, target_len] %bin.slice =&suffix, last_step }",
            "#{\n  first_step\n\n  target_len\n  ~> [~, suffix_len] %num.sub\n  ~> [target, ~, target_len] %bin.slice\n  ~> =&suffix\n\n  last_step\n}\n",
        );
        // …but a body of only short steps stays packed (no imposed blanks).
        assert_formats("#{ aa, bb, cc }", "#{ aa, bb, cc }\n");
    }

    #[test]
    fn binding_pipeline_breaks_after_equals() {
        // A binding whose value breaks into a `~>` pipeline breaks after the `=` and indents the
        // pipeline, rather than leaving the head on the `=` line and dangling the continuations.
        assert_formats(
            "#{ char_end = byte_pos [~, 1] %num.add [data, ~, data_len] skip_continuation }",
            "#{\n  char_end =\n    byte_pos\n    ~> [~, 1] %num.add\n    ~> [data, ~, data_len] skip_continuation\n}\n",
        );
        // A short binding stays inline…
        assert_formats(
            "#{ x = byte_pos [~, 1] %num.add }",
            "#{ x = byte_pos [~, 1] %num.add }\n",
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
            "#{ x { haystack_len [~, needle_len] %num.sub [haystack, needle, 0, ~] find_index => Ok | other } }",
            "#{\n  x {\n    | haystack_len\n      ~> [~, needle_len] %num.sub\n      ~> [haystack, needle, 0, ~] find_index => Ok\n    | other\n  }\n}\n",
        );
        // A consequence that follows the last continuation on the same line lays out relative to
        // that deeper indent too — a breaking tuple's fields indent under its `[`, `]` aligned.
        assert_formats(
            "#{ x { [haystack, delim, start, end] find_index =('int)index => [haystack [~, start, index] %bin.slice Str[~], [index, delim_len] %num.add, another_field, one_more_field] | other } }",
            "#{\n  x {\n    | [haystack, delim, start, end] find_index\n      ~> =('int)index => [\n        haystack [~, start, index] %bin.slice Str[~],\n        [index, delim_len] %num.add,\n        another_field,\n        one_more_field,\n      ]\n    | other\n  }\n}\n",
        );
    }

    #[test]
    fn wraps_breaking_consequence_pipeline() {
        // A consequence that is a single chain breaking into a `~>` pipeline is wrapped in grouping
        // braces, so the continuation reads as a delimited body instead of dangling at the bar.
        assert_formats(
            "f = #'t { =Cons[[k, val], t] => [&put, d, k, val, k hash, 0] put [&self, ~, t] self | =Nil => d }",
            "f = #'t {\n  | =Cons[[k, val], t] => {\n    [&put, d, k, val, k hash, 0] put\n    ~> [&self, ~, t] self\n  }\n  | =Nil => d\n}\n",
        );
        // A short consequence stays bare (it does not break).
        assert_formats(
            "f = #'t { =A => a b | =B => c }",
            "f = #'t { =A => a b | =B => c }\n",
        );
    }

    #[test]
    fn groups_compound_consequences() {
        // A bare multi-step frame-free consequence is wrapped in grouping braces…
        assert_formats("x = 5 { =0 => a, b | c }", "x = 5 { =0 => { a, b } | c }\n");
        // …a single-step consequence (one chain, many terms) stays bare…
        assert_formats("x = 5 { =0 => a b | c }", "x = 5 { =0 => a b | c }\n");
        // …a binding consequence keeps its own markers and stays bare…
        assert_formats(
            "x = 5 { =0 => y = 1, [y, 2] g | c }",
            "x = 5 { =0 => y = 1, [y, 2] g | c }\n",
        );
        // …and an already-braced consequence is not double-wrapped.
        assert_formats(
            "x = 5 { =0 => { a, b } | c }",
            "x = 5 { =0 => { a, b } | c }\n",
        );
    }

    #[test]
    fn flatten_paths_do_not_corrupt_comments() {
        // Regression: flattening a chain head or guard condition that carries a comment must not
        // inline the comment (which would comment out the rest of the line). The output must reparse
        // and be a fixpoint — the corrupt output of the old bug did neither.
        assert_idempotent("#{\n  // c\n  foo\n} g [x, y]", "commented chain head");
        assert_idempotent(
            "5 {\n  [ z: ~, // c\n  ] foo? => x | y\n}",
            "comment on a guard field",
        );
    }

    #[test]
    fn keeps_narrowing_barrier_and_unsafe_tail_blocks() {
        // A block wrapping a match can be a deliberate narrowing barrier — never strip it.
        assert_formats("x = 5 { { =A } => 1 | 2 }", "x = 5 { { =A } => 1 | 2 }\n");
        // A tail call that is not the chain's last term keeps its block (no mid-chain dead code)…
        assert_formats("x = { [a] ^ } f", "x = { [a] ^ } f\n");
        // …a non-final tail call inside the body keeps the block too…
        assert_formats("x = 5 { ^ foo }", "x = 5 { ^ foo }\n");
        // …but a block whose tail call ends up last after splicing is dropped cleanly.
        assert_formats("x = y { [a] ^ }", "x = y [a] ^\n");
    }

    #[test]
    fn keeps_meaningful_blocks() {
        // A binding inside the block would leak if inlined, so the block stays.
        assert_formats("x = { 5 =y, y }", "x = { 5 =y, y }\n");
        // Multiple branches are not redundant.
        assert_formats("x = 5 { =0 => a | b }", "x = 5 { =0 => a | b }\n");
        // A comment inside the block keeps it (so the comment is not lost).
        assert_formats("x = {\n  // note\n  5 f\n}", "x = {\n  // note\n  5 f\n}\n");
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
            // An embedded quote now round-trips as `\"` rather than falling back to `Str[0x…]`.
            r#""say \"hi\"""#,
        ] {
            assert_idempotent(source, source);
        }
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
            // --- argument-first application ---
            "[3, 4] add [~, 2] mul",
            "x f",
            "[[x] g, y] f",
            "[3, 4] __integer_add__",
            "&g f",
            "5 f",
            // --- bare ripple (flowing-value) terms: no juxtaposed argument ---
            "5 ~",
            "num ~.add",
            "&g ^~",
            "&g @~",
            // --- argument-first tail calls ---
            "[a, b] ^",
            "[x] ^foo",
            // --- spawn ---
            "@f",
            "@{ 5 }",
            "@'int { $ }",
            "@('int | 'bin) { $ }",
            "x @counter",
            // --- select / process / references ---
            "!'int",
            "!'int { =0 => Ok | [] }",
            "! [p, 1000]",
            "!p",
            "! []",
            "&f",
            "&.",
            "&__integer_add__",
            "42 pid",
            "@3",
            // --- ripple / spread values ---
            "5 [~, 1]",
            "0 Point[x: ~, y: ~]",
            "a[..., y: 3]",
            "~[..., y: 3]",
            "A[x: 1] B[...]",
            "[...a, ...b]",
            "[w: 0, ...a]",
            // --- field access / self / operators ---
            "point.x .name",
            "$ $.x $.0",
            "5 ==",
            "[] <>",
            "42 .",
            // --- match forms ---
            "=Point[x, y]",
            "=(x: 'int)",
            "=Config*",
            "=*",
            "=_",
            "=&y",
            "=('int)n",
            "=([a] | [b])",
            "='int",
            "=Circle[radius: r]",
            "=\"hello\"",
            "=42",
            "=0x0a1b",
            "Point[x, y] = p",
            "x = 5",
            "(a, b) = p",
            "[x: a, y: b] = p",
            "Config(host, port) = c",
            // --- blocks / branches / consequence ---
            "v { =0 => \"zero\" | \"neg\" }",
            "item { is_valid? process | [] show_error }",
            "{ =Square[x] [x, 10] num.gt? => \"large\" | \"small\" }",
            // --- functions ---
            "xs [~, #{ $0 }, Nil] map",
            "#['int, 'int] { =[a, b] => [b, a] }",
            "#<'t>'t { $ }",
            "#'int",
            "#'int -> 'bin { $ }",
            // --- multi-chain sequences & control flow ---
            "tag = %ref, [tag, 42] =[&tag, x], x",
            "[], 5",
            // --- type aliases: unions, intersections, partials, modules, recursion ---
            "'bool = True | False",
            "'shape = Circle[radius: 'int] | Rectangle[width: 'int, height: 'int]",
            "'rw = 'readable & 'writable",
            "'inter = 'a & 'b | 'c",
            "'list<'t> = Nil | Cons['t, ^]",
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
            "'both = (@'int -> 'bin)",
            "'ret = (@-> 'bin)",
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
