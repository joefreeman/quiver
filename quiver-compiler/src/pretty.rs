//! A minimal Wadler/Prettier-style document algebra for the formatter.
//!
//! The formatter renders the AST to a [`Doc`] tree, then [`print`] lays it out against a target
//! width. Line breaking is not decided per construct; it falls out of one rule: a [`Doc::Group`] is
//! laid out flat if it fits the remaining width, otherwise it breaks, which switches the
//! `Line`/`SoftLine`/`IfBreak` nodes inside it to their broken form. A forced break ([`Doc::HardLine`],
//! e.g. from a comment) propagates outward, opening every enclosing group.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Flat,
    Break,
}

#[derive(Debug, Clone)]
pub enum Doc {
    Nil,
    /// Atomic text; must not contain newlines.
    Text(String),
    /// A space when flat, a newline+indent when broken.
    Line,
    /// Empty when flat, a newline+indent when broken.
    SoftLine,
    /// Always a newline+indent. Forces every enclosing group to break.
    HardLine,
    Concat(Vec<Doc>),
    /// Indent the contained breaks by `0` extra spaces (relative).
    Nest(usize, Box<Doc>),
    /// `(content, should_break)` — `should_break` is precomputed by [`group`] from a forced break
    /// anywhere in `content`, so a hard line deep inside opens this group without a width check.
    Group(Box<Doc>, bool),
    /// `broken` when the enclosing group is broken, `flat` otherwise. How leading `|`, `~>`
    /// continuations, and trailing commas appear only when a construct is laid out vertically.
    IfBreak(Box<Doc>, Box<Doc>),
    /// Content deferred to the end of the current line (a trailing comment): buffered when reached
    /// and flushed just before the next line break. It does not count toward line width.
    LineSuffix(Box<Doc>),
    /// Renders nothing, but forces every enclosing group to break. Pairs with `LineSuffix` so the
    /// content after a trailing comment moves onto a new line rather than being commented out.
    BreakParent,
}

pub fn nil() -> Doc {
    Doc::Nil
}

pub fn text(s: impl Into<String>) -> Doc {
    Doc::Text(s.into())
}

pub fn line() -> Doc {
    Doc::Line
}

pub fn softline() -> Doc {
    Doc::SoftLine
}

pub fn hardline() -> Doc {
    Doc::HardLine
}

pub fn concat(docs: Vec<Doc>) -> Doc {
    Doc::Concat(docs)
}

pub fn nest(indent: usize, doc: Doc) -> Doc {
    Doc::Nest(indent, Box::new(doc))
}

pub fn group(doc: Doc) -> Doc {
    let should_break = forces_break(&doc);
    Doc::Group(Box::new(doc), should_break)
}

pub fn if_break(broken: Doc, flat: Doc) -> Doc {
    Doc::IfBreak(Box::new(broken), Box::new(flat))
}

pub fn line_suffix(doc: Doc) -> Doc {
    Doc::LineSuffix(Box::new(doc))
}

pub fn break_parent() -> Doc {
    Doc::BreakParent
}

/// Join `docs` with `separator` between each pair.
pub fn join(separator: Doc, docs: Vec<Doc>) -> Doc {
    let mut out = Vec::new();
    for (i, doc) in docs.into_iter().enumerate() {
        if i > 0 {
            out.push(separator.clone());
        }
        out.push(doc);
    }
    concat(out)
}

/// Whether a doc contains a break that must always render (a `HardLine`/`BreakParent`, or such a
/// break inside a nested group). The flat branch of an `IfBreak` matters (it would render when the
/// enclosing group is flat); the broken branch never forces, since it only renders once already
/// broken. Callers also use this to refuse [`flatten`] on a doc that cannot legally sit on one line
/// (anything carrying a comment forces a break, and flattening it would corrupt the source).
pub fn forces_break(doc: &Doc) -> bool {
    match doc {
        Doc::HardLine | Doc::BreakParent => true,
        Doc::Concat(docs) => docs.iter().any(forces_break),
        Doc::Nest(_, inner) => forces_break(inner),
        Doc::Group(_, should_break) => *should_break,
        Doc::IfBreak(_, flat) => forces_break(flat),
        // A `LineSuffix`'s content is deferred, so it never forces a break on its own.
        Doc::Nil | Doc::Text(_) | Doc::Line | Doc::SoftLine | Doc::LineSuffix(_) => false,
    }
}

/// A pending layout step: the indent and mode under which a doc is to be rendered.
type Frame<'a> = (usize, Mode, &'a Doc);

/// Lay out `doc` to a string, breaking groups that would exceed `width` columns. Trailing
/// whitespace (e.g. indentation emitted before a blank line) is stripped.
pub fn print(doc: &Doc, width: usize) -> String {
    let mut out = String::new();
    let mut col = 0usize;
    let mut stack: Vec<Frame> = vec![(0, Mode::Break, doc)];
    // Trailing-comment content reached but not yet emitted; flushed before the next line break.
    let mut suffixes: Vec<Frame> = Vec::new();
    loop {
        let Some((indent, mode, doc)) = stack.pop() else {
            if suffixes.is_empty() {
                break;
            }
            // End of input with buffered trailing comments: flush them.
            stack.extend(suffixes.drain(..).rev());
            continue;
        };
        match doc {
            Doc::Nil | Doc::BreakParent => {}
            Doc::Text(s) => {
                out.push_str(s);
                col += s.chars().count();
            }
            Doc::Concat(docs) => {
                for child in docs.iter().rev() {
                    stack.push((indent, mode, child));
                }
            }
            Doc::Nest(extra, inner) => stack.push((indent + extra, mode, inner)),
            Doc::LineSuffix(inner) => suffixes.push((indent, mode, inner)),
            Doc::Line if mode == Mode::Flat => {
                out.push(' ');
                col += 1;
            }
            Doc::SoftLine if mode == Mode::Flat => {}
            // A real line break (`Line`/`SoftLine` in break mode, or any `HardLine`): flush any
            // buffered trailing comments onto this line first, then emit the break.
            Doc::Line | Doc::SoftLine | Doc::HardLine => {
                if suffixes.is_empty() {
                    col = newline(&mut out, indent);
                } else {
                    stack.push((indent, mode, doc));
                    stack.extend(suffixes.drain(..).rev());
                }
            }
            Doc::IfBreak(broken, flat) => {
                stack.push((
                    indent,
                    mode,
                    if mode == Mode::Break { broken } else { flat },
                ));
            }
            Doc::Group(inner, should_break) => {
                let mode =
                    if *should_break || !fits(width.saturating_sub(col), indent, inner, &stack) {
                        Mode::Break
                    } else {
                        Mode::Flat
                    };
                stack.push((indent, mode, inner));
            }
        }
    }
    strip_trailing_whitespace(&out)
}

/// Lay out `doc` entirely flat onto one line: every `Line` is a space, every `SoftLine`/flat
/// `IfBreak` collapses. **Callers must first check [`forces_break`] is false** — a doc that forces a
/// break carries a comment or hard line that cannot legally sit on one line, and flattening it would
/// silently comment out the following code. Used to keep a chain's head (or a guard condition) on a
/// single line so a large trailing container does not push it onto `~>` lines.
pub fn flatten(doc: &Doc) -> String {
    let mut out = String::new();
    let mut stack: Vec<&Doc> = vec![doc];
    while let Some(doc) = stack.pop() {
        match doc {
            Doc::Nil | Doc::SoftLine | Doc::BreakParent => {}
            Doc::Text(s) => out.push_str(s),
            Doc::Line => out.push(' '),
            Doc::HardLine => out.push('\n'),
            Doc::Concat(docs) => stack.extend(docs.iter().rev()),
            Doc::Nest(_, inner) | Doc::Group(inner, _) | Doc::LineSuffix(inner) => {
                stack.push(inner)
            }
            Doc::IfBreak(_, flat) => stack.push(flat),
        }
    }
    strip_trailing_whitespace(&out)
}

/// The flat (single-line) width of `doc` in columns, or `None` if it would exceed `max` or cannot be
/// flat (it contains a `HardLine` or `BreakParent`). `LineSuffix` content is deferred and not counted.
/// Walks without allocating a rendered string and exits early once `max` is passed.
pub fn flat_width(doc: &Doc, max: usize) -> Option<usize> {
    let mut width = 0usize;
    let mut stack: Vec<&Doc> = vec![doc];
    while let Some(doc) = stack.pop() {
        match doc {
            Doc::Nil | Doc::SoftLine | Doc::LineSuffix(_) => {}
            Doc::Text(s) => width += s.chars().count(),
            Doc::Line => width += 1,
            Doc::HardLine | Doc::BreakParent => return None,
            Doc::Concat(docs) => stack.extend(docs.iter().rev()),
            Doc::Nest(_, inner) | Doc::Group(inner, _) => stack.push(inner),
            Doc::IfBreak(_, flat) => stack.push(flat),
        }
        if width > max {
            return None;
        }
    }
    Some(width)
}

fn newline(out: &mut String, indent: usize) -> usize {
    out.push('\n');
    for _ in 0..indent {
        out.push(' ');
    }
    indent
}

/// Whether `group_inner` (laid out flat) plus the continuation `rest` fits in `remaining` columns
/// before the next line break. Reaching a break (a `Line`/`SoftLine` already in break mode, or a
/// `HardLine`) ends the measured line, so it fits.
fn fits(remaining: usize, indent: usize, group_inner: &Doc, rest: &[Frame]) -> bool {
    let mut remaining = remaining as isize;
    // The group's own contents, processed flat; once exhausted we continue into `rest` (the
    // continuation stack) by index, top first — no clone of the whole layout stack per group check.
    let mut local: Vec<Frame> = vec![(indent, Mode::Flat, group_inner)];
    let mut rest_top = rest.len();
    while remaining >= 0 {
        let Some((indent, mode, doc)) = local.pop().or_else(|| {
            rest_top = rest_top.checked_sub(1)?;
            Some(rest[rest_top])
        }) else {
            return true;
        };
        match doc {
            Doc::Nil => {}
            Doc::Text(s) => remaining -= s.chars().count() as isize,
            Doc::Concat(docs) => {
                for child in docs.iter().rev() {
                    local.push((indent, mode, child));
                }
            }
            Doc::Nest(extra, inner) => local.push((indent + extra, mode, inner)),
            Doc::Line => match mode {
                Mode::Flat => remaining -= 1,
                Mode::Break => return true,
            },
            Doc::SoftLine => match mode {
                Mode::Flat => {}
                Mode::Break => return true,
            },
            Doc::HardLine => return true,
            // A trailing comment is deferred (zero width here); a break-parent renders nothing.
            Doc::LineSuffix(_) | Doc::BreakParent => {}
            Doc::IfBreak(broken, flat) => {
                local.push((
                    indent,
                    mode,
                    if mode == Mode::Break { broken } else { flat },
                ));
            }
            Doc::Group(inner, should_break) => {
                let mode = if *should_break {
                    Mode::Break
                } else {
                    Mode::Flat
                };
                local.push((indent, mode, inner));
            }
        }
    }
    false
}

fn strip_trailing_whitespace(s: &str) -> String {
    s.lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n")
}
