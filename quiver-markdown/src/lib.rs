//! Quiver code embedded in Markdown.
//!
//! A document is a sequence of chapters, each holding the fenced `quiver` blocks written under
//! it. A chapter is the unit of scope: its blocks are fragments of one accumulating session,
//! in document order, the way a reader accumulates them. Splitting a block into steps is the
//! only part that needs the language, and it uses the real parser — a line scan would be
//! defeated by a `"""` string, and by the `//=` line that continues the step above it.
//!
//! Nothing here runs anything or touches the filesystem: a caller drives the steps, so the same
//! document can be checked by the CLI and by the browser.

use quiver_compiler::ast::{
    Block as AstBlock, Chain, FieldValue, Sequence, Step as AstStep, StrSegment, Term,
};
use quiver_compiler::parser::Error as ParseError;

/// What the runner does with a block, from its fence annotation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// The default. The block's steps join the chapter's accumulating session.
    Session,
    /// ```` ```quiver program ```` — compiled and run whole, as a standalone file, because its
    /// last step is an entry-point function rather than something to evaluate in a session.
    Program,
    /// ```` ```quiver ignore ```` — not run. For fragments that need the network, a second
    /// file, or an elided body.
    Ignore,
}

/// A fenced `quiver` block.
#[derive(Debug, Clone)]
pub struct Block {
    pub mode: Mode,
    /// 1-based line of the block's first body line in the document.
    pub line: usize,
    pub source: String,
}

/// One step of a block: what a caller feeds to the evaluator, one at a time.
#[derive(Debug, Clone)]
pub struct Step {
    pub source: String,
    /// 1-based line in the document.
    pub line: usize,
    /// 1-based column of the step's first character.
    pub column: usize,
    /// How many `//= P` assertions the step carries. A step may carry several, since a leading
    /// `//=` continues the step above rather than opening a new one.
    pub assertions: usize,
    /// A `//! text` failure expectation: the step must fail, and the rendered error must contain
    /// `text`. An empty fragment accepts any failure.
    pub expect_failure: Option<String>,
}

/// The `//! text` on a step's last line, if it carries one.
///
/// Read from the text rather than the AST, unlike a `//=` assertion. The language has no such
/// construct — to the compiler `//!` is an ordinary comment — because the whole point of the
/// marker is to sit on a step the compiler *rejects*, where there would be no tree to hang it
/// on. Hence a scan: confined to the step's last line, and skipping a `//!` inside a string.
///
/// Being textual, it can in principle misread (a `//!` inside a multi-line string that ends on
/// the step's last line). That direction is safe: a step wrongly marked as expected-to-fail
/// *succeeds*, which the runner reports. It cannot turn a real failure into a pass.
fn failure_expectation(source: &str) -> Option<String> {
    let line = source.lines().next_back()?;
    let mut quoted = false;
    let mut chars = line.char_indices();
    while let Some((index, c)) = chars.next() {
        match c {
            '\\' if quoted => {
                chars.next();
            }
            '"' => quoted = !quoted,
            '/' if !quoted && line[index..].starts_with("//!") => {
                // As after a `//=` pattern, a run of three or more spaces starts a prose note
                // that is not part of the expectation.
                let rest = line[index + 3..].trim_start();
                let message = match rest.find("   ") {
                    Some(note) => &rest[..note],
                    None => rest,
                };
                return Some(message.trim_end().to_string());
            }
            _ => {}
        }
    }
    None
}

impl Step {
    /// The step's source shifted to the position it occupies in the document, by padding with
    /// the blank lines and columns that precede it. Compiling this rather than the bare text
    /// makes every span the compiler reports — a parse error, an assertion failure — carry
    /// document coordinates, with no mapping to maintain on this side.
    pub fn positioned(&self) -> String {
        format!(
            "{}{}{}",
            "\n".repeat(self.line - 1),
            " ".repeat(self.column - 1),
            self.source
        )
    }
}

impl Block {
    /// Split the block into the steps the language sees, each sliced from the source so it
    /// carries its own comments and assertions verbatim.
    ///
    /// The block is parsed as one sequence and then cut at each step's start offset, rather than
    /// parsed per step: a step's text is only identifiable once the whole block has been read.
    pub fn steps(&self) -> Result<Vec<Step>, ParseError> {
        let sequence = quiver_compiler::parse(&self.source)?;
        let starts: Vec<_> = sequence
            .steps
            .iter()
            .map(|step| {
                step.span()
                    .get()
                    .expect("a parsed step always records a span")
            })
            .collect();

        Ok(starts
            .iter()
            .enumerate()
            .map(|(index, span)| {
                let end = starts
                    .get(index + 1)
                    .map_or(self.source.len(), |next| next.offset);
                let source = self.source[span.offset..end].trim_end().to_string();
                Step {
                    expect_failure: failure_expectation(&source),
                    source,
                    line: self.line + span.line - 1,
                    column: span.column,
                    assertions: match &sequence.steps[index] {
                        // Nested assertions included: they are checked when the step runs, so
                        // they are part of what running it establishes.
                        AstStep::Chain(chain) => chain_assertions(chain),
                        // An alias produces no value to assert on; the parser rejects one that
                        // carries an assertion.
                        AstStep::TypeAlias { .. } => 0,
                    },
                }
            })
            .collect())
    }
}

/// Every `//= P` assertion the source makes, at any depth — one inside a function body or a
/// branch is checked exactly as a step-final one is, so it counts.
///
/// The traversal matches exhaustively rather than falling through on a wildcard: an AST node
/// that grows a nested sequence must then be considered here, instead of silently dropping the
/// assertions inside it from a total that reads as complete.
pub fn count_assertions(source: &str) -> Result<usize, ParseError> {
    Ok(sequence_assertions(&quiver_compiler::parse(source)?))
}

fn sequence_assertions(sequence: &Sequence) -> usize {
    sequence
        .steps
        .iter()
        .map(|step| match step {
            AstStep::Chain(chain) => chain_assertions(chain),
            AstStep::TypeAlias { .. } => 0,
        })
        .sum()
}

fn chain_assertions(chain: &Chain) -> usize {
    chain.assertions.len() + chain.terms.iter().map(term_assertions).sum::<usize>()
}

fn block_assertions(block: &AstBlock) -> usize {
    block
        .annotations
        .iter()
        .map(|annotation| chain_assertions(&annotation.value))
        .chain(block.branches.iter().map(|branch| {
            sequence_assertions(&branch.condition)
                + branch.consequence.as_ref().map_or(0, sequence_assertions)
        }))
        .sum()
}

fn term_assertions(term: &Term) -> usize {
    match term {
        Term::Tuple(tuple) => tuple
            .fields
            .iter()
            .map(|field| match &field.value {
                FieldValue::Chain(chain) => chain_assertions(chain),
                FieldValue::Spread(_) => 0,
            })
            .sum(),
        Term::String(_, segments) => segments
            .iter()
            .map(|segment| match segment {
                StrSegment::Hole(block) => block_assertions(block),
                StrSegment::Text(_) => 0,
            })
            .sum(),
        Term::Block(block) => block_assertions(block),
        Term::Function(function) => function.body.as_ref().map_or(0, block_assertions),
        Term::Spawn(inner, argument, _) => {
            term_assertions(inner) + argument.as_ref().map_or(0, |a| term_assertions(a))
        }
        Term::Apply(_, argument) => term_assertions(argument),
        Term::Select(sources, _) => sources.iter().flatten().map(chain_assertions).sum(),
        // A dialect's content is the dialect's grammar, not Quiver, and its expansion is
        // produced at compile time — there is no source text here to carry an assertion.
        Term::Dialect(_) => 0,
        Term::Literal(_)
        | Term::Match(_)
        | Term::Access(_)
        | Term::Self_
        | Term::State(..)
        | Term::Process(_) => 0,
    }
}

/// The source with the `//= P` assertions of its own chains cut out, leaving line structure
/// intact (an assertion runs to the end of its line, so nothing but the check is removed).
///
/// This is how a failed assertion's *actual* value is recovered: the check aborts the process
/// before anything can format what it saw, so the step is re-run without it. Assertions nested
/// inside the step — in a function body, a branch — are left in place, since one of those
/// failing is a different site than the one being reported, and its own span already names it.
pub fn without_assertions(source: &str) -> Result<String, ParseError> {
    let sequence = quiver_compiler::parse(source)?;
    let mut spans: Vec<_> = sequence
        .chains()
        .flat_map(|chain| &chain.assertions)
        .filter_map(|assertion| assertion.span.get())
        .collect();
    spans.sort_by_key(|span| span.offset);

    let mut out = String::new();
    let mut cursor = 0;
    for span in spans {
        out.push_str(&source[cursor..span.offset]);
        cursor = span.offset + span.length;
    }
    out.push_str(&source[cursor..]);
    Ok(out)
}

/// The blocks written under one `##` heading — and, before the first, the region the document
/// opens with.
#[derive(Debug, Clone)]
pub struct Chapter {
    /// The heading text, or `None` for the region before the first `##`.
    pub title: Option<String>,
    pub blocks: Vec<Block>,
}

impl Chapter {
    pub fn runnable(&self) -> impl Iterator<Item = &Block> {
        self.blocks.iter().filter(|b| b.mode != Mode::Ignore)
    }

    pub fn skipped(&self) -> usize {
        self.blocks
            .iter()
            .filter(|b| b.mode == Mode::Ignore)
            .count()
    }
}

#[derive(Debug, Clone)]
pub struct Document {
    pub chapters: Vec<Chapter>,
}

impl Document {
    /// Scan a Markdown document for its chapters and `quiver` blocks.
    ///
    /// Fences of every language are tracked, not just `quiver` ones, so that a `##` line inside
    /// a fenced block is content rather than a heading.
    pub fn parse(text: &str) -> Document {
        let mut chapters = vec![Chapter {
            title: None,
            blocks: Vec::new(),
        }];
        let mut lines = text.lines().enumerate();

        while let Some((index, line)) = lines.next() {
            let Some(fence) = opening_fence(line) else {
                if let Some(title) = line.strip_prefix("## ") {
                    chapters.push(Chapter {
                        title: Some(title.trim().to_string()),
                        blocks: Vec::new(),
                    });
                }
                continue;
            };

            let mut body = Vec::new();
            for (_, line) in lines.by_ref() {
                if closing_fence(line, fence.backticks) {
                    break;
                }
                body.push(line);
            }

            if let Some(mode) = fence.mode {
                chapters
                    .last_mut()
                    .expect("always at least one")
                    .blocks
                    .push(Block {
                        mode,
                        line: index + 2,
                        source: body.join("\n"),
                    });
            }
        }

        chapters.retain(|chapter| !chapter.blocks.is_empty());
        Document { chapters }
    }

    pub fn blocks(&self) -> impl Iterator<Item = &Block> {
        self.chapters.iter().flat_map(|c| c.blocks.iter())
    }
}

struct Fence {
    backticks: usize,
    /// `None` for a fence in another language, which is tracked but not collected.
    mode: Option<Mode>,
}

fn opening_fence(line: &str) -> Option<Fence> {
    let backticks = line.chars().take_while(|c| *c == '`').count();
    if backticks < 3 {
        return None;
    }
    let mut words = line[backticks..].split_whitespace();
    let mode = match words.next() {
        Some("quiver") => match words.next() {
            None => Some(Mode::Session),
            Some("ignore") => Some(Mode::Ignore),
            Some("program") => Some(Mode::Program),
            // An unrecognised annotation must not silently downgrade to a checked block: it
            // would report green while meaning something else. Treat it as another language.
            Some(_) => None,
        },
        _ => None,
    };
    Some(Fence { backticks, mode })
}

fn closing_fence(line: &str, backticks: usize) -> bool {
    let run = line.chars().take_while(|c| *c == '`').count();
    run >= backticks && line[run..].trim().is_empty()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splits_chapters_and_collects_blocks() {
        let doc = Document::parse(
            "# Title\n\n```quiver\n1\n```\n\n## One\n\n```quiver ignore\n...\n```\n\n\
             ```quiver program\n#{ 1 }\n```\n",
        );
        assert_eq!(doc.chapters.len(), 2);
        assert_eq!(doc.chapters[0].title, None);
        assert_eq!(doc.chapters[0].blocks[0].mode, Mode::Session);
        assert_eq!(doc.chapters[1].title.as_deref(), Some("One"));
        assert_eq!(doc.chapters[1].blocks[0].mode, Mode::Ignore);
        assert_eq!(doc.chapters[1].blocks[1].mode, Mode::Program);
        assert_eq!(doc.chapters[1].skipped(), 1);
    }

    #[test]
    fn a_heading_inside_a_fence_is_content() {
        let doc = Document::parse("```toml\n## not a heading\n```\n\n```quiver\n1\n```\n");
        assert_eq!(doc.chapters.len(), 1);
        assert_eq!(doc.chapters[0].title, None);
    }

    #[test]
    fn an_unknown_annotation_is_not_run() {
        let doc = Document::parse("```quiver no_such_thing\n1\n```\n");
        assert!(doc.chapters.is_empty());
    }

    #[test]
    fn block_lines_are_document_lines() {
        let doc = Document::parse("# T\n\n## One\n\n```quiver\nx = 1\ny = 2\n```\n");
        let steps = doc.chapters[0].blocks[0].steps().unwrap();
        assert_eq!(steps[0].line, 6);
        assert_eq!(steps[1].line, 7);
    }

    #[test]
    fn a_multiline_string_does_not_split_the_step() {
        let block = Block {
            mode: Mode::Session,
            line: 1,
            source: "msg = \"\"\"\n  a\n\n  b\n  \"\"\"\nmsg".to_string(),
        };
        let steps = block.steps().unwrap();
        assert_eq!(steps.len(), 2);
        assert_eq!(steps[1].source, "msg");
    }

    #[test]
    fn an_own_line_assertion_continues_the_step_above() {
        let block = Block {
            mode: Mode::Session,
            line: 1,
            source: "5 //= 5\n//= ('int)\n6".to_string(),
        };
        let steps = block.steps().unwrap();
        assert_eq!(steps.len(), 2);
        assert_eq!(steps[0].assertions, 2);
        assert_eq!(steps[1].source, "6");
    }

    #[test]
    fn a_failure_expectation_is_read_from_the_last_line() {
        let block = Block {
            mode: Mode::Session,
            line: 1,
            source: "5 ~> 99   //! must use the value   a prose note\nx = \"//! not this\"\n1 //!"
                .to_string(),
        };
        let steps = block.steps().unwrap();
        // Three or more spaces end the expectation and start a note, as after a `//=` pattern.
        assert_eq!(
            steps[0].expect_failure.as_deref(),
            Some("must use the value")
        );
        // Inside a string, on a line that is not the step's last: not an expectation.
        assert_eq!(steps[1].expect_failure, None);
        // Bare `//!` accepts any failure.
        assert_eq!(steps[2].expect_failure.as_deref(), Some(""));
    }

    #[test]
    fn a_type_alias_is_a_step() {
        let block = Block {
            mode: Mode::Session,
            line: 1,
            source: "'point = [x: 'int]\n[x: 1] ~> =('point)p".to_string(),
        };
        let steps = block.steps().unwrap();
        assert_eq!(steps.len(), 2);
        assert_eq!(steps[0].source, "'point = [x: 'int]");
        assert_eq!(steps[0].assertions, 0);
    }
}
