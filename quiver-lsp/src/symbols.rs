//! Document symbols (outline) derived directly from the parsed AST: top-level type
//! aliases and bindings. Independent of typechecking, so the outline still works when a
//! file has type errors.

use crate::convert::span_to_range;
use crate::documents::LineIndex;
use quiver_compiler::ast::{Chain, Match, Program, Statement, Term};
use quiver_compiler::parser::SourceSpan;
use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

/// A module's exported members and the spans of their field labels, when the module's value is
/// a simple tuple literal (`[ double: ..., triple: ... ]`). Empty otherwise. Used to navigate
/// onto, and find references from, a module's members.
pub fn module_members(program: &Program) -> Vec<(String, SourceSpan)> {
    // A module's value is the result of its last expression statement's last chain.
    let Some(expression) = program
        .statements
        .iter()
        .rev()
        .find_map(|statement| match statement {
            Statement::Expression(expression) => Some(expression),
            _ => None,
        })
    else {
        return Vec::new();
    };
    let Some(chain) = expression.chains.last() else {
        return Vec::new();
    };
    // Only the simple `[ ... ]` form: a single tuple term with no binding.
    if chain.match_pattern.is_some() {
        return Vec::new();
    }
    let [Term::Tuple(tuple)] = chain.terms.as_slice() else {
        return Vec::new();
    };
    tuple
        .fields
        .iter()
        .filter_map(|field| Some((field.name.clone()?, field.name_span.get()?)))
        .collect()
}

/// The span of a module's exported member `member` — its field label — for go-to-definition
/// onto an imported member. `None` unless the module's value is a simple tuple literal with
/// that named field.
pub fn module_member_span(program: &Program, member: &str) -> Option<SourceSpan> {
    module_members(program)
        .into_iter()
        .find(|(name, _)| name == member)
        .map(|(_, span)| span)
}

/// The module member whose field label covers `offset`, if the cursor is on a member definition
/// (the `double` in `double: ...`). Used to find references from a member's definition site.
pub fn module_member_at(program: &Program, offset: usize) -> Option<(String, SourceSpan)> {
    module_members(program)
        .into_iter()
        .find(|(_, span)| span.offset <= offset && offset < span.offset + span.length)
}

pub fn document_symbols(program: &Program, text: &str, index: &LineIndex) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    for statement in &program.statements {
        match statement {
            Statement::TypeAlias {
                name, name_span, ..
            } => {
                if let Some(span) = name_span.get() {
                    // The parser strips the leading `'`; restore it so the outline matches
                    // the source (`'point`).
                    let name = format!("'{name}");
                    symbols.push(symbol(name, SymbolKind::CLASS, span, text, index));
                }
            }
            Statement::Expression(expression) => {
                for chain in &expression.chains {
                    // Only simple `name = ...` bindings become symbols.
                    if let Some(Match::Identifier(name, _)) = &chain.match_pattern
                        && let Some(span) = chain.bind_span.get()
                    {
                        let kind = if is_function_binding(chain) {
                            SymbolKind::FUNCTION
                        } else {
                            SymbolKind::VARIABLE
                        };
                        symbols.push(symbol(name.clone(), kind, span, text, index));
                    }
                }
            }
            Statement::TypeImport { .. } => {}
        }
    }
    symbols
}

/// A binding whose right-hand side is a single function literal (`f = #'int { ... }`).
fn is_function_binding(chain: &Chain) -> bool {
    matches!(chain.terms.as_slice(), [Term::Function(_)])
}

fn symbol(
    name: String,
    kind: SymbolKind,
    span: SourceSpan,
    text: &str,
    index: &LineIndex,
) -> DocumentSymbol {
    let range = span_to_range(text, index, span);
    #[allow(deprecated)] // `deprecated` is a required (if deprecated) field of DocumentSymbol
    DocumentSymbol {
        name,
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    }
}
