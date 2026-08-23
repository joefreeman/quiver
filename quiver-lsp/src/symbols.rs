//! Document symbols (outline) derived directly from the parsed AST: top-level type
//! aliases and bindings. Independent of typechecking, so the outline still works when a
//! file has type errors.

use crate::convert::span_to_range;
use crate::documents::LineIndex;
use quiver_compiler::ast::{
    AccessSource, Block, Chain, FieldValue, Function, Match, Sequence, Step, StrSegment, Term,
};
use quiver_compiler::parser::SourceSpan;
use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

/// A module's exported members and the spans of their field labels, when the module's value is
/// a simple tuple literal (`[ double: ..., triple: ... ]`). Empty otherwise. Used to navigate
/// onto, and find references from, a module's members.
pub fn module_members(program: &Sequence) -> Vec<(String, SourceSpan)> {
    // A module's value is the result of its last chain step.
    let Some(chain) = program.steps.iter().rev().find_map(Step::as_chain) else {
        return Vec::new();
    };
    // Only the simple `[ ... ]` form: a single tuple term with no binding.
    if chain.binding.is_some() {
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
pub fn module_member_span(program: &Sequence, member: &str) -> Option<SourceSpan> {
    module_members(program)
        .into_iter()
        .find(|(name, _)| name == member)
        .map(|(_, span)| span)
}

/// The module member whose field label covers `offset`, if the cursor is on a member definition
/// (the `double` in `double: ...`). Used to find references from a member's definition site.
pub fn module_member_at(program: &Sequence, offset: usize) -> Option<(String, SourceSpan)> {
    module_members(program)
        .into_iter()
        .find(|(_, span)| span.offset <= offset && offset < span.offset + span.length)
}

pub fn document_symbols(program: &Sequence, text: &str, index: &LineIndex) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    for step in &program.steps {
        match step {
            Step::TypeAlias {
                name, name_span, ..
            } => {
                if let Some(span) = name_span.get() {
                    // The parser strips the leading `'`; restore it so the outline matches
                    // the source (`'point`). A nameless default-type marker shows as `'`.
                    let label = match name {
                        Some(name) => format!("'{name}"),
                        None => "'".to_string(),
                    };
                    symbols.push(symbol(label, SymbolKind::CLASS, span, text, index));
                }
            }
            Step::Chain(chain) => {
                // Only simple `name = ...` bindings become symbols.
                if let Some(Match::Identifier(name, _)) = &chain.binding
                    && let Some(span) = chain.binding_span.get()
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

// ---------------------------------------------------------------------------
// Docstrings (`:doc` annotations)
// ---------------------------------------------------------------------------

/// The `:doc` string of the binding defined at `definition` (its binding span), searching
/// nested scopes. `None` unless the binding's value is a single function literal whose
/// body carries a `:doc` annotation with a plain string value.
pub fn doc_at_definition(program: &Sequence, definition: SourceSpan) -> Option<String> {
    let mut found = None;
    visit_sequence_for_doc(program, definition, &mut found);
    found
}

/// The `:doc` string of module member `member`, when the module's value is a simple
/// tuple literal and the member's field is a function literal with a `:doc` annotation.
pub fn member_doc(program: &Sequence, member: &str) -> Option<String> {
    let chain = program.steps.iter().rev().find_map(Step::as_chain)?;
    // The module record itself, or the record flowing into an identity-plus-attach
    // annotation block (`[…] { :dialect f }`) — the block leaves the value unchanged.
    let tuple = match chain.terms.as_slice() {
        [Term::Tuple(tuple)] => tuple,
        [Term::Tuple(tuple), Term::Block(block)] if block.branches.is_empty() => tuple,
        _ => return None,
    };
    tuple
        .fields
        .iter()
        .find(|field| field.name.as_deref() == Some(member))
        .and_then(|field| match &field.value {
            FieldValue::Chain(chain) => {
                doc_of_chain(chain).or_else(|| doc_behind_reference(program, chain))
            }
            FieldValue::Spread(_) => None,
        })
}

/// The `:doc` of the top-level binding a `member: local` field refers to — annotations
/// ride the referenced closure into the module tuple, so the binding's doc is the
/// member's doc.
fn doc_behind_reference(program: &Sequence, chain: &Chain) -> Option<String> {
    let [Term::Access(access)] = chain.terms.as_slice() else {
        return None;
    };
    let Some(AccessSource::Identifier(name)) = &access.source else {
        return None;
    };
    if !access.accessors.is_empty() {
        return None;
    }
    for chain in program.steps.iter().filter_map(Step::as_chain) {
        if let Some(Match::Identifier(bound, _)) = &chain.binding
            && bound == name
        {
            return doc_of_chain(chain);
        }
    }
    None
}

fn visit_sequence_for_doc(sequence: &Sequence, definition: SourceSpan, found: &mut Option<String>) {
    for chain in sequence.chains() {
        if chain.binding_span.get() == Some(definition)
            && let Some(doc) = doc_of_chain(chain)
        {
            *found = Some(doc);
            return;
        }
        for term in &chain.terms {
            visit_term_for_doc(term, definition, found);
            if found.is_some() {
                return;
            }
        }
    }
}

fn visit_term_for_doc(term: &Term, definition: SourceSpan, found: &mut Option<String>) {
    match term {
        Term::Block(block) => visit_block_for_doc(block, definition, found),
        Term::Function(function) => {
            if let Some(body) = &function.body {
                visit_block_for_doc(body, definition, found);
            }
        }
        Term::Spawn(inner, argument, _) => {
            visit_term_for_doc(inner, definition, found);
            if let Some(argument) = argument {
                visit_term_for_doc(argument, definition, found);
            }
        }
        Term::Apply(_, argument) => visit_term_for_doc(argument, definition, found),
        Term::Tuple(tuple) => {
            for field in &tuple.fields {
                if let FieldValue::Chain(chain) = &field.value {
                    if chain.binding_span.get() == Some(definition)
                        && let Some(doc) = doc_of_chain(chain)
                    {
                        *found = Some(doc);
                        return;
                    }
                    for term in &chain.terms {
                        visit_term_for_doc(term, definition, found);
                    }
                }
            }
        }
        _ => {}
    }
}

fn visit_block_for_doc(block: &Block, definition: SourceSpan, found: &mut Option<String>) {
    for branch in &block.branches {
        visit_sequence_for_doc(&branch.condition, definition, found);
        if found.is_some() {
            return;
        }
        if let Some(consequence) = &branch.consequence {
            visit_sequence_for_doc(consequence, definition, found);
            if found.is_some() {
                return;
            }
        }
    }
}

/// The chain's `:doc` text: a single function literal with a doc prefix, or a chain
/// ending in a block attach (`expr ~> { :doc "..." }` — e.g. an annotated builtin export
/// like `__binary_and__ ~> { :doc … }`). Plain-string docs only.
fn doc_of_chain(chain: &Chain) -> Option<String> {
    match chain.terms.as_slice() {
        [Term::Function(function)] => doc_of_function(function),
        [.., Term::Block(block)] => doc_of_block(block),
        _ => None,
    }
}

fn doc_of_function(function: &Function) -> Option<String> {
    doc_of_block(function.body.as_ref()?)
}

fn doc_of_block(body: &Block) -> Option<String> {
    let annotation = body
        .annotations
        .iter()
        .find(|annotation| annotation.name == "doc")?;
    let [Term::String(_, segments)] = annotation.value.terms.as_slice() else {
        return None;
    };
    let mut out = String::new();
    for segment in segments {
        match segment {
            StrSegment::Text(bytes) => out.push_str(std::str::from_utf8(bytes).ok()?),
            // An interpolated docstring has no static text; skip it.
            StrSegment::Hole(_) => return None,
        }
    }
    Some(out)
}

#[cfg(test)]
mod doc_tests {
    use super::*;

    fn binding_span_of(program: &Sequence, name: &str) -> SourceSpan {
        for chain in program.steps.iter().filter_map(Step::as_chain) {
            if let Some(Match::Identifier(bound, _)) = &chain.binding
                && bound == name
            {
                return chain.binding_span.get().expect("bind span");
            }
        }
        panic!("binding {name} not found");
    }

    #[test]
    fn doc_of_a_local_function_binding() {
        let source =
            "double = #'int {\n  :doc \"Doubles an integer.\"\n  [~, 2] ~> %num.mul ~\n}\n";
        let ast = quiver_compiler::parse(source).expect("parse");
        let definition = binding_span_of(&ast, "double");
        assert_eq!(
            doc_at_definition(&ast, definition).as_deref(),
            Some("Doubles an integer.")
        );
    }

    #[test]
    fn no_doc_yields_none() {
        let source = "double = #'int { [~, 2] ~> %num.mul ~ }\n";
        let ast = quiver_compiler::parse(source).expect("parse");
        let definition = binding_span_of(&ast, "double");
        assert_eq!(doc_at_definition(&ast, definition), None);
    }

    #[test]
    fn doc_of_a_module_member() {
        let source =
            "[\n  greet: #'int {\n    :doc \"Greets.\"\n    [~, 1] ~> %num.add ~\n  },\n]\n";
        let ast = quiver_compiler::parse(source).expect("parse");
        assert_eq!(member_doc(&ast, "greet").as_deref(), Some("Greets."));
        assert_eq!(member_doc(&ast, "missing"), None);
    }
}

#[cfg(test)]
mod reference_doc_tests {
    use super::*;

    #[test]
    fn member_doc_chases_a_local_reference() {
        let source = "\
floor = #'int {\n  :doc \"Rounds down.\"\n  $\n}\n\n[\n  floor: floor,\n]\n";
        let ast = quiver_compiler::parse(source).expect("parse");
        assert_eq!(member_doc(&ast, "floor").as_deref(), Some("Rounds down."));
    }
}
