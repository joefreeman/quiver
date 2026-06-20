//! A span-indexed record of semantic facts (the type and definition of the symbol at a
//! source position), produced on demand during compilation for the language server.
//!
//! It is built by hooking the compiler's existing name/type resolution, so it reflects real
//! inference, narrowing and shadowing rather than a second, divergent resolver. Building it
//! is opt-in (pass a [`Recorder`] to `Compiler::compile`); ordinary compilation pays nothing.
//!
//! The same value is recorded into during compilation and queried (via [`Recorder::at_offset`])
//! afterwards — there is no separate finished form, because the query side is just a linear
//! scan over the recorded entries.

use crate::parser::SourceSpan;
use std::collections::HashMap;
use std::path::PathBuf;

/// What kind of symbol a reference resolves to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Parameter,
    Builtin,
    /// A module import or one of its members (`%math`, `%math.add`).
    Import,
}

/// The semantic facts about one reference: its (narrowed) type, where it is defined, what
/// kind of symbol it is, and a display label (the symbol's name, `$`, or a builtin name)
/// for richer hover text like `__add__: #['int, 'int] -> 'int`.
#[derive(Debug, Clone)]
pub struct SemanticInfo {
    pub type_id: usize,
    pub definition: Option<SourceSpan>,
    /// For imports, the file the module resolves to — its definition lives in another document,
    /// so go-to-definition jumps there rather than to a span in the current file. `None` for
    /// local symbols and for imports with no openable origin (the embedded standard library).
    pub definition_module: Option<PathBuf>,
    pub kind: SymbolKind,
    pub label: Option<String>,
}

/// Records the symbol at each source position during compilation, and answers position
/// queries (hover, go-to-definition) afterwards.
///
/// `definitions` maps a name to the span of its most recent binding; it is scratch used while
/// recording (to resolve a reference to its definition) and is unused once querying begins.
#[derive(Debug, Default)]
pub struct Recorder {
    entries: Vec<(SourceSpan, SemanticInfo)>,
    definitions: HashMap<String, SourceSpan>,
}

impl Recorder {
    /// Record that `name` is bound at `span` (a binding/definition site).
    pub fn record_definition(&mut self, name: &str, span: SourceSpan) {
        self.definitions.insert(name.to_string(), span);
    }

    /// Record a reference to a named symbol, resolving its definition from what's bound so
    /// far (by `name`). `label` is the hover text and may differ from `name` when an accessor
    /// path is involved (`foo.bar`, `$.0`).
    pub fn record_reference(
        &mut self,
        span: SourceSpan,
        name: &str,
        type_id: usize,
        kind: SymbolKind,
        label: Option<String>,
    ) {
        let definition = self.definitions.get(name).copied();
        self.entries.push((
            span,
            SemanticInfo {
                type_id,
                definition,
                definition_module: None,
                kind,
                label,
            },
        ));
    }

    /// Record a reference that has no named definition (e.g. `$`, a builtin). `label` is the
    /// optional hover label (`$`, a builtin name).
    pub fn record_typed(
        &mut self,
        span: SourceSpan,
        type_id: usize,
        kind: SymbolKind,
        label: Option<String>,
    ) {
        self.entries.push((
            span,
            SemanticInfo {
                type_id,
                definition: None,
                definition_module: None,
                kind,
                label,
            },
        ));
    }

    /// Record a reference to an imported module (or one of its members). `definition_module` is
    /// the file the import resolves to, for cross-file go-to-definition (`None` when the origin
    /// is not openable, e.g. the embedded standard library).
    pub fn record_import(
        &mut self,
        span: SourceSpan,
        type_id: usize,
        label: Option<String>,
        definition_module: Option<PathBuf>,
    ) {
        self.entries.push((
            span,
            SemanticInfo {
                type_id,
                definition: None,
                definition_module,
                kind: SymbolKind::Import,
                label,
            },
        ));
    }

    /// The semantics of the smallest recorded span containing `offset`.
    pub fn at_offset(&self, offset: usize) -> Option<&SemanticInfo> {
        self.entries
            .iter()
            .filter(|(span, _)| span.offset <= offset && offset < span.offset + span.length)
            .min_by_key(|(span, _)| span.length)
            .map(|(_, info)| info)
    }
}
