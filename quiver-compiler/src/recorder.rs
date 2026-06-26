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
use std::path::{Path, PathBuf};

/// What kind of symbol a reference resolves to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Parameter,
    Builtin,
    /// A module import or one of its members (`%num`, `%num.add`).
    Import,
    /// A tuple field label (`doubled` in `[ doubled: ... ]`, a module member's name).
    Field,
    /// A constructed/operator expression whose inferred type is shown on hover: a function
    /// literal (`#`), a tuple literal, a spawn (`@`), a select (`!`), or a ripple (`~`).
    Expression,
}

/// The semantic facts about one reference: its (narrowed) type, where it is defined, what
/// kind of symbol it is, and a display label (the symbol's name, `$`, or a builtin name)
/// for richer hover text like `__integer_add__: #['int, 'int] -> 'int`.
#[derive(Debug, Clone)]
pub struct SemanticInfo {
    pub type_id: usize,
    pub definition: Option<SourceSpan>,
    /// For imports, the file the module resolves to — its definition lives in another document,
    /// so go-to-definition jumps there rather than to a span in the current file. `None` for
    /// local symbols and for imports with no openable origin (the embedded standard library).
    pub definition_module: Option<PathBuf>,
    /// For an import access, the first accessed member (`%util.double` → `"double"`); `None` for
    /// a bare module import (`%util`). With `definition_module`, this is the canonical identity
    /// used to find references to an imported symbol across the project.
    pub import_member: Option<String>,
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
    /// Destructured imports (`(double) = %util`): the span of the field that selects a module
    /// member, with the module file and member name. These aren't `entries` (the same span is
    /// also a local binding, used for hover/go-to-definition); they feed find-references so a
    /// member's uses include the destructure site and the local binding it introduces.
    import_member_refs: Vec<(SourceSpan, PathBuf, String)>,
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
                import_member: None,
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
                import_member: None,
                kind,
                label,
            },
        ));
    }

    /// Record a reference to an imported module (or one of its members). `definition_module` is
    /// the file the import resolves to (for cross-file go-to-definition); `member` is the first
    /// accessed field (`%util.double` → `"double"`), or `None` for a bare module import.
    pub fn record_import(
        &mut self,
        span: SourceSpan,
        type_id: usize,
        label: Option<String>,
        definition_module: Option<PathBuf>,
        member: Option<String>,
    ) {
        self.entries.push((
            span,
            SemanticInfo {
                type_id,
                definition: None,
                definition_module,
                import_member: member,
                kind: SymbolKind::Import,
                label,
            },
        ));
    }

    /// The semantics of the smallest recorded span containing `offset`.
    pub fn at_offset(&self, offset: usize) -> Option<&SemanticInfo> {
        self.entries
            .iter()
            .filter(|(span, _)| contains(span, offset))
            .min_by_key(|(span, _)| span.length)
            .map(|(_, info)| info)
    }

    /// The definition span of the local symbol at `offset` — whether the cursor is on a use
    /// (returns what it resolves to) or on the binding itself. `None` if the symbol at `offset`
    /// is not a local with a definition in this document (e.g. an import or builtin).
    pub fn local_definition_at(&self, offset: usize) -> Option<SourceSpan> {
        // Cursor on a reference: the smallest entry covering `offset` that has a definition.
        if let Some(def) = self
            .entries
            .iter()
            .filter(|(span, info)| contains(span, offset) && info.definition.is_some())
            .min_by_key(|(span, _)| span.length)
            .and_then(|(_, info)| info.definition)
        {
            return Some(def);
        }
        // Cursor on the binding site itself (definitions are not entries, but they are the
        // `definition` of their references).
        self.entries
            .iter()
            .filter_map(|(_, info)| info.definition)
            .find(|def| contains(def, offset))
    }

    /// All references to the local symbol at `offset`, optionally including its declaration.
    /// Empty if `offset` is not on a local symbol.
    pub fn local_references(&self, offset: usize, include_declaration: bool) -> Vec<SourceSpan> {
        let Some(def) = self.local_definition_at(offset) else {
            return Vec::new();
        };
        // Entries sharing this definition: the binding occurrence (recorded as a self-reference,
        // so it is hoverable) plus every use.
        let mut spans: Vec<SourceSpan> = self
            .entries
            .iter()
            .filter(|(_, info)| info.definition == Some(def))
            .map(|(span, _)| *span)
            .collect();
        if include_declaration {
            if !spans.contains(&def) {
                spans.push(def);
            }
        } else {
            spans.retain(|span| *span != def);
        }
        dedup_spans(spans)
    }

    /// If the symbol at `offset` is an import with an openable origin, its canonical identity:
    /// the module file and the accessed member (`None` for a bare module import).
    pub fn import_at(&self, offset: usize) -> Option<(PathBuf, Option<String>)> {
        self.entries
            .iter()
            .filter(|(span, info)| {
                contains(span, offset)
                    && info.kind == SymbolKind::Import
                    && info.definition_module.is_some()
            })
            .min_by_key(|(span, _)| span.length)
            .map(|(_, info)| {
                (
                    info.definition_module.clone().unwrap(),
                    info.import_member.clone(),
                )
            })
    }

    /// Record a destructured import: the field `member` of `module` is selected at `span`
    /// (`(double) = %util`).
    pub fn record_import_member_ref(&mut self, span: SourceSpan, module: PathBuf, member: String) {
        self.import_member_refs.push((span, module, member));
    }

    /// If `offset` is on a destructured import's field (the `double` in `(double) = %util`), its
    /// canonical identity — the module file and member name. The same span is also a local
    /// binding; this lets go-to-definition chain onward from the binding to the member it imports.
    pub fn import_member_at(&self, offset: usize) -> Option<(PathBuf, String)> {
        self.import_member_refs
            .iter()
            .find(|(span, _, _)| contains(span, offset))
            .map(|(_, module, member)| (module.clone(), member.clone()))
    }

    /// Every reference in this document to `module` (and, when `member` is `Some`, that specific
    /// member): direct accesses (`%util.double`) and destructure sites (`(double) = %util`). A
    /// `None` member matches all imports of the module. Downstream uses of a local binding that
    /// such a site introduces are references to *that local*, found by querying it directly —
    /// not folded in here.
    pub fn import_references(&self, module: &Path, member: Option<&str>) -> Vec<SourceSpan> {
        // A member query matches that accessor; a module query matches the base (`%util`)
        // occurrences — one per use — not the member accessors layered on top of them.
        let member_matches = |m: Option<&str>| match member {
            Some(q) => m == Some(q),
            None => m.is_none(),
        };

        let mut spans: Vec<SourceSpan> = self
            .entries
            .iter()
            .filter(|(_, info)| {
                info.kind == SymbolKind::Import
                    && info.definition_module.as_deref() == Some(module)
                    && member_matches(info.import_member.as_deref())
            })
            .map(|(span, _)| *span)
            .collect();

        for (span, module_path, member_name) in &self.import_member_refs {
            if module_path.as_path() == module && member_matches(Some(member_name)) {
                spans.push(*span);
            }
        }

        dedup_spans(spans)
    }
}

fn contains(span: &SourceSpan, offset: usize) -> bool {
    span.offset <= offset && offset < span.offset + span.length
}

fn dedup_spans(mut spans: Vec<SourceSpan>) -> Vec<SourceSpan> {
    spans.sort_by_key(|s| (s.offset, s.length));
    spans.dedup_by_key(|s| (s.offset, s.length));
    spans
}
