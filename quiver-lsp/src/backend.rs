//! The tower-lsp [`LanguageServer`] implementation: document lifecycle and request routing.

use crate::analysis::{Analysis, analyze};
use crate::convert::{offset_to_position, position_to_offset, span_to_range};
use crate::documents::{DocumentStore, LineIndex};
use crate::loader::resolver_for;
use dashmap::DashMap;
use quiver_compiler::{Overlay, PackageResolver, find_project_root};
use quiver_core::format::format_type_by_id;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

pub struct Backend {
    client: Client,
    documents: DocumentStore,
    analyses: DashMap<Url, Analysis>,
    /// Open buffers overlaid on disk, shared with each per-document resolver so cross-module
    /// imports see unsaved edits.
    overlay: Arc<Overlay>,
    /// The editor's workspace root (from `initialize`). Used to bound project-wide scans so a
    /// dependency's references are found in the *consuming* project, not just the dependency's
    /// own package (whose root sits below the workspace).
    workspace_root: Mutex<Option<PathBuf>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DocumentStore::default(),
            analyses: DashMap::new(),
            overlay: Arc::new(Overlay::new()),
            workspace_root: Mutex::new(None),
        }
    }

    /// Mirror a document into the editor overlay, so other documents that import it resolve to
    /// its live content. No-op for documents with no file path (untitled buffers).
    fn overlay_set(&self, uri: &Url, text: &str) {
        if let Ok(path) = uri.to_file_path() {
            self.overlay.set(&path, text.to_string());
        }
    }

    fn overlay_remove(&self, uri: &Url) {
        if let Ok(path) = uri.to_file_path() {
            self.overlay.remove(&path);
        }
    }

    /// Re-analyse a document, cache the result, and publish its diagnostics. The parse +
    /// typecheck runs on a blocking worker so it never stalls the async executor (which also
    /// services stdin and concurrent requests).
    async fn refresh(&self, uri: Url) {
        // Snapshot the buffer out of the document store so no lock is held across the analysis.
        let Some((text, line_index, version)) = self
            .documents
            .get(&uri)
            .map(|doc| (doc.text.clone(), doc.line_index.clone(), doc.version))
        else {
            return;
        };

        let overlay = self.overlay.clone();
        let analysis_uri = uri.clone();
        let analysis = tokio::task::spawn_blocking(move || {
            let resolver = resolver_for(&analysis_uri, overlay);
            analyze(&text, &line_index, &resolver)
        })
        .await
        .expect("analysis task panicked");

        // Drop a result that a newer edit has already superseded, so stale diagnostics and a
        // stale semantic index never overwrite a fresher analysis.
        if self.documents.get(&uri).map(|doc| doc.version) != Some(version) {
            return;
        }

        let diagnostics = analysis.diagnostics.clone();
        self.analyses.insert(uri.clone(), analysis);
        self.client
            .publish_diagnostics(uri, diagnostics, Some(version))
            .await;
    }

    /// Re-analyse every open document. A change in one module can alter diagnostics in any open
    /// file that imports it, so edits to shared modules (and external file changes) fan out here.
    async fn refresh_all_open(&self) {
        for uri in self.documents.uris() {
            self.refresh(uri).await;
        }
    }

    /// Byte offset of an LSP position in a document, if the document is open.
    fn offset_at(&self, uri: &Url, position: Position) -> Option<usize> {
        let doc = self.documents.get(uri)?;
        Some(position_to_offset(&doc.text, &doc.line_index, position))
    }

    /// The current text of a file: the open buffer if one exists, otherwise the disk content.
    fn file_text(&self, uri: &Url, path: &Path) -> Option<String> {
        if let Some(doc) = self.documents.get(uri) {
            return Some(doc.text.clone());
        }
        std::fs::read_to_string(path).ok()
    }

    /// A go-to-definition response landing on `member`'s field label in module file `module`,
    /// falling back to the file top when the member's exact location can't be determined.
    fn member_location(
        &self,
        module: &Path,
        member: Option<&str>,
    ) -> Option<GotoDefinitionResponse> {
        let uri = Url::from_file_path(module).ok()?;
        let range = self
            .member_definition_range(module, member)
            .unwrap_or_default();
        Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
    }

    /// The range of `member`'s definition within the module file `module` — its field label in
    /// the module's value tuple. `None` for a bare module import, or when the member's location
    /// can't be determined (then go-to-definition falls back to the file top).
    fn member_definition_range(&self, module: &Path, member: Option<&str>) -> Option<Range> {
        let member = member?;
        let uri = Url::from_file_path(module).ok()?;
        let text = self.file_text(&uri, module)?;
        let ast = quiver_compiler::parse(&text).ok()?;
        let span = crate::symbols::module_member_span(&ast, member)?;
        Some(span_to_range(&text, &LineIndex::new(&text), span))
    }

    /// The root to scan for references to a symbol in `from_uri`: the workspace root when the
    /// file is inside it (so a dependency's references are found in the consuming project),
    /// otherwise the file's nearest project root.
    fn scan_root(&self, from_uri: &Url) -> Option<PathBuf> {
        let file = from_uri.to_file_path().ok()?;
        let canonical = std::fs::canonicalize(&file).unwrap_or_else(|_| file.clone());
        if let Some(workspace) = self.workspace_root.lock().unwrap().clone()
            && canonical.starts_with(&workspace)
        {
            return Some(workspace);
        }
        find_project_root(file.parent().unwrap_or(Path::new(".")))
    }

    /// All references across the workspace to an imported symbol: the module file `module`, and
    /// (when `member` is `Some`) that specific member. Scans every `.qv` file under the scan
    /// root, analysing each in its own package context. The scan (parse + typecheck of every
    /// project file) runs on a blocking worker so it never stalls the async executor.
    async fn project_import_references(
        &self,
        from_uri: &Url,
        module: &Path,
        member: Option<&str>,
        include_declaration: bool,
    ) -> Vec<Location> {
        let Some(root) = self.scan_root(from_uri) else {
            return Vec::new(); // No project: cross-file references aren't defined.
        };

        let overlay = self.overlay.clone();
        let open_buffers = self.documents.open_buffers();
        let module = module.to_path_buf();
        let member = member.map(str::to_string);
        tokio::task::spawn_blocking(move || {
            scan_import_references(
                &root,
                overlay,
                &open_buffers,
                &module,
                member.as_deref(),
                include_declaration,
            )
        })
        .await
        .expect("reference-scan task panicked")
    }
}

/// Scan every `.qv` file under `root` for references to an imported symbol. Pure and blocking:
/// runs off the async executor. Open buffers in `open_buffers` (keyed by canonical path) take
/// precedence over on-disk content so the scan sees unsaved edits.
fn scan_import_references(
    root: &Path,
    overlay: Arc<Overlay>,
    open_buffers: &std::collections::HashMap<PathBuf, String>,
    module: &Path,
    member: Option<&str>,
    include_declaration: bool,
) -> Vec<Location> {
    let mut locations = Vec::new();

    for file in qv_files(root) {
        let Ok(file_uri) = Url::from_file_path(&file) else {
            continue;
        };
        let canonical = std::fs::canonicalize(&file).unwrap_or_else(|_| file.clone());
        let text = match open_buffers.get(&canonical) {
            Some(text) => text.clone(),
            None => match std::fs::read_to_string(&file) {
                Ok(text) => text,
                Err(_) => continue,
            },
        };
        let index = LineIndex::new(&text);
        let resolver = PackageResolver::for_entry_file_with_overlay(&file, overlay.clone());
        let analysis = analyze(&text, &index, &resolver);
        let Some(semantics) = &analysis.semantics else {
            continue;
        };
        for span in semantics.import_references(module, member) {
            locations.push(Location {
                uri: file_uri.clone(),
                range: span_to_range(&text, &index, span),
            });
        }
    }

    // Include the module file itself as the declaration (its top — member-level precision is
    // a later refinement).
    if include_declaration && let Ok(decl_uri) = Url::from_file_path(module) {
        locations.push(Location {
            uri: decl_uri,
            range: Range::default(),
        });
    }

    locations
}

/// Every `.qv` file under `root`, recursively (skipping dot-directories).
fn qv_files(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    collect_qv_files(root, &mut out);
    out
}

fn collect_qv_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let name = entry.file_name();
        if name.to_string_lossy().starts_with('.') {
            continue;
        }
        if path.is_dir() {
            collect_qv_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "qv") {
            out.push(path);
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Remember the workspace root so project-wide scans can reach across package boundaries.
        let root = params
            .workspace_folders
            .and_then(|folders| folders.into_iter().next())
            .and_then(|folder| folder.uri.to_file_path().ok())
            .or_else(|| params.root_uri.and_then(|uri| uri.to_file_path().ok()));
        if let Some(root) = root {
            *self.workspace_root.lock().unwrap() =
                Some(std::fs::canonicalize(&root).unwrap_or(root));
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        // Ask for save notifications (text not needed: the open buffer is already
                        // current). A save is when we cheaply refresh dependents of a module.
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(false),
                        })),
                        ..Default::default()
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "quiver-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        // Watch every `.qv` file so external changes (git operations, edits in another tool) to a
        // module re-trigger diagnostics in the open files that import it. Dynamic registration;
        // clients that don't support it simply won't send the notifications.
        let registration = Registration {
            id: "quiver-watch-qv".to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
                watchers: vec![FileSystemWatcher {
                    glob_pattern: GlobPattern::String("**/*.qv".to_string()),
                    kind: None,
                }],
            })
            .ok(),
        };
        if let Err(err) = self.client.register_capability(vec![registration]).await {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("quiver-lsp: file watching unavailable ({err})"),
                )
                .await;
        }

        self.client
            .log_message(MessageType::INFO, "quiver-lsp initialized")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let doc = params.text_document;
        self.overlay_set(&doc.uri, &doc.text);
        self.documents.set(doc.uri.clone(), doc.text, doc.version);
        self.refresh(doc.uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // FULL sync: the last content change carries the entire document text.
        let Some(change) = params.content_changes.into_iter().last() else {
            return;
        };
        let uri = params.text_document.uri;
        self.overlay_set(&uri, &change.text);
        self.documents
            .set(uri.clone(), change.text, params.text_document.version);
        self.refresh(uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.overlay_remove(&uri);
        self.documents.remove(&uri);
        self.analyses.remove(&uri);
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        // A saved module may change diagnostics in any open file that imports it. `did_change`
        // only refreshes the edited document; a save is the cheap moment to fan out to dependents.
        self.refresh_all_open().await;
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        // External changes to `.qv` files (git checkout, another editor). The buffers of open
        // documents are authoritative, so just re-analyse them against the new on-disk modules.
        if !params.changes.is_empty() {
            self.refresh_all_open().await;
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(offset) = self.offset_at(&uri, position) else {
            return Ok(None);
        };
        let Some(analysis) = self.analyses.get(&uri) else {
            return Ok(None);
        };
        let (Some(semantics), Some(program)) = (&analysis.semantics, &analysis.program) else {
            return Ok(None);
        };
        let Some(info) = semantics.at_offset(offset) else {
            return Ok(None);
        };
        let type_text = format_type_by_id(program, info.type_id);
        let body = match &info.label {
            Some(label) => format!("{label}: {type_text}"),
            None => type_text,
        };
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```quiver\n{body}\n```"),
            }),
            range: None,
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(offset) = self.offset_at(&uri, position) else {
            return Ok(None);
        };
        // The local definition span, a cross-file module origin + member (import access), and
        // — for a destructured import like `(double) = %util` — the member it selects.
        let (local_definition, module_path, member, destructured_import) = {
            let Some(analysis) = self.analyses.get(&uri) else {
                return Ok(None);
            };
            let Some(semantics) = &analysis.semantics else {
                return Ok(None);
            };
            let destructured_import = semantics.import_member_at(offset);
            let Some(info) = semantics.at_offset(offset) else {
                // Not on a recorded symbol, but possibly on a destructure-import binding.
                if let Some((module, member)) = destructured_import {
                    return Ok(self.member_location(&module, Some(&member)));
                }
                return Ok(None);
            };
            (
                info.definition,
                info.definition_module.clone(),
                info.import_member.clone(),
                destructured_import,
            )
        };

        // An import access jumps into its module's file — to the member's field label.
        if let Some(path) = module_path {
            return Ok(self.member_location(&path, member.as_deref()));
        }

        // A destructured-import binding (`(double)`) is also a reference to the module member, so
        // chain onward from the binding to the member's definition rather than landing on itself.
        if let Some((module, member)) = destructured_import {
            return Ok(self.member_location(&module, Some(&member)));
        }

        let Some(definition) = local_definition else {
            return Ok(None);
        };
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };
        let range = span_to_range(&doc.text, &doc.line_index, definition);
        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range,
        })))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;
        let Some(offset) = self.offset_at(&uri, position) else {
            return Ok(None);
        };

        // Is the symbol under the cursor an imported one (references span the project) or a local
        // (references are confined to this file)?
        let import_target = {
            let Some(analysis) = self.analyses.get(&uri) else {
                return Ok(None);
            };
            let Some(semantics) = &analysis.semantics else {
                return Ok(None);
            };
            semantics.import_at(offset)
        };

        if let Some((module, member)) = import_target {
            let locations = self
                .project_import_references(&uri, &module, member.as_deref(), include_declaration)
                .await;
            return Ok(Some(locations));
        }

        // Is the cursor on a module member's *definition* (the `double:` label in this file)?
        // Then references span the project: every import of that member. Reuse the AST cached
        // by the last analysis rather than re-parsing the document.
        if let Ok(path) = uri.to_file_path() {
            let member_def = self.analyses.get(&uri).and_then(|analysis| {
                let ast = analysis.ast.as_ref()?;
                crate::symbols::module_member_at(ast, offset)
            });
            if let Some((member, label_span)) = member_def {
                let module = std::fs::canonicalize(&path).unwrap_or(path);
                let mut locations = self
                    .project_import_references(&uri, &module, Some(&member), false)
                    .await;
                if include_declaration && let Some(doc) = self.documents.get(&uri) {
                    locations.push(Location {
                        uri: uri.clone(),
                        range: span_to_range(&doc.text, &doc.line_index, label_span),
                    });
                }
                return Ok(Some(locations));
            }
        }

        // Local symbol: references within this document.
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };
        let Some(analysis) = self.analyses.get(&uri) else {
            return Ok(None);
        };
        let Some(semantics) = &analysis.semantics else {
            return Ok(None);
        };
        let locations = semantics
            .local_references(offset, include_declaration)
            .into_iter()
            .map(|span| Location {
                uri: uri.clone(),
                range: span_to_range(&doc.text, &doc.line_index, span),
            })
            .collect();
        Ok(Some(locations))
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(offset) = self.offset_at(&uri, position) else {
            return Ok(None);
        };
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };
        let Some(analysis) = self.analyses.get(&uri) else {
            return Ok(None);
        };
        let Some(semantics) = &analysis.semantics else {
            return Ok(None);
        };

        let highlight = |span, kind| DocumentHighlight {
            range: span_to_range(&doc.text, &doc.line_index, span),
            kind: Some(kind),
        };

        // An import symbol: highlight its occurrences within this file only. Document highlight is
        // single-file, so unlike `references` there's no project-wide scan.
        if let Some((module, member)) = semantics.import_at(offset) {
            let highlights = semantics
                .import_references(&module, member.as_deref())
                .into_iter()
                .map(|span| highlight(span, DocumentHighlightKind::READ))
                .collect();
            return Ok(Some(highlights));
        }

        // Local symbol: every occurrence in this document, with the binding marked as a write.
        let definition = semantics.local_definition_at(offset);
        let highlights = semantics
            .local_references(offset, true)
            .into_iter()
            .map(|span| {
                let kind = if Some(span) == definition {
                    DocumentHighlightKind::WRITE
                } else {
                    DocumentHighlightKind::READ
                };
                highlight(span, kind)
            })
            .collect();
        Ok(Some(highlights))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let Some(analysis) = self.analyses.get(&params.text_document.uri) else {
            return Ok(None);
        };
        if analysis.symbols.is_empty() {
            return Ok(None);
        }
        Ok(Some(DocumentSymbolResponse::Nested(
            analysis.symbols.clone(),
        )))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let Some(doc) = self.documents.get(&params.text_document.uri) else {
            return Ok(None);
        };
        // Formatting is purely syntactic; if the document does not parse there is nothing to format,
        // so leave it untouched (the parse error is already surfaced as a diagnostic).
        let Ok(ast) = quiver_compiler::parse(&doc.text) else {
            return Ok(None);
        };
        let formatted = quiver_compiler::format_program(&ast, &doc.text);
        if formatted == doc.text {
            return Ok(None);
        }
        // Replace the whole document with the formatted text.
        let range = Range {
            start: Position::default(),
            end: offset_to_position(&doc.text, &doc.line_index, doc.text.len()),
        };
        Ok(Some(vec![TextEdit {
            range,
            new_text: formatted,
        }]))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
