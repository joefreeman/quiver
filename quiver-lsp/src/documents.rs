//! In-memory store of open documents and a byte-offset ↔ line index.

use dashmap::DashMap;
use std::collections::HashMap;
use std::path::PathBuf;
use tower_lsp::lsp_types::Url;

/// An open text document, kept in sync with the editor via full-text updates.
pub struct Document {
    pub text: String,
    pub version: i32,
    pub line_index: LineIndex,
}

impl Document {
    pub fn new(text: String, version: i32) -> Self {
        let line_index = LineIndex::new(&text);
        Self {
            text,
            version,
            line_index,
        }
    }
}

/// The set of open documents, keyed by URI.
#[derive(Default)]
pub struct DocumentStore {
    docs: DashMap<Url, Document>,
}

impl DocumentStore {
    pub fn set(&self, uri: Url, text: String, version: i32) {
        self.docs.insert(uri, Document::new(text, version));
    }

    pub fn remove(&self, uri: &Url) {
        self.docs.remove(uri);
    }

    pub fn get(&self, uri: &Url) -> Option<dashmap::mapref::one::Ref<'_, Url, Document>> {
        self.docs.get(uri)
    }

    /// The URIs of every open document. Used to refresh dependents when a module changes.
    pub fn uris(&self) -> Vec<Url> {
        self.docs.iter().map(|entry| entry.key().clone()).collect()
    }

    /// A snapshot of every open file-backed buffer, keyed by canonical path. Passed into the
    /// off-thread project scan so it sees unsaved edits without holding any document lock.
    pub fn open_buffers(&self) -> HashMap<PathBuf, String> {
        self.docs
            .iter()
            .filter_map(|entry| {
                let path = entry.key().to_file_path().ok()?;
                let path = std::fs::canonicalize(&path).unwrap_or(path);
                Some((path, entry.value().text.clone()))
            })
            .collect()
    }
}

/// Maps byte offsets to 0-based line numbers (and back). Built once per document version.
///
/// Stores only the byte offset of each line start; UTF-16 column conversion (which LSP
/// positions require) is computed against the document text in [`crate::convert`].
#[derive(Clone)]
pub struct LineIndex {
    line_starts: Vec<usize>,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0usize];
        for (i, b) in text.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    /// The 0-based line containing `offset`.
    pub fn line_at(&self, offset: usize) -> usize {
        match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(next) => next - 1,
        }
    }

    /// The byte offset at which a 0-based line starts (clamped to the last line).
    pub fn line_start(&self, line: usize) -> usize {
        self.line_starts
            .get(line)
            .copied()
            .unwrap_or_else(|| *self.line_starts.last().unwrap())
    }
}
