//! Document state management for the LSP server.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::RwLock;

use tower_lsp::lsp_types::Url;

use super::position::LineIndex;

/// A document being edited in the IDE.
#[derive(Debug, Clone)]
pub struct Document {
    /// Document version (incremented on each change)
    pub version: i32,
    /// Document content
    pub content: String,
    /// Line index for position conversion
    pub line_index: LineIndex,
    /// File path (if known)
    pub path: Option<PathBuf>,
}

impl Document {
    /// Create a new document from content.
    pub fn new(content: String, version: i32, path: Option<PathBuf>) -> Self {
        let line_index = LineIndex::new(&content);
        Self {
            version,
            content,
            line_index,
            path,
        }
    }

    /// Update the document content.
    pub fn update(&mut self, content: String, version: i32) {
        self.content = content;
        self.version = version;
        self.line_index = LineIndex::new(&self.content);
    }
}

/// Manager for all open documents.
#[derive(Debug, Default)]
pub struct DocumentManager {
    documents: RwLock<HashMap<Url, Document>>,
}

impl DocumentManager {
    /// Create a new document manager.
    pub fn new() -> Self {
        Self {
            documents: RwLock::new(HashMap::new()),
        }
    }

    /// Open a document.
    pub fn open(&self, uri: Url, content: String, version: i32) {
        let path = uri.to_file_path().ok();
        let doc = Document::new(content, version, path);
        self.documents.write().unwrap().insert(uri, doc);
    }

    /// Update a document's content.
    pub fn change(&self, uri: &Url, content: String, version: i32) {
        if let Some(doc) = self.documents.write().unwrap().get_mut(uri) {
            doc.update(content, version);
        }
    }

    /// Close a document.
    pub fn close(&self, uri: &Url) {
        self.documents.write().unwrap().remove(uri);
    }

    /// Get a document by URI.
    pub fn get(&self, uri: &Url) -> Option<Document> {
        self.documents.read().unwrap().get(uri).cloned()
    }
}
