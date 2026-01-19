//! LSP request handlers.

pub mod completion;
pub mod definition;
pub mod diagnostics;
pub mod hover;
pub mod references;
pub mod semantic_tokens;

pub use completion::handle_completion;
pub use definition::handle_goto_definition;
pub use diagnostics::publish_diagnostics;
pub use hover::handle_hover;
pub use references::handle_references;
pub use semantic_tokens::{get_legend, handle_semantic_tokens_full};
