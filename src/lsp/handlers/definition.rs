//! Go to definition handler.

use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position, Url};

use crate::compiler::{Item, Module};
use crate::lsp::position::LineIndex;

/// Handle go to definition request.
///
/// Finds the definition of the symbol at the given position.
pub fn handle_goto_definition(
    module: &Module,
    line_index: &LineIndex,
    position: Position,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let offset = line_index.position_to_offset(position)?;

    // Find what's at the cursor position
    // For now, check if we're on a function call or identifier
    // and look up its definition in the module

    // Simplified: scan module for definitions
    for item in &module.items {
        match item {
            Item::Function(func) => {
                // Check if the cursor is on this function's name
                // Function span includes the whole function, but the name
                // is near the start after "fn "
                let span = &func.span;
                if offset >= span.start && offset <= span.start + 3 + func.name.len() + 5 {
                    // User is on the function definition itself
                    // Return the definition location
                    if let Some(range) = line_index.span_to_range(span.clone()) {
                        return Some(GotoDefinitionResponse::Scalar(Location {
                            uri: uri.clone(),
                            range,
                        }));
                    }
                }
            }
            Item::Struct(_) => {
                // Structs don't have spans currently
            }
            Item::Enum(_) => {
                // Enums don't have spans currently
            }
            _ => {}
        }
    }

    // TODO: Implement proper identifier resolution
    // This would require:
    // 1. Parsing the source to find what identifier is at the cursor
    // 2. Looking up that identifier in the module's scope
    // 3. Finding the definition span

    None
}
