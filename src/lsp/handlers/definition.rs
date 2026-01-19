//! Go to definition handler.

use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position, Url};

use crate::compiler::Module;
use crate::lsp::lookup::{find_symbol_at_offset, SymbolInfo};
use crate::lsp::position::LineIndex;

/// Handle go to definition request.
///
/// Finds the definition of the symbol at the given position.
pub fn handle_goto_definition(
    module: &Module,
    line_index: &LineIndex,
    position: Position,
    uri: &Url,
    stdlib_modules: &[Module],
) -> Option<GotoDefinitionResponse> {
    let offset = line_index.position_to_offset(position)?;

    // Find symbol at cursor position
    let symbol = find_symbol_at_offset(module, offset)?;

    match symbol {
        SymbolInfo::Variable {
            definition_span: Some(def_span),
            ..
        } => {
            // Variable with known definition - jump to it
            let range = line_index.span_to_range(def_span)?;
            Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            }))
        }

        SymbolInfo::FunctionCall { module: mod_path, name, .. } => {
            // Try to find the function definition
            if mod_path.is_empty() {
                // Local function call - search current module
                find_function_in_module(module, &name, line_index, uri)
            } else {
                // Module-qualified call (e.g., io::println)
                let mod_name = &mod_path[0];

                // Search stdlib modules
                for stdlib_mod in stdlib_modules {
                    // Check for module name match (could be surreal::io, io, etc.)
                    if stdlib_mod.name.ends_with(mod_name) || stdlib_mod.name == format!("surreal::{}", mod_name) {
                        if let Some(response) = find_function_in_module(stdlib_mod, &name, line_index, uri) {
                            return Some(response);
                        }
                    }
                }

                // Search current module (might be a local module)
                find_function_in_module(module, &name, line_index, uri)
            }
        }

        SymbolInfo::FunctionDef { span, .. } => {
            // Already on a function definition - return its location
            let range = line_index.span_to_range(span)?;
            Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            }))
        }

        SymbolInfo::StructRef { name, .. } => {
            // Try to find struct definition
            find_struct_in_module(module, &name, line_index, uri)
        }

        _ => None,
    }
}

/// Find a function definition in a module.
fn find_function_in_module(
    module: &Module,
    func_name: &str,
    line_index: &LineIndex,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    use crate::compiler::Item;

    for item in &module.items {
        match item {
            Item::Function(func) if func.name == func_name => {
                let range = line_index.span_to_range(func.span.clone())?;
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range,
                }));
            }
            Item::Impl(impl_block) => {
                for method in &impl_block.methods {
                    if method.name == func_name {
                        let range = line_index.span_to_range(method.span.clone())?;
                        return Some(GotoDefinitionResponse::Scalar(Location {
                            uri: uri.clone(),
                            range,
                        }));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

/// Find a struct definition in a module.
fn find_struct_in_module(
    module: &Module,
    struct_name: &str,
    line_index: &LineIndex,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    use crate::compiler::Item;

    for item in &module.items {
        if let Item::Struct(s) = item {
            if s.name == struct_name {
                let range = line_index.span_to_range(s.span.clone())?;
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range,
                }));
            }
        }
    }
    None
}
