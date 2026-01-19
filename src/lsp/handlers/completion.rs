//! Completion handler - provide code completions.

use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
};

use crate::compiler::{Item, Module, Pattern, Type};
use crate::lsp::position::LineIndex;

/// Dream language keywords.
const KEYWORDS: &[&str] = &[
    "fn", "let", "mut", "if", "else", "match", "return", "true", "false", "struct", "enum",
    "impl", "trait", "pub", "mod", "use", "spawn", "receive", "self", "for", "in", "while",
    "loop", "break", "continue", "type", "extern", "when", "where",
];

/// Handle completion request.
pub fn handle_completion(
    module: &Module,
    stdlib_modules: &[Module],
    line_index: &LineIndex,
    params: &CompletionParams,
    source: &str,
) -> Option<CompletionResponse> {
    let position = params.text_document_position.position;
    let offset = line_index.position_to_offset(position)?;

    // Find the word being typed
    let (_word_start, word) = find_word_at_offset(source, offset);

    let mut completions = Vec::new();

    // Check if completing after `::`
    if word.contains("::") {
        let parts: Vec<&str> = word.rsplitn(2, "::").collect();
        if parts.len() == 2 {
            let func_prefix = parts[0];
            let module_name = parts[1];

            // Search in stdlib modules
            for m in stdlib_modules {
                if m.name == module_name || m.name.ends_with(&format!("::{}", module_name)) {
                    add_module_functions(&mut completions, m, func_prefix);
                }
            }

            // Search in current module's imports
            // For now, just look at function names
            if module_name == module.name || module_name.is_empty() {
                add_module_functions(&mut completions, module, func_prefix);
            }

            return Some(CompletionResponse::Array(completions));
        }
    }

    // Keyword completions
    for kw in KEYWORDS {
        if kw.starts_with(word) && !word.is_empty() {
            completions.push(CompletionItem {
                label: kw.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }
    }

    // Local function completions
    add_module_functions(&mut completions, module, word);

    // Stdlib module name completions (with :: suffix)
    for m in stdlib_modules {
        let name = m.name.strip_prefix("dream::").unwrap_or(&m.name);
        if name.starts_with(word) && !word.is_empty() {
            completions.push(CompletionItem {
                label: format!("{}::", name),
                kind: Some(CompletionItemKind::MODULE),
                insert_text: Some(format!("{}::", name)),
                ..Default::default()
            });
        }
    }

    // Type completions (structs, enums)
    for item in &module.items {
        match item {
            Item::Struct(s) => {
                if s.name.starts_with(word) && !word.is_empty() {
                    completions.push(CompletionItem {
                        label: s.name.clone(),
                        kind: Some(CompletionItemKind::STRUCT),
                        ..Default::default()
                    });
                }
            }
            Item::Enum(e) => {
                if e.name.starts_with(word) && !word.is_empty() {
                    completions.push(CompletionItem {
                        label: e.name.clone(),
                        kind: Some(CompletionItemKind::ENUM),
                        ..Default::default()
                    });
                }
            }
            _ => {}
        }
    }

    // Stdlib types
    for m in stdlib_modules {
        for item in &m.items {
            match item {
                Item::Struct(s) => {
                    if s.name.starts_with(word) && !word.is_empty() {
                        completions.push(CompletionItem {
                            label: s.name.clone(),
                            kind: Some(CompletionItemKind::STRUCT),
                            detail: Some(format!("from {}", m.name)),
                            ..Default::default()
                        });
                    }
                }
                Item::Enum(e) => {
                    if e.name.starts_with(word) && !word.is_empty() {
                        completions.push(CompletionItem {
                            label: e.name.clone(),
                            kind: Some(CompletionItemKind::ENUM),
                            detail: Some(format!("from {}", m.name)),
                            ..Default::default()
                        });
                    }
                }
                _ => {}
            }
        }
    }

    Some(CompletionResponse::Array(completions))
}

fn add_module_functions(completions: &mut Vec<CompletionItem>, module: &Module, prefix: &str) {
    for item in &module.items {
        if let Item::Function(func) = item {
            if func.name.starts_with(prefix) && (prefix.is_empty() || !func.name.starts_with("_")) {
                // Format parameters
                let params_str: Vec<String> = func
                    .params
                    .iter()
                    .map(|p| format!("{}: {}", format_pattern(&p.pattern), format_type(&p.ty)))
                    .collect();

                let ret_str = func
                    .return_type
                    .as_ref()
                    .map(|t| format!(" -> {}", format_type(t)))
                    .unwrap_or_default();

                let detail = format!("fn({}){}", params_str.join(", "), ret_str);

                completions.push(CompletionItem {
                    label: func.name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(detail),
                    ..Default::default()
                });
            }
        }
    }
}

fn find_word_at_offset(source: &str, offset: usize) -> (usize, &str) {
    let bytes = source.as_bytes();
    let mut start = offset;

    // Walk backward to find word start
    while start > 0 {
        let c = bytes[start - 1] as char;
        if c.is_alphanumeric() || c == '_' || c == ':' {
            start -= 1;
        } else {
            break;
        }
    }

    // Return the word from start to offset
    let word = &source[start..offset];
    (start, word)
}

fn format_pattern(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Ident(name) => name.clone(),
        Pattern::Wildcard => "_".to_string(),
        _ => "...".to_string(),
    }
}

fn format_type(ty: &Type) -> String {
    match ty {
        Type::Named { name, type_args } => {
            if type_args.is_empty() {
                name.clone()
            } else {
                let args: Vec<_> = type_args.iter().map(format_type).collect();
                format!("{}<{}>", name, args.join(", "))
            }
        }
        Type::TypeVar(name) => name.clone(),
        Type::Tuple(types) => {
            let types_str: Vec<_> = types.iter().map(format_type).collect();
            format!("({})", types_str.join(", "))
        }
        Type::List(inner) => format!("[{}]", format_type(inner)),
        Type::Fn { params, ret } => {
            let params_str: Vec<_> = params.iter().map(format_type).collect();
            format!("fn({}) -> {}", params_str.join(", "), format_type(ret))
        }
        Type::Int => "Int".to_string(),
        Type::String => "String".to_string(),
        Type::Atom => "Atom".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "()".to_string(),
        Type::Any => "Any".to_string(),
        _ => format!("{:?}", ty),
    }
}
