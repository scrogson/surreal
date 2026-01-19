//! Hover handler - show type information on hover.

use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

use crate::compiler::{Function, Item, Module, Param, Pattern, Type};
use crate::lsp::position::LineIndex;

/// Handle hover request.
pub fn handle_hover(
    module: &Module,
    line_index: &LineIndex,
    position: Position,
) -> Option<Hover> {
    let offset = line_index.position_to_offset(position)?;

    // Find item at position
    for item in &module.items {
        if let Some(hover) = hover_for_item(item, offset, line_index) {
            return Some(hover);
        }
    }

    None
}

fn hover_for_item(item: &Item, offset: usize, line_index: &LineIndex) -> Option<Hover> {
    match item {
        Item::Function(func) => hover_for_function(func, offset, line_index),
        Item::Struct(_s) => None,
        Item::Enum(_e) => None,
        Item::TypeAlias(_t) => None,
        _ => None,
    }
}

fn hover_for_function(func: &Function, offset: usize, line_index: &LineIndex) -> Option<Hover> {
    let span = &func.span;

    // Check if offset is within function span
    if offset < span.start || offset > span.end {
        return None;
    }

    // For now, show the function signature whenever hovering anywhere in the function
    // This gives useful context even if we can't identify the specific symbol
    let signature = format_function_signature(func);
    let range = line_index.span_to_range(span.clone());

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("```dream\n{}\n```\n\nInside function `{}`", signature, func.name),
        }),
        range,
    })
}


fn format_function_signature(func: &Function) -> String {
    let mut sig = String::new();

    if func.is_pub {
        sig.push_str("pub ");
    }
    sig.push_str("fn ");
    sig.push_str(&func.name);

    if !func.type_params.is_empty() {
        sig.push('<');
        for (i, tp) in func.type_params.iter().enumerate() {
            if i > 0 {
                sig.push_str(", ");
            }
            sig.push_str(&tp.name);
            if !tp.bounds.is_empty() {
                sig.push_str(": ");
                for (j, bound) in tp.bounds.iter().enumerate() {
                    if j > 0 {
                        sig.push_str(" + ");
                    }
                    sig.push_str(bound);
                }
            }
        }
        sig.push('>');
    }

    sig.push('(');
    for (i, param) in func.params.iter().enumerate() {
        if i > 0 {
            sig.push_str(", ");
        }
        sig.push_str(&format_param(param));
    }
    sig.push(')');

    if let Some(ref ret) = func.return_type {
        sig.push_str(" -> ");
        sig.push_str(&format_type(ret));
    }

    sig
}

fn format_param(param: &Param) -> String {
    format!("{}: {}", format_pattern(&param.pattern), format_type(&param.ty))
}

fn format_pattern(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Ident(name) => name.clone(),
        Pattern::Wildcard => "_".to_string(),
        Pattern::Tuple(elements) => {
            let elems: Vec<_> = elements.iter().map(format_pattern).collect();
            format!("({})", elems.join(", "))
        }
        Pattern::Struct { name, fields } => {
            let fields_str: Vec<_> = fields
                .iter()
                .map(|(n, p)| format!("{}: {}", n, format_pattern(p)))
                .collect();
            format!("{} {{ {} }}", name, fields_str.join(", "))
        }
        Pattern::List(elements) => {
            let elems: Vec<_> = elements.iter().map(format_pattern).collect();
            format!("[{}]", elems.join(", "))
        }
        Pattern::Int(n) => n.to_string(),
        Pattern::String(s) => format!("\"{}\"", s),
        Pattern::Atom(a) => format!(":{}", a),
        Pattern::Bool(b) => b.to_string(),
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
        Type::Union(types) => {
            let types_str: Vec<_> = types.iter().map(format_type).collect();
            types_str.join(" | ")
        }
        Type::Int => "Int".to_string(),
        Type::String => "String".to_string(),
        Type::Atom => "Atom".to_string(),
        Type::AtomLiteral(s) => format!(":{}", s),
        Type::Bool => "Bool".to_string(),
        Type::Float => "Float".to_string(),
        Type::Unit => "()".to_string(),
        Type::Pid => "Pid".to_string(),
        Type::Ref => "Ref".to_string(),
        Type::Binary => "Binary".to_string(),
        Type::Map => "Map".to_string(),
        Type::Any => "Any".to_string(),
        Type::AssociatedType { base, name } => format!("{}::{}", base, name),
    }
}
