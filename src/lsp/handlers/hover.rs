//! Hover handler - show type information on hover.

use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

use crate::compiler::{Function, Item, Module, Param, Pattern, Type};
use crate::lsp::lookup::{find_symbol_at_offset, SymbolInfo};
use crate::lsp::position::LineIndex;

/// Handle hover request.
pub fn handle_hover(
    module: &Module,
    line_index: &LineIndex,
    position: Position,
    stdlib_modules: &[Module],
) -> Option<Hover> {
    let offset = line_index.position_to_offset(position)?;

    // First try to find a specific symbol at the position
    if let Some(symbol) = find_symbol_at_offset(module, offset) {
        if let Some(hover) = hover_for_symbol(&symbol, module, stdlib_modules, line_index) {
            return Some(hover);
        }
    }

    // Fall back to showing context info
    for item in &module.items {
        if let Some(hover) = hover_for_item(item, offset, line_index) {
            return Some(hover);
        }
    }

    None
}

fn hover_for_symbol(
    symbol: &SymbolInfo,
    module: &Module,
    stdlib_modules: &[Module],
    line_index: &LineIndex,
) -> Option<Hover> {
    match symbol {
        SymbolInfo::Variable {
            name,
            definition_span,
            ..
        } => {
            // Try to find the variable's type from its definition
            if let Some(def_span) = definition_span {
                // Look for the let binding or parameter that defines this variable
                if let Some(type_info) = find_variable_type(module, name, def_span) {
                    let range = line_index.span_to_range(def_span.clone());
                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!("```dream\nlet {}: {}\n```", name, type_info),
                        }),
                        range,
                    });
                }
            }

            // Variable without type info
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```dream\n{}\n```\n\nLocal variable", name),
                }),
                range: None,
            })
        }

        SymbolInfo::FunctionCall {
            module: mod_path,
            name,
            call_span,
        } => {
            // Look up the function signature
            let func = if mod_path.is_empty() {
                // Local function
                find_function(module, name)
            } else {
                // Module-qualified function
                let mod_name = &mod_path[0];
                find_function_in_modules(stdlib_modules, mod_name, name)
                    .or_else(|| find_function(module, name))
            };

            if let Some(func) = func {
                let sig = format_function_signature(func);
                let range = line_index.span_to_range(call_span.clone());
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```dream\n{}\n```", sig),
                    }),
                    range,
                });
            }

            None
        }

        SymbolInfo::FunctionDef { name, span } => {
            // Show the function's own signature
            if let Some(func) = find_function(module, name) {
                let sig = format_function_signature(func);
                let range = line_index.span_to_range(span.clone());
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```dream\n{}\n```", sig),
                    }),
                    range,
                });
            }
            None
        }

        SymbolInfo::StructRef { name, span } => {
            // Look up struct definition
            if let Some(struct_def) = find_struct(module, name) {
                let fields: Vec<_> = struct_def
                    .fields
                    .iter()
                    .map(|(n, t)| format!("    {}: {}", n, format_type(&t.ty)))
                    .collect();
                let range = line_index.span_to_range(span.clone());
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!(
                            "```dream\nstruct {} {{\n{}\n}}\n```",
                            name,
                            fields.join(",\n")
                        ),
                    }),
                    range,
                });
            }
            None
        }

        SymbolInfo::FieldAccess { field, span } => {
            let range = line_index.span_to_range(span.clone());
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```dream\n.{}\n```\n\nField access", field),
                }),
                range,
            })
        }
    }
}

fn find_variable_type(
    module: &Module,
    var_name: &str,
    _def_span: &std::ops::Range<usize>,
) -> Option<String> {
    // Search through functions for the variable binding
    for item in &module.items {
        if let Item::Function(func) = item {
            // Check parameters
            for param in &func.params {
                if let Pattern::Ident(name) = &param.pattern {
                    if name == var_name {
                        return Some(format_type(&param.ty.ty));
                    }
                }
            }

            // Check let bindings in the body
            for stmt in &func.body.stmts {
                if let crate::compiler::Stmt::Let { pattern, ty, .. } = stmt {
                    if let Pattern::Ident(name) = pattern {
                        if name == var_name {
                            if let Some(t) = ty {
                                return Some(format_type(&t.ty));
                            }
                            // No explicit type annotation - could try to infer
                            return None;
                        }
                    }
                }
            }
        }
    }
    None
}

fn find_function<'a>(module: &'a Module, func_name: &str) -> Option<&'a Function> {
    for item in &module.items {
        match item {
            Item::Function(func) if func.name == func_name => return Some(func),
            Item::Impl(impl_block) => {
                for method in &impl_block.methods {
                    if method.name == func_name {
                        return Some(method);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_function_in_modules<'a>(
    modules: &'a [Module],
    mod_name: &str,
    func_name: &str,
) -> Option<&'a Function> {
    for module in modules {
        if module.name.ends_with(mod_name) || module.name == format!("dream::{}", mod_name) {
            if let Some(func) = find_function(module, func_name) {
                return Some(func);
            }
        }
    }
    None
}

fn find_struct<'a>(module: &'a Module, struct_name: &str) -> Option<&'a crate::compiler::StructDef> {
    for item in &module.items {
        if let Item::Struct(s) = item {
            if s.name == struct_name {
                return Some(s);
            }
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
            value: format!(
                "```dream\n{}\n```\n\nInside function `{}`",
                signature, func.name
            ),
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
        sig.push_str(&format_type(&ret.ty));
    }

    sig
}

fn format_param(param: &Param) -> String {
    format!(
        "{}: {}",
        format_pattern(&param.pattern),
        format_type(&param.ty.ty)
    )
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
