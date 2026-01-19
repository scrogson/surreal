//! Semantic tokens handler for syntax highlighting.
//!
//! This module provides `textDocument/semanticTokens/full` support for Dream code.
//! It enables type-aware syntax highlighting in editors by distinguishing between
//! different types of identifiers (variables, functions, types, etc.).

use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensLegend, SemanticTokensResult,
};

use crate::compiler::{
    Block, EnumPatternFields, EnumVariantArgs, Expr, ForClause, Function, Item, MatchArm, Module,
    Pattern, Span, SpannedExpr, Stmt, Type,
};
use crate::lsp::position::LineIndex;

// Token type indices (must match legend order)
const TYPE_NAMESPACE: u32 = 0;
const TYPE_TYPE: u32 = 1;
const TYPE_STRUCT: u32 = 2;
const TYPE_ENUM: u32 = 3;
const TYPE_INTERFACE: u32 = 4;
const TYPE_FUNCTION: u32 = 5;
const TYPE_VARIABLE: u32 = 6;
const TYPE_PARAMETER: u32 = 7;
const TYPE_PROPERTY: u32 = 8;
const TYPE_ENUM_MEMBER: u32 = 9;
const TYPE_STRING: u32 = 10;
const TYPE_NUMBER: u32 = 11;
const TYPE_KEYWORD: u32 = 12;

// Modifier bits
const MOD_DECLARATION: u32 = 0x01;
const MOD_DEFINITION: u32 = 0x02;
const MOD_READONLY: u32 = 0x04;
const MOD_STATIC: u32 = 0x08;

/// Get the semantic tokens legend for capability registration.
pub fn get_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::NAMESPACE,   // 0 - module names
            SemanticTokenType::TYPE,        // 1 - type names
            SemanticTokenType::STRUCT,      // 2 - struct definitions
            SemanticTokenType::ENUM,        // 3 - enum definitions
            SemanticTokenType::INTERFACE,   // 4 - trait definitions
            SemanticTokenType::FUNCTION,    // 5 - function names
            SemanticTokenType::VARIABLE,    // 6 - variable names
            SemanticTokenType::PARAMETER,   // 7 - function parameters
            SemanticTokenType::PROPERTY,    // 8 - struct fields
            SemanticTokenType::ENUM_MEMBER, // 9 - enum variants
            SemanticTokenType::STRING,      // 10 - string literals
            SemanticTokenType::NUMBER,      // 11 - numeric literals
            SemanticTokenType::KEYWORD,     // 12 - keywords (for atoms)
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::STATIC,
            SemanticTokenModifier::DEFAULT_LIBRARY,
        ],
    }
}

/// A raw token before delta encoding.
#[derive(Debug)]
struct RawToken {
    line: u32,
    col: u32,
    length: u32,
    token_type: u32,
    modifiers: u32,
}

/// Collector for semantic tokens.
struct TokenCollector {
    tokens: Vec<RawToken>,
    line_index: LineIndex,
    source: String,
}

impl TokenCollector {
    fn new(line_index: LineIndex, source: String) -> Self {
        Self {
            tokens: Vec::new(),
            line_index,
            source,
        }
    }

    /// Add a token at the given span.
    fn add_token(&mut self, span: &Span, token_type: u32, modifiers: u32) {
        // Skip invalid spans
        if span.start >= span.end {
            return;
        }

        // Convert span to position
        if let Some(range) = self.line_index.span_to_range(span.clone()) {
            self.tokens.push(RawToken {
                line: range.start.line,
                col: range.start.character,
                length: (span.end - span.start) as u32,
                token_type,
                modifiers,
            });
        }
    }

    /// Add a token for an identifier at a calculated position.
    fn add_token_for_name(&mut self, name: &str, start_offset: usize, token_type: u32, modifiers: u32) {
        let span = start_offset..start_offset + name.len();
        self.add_token(&span, token_type, modifiers);
    }

    /// Convert collected tokens to delta-encoded SemanticTokens.
    fn into_semantic_tokens(mut self) -> SemanticTokens {
        // Sort by position (line, then column)
        self.tokens.sort_by(|a, b| {
            a.line.cmp(&b.line).then(a.col.cmp(&b.col))
        });

        // Delta encode
        let mut data = Vec::with_capacity(self.tokens.len() * 5);
        let mut prev_line = 0u32;
        let mut prev_col = 0u32;

        for token in self.tokens {
            let delta_line = token.line - prev_line;
            let delta_col = if delta_line == 0 {
                token.col - prev_col
            } else {
                token.col
            };

            data.push(SemanticToken {
                delta_line,
                delta_start: delta_col,
                length: token.length,
                token_type: token.token_type,
                token_modifiers_bitset: token.modifiers,
            });

            prev_line = token.line;
            prev_col = token.col;
        }

        SemanticTokens {
            result_id: None,
            data,
        }
    }

    /// Find the byte offset of a substring within a span.
    fn find_name_in_span(&self, name: &str, span: &Span) -> Option<usize> {
        let source_slice = self.source.get(span.start..span.end)?;
        source_slice.find(name).map(|pos| span.start + pos)
    }
}

/// Handle semantic tokens full request.
pub fn handle_semantic_tokens_full(
    module: &Module,
    line_index: &LineIndex,
    source: &str,
) -> Option<SemanticTokensResult> {
    let mut collector = TokenCollector::new(line_index.clone(), source.to_string());

    for item in &module.items {
        collect_item_tokens(&mut collector, item);
    }

    Some(SemanticTokensResult::Tokens(collector.into_semantic_tokens()))
}

fn collect_item_tokens(collector: &mut TokenCollector, item: &Item) {
    match item {
        Item::Function(func) => collect_function_tokens(collector, func, true),
        Item::Struct(s) => {
            // Struct name token
            if let Some(name_offset) = collector.find_name_in_span(&s.name, &s.span) {
                collector.add_token_for_name(
                    &s.name,
                    name_offset,
                    TYPE_STRUCT,
                    MOD_DECLARATION | MOD_DEFINITION,
                );
            }
            // Struct fields
            for (field_name, field_type) in &s.fields {
                // Field names in struct definition
                // We don't have exact field spans, so we search in the struct span
                if let Some(field_offset) = collector.find_name_in_span(field_name, &s.span) {
                    collector.add_token_for_name(field_name, field_offset, TYPE_PROPERTY, MOD_DECLARATION);
                }
                // Field type
                collect_type_tokens(collector, &field_type.ty, &field_type.span);
            }
        }
        Item::Enum(e) => {
            // Enum name token
            if let Some(name_offset) = collector.find_name_in_span(&e.name, &e.span) {
                collector.add_token_for_name(
                    &e.name,
                    name_offset,
                    TYPE_ENUM,
                    MOD_DECLARATION | MOD_DEFINITION,
                );
            }
            // Enum variants
            for variant in &e.variants {
                if let Some(variant_offset) = collector.find_name_in_span(&variant.name, &e.span) {
                    collector.add_token_for_name(
                        &variant.name,
                        variant_offset,
                        TYPE_ENUM_MEMBER,
                        MOD_DECLARATION,
                    );
                }
            }
        }
        Item::Trait(t) => {
            // Trait name token
            if let Some(name_offset) = collector.find_name_in_span(&t.name, &t.span) {
                collector.add_token_for_name(
                    &t.name,
                    name_offset,
                    TYPE_INTERFACE,
                    MOD_DECLARATION | MOD_DEFINITION,
                );
            }
            // Trait methods
            for method in &t.methods {
                if let Some(body) = &method.body {
                    // Method with default implementation
                    if let Some(name_offset) = collector.find_name_in_span(&method.name, &body.span) {
                        collector.add_token_for_name(&method.name, name_offset, TYPE_FUNCTION, MOD_DECLARATION);
                    }
                }
            }
        }
        Item::Impl(impl_block) => {
            // Type name being implemented
            if let Some(name_offset) = collector.find_name_in_span(&impl_block.type_name, &impl_block.span) {
                collector.add_token_for_name(&impl_block.type_name, name_offset, TYPE_TYPE, 0);
            }
            // Methods
            for method in &impl_block.methods {
                collect_function_tokens(collector, method, false);
            }
        }
        Item::TraitImpl(trait_impl) => {
            // Trait name
            if let Some(name_offset) = collector.find_name_in_span(&trait_impl.trait_name, &trait_impl.span) {
                collector.add_token_for_name(&trait_impl.trait_name, name_offset, TYPE_INTERFACE, 0);
            }
            // Type name
            if let Some(name_offset) = collector.find_name_in_span(&trait_impl.type_name, &trait_impl.span) {
                collector.add_token_for_name(&trait_impl.type_name, name_offset, TYPE_TYPE, 0);
            }
            // Methods
            for method in &trait_impl.methods {
                collect_function_tokens(collector, method, false);
            }
        }
        Item::TypeAlias(_) | Item::ModDecl(_) | Item::Use(_) | Item::TraitDecl(_) | Item::ExternMod(_) => {
            // These don't need special handling for now
        }
    }
}

fn collect_function_tokens(collector: &mut TokenCollector, func: &Function, is_top_level: bool) {
    // Function name
    if let Some(name_offset) = collector.find_name_in_span(&func.name, &func.span) {
        let modifiers = MOD_DECLARATION | MOD_DEFINITION | if is_top_level && func.is_pub { MOD_STATIC } else { 0 };
        collector.add_token_for_name(&func.name, name_offset, TYPE_FUNCTION, modifiers);
    }

    // Parameters
    for param in &func.params {
        collect_param_tokens(collector, param, &func.span);
    }

    // Return type
    if let Some(ret_type) = &func.return_type {
        collect_type_tokens(collector, &ret_type.ty, &ret_type.span);
    }

    // Function body
    collect_block_tokens(collector, &func.body);
}

fn collect_param_tokens(collector: &mut TokenCollector, param: &crate::compiler::Param, func_span: &Span) {
    // Parameter name from pattern
    collect_pattern_tokens(collector, &param.pattern, func_span, true);

    // Parameter type
    collect_type_tokens(collector, &param.ty.ty, &param.ty.span);
}

fn collect_pattern_tokens(collector: &mut TokenCollector, pattern: &Pattern, context_span: &Span, is_param: bool) {
    match pattern {
        Pattern::Ident(name) => {
            if let Some(name_offset) = collector.find_name_in_span(name, context_span) {
                let token_type = if is_param { TYPE_PARAMETER } else { TYPE_VARIABLE };
                collector.add_token_for_name(name, name_offset, token_type, MOD_DECLARATION);
            }
        }
        Pattern::Tuple(elements) | Pattern::List(elements) => {
            for elem in elements {
                collect_pattern_tokens(collector, elem, context_span, is_param);
            }
        }
        Pattern::ListCons { head, tail } => {
            collect_pattern_tokens(collector, head, context_span, is_param);
            collect_pattern_tokens(collector, tail, context_span, is_param);
        }
        Pattern::Struct { name, fields } => {
            // Struct name
            if let Some(name_offset) = collector.find_name_in_span(name, context_span) {
                collector.add_token_for_name(name, name_offset, TYPE_STRUCT, 0);
            }
            // Fields
            for (field_name, field_pattern) in fields {
                if let Some(field_offset) = collector.find_name_in_span(field_name, context_span) {
                    collector.add_token_for_name(field_name, field_offset, TYPE_PROPERTY, 0);
                }
                collect_pattern_tokens(collector, field_pattern, context_span, is_param);
            }
        }
        Pattern::Enum { name, variant, fields } => {
            // Enum/Type name
            if let Some(name_offset) = collector.find_name_in_span(name, context_span) {
                collector.add_token_for_name(name, name_offset, TYPE_ENUM, 0);
            }
            // Variant name
            if let Some(variant_offset) = collector.find_name_in_span(variant, context_span) {
                collector.add_token_for_name(variant, variant_offset, TYPE_ENUM_MEMBER, 0);
            }
            // Field patterns
            match fields {
                EnumPatternFields::Tuple(pats) => {
                    for pat in pats {
                        collect_pattern_tokens(collector, pat, context_span, is_param);
                    }
                }
                EnumPatternFields::Struct(field_pats) => {
                    for (_, pat) in field_pats {
                        collect_pattern_tokens(collector, pat, context_span, is_param);
                    }
                }
                EnumPatternFields::Unit => {}
            }
        }
        Pattern::Int(_) => {
            // We'd need the exact span for the integer literal
        }
        Pattern::String(_) => {
            // We'd need the exact span for the string literal
        }
        Pattern::Atom(atom) => {
            // Atoms are like keywords/constants
            if let Some(offset) = collector.find_name_in_span(atom, context_span) {
                collector.add_token_for_name(atom, offset, TYPE_KEYWORD, MOD_READONLY);
            }
        }
        Pattern::Bool(_) | Pattern::Charlist(_) | Pattern::Wildcard | Pattern::BitString(_) => {
            // These don't need special highlighting
        }
    }
}

fn collect_type_tokens(collector: &mut TokenCollector, ty: &Type, span: &Span) {
    // Skip dummy spans
    if span.start == 0 && span.end == 0 {
        return;
    }

    match ty {
        Type::Named { name, type_args } => {
            if let Some(name_offset) = collector.find_name_in_span(name, span) {
                collector.add_token_for_name(name, name_offset, TYPE_TYPE, 0);
            }
            for type_arg in type_args {
                // Type args don't have individual spans, so we can't highlight them precisely
                // We'd need more span info in the AST
                collect_type_tokens(collector, type_arg, span);
            }
        }
        Type::TypeVar(name) => {
            if let Some(name_offset) = collector.find_name_in_span(name, span) {
                collector.add_token_for_name(name, name_offset, TYPE_TYPE, 0);
            }
        }
        Type::Tuple(types) => {
            for t in types {
                collect_type_tokens(collector, t, span);
            }
        }
        Type::List(inner) => {
            collect_type_tokens(collector, inner, span);
        }
        Type::Fn { params, ret } => {
            for p in params {
                collect_type_tokens(collector, p, span);
            }
            collect_type_tokens(collector, ret, span);
        }
        Type::Union(types) => {
            for t in types {
                collect_type_tokens(collector, t, span);
            }
        }
        Type::AssociatedType { base, name } => {
            // Base and associated type name
            if let Some(base_offset) = collector.find_name_in_span(base, span) {
                collector.add_token_for_name(base, base_offset, TYPE_TYPE, 0);
            }
            if let Some(name_offset) = collector.find_name_in_span(name, span) {
                collector.add_token_for_name(name, name_offset, TYPE_TYPE, 0);
            }
        }
        // Primitive types don't need extra highlighting - they're keywords
        Type::Int | Type::String | Type::Atom | Type::Bool | Type::Float |
        Type::Unit | Type::Pid | Type::Ref | Type::Binary | Type::Map | Type::Any |
        Type::AtomLiteral(_) => {}
    }
}

fn collect_block_tokens(collector: &mut TokenCollector, block: &Block) {
    for stmt in &block.stmts {
        collect_stmt_tokens(collector, stmt);
    }

    if let Some(expr) = &block.expr {
        collect_expr_tokens(collector, expr);
    }
}

fn collect_stmt_tokens(collector: &mut TokenCollector, stmt: &Stmt) {
    match stmt {
        Stmt::Let { pattern, ty, value, else_block, span } => {
            // Pattern bindings (variables)
            collect_pattern_tokens(collector, pattern, span, false);

            // Type annotation
            if let Some(type_annotation) = ty {
                collect_type_tokens(collector, &type_annotation.ty, &type_annotation.span);
            }

            // Value expression
            collect_expr_tokens(collector, value);

            // Else block
            if let Some(else_blk) = else_block {
                collect_block_tokens(collector, else_blk);
            }
        }
        Stmt::Expr { expr, .. } => {
            collect_expr_tokens(collector, expr);
        }
    }
}

fn collect_expr_tokens(collector: &mut TokenCollector, expr: &SpannedExpr) {
    match &expr.expr {
        Expr::Ident(name) => {
            // Variable reference
            collector.add_token_for_name(name, expr.span.start, TYPE_VARIABLE, 0);
        }

        Expr::Int(_) => {
            collector.add_token(&expr.span, TYPE_NUMBER, 0);
        }

        Expr::String(_) | Expr::Charlist(_) => {
            collector.add_token(&expr.span, TYPE_STRING, 0);
        }

        Expr::StringInterpolation(parts) => {
            // For interpolated strings, the outer span is a string
            // but we should highlight the embedded expressions
            for part in parts {
                if let crate::compiler::StringPart::Expr(inner_expr) = part {
                    collect_expr_tokens(collector, inner_expr);
                }
            }
        }

        Expr::Atom(atom) => {
            // Atoms are like constant keywords
            collector.add_token(&expr.span, TYPE_KEYWORD, MOD_READONLY);
            let _ = atom; // used
        }

        Expr::Bool(_) => {
            collector.add_token(&expr.span, TYPE_KEYWORD, MOD_READONLY);
        }

        Expr::Path { segments } => {
            // Module path like io::println or just println
            if segments.len() >= 2 {
                // Module segments (all but last)
                let mut current_offset = expr.span.start;
                for (i, segment) in segments.iter().enumerate() {
                    if i < segments.len() - 1 {
                        // Module/namespace
                        collector.add_token_for_name(segment, current_offset, TYPE_NAMESPACE, 0);
                        current_offset += segment.len() + 2; // + "::"
                    } else {
                        // Last segment is function
                        collector.add_token_for_name(segment, current_offset, TYPE_FUNCTION, 0);
                    }
                }
            } else if segments.len() == 1 {
                // Could be function or variable
                collector.add_token_for_name(&segments[0], expr.span.start, TYPE_FUNCTION, 0);
            }
        }

        Expr::Call { func, args, .. } => {
            collect_expr_tokens(collector, func);
            for arg in args {
                collect_expr_tokens(collector, arg);
            }
        }

        Expr::MethodCall { receiver, method, args, .. } => {
            collect_expr_tokens(collector, receiver);

            // Method name comes after receiver and "."
            let method_start = receiver.span.end + 1;
            collector.add_token_for_name(method, method_start, TYPE_FUNCTION, 0);

            for arg in args {
                collect_expr_tokens(collector, arg);
            }
        }

        Expr::FieldAccess { expr: inner, field } => {
            collect_expr_tokens(collector, inner);

            // Field name comes after inner expr and "."
            let field_start = inner.span.end + 1;
            collector.add_token_for_name(field, field_start, TYPE_PROPERTY, 0);
        }

        Expr::Binary { left, right, .. } => {
            collect_expr_tokens(collector, left);
            collect_expr_tokens(collector, right);
        }

        Expr::Unary { expr: inner, .. } => {
            collect_expr_tokens(collector, inner);
        }

        Expr::If { cond, then_block, else_block } => {
            collect_expr_tokens(collector, cond);
            collect_block_tokens(collector, then_block);
            if let Some(else_blk) = else_block {
                collect_block_tokens(collector, else_blk);
            }
        }

        Expr::Match { expr: inner, arms } => {
            collect_expr_tokens(collector, inner);
            for arm in arms {
                collect_match_arm_tokens(collector, arm);
            }
        }

        Expr::Block(block) => {
            collect_block_tokens(collector, block);
        }

        Expr::Tuple(elements) | Expr::List(elements) => {
            for elem in elements {
                collect_expr_tokens(collector, elem);
            }
        }

        Expr::StructInit { name, fields, base } => {
            // Struct name
            if let Some(name_offset) = collector.find_name_in_span(name, &expr.span) {
                collector.add_token_for_name(name, name_offset, TYPE_STRUCT, 0);
            }

            // Field names and values
            for (field_name, field_expr) in fields {
                if let Some(field_offset) = collector.find_name_in_span(field_name, &expr.span) {
                    collector.add_token_for_name(field_name, field_offset, TYPE_PROPERTY, 0);
                }
                collect_expr_tokens(collector, field_expr);
            }

            if let Some(base_expr) = base {
                collect_expr_tokens(collector, base_expr);
            }
        }

        Expr::EnumVariant { type_name, variant, args } => {
            // Type name (if present)
            if let Some(tn) = type_name {
                if let Some(offset) = collector.find_name_in_span(tn, &expr.span) {
                    collector.add_token_for_name(tn, offset, TYPE_ENUM, 0);
                }
            }

            // Variant name
            if let Some(offset) = collector.find_name_in_span(variant, &expr.span) {
                collector.add_token_for_name(variant, offset, TYPE_ENUM_MEMBER, 0);
            }

            // Arguments
            match args {
                EnumVariantArgs::Tuple(exprs) => {
                    for e in exprs {
                        collect_expr_tokens(collector, e);
                    }
                }
                EnumVariantArgs::Struct(field_exprs) => {
                    for (_, e) in field_exprs {
                        collect_expr_tokens(collector, e);
                    }
                }
                EnumVariantArgs::Unit => {}
            }
        }

        Expr::For { clauses, body, .. } => {
            for clause in clauses {
                collect_for_clause_tokens(collector, clause);
            }
            collect_expr_tokens(collector, body);
        }

        Expr::Closure { params, body } => {
            // Closure parameters
            for param in params {
                if let Some(offset) = collector.find_name_in_span(param, &body.span) {
                    collector.add_token_for_name(param, offset, TYPE_PARAMETER, MOD_DECLARATION);
                }
            }
            collect_block_tokens(collector, body);
        }

        Expr::Receive { arms, timeout } => {
            for arm in arms {
                collect_match_arm_tokens(collector, arm);
            }
            if let Some((timeout_expr, timeout_block)) = timeout {
                collect_expr_tokens(collector, timeout_expr);
                collect_block_tokens(collector, timeout_block);
            }
        }

        Expr::Spawn(inner) => {
            collect_expr_tokens(collector, inner);
        }

        Expr::SpawnClosure(block) => {
            collect_block_tokens(collector, block);
        }

        Expr::Return(Some(inner)) => {
            collect_expr_tokens(collector, inner);
        }

        Expr::Send { to, msg } => {
            collect_expr_tokens(collector, to);
            collect_expr_tokens(collector, msg);
        }

        Expr::Pipe { left, right } => {
            collect_expr_tokens(collector, left);
            collect_expr_tokens(collector, right);
        }

        Expr::Try { expr: inner } => {
            collect_expr_tokens(collector, inner);
        }

        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_expr_tokens(collector, key);
                collect_expr_tokens(collector, value);
            }
        }

        Expr::ListCons { head, tail } => {
            collect_expr_tokens(collector, head);
            collect_expr_tokens(collector, tail);
        }

        Expr::ExternCall { module, function, args } => {
            // Module name
            if let Some(offset) = collector.find_name_in_span(module, &expr.span) {
                collector.add_token_for_name(module, offset, TYPE_NAMESPACE, 0);
            }
            // Function name
            if let Some(offset) = collector.find_name_in_span(function, &expr.span) {
                collector.add_token_for_name(function, offset, TYPE_FUNCTION, 0);
            }
            for arg in args {
                collect_expr_tokens(collector, arg);
            }
        }

        Expr::Quote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplice(inner) | Expr::UnquoteAtom(inner) => {
            collect_expr_tokens(collector, inner);
        }

        Expr::QuoteRepetition { pattern, .. } => {
            collect_expr_tokens(collector, pattern);
        }

        // These don't need additional handling
        Expr::Unit | Expr::Return(None) | Expr::BitString(_) |
        Expr::QuoteItem(_) | Expr::UnquoteFieldAccess { .. } => {}
    }
}

fn collect_match_arm_tokens(collector: &mut TokenCollector, arm: &MatchArm) {
    // Pattern bindings
    collect_pattern_tokens(collector, &arm.pattern, &arm.span, false);

    // Guard expression
    if let Some(guard) = &arm.guard {
        collect_expr_tokens(collector, guard);
    }

    // Body
    collect_expr_tokens(collector, &arm.body);
}

fn collect_for_clause_tokens(collector: &mut TokenCollector, clause: &ForClause) {
    match clause {
        ForClause::Generator { pattern, source, .. } => {
            collect_expr_tokens(collector, source);
            collect_pattern_tokens(collector, pattern, &source.span, false);
        }
        ForClause::When(expr) => {
            collect_expr_tokens(collector, expr);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Parser;

    #[test]
    fn test_semantic_tokens_simple_function() {
        let source = r#"mod test {
    fn hello(name: String) -> String {
        let greeting = "Hello";
        greeting
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();
        let line_index = LineIndex::new(source);

        let result = handle_semantic_tokens_full(&module, &line_index, source);
        assert!(result.is_some());

        let tokens = match result.unwrap() {
            SemanticTokensResult::Tokens(t) => t,
            _ => panic!("Expected Tokens"),
        };

        // Should have some tokens
        assert!(!tokens.data.is_empty(), "Expected semantic tokens");
    }

    #[test]
    fn test_semantic_tokens_struct() {
        let source = r#"mod test {
    struct Point {
        x: Int,
        y: Int,
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();
        let line_index = LineIndex::new(source);

        let result = handle_semantic_tokens_full(&module, &line_index, source);
        assert!(result.is_some());

        let tokens = match result.unwrap() {
            SemanticTokensResult::Tokens(t) => t,
            _ => panic!("Expected Tokens"),
        };

        // Should have tokens for struct name and fields
        assert!(!tokens.data.is_empty(), "Expected semantic tokens for struct");
    }

    #[test]
    fn test_delta_encoding() {
        let source = r#"mod test {
    fn foo() {
        let x = 1;
        let y = 2;
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();
        let line_index = LineIndex::new(source);

        let result = handle_semantic_tokens_full(&module, &line_index, source);
        assert!(result.is_some());

        let tokens = match result.unwrap() {
            SemanticTokensResult::Tokens(t) => t,
            _ => panic!("Expected Tokens"),
        };

        // Verify delta encoding is correct (lines should be non-negative, etc.)
        for token in &tokens.data {
            // Delta values should be reasonable
            assert!(token.delta_line < 1000, "Delta line too large");
            assert!(token.delta_start < 1000, "Delta start too large");
            assert!(token.length > 0, "Token length should be positive");
        }
    }
}
