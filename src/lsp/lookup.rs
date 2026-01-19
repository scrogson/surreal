//! AST lookup utilities for LSP features.
//!
//! This module provides utilities for finding AST nodes at specific positions,
//! tracking variable scopes, and resolving symbol definitions.

use std::collections::HashMap;

use crate::compiler::{
    Block, EnumPatternFields, EnumVariantArgs, Expr, ForClause, Function, Item, MatchArm, Module,
    Pattern, Span, SpannedExpr, Stmt,
};

/// Information about a symbol found at a position.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum SymbolInfo {
    /// A variable reference
    Variable {
        name: String,
        /// Span of the variable usage
        usage_span: Span,
        /// Span of the variable's definition (let binding or parameter)
        definition_span: Option<Span>,
        /// Type of the variable (if known)
        type_info: Option<String>,
    },
    /// A function call
    FunctionCall {
        /// Module path (e.g., ["io"] for io::println)
        module: Vec<String>,
        /// Function name
        name: String,
        /// Span of the call expression
        call_span: Span,
    },
    /// A function definition
    FunctionDef {
        name: String,
        span: Span,
    },
    /// A field access
    FieldAccess {
        field: String,
        span: Span,
    },
    /// A struct name
    StructRef {
        name: String,
        span: Span,
    },
}

/// Scope tracking for variable resolution.
#[derive(Debug, Clone)]
struct Scope {
    /// Variable name -> definition span
    variables: HashMap<String, Span>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, span: Span) {
        self.variables.insert(name, span);
    }

    fn lookup(&self, name: &str) -> Option<Span> {
        self.variables.get(name).cloned()
    }
}

/// Context for AST lookup operations.
pub struct LookupContext {
    /// Stack of scopes (innermost last)
    scopes: Vec<Scope>,
    /// Target offset we're looking for
    target_offset: usize,
    /// Found symbol info
    found: Option<SymbolInfo>,
}

impl LookupContext {
    fn new(target_offset: usize) -> Self {
        Self {
            scopes: vec![Scope::new()],
            target_offset,
            found: None,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_variable(&mut self, name: String, span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define(name, span);
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<Span> {
        // Search from innermost scope outward
        for scope in self.scopes.iter().rev() {
            if let Some(span) = scope.lookup(name) {
                return Some(span);
            }
        }
        None
    }

    fn is_in_range(&self, span: &Span) -> bool {
        // Treat unset spans (0..0) as "unknown, might contain target"
        if span.start == 0 && span.end == 0 {
            return true;
        }
        self.target_offset >= span.start && self.target_offset < span.end
    }

    fn set_found(&mut self, info: SymbolInfo) {
        // Only set if we haven't found something more specific
        if self.found.is_none() {
            self.found = Some(info);
        }
    }
}

/// Find symbol information at the given offset in a module.
pub fn find_symbol_at_offset(module: &Module, offset: usize) -> Option<SymbolInfo> {
    let mut ctx = LookupContext::new(offset);

    for item in &module.items {
        visit_item(&mut ctx, item);
        if ctx.found.is_some() {
            break;
        }
    }

    ctx.found
}

fn visit_item(ctx: &mut LookupContext, item: &Item) {
    match item {
        Item::Function(func) => visit_function(ctx, func),
        Item::Impl(impl_block) => {
            for method in &impl_block.methods {
                visit_function(ctx, method);
                if ctx.found.is_some() {
                    return;
                }
            }
        }
        Item::TraitImpl(trait_impl) => {
            for method in &trait_impl.methods {
                visit_function(ctx, method);
                if ctx.found.is_some() {
                    return;
                }
            }
        }
        _ => {}
    }
}

fn visit_function(ctx: &mut LookupContext, func: &Function) {
    // Check if we're in this function at all
    if !ctx.is_in_range(&func.span) {
        return;
    }

    // Check if we're on the function name itself
    // Function name is after "fn " (or "pub fn ")
    let name_start = if func.is_pub {
        func.span.start + 7 // "pub fn "
    } else {
        func.span.start + 3 // "fn "
    };
    let name_end = name_start + func.name.len();

    if ctx.target_offset >= name_start && ctx.target_offset < name_end {
        ctx.set_found(SymbolInfo::FunctionDef {
            name: func.name.clone(),
            span: func.span.clone(),
        });
        return;
    }

    // Create a new scope for the function
    ctx.push_scope();

    // Add parameters to scope
    for param in &func.params {
        if let Some((name, span)) = extract_pattern_binding(&param.pattern) {
            ctx.define_variable(name, span);
        }
    }

    // Visit the function body
    visit_block(ctx, &func.body);

    ctx.pop_scope();
}

fn visit_block(ctx: &mut LookupContext, block: &Block) {
    // Check if we're in this block
    if !ctx.is_in_range(&block.span) {
        return;
    }

    ctx.push_scope();

    for stmt in &block.stmts {
        visit_stmt(ctx, stmt);
        if ctx.found.is_some() {
            ctx.pop_scope();
            return;
        }
    }

    if let Some(expr) = &block.expr {
        visit_expr(ctx, expr);
    }

    ctx.pop_scope();
}

fn visit_stmt(ctx: &mut LookupContext, stmt: &Stmt) {
    match stmt {
        Stmt::Let {
            pattern,
            value,
            span,
            ..
        } => {
            // Visit the value expression first
            visit_expr(ctx, value);
            if ctx.found.is_some() {
                return;
            }

            // Add binding to scope
            if let Some((name, def_span)) = extract_pattern_binding(pattern) {
                // Check if cursor is on the pattern itself
                if ctx.is_in_range(&def_span) {
                    ctx.set_found(SymbolInfo::Variable {
                        name: name.clone(),
                        usage_span: def_span.clone(),
                        definition_span: Some(span.clone()),
                        type_info: None,
                    });
                    return;
                }
                ctx.define_variable(name, span.clone());
            }
        }
        Stmt::Expr { expr, .. } => {
            visit_expr(ctx, expr);
        }
    }
}

fn visit_expr(ctx: &mut LookupContext, expr: &SpannedExpr) {
    // Check if we're in this expression
    if !ctx.is_in_range(&expr.span) {
        return;
    }

    match &expr.expr {
        Expr::Ident(name) => {
            // Look up variable definition
            let def_span = ctx.lookup_variable(name);
            ctx.set_found(SymbolInfo::Variable {
                name: name.clone(),
                usage_span: expr.span.clone(),
                definition_span: def_span,
                type_info: None,
            });
        }

        Expr::Path { segments } => {
            if segments.len() == 2 {
                // Module::function reference
                ctx.set_found(SymbolInfo::FunctionCall {
                    module: vec![segments[0].clone()],
                    name: segments[1].clone(),
                    call_span: expr.span.clone(),
                });
            } else if segments.len() == 1 {
                // Could be a function name or type
                let def_span = ctx.lookup_variable(&segments[0]);
                ctx.set_found(SymbolInfo::Variable {
                    name: segments[0].clone(),
                    usage_span: expr.span.clone(),
                    definition_span: def_span,
                    type_info: None,
                });
            }
        }

        Expr::Call { func, args, .. } => {
            // Check if cursor is on the function part
            visit_expr(ctx, func);
            if ctx.found.is_some() {
                return;
            }

            // Check arguments
            for arg in args {
                visit_expr(ctx, arg);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            visit_expr(ctx, receiver);
            if ctx.found.is_some() {
                return;
            }

            // Check if cursor is on the method name
            // Method name comes after receiver and "."
            let method_start = receiver.span.end + 1; // after the dot
            let method_end = method_start + method.len();
            if ctx.target_offset >= method_start && ctx.target_offset < method_end {
                ctx.set_found(SymbolInfo::FunctionCall {
                    module: vec![],
                    name: method.clone(),
                    call_span: expr.span.clone(),
                });
                return;
            }

            for arg in args {
                visit_expr(ctx, arg);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::FieldAccess { expr: inner, field } => {
            visit_expr(ctx, inner);
            if ctx.found.is_some() {
                return;
            }

            // Check if cursor is on the field name
            let field_start = inner.span.end + 1;
            let field_end = field_start + field.len();
            if ctx.target_offset >= field_start && ctx.target_offset < field_end {
                ctx.set_found(SymbolInfo::FieldAccess {
                    field: field.clone(),
                    span: expr.span.clone(),
                });
            }
        }

        Expr::Binary { left, right, .. } => {
            visit_expr(ctx, left);
            if ctx.found.is_some() {
                return;
            }
            visit_expr(ctx, right);
        }

        Expr::Unary { expr: inner, .. } => {
            visit_expr(ctx, inner);
        }

        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            visit_expr(ctx, cond);
            if ctx.found.is_some() {
                return;
            }
            visit_block(ctx, then_block);
            if ctx.found.is_some() {
                return;
            }
            if let Some(else_blk) = else_block {
                visit_block(ctx, else_blk);
            }
        }

        Expr::Match { expr: inner, arms } => {
            visit_expr(ctx, inner);
            if ctx.found.is_some() {
                return;
            }
            for arm in arms {
                visit_match_arm(ctx, arm);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::Block(block) => {
            visit_block(ctx, block);
        }

        Expr::Tuple(elements) | Expr::List(elements) => {
            for elem in elements {
                visit_expr(ctx, elem);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::StructInit { name, fields, base } => {
            // Check if cursor is on struct name
            let name_end = expr.span.start + name.len();
            if ctx.target_offset >= expr.span.start && ctx.target_offset < name_end {
                ctx.set_found(SymbolInfo::StructRef {
                    name: name.clone(),
                    span: expr.span.clone(),
                });
                return;
            }

            for (_, field_expr) in fields {
                visit_expr(ctx, field_expr);
                if ctx.found.is_some() {
                    return;
                }
            }

            if let Some(base_expr) = base {
                visit_expr(ctx, base_expr);
            }
        }

        Expr::For { clauses, body, .. } => {
            ctx.push_scope();
            for clause in clauses {
                visit_for_clause(ctx, clause);
                if ctx.found.is_some() {
                    ctx.pop_scope();
                    return;
                }
            }
            visit_expr(ctx, body);
            ctx.pop_scope();
        }

        Expr::Closure { body, .. } => {
            ctx.push_scope();
            // Could add closure params to scope here
            visit_block(ctx, body);
            ctx.pop_scope();
        }

        Expr::Receive { arms, timeout } => {
            for arm in arms {
                visit_match_arm(ctx, arm);
                if ctx.found.is_some() {
                    return;
                }
            }
            if let Some((timeout_expr, timeout_block)) = timeout {
                visit_expr(ctx, timeout_expr);
                if ctx.found.is_some() {
                    return;
                }
                visit_block(ctx, timeout_block);
            }
        }

        Expr::Spawn(inner) => {
            visit_expr(ctx, inner);
        }

        Expr::SpawnClosure(block) => {
            ctx.push_scope();
            visit_block(ctx, block);
            ctx.pop_scope();
        }

        Expr::Return(Some(inner)) => {
            visit_expr(ctx, inner);
        }

        Expr::Send { to, msg } => {
            visit_expr(ctx, to);
            if ctx.found.is_some() {
                return;
            }
            visit_expr(ctx, msg);
        }

        Expr::Pipe { left, right } => {
            visit_expr(ctx, left);
            if ctx.found.is_some() {
                return;
            }
            visit_expr(ctx, right);
        }

        Expr::Try { expr: inner } => {
            visit_expr(ctx, inner);
        }

        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                visit_expr(ctx, key);
                if ctx.found.is_some() {
                    return;
                }
                visit_expr(ctx, value);
                if ctx.found.is_some() {
                    return;
                }
            }
        }

        Expr::ListCons { head, tail } => {
            visit_expr(ctx, head);
            if ctx.found.is_some() {
                return;
            }
            visit_expr(ctx, tail);
        }

        Expr::EnumVariant { args, .. } => {
            match args {
                EnumVariantArgs::Tuple(exprs) => {
                    for e in exprs {
                        visit_expr(ctx, e);
                        if ctx.found.is_some() {
                            return;
                        }
                    }
                }
                EnumVariantArgs::Struct(fields) => {
                    for (_, e) in fields {
                        visit_expr(ctx, e);
                        if ctx.found.is_some() {
                            return;
                        }
                    }
                }
                EnumVariantArgs::Unit => {}
            }
        }

        // Literals and other simple expressions
        _ => {}
    }
}

fn visit_match_arm(ctx: &mut LookupContext, arm: &MatchArm) {
    if !ctx.is_in_range(&arm.span) {
        return;
    }

    ctx.push_scope();

    // Add pattern bindings to scope
    add_pattern_bindings(ctx, &arm.pattern, &arm.span);

    // Visit guard if present
    if let Some(guard) = &arm.guard {
        visit_expr(ctx, guard);
        if ctx.found.is_some() {
            ctx.pop_scope();
            return;
        }
    }

    // Visit body
    visit_expr(ctx, &arm.body);

    ctx.pop_scope();
}

fn visit_for_clause(ctx: &mut LookupContext, clause: &ForClause) {
    match clause {
        ForClause::Generator {
            pattern, source, ..
        } => {
            visit_expr(ctx, source);
            if ctx.found.is_some() {
                return;
            }
            // Add pattern bindings
            add_pattern_bindings(ctx, pattern, &source.span);
        }
        ForClause::When(expr) => {
            visit_expr(ctx, expr);
        }
    }
}

fn add_pattern_bindings(ctx: &mut LookupContext, pattern: &Pattern, def_span: &Span) {
    match pattern {
        Pattern::Ident(name) => {
            ctx.define_variable(name.clone(), def_span.clone());
        }
        Pattern::Tuple(elements) | Pattern::List(elements) => {
            for elem in elements {
                add_pattern_bindings(ctx, elem, def_span);
            }
        }
        Pattern::ListCons { head, tail } => {
            add_pattern_bindings(ctx, head, def_span);
            add_pattern_bindings(ctx, tail, def_span);
        }
        Pattern::Struct { fields, .. } => {
            for (_, pat) in fields {
                add_pattern_bindings(ctx, pat, def_span);
            }
        }
        Pattern::Enum { fields, .. } => {
            match fields {
                EnumPatternFields::Tuple(pats) => {
                    for pat in pats {
                        add_pattern_bindings(ctx, pat, def_span);
                    }
                }
                EnumPatternFields::Struct(field_pats) => {
                    for (_, pat) in field_pats {
                        add_pattern_bindings(ctx, pat, def_span);
                    }
                }
                EnumPatternFields::Unit => {}
            }
        }
        _ => {}
    }
}

/// Extract the primary binding name and a rough span from a pattern.
fn extract_pattern_binding(pattern: &Pattern) -> Option<(String, Span)> {
    match pattern {
        Pattern::Ident(name) => Some((name.clone(), 0..name.len())), // Placeholder span
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Parser;

    #[test]
    fn test_find_variable_reference() {
        let source = r#"mod test {
    fn example() {
        let x = 42;
        x
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find the position of the second 'x' (the reference on its own line)
        let x_ref_pos = source.rfind("\n        x").unwrap() + 9; // After "\n        "
        let info = find_symbol_at_offset(&module, x_ref_pos);

        assert!(info.is_some(), "Expected to find symbol at offset {}", x_ref_pos);
        if let Some(SymbolInfo::Variable { name, .. }) = info {
            assert_eq!(name, "x");
        } else {
            panic!("Expected variable info, got {:?}", info);
        }
    }

    #[test]
    fn test_find_function_call() {
        let source = r#"mod test {
    fn example() {
        io::println("hello")
    }
}"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find position of "io::println"
        let call_pos = source.find("io::").unwrap();
        let info = find_symbol_at_offset(&module, call_pos);

        assert!(info.is_some(), "Expected to find symbol at offset {}", call_pos);
        if let Some(SymbolInfo::FunctionCall { module, name, .. }) = info {
            assert_eq!(module, vec!["io"]);
            assert_eq!(name, "println");
        } else {
            panic!("Expected function call info, got {:?}", info);
        }
    }
}
