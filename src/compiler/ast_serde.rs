//! AST serialization to/from Erlang term format.
//!
//! This module handles converting Dream AST nodes to Erlang term text format
//! and parsing Erlang terms back to AST nodes. This enables macros to run on
//! BEAM and manipulate AST as Erlang data structures.
//!
//! ## Erlang Term Format
//!
//! The format uses tagged tuples for each AST node type:
//! - `{int, 42}` for integer literals
//! - `{string, <<"hello">>}` for string literals
//! - `{ident, 'name'}` for identifiers
//! - `{binary_op, '+', Left, Right}` for binary operations
//! - `{struct, 'Name', Fields}` for struct definitions
//!
//! Lists use Erlang list syntax: `[Item1, Item2, ...]`
//! Atoms use single quotes: `'atom_name'`

use crate::compiler::ast::*;

/// Escape a string for inclusion in an Erlang binary literal.
fn escape_binary_string(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            _ => result.push(c),
        }
    }
    result
}

/// Escape a string for inclusion in an Erlang atom.
fn escape_atom(s: &str) -> String {
    // Simple atoms don't need escaping
    if s.chars().all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
        && s.chars().next().map_or(false, |c| c.is_ascii_lowercase()) {
        s.to_string()
    } else {
        // Need to quote and escape
        let mut result = String::new();
        for c in s.chars() {
            match c {
                '\'' => result.push_str("\\'"),
                '\\' => result.push_str("\\\\"),
                _ => result.push(c),
            }
        }
        result
    }
}

// =============================================================================
// AST to Erlang Term (Serialization)
// =============================================================================

/// Convert an expression to Erlang term format.
pub fn expr_to_erlang_term(expr: &Expr) -> String {
    match expr {
        Expr::Int(n) => format!("{{int, {}}}", n),

        Expr::String(s) => format!("{{string, <<\"{}\">>}}", escape_binary_string(s)),

        Expr::Charlist(s) => format!("{{charlist, \"{}\"}}", escape_binary_string(s)),

        Expr::Atom(a) => format!("{{atom, '{}'}}", escape_atom(a)),

        Expr::Bool(b) => format!("{{bool, {}}}", b),

        Expr::Unit => "{unit}".to_string(),

        Expr::Ident(name) => format!("{{ident, '{}'}}", escape_atom(name)),

        Expr::Path { segments } => {
            let segs: Vec<String> = segments.iter()
                .map(|s| format!("'{}'", escape_atom(s)))
                .collect();
            format!("{{path, [{}]}}", segs.join(", "))
        }

        Expr::Binary { op, left, right } => {
            format!("{{binary_op, '{}', {}, {}}}",
                binop_to_atom(op),
                expr_to_erlang_term(left),
                expr_to_erlang_term(right))
        }

        Expr::Unary { op, expr } => {
            format!("{{unary_op, '{}', {}}}",
                unaryop_to_atom(op),
                expr_to_erlang_term(expr))
        }

        Expr::Call { func, args, .. } => {
            let args_str: Vec<String> = args.iter()
                .map(|a| expr_to_erlang_term(a))
                .collect();
            format!("{{call, {}, [{}]}}",
                expr_to_erlang_term(func),
                args_str.join(", "))
        }

        Expr::MethodCall { receiver, method, args, .. } => {
            let args_str: Vec<String> = args.iter()
                .map(|a| expr_to_erlang_term(a))
                .collect();
            format!("{{method_call, {}, '{}', [{}]}}",
                expr_to_erlang_term(receiver),
                escape_atom(method),
                args_str.join(", "))
        }

        Expr::FieldAccess { expr, field } => {
            format!("{{field_access, {}, '{}'}}",
                expr_to_erlang_term(expr),
                escape_atom(field))
        }

        Expr::Tuple(elems) => {
            let elems_str: Vec<String> = elems.iter()
                .map(|e| expr_to_erlang_term(e))
                .collect();
            format!("{{tuple, [{}]}}", elems_str.join(", "))
        }

        Expr::List(elems) => {
            let elems_str: Vec<String> = elems.iter()
                .map(|e| expr_to_erlang_term(e))
                .collect();
            format!("{{list, [{}]}}", elems_str.join(", "))
        }

        Expr::StructInit { name, fields, base } => {
            let fields_str: Vec<String> = fields.iter()
                .map(|(n, e)| format!("{{'{}', {}}}", escape_atom(n), expr_to_erlang_term(e)))
                .collect();
            let base_str = base.as_ref()
                .map(|b| format!(", {{base, {}}}", expr_to_erlang_term(b)))
                .unwrap_or_default();
            format!("{{struct_init, '{}', [{}]{}}}",
                escape_atom(name),
                fields_str.join(", "),
                base_str)
        }

        Expr::EnumVariant { type_name, variant, args } => {
            let type_str = type_name.as_ref()
                .map(|t| format!("'{}'", escape_atom(t)))
                .unwrap_or_else(|| "none".to_string());
            let args_str = match args {
                EnumVariantArgs::Unit => "unit".to_string(),
                EnumVariantArgs::Tuple(exprs) => {
                    let es: Vec<String> = exprs.iter()
                        .map(|e| expr_to_erlang_term(e))
                        .collect();
                    format!("{{tuple, [{}]}}", es.join(", "))
                }
                EnumVariantArgs::Struct(fields) => {
                    let fs: Vec<String> = fields.iter()
                        .map(|(n, e)| format!("{{'{}', {}}}", escape_atom(n), expr_to_erlang_term(e)))
                        .collect();
                    format!("{{struct, [{}]}}", fs.join(", "))
                }
            };
            format!("{{enum_variant, {}, '{}', {}}}",
                type_str,
                escape_atom(variant),
                args_str)
        }

        Expr::If { cond, then_block, else_block } => {
            let else_str = else_block.as_ref()
                .map(|b| format!(", {}", block_to_erlang_term(b)))
                .unwrap_or_else(|| ", none".to_string());
            format!("{{if, {}, {}{}}}",
                expr_to_erlang_term(cond),
                block_to_erlang_term(then_block),
                else_str)
        }

        Expr::Match { expr, arms } => {
            let arms_str: Vec<String> = arms.iter()
                .map(|a| match_arm_to_erlang_term(a))
                .collect();
            format!("{{match, {}, [{}]}}",
                expr_to_erlang_term(expr),
                arms_str.join(", "))
        }

        Expr::Block(block) => {
            format!("{{block, {}}}", block_to_erlang_term(block))
        }

        Expr::Return(e) => {
            let inner = e.as_ref()
                .map(|e| expr_to_erlang_term(e))
                .unwrap_or_else(|| "none".to_string());
            format!("{{return, {}}}", inner)
        }

        Expr::Quote(inner) => {
            format!("{{quote, {}}}", expr_to_erlang_term(inner))
        }

        Expr::Unquote(inner) => {
            format!("{{unquote, {}}}", expr_to_erlang_term(inner))
        }

        Expr::UnquoteSplice(inner) => {
            format!("{{unquote_splice, {}}}", expr_to_erlang_term(inner))
        }

        Expr::ExternCall { module, function, args } => {
            let args_str: Vec<String> = args.iter()
                .map(|a| expr_to_erlang_term(a))
                .collect();
            format!("{{extern_call, '{}', '{}', [{}]}}",
                escape_atom(module),
                escape_atom(function),
                args_str.join(", "))
        }

        // Simplified handling for other expressions
        Expr::Spawn(e) => format!("{{spawn, {}}}", expr_to_erlang_term(e)),
        Expr::SpawnClosure(block) => format!("{{spawn_closure, {}}}", block_to_erlang_term(block)),
        Expr::Send { to, msg } => format!("{{send, {}, {}}}", expr_to_erlang_term(to), expr_to_erlang_term(msg)),
        Expr::Pipe { left, right } => format!("{{pipe, {}, {}}}", expr_to_erlang_term(left), expr_to_erlang_term(right)),
        Expr::Closure { params, body } => {
            let params_str: Vec<String> = params.iter().map(|p| format!("'{}'", escape_atom(p))).collect();
            format!("{{closure, [{}], {}}}", params_str.join(", "), block_to_erlang_term(body))
        }
        Expr::Try { expr } => format!("{{try, {}}}", expr_to_erlang_term(expr)),
        Expr::Receive { arms, timeout } => {
            let arms_str: Vec<String> = arms.iter().map(|a| match_arm_to_erlang_term(a)).collect();
            let timeout_str = timeout.as_ref()
                .map(|(dur, block)| format!(", {{after, {}, {}}}", expr_to_erlang_term(dur), block_to_erlang_term(block)))
                .unwrap_or_default();
            format!("{{receive, [{}]{}}}",  arms_str.join(", "), timeout_str)
        }
        Expr::BitString(_) => "{bitstring}".to_string(), // Simplified
        Expr::StringInterpolation(_) => "{string_interpolation}".to_string(), // Simplified
    }
}

/// Convert a binary operator to an Erlang atom.
fn binop_to_atom(op: &BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Mod => "rem",
        BinOp::Eq => "==",
        BinOp::Ne => "/=",
        BinOp::Lt => "<",
        BinOp::Le => "=<",
        BinOp::Gt => ">",
        BinOp::Ge => ">=",
        BinOp::And => "and",
        BinOp::Or => "or",
    }
}

/// Convert a unary operator to an Erlang atom.
fn unaryop_to_atom(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Not => "not",
    }
}

/// Convert a block to Erlang term format.
pub fn block_to_erlang_term(block: &Block) -> String {
    let stmts_str: Vec<String> = block.stmts.iter()
        .map(|s| stmt_to_erlang_term(s))
        .collect();
    let expr_str = block.expr.as_ref()
        .map(|e| expr_to_erlang_term(e))
        .unwrap_or_else(|| "none".to_string());
    format!("{{[{}], {}}}", stmts_str.join(", "), expr_str)
}

/// Convert a statement to Erlang term format.
pub fn stmt_to_erlang_term(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Let { pattern, ty, value } => {
            let ty_str = ty.as_ref()
                .map(|t| type_to_erlang_term(t))
                .unwrap_or_else(|| "none".to_string());
            format!("{{let, {}, {}, {}}}",
                pattern_to_erlang_term(pattern),
                ty_str,
                expr_to_erlang_term(value))
        }
        Stmt::Expr(e) => {
            format!("{{expr, {}}}", expr_to_erlang_term(e))
        }
    }
}

/// Convert a match arm to Erlang term format.
pub fn match_arm_to_erlang_term(arm: &MatchArm) -> String {
    let guard_str = arm.guard.as_ref()
        .map(|g| expr_to_erlang_term(g))
        .unwrap_or_else(|| "none".to_string());
    format!("{{{}, {}, {}}}",
        pattern_to_erlang_term(&arm.pattern),
        guard_str,
        expr_to_erlang_term(&arm.body))
}

/// Convert a pattern to Erlang term format.
pub fn pattern_to_erlang_term(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Wildcard => "{wildcard}".to_string(),
        Pattern::Ident(name) => format!("{{ident, '{}'}}", escape_atom(name)),
        Pattern::Int(n) => format!("{{int, {}}}", n),
        Pattern::String(s) => format!("{{string, <<\"{}\">>}}", escape_binary_string(s)),
        Pattern::Charlist(s) => format!("{{charlist, \"{}\"}}", escape_binary_string(s)),
        Pattern::Atom(a) => format!("{{atom, '{}'}}", escape_atom(a)),
        Pattern::Bool(b) => format!("{{bool, {}}}", b),
        Pattern::Tuple(pats) => {
            let pats_str: Vec<String> = pats.iter()
                .map(|p| pattern_to_erlang_term(p))
                .collect();
            format!("{{tuple, [{}]}}", pats_str.join(", "))
        }
        Pattern::List(pats) => {
            let pats_str: Vec<String> = pats.iter()
                .map(|p| pattern_to_erlang_term(p))
                .collect();
            format!("{{list, [{}]}}", pats_str.join(", "))
        }
        Pattern::ListCons { head, tail } => {
            format!("{{list_cons, {}, {}}}",
                pattern_to_erlang_term(head),
                pattern_to_erlang_term(tail))
        }
        Pattern::Struct { name, fields } => {
            let fields_str: Vec<String> = fields.iter()
                .map(|(n, p)| format!("{{'{}', {}}}", escape_atom(n), pattern_to_erlang_term(p)))
                .collect();
            format!("{{struct, '{}', [{}]}}", escape_atom(name), fields_str.join(", "))
        }
        Pattern::Enum { name, variant, fields } => {
            match fields {
                EnumPatternFields::Unit => {
                    format!("{{enum, '{}', '{}', unit}}", escape_atom(name), escape_atom(variant))
                }
                EnumPatternFields::Tuple(pats) => {
                    let pats_str: Vec<String> = pats.iter()
                        .map(|p| pattern_to_erlang_term(p))
                        .collect();
                    format!("{{enum, '{}', '{}', {{tuple, [{}]}}}}",
                        escape_atom(name), escape_atom(variant), pats_str.join(", "))
                }
                EnumPatternFields::Struct(fields) => {
                    let fields_str: Vec<String> = fields.iter()
                        .map(|(n, p)| format!("{{'{}', {}}}", escape_atom(n), pattern_to_erlang_term(p)))
                        .collect();
                    format!("{{enum, '{}', '{}', {{struct, [{}]}}}}",
                        escape_atom(name), escape_atom(variant), fields_str.join(", "))
                }
            }
        }
        Pattern::BitString(_) => "{bitstring}".to_string(), // Simplified
    }
}

/// Convert a type to Erlang term format.
pub fn type_to_erlang_term(ty: &Type) -> String {
    match ty {
        Type::Int => "{type, int}".to_string(),
        Type::Float => "{type, float}".to_string(),
        Type::String => "{type, string}".to_string(),
        Type::Atom => "{type, atom}".to_string(),
        Type::Bool => "{type, bool}".to_string(),
        Type::Unit => "{type, unit}".to_string(),
        Type::Pid => "{type, pid}".to_string(),
        Type::Ref => "{type, ref}".to_string(),
        Type::Binary => "{type, binary}".to_string(),
        Type::Any => "{type, any}".to_string(),
        Type::Map => "{type, map}".to_string(),
        Type::List(inner) => format!("{{list, {}}}", type_to_erlang_term(inner)),
        Type::Tuple(types) => {
            let types_str: Vec<String> = types.iter()
                .map(|t| type_to_erlang_term(t))
                .collect();
            format!("{{tuple, [{}]}}", types_str.join(", "))
        }
        Type::Named { name, type_args } => {
            if type_args.is_empty() {
                format!("{{named, '{}'}}", escape_atom(name))
            } else {
                let args_str: Vec<String> = type_args.iter()
                    .map(|t| type_to_erlang_term(t))
                    .collect();
                format!("{{named, '{}', [{}]}}", escape_atom(name), args_str.join(", "))
            }
        }
        Type::TypeVar(name) => format!("{{type_var, '{}'}}", escape_atom(name)),
        Type::Fn { params, ret } => {
            let params_str: Vec<String> = params.iter()
                .map(|t| type_to_erlang_term(t))
                .collect();
            format!("{{fn, [{}], {}}}", params_str.join(", "), type_to_erlang_term(ret))
        }
        Type::AtomLiteral(a) => format!("{{atom_literal, '{}'}}", escape_atom(a)),
        Type::Union(types) => {
            let types_str: Vec<String> = types.iter()
                .map(|t| type_to_erlang_term(t))
                .collect();
            format!("{{union, [{}]}}", types_str.join(", "))
        }
        Type::AssociatedType { base, name } => {
            format!("{{assoc_type, '{}', '{}'}}", escape_atom(base), escape_atom(name))
        }
    }
}

/// Convert a struct definition to Erlang term format.
pub fn struct_def_to_erlang_term(s: &StructDef) -> String {
    let fields_str: Vec<String> = s.fields.iter()
        .map(|(name, ty)| format!("{{'{}', {}}}", escape_atom(name), type_to_erlang_term(ty)))
        .collect();
    let type_params_str: Vec<String> = s.type_params.iter()
        .map(|tp| format!("'{}'", escape_atom(&tp.name)))
        .collect();
    format!("{{struct, '{}', [{}], [{}]}}",
        escape_atom(&s.name),
        fields_str.join(", "),
        type_params_str.join(", "))
}

/// Convert an enum definition to Erlang term format.
pub fn enum_def_to_erlang_term(e: &EnumDef) -> String {
    let variants_str: Vec<String> = e.variants.iter()
        .map(|v| variant_def_to_erlang_term(v))
        .collect();
    let type_params_str: Vec<String> = e.type_params.iter()
        .map(|tp| format!("'{}'", escape_atom(&tp.name)))
        .collect();
    format!("{{enum, '{}', [{}], [{}]}}",
        escape_atom(&e.name),
        variants_str.join(", "),
        type_params_str.join(", "))
}

/// Convert an enum variant definition to Erlang term format.
fn variant_def_to_erlang_term(v: &EnumVariant) -> String {
    let kind_str = match &v.kind {
        VariantKind::Unit => "unit".to_string(),
        VariantKind::Tuple(types) => {
            let types_str: Vec<String> = types.iter()
                .map(|t| type_to_erlang_term(t))
                .collect();
            format!("{{tuple, [{}]}}", types_str.join(", "))
        }
        VariantKind::Struct(fields) => {
            let fields_str: Vec<String> = fields.iter()
                .map(|(n, t)| format!("{{'{}', {}}}", escape_atom(n), type_to_erlang_term(t)))
                .collect();
            format!("{{struct, [{}]}}", fields_str.join(", "))
        }
    };
    format!("{{'{}', {}}}", escape_atom(&v.name), kind_str)
}

/// Convert a function to Erlang term format.
pub fn function_to_erlang_term(f: &Function) -> String {
    let params_str: Vec<String> = f.params.iter()
        .map(|p| param_to_erlang_term(p))
        .collect();
    let return_type_str = f.return_type.as_ref()
        .map(|t| type_to_erlang_term(t))
        .unwrap_or_else(|| "none".to_string());
    let type_params_str: Vec<String> = f.type_params.iter()
        .map(|tp| format!("'{}'", escape_atom(&tp.name)))
        .collect();
    format!("{{function, '{}', [{}], {}, {}, {}}}",
        escape_atom(&f.name),
        type_params_str.join(", "),
        format!("[{}]", params_str.join(", ")),
        return_type_str,
        block_to_erlang_term(&f.body))
}

/// Convert a function parameter to Erlang term format.
fn param_to_erlang_term(p: &Param) -> String {
    format!("{{{}, {}}}", pattern_to_erlang_term(&p.pattern), type_to_erlang_term(&p.ty))
}

/// Convert an impl block to Erlang term format.
pub fn impl_block_to_erlang_term(impl_block: &ImplBlock) -> String {
    let methods_str: Vec<String> = impl_block.methods.iter()
        .map(|m| function_to_erlang_term(m))
        .collect();
    format!("{{impl, '{}', [{}]}}",
        escape_atom(&impl_block.type_name),
        methods_str.join(", "))
}

/// Convert an Item to Erlang term format.
pub fn item_to_erlang_term(item: &Item) -> String {
    match item {
        Item::Struct(s) => struct_def_to_erlang_term(s),
        Item::Enum(e) => enum_def_to_erlang_term(e),
        Item::Function(f) => function_to_erlang_term(f),
        Item::Impl(impl_block) => impl_block_to_erlang_term(impl_block),
        _ => "{unsupported_item}".to_string(),
    }
}

// =============================================================================
// Erlang Term to AST (Deserialization) - TODO
// =============================================================================

// Note: Deserialization is more complex and will be implemented when needed.
// For now, macros will return text that gets parsed by the Dream parser,
// or we can implement specific deserialization for ImplBlock which is the
// main output type from derive macros.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr_int() {
        assert_eq!(expr_to_erlang_term(&Expr::Int(42)), "{int, 42}");
    }

    #[test]
    fn test_expr_string() {
        assert_eq!(
            expr_to_erlang_term(&Expr::String("hello".to_string())),
            "{string, <<\"hello\">>}"
        );
    }

    #[test]
    fn test_expr_string_escape() {
        assert_eq!(
            expr_to_erlang_term(&Expr::String("hello \"world\"".to_string())),
            "{string, <<\"hello \\\"world\\\"\">>}"
        );
    }

    #[test]
    fn test_expr_ident() {
        assert_eq!(
            expr_to_erlang_term(&Expr::Ident("foo".to_string())),
            "{ident, 'foo'}"
        );
    }

    #[test]
    fn test_expr_binary_op() {
        let expr = Expr::Binary {
            op: BinOp::Add,
            left: Box::new(Expr::Int(1)),
            right: Box::new(Expr::Int(2)),
        };
        assert_eq!(
            expr_to_erlang_term(&expr),
            "{binary_op, '+', {int, 1}, {int, 2}}"
        );
    }

    #[test]
    fn test_type_named() {
        let ty = Type::Named {
            name: "Point".to_string(),
            type_args: vec![],
        };
        assert_eq!(type_to_erlang_term(&ty), "{named, 'Point'}");
    }

    #[test]
    fn test_type_list() {
        let ty = Type::List(Box::new(Type::Int));
        assert_eq!(type_to_erlang_term(&ty), "{list, {type, int}}");
    }

    #[test]
    fn test_struct_def() {
        let s = StructDef {
            is_pub: true,
            name: "Point".to_string(),
            type_params: vec![],
            fields: vec![
                ("x".to_string(), Type::Int),
                ("y".to_string(), Type::Int),
            ],
            attrs: vec![],
        };
        assert_eq!(
            struct_def_to_erlang_term(&s),
            "{struct, 'Point', [{'x', {type, int}}, {'y', {type, int}}], []}"
        );
    }
}
