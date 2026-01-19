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

        Expr::ListCons { head, tail } => {
            format!("{{list_cons, {}, {}}}",
                expr_to_erlang_term(head),
                expr_to_erlang_term(tail))
        }

        Expr::MapLiteral(pairs) => {
            let pairs_str: Vec<String> = pairs.iter()
                .map(|(k, v)| format!("{{{}, {}}}", expr_to_erlang_term(k), expr_to_erlang_term(v)))
                .collect();
            format!("{{map_literal, [{}]}}", pairs_str.join(", "))
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

        Expr::UnquoteAtom(inner) => {
            format!("{{unquote_atom, {}}}", expr_to_erlang_term(inner))
        }

        Expr::UnquoteFieldAccess { expr, field_expr } => {
            format!("{{unquote_field_access, {}, {}}}", expr_to_erlang_term(expr), expr_to_erlang_term(field_expr))
        }

        Expr::QuoteItem(item) => {
            // For now, just note it's a quote item - full serialization could be added later
            format!("{{quote_item, {}}}", item_to_erlang_term(item))
        }

        Expr::QuoteRepetition { pattern, separator } => {
            let sep_str = match separator {
                Some(s) => format!("'{}'", s),
                None => "none".to_string(),
            };
            format!("{{quote_repetition, {}, {}}}", expr_to_erlang_term(pattern), sep_str)
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
        Expr::For { .. } => "{for}".to_string(), // Simplified
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
        Stmt::Let { pattern, ty, value, else_block, .. } => {
            let ty_str = ty.as_ref()
                .map(|t| type_to_erlang_term(t))
                .unwrap_or_else(|| "none".to_string());
            let else_str = else_block.as_ref()
                .map(|b| block_to_erlang_term(b))
                .unwrap_or_else(|| "none".to_string());
            format!("{{let, {}, {}, {}, {}}}",
                pattern_to_erlang_term(pattern),
                ty_str,
                expr_to_erlang_term(value),
                else_str)
        }
        Stmt::Expr { expr: e, .. } => {
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

/// Convert a trait impl to Erlang term format.
/// Format: {traitimpl, TraitName, TypeName, [Methods]}
pub fn trait_impl_to_erlang_term(trait_impl: &TraitImpl) -> String {
    let methods_str: Vec<String> = trait_impl.methods.iter()
        .map(|m| function_to_erlang_term(m))
        .collect();
    format!("{{traitimpl, '{}', '{}', [{}]}}",
        escape_atom(&trait_impl.trait_name),
        escape_atom(&trait_impl.type_name),
        methods_str.join(", "))
}

/// Convert an Item to Erlang term format.
pub fn item_to_erlang_term(item: &Item) -> String {
    match item {
        Item::Struct(s) => struct_def_to_erlang_term(s),
        Item::Enum(e) => enum_def_to_erlang_term(e),
        Item::Function(f) => function_to_erlang_term(f),
        Item::Impl(impl_block) => impl_block_to_erlang_term(impl_block),
        Item::TraitImpl(trait_impl) => trait_impl_to_erlang_term(trait_impl),
        _ => "{unsupported_item}".to_string(),
    }
}

// =============================================================================
// DeriveInput Format (syn-style structured input for derive macros)
// =============================================================================

/// Convert a struct definition to DeriveInput format.
/// This format is more explicit and matches syn's DeriveInput structure.
///
/// Format:
/// ```erlang
/// {derive_input,
///   'StructName',
///   [{type_param, 'T', []}],  % generics
///   {struct_data, named, [
///     {field, 'field_name', is_pub, Type}
///   ]}
/// }
/// ```
pub fn struct_to_derive_input(s: &StructDef) -> String {
    let type_params_str: Vec<String> = s.type_params.iter()
        .map(|tp| {
            let bounds: Vec<String> = tp.bounds.iter()
                .map(|b| format!("'{}'", escape_atom(b)))
                .collect();
            format!("{{type_param, '{}', [{}]}}", escape_atom(&tp.name), bounds.join(", "))
        })
        .collect();

    let fields_str: Vec<String> = s.fields.iter()
        .map(|(name, ty)| {
            format!("{{field, '{}', true, {}}}", escape_atom(name), type_to_erlang_term(ty))
        })
        .collect();

    format!("{{derive_input, '{}', [{}], {{struct_data, named, [{}]}}}}",
        escape_atom(&s.name),
        type_params_str.join(", "),
        fields_str.join(", "))
}

/// Convert an enum definition to DeriveInput format.
///
/// Format:
/// ```erlang
/// {derive_input,
///   'EnumName',
///   [{type_param, 'T', []}],  % generics
///   {enum_data, [
///     {variant, 'VariantName', unit | {tuple, [Type]} | {struct, [Field]}}
///   ]}
/// }
/// ```
pub fn enum_to_derive_input(e: &EnumDef) -> String {
    let type_params_str: Vec<String> = e.type_params.iter()
        .map(|tp| {
            let bounds: Vec<String> = tp.bounds.iter()
                .map(|b| format!("'{}'", escape_atom(b)))
                .collect();
            format!("{{type_param, '{}', [{}]}}", escape_atom(&tp.name), bounds.join(", "))
        })
        .collect();

    let variants_str: Vec<String> = e.variants.iter()
        .map(|v| {
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
                        .map(|(n, t)| format!("{{field, '{}', true, {}}}", escape_atom(n), type_to_erlang_term(t)))
                        .collect();
                    format!("{{struct, [{}]}}", fields_str.join(", "))
                }
            };
            format!("{{variant, '{}', {}}}", escape_atom(&v.name), kind_str)
        })
        .collect();

    format!("{{derive_input, '{}', [{}], {{enum_data, [{}]}}}}",
        escape_atom(&e.name),
        type_params_str.join(", "),
        variants_str.join(", "))
}

/// Convert an Item to DeriveInput format (for derive macros).
/// Returns None for items that can't be used with derive.
pub fn item_to_derive_input(item: &Item) -> Option<String> {
    match item {
        Item::Struct(s) => Some(struct_to_derive_input(s)),
        Item::Enum(e) => Some(enum_to_derive_input(e)),
        _ => None,
    }
}

// =============================================================================
// TokenStream Serialization (for proc_macro input)
// =============================================================================

/// Convert an Item to TokenStream format.
/// This is the lexical representation of the item as a list of tokens.
///
/// Format: List of token tuples matching proc_macro::TokenTree
/// - `{ident, name}` - Identifier
/// - `{type_ident, name}` - Type identifier (PascalCase)
/// - `{int, value}` - Integer literal
/// - `{string, value}` - String literal
/// - `{atom, name}` - Atom literal
/// - `{punct, chars}` - Punctuation
/// - `{keyword, name}` - Keyword
/// - `{group, delimiter, tokens}` - Delimited group
pub fn item_to_token_stream(item: &Item) -> Option<String> {
    match item {
        Item::Struct(s) => Some(struct_to_token_stream(s)),
        Item::Enum(e) => Some(enum_to_token_stream(e)),
        _ => None,
    }
}

/// Helper to create a punctuation token with binary string.
fn punct_token(s: &str) -> String {
    format!("{{punct, <<\"{}\">>}}", escape_binary_string(s))
}

/// Convert a struct definition to TokenStream format.
pub fn struct_to_token_stream(s: &StructDef) -> String {
    let mut tokens = Vec::new();

    // pub keyword (if public - we assume pub for derive targets)
    tokens.push("{keyword, pub}".to_string());

    // struct keyword
    tokens.push("{keyword, struct}".to_string());

    // Struct name
    tokens.push(format!("{{type_ident, '{}'}}", escape_atom(&s.name)));

    // Type parameters
    if !s.type_params.is_empty() {
        let params: Vec<String> = s.type_params.iter()
            .enumerate()
            .flat_map(|(i, tp)| {
                let mut param_tokens = Vec::new();
                if i > 0 {
                    param_tokens.push(punct_token(","));
                }
                param_tokens.push(format!("{{type_ident, '{}'}}", escape_atom(&tp.name)));
                // Add bounds if present
                for (j, bound) in tp.bounds.iter().enumerate() {
                    if j == 0 {
                        param_tokens.push(punct_token(":"));
                    } else {
                        param_tokens.push(punct_token("+"));
                    }
                    param_tokens.push(format!("{{type_ident, '{}'}}", escape_atom(bound)));
                }
                param_tokens
            })
            .collect();
        tokens.push(format!("{{group, angle, [{}]}}", params.join(", ")));
    }

    // Fields group
    let fields: Vec<String> = s.fields.iter()
        .enumerate()
        .flat_map(|(i, (name, ty))| {
            let mut field_tokens = Vec::new();
            if i > 0 {
                field_tokens.push(punct_token(","));
            }
            field_tokens.push(format!("{{ident, '{}'}}", escape_atom(name)));
            field_tokens.push(punct_token(":"));
            field_tokens.extend(type_to_tokens(ty));
            field_tokens
        })
        .collect();

    tokens.push(format!("{{group, brace, [{}]}}", fields.join(", ")));

    format!("[{}]", tokens.join(", "))
}

/// Convert an enum definition to TokenStream format.
pub fn enum_to_token_stream(e: &EnumDef) -> String {
    let mut tokens = Vec::new();

    // pub keyword
    tokens.push("{keyword, pub}".to_string());

    // enum keyword
    tokens.push("{keyword, enum}".to_string());

    // Enum name
    tokens.push(format!("{{type_ident, '{}'}}", escape_atom(&e.name)));

    // Type parameters
    if !e.type_params.is_empty() {
        let params: Vec<String> = e.type_params.iter()
            .enumerate()
            .flat_map(|(i, tp)| {
                let mut param_tokens = Vec::new();
                if i > 0 {
                    param_tokens.push(punct_token(","));
                }
                param_tokens.push(format!("{{type_ident, '{}'}}", escape_atom(&tp.name)));
                param_tokens
            })
            .collect();
        tokens.push(format!("{{group, angle, [{}]}}", params.join(", ")));
    }

    // Variants group
    let variants: Vec<String> = e.variants.iter()
        .enumerate()
        .flat_map(|(i, v)| {
            let mut variant_tokens = Vec::new();
            if i > 0 {
                variant_tokens.push(punct_token(","));
            }
            variant_tokens.push(format!("{{type_ident, '{}'}}", escape_atom(&v.name)));

            match &v.kind {
                VariantKind::Unit => {}
                VariantKind::Tuple(types) => {
                    let type_tokens: Vec<String> = types.iter()
                        .enumerate()
                        .flat_map(|(j, ty)| {
                            let mut toks = Vec::new();
                            if j > 0 {
                                toks.push(punct_token(","));
                            }
                            toks.extend(type_to_tokens(ty));
                            toks
                        })
                        .collect();
                    variant_tokens.push(format!("{{group, paren, [{}]}}", type_tokens.join(", ")));
                }
                VariantKind::Struct(fields) => {
                    let field_tokens: Vec<String> = fields.iter()
                        .enumerate()
                        .flat_map(|(j, (name, ty))| {
                            let mut ftoks = Vec::new();
                            if j > 0 {
                                ftoks.push(punct_token(","));
                            }
                            ftoks.push(format!("{{ident, '{}'}}", escape_atom(name)));
                            ftoks.push(punct_token(":"));
                            ftoks.extend(type_to_tokens(ty));
                            ftoks
                        })
                        .collect();
                    variant_tokens.push(format!("{{group, brace, [{}]}}", field_tokens.join(", ")));
                }
            }
            variant_tokens
        })
        .collect();

    tokens.push(format!("{{group, brace, [{}]}}", variants.join(", ")));

    format!("[{}]", tokens.join(", "))
}

/// Convert a Type to token representation.
fn type_to_tokens(ty: &Type) -> Vec<String> {
    match ty {
        Type::Int => vec!["{keyword, int}".to_string()],
        Type::Float => vec!["{keyword, float}".to_string()],
        Type::String => vec!["{keyword, string}".to_string()],
        Type::Atom => vec!["{keyword, atom}".to_string()],
        Type::Bool => vec!["{keyword, bool}".to_string()],
        Type::Unit => vec![punct_token("()")],
        Type::Pid => vec!["{keyword, pid}".to_string()],
        Type::Ref => vec!["{keyword, ref}".to_string()],
        Type::Binary => vec!["{keyword, binary}".to_string()],
        Type::Any => vec!["{keyword, any}".to_string()],
        Type::Map => vec!["{keyword, map}".to_string()],
        Type::Named { name, type_args } => {
            let mut tokens = vec![format!("{{type_ident, '{}'}}", escape_atom(name))];
            if !type_args.is_empty() {
                let args: Vec<String> = type_args.iter()
                    .enumerate()
                    .flat_map(|(i, t)| {
                        let mut arg_toks = Vec::new();
                        if i > 0 {
                            arg_toks.push(punct_token(","));
                        }
                        arg_toks.extend(type_to_tokens(t));
                        arg_toks
                    })
                    .collect();
                tokens.push(format!("{{group, angle, [{}]}}", args.join(", ")));
            }
            tokens
        }
        Type::List(inner) => {
            let mut tokens = vec![punct_token("[")];
            tokens.extend(type_to_tokens(inner));
            tokens.push(punct_token("]"));
            tokens
        }
        Type::Tuple(types) => {
            let inner: Vec<String> = types.iter()
                .enumerate()
                .flat_map(|(i, t)| {
                    let mut toks = Vec::new();
                    if i > 0 {
                        toks.push(punct_token(","));
                    }
                    toks.extend(type_to_tokens(t));
                    toks
                })
                .collect();
            vec![format!("{{group, paren, [{}]}}", inner.join(", "))]
        }
        Type::TypeVar(name) => vec![format!("{{type_ident, '{}'}}", escape_atom(name))],
        Type::Fn { params, ret } => {
            let mut tokens = vec!["{keyword, fn}".to_string()];
            let param_tokens: Vec<String> = params.iter()
                .enumerate()
                .flat_map(|(i, t)| {
                    let mut toks = Vec::new();
                    if i > 0 {
                        toks.push(punct_token(","));
                    }
                    toks.extend(type_to_tokens(t));
                    toks
                })
                .collect();
            tokens.push(format!("{{group, paren, [{}]}}", param_tokens.join(", ")));
            tokens.push(punct_token("->"));
            tokens.extend(type_to_tokens(ret));
            tokens
        }
        _ => vec!["{keyword, any}".to_string()], // Fallback for unsupported types
    }
}

// =============================================================================
// Erlang Term to AST (Deserialization)
// =============================================================================

/// Error type for term parsing failures.
#[derive(Debug)]
pub struct TermParseError {
    pub message: String,
    pub position: usize,
}

impl TermParseError {
    fn new(message: impl Into<String>, position: usize) -> Self {
        TermParseError {
            message: message.into(),
            position,
        }
    }
}

impl std::fmt::Display for TermParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error at position {}: {}", self.position, self.message)
    }
}

impl std::error::Error for TermParseError {}

/// Result type for term parsing.
pub type TermParseResult<T> = Result<T, TermParseError>;

/// Erlang term representation for parsing.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Atom(String),
    Int(i64),
    Float(f64),
    String(String),      // Binary string: <<"...">>
    Tuple(Vec<Term>),
    List(Vec<Term>),
}

/// Parser for Erlang term text format.
pub struct TermParser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> TermParser<'a> {
    pub fn new(input: &'a str) -> Self {
        TermParser { input, pos: 0 }
    }

    /// Parse the entire input as a term.
    pub fn parse(&mut self) -> TermParseResult<Term> {
        self.skip_whitespace();
        let term = self.parse_term()?;
        self.skip_whitespace();
        if self.pos < self.input.len() {
            return Err(TermParseError::new(
                format!("unexpected character: '{}'", self.peek().unwrap_or('\0')),
                self.pos,
            ));
        }
        Ok(term)
    }

    fn parse_term(&mut self) -> TermParseResult<Term> {
        self.skip_whitespace();

        match self.peek() {
            Some('{') => self.parse_tuple(),
            Some('[') => self.parse_list(),
            Some('\'') => self.parse_atom(),
            Some('<') => self.parse_binary_string(),
            Some('"') => self.parse_charlist(),
            Some(c) if c.is_ascii_digit() || c == '-' => self.parse_number(),
            Some(c) if c.is_ascii_lowercase() => self.parse_bare_atom(),
            Some(c) => Err(TermParseError::new(
                format!("unexpected character: '{}'", c),
                self.pos,
            )),
            None => Err(TermParseError::new("unexpected end of input", self.pos)),
        }
    }

    fn parse_tuple(&mut self) -> TermParseResult<Term> {
        self.expect('{')?;
        let mut elements = Vec::new();

        self.skip_whitespace();
        if self.peek() == Some('}') {
            self.advance();
            return Ok(Term::Tuple(elements));
        }

        loop {
            elements.push(self.parse_term()?);
            self.skip_whitespace();

            match self.peek() {
                Some(',') => {
                    self.advance();
                    self.skip_whitespace();
                }
                Some('}') => {
                    self.advance();
                    break;
                }
                Some(c) => return Err(TermParseError::new(
                    format!("expected ',' or '}}', found '{}'", c),
                    self.pos,
                )),
                None => return Err(TermParseError::new("unclosed tuple", self.pos)),
            }
        }

        Ok(Term::Tuple(elements))
    }

    fn parse_list(&mut self) -> TermParseResult<Term> {
        self.expect('[')?;
        let mut elements = Vec::new();

        self.skip_whitespace();
        if self.peek() == Some(']') {
            self.advance();
            return Ok(Term::List(elements));
        }

        loop {
            elements.push(self.parse_term()?);
            self.skip_whitespace();

            match self.peek() {
                Some(',') => {
                    self.advance();
                    self.skip_whitespace();
                }
                Some(']') => {
                    self.advance();
                    break;
                }
                Some(c) => return Err(TermParseError::new(
                    format!("expected ',' or ']', found '{}'", c),
                    self.pos,
                )),
                None => return Err(TermParseError::new("unclosed list", self.pos)),
            }
        }

        Ok(Term::List(elements))
    }

    fn parse_atom(&mut self) -> TermParseResult<Term> {
        self.expect('\'')?;
        let mut name = String::new();

        loop {
            match self.peek() {
                Some('\'') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.peek() {
                        Some('\'') => { self.advance(); name.push('\''); }
                        Some('\\') => { self.advance(); name.push('\\'); }
                        Some('n') => { self.advance(); name.push('\n'); }
                        Some('t') => { self.advance(); name.push('\t'); }
                        Some(c) => { self.advance(); name.push(c); }
                        None => return Err(TermParseError::new("unexpected end in escape", self.pos)),
                    }
                }
                Some(c) => {
                    self.advance();
                    name.push(c);
                }
                None => return Err(TermParseError::new("unclosed atom", self.pos)),
            }
        }

        Ok(Term::Atom(name))
    }

    fn parse_bare_atom(&mut self) -> TermParseResult<Term> {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' || c == '@' {
                self.advance();
            } else {
                break;
            }
        }
        let name = &self.input[start..self.pos];
        Ok(Term::Atom(name.to_string()))
    }

    fn parse_binary_string(&mut self) -> TermParseResult<Term> {
        self.expect('<')?;
        self.expect('<')?;

        self.skip_whitespace();

        // Check if it's a quoted string or byte list
        if self.peek() == Some('"') {
            // Quoted string format: <<"hello">>
            self.advance();
            let mut content = String::new();

            loop {
                match self.peek() {
                    Some('"') => {
                        self.advance();
                        break;
                    }
                    Some('\\') => {
                        self.advance();
                        match self.peek() {
                            Some('"') => { self.advance(); content.push('"'); }
                            Some('\\') => { self.advance(); content.push('\\'); }
                            Some('n') => { self.advance(); content.push('\n'); }
                            Some('r') => { self.advance(); content.push('\r'); }
                            Some('t') => { self.advance(); content.push('\t'); }
                            Some(c) => { self.advance(); content.push(c); }
                            None => return Err(TermParseError::new("unexpected end in escape", self.pos)),
                        }
                    }
                    Some(c) => {
                        self.advance();
                        content.push(c);
                    }
                    None => return Err(TermParseError::new("unclosed binary string", self.pos)),
                }
            }

            self.expect('>')?;
            self.expect('>')?;

            Ok(Term::String(content))
        } else if self.peek() == Some('>') {
            // Empty binary: <<>>
            self.expect('>')?;
            self.expect('>')?;
            Ok(Term::String(String::new()))
        } else {
            // Byte list format: <<80,111,105,110,116>>
            let mut bytes = Vec::new();

            loop {
                self.skip_whitespace();

                if self.peek() == Some('>') {
                    break;
                }

                // Parse a byte value
                let start = self.pos;
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let num_str = &self.input[start..self.pos];
                if num_str.is_empty() {
                    return Err(TermParseError::new("expected byte value in binary", self.pos));
                }
                let byte: u8 = num_str.parse()
                    .map_err(|_| TermParseError::new(format!("invalid byte: {}", num_str), start))?;
                bytes.push(byte);

                self.skip_whitespace();
                if self.peek() == Some(',') {
                    self.advance();
                }
            }

            self.expect('>')?;
            self.expect('>')?;

            // Convert bytes to string
            let content = String::from_utf8(bytes)
                .map_err(|_| TermParseError::new("invalid UTF-8 in binary", self.pos))?;

            Ok(Term::String(content))
        }
    }

    fn parse_charlist(&mut self) -> TermParseResult<Term> {
        self.expect('"')?;

        let mut content = String::new();

        loop {
            match self.peek() {
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.peek() {
                        Some('"') => { self.advance(); content.push('"'); }
                        Some('\\') => { self.advance(); content.push('\\'); }
                        Some('n') => { self.advance(); content.push('\n'); }
                        Some('r') => { self.advance(); content.push('\r'); }
                        Some('t') => { self.advance(); content.push('\t'); }
                        Some(c) => { self.advance(); content.push(c); }
                        None => return Err(TermParseError::new("unexpected end in escape", self.pos)),
                    }
                }
                Some(c) => {
                    self.advance();
                    content.push(c);
                }
                None => return Err(TermParseError::new("unclosed string", self.pos)),
            }
        }

        // Return as string (charlists are used for format strings in Dream)
        Ok(Term::String(content))
    }

    fn parse_number(&mut self) -> TermParseResult<Term> {
        let start = self.pos;
        let mut has_dot = false;

        if self.peek() == Some('-') {
            self.advance();
        }

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else if c == '.' && !has_dot {
                has_dot = true;
                self.advance();
            } else {
                break;
            }
        }

        let num_str = &self.input[start..self.pos];

        if has_dot {
            num_str.parse::<f64>()
                .map(Term::Float)
                .map_err(|_| TermParseError::new(format!("invalid float: {}", num_str), start))
        } else {
            num_str.parse::<i64>()
                .map(Term::Int)
                .map_err(|_| TermParseError::new(format!("invalid integer: {}", num_str), start))
        }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance(&mut self) {
        if let Some(c) = self.peek() {
            self.pos += c.len_utf8();
        }
    }

    fn expect(&mut self, expected: char) -> TermParseResult<()> {
        match self.peek() {
            Some(c) if c == expected => {
                self.advance();
                Ok(())
            }
            Some(c) => Err(TermParseError::new(
                format!("expected '{}', found '{}'", expected, c),
                self.pos,
            )),
            None => Err(TermParseError::new(
                format!("expected '{}', found end of input", expected),
                self.pos,
            )),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }
}

/// Parse an Erlang term string.
pub fn parse_term(input: &str) -> TermParseResult<Term> {
    TermParser::new(input).parse()
}

// =============================================================================
// Term to AST Conversion
// =============================================================================

/// Convert an Erlang term to an Expression.
pub fn term_to_expr(term: &Term) -> TermParseResult<Expr> {
    match term {
        Term::Tuple(elements) => {
            if elements.is_empty() {
                return Ok(Expr::Unit);
            }

            let tag = match &elements[0] {
                Term::Atom(s) => s.as_str(),
                _ => return Err(TermParseError::new("expected atom tag in tuple", 0)),
            };

            match tag {
                "int" => {
                    let n = expect_int(&elements[1])?;
                    Ok(Expr::Int(n))
                }
                "string" => {
                    let s = expect_string(&elements[1])?;
                    Ok(Expr::String(s))
                }
                "charlist" => {
                    let s = expect_string(&elements[1])?;
                    Ok(Expr::Charlist(s))
                }
                "atom" => {
                    let s = expect_atom(&elements[1])?;
                    Ok(Expr::Atom(s))
                }
                "bool" => {
                    let b = expect_atom(&elements[1])?;
                    Ok(Expr::Bool(b == "true"))
                }
                "unit" => Ok(Expr::Unit),
                "ident" => {
                    let name = expect_atom(&elements[1])?;
                    Ok(Expr::Ident(name))
                }
                "path" => {
                    let segments = expect_list(&elements[1])?
                        .iter()
                        .map(expect_atom)
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Expr::Path { segments })
                }
                "binary_op" => {
                    let op = atom_to_binop(&expect_atom(&elements[1])?)?;
                    let left = term_to_expr(&elements[2])?;
                    let right = term_to_expr(&elements[3])?;
                    Ok(Expr::Binary {
                        op,
                        left: SpannedExpr::boxed(left),
                        right: SpannedExpr::boxed(right),
                    })
                }
                "unary_op" => {
                    let op = atom_to_unaryop(&expect_atom(&elements[1])?)?;
                    let expr = term_to_expr(&elements[2])?;
                    Ok(Expr::Unary {
                        op,
                        expr: SpannedExpr::boxed(expr),
                    })
                }
                "call" => {
                    let func = term_to_expr(&elements[1])?;
                    let args = expect_list(&elements[2])?
                        .iter()
                        .map(|t| term_to_expr(t).map(SpannedExpr::unspanned))
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Expr::Call {
                        func: SpannedExpr::boxed(func),
                        args,
                        type_args: vec![],
                        inferred_type_args: vec![],
                    })
                }
                "method_call" => {
                    let receiver = term_to_expr(&elements[1])?;
                    let method = expect_atom(&elements[2])?;
                    let args = expect_list(&elements[3])?
                        .iter()
                        .map(|t| term_to_expr(t).map(SpannedExpr::unspanned))
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Expr::MethodCall {
                        receiver: SpannedExpr::boxed(receiver),
                        method,
                        args,
                        type_args: vec![],
                        resolved_module: None,
                        inferred_type_args: vec![],
                    })
                }
                "field_access" => {
                    let expr = term_to_expr(&elements[1])?;
                    let field = expect_atom(&elements[2])?;
                    Ok(Expr::FieldAccess {
                        expr: SpannedExpr::boxed(expr),
                        field,
                    })
                }
                "tuple" => {
                    let elems = expect_list(&elements[1])?
                        .iter()
                        .map(|t| term_to_expr(t).map(SpannedExpr::unspanned))
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Expr::Tuple(elems))
                }
                "list" => {
                    let elems = expect_list(&elements[1])?
                        .iter()
                        .map(|t| term_to_expr(t).map(SpannedExpr::unspanned))
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Expr::List(elems))
                }
                "struct_init" => {
                    let name = expect_atom(&elements[1])?;
                    let fields = expect_list(&elements[2])?
                        .iter()
                        .map(|t| {
                            let pair = expect_tuple(t)?;
                            let fname = expect_atom(&pair[0])?;
                            let fexpr = term_to_expr(&pair[1])?;
                            Ok((fname, SpannedExpr::unspanned(fexpr)))
                        })
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Expr::StructInit {
                        name,
                        fields,
                        base: None,
                    })
                }
                "if" => {
                    let cond = term_to_expr(&elements[1])?;
                    let then_block = term_to_block(&elements[2])?;
                    let else_block = if elements.len() > 3 && !is_none(&elements[3]) {
                        Some(term_to_block(&elements[3])?)
                    } else {
                        None
                    };
                    Ok(Expr::If {
                        cond: SpannedExpr::boxed(cond),
                        then_block,
                        else_block,
                    })
                }
                "block" => {
                    let block = term_to_block(&elements[1])?;
                    Ok(Expr::Block(block))
                }
                "return" => {
                    let inner = if is_none(&elements[1]) {
                        None
                    } else {
                        Some(SpannedExpr::boxed(term_to_expr(&elements[1])?))
                    };
                    Ok(Expr::Return(inner))
                }
                "extern_call" => {
                    let module = expect_atom(&elements[1])?;
                    let function = expect_atom(&elements[2])?;
                    let args = expect_list(&elements[3])?
                        .iter()
                        .map(|t| term_to_expr(t).map(SpannedExpr::unspanned))
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Expr::ExternCall { module, function, args })
                }
                "quote" => {
                    let inner = term_to_expr(&elements[1])?;
                    Ok(Expr::Quote(SpannedExpr::boxed(inner)))
                }
                "unquote" => {
                    let inner = term_to_expr(&elements[1])?;
                    Ok(Expr::Unquote(SpannedExpr::boxed(inner)))
                }
                "unquote_splice" => {
                    let inner = term_to_expr(&elements[1])?;
                    Ok(Expr::UnquoteSplice(SpannedExpr::boxed(inner)))
                }
                "spawn" => {
                    let inner = term_to_expr(&elements[1])?;
                    Ok(Expr::Spawn(SpannedExpr::boxed(inner)))
                }
                "send" => {
                    let to = term_to_expr(&elements[1])?;
                    let msg = term_to_expr(&elements[2])?;
                    Ok(Expr::Send {
                        to: SpannedExpr::boxed(to),
                        msg: SpannedExpr::boxed(msg),
                    })
                }
                "pipe" => {
                    let left = term_to_expr(&elements[1])?;
                    let right = term_to_expr(&elements[2])?;
                    Ok(Expr::Pipe {
                        left: SpannedExpr::boxed(left),
                        right: SpannedExpr::boxed(right),
                    })
                }
                _ => Err(TermParseError::new(format!("unknown expression tag: {}", tag), 0)),
            }
        }
        _ => Err(TermParseError::new("expected tuple for expression", 0)),
    }
}

/// Convert an Erlang term to a Block.
fn term_to_block(term: &Term) -> TermParseResult<Block> {
    let tuple = expect_tuple(term)?;
    if tuple.len() != 2 {
        return Err(TermParseError::new("block should be {stmts, expr}", 0));
    }

    let stmts = expect_list(&tuple[0])?
        .iter()
        .map(term_to_stmt)
        .collect::<TermParseResult<Vec<_>>>()?;

    let expr = if is_none(&tuple[1]) {
        None
    } else {
        Some(SpannedExpr::boxed(term_to_expr(&tuple[1])?))
    };

    Ok(Block { stmts, expr, span: 0..0 })
}

/// Convert an Erlang term to a Statement.
fn term_to_stmt(term: &Term) -> TermParseResult<Stmt> {
    let tuple = expect_tuple(term)?;
    let tag = expect_atom(&tuple[0])?;

    match tag.as_str() {
        "let" => {
            let pattern = term_to_pattern(&tuple[1])?;
            let ty = if is_none(&tuple[2]) {
                None
            } else {
                Some(SpannedType::unspanned(term_to_type(&tuple[2])?))
            };
            let value = SpannedExpr::unspanned(term_to_expr(&tuple[3])?);
            let else_block = if tuple.len() > 4 && !is_none(&tuple[4]) {
                Some(term_to_block(&tuple[4])?)
            } else {
                None
            };
            Ok(Stmt::Let { pattern, ty, value, else_block, span: 0..0 })
        }
        "expr" => {
            let expr = SpannedExpr::unspanned(term_to_expr(&tuple[1])?);
            Ok(Stmt::Expr { expr, span: None })
        }
        _ => Err(TermParseError::new(format!("unknown statement tag: {}", tag), 0)),
    }
}

/// Convert an Erlang term to a Type.
pub fn term_to_type(term: &Term) -> TermParseResult<Type> {
    match term {
        Term::Tuple(elements) => {
            if elements.is_empty() {
                return Err(TermParseError::new("empty tuple for type", 0));
            }

            let tag = expect_atom(&elements[0])?;

            match tag.as_str() {
                "type" => {
                    let type_name = expect_atom(&elements[1])?;
                    match type_name.as_str() {
                        "int" => Ok(Type::Int),
                        "float" => Ok(Type::Float),
                        "string" => Ok(Type::String),
                        "atom" => Ok(Type::Atom),
                        "bool" => Ok(Type::Bool),
                        "unit" => Ok(Type::Unit),
                        "pid" => Ok(Type::Pid),
                        "ref" => Ok(Type::Ref),
                        "binary" => Ok(Type::Binary),
                        "any" => Ok(Type::Any),
                        "map" => Ok(Type::Map),
                        _ => Err(TermParseError::new(format!("unknown primitive type: {}", type_name), 0)),
                    }
                }
                "named" => {
                    let name = expect_atom(&elements[1])?;
                    let type_args = if elements.len() > 2 {
                        expect_list(&elements[2])?
                            .iter()
                            .map(term_to_type)
                            .collect::<TermParseResult<Vec<_>>>()?
                    } else {
                        vec![]
                    };
                    Ok(Type::Named { name, type_args })
                }
                "list" => {
                    let inner = term_to_type(&elements[1])?;
                    Ok(Type::List(Box::new(inner)))
                }
                "tuple" => {
                    let types = expect_list(&elements[1])?
                        .iter()
                        .map(term_to_type)
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Type::Tuple(types))
                }
                "type_var" => {
                    let name = expect_atom(&elements[1])?;
                    Ok(Type::TypeVar(name))
                }
                "fn" => {
                    let params = expect_list(&elements[1])?
                        .iter()
                        .map(term_to_type)
                        .collect::<TermParseResult<Vec<_>>>()?;
                    let ret = term_to_type(&elements[2])?;
                    Ok(Type::Fn {
                        params,
                        ret: Box::new(ret),
                    })
                }
                "atom_literal" => {
                    let name = expect_atom(&elements[1])?;
                    Ok(Type::AtomLiteral(name))
                }
                "union" => {
                    let types = expect_list(&elements[1])?
                        .iter()
                        .map(term_to_type)
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Type::Union(types))
                }
                "assoc_type" => {
                    let base = expect_atom(&elements[1])?;
                    let name = expect_atom(&elements[2])?;
                    Ok(Type::AssociatedType { base, name })
                }
                _ => Err(TermParseError::new(format!("unknown type tag: {}", tag), 0)),
            }
        }
        _ => Err(TermParseError::new("expected tuple for type", 0)),
    }
}

/// Convert an Erlang term to a Pattern.
pub fn term_to_pattern(term: &Term) -> TermParseResult<Pattern> {
    match term {
        Term::Tuple(elements) => {
            if elements.is_empty() {
                return Err(TermParseError::new("empty tuple for pattern", 0));
            }

            let tag = expect_atom(&elements[0])?;

            match tag.as_str() {
                "wildcard" => Ok(Pattern::Wildcard),
                "ident" => {
                    let name = expect_atom(&elements[1])?;
                    Ok(Pattern::Ident(name))
                }
                "int" => {
                    let n = expect_int(&elements[1])?;
                    Ok(Pattern::Int(n))
                }
                "string" => {
                    let s = expect_string(&elements[1])?;
                    Ok(Pattern::String(s))
                }
                "charlist" => {
                    let s = expect_string(&elements[1])?;
                    Ok(Pattern::Charlist(s))
                }
                "atom" => {
                    let s = expect_atom(&elements[1])?;
                    Ok(Pattern::Atom(s))
                }
                "bool" => {
                    let b = expect_atom(&elements[1])?;
                    Ok(Pattern::Bool(b == "true"))
                }
                "tuple" => {
                    let pats = expect_list(&elements[1])?
                        .iter()
                        .map(term_to_pattern)
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Pattern::Tuple(pats))
                }
                "list" => {
                    let pats = expect_list(&elements[1])?
                        .iter()
                        .map(term_to_pattern)
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Pattern::List(pats))
                }
                "list_cons" => {
                    let head = term_to_pattern(&elements[1])?;
                    let tail = term_to_pattern(&elements[2])?;
                    Ok(Pattern::ListCons {
                        head: Box::new(head),
                        tail: Box::new(tail),
                    })
                }
                "struct" => {
                    let name = expect_atom(&elements[1])?;
                    let fields = expect_list(&elements[2])?
                        .iter()
                        .map(|t| {
                            let pair = expect_tuple(t)?;
                            let fname = expect_atom(&pair[0])?;
                            let fpat = term_to_pattern(&pair[1])?;
                            Ok((fname, fpat))
                        })
                        .collect::<TermParseResult<Vec<_>>>()?;
                    Ok(Pattern::Struct { name, fields })
                }
                _ => Err(TermParseError::new(format!("unknown pattern tag: {}", tag), 0)),
            }
        }
        _ => Err(TermParseError::new("expected tuple for pattern", 0)),
    }
}

/// Convert an Erlang term to an ImplBlock (main output of derive macros).
pub fn term_to_impl_block(term: &Term) -> TermParseResult<ImplBlock> {
    let tuple = expect_tuple(term)?;
    let tag = expect_atom(&tuple[0])?;

    if tag != "impl" {
        return Err(TermParseError::new(format!("expected 'impl' tag, found '{}'", tag), 0));
    }

    let type_name = expect_atom(&tuple[1])?;
    let methods = expect_list(&tuple[2])?
        .iter()
        .map(term_to_function)
        .collect::<TermParseResult<Vec<_>>>()?;

    Ok(ImplBlock { type_name, methods, span: 0..0 })
}

/// Convert an Erlang term to a TraitImpl.
/// Format: {traitimpl, TraitName, TypeName, [Methods]}
pub fn term_to_trait_impl(term: &Term) -> TermParseResult<TraitImpl> {
    let tuple = expect_tuple(term)?;
    let tag = expect_atom(&tuple[0])?;

    if tag != "traitimpl" {
        return Err(TermParseError::new(format!("expected 'traitimpl' tag, found '{}'", tag), 0));
    }

    let trait_name = expect_atom(&tuple[1])?;
    let type_name = expect_atom(&tuple[2])?;
    let methods = expect_list(&tuple[3])?
        .iter()
        .map(term_to_function)
        .collect::<TermParseResult<Vec<_>>>()?;

    Ok(TraitImpl {
        trait_name,
        trait_type_args: vec![],
        type_name,
        type_bindings: vec![],
        methods,
        span: 0..0,
    })
}

/// Convert an Erlang term to a Function.
pub fn term_to_function(term: &Term) -> TermParseResult<Function> {
    let tuple = expect_tuple(term)?;
    let tag = expect_atom(&tuple[0])?;

    if tag != "function" {
        return Err(TermParseError::new(format!("expected 'function' tag, found '{}'", tag), 0));
    }

    let name = expect_atom(&tuple[1])?;
    let _type_params = expect_list(&tuple[2])?; // TODO: parse type params
    let params = expect_list(&tuple[3])?
        .iter()
        .map(term_to_param)
        .collect::<TermParseResult<Vec<_>>>()?;
    let return_type = if is_none(&tuple[4]) {
        None
    } else {
        Some(SpannedType::unspanned(term_to_type(&tuple[4])?))
    };
    let body = term_to_block(&tuple[5])?;

    Ok(Function {
        attrs: vec![],
        name,
        type_params: vec![],
        params,
        guard: None,
        return_type,
        body,
        is_pub: true,
        span: crate::compiler::lexer::Span::default(),
    })
}

/// Convert an Erlang term to a Param.
fn term_to_param(term: &Term) -> TermParseResult<Param> {
    let tuple = expect_tuple(term)?;
    let pattern = term_to_pattern(&tuple[0])?;
    let ty = SpannedType::unspanned(term_to_type(&tuple[1])?);
    Ok(Param { pattern, ty })
}

/// Convert an Erlang term to an Item.
pub fn term_to_item(term: &Term) -> TermParseResult<Item> {
    let tuple = expect_tuple(term)?;
    let tag = expect_atom(&tuple[0])?;

    match tag.as_str() {
        "impl" => Ok(Item::Impl(term_to_impl_block(term)?)),
        "traitimpl" => Ok(Item::TraitImpl(term_to_trait_impl(term)?)),
        "function" => Ok(Item::Function(term_to_function(term)?)),
        _ => Err(TermParseError::new(format!("unsupported item tag: {}", tag), 0)),
    }
}

// =============================================================================
// Helper functions for term extraction
// =============================================================================

fn expect_atom(term: &Term) -> TermParseResult<String> {
    match term {
        Term::Atom(s) => Ok(s.clone()),
        _ => Err(TermParseError::new("expected atom", 0)),
    }
}

fn expect_int(term: &Term) -> TermParseResult<i64> {
    match term {
        Term::Int(n) => Ok(*n),
        _ => Err(TermParseError::new("expected integer", 0)),
    }
}

fn expect_string(term: &Term) -> TermParseResult<String> {
    match term {
        Term::String(s) => Ok(s.clone()),
        _ => Err(TermParseError::new("expected string", 0)),
    }
}

fn expect_list(term: &Term) -> TermParseResult<&Vec<Term>> {
    match term {
        Term::List(items) => Ok(items),
        _ => Err(TermParseError::new("expected list", 0)),
    }
}

fn expect_tuple(term: &Term) -> TermParseResult<&Vec<Term>> {
    match term {
        Term::Tuple(items) => Ok(items),
        _ => Err(TermParseError::new("expected tuple", 0)),
    }
}

fn is_none(term: &Term) -> bool {
    matches!(term, Term::Atom(s) if s == "none")
}

fn atom_to_binop(s: &str) -> TermParseResult<BinOp> {
    match s {
        "+" => Ok(BinOp::Add),
        "-" => Ok(BinOp::Sub),
        "*" => Ok(BinOp::Mul),
        "/" => Ok(BinOp::Div),
        "rem" => Ok(BinOp::Mod),
        "==" => Ok(BinOp::Eq),
        "/=" => Ok(BinOp::Ne),
        "<" => Ok(BinOp::Lt),
        "=<" => Ok(BinOp::Le),
        ">" => Ok(BinOp::Gt),
        ">=" => Ok(BinOp::Ge),
        "and" => Ok(BinOp::And),
        "or" => Ok(BinOp::Or),
        _ => Err(TermParseError::new(format!("unknown binary op: {}", s), 0)),
    }
}

fn atom_to_unaryop(s: &str) -> TermParseResult<UnaryOp> {
    match s {
        "-" => Ok(UnaryOp::Neg),
        "not" => Ok(UnaryOp::Not),
        _ => Err(TermParseError::new(format!("unknown unary op: {}", s), 0)),
    }
}

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
            left: SpannedExpr::boxed(Expr::Int(1)),
            right: SpannedExpr::boxed(Expr::Int(2)),
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
                ("x".to_string(), SpannedType::unspanned(Type::Int)),
                ("y".to_string(), SpannedType::unspanned(Type::Int)),
            ],
            attrs: vec![],
            span: 0..0,
        };
        assert_eq!(
            struct_def_to_erlang_term(&s),
            "{struct, 'Point', [{'x', {type, int}}, {'y', {type, int}}], []}"
        );
    }

    // =============================================================================
    // Term Parser Tests
    // =============================================================================

    #[test]
    fn test_parse_int() {
        let term = parse_term("42").unwrap();
        assert_eq!(term, Term::Int(42));
    }

    #[test]
    fn test_parse_negative_int() {
        let term = parse_term("-42").unwrap();
        assert_eq!(term, Term::Int(-42));
    }

    #[test]
    fn test_parse_atom() {
        let term = parse_term("'hello'").unwrap();
        assert_eq!(term, Term::Atom("hello".to_string()));
    }

    #[test]
    fn test_parse_bare_atom() {
        let term = parse_term("hello").unwrap();
        assert_eq!(term, Term::Atom("hello".to_string()));
    }

    #[test]
    fn test_parse_binary_string() {
        let term = parse_term("<<\"hello\">>").unwrap();
        assert_eq!(term, Term::String("hello".to_string()));
    }

    #[test]
    fn test_parse_tuple() {
        let term = parse_term("{int, 42}").unwrap();
        assert_eq!(term, Term::Tuple(vec![
            Term::Atom("int".to_string()),
            Term::Int(42),
        ]));
    }

    #[test]
    fn test_parse_list() {
        let term = parse_term("[1, 2, 3]").unwrap();
        assert_eq!(term, Term::List(vec![
            Term::Int(1),
            Term::Int(2),
            Term::Int(3),
        ]));
    }

    #[test]
    fn test_parse_nested() {
        let term = parse_term("{binary_op, '+', {int, 1}, {int, 2}}").unwrap();
        assert_eq!(term, Term::Tuple(vec![
            Term::Atom("binary_op".to_string()),
            Term::Atom("+".to_string()),
            Term::Tuple(vec![Term::Atom("int".to_string()), Term::Int(1)]),
            Term::Tuple(vec![Term::Atom("int".to_string()), Term::Int(2)]),
        ]));
    }

    // =============================================================================
    // Term to AST Conversion Tests
    // =============================================================================

    #[test]
    fn test_term_to_expr_int() {
        let term = parse_term("{int, 42}").unwrap();
        let expr = term_to_expr(&term).unwrap();
        assert_eq!(expr, Expr::Int(42));
    }

    #[test]
    fn test_term_to_expr_string() {
        let term = parse_term("{string, <<\"hello\">>}").unwrap();
        let expr = term_to_expr(&term).unwrap();
        assert_eq!(expr, Expr::String("hello".to_string()));
    }

    #[test]
    fn test_term_to_expr_ident() {
        let term = parse_term("{ident, 'foo'}").unwrap();
        let expr = term_to_expr(&term).unwrap();
        assert_eq!(expr, Expr::Ident("foo".to_string()));
    }

    #[test]
    fn test_term_to_expr_binary_op() {
        let term = parse_term("{binary_op, '+', {int, 1}, {int, 2}}").unwrap();
        let expr = term_to_expr(&term).unwrap();
        assert_eq!(expr, Expr::Binary {
            op: BinOp::Add,
            left: SpannedExpr::boxed(Expr::Int(1)),
            right: SpannedExpr::boxed(Expr::Int(2)),
        });
    }

    #[test]
    fn test_term_to_type_int() {
        let term = parse_term("{type, int}").unwrap();
        let ty = term_to_type(&term).unwrap();
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn test_term_to_type_named() {
        let term = parse_term("{named, 'Point'}").unwrap();
        let ty = term_to_type(&term).unwrap();
        assert_eq!(ty, Type::Named { name: "Point".to_string(), type_args: vec![] });
    }

    // =============================================================================
    // Round-trip Tests (serialize then deserialize)
    // =============================================================================

    #[test]
    fn test_roundtrip_expr_int() {
        let original = Expr::Int(42);
        let term_str = expr_to_erlang_term(&original);
        let term = parse_term(&term_str).unwrap();
        let result = term_to_expr(&term).unwrap();
        assert_eq!(result, original);
    }

    #[test]
    fn test_roundtrip_expr_string() {
        let original = Expr::String("hello world".to_string());
        let term_str = expr_to_erlang_term(&original);
        let term = parse_term(&term_str).unwrap();
        let result = term_to_expr(&term).unwrap();
        assert_eq!(result, original);
    }

    #[test]
    fn test_roundtrip_expr_binary_op() {
        let original = Expr::Binary {
            op: BinOp::Add,
            left: SpannedExpr::boxed(Expr::Int(1)),
            right: SpannedExpr::boxed(Expr::Int(2)),
        };
        let term_str = expr_to_erlang_term(&original);
        let term = parse_term(&term_str).unwrap();
        let result = term_to_expr(&term).unwrap();
        assert_eq!(result, original);
    }

    #[test]
    fn test_roundtrip_type_int() {
        let original = Type::Int;
        let term_str = type_to_erlang_term(&original);
        let term = parse_term(&term_str).unwrap();
        let result = term_to_type(&term).unwrap();
        assert_eq!(result, original);
    }

    #[test]
    fn test_roundtrip_type_named() {
        let original = Type::Named { name: "Point".to_string(), type_args: vec![] };
        let term_str = type_to_erlang_term(&original);
        let term = parse_term(&term_str).unwrap();
        let result = term_to_type(&term).unwrap();
        assert_eq!(result, original);
    }

    // =============================================================================
    // TokenStream Serialization Tests
    // =============================================================================

    #[test]
    fn test_struct_to_token_stream() {
        let s = StructDef {
            is_pub: true,
            name: "User".to_string(),
            type_params: vec![],
            fields: vec![
                ("id".to_string(), SpannedType::unspanned(Type::Int)),
                ("name".to_string(), SpannedType::unspanned(Type::String)),
            ],
            attrs: vec![],
            span: 0..0,
        };
        let tokens = struct_to_token_stream(&s);
        // Should contain keyword pub, keyword struct, type_ident User, and a group with fields
        assert!(tokens.contains("{keyword, pub}"));
        assert!(tokens.contains("{keyword, struct}"));
        assert!(tokens.contains("{type_ident, 'User'}"));
        assert!(tokens.contains("{ident, 'id'}"));
        assert!(tokens.contains("{ident, 'name'}"));
        assert!(tokens.contains("{keyword, int}"));
        assert!(tokens.contains("{keyword, string}"));
    }

    #[test]
    fn test_struct_with_generics_to_token_stream() {
        let s = StructDef {
            is_pub: true,
            name: "Container".to_string(),
            type_params: vec![
                TypeParam { name: "T".to_string(), bounds: vec![] },
            ],
            fields: vec![
                ("value".to_string(), SpannedType::unspanned(Type::TypeVar("T".to_string()))),
            ],
            attrs: vec![],
            span: 0..0,
        };
        let tokens = struct_to_token_stream(&s);
        // Should contain the type parameter
        assert!(tokens.contains("{type_ident, 'Container'}"));
        assert!(tokens.contains("{type_ident, 'T'}"));
        assert!(tokens.contains("group, angle"));
    }

    #[test]
    fn test_enum_to_token_stream() {
        let e = EnumDef {
            is_pub: true,
            name: "Option".to_string(),
            type_params: vec![
                TypeParam { name: "T".to_string(), bounds: vec![] },
            ],
            variants: vec![
                EnumVariant { name: "Some".to_string(), kind: VariantKind::Tuple(vec![SpannedType::unspanned(Type::TypeVar("T".to_string()))]) },
                EnumVariant { name: "None".to_string(), kind: VariantKind::Unit },
            ],
            attrs: vec![],
            span: 0..0,
        };
        let tokens = enum_to_token_stream(&e);
        // Should contain keyword enum, type_ident Option, variants
        assert!(tokens.contains("{keyword, pub}"));
        assert!(tokens.contains("{keyword, enum}"));
        assert!(tokens.contains("{type_ident, 'Option'}"));
        assert!(tokens.contains("{type_ident, 'Some'}"));
        assert!(tokens.contains("{type_ident, 'None'}"));
    }
}
