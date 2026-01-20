//! Expression building for Core Erlang AST.

use rustler::{Atom, Encoder, Env, NifResult, Term};
use surreal::compiler::{BinOp, Block, Expr, MatchArm, SpannedExpr, Stmt, UnaryOp};
use crate::atoms;
use super::context::BuildContext;
use super::patterns::build_pattern;
use super::primitives::{build_c_literal, build_c_var, build_c_var_simple};

/// Build a block expression.
pub fn build_block<'a>(env: Env<'a>, ctx: &BuildContext, block: &Block) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    // Get the final value of the block
    let final_expr = if let Some(ref expr) = block.expr {
        build_expr(env, ctx, expr)?
    } else if let Some(last_stmt) = block.stmts.last() {
        match last_stmt {
            Stmt::Expr { expr, .. } => build_expr(env, ctx, expr)?,
            Stmt::Let { .. } => {
                // Let binding without trailing expr - return :ok
                let atom = Atom::from_str(env, "ok")?;
                (atoms::c_literal(), empty_list.clone(), atom).encode(env)
            }
        }
    } else {
        // Empty block returns :ok
        let atom = Atom::from_str(env, "ok")?;
        (atoms::c_literal(), empty_list.clone(), atom).encode(env)
    };

    // Process statements in reverse to build nested let expressions
    // let x = 1; let y = 2; expr  =>  let x = 1 in (let y = 2 in expr)
    let stmts_to_process: Vec<_> = if block.expr.is_some() {
        // All statements need to be wrapped
        block.stmts.iter().collect()
    } else if !block.stmts.is_empty() {
        // All but the last statement (which becomes the final value)
        block.stmts[..block.stmts.len().saturating_sub(1)].iter().collect()
    } else {
        vec![]
    };

    // Fold from right to left, wrapping in c_let
    stmts_to_process.iter().rev().try_fold(final_expr, |body, stmt| {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                // {c_let, [], [Vars], Arg, Body}
                let var_term = build_pattern(env, pattern)?;
                let val_term = build_expr(env, ctx, value)?;
                Ok((atoms::c_let(), empty_list.clone(), vec![var_term], val_term, body).encode(env))
            }
            Stmt::Expr { expr, .. } => {
                // Expression statement - use c_seq to sequence effects
                // {c_seq, [], Arg, Body}
                let expr_term = build_expr(env, ctx, expr)?;
                Ok((atoms::c_seq(), empty_list.clone(), expr_term, body).encode(env))
            }
        }
    })
}

/// Build an expression.
pub fn build_expr<'a>(env: Env<'a>, ctx: &BuildContext, expr: &Expr) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    match expr {
        Expr::Atom(name) => {
            let atom = Atom::from_str(env, name)?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
        Expr::Int(n) => {
            Ok((atoms::c_literal(), empty_list, *n).encode(env))
        }
        Expr::String(s) => {
            // Strings in Core Erlang are binaries
            Ok((atoms::c_literal(), empty_list, s.as_bytes()).encode(env))
        }
        Expr::Charlist(s) => {
            // Charlist is a list of codepoints - compile as literal list
            let chars: Vec<i64> = s.chars().map(|c| c as i64).collect();
            Ok((atoms::c_literal(), empty_list, chars).encode(env))
        }
        Expr::Bool(b) => {
            let atom = Atom::from_str(env, if *b { "true" } else { "false" })?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
        Expr::Unit => {
            let atom = Atom::from_str(env, "ok")?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
        Expr::Ident(name) => {
            let name_atom = Atom::from_str(env, name)?;
            Ok((atoms::c_var(), empty_list, name_atom).encode(env))
        }
        Expr::Tuple(elements) => {
            let elems: Vec<Term<'a>> = elements.iter()
                .map(|e| build_expr(env, ctx, e))
                .collect::<Result<Vec<_>, _>>()?;
            Ok((atoms::c_tuple(), empty_list, elems).encode(env))
        }
        Expr::List(elements) => {
            let nil = (atoms::c_nil(), empty_list.clone()).encode(env);
            elements.iter().rev().try_fold(nil, |acc, elem| {
                let e = build_expr(env, ctx, elem)?;
                Ok((atoms::c_cons(), empty_list.clone(), e, acc).encode(env))
            })
        }
        Expr::ListCons { head, tail } => {
            let h = build_expr(env, ctx, head)?;
            let t = build_expr(env, ctx, tail)?;
            Ok((atoms::c_cons(), empty_list, h, t).encode(env))
        }
        Expr::Block(block) => {
            build_block(env, ctx, block)
        }
        Expr::Call { func, args, .. } => {
            build_call(env, ctx, func, args)
        }
        Expr::ExternCall { module, function, args } => {
            build_extern_call(env, ctx, module, function, args)
        }
        Expr::Binary { op, left, right } => {
            build_binary_op(env, ctx, op, left, right)
        }
        Expr::Unary { op, expr: inner } => {
            build_unary_op(env, ctx, op, inner)
        }
        Expr::If { cond, then_block, else_block } => {
            build_if(env, ctx, cond, then_block, else_block)
        }
        Expr::Match { expr: scrutinee, arms } => {
            build_match(env, ctx, scrutinee, arms)
        }
        Expr::Receive { arms, timeout } => {
            build_receive(env, ctx, arms, timeout)
        }
        Expr::Closure { params, body } => {
            build_closure(env, ctx, params, body)
        }
        Expr::MapLiteral(pairs) => {
            build_map(env, ctx, pairs)
        }
        Expr::Path { segments } => {
            // Module path as value - emit as atom with surreal:: prefix
            let path = segments.join("::");
            let resolved = format!("surreal::{}", path);
            let atom = Atom::from_str(env, &resolved)?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
        Expr::Send { to, msg } => {
            build_extern_call(env, ctx, "erlang", "send",
                &vec![(**to).clone(), (**msg).clone()])
        }
        Expr::Spawn(inner) => {
            build_extern_call(env, ctx, "erlang", "spawn",
                &vec![(**inner).clone()])
        }
        Expr::Return(maybe_expr) => {
            match maybe_expr {
                Some(e) => build_expr(env, ctx, e),
                None => {
                    let atom = Atom::from_str(env, "ok")?;
                    Ok((atoms::c_literal(), empty_list, atom).encode(env))
                }
            }
        }
        // TODO: Handle more expression types
        _ => {
            let atom = Atom::from_str(env, "not_implemented")?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
    }
}

/// Build a function call expression.
fn build_call<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    func: &SpannedExpr,
    args: &[SpannedExpr],
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let arg_terms: Vec<Term<'a>> = args.iter()
        .map(|a| build_expr(env, ctx, a))
        .collect::<Result<Vec<_>, _>>()?;

    match &func.expr {
        Expr::Ident(name) => {
            // Check if this is a local function or a variable
            if let Some(arity) = ctx.get_function_arity(name) {
                let fun_var = build_c_var(env, name, arity)?;
                Ok((atoms::c_apply(), empty_list, fun_var, arg_terms).encode(env))
            } else {
                let fun_var = build_c_var_simple(env, name)?;
                Ok((atoms::c_apply(), empty_list, fun_var, arg_terms).encode(env))
            }
        }
        Expr::Path { segments } => {
            if segments.len() >= 2 {
                let func_name = segments.last().unwrap();
                let module_path = segments[..segments.len()-1].join("::");
                let resolved_module = format!("surreal::{}", module_path);

                let module_lit = build_c_literal(env, &resolved_module)?;
                let func_lit = build_c_literal(env, func_name)?;

                Ok((atoms::c_call(), empty_list, module_lit, func_lit, arg_terms).encode(env))
            } else {
                let name = &segments[0];
                if let Some(arity) = ctx.get_function_arity(name) {
                    let fun_var = build_c_var(env, name, arity)?;
                    Ok((atoms::c_apply(), empty_list, fun_var, arg_terms).encode(env))
                } else {
                    let fun_var = build_c_var_simple(env, name)?;
                    Ok((atoms::c_apply(), empty_list, fun_var, arg_terms).encode(env))
                }
            }
        }
        _ => {
            let fun_term = build_expr(env, ctx, func)?;
            Ok((atoms::c_apply(), empty_list, fun_term, arg_terms).encode(env))
        }
    }
}

/// Build an external function call (erlang/elixir interop).
fn build_extern_call<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    module: &str,
    function: &str,
    args: &[SpannedExpr],
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let module_lit = build_c_literal(env, module)?;
    let func_lit = build_c_literal(env, function)?;

    let arg_terms: Vec<Term<'a>> = args.iter()
        .map(|a| build_expr(env, ctx, a))
        .collect::<Result<Vec<_>, _>>()?;

    Ok((atoms::c_call(), empty_list, module_lit, func_lit, arg_terms).encode(env))
}

/// Build a binary operation.
fn build_binary_op<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    op: &BinOp,
    left: &SpannedExpr,
    right: &SpannedExpr,
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let (module, func) = match op {
        BinOp::Add => ("erlang", "+"),
        BinOp::Sub => ("erlang", "-"),
        BinOp::Mul => ("erlang", "*"),
        BinOp::Div => ("erlang", "/"),
        BinOp::Mod => ("erlang", "rem"),
        BinOp::Eq => ("erlang", "=:="),
        BinOp::Ne => ("erlang", "=/="),
        BinOp::Lt => ("erlang", "<"),
        BinOp::Le => ("erlang", "=<"),
        BinOp::Gt => ("erlang", ">"),
        BinOp::Ge => ("erlang", ">="),
        BinOp::And => ("erlang", "and"),
        BinOp::Or => ("erlang", "or"),
    };

    let module_lit = build_c_literal(env, module)?;
    let func_lit = build_c_literal(env, func)?;

    let left_term = build_expr(env, ctx, left)?;
    let right_term = build_expr(env, ctx, right)?;

    Ok((atoms::c_call(), empty_list, module_lit, func_lit, vec![left_term, right_term]).encode(env))
}

/// Build a unary operation.
fn build_unary_op<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    op: &UnaryOp,
    expr: &SpannedExpr,
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let (module, func) = match op {
        UnaryOp::Neg => ("erlang", "-"),
        UnaryOp::Not => ("erlang", "not"),
    };

    let module_lit = build_c_literal(env, module)?;
    let func_lit = build_c_literal(env, func)?;

    let expr_term = build_expr(env, ctx, expr)?;

    Ok((atoms::c_call(), empty_list, module_lit, func_lit, vec![expr_term]).encode(env))
}

/// Build an if expression (compiles to c_case).
fn build_if<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    cond: &SpannedExpr,
    then_block: &Block,
    else_block: &Option<Block>,
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let cond_term = build_expr(env, ctx, cond)?;
    let then_term = build_block(env, ctx, then_block)?;

    let else_term = match else_block {
        Some(block) => build_block(env, ctx, block)?,
        None => {
            let ok_atom = Atom::from_str(env, "ok")?;
            (atoms::c_literal(), empty_list.clone(), ok_atom).encode(env)
        }
    };

    let true_atom = Atom::from_str(env, "true")?;
    let true_lit = (atoms::c_literal(), empty_list.clone(), true_atom).encode(env);
    let true_clause = (
        atoms::c_clause(),
        empty_list.clone(),
        vec![true_lit.clone()],
        true_lit.clone(),
        then_term,
    ).encode(env);

    let false_atom = Atom::from_str(env, "false")?;
    let false_lit = (atoms::c_literal(), empty_list.clone(), false_atom).encode(env);
    let false_clause = (
        atoms::c_clause(),
        empty_list.clone(),
        vec![false_lit],
        true_lit,
        else_term,
    ).encode(env);

    Ok((atoms::c_case(), empty_list, cond_term, vec![true_clause, false_clause]).encode(env))
}

/// Build a match expression.
fn build_match<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    scrutinee: &SpannedExpr,
    arms: &[MatchArm],
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let scrutinee_term = build_expr(env, ctx, scrutinee)?;

    let clauses: Vec<Term<'a>> = arms.iter()
        .map(|arm| build_clause(env, ctx, arm))
        .collect::<Result<Vec<_>, _>>()?;

    Ok((atoms::c_case(), empty_list, scrutinee_term, clauses).encode(env))
}

/// Build a match clause.
fn build_clause<'a>(env: Env<'a>, ctx: &BuildContext, arm: &MatchArm) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let pattern_term = build_pattern(env, &arm.pattern)?;
    let body_term = build_expr(env, ctx, &arm.body)?;

    let guard_term = match &arm.guard {
        Some(g) => build_expr(env, ctx, g)?,
        None => {
            let true_atom = Atom::from_str(env, "true")?;
            (atoms::c_literal(), empty_list.clone(), true_atom).encode(env)
        }
    };

    Ok((atoms::c_clause(), empty_list, vec![pattern_term], guard_term, body_term).encode(env))
}

/// Build a receive expression.
fn build_receive<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    arms: &[MatchArm],
    timeout: &Option<(Box<SpannedExpr>, Block)>,
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let clauses: Vec<Term<'a>> = arms.iter()
        .map(|arm| build_clause(env, ctx, arm))
        .collect::<Result<Vec<_>, _>>()?;

    match timeout {
        Some((timeout_expr, timeout_body)) => {
            let timeout_term = build_expr(env, ctx, timeout_expr)?;
            let timeout_action = build_block(env, ctx, timeout_body)?;
            Ok((atoms::c_receive(), empty_list, clauses, timeout_term, timeout_action).encode(env))
        }
        None => {
            let infinity = Atom::from_str(env, "infinity")?;
            let infinity_lit = (atoms::c_literal(), empty_list.clone(), infinity).encode(env);
            let ok_atom = Atom::from_str(env, "ok")?;
            let ok_lit = (atoms::c_literal(), empty_list.clone(), ok_atom).encode(env);
            Ok((atoms::c_receive(), empty_list, clauses, infinity_lit, ok_lit).encode(env))
        }
    }
}

/// Build a closure (anonymous function).
fn build_closure<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    params: &[String],
    body: &Block,
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let param_terms: Vec<Term<'a>> = params.iter()
        .map(|name| build_c_var_simple(env, name))
        .collect::<Result<Vec<_>, _>>()?;

    let body_term = build_block(env, ctx, body)?;

    Ok((atoms::c_fun(), empty_list, param_terms, body_term).encode(env))
}

/// Build a map literal.
fn build_map<'a>(
    env: Env<'a>,
    ctx: &BuildContext,
    pairs: &[(SpannedExpr, SpannedExpr)],
) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    let pair_terms: Vec<Term<'a>> = pairs.iter()
        .map(|(k, v)| {
            let key = build_expr(env, ctx, k)?;
            let val = build_expr(env, ctx, v)?;
            let assoc = Atom::from_str(env, "assoc")?;
            Ok((atoms::c_map_pair(), empty_list.clone(), assoc, key, val).encode(env))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let empty_map = (atoms::c_literal(), empty_list.clone(), Atom::from_str(env, "#{}").ok()).encode(env);
    Ok((atoms::c_map(), empty_list, empty_map, pair_terms).encode(env))
}
