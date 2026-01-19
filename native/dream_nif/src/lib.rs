//! Dream Compiler NIF
//!
//! Exposes the Dream compiler to Erlang/Elixir via Rustler NIFs.
//! This enables in-memory compilation without disk I/O or erlc process spawning.

use rustler::{Atom, Encoder, Env, NifResult, Term};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        // Core Erlang AST atoms
        c_module,
        c_literal,
        c_var,
        c_fun,
        c_apply,
        c_call,
        c_case,
        c_clause,
        c_let,
        c_letrec,
        c_tuple,
        c_cons,
        c_nil,
        c_primop,
        c_try,
        c_catch,
        c_receive,
        c_binary,
        c_bitstr,
        c_map,
        c_map_pair,
        c_alias,
        c_values,
        c_seq,
    }
}

/// Parse Dream source code and return module information.
///
/// Returns: {:ok, %{name: atom, functions: [{name, arity}, ...]}} | {:error, reason}
#[rustler::nif]
fn parse<'a>(env: Env<'a>, source: String) -> Term<'a> {
    use dream::compiler::Parser;

    let mut parser = Parser::new(&source);

    match parser.parse_file_modules("input") {
        Ok(modules) => {
            // Return basic module info
            let module_infos: Vec<_> = modules.iter().map(|m| {
                let name = m.name.as_str();
                let funcs: Vec<_> = m.items.iter().filter_map(|item| {
                    if let dream::compiler::Item::Function(f) = item {
                        Some((f.name.as_str(), f.params.len()))
                    } else {
                        None
                    }
                }).collect();
                (name, funcs)
            }).collect();

            (atoms::ok(), module_infos).encode(env)
        }
        Err(error) => {
            // ParseError is a single error, not a vec
            (atoms::error(), error.message.clone()).encode(env)
        }
    }
}

/// Generate Core Erlang AST from Dream source code.
///
/// Returns: {:ok, CoreErlangAST} | {:error, reason}
///
/// The CoreErlangAST is in the format expected by compile:forms/2 with [from_core, binary].
#[rustler::nif]
fn generate_core_ast<'a>(env: Env<'a>, source: String) -> Term<'a> {
    use dream::compiler::{Parser, check_modules};

    let mut parser = Parser::new(&source);

    // Parse
    let modules = match parser.parse_file_modules("input") {
        Ok(m) => m,
        Err(error) => {
            return (atoms::error(), error.message.clone()).encode(env);
        }
    };

    if modules.is_empty() {
        return (atoms::error(), "no modules found").encode(env);
    }

    // Type check - returns Vec<(String, Result<Module, TypeError>)>
    let results = check_modules(&modules);

    // Collect any type errors
    let type_errors: Vec<String> = results.iter()
        .filter_map(|(name, result)| {
            if let Err(e) = result {
                Some(format!("{}: {}", name, e.message))
            } else {
                None
            }
        })
        .collect();

    if !type_errors.is_empty() {
        return (atoms::error(), type_errors).encode(env);
    }

    // Get the first successfully typed module
    let typed_module = match results.into_iter().next() {
        Some((_, Ok(m))) => m,
        _ => return (atoms::error(), "no modules type checked").encode(env),
    };

    // Generate Core Erlang AST for the module
    match build_core_ast(env, &typed_module) {
        Ok(core_ast) => (atoms::ok(), core_ast).encode(env),
        Err(e) => (atoms::error(), format!("AST generation failed: {:?}", e)).encode(env),
    }
}

/// Build Core Erlang AST as Erlang terms.
fn build_core_ast<'a>(env: Env<'a>, module: &dream::compiler::Module) -> NifResult<Term<'a>> {
    // Build module name as c_literal
    let module_name = build_c_literal(env, &module.name)?;

    // Build exports list
    let exports: Vec<Term<'a>> = module.items.iter().filter_map(|item| {
        if let dream::compiler::Item::Function(f) = item {
            if f.is_pub {
                Some(build_c_var(env, &f.name, f.params.len()))
            } else {
                None
            }
        } else {
            None
        }
    }).collect::<Result<Vec<_>, _>>()?;

    // Build function definitions
    let fundefs: Vec<Term<'a>> = module.items.iter().filter_map(|item| {
        if let dream::compiler::Item::Function(f) = item {
            match build_fundef(env, f) {
                Ok(fundef) => Some(Ok(fundef)),
                Err(e) => Some(Err(e)),
            }
        } else {
            None
        }
    }).collect::<Result<Vec<_>, _>>()?;

    // Build c_module tuple
    // {c_module, [], ModuleName, Exports, Attributes, FunDefs}
    let empty_list: Vec<Term<'a>> = vec![];
    let c_module = (
        atoms::c_module(),
        empty_list.clone(),  // annotations
        module_name,
        exports,
        empty_list,  // attributes
        fundefs,
    );

    Ok(c_module.encode(env))
}

fn build_c_literal<'a>(env: Env<'a>, value: &str) -> NifResult<Term<'a>> {
    // {c_literal, [], Value}
    let empty_list: Vec<Term<'a>> = vec![];
    let atom = Atom::from_str(env, value)?;
    Ok((atoms::c_literal(), empty_list, atom).encode(env))
}

fn build_c_var<'a>(env: Env<'a>, name: &str, arity: usize) -> NifResult<Term<'a>> {
    // {c_var, [], {Name, Arity}}
    let empty_list: Vec<Term<'a>> = vec![];
    let name_atom = Atom::from_str(env, name)?;
    Ok((atoms::c_var(), empty_list, (name_atom, arity)).encode(env))
}

fn build_fundef<'a>(env: Env<'a>, func: &dream::compiler::Function) -> NifResult<Term<'a>> {
    // {{c_var, [], {Name, Arity}}, {c_fun, [], Params, Body}}
    let var = build_c_var(env, &func.name, func.params.len())?;
    let fun = build_c_fun(env, func)?;
    Ok((var, fun).encode(env))
}

fn build_c_fun<'a>(env: Env<'a>, func: &dream::compiler::Function) -> NifResult<Term<'a>> {
    // {c_fun, [], Params, Body}
    let empty_list: Vec<Term<'a>> = vec![];

    // Build params as c_var terms - extract name from pattern
    let params: Vec<Term<'a>> = func.params.iter().filter_map(|p| {
        // Extract variable name from pattern
        if let dream::compiler::Pattern::Ident(name) = &p.pattern {
            Some(build_c_var_simple(env, name))
        } else {
            // For complex patterns, generate a placeholder name
            Some(build_c_var_simple(env, "_"))
        }
    }).collect::<Result<Vec<_>, _>>()?;

    // Build body from block
    let body = build_block(env, &func.body)?;

    Ok((atoms::c_fun(), empty_list, params, body).encode(env))
}

fn build_c_var_simple<'a>(env: Env<'a>, name: &str) -> NifResult<Term<'a>> {
    // {c_var, [], Name} - for local variables (no arity)
    let empty_list: Vec<Term<'a>> = vec![];
    let name_atom = Atom::from_str(env, name)?;
    Ok((atoms::c_var(), empty_list, name_atom).encode(env))
}

fn build_block<'a>(env: Env<'a>, block: &dream::compiler::Block) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    // If block has a trailing expression, use that
    if let Some(ref expr) = block.expr {
        return build_expr(env, expr);
    }

    // Otherwise, try to get value from last statement
    if let Some(last_stmt) = block.stmts.last() {
        match last_stmt {
            dream::compiler::Stmt::Expr { expr, .. } => {
                return build_expr(env, expr);
            }
            _ => {}
        }
    }

    // Empty block returns :ok
    let atom = Atom::from_str(env, "ok")?;
    Ok((atoms::c_literal(), empty_list, atom).encode(env))
}

fn build_expr<'a>(env: Env<'a>, expr: &dream::compiler::Expr) -> NifResult<Term<'a>> {
    use dream::compiler::Expr;

    let empty_list: Vec<Term<'a>> = vec![];

    match expr {
        Expr::Atom(name) => {
            // {c_literal, [], atom}
            let atom = Atom::from_str(env, name)?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
        Expr::Int(n) => {
            // {c_literal, [], integer}
            Ok((atoms::c_literal(), empty_list, *n).encode(env))
        }
        Expr::String(s) => {
            // {c_literal, [], binary}
            // Strings in Core Erlang are binaries
            Ok((atoms::c_literal(), empty_list, s.as_bytes()).encode(env))
        }
        Expr::Ident(name) => {
            // {c_var, [], Name}
            let name_atom = Atom::from_str(env, name)?;
            Ok((atoms::c_var(), empty_list, name_atom).encode(env))
        }
        Expr::Tuple(elements) => {
            // {c_tuple, [], [Elements]}
            let elems: Vec<Term<'a>> = elements.iter()
                .map(|e| build_expr(env, e))
                .collect::<Result<Vec<_>, _>>()?;
            Ok((atoms::c_tuple(), empty_list, elems).encode(env))
        }
        Expr::List(elements) => {
            // Build as nested c_cons ending in c_nil
            let nil = (atoms::c_nil(), empty_list.clone()).encode(env);
            elements.iter().rev().try_fold(nil, |acc, elem| {
                let e = build_expr(env, elem)?;
                Ok((atoms::c_cons(), empty_list.clone(), e, acc).encode(env))
            })
        }
        Expr::Block(block) => {
            build_block(env, block)
        }
        // TODO: Handle more expression types
        _ => {
            // Placeholder for unimplemented expressions
            let atom = Atom::from_str(env, "not_implemented")?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
    }
}

rustler::init!("dream_nif", load = on_load);

fn on_load(_env: Env, _info: Term) -> bool {
    // Nothing to initialize, just return success
    true
}
