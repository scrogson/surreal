//! Surreal Compiler NIF
//!
//! Exposes the Surreal compiler to Erlang/Elixir via Rustler NIFs.
//! This enables in-memory compilation without disk I/O or erlc process spawning.

use rustler::{Encoder, Env, Term};

mod atoms;
mod core_erlang;
mod format;

/// Parse Surreal source code and return module information.
///
/// Returns: {:ok, [{name, [{func_name, arity}, ...]}]} | {:error, reason}
#[rustler::nif(schedule = "DirtyCpu")]
fn parse<'a>(env: Env<'a>, source: String) -> Term<'a> {
    use surreal::compiler::Parser;

    let mut parser = Parser::new(&source);

    match parser.parse_file_modules("input") {
        Ok(modules) => {
            let module_infos: Vec<_> = modules.iter().map(|m| {
                let name = m.name.as_str();
                let funcs: Vec<_> = m.items.iter().filter_map(|item| {
                    if let surreal::compiler::Item::Function(f) = item {
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
            (atoms::error(), error.message.clone()).encode(env)
        }
    }
}

/// Generate Core Erlang AST from Surreal source code.
///
/// Returns: {:ok, CoreErlangAST} | {:error, reason}
///
/// The CoreErlangAST is in the format expected by compile:forms/2 with [from_core, binary].
#[rustler::nif(schedule = "DirtyCpu")]
fn generate_core_ast<'a>(env: Env<'a>, source: String) -> Term<'a> {
    use surreal::compiler::{Parser, check_modules};

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

    // Type check
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

    // Generate Core Erlang AST
    match core_erlang::build_module_ast(env, &typed_module) {
        Ok(core_ast) => (atoms::ok(), core_ast).encode(env),
        Err(e) => (atoms::error(), format!("AST generation failed: {:?}", e)).encode(env),
    }
}

/// Format an Erlang term as Surreal syntax.
///
/// Converts Erlang runtime values to Surreal-style string representation.
/// Useful for REPL output formatting.
///
/// Examples:
///   ok -> ":ok"
///   {ok, 42} -> "Ok(42)"
///   {error, reason} -> "Err(:reason)"
///   [1, 2, 3] -> "[1, 2, 3]"
///   #{a => 1} -> "{a: 1}"
///   <<"hello">> -> "\"hello\""
///   "hello" -> "'hello'" (charlist)
#[rustler::nif]
fn format_result<'a>(env: Env<'a>, term: Term<'a>) -> String {
    format::format_term(env, term)
}

rustler::init!("surreal_native", load = on_load);

fn on_load(_env: Env, _info: Term) -> bool {
    true
}
