//! Core Erlang AST generation from Surreal modules.

mod context;
mod expressions;
mod patterns;
mod primitives;

pub use context::BuildContext;
pub use expressions::build_block;
pub use primitives::{build_c_literal, build_c_var, build_c_var_simple};

use rustler::{Encoder, Env, NifResult, Term};
use surreal::compiler::{Function, Item, Module, Pattern};
use crate::atoms;

/// Build the complete Core Erlang AST for a module.
pub fn build_module_ast<'a>(env: Env<'a>, module: &Module) -> NifResult<Term<'a>> {
    let ctx = BuildContext::new(module);

    // Build module name as c_literal
    let module_name = build_c_literal(env, &module.name)?;

    // Build exports list
    let exports: Vec<Term<'a>> = module.items.iter().filter_map(|item| {
        if let Item::Function(f) = item {
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
        if let Item::Function(f) = item {
            match build_fundef(env, &ctx, f) {
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

/// Build a function definition.
fn build_fundef<'a>(env: Env<'a>, ctx: &BuildContext, func: &Function) -> NifResult<Term<'a>> {
    let var = build_c_var(env, &func.name, func.params.len())?;
    let fun = build_c_fun(env, ctx, func)?;
    Ok((var, fun).encode(env))
}

/// Build a c_fun node.
fn build_c_fun<'a>(env: Env<'a>, ctx: &BuildContext, func: &Function) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    // Build params as c_var terms
    let params: Vec<Term<'a>> = func.params.iter().filter_map(|p| {
        if let Pattern::Ident(name) = &p.pattern {
            Some(build_c_var_simple(env, name))
        } else {
            Some(build_c_var_simple(env, "_"))
        }
    }).collect::<Result<Vec<_>, _>>()?;

    let body = build_block(env, ctx, &func.body)?;

    Ok((atoms::c_fun(), empty_list, params, body).encode(env))
}
