//! Primitive Core Erlang AST building blocks.

use rustler::{Atom, Encoder, Env, NifResult, Term};
use crate::atoms;

/// Build a c_literal node for an atom value.
pub fn build_c_literal<'a>(env: Env<'a>, value: &str) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];
    let atom = Atom::from_str(env, value)?;
    Ok((atoms::c_literal(), empty_list, atom).encode(env))
}

/// Build a c_var node with arity (for function references).
pub fn build_c_var<'a>(env: Env<'a>, name: &str, arity: usize) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];
    let name_atom = Atom::from_str(env, name)?;
    Ok((atoms::c_var(), empty_list, (name_atom, arity)).encode(env))
}

/// Build a simple c_var node (for local variables, no arity).
pub fn build_c_var_simple<'a>(env: Env<'a>, name: &str) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];
    let name_atom = Atom::from_str(env, name)?;
    Ok((atoms::c_var(), empty_list, name_atom).encode(env))
}
