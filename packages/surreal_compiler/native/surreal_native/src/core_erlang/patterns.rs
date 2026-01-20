//! Pattern building for Core Erlang AST.

use rustler::{Atom, Encoder, Env, NifResult, Term};
use surreal::compiler::Pattern;
use crate::atoms;

/// Build a pattern for matching.
pub fn build_pattern<'a>(env: Env<'a>, pattern: &Pattern) -> NifResult<Term<'a>> {
    let empty_list: Vec<Term<'a>> = vec![];

    match pattern {
        Pattern::Wildcard => {
            // Wildcard becomes a fresh variable named "_"
            let name_atom = Atom::from_str(env, "_")?;
            Ok((atoms::c_var(), empty_list, name_atom).encode(env))
        }
        Pattern::Ident(name) => {
            let name_atom = Atom::from_str(env, name)?;
            Ok((atoms::c_var(), empty_list, name_atom).encode(env))
        }
        Pattern::Int(n) => {
            Ok((atoms::c_literal(), empty_list, *n).encode(env))
        }
        Pattern::Atom(name) => {
            let atom = Atom::from_str(env, name)?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
        Pattern::String(s) => {
            Ok((atoms::c_literal(), empty_list, s.as_bytes()).encode(env))
        }
        Pattern::Bool(b) => {
            let atom = Atom::from_str(env, if *b { "true" } else { "false" })?;
            Ok((atoms::c_literal(), empty_list, atom).encode(env))
        }
        Pattern::Tuple(elements) => {
            let elems: Vec<Term<'a>> = elements.iter()
                .map(|p| build_pattern(env, p))
                .collect::<Result<Vec<_>, _>>()?;
            Ok((atoms::c_tuple(), empty_list, elems).encode(env))
        }
        Pattern::List(elements) => {
            let nil = (atoms::c_nil(), empty_list.clone()).encode(env);
            elements.iter().rev().try_fold(nil, |acc, elem| {
                let p = build_pattern(env, elem)?;
                Ok((atoms::c_cons(), empty_list.clone(), p, acc).encode(env))
            })
        }
        Pattern::ListCons { head, tail } => {
            let h = build_pattern(env, head)?;
            let t = build_pattern(env, tail)?;
            Ok((atoms::c_cons(), empty_list, h, t).encode(env))
        }
        // TODO: Handle more pattern types (struct, enum, etc.)
        _ => {
            // Fallback to wildcard for unimplemented patterns
            let name_atom = Atom::from_str(env, "_")?;
            Ok((atoms::c_var(), empty_list, name_atom).encode(env))
        }
    }
}
