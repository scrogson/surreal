//! Atom definitions for the Surreal NIF.

rustler::atoms! {
    // Result atoms
    ok,
    error,
    some,
    none,

    // Boolean atoms
    r#true = "true",
    r#false = "false",

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
