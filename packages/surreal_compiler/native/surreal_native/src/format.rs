//! Format Erlang terms as Surreal syntax.
//!
//! Converts Erlang runtime values back to Surreal-style representation for REPL display.

use rustler::{Atom, Env, Term, types::{Binary, ListIterator, MapIterator, tuple::get_tuple}};

/// Format an Erlang term as a Surreal string.
pub fn format_term<'a>(env: Env<'a>, term: Term<'a>) -> String {
    format_value(env, term, 0)
}

fn format_value<'a>(env: Env<'a>, term: Term<'a>, depth: usize) -> String {
    // Prevent infinite recursion
    if depth > 100 {
        return "...".to_string();
    }

    // Check for special tuple forms first (Result, Option)
    if let Ok(tuple) = get_tuple(term) {
        return format_tuple_value(env, &tuple, depth);
    }

    // Atom
    if let Ok(atom) = term.decode::<Atom>() {
        return format_atom(env, atom);
    }

    // Integer
    if let Ok(n) = term.decode::<i64>() {
        return n.to_string();
    }

    // Float
    if let Ok(f) = term.decode::<f64>() {
        return format!("{}", f);
    }

    // Binary (Surreal string)
    if let Ok(binary) = term.decode::<Binary>() {
        return format_binary(&binary);
    }

    // List (could be charlist or regular list)
    if let Ok(iter) = term.decode::<ListIterator>() {
        return format_list(env, iter, depth);
    }

    // Map
    if let Ok(iter) = term.decode::<MapIterator>() {
        return format_map(env, iter, depth);
    }

    // Pid - display as #Pid (details not easily accessible from NIF)
    if term.is_pid() {
        return "#Pid".to_string();
    }

    // Reference
    if term.is_ref() {
        return "#Ref".to_string();
    }

    // Port
    if term.is_port() {
        return "#Port".to_string();
    }

    // Function
    if term.is_fun() {
        return "#Function".to_string();
    }

    // Fallback
    term_to_string(env, term)
}

fn format_atom(env: Env, atom: Atom) -> String {
    let name = atom.to_term(env).atom_to_string()
        .unwrap_or_else(|_| "unknown".to_string());

    match name.as_str() {
        "true" => "true".to_string(),
        "false" => "false".to_string(),
        "nil" => "nil".to_string(),
        "ok" => ":ok".to_string(),
        "error" => ":error".to_string(),
        "none" => "None".to_string(),
        _ => {
            // Check if atom needs quoting (contains special chars or spaces)
            if needs_quoting(&name) {
                format!(":'{}'", name)
            } else {
                format!(":{}", name)
            }
        }
    }
}

fn needs_quoting(s: &str) -> bool {
    if s.is_empty() {
        return true;
    }
    let first = s.chars().next().unwrap();
    if !first.is_ascii_lowercase() && first != '_' {
        return true;
    }
    !s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn format_binary(binary: &Binary) -> String {
    // Try to interpret as UTF-8 string
    match std::str::from_utf8(binary.as_slice()) {
        Ok(s) => {
            // Escape special characters
            let escaped: String = s.chars().map(|c| match c {
                '"' => "\\\"".to_string(),
                '\\' => "\\\\".to_string(),
                '\n' => "\\n".to_string(),
                '\r' => "\\r".to_string(),
                '\t' => "\\t".to_string(),
                c if c.is_control() => format!("\\x{:02x}", c as u32),
                c => c.to_string(),
            }).collect();
            format!("\"{}\"", escaped)
        }
        Err(_) => {
            // Not valid UTF-8, format as byte list
            let bytes: Vec<String> = binary.as_slice().iter()
                .map(|b| b.to_string())
                .collect();
            format!("<<{}>>", bytes.join(", "))
        }
    }
}

fn format_list<'a>(env: Env<'a>, iter: ListIterator<'a>, depth: usize) -> String {
    let items: Vec<Term> = iter.collect();

    // Check if it's a charlist (all small integers that could be chars)
    if is_charlist(&items) {
        let chars: String = items.iter()
            .filter_map(|t| t.decode::<i64>().ok())
            .filter_map(|n| char::from_u32(n as u32))
            .collect();
        return format!("'{}'", chars.replace('\'', "\\'"));
    }

    // Regular list
    let formatted: Vec<String> = items.iter()
        .map(|item| format_value(env, *item, depth + 1))
        .collect();
    format!("[{}]", formatted.join(", "))
}

fn is_charlist(items: &[Term]) -> bool {
    if items.is_empty() {
        return false;
    }
    items.iter().all(|t| {
        if let Ok(n) = t.decode::<i64>() {
            // Valid character range (printable ASCII + common unicode)
            (32..=126).contains(&n) || (160..=255).contains(&n) || n == 9 || n == 10 || n == 13
        } else {
            false
        }
    })
}

fn format_map<'a>(env: Env<'a>, iter: MapIterator<'a>, depth: usize) -> String {
    let pairs: Vec<(Term, Term)> = iter.collect();

    let formatted: Vec<String> = pairs.iter()
        .map(|(k, v)| {
            let key = format_map_key(env, *k, depth + 1);
            let val = format_value(env, *v, depth + 1);
            format!("{}: {}", key, val)
        })
        .collect();

    format!("{{{}}}", formatted.join(", "))
}

fn format_map_key<'a>(env: Env<'a>, term: Term<'a>, depth: usize) -> String {
    // For atom keys, format without the colon prefix
    if let Ok(atom) = term.decode::<Atom>() {
        let name = atom.to_term(env).atom_to_string()
            .unwrap_or_else(|_| "unknown".to_string());
        if needs_quoting(&name) {
            format!("'{}'", name)
        } else {
            name
        }
    } else {
        // Non-atom keys get formatted normally
        format_value(env, term, depth)
    }
}

fn format_tuple_value<'a>(env: Env<'a>, tuple: &[Term<'a>], depth: usize) -> String {
    if tuple.is_empty() {
        return "()".to_string();
    }

    // Check for Result/Option patterns
    if tuple.len() == 2 {
        if let Ok(atom) = tuple[0].decode::<Atom>() {
            let name = atom.to_term(env).atom_to_string()
                .unwrap_or_else(|_| String::new());

            match name.as_str() {
                "ok" => {
                    let inner = format_value(env, tuple[1], depth + 1);
                    return format!("Ok({})", inner);
                }
                "error" => {
                    let inner = format_value(env, tuple[1], depth + 1);
                    return format!("Err({})", inner);
                }
                "some" => {
                    let inner = format_value(env, tuple[1], depth + 1);
                    return format!("Some({})", inner);
                }
                _ => {}
            }
        }
    }

    // Regular tuple
    let formatted: Vec<String> = tuple.iter()
        .map(|item| format_value(env, *item, depth + 1))
        .collect();
    format!("({})", formatted.join(", "))
}

fn term_to_string<'a>(_env: Env<'a>, term: Term<'a>) -> String {
    // Use Erlang's term_to_binary and then format
    // This is a fallback for types we can't handle directly
    format!("{:?}", term.get_type())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_needs_quoting() {
        assert!(!needs_quoting("hello"));
        assert!(!needs_quoting("hello_world"));
        assert!(needs_quoting("Hello"));
        assert!(needs_quoting("hello world"));
        assert!(needs_quoting("hello-world"));
        assert!(needs_quoting(""));
    }
}
