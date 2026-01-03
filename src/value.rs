//! Runtime values stored in registers.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::Pid;

/// Runtime value stored in registers
#[derive(Clone, PartialEq, Eq)]
pub enum Value {
    /// Integer
    Int(i64),
    /// Process identifier
    Pid(Pid),
    /// String (binary in Erlang terms)
    String(String),
    /// Atom - an interned symbol
    Atom(String),
    /// Tuple - fixed-size container of values
    Tuple(Vec<Value>),
    /// List - variable-size linked list of values
    List(Vec<Value>),
    /// Map - key-value hash table (immutable, functional updates)
    Map(HashMap<Value, Value>),
    /// Function reference (module:function/arity)
    Fun {
        module: String,
        function: String,
        arity: u8,
    },
    /// Closure - function reference with captured environment
    Closure {
        module: String,
        function: String,
        arity: u8,
        captured: Vec<Value>,
    },
    /// No value / uninitialized
    None,
}

impl Value {
    /// Try to extract an integer from this value
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Pid(p) => write!(f, "Pid({})", p.0),
            Value::String(s) => write!(f, "{:?}", s),
            Value::Atom(a) => write!(f, ":{}", a),
            Value::Tuple(elements) => {
                write!(f, "{{")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", elem)?;
                }
                write!(f, "}}")
            }
            Value::List(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", elem)?;
                }
                write!(f, "]")
            }
            Value::Map(entries) => {
                write!(f, "%{{")?;
                for (i, (k, v)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?} => {:?}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Fun {
                module,
                function,
                arity,
            } => write!(f, "fun {}:{}/{}", module, function, arity),
            Value::Closure {
                module,
                function,
                arity,
                captured,
            } => write!(
                f,
                "closure {}:{}/{} {:?}",
                module, function, arity, captured
            ),
            Value::None => write!(f, "None"),
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Int(n) => n.hash(state),
            Value::Pid(p) => p.0.hash(state),
            Value::String(s) => s.hash(state),
            Value::Atom(a) => a.hash(state),
            Value::Tuple(elems) => {
                elems.len().hash(state);
                for elem in elems {
                    elem.hash(state);
                }
            }
            Value::List(elems) => {
                elems.len().hash(state);
                for elem in elems {
                    elem.hash(state);
                }
            }
            Value::Map(entries) => {
                // Hash map entries in a deterministic order
                // by collecting and sorting by key hash
                entries.len().hash(state);
                let mut pairs: Vec<_> = entries.iter().collect();
                pairs.sort_by(|a, b| {
                    let mut ha = std::collections::hash_map::DefaultHasher::new();
                    let mut hb = std::collections::hash_map::DefaultHasher::new();
                    a.0.hash(&mut ha);
                    b.0.hash(&mut hb);
                    ha.finish().cmp(&hb.finish())
                });
                for (k, v) in pairs {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Value::Fun {
                module,
                function,
                arity,
            } => {
                module.hash(state);
                function.hash(state);
                arity.hash(state);
            }
            Value::Closure {
                module,
                function,
                arity,
                captured,
            } => {
                module.hash(state);
                function.hash(state);
                arity.hash(state);
                captured.len().hash(state);
                for c in captured {
                    c.hash(state);
                }
            }
            Value::None => {}
        }
    }
}

impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::Int(n)
    }
}
