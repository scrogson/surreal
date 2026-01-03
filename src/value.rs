//! Runtime values stored in registers.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::Pid;

/// Runtime value stored in registers
#[derive(Clone)]
pub enum Value {
    /// Integer
    Int(i64),
    /// Floating-point number
    Float(f64),
    /// Process identifier
    Pid(Pid),
    /// Unique reference (for request/response correlation)
    Ref(u64),
    /// String (text data)
    String(String),
    /// Binary (raw byte array)
    Binary(Vec<u8>),
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

    /// Try to extract a binary from this value
    pub fn as_binary(&self) -> Option<&[u8]> {
        match self {
            Value::Binary(bytes) => Some(bytes),
            _ => None,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(x) => write!(f, "{}", x),
            Value::Pid(p) => write!(f, "Pid({})", p.0),
            Value::Ref(r) => write!(f, "#Ref<{}>", r),
            Value::String(s) => write!(f, "{:?}", s),
            Value::Binary(bytes) => {
                write!(f, "<<")?;
                for (i, byte) in bytes.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", byte)?;
                }
                write!(f, ">>")
            }
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
            Value::Float(f) => f.to_bits().hash(state),
            Value::Pid(p) => p.0.hash(state),
            Value::Ref(r) => r.hash(state),
            Value::String(s) => s.hash(state),
            Value::Binary(bytes) => bytes.hash(state),
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

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
            (Value::Pid(a), Value::Pid(b)) => a == b,
            (Value::Ref(a), Value::Ref(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Binary(a), Value::Binary(b)) => a == b,
            (Value::Atom(a), Value::Atom(b)) => a == b,
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Fun { module: m1, function: f1, arity: a1 },
             Value::Fun { module: m2, function: f2, arity: a2 }) => {
                m1 == m2 && f1 == f2 && a1 == a2
            }
            (Value::Closure { module: m1, function: f1, arity: a1, captured: c1 },
             Value::Closure { module: m2, function: f2, arity: a2, captured: c2 }) => {
                m1 == m2 && f1 == f2 && a1 == a2 && c1 == c2
            }
            (Value::None, Value::None) => true,
            _ => false,
        }
    }
}

impl Eq for Value {}
