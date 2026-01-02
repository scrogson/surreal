//! Runtime values stored in registers.

use crate::Pid;

/// Runtime value stored in registers
#[derive(Clone, PartialEq)]
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
    /// Function reference (module:function/arity)
    Fun {
        module: String,
        function: String,
        arity: u8,
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
            Value::Fun {
                module,
                function,
                arity,
            } => write!(f, "fun {}:{}/{}", module, function, arity),
            Value::None => write!(f, "None"),
        }
    }
}

impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::Int(n)
    }
}
