//! Bytecode instructions for the VM.

use crate::Pid;

/// Bytecode instructions
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Process is done
    End,

    /// Do some work (costs `amount` reductions)
    Work { amount: u32 },

    /// Spawn a new process running `code`, store child PID in register
    Spawn { code: Vec<Instruction>, dest: Register },

    /// Spawn a new process and atomically link to it
    SpawnLink { code: Vec<Instruction>, dest: Register },

    /// Send a message to a process
    Send { to: Source, msg: String },

    /// Receive a message matching a pattern, block if none available
    /// For now, just receives any user message into a register
    Receive { dest: Register },

    /// Receive with timeout (in reductions). If no message arrives
    /// before timeout expires, receives "TIMEOUT" instead.
    ReceiveTimeout { dest: Register, timeout: u32 },

    /// Link to another process (bidirectional crash notification)
    Link { target: Source },

    /// Monitor another process (one-way crash notification)
    Monitor { target: Source },

    /// Register current process with a name
    Register { name: String },

    /// Unregister a name
    Unregister { name: String },

    /// Look up a registered name, store PID (or None) in register
    WhereIs { name: String, dest: Register },

    /// Print a value (for debugging)
    Print { source: Source },

    /// Crash the process (for testing links)
    Crash,

    // ========== Arithmetic & Logic ==========
    /// Load an immediate integer into a register
    LoadInt { value: i64, dest: Register },

    /// Move (copy) any value from source register to dest register
    Move { source: Register, dest: Register },

    /// Add two operands, store result in dest
    Add { a: Operand, b: Operand, dest: Register },

    /// Subtract b from a, store result in dest
    Sub { a: Operand, b: Operand, dest: Register },

    /// Multiply two operands, store result in dest
    Mul { a: Operand, b: Operand, dest: Register },

    /// Divide a by b, store result in dest (integer division)
    Div { a: Operand, b: Operand, dest: Register },

    /// Modulo a by b, store result in dest
    Mod { a: Operand, b: Operand, dest: Register },

    // ========== Comparisons ==========
    /// Compare equal: dest = (a == b) ? 1 : 0
    Eq { a: Operand, b: Operand, dest: Register },

    /// Compare not equal: dest = (a != b) ? 1 : 0
    Ne { a: Operand, b: Operand, dest: Register },

    /// Compare less than: dest = (a < b) ? 1 : 0
    Lt { a: Operand, b: Operand, dest: Register },

    /// Compare less than or equal: dest = (a <= b) ? 1 : 0
    Lte { a: Operand, b: Operand, dest: Register },

    /// Compare greater than: dest = (a > b) ? 1 : 0
    Gt { a: Operand, b: Operand, dest: Register },

    /// Compare greater than or equal: dest = (a >= b) ? 1 : 0
    Gte { a: Operand, b: Operand, dest: Register },

    // ========== Control Flow ==========
    /// Unconditional jump to instruction index
    Jump { target: usize },

    /// Jump to target if condition is truthy (non-zero integer)
    JumpIf { cond: Operand, target: usize },

    /// Jump to target if condition is falsy (zero or non-integer)
    JumpUnless { cond: Operand, target: usize },

    /// Call a function: push return address onto call stack, jump to target
    Call { target: usize },

    /// Return from function: pop return address from call stack, jump to it
    /// If call stack is empty, ends the process
    Return,

    // ========== Module Function Calls ==========
    /// Call a function by MFA (Module:Function/Arity)
    /// Arguments must be in R0..R(arity-1) before call
    /// Return value will be in R0
    CallMFA {
        module: String,
        function: String,
        arity: u8,
    },

    /// Call a local function in the current module
    /// More efficient than CallMFA when calling within same module
    CallLocal { function: String, arity: u8 },

    /// Tail call MFA - call without pushing return frame
    TailCallMFA {
        module: String,
        function: String,
        arity: u8,
    },

    /// Tail call local - call without pushing return frame
    TailCallLocal { function: String, arity: u8 },

    /// Create a function reference and store in register
    MakeFun {
        module: String,
        function: String,
        arity: u8,
        dest: Register,
    },

    /// Apply a function reference (from register) to arguments
    /// Fun must be a Value::Fun or Value::Closure in the specified register
    /// Arguments in R0..R(arity-1)
    Apply { fun: Register, arity: u8 },

    /// Create a closure capturing values from registers
    /// The closure references module:function but captures current register values
    /// When applied, captured values are placed after explicit arguments
    MakeClosure {
        module: String,
        function: String,
        arity: u8,
        captures: Vec<Register>,
        dest: Register,
    },

    /// Spawn a process running module:function/arity
    /// Arguments for the function must be in R0..R(arity-1)
    SpawnMFA {
        module: String,
        function: String,
        arity: u8,
        dest: Register,
    },

    /// Spawn and link, running module:function/arity
    SpawnLinkMFA {
        module: String,
        function: String,
        arity: u8,
        dest: Register,
    },

    // ========== Stack Operations ==========
    /// Push a value onto the data stack
    Push { source: Operand },

    /// Pop a value from the data stack into a register
    /// Crashes if stack is empty
    Pop { dest: Register },

    // ========== Atoms & Tuples ==========
    /// Load an atom into a register
    LoadAtom { name: String, dest: Register },

    /// Create a tuple from the top `arity` stack elements
    /// Elements are popped in reverse order (first pushed = first element)
    MakeTuple { arity: u8, dest: Register },

    /// Get an element from a tuple by index (0-based)
    /// Crashes if not a tuple or index out of bounds
    TupleElement { tuple: Register, index: u8, dest: Register },

    /// Get the arity (size) of a tuple
    /// Crashes if not a tuple
    TupleArity { tuple: Register, dest: Register },

    // ========== Pattern Matching ==========
    /// Match a value against a pattern
    /// On success: binds variables and continues to next instruction
    /// On failure: jumps to fail_target
    Match {
        source: Register,
        pattern: Pattern,
        fail_target: usize,
    },

    /// Receive with pattern matching
    /// Scans mailbox for first message matching any clause pattern.
    /// On match: removes message, binds variables, jumps to clause target.
    /// No match: blocks until a message arrives.
    /// Timeout: if set and expires, jumps to timeout_target.
    ReceiveMatch {
        /// List of (pattern, jump_target) clauses
        clauses: Vec<(Pattern, usize)>,
        /// Optional timeout in reductions
        timeout: Option<u32>,
        /// Where to jump on timeout
        timeout_target: usize,
    },

    // ========== Lists ==========
    /// Create a list from the top `length` stack elements
    /// Elements are popped in reverse order (first pushed = first element)
    MakeList { length: u8, dest: Register },

    /// Cons: prepend an element to a list [elem | list]
    /// Crashes if tail is not a list
    Cons { head: Register, tail: Register, dest: Register },

    /// Get the head (first element) of a list
    /// Crashes if not a list or empty
    ListHead { list: Register, dest: Register },

    /// Get the tail (rest) of a list
    /// Crashes if not a list or empty
    ListTail { list: Register, dest: Register },

    /// Check if a list is empty, store 1 (true) or 0 (false)
    ListIsEmpty { list: Register, dest: Register },

    /// Get the length of a list (O(n))
    ListLength { list: Register, dest: Register },

    /// Append two lists (a ++ b)
    /// Crashes if either is not a list
    ListAppend { a: Register, b: Register, dest: Register },

    /// Reverse a list
    /// Crashes if not a list
    ListReverse { list: Register, dest: Register },

    /// Get the nth element of a list (0-based)
    /// Crashes if not a list or index out of bounds
    ListNth { list: Register, n: Register, dest: Register },

    /// Check if an element is a member of a list
    /// Stores 1 (true) or 0 (false)
    ListMember { elem: Register, list: Register, dest: Register },

    // ========== Type Checking ==========
    /// Check if value is an integer
    /// Stores 1 (true) or 0 (false)
    IsInteger { source: Register, dest: Register },

    /// Check if value is an atom
    /// Stores 1 (true) or 0 (false)
    IsAtom { source: Register, dest: Register },

    /// Check if value is a tuple
    /// Stores 1 (true) or 0 (false)
    IsTuple { source: Register, dest: Register },

    /// Check if value is a list (including empty list)
    /// Stores 1 (true) or 0 (false)
    IsList { source: Register, dest: Register },

    /// Check if value is a PID
    /// Stores 1 (true) or 0 (false)
    IsPid { source: Register, dest: Register },

    /// Check if value is a function (Fun or Closure)
    /// Stores 1 (true) or 0 (false)
    IsFunction { source: Register, dest: Register },

    /// Check if value is a string/binary
    /// Stores 1 (true) or 0 (false)
    IsString { source: Register, dest: Register },
}

/// An operand for arithmetic/comparison operations
#[derive(Debug, Clone)]
pub enum Operand {
    /// Read from a register
    Reg(Register),
    /// Immediate integer value
    Int(i64),
}

/// Where to read a value from
#[derive(Debug, Clone)]
pub enum Source {
    /// A register
    Reg(Register),
    /// The process's own PID
    Self_,
    /// The parent process's PID (if any)
    Parent,
    /// A literal PID
    Pid(Pid),
    /// A named process from the registry
    Named(String),
}

/// Register index (processes have a small set of registers)
#[derive(Debug, Clone, Copy)]
pub struct Register(pub u8);

/// A pattern for matching values
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Match any value, ignore it
    Wildcard,

    /// Match any value, bind it to a register
    Variable(Register),

    /// Match a specific integer
    Int(i64),

    /// Match a specific atom
    Atom(String),

    /// Match a specific string
    String(String),

    /// Match a tuple with specific arity and element patterns
    Tuple(Vec<Pattern>),

    /// Match an empty list []
    ListEmpty,

    /// Match a non-empty list [head | tail]
    ListCons {
        head: Box<Pattern>,
        tail: Box<Pattern>,
    },
}
