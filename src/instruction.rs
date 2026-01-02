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
