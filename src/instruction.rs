//! Bytecode instructions for the VM.

use crate::Pid;

// ========== Bit Syntax Types ==========

/// Type specifier for a binary segment
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitType {
    /// Integer (default)
    Integer,
    /// IEEE 754 float (32 or 64 bits)
    Float,
    /// Raw binary/bytes
    Binary,
    /// UTF-8 encoded codepoint
    Utf8,
}

/// Endianness for multi-byte values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endianness {
    /// Big endian (network byte order, default)
    Big,
    /// Little endian
    Little,
}

/// Signedness for integer values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Signedness {
    /// Unsigned (default)
    Unsigned,
    /// Signed (two's complement)
    Signed,
}

/// A segment specification for binary construction or matching
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitSegment {
    /// Type of the segment
    pub bit_type: BitType,
    /// Size in bits (None means use default for type or "rest" for binary)
    pub size: Option<u32>,
    /// Endianness for multi-byte values
    pub endianness: Endianness,
    /// Signedness for integers
    pub signedness: Signedness,
}

impl Default for BitSegment {
    fn default() -> Self {
        BitSegment {
            bit_type: BitType::Integer,
            size: Some(8), // default is 8 bits for integer
            endianness: Endianness::Big,
            signedness: Signedness::Unsigned,
        }
    }
}

impl BitSegment {
    /// Create a default integer segment with specified size in bits
    pub fn integer(bits: u32) -> Self {
        BitSegment {
            bit_type: BitType::Integer,
            size: Some(bits),
            endianness: Endianness::Big,
            signedness: Signedness::Unsigned,
        }
    }

    /// Create a binary segment that matches the rest
    pub fn binary_rest() -> Self {
        BitSegment {
            bit_type: BitType::Binary,
            size: None, // rest of binary
            endianness: Endianness::Big,
            signedness: Signedness::Unsigned,
        }
    }

    /// Create a binary segment with fixed byte size
    pub fn binary(bytes: u32) -> Self {
        BitSegment {
            bit_type: BitType::Binary,
            size: Some(bytes * 8),
            endianness: Endianness::Big,
            signedness: Signedness::Unsigned,
        }
    }

    /// Create a 32-bit float segment
    pub fn float32() -> Self {
        BitSegment {
            bit_type: BitType::Float,
            size: Some(32),
            endianness: Endianness::Big,
            signedness: Signedness::Unsigned,
        }
    }

    /// Create a 64-bit float segment
    pub fn float64() -> Self {
        BitSegment {
            bit_type: BitType::Float,
            size: Some(64),
            endianness: Endianness::Big,
            signedness: Signedness::Unsigned,
        }
    }

    /// Set endianness to little
    pub fn little(mut self) -> Self {
        self.endianness = Endianness::Little;
        self
    }

    /// Set signedness to signed
    pub fn signed(mut self) -> Self {
        self.signedness = Signedness::Signed;
        self
    }
}

/// Source of a segment value for binary construction
#[derive(Debug, Clone)]
pub enum SegmentSource {
    /// Immediate integer value
    Int(i64),
    /// Value from register
    Reg(Register),
}

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

    /// Remove a bidirectional link to another process
    Unlink { target: Source },

    /// Monitor another process (one-way crash notification)
    /// Returns a monitor reference in dest for later demonitoring
    Monitor { target: Source, dest: Register },

    /// Cancel a monitor by its reference
    Demonitor { monitor_ref: Register },

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

    /// Exit the process with a reason (from register)
    /// If reason is :normal, linked processes are not signaled (unless they trap_exit)
    /// If reason is abnormal, linked processes receive exit signal
    Exit { reason: Register },

    /// Set the trap_exit flag for this process
    /// When true, exit signals from linked processes become {:EXIT, Pid, Reason} messages
    /// When false (default), abnormal exit signals kill this process
    TrapExit { enable: bool },

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

    /// Check if value is a map
    /// Stores 1 (true) or 0 (false)
    IsMap { source: Register, dest: Register },

    // ========== Process Dictionary ==========
    /// Store value in process dictionary, returns old value (or None) in dest
    PutDict { key: Register, value: Register, dest: Register },

    /// Get value from process dictionary, stores None if key not found
    GetDict { key: Register, dest: Register },

    /// Remove key from process dictionary, returns old value (or None) in dest
    EraseDict { key: Register, dest: Register },

    /// Get all keys from process dictionary as a list
    GetDictKeys { dest: Register },

    // ========== Maps ==========
    /// Create a map from key-value pairs on the stack
    /// Pops `count` pairs (2*count values: k1, v1, k2, v2, ...) from stack
    MakeMap { count: u8, dest: Register },

    /// Get value from map by key, crashes if key not found
    MapGet { map: Register, key: Register, dest: Register },

    /// Get value from map by key, returns default if not found
    MapGetDefault {
        map: Register,
        key: Register,
        default: Register,
        dest: Register,
    },

    /// Insert/update key-value in map, returns new map (maps are immutable)
    MapPut {
        map: Register,
        key: Register,
        value: Register,
        dest: Register,
    },

    /// Remove key from map, returns new map (maps are immutable)
    MapRemove { map: Register, key: Register, dest: Register },

    /// Check if key exists in map, stores 1 (true) or 0 (false)
    MapHas { map: Register, key: Register, dest: Register },

    /// Get number of entries in map
    MapSize { map: Register, dest: Register },

    /// Get all keys from map as a list
    MapKeys { map: Register, dest: Register },

    /// Get all values from map as a list
    MapValues { map: Register, dest: Register },

    // ========== Binaries ==========
    /// Create a binary from literal bytes
    MakeBinary { bytes: Vec<u8>, dest: Register },

    /// Get the size (byte length) of a binary
    BinarySize { bin: Register, dest: Register },

    /// Get a byte at index (0-based)
    /// Crashes if not a binary or index out of bounds
    BinaryAt { bin: Register, index: Register, dest: Register },

    /// Extract a slice from a binary
    /// Crashes if not a binary or range out of bounds
    BinarySlice {
        bin: Register,
        start: Register,
        len: Register,
        dest: Register,
    },

    /// Concatenate two binaries
    /// Crashes if either is not a binary
    BinaryConcat { a: Register, b: Register, dest: Register },

    /// Check if value is a binary
    /// Stores 1 (true) or 0 (false)
    IsBinary { source: Register, dest: Register },

    /// Convert a string to a binary (UTF-8 encoded)
    StringToBinary { source: Register, dest: Register },

    /// Convert a binary to a string (assumes UTF-8)
    /// Crashes if binary is not valid UTF-8
    BinaryToString { source: Register, dest: Register },

    // ========== Bit Syntax ==========
    /// Construct a binary from segments
    /// Each segment specifies a value, type, size, endianness, and signedness
    /// Usage: push segment values to stack, then call with segment specs
    BinaryConstructSegments {
        /// Segment specifications (in order)
        segments: Vec<(SegmentSource, BitSegment)>,
        /// Destination register for the constructed binary
        dest: Register,
    },

    /// Start matching a binary - prepares for segment extraction
    /// Stores match state (current position) internally
    BinaryMatchStart {
        /// Source binary to match
        source: Register,
    },

    /// Match a segment from the binary at current position
    /// Advances the match position by the segment size
    /// Jumps to fail_target if match fails (not enough bytes, etc.)
    BinaryMatchSegment {
        /// Segment specification
        segment: BitSegment,
        /// Destination register for extracted value
        dest: Register,
        /// Jump target if match fails
        fail_target: usize,
    },

    /// Get the remaining bytes after matching
    /// Returns the rest of the binary as a new binary value
    BinaryMatchRest {
        /// Destination register for remaining bytes
        dest: Register,
    },

    /// Get an integer from a binary at bit offset with specified segment
    /// For random access bit extraction
    BinaryGetInteger {
        /// Source binary
        bin: Register,
        /// Bit offset (from start)
        bit_offset: Register,
        /// Segment specification
        segment: BitSegment,
        /// Destination register
        dest: Register,
    },

    /// Put an integer into a binary at bit offset with specified segment
    /// Creates a new binary with the value inserted
    BinaryPutInteger {
        /// Source binary
        bin: Register,
        /// Bit offset (from start)
        bit_offset: Register,
        /// Value to insert
        value: Register,
        /// Segment specification
        segment: BitSegment,
        /// Destination register
        dest: Register,
    },

    // ========== References ==========
    /// Create a new unique reference
    MakeRef { dest: Register },

    /// Check if value is a reference
    /// Stores 1 (true) or 0 (false)
    IsRef { source: Register, dest: Register },

    // ========== Floats ==========
    /// Load a floating-point immediate into a register
    LoadFloat { value: f64, dest: Register },

    /// Check if value is a float
    /// Stores 1 (true) or 0 (false)
    IsFloat { source: Register, dest: Register },

    /// Convert integer to float
    IntToFloat { source: Register, dest: Register },

    /// Convert float to integer (truncates toward zero)
    FloatToInt { source: Register, dest: Register },

    /// Floor: round down to nearest integer (as float)
    Floor { source: Register, dest: Register },

    /// Ceil: round up to nearest integer (as float)
    Ceil { source: Register, dest: Register },

    /// Round: round to nearest integer (as float)
    Round { source: Register, dest: Register },

    /// Trunc: truncate toward zero (as float)
    Trunc { source: Register, dest: Register },

    /// Square root
    Sqrt { source: Register, dest: Register },

    /// Absolute value (works for both int and float)
    Abs { source: Register, dest: Register },

    /// Power: base^exponent (both operands, result is float)
    Pow { base: Register, exp: Register, dest: Register },

    // ========== Timers ==========
    /// Send a message to a process after a delay (in reductions)
    /// Returns a timer reference in dest for cancellation
    SendAfter {
        delay: u32,
        to: Source,
        msg: Register,
        dest: Register,
    },

    /// Start a timer that sends {:timeout, ref, msg} to self after delay
    /// Returns the timer reference in dest
    StartTimer { delay: u32, msg: Register, dest: Register },

    /// Cancel a pending timer by its reference
    /// Returns the remaining time if timer was active, or :ok if already fired
    CancelTimer { timer_ref: Register, dest: Register },

    /// Read the remaining time on a timer (0 if already fired or cancelled)
    ReadTimer { timer_ref: Register, dest: Register },

    // ========== IO ==========
    /// Print a value to stdout with newline
    PrintLn { source: Register },

    /// Read a line from stdin into register as String
    /// Returns :eof atom if end of input
    ReadLine { dest: Register },

    /// Read entire file contents as binary
    /// Returns {:ok, binary} or {:error, reason}
    FileRead { path: Register, dest: Register },

    /// Write binary/string to file
    /// Returns :ok or {:error, reason}
    FileWrite { path: Register, content: Register, dest: Register },

    /// Check if file exists
    /// Returns 1 (true) or 0 (false)
    FileExists { path: Register, dest: Register },

    /// Delete a file
    /// Returns :ok or {:error, reason}
    FileDelete { path: Register, dest: Register },

    // ========== System Info ==========
    /// Get own PID into register
    SelfPid { dest: Register },

    /// Get list of all process PIDs
    ProcessList { dest: Register },

    /// Get count of live processes
    ProcessCount { dest: Register },

    /// Check if a process is alive
    /// Returns 1 (true) or 0 (false)
    IsAlive { pid: Register, dest: Register },

    /// Get process info as tuple
    /// Returns {status, mailbox_len, links_count, monitors_count, trap_exit}
    ProcessInfo { pid: Register, dest: Register },

    /// Get list of loaded module names
    ModuleList { dest: Register },

    /// Check if function is exported
    /// Returns 1 (true) or 0 (false)
    FunctionExported {
        module: Register,
        function: Register,
        arity: Register,
        dest: Register,
    },

    // ========== Exception Handling ==========
    /// Begin a try block. Pushes exception handler onto try stack.
    /// catch_target: where to jump on exception
    /// after_target: optional cleanup block (always runs)
    Try {
        catch_target: usize,
        after_target: Option<usize>,
    },

    /// End a try block successfully. Pops handler from try stack.
    /// If there's an after block, jumps to it.
    EndTry,

    /// Throw an exception. Unwinds to nearest catch handler.
    /// class: :error, :exit, or :throw
    /// reason: the exception reason (from register)
    Throw { class: Register, reason: Register },

    /// Get the current exception as {class, reason, stacktrace} tuple
    /// Used in catch blocks to access exception details
    GetException { dest: Register },

    /// Clear the current exception (after handling)
    ClearException,

    /// Re-raise the current exception (in catch block)
    Reraise,
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

    /// Match a specific binary
    Binary(Vec<u8>),

    /// Match a tuple with specific arity and element patterns
    Tuple(Vec<Pattern>),

    /// Match an empty list []
    ListEmpty,

    /// Match a non-empty list [head | tail]
    ListCons {
        head: Box<Pattern>,
        tail: Box<Pattern>,
    },

    /// Match a map containing specific key-value patterns
    /// The map may contain additional keys not in the pattern
    Map(Vec<(Pattern, Pattern)>),
}
