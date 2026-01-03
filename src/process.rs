//! Process state and status.

use std::collections::{HashMap, VecDeque};

use crate::{Instruction, Message, Pid, Value};

/// A frame on the call stack, enabling cross-module calls.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// Module to return to (None = inline code).
    pub module: Option<String>,
    /// Instruction to return to.
    pub return_pc: usize,
}

/// A frame on the exception handler stack for try/catch/after.
#[derive(Debug, Clone)]
pub struct TryFrame {
    /// Where to jump on exception (catch block)
    pub catch_target: usize,
    /// Where to jump for cleanup (after block, always runs)
    pub after_target: Option<usize>,
    /// Call stack depth when try was entered (for unwinding)
    pub call_stack_depth: usize,
    /// Data stack depth when try was entered (for unwinding)
    pub stack_depth: usize,
}

/// Process state
#[derive(Debug)]
pub struct Process {
    pub pid: Pid,
    pub parent: Option<Pid>,
    /// Current module being executed (None if running inline code).
    pub current_module: Option<String>,
    /// Program counter - index into current module's code or inline_code.
    pub pc: usize,
    /// Inline code for processes spawned with raw instructions (backwards compat).
    pub inline_code: Option<Vec<Instruction>>,
    pub registers: [Value; 8],
    pub mailbox: VecDeque<Message>,
    pub links: Vec<Pid>,
    /// Monitors this process has set up: (ref, target_pid)
    pub monitors: Vec<(u64, Pid)>,
    /// Processes that are monitoring this one: (ref, monitoring_pid)
    pub monitored_by: Vec<(u64, Pid)>,
    pub status: ProcessStatus,
    /// Exit reason when process terminates (default: :normal for Done, :crashed for Crashed)
    pub exit_reason: Value,
    /// When true, exit signals from linked processes become messages instead of killing this process
    pub trap_exit: bool,
    /// Remaining timeout (in reductions) when waiting for a message
    pub timeout: Option<u32>,
    /// Call stack for function calls (stores return context)
    pub call_stack: Vec<CallFrame>,
    /// Data stack for saving/restoring values (Push/Pop)
    pub stack: Vec<Value>,
    /// Process dictionary for per-process key-value storage
    pub dictionary: HashMap<Value, Value>,
    /// Exception handler stack for try/catch/after
    pub try_stack: Vec<TryFrame>,
    /// Current exception being handled (class, reason, stacktrace)
    pub current_exception: Option<(Value, Value, Vec<String>)>,
    /// Binary match state: (binary bytes, current bit position)
    pub binary_match_state: Option<(Vec<u8>, usize)>,
}

/// Process execution status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessStatus {
    /// Ready to run
    Ready,
    /// Waiting for a message
    Waiting,
    /// Finished normally
    Done,
    /// Crashed
    Crashed,
}

impl Process {
    /// Create a new process with inline code (backwards compatible).
    pub fn new(pid: Pid, parent: Option<Pid>, code: Vec<Instruction>) -> Self {
        Self {
            pid,
            parent,
            current_module: None,
            pc: 0,
            inline_code: Some(code),
            registers: std::array::from_fn(|_| Value::None),
            mailbox: VecDeque::new(),
            links: Vec::new(),
            monitors: Vec::new(),
            monitored_by: Vec::new(),
            status: ProcessStatus::Ready,
            exit_reason: Value::Atom("normal".to_string()),
            trap_exit: false,
            timeout: None,
            call_stack: Vec::new(),
            stack: Vec::new(),
            dictionary: HashMap::new(),
            try_stack: Vec::new(),
            current_exception: None,
            binary_match_state: None,
        }
    }

    /// Create a new process that runs a module function.
    pub fn new_with_module(pid: Pid, parent: Option<Pid>, module: String, entry: usize) -> Self {
        Self {
            pid,
            parent,
            current_module: Some(module),
            pc: entry,
            inline_code: None,
            registers: std::array::from_fn(|_| Value::None),
            mailbox: VecDeque::new(),
            links: Vec::new(),
            monitors: Vec::new(),
            monitored_by: Vec::new(),
            status: ProcessStatus::Ready,
            exit_reason: Value::Atom("normal".to_string()),
            trap_exit: false,
            timeout: None,
            call_stack: Vec::new(),
            stack: Vec::new(),
            dictionary: HashMap::new(),
            try_stack: Vec::new(),
            current_exception: None,
            binary_match_state: None,
        }
    }
}
