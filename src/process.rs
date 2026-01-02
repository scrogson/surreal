//! Process state and status.

use std::collections::VecDeque;

use crate::{Instruction, Message, Pid, Value};

/// A frame on the call stack, enabling cross-module calls.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// Module to return to (None = inline code).
    pub module: Option<String>,
    /// Instruction to return to.
    pub return_pc: usize,
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
    /// Processes that are monitoring this one (one-way)
    pub monitored_by: Vec<Pid>,
    pub status: ProcessStatus,
    /// Remaining timeout (in reductions) when waiting for a message
    pub timeout: Option<u32>,
    /// Call stack for function calls (stores return context)
    pub call_stack: Vec<CallFrame>,
    /// Data stack for saving/restoring values (Push/Pop)
    pub stack: Vec<Value>,
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
            monitored_by: Vec::new(),
            status: ProcessStatus::Ready,
            timeout: None,
            call_stack: Vec::new(),
            stack: Vec::new(),
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
            monitored_by: Vec::new(),
            status: ProcessStatus::Ready,
            timeout: None,
            call_stack: Vec::new(),
            stack: Vec::new(),
        }
    }
}
