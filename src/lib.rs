//! Dream - A programming language with Rust-like syntax and Erlang-style concurrency.
//!
//! Dream compiles to Core Erlang for BEAM execution or to bytecode for the Dream VM.
//!
//! ## Features
//! - Process spawning with parent tracking
//! - Message passing between processes
//! - Process links (bidirectional crash notification)
//! - Process monitors (one-way crash notification)
//! - Process registry for named processes
//! - Receive with timeout
//! - Cooperative scheduling with reduction budgets

pub mod compiler;
pub mod config;
mod instruction;
mod message;
mod module;
mod pid;
mod process;
mod scheduler;
mod value;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

// Re-export public API
pub use instruction::{
    BitSegment, BitType, Endianness, Instruction, Operand, Pattern, Register, SegmentSource,
    Signedness, Source,
};
pub use message::{Message, SystemMsg};
pub use module::{FunctionDef, Module};
pub use pid::Pid;
pub use process::{CallFrame, Process, ProcessStatus, TryFrame};
pub use scheduler::{Scheduler, StepResult};
pub use value::Value;
