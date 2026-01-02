//! ToyBEAM - A toy BEAM virtual machine in Rust.
//!
//! This is a lightweight BEAM-inspired VM that can compile to WebAssembly.
//! It implements core BEAM primitives:
//! - Process spawning with parent tracking
//! - Message passing between processes
//! - Process links (bidirectional crash notification)
//! - Process monitors (one-way crash notification)
//! - Process registry for named processes
//! - Receive with timeout
//! - Cooperative scheduling with reduction budgets

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
pub use instruction::{Instruction, Operand, Pattern, Register, Source};
pub use message::{DownReason, Message, SystemMsg};
pub use module::{FunctionDef, Module};
pub use pid::Pid;
pub use process::{CallFrame, Process, ProcessStatus};
pub use scheduler::{Scheduler, StepResult};
pub use value::Value;
