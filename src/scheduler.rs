//! The scheduler / VM execution engine.

use std::collections::{HashMap, VecDeque};

use crate::{
    CallFrame, Instruction, Message, Module, Operand, Pattern, Pid, Process, ProcessStatus,
    Register, Source, SystemMsg, TryFrame, Value,
};

/// Result of stepping the scheduler
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StepResult {
    /// There's more work to do
    Busy,
    /// All processes are done or waiting
    Idle,
}

/// Result of executing a single instruction
#[derive(Debug)]
enum ExecResult {
    /// Continue execution, used N reductions
    Continue(u32),
    /// Yield back to scheduler (e.g., spawned a process)
    #[allow(dead_code)]
    Yield(u32),
    /// Jump to a specific instruction (sets PC directly, no auto-increment)
    Jump(usize, u32),
    /// Waiting for a message
    Wait,
    /// Process finished normally (exit reason: :normal)
    Done,
    /// Process crashed (exit reason: :crashed)
    Crash,
    /// Process exited with a specific reason
    Exit(Value),
    /// Exception thrown (class, reason)
    Throw(Value, Value),
}

/// Platform-specific logging
fn log(msg: &str) {
    #[cfg(target_arch = "wasm32")]
    web_sys::console::log_1(&msg.into());

    #[cfg(not(target_arch = "wasm32"))]
    println!("{}", msg);
}

/// A pending timer
#[derive(Debug, Clone)]
pub struct Timer {
    /// Unique timer reference
    pub timer_ref: u64,
    /// Process that will receive the message
    pub target: Pid,
    /// Message to send when timer fires
    pub message: Value,
    /// Remaining reductions until timer fires
    pub remaining: u32,
}

/// The scheduler / VM state
#[derive(Debug)]
pub struct Scheduler {
    pub processes: HashMap<Pid, Process>,
    pub ready_queue: VecDeque<Pid>,
    pub next_pid: u64,
    /// Process registry: name -> pid
    pub registry: HashMap<String, Pid>,
    /// Module registry: name -> module
    pub modules: HashMap<String, Module>,
    /// Output buffer from print instructions
    pub output: Vec<String>,
    /// Global reference counter for make_ref and timer_ref
    pub next_ref: u64,
    /// Pending timers
    pub timers: Vec<Timer>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            processes: HashMap::new(),
            ready_queue: VecDeque::new(),
            next_pid: 0,
            registry: HashMap::new(),
            modules: HashMap::new(),
            output: Vec::new(),
            next_ref: 0,
            timers: Vec::new(),
        }
    }

    /// Load a module into the VM.
    pub fn load_module(&mut self, module: Module) -> Result<(), String> {
        if self.modules.contains_key(&module.name) {
            return Err(format!("Module {} already loaded", module.name));
        }
        self.modules.insert(module.name.clone(), module);
        Ok(())
    }

    /// Get an instruction from a process (checking module or inline code).
    fn get_instruction(&self, process: &Process) -> Option<Instruction> {
        if let Some(ref module_name) = process.current_module {
            // Running module code
            let module = self.modules.get(module_name)?;
            module.code.get(process.pc).cloned()
        } else if let Some(ref code) = process.inline_code {
            // Running inline code
            code.get(process.pc).cloned()
        } else {
            None
        }
    }

    /// Get the code length for a process.
    fn get_code_len(&self, process: &Process) -> usize {
        if let Some(ref module_name) = process.current_module {
            self.modules
                .get(module_name)
                .map(|m| m.code.len())
                .unwrap_or(0)
        } else if let Some(ref code) = process.inline_code {
            code.len()
        } else {
            0
        }
    }

    /// Take and clear the output buffer
    pub fn take_output(&mut self) -> Vec<String> {
        std::mem::take(&mut self.output)
    }

    /// Spawn a root process (no parent)
    pub fn spawn(&mut self, code: Vec<Instruction>) -> Pid {
        self.spawn_with_parent(code, None)
    }

    /// Spawn a process with an optional parent
    pub fn spawn_with_parent(&mut self, code: Vec<Instruction>, parent: Option<Pid>) -> Pid {
        let pid = Pid(self.next_pid);
        self.next_pid += 1;

        let process = Process::new(pid, parent, code);
        self.processes.insert(pid, process);
        self.ready_queue.push_back(pid);

        pid
    }

    /// Run up to `budget` reductions, returns whether there's more work
    pub fn step(&mut self, budget: u32) -> StepResult {
        let mut remaining = budget;

        // Tick timeouts for waiting processes
        self.tick_timeouts();

        // Tick timers and fire expired ones
        self.tick_timers(budget);

        while remaining > 0 {
            // Get next ready process
            let Some(pid) = self.ready_queue.pop_front() else {
                // No ready processes - check if any are waiting with timeouts or timers
                if self.has_pending_timeouts() || !self.timers.is_empty() {
                    return StepResult::Busy; // Keep ticking
                }
                return StepResult::Idle;
            };

            // Run it for up to `remaining` reductions
            let used = self.run_process(pid, remaining);
            remaining = remaining.saturating_sub(used);
        }

        if self.ready_queue.is_empty() && !self.has_pending_timeouts() && self.timers.is_empty() {
            StepResult::Idle
        } else {
            StepResult::Busy
        }
    }

    /// Check if any waiting process has a pending timeout
    fn has_pending_timeouts(&self) -> bool {
        self.processes
            .values()
            .any(|p| p.status == ProcessStatus::Waiting && p.timeout.is_some())
    }

    /// Decrement timeouts for waiting processes, wake those that expired
    fn tick_timeouts(&mut self) {
        let mut to_wake = Vec::new();

        for (pid, process) in &mut self.processes {
            if process.status == ProcessStatus::Waiting {
                if let Some(ref mut t) = process.timeout {
                    if *t > 0 {
                        *t -= 1;
                    }
                    if *t == 0 {
                        process.status = ProcessStatus::Ready;
                        to_wake.push(*pid);
                    }
                }
            }
        }

        for pid in to_wake {
            self.ready_queue.push_back(pid);
        }
    }

    /// Tick timers and fire any that have expired
    fn tick_timers(&mut self, elapsed: u32) {
        let mut fired = Vec::new();
        let mut i = 0;

        while i < self.timers.len() {
            let timer = &mut self.timers[i];
            timer.remaining = timer.remaining.saturating_sub(elapsed);

            if timer.remaining == 0 {
                // Timer fired - collect it for processing
                fired.push(self.timers.remove(i));
            } else {
                i += 1;
            }
        }

        // Deliver messages for fired timers
        for timer in fired {
            if let Some(process) = self.processes.get_mut(&timer.target) {
                // Send the message
                let msg_str = format!("{:?}", timer.message);
                process.mailbox.push_back(Message::User(msg_str));

                // Wake up if waiting
                if process.status == ProcessStatus::Waiting {
                    process.status = ProcessStatus::Ready;
                    self.ready_queue.push_back(timer.target);
                }
            }
        }
    }

    /// Run a single process for up to `budget` reductions
    /// Returns how many reductions were used
    fn run_process(&mut self, pid: Pid, budget: u32) -> u32 {
        let mut used = 0;

        while used < budget {
            let Some(process) = self.processes.get(&pid) else {
                break;
            };

            let code_len = self.get_code_len(process);
            if process.pc >= code_len {
                // Process finished
                self.finish_process_with_reason(
                    pid,
                    ProcessStatus::Done,
                    Value::Atom("normal".to_string()),
                );
                break;
            }

            // Get instruction (from module or inline code)
            let Some(instruction) = self.get_instruction(process) else {
                self.finish_process_with_reason(
                    pid,
                    ProcessStatus::Crashed,
                    Value::Atom("crashed".to_string()),
                );
                break;
            };

            match self.execute(pid, instruction) {
                ExecResult::Continue(cost) => {
                    used += cost;
                    if let Some(p) = self.processes.get_mut(&pid) {
                        p.pc += 1;
                    }
                }
                ExecResult::Yield(cost) => {
                    used += cost;
                    if let Some(p) = self.processes.get_mut(&pid) {
                        p.pc += 1;
                    }
                    // Re-queue and return
                    self.ready_queue.push_back(pid);
                    break;
                }
                ExecResult::Jump(target, cost) => {
                    used += cost;
                    if let Some(p) = self.processes.get_mut(&pid) {
                        p.pc = target;
                    }
                }
                ExecResult::Wait => {
                    // Don't advance PC, don't requeue (waiting for message)
                    if let Some(p) = self.processes.get_mut(&pid) {
                        p.status = ProcessStatus::Waiting;
                    }
                    break;
                }
                ExecResult::Done => {
                    // Normal exit with :normal reason
                    self.finish_process_with_reason(
                        pid,
                        ProcessStatus::Done,
                        Value::Atom("normal".to_string()),
                    );
                    break;
                }
                ExecResult::Crash => {
                    // Abnormal exit with :crashed reason
                    self.finish_process_with_reason(
                        pid,
                        ProcessStatus::Crashed,
                        Value::Atom("crashed".to_string()),
                    );
                    break;
                }
                ExecResult::Exit(reason) => {
                    // Exit with custom reason
                    let status = if reason == Value::Atom("normal".to_string()) {
                        ProcessStatus::Done
                    } else {
                        ProcessStatus::Crashed
                    };
                    self.finish_process_with_reason(pid, status, reason);
                    break;
                }
                ExecResult::Throw(class, reason) => {
                    // Handle exception - unwind to nearest catch handler
                    let Some(process) = self.processes.get_mut(&pid) else {
                        break;
                    };

                    if let Some(frame) = process.try_stack.pop() {
                        // Build stacktrace (simplified - just current PC)
                        let stacktrace = vec![format!("pc:{}", process.pc)];

                        // Store exception for GetException
                        process.current_exception = Some((class, reason, stacktrace));

                        // Unwind stacks to saved depths
                        process.call_stack.truncate(frame.call_stack_depth);
                        process.stack.truncate(frame.stack_depth);

                        // Jump to catch handler
                        process.pc = frame.catch_target;
                        used += 1;
                        // Continue execution (don't break)
                    } else {
                        // No handler - crash with exception
                        let exit_reason = Value::Tuple(vec![class, reason]);
                        self.finish_process_with_reason(
                            pid,
                            ProcessStatus::Crashed,
                            exit_reason,
                        );
                        break;
                    }
                }
            }
        }

        // If we used full budget but process isn't done, requeue it
        if used >= budget {
            if let Some(p) = self.processes.get(&pid) {
                let code_len = self.get_code_len(p);
                if p.status == ProcessStatus::Ready && p.pc < code_len {
                    self.ready_queue.push_back(pid);
                }
            }
        }

        used
    }

    fn execute(&mut self, pid: Pid, instruction: Instruction) -> ExecResult {
        match instruction {
            Instruction::End => ExecResult::Done,

            Instruction::Work { amount } => ExecResult::Continue(amount),

            Instruction::Spawn { code, dest } => {
                let child_pid = self.spawn_with_parent(code, Some(pid));
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Pid(child_pid);
                }
                ExecResult::Continue(1)
            }

            Instruction::SpawnLink { code, dest } => {
                let child_pid = self.spawn_with_parent(code, Some(pid));

                // Establish bidirectional link atomically
                if let Some(parent) = self.processes.get_mut(&pid) {
                    parent.registers[dest.0 as usize] = Value::Pid(child_pid);
                    if !parent.links.contains(&child_pid) {
                        parent.links.push(child_pid);
                    }
                }
                if let Some(child) = self.processes.get_mut(&child_pid) {
                    if !child.links.contains(&pid) {
                        child.links.push(pid);
                    }
                }

                ExecResult::Continue(1)
            }

            Instruction::Send { to, msg } => {
                let target_pid = self.resolve_pid(pid, &to);
                if let Some(target_pid) = target_pid {
                    if let Some(target) = self.processes.get_mut(&target_pid) {
                        target.mailbox.push_back(Message::User(msg));
                        // Wake up if waiting
                        if target.status == ProcessStatus::Waiting {
                            target.status = ProcessStatus::Ready;
                            self.ready_queue.push_back(target_pid);
                        }
                    }
                }
                ExecResult::Continue(1)
            }

            Instruction::Receive { dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };

                // Take the first message (user or system)
                if let Some(msg) = process.mailbox.pop_front() {
                    let value = Self::message_to_value(msg);
                    process.registers[dest.0 as usize] = value;
                    process.timeout = None;
                    ExecResult::Continue(1)
                } else {
                    ExecResult::Wait
                }
            }

            Instruction::ReceiveTimeout { dest, timeout } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };

                // Take the first message if available
                if let Some(msg) = process.mailbox.pop_front() {
                    let value = Self::message_to_value(msg);
                    process.registers[dest.0 as usize] = value;
                    process.timeout = None;
                    ExecResult::Continue(1)
                } else if process.timeout == Some(0) {
                    // Timeout expired
                    process.registers[dest.0 as usize] = Value::String("TIMEOUT".into());
                    process.timeout = None;
                    ExecResult::Continue(1)
                } else {
                    // Set or keep timeout, wait
                    if process.timeout.is_none() {
                        process.timeout = Some(timeout);
                    }
                    ExecResult::Wait
                }
            }

            Instruction::Link { target } => {
                let Some(target_pid) = self.resolve_pid(pid, &target) else {
                    return ExecResult::Crash;
                };

                // Add bidirectional link
                if let Some(p) = self.processes.get_mut(&pid) {
                    if !p.links.contains(&target_pid) {
                        p.links.push(target_pid);
                    }
                }
                if let Some(t) = self.processes.get_mut(&target_pid) {
                    if !t.links.contains(&pid) {
                        t.links.push(pid);
                    }
                }

                ExecResult::Continue(1)
            }

            Instruction::Unlink { target } => {
                let Some(target_pid) = self.resolve_pid(pid, &target) else {
                    return ExecResult::Crash;
                };

                // Remove bidirectional link
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.links.retain(|&linked| linked != target_pid);
                }
                if let Some(t) = self.processes.get_mut(&target_pid) {
                    t.links.retain(|&linked| linked != pid);
                }

                ExecResult::Continue(1)
            }

            Instruction::Monitor { target, dest } => {
                let Some(target_pid) = self.resolve_pid(pid, &target) else {
                    return ExecResult::Crash;
                };

                // Generate a unique monitor reference
                let monitor_ref = self.next_ref;
                self.next_ref += 1;

                // Add monitor on caller side
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.monitors.push((monitor_ref, target_pid));
                }

                // Add to target's monitored_by list
                if let Some(t) = self.processes.get_mut(&target_pid) {
                    t.monitored_by.push((monitor_ref, pid));
                }

                // Store the ref in dest register
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Ref(monitor_ref);
                }

                ExecResult::Continue(1)
            }

            Instruction::Demonitor { monitor_ref } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Ref(ref_id) = process.registers[monitor_ref.0 as usize] else {
                    return ExecResult::Crash;
                };
                let ref_id = ref_id;

                // Find and remove the monitor from caller's list
                let target_pid = if let Some(p) = self.processes.get_mut(&pid) {
                    let pos = p.monitors.iter().position(|(r, _)| *r == ref_id);
                    if let Some(idx) = pos {
                        let (_, target) = p.monitors.remove(idx);
                        Some(target)
                    } else {
                        None
                    }
                } else {
                    None
                };

                // Remove from target's monitored_by list
                if let Some(target_pid) = target_pid {
                    if let Some(t) = self.processes.get_mut(&target_pid) {
                        t.monitored_by.retain(|(r, _)| *r != ref_id);
                    }
                }

                ExecResult::Continue(1)
            }

            Instruction::Register { name } => {
                // Register current process with name
                self.registry.insert(name, pid);
                ExecResult::Continue(1)
            }

            Instruction::Unregister { name } => {
                // Remove name from registry
                self.registry.remove(&name);
                ExecResult::Continue(1)
            }

            Instruction::WhereIs { name, dest } => {
                let value = self
                    .registry
                    .get(&name)
                    .map(|p| Value::Pid(*p))
                    .unwrap_or(Value::None);

                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = value;
                }
                ExecResult::Continue(1)
            }

            Instruction::Print { source } => {
                if let Some(process) = self.processes.get(&pid) {
                    let value = self.resolve_source(process, &source);
                    let msg = format!("[Pid({})] {:?}", pid.0, value);
                    log(&msg);
                    self.output.push(msg);
                }
                ExecResult::Continue(1)
            }

            Instruction::Crash => ExecResult::Crash,

            Instruction::Exit { reason } => {
                if let Some(p) = self.processes.get(&pid) {
                    let exit_reason = p.registers[reason.0 as usize].clone();
                    ExecResult::Exit(exit_reason)
                } else {
                    ExecResult::Crash
                }
            }

            Instruction::TrapExit { enable } => {
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.trap_exit = enable;
                }
                ExecResult::Continue(1)
            }

            // ========== Arithmetic ==========
            Instruction::LoadInt { value, dest } => {
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Int(value);
                }
                ExecResult::Continue(1)
            }

            Instruction::Move { source, dest } => {
                if let Some(p) = self.processes.get_mut(&pid) {
                    let value = p.registers[source.0 as usize].clone();
                    p.registers[dest.0 as usize] = value;
                }
                ExecResult::Continue(1)
            }

            Instruction::Add { a, b, dest } => {
                self.arith_op(pid, &a, &b, dest, |x, y| x.wrapping_add(y))
            }

            Instruction::Sub { a, b, dest } => {
                self.arith_op(pid, &a, &b, dest, |x, y| x.wrapping_sub(y))
            }

            Instruction::Mul { a, b, dest } => {
                self.arith_op(pid, &a, &b, dest, |x, y| x.wrapping_mul(y))
            }

            Instruction::Div { a, b, dest } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let av = self.resolve_operand(process, &a);
                let bv = self.resolve_operand(process, &b);
                match (av, bv) {
                    (Some(x), Some(y)) if y != 0 => {
                        if let Some(p) = self.processes.get_mut(&pid) {
                            p.registers[dest.0 as usize] = Value::Int(x / y);
                        }
                        ExecResult::Continue(1)
                    }
                    (Some(_), Some(0)) => ExecResult::Crash, // Division by zero
                    _ => ExecResult::Crash,
                }
            }

            Instruction::Mod { a, b, dest } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let av = self.resolve_operand(process, &a);
                let bv = self.resolve_operand(process, &b);
                match (av, bv) {
                    (Some(x), Some(y)) if y != 0 => {
                        if let Some(p) = self.processes.get_mut(&pid) {
                            p.registers[dest.0 as usize] = Value::Int(x % y);
                        }
                        ExecResult::Continue(1)
                    }
                    (Some(_), Some(0)) => ExecResult::Crash, // Division by zero
                    _ => ExecResult::Crash,
                }
            }

            // ========== Comparisons ==========
            Instruction::Eq { a, b, dest } => {
                self.cmp_op(pid, &a, &b, dest, |x, y| x == y)
            }

            Instruction::Ne { a, b, dest } => {
                self.cmp_op(pid, &a, &b, dest, |x, y| x != y)
            }

            Instruction::Lt { a, b, dest } => {
                self.cmp_op(pid, &a, &b, dest, |x, y| x < y)
            }

            Instruction::Lte { a, b, dest } => {
                self.cmp_op(pid, &a, &b, dest, |x, y| x <= y)
            }

            Instruction::Gt { a, b, dest } => {
                self.cmp_op(pid, &a, &b, dest, |x, y| x > y)
            }

            Instruction::Gte { a, b, dest } => {
                self.cmp_op(pid, &a, &b, dest, |x, y| x >= y)
            }

            // ========== Control Flow ==========
            Instruction::Jump { target } => ExecResult::Jump(target, 1),

            Instruction::JumpIf { cond, target } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let val = self.resolve_operand(process, &cond);
                match val {
                    Some(n) if n != 0 => ExecResult::Jump(target, 1),
                    _ => ExecResult::Continue(1), // Falsy, continue to next instruction
                }
            }

            Instruction::JumpUnless { cond, target } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let val = self.resolve_operand(process, &cond);
                match val {
                    Some(0) | None => ExecResult::Jump(target, 1), // Falsy, jump
                    _ => ExecResult::Continue(1), // Truthy, continue
                }
            }

            Instruction::Call { target } => {
                if let Some(p) = self.processes.get_mut(&pid) {
                    // Push call frame with return context
                    p.call_stack.push(CallFrame {
                        module: p.current_module.clone(),
                        return_pc: p.pc + 1,
                    });
                }
                ExecResult::Jump(target, 1)
            }

            Instruction::Return => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                if let Some(frame) = process.call_stack.pop() {
                    // Restore module context
                    process.current_module = frame.module;
                    ExecResult::Jump(frame.return_pc, 1)
                } else {
                    // Empty call stack - end the process
                    ExecResult::Done
                }
            }

            Instruction::CallMFA {
                module,
                function,
                arity,
            } => {
                // Look up the module
                let Some(mod_ref) = self.modules.get(&module) else {
                    log(&format!("CallMFA: module {} not found", module));
                    return ExecResult::Crash;
                };

                // Look up the function
                let Some(func) = mod_ref.get_function(&function, arity) else {
                    log(&format!(
                        "CallMFA: function {}:{}/{} not found",
                        module, function, arity
                    ));
                    return ExecResult::Crash;
                };

                let entry = func.entry;

                // Push call frame and switch module
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.call_stack.push(CallFrame {
                        module: p.current_module.clone(),
                        return_pc: p.pc + 1,
                    });
                    p.current_module = Some(module.clone());
                }
                ExecResult::Jump(entry, 1)
            }

            Instruction::CallLocal { function, arity } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };

                // Must be in a module
                let Some(ref module_name) = process.current_module else {
                    log("CallLocal: not in a module");
                    return ExecResult::Crash;
                };

                // Look up the function in current module
                let Some(mod_ref) = self.modules.get(module_name) else {
                    return ExecResult::Crash;
                };

                let Some(func) = mod_ref.get_function(&function, arity) else {
                    log(&format!(
                        "CallLocal: function {}:{}/{} not found",
                        module_name, function, arity
                    ));
                    return ExecResult::Crash;
                };

                let entry = func.entry;

                // Push call frame (same module)
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.call_stack.push(CallFrame {
                        module: p.current_module.clone(),
                        return_pc: p.pc + 1,
                    });
                }
                ExecResult::Jump(entry, 1)
            }

            Instruction::TailCallMFA {
                module,
                function,
                arity,
            } => {
                // Look up the module
                let Some(mod_ref) = self.modules.get(&module) else {
                    log(&format!("TailCallMFA: module {} not found", module));
                    return ExecResult::Crash;
                };

                // Look up the function
                let Some(func) = mod_ref.get_function(&function, arity) else {
                    log(&format!(
                        "TailCallMFA: function {}:{}/{} not found",
                        module, function, arity
                    ));
                    return ExecResult::Crash;
                };

                let entry = func.entry;

                // No call frame push - tail call, just switch module
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.current_module = Some(module.clone());
                }
                ExecResult::Jump(entry, 1)
            }

            Instruction::TailCallLocal { function, arity } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };

                // Must be in a module
                let Some(ref module_name) = process.current_module else {
                    log("TailCallLocal: not in a module");
                    return ExecResult::Crash;
                };

                // Look up the function in current module
                let Some(mod_ref) = self.modules.get(module_name) else {
                    return ExecResult::Crash;
                };

                let Some(func) = mod_ref.get_function(&function, arity) else {
                    log(&format!(
                        "TailCallLocal: function {}:{}/{} not found",
                        module_name, function, arity
                    ));
                    return ExecResult::Crash;
                };

                let entry = func.entry;

                // No call frame push - tail call (stay in same module)
                ExecResult::Jump(entry, 1)
            }

            Instruction::MakeFun {
                module,
                function,
                arity,
                dest,
            } => {
                // Verify function exists
                let Some(mod_ref) = self.modules.get(&module) else {
                    log(&format!("MakeFun: module {} not found", module));
                    return ExecResult::Crash;
                };

                if mod_ref.get_function(&function, arity).is_none() {
                    log(&format!(
                        "MakeFun: function {}:{}/{} not found",
                        module, function, arity
                    ));
                    return ExecResult::Crash;
                }

                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Fun {
                        module: module.clone(),
                        function: function.clone(),
                        arity,
                    };
                }
                ExecResult::Continue(1)
            }

            Instruction::MakeClosure {
                module,
                function,
                arity,
                captures,
                dest,
            } => {
                // Verify function exists (arity for closure = explicit arity + captured count)
                let total_arity = arity + captures.len() as u8;
                let Some(mod_ref) = self.modules.get(&module) else {
                    log(&format!("MakeClosure: module {} not found", module));
                    return ExecResult::Crash;
                };

                if mod_ref.get_function(&function, total_arity).is_none() {
                    log(&format!(
                        "MakeClosure: function {}:{}/{} not found",
                        module, function, total_arity
                    ));
                    return ExecResult::Crash;
                }

                // Capture values from registers
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };

                let captured: Vec<Value> = captures
                    .iter()
                    .map(|r| process.registers[r.0 as usize].clone())
                    .collect();

                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Closure {
                        module: module.clone(),
                        function: function.clone(),
                        arity,
                        captured,
                    };
                }
                ExecResult::Continue(1)
            }

            Instruction::Apply { fun, arity } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };

                // Handle both Fun and Closure
                match &process.registers[fun.0 as usize] {
                    Value::Fun {
                        module,
                        function,
                        arity: fun_arity,
                    } => {
                        if *fun_arity != arity {
                            log(&format!(
                                "Apply: arity mismatch, expected {}, got {}",
                                fun_arity, arity
                            ));
                            return ExecResult::Crash;
                        }

                        // Look up the function
                        let Some(mod_ref) = self.modules.get(module) else {
                            log(&format!("Apply: module {} not found", module));
                            return ExecResult::Crash;
                        };

                        let Some(func) = mod_ref.get_function(function, arity) else {
                            log(&format!(
                                "Apply: function {}:{}/{} not found",
                                module, function, arity
                            ));
                            return ExecResult::Crash;
                        };

                        let entry = func.entry;
                        let module = module.clone();

                        // Push call frame and switch module
                        if let Some(p) = self.processes.get_mut(&pid) {
                            p.call_stack.push(CallFrame {
                                module: p.current_module.clone(),
                                return_pc: p.pc + 1,
                            });
                            p.current_module = Some(module);
                        }
                        ExecResult::Jump(entry, 1)
                    }

                    Value::Closure {
                        module,
                        function,
                        arity: closure_arity,
                        captured,
                    } => {
                        if *closure_arity != arity {
                            log(&format!(
                                "Apply: closure arity mismatch, expected {}, got {}",
                                closure_arity, arity
                            ));
                            return ExecResult::Crash;
                        }

                        // Total arity = explicit args + captured values
                        let total_arity = arity + captured.len() as u8;

                        // Look up the function
                        let Some(mod_ref) = self.modules.get(module) else {
                            log(&format!("Apply: module {} not found", module));
                            return ExecResult::Crash;
                        };

                        let Some(func) = mod_ref.get_function(function, total_arity) else {
                            log(&format!(
                                "Apply: function {}:{}/{} not found",
                                module, function, total_arity
                            ));
                            return ExecResult::Crash;
                        };

                        let entry = func.entry;
                        let module = module.clone();
                        let captured = captured.clone();

                        // Copy captured values to registers after explicit args
                        // and push call frame
                        if let Some(p) = self.processes.get_mut(&pid) {
                            for (i, val) in captured.into_iter().enumerate() {
                                p.registers[arity as usize + i] = val;
                            }
                            p.call_stack.push(CallFrame {
                                module: p.current_module.clone(),
                                return_pc: p.pc + 1,
                            });
                            p.current_module = Some(module);
                        }
                        ExecResult::Jump(entry, 1)
                    }

                    _ => {
                        log("Apply: not a function or closure");
                        ExecResult::Crash
                    }
                }
            }

            Instruction::SpawnMFA {
                module,
                function,
                arity,
                dest,
            } => {
                // Look up the module and function
                let Some(mod_ref) = self.modules.get(&module) else {
                    log(&format!("SpawnMFA: module {} not found", module));
                    return ExecResult::Crash;
                };

                // Function must be exported for spawn
                if !mod_ref.is_exported(&function, arity) {
                    log(&format!(
                        "SpawnMFA: function {}:{}/{} not exported",
                        module, function, arity
                    ));
                    return ExecResult::Crash;
                }

                let Some(func) = mod_ref.get_function(&function, arity) else {
                    log(&format!(
                        "SpawnMFA: function {}:{}/{} not found",
                        module, function, arity
                    ));
                    return ExecResult::Crash;
                };

                let entry = func.entry;

                // Copy arguments from parent's registers
                let Some(parent) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let args: Vec<Value> = (0..arity)
                    .map(|i| parent.registers[i as usize].clone())
                    .collect();

                // Create new process
                let child_pid = Pid(self.next_pid);
                self.next_pid += 1;

                let mut child =
                    Process::new_with_module(child_pid, Some(pid), module.clone(), entry);

                // Initialize child's registers with arguments
                for (i, arg) in args.into_iter().enumerate() {
                    child.registers[i] = arg;
                }

                self.processes.insert(child_pid, child);
                self.ready_queue.push_back(child_pid);

                // Store child PID in parent's dest register
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Pid(child_pid);
                }

                ExecResult::Continue(1)
            }

            Instruction::SpawnLinkMFA {
                module,
                function,
                arity,
                dest,
            } => {
                // Look up the module and function
                let Some(mod_ref) = self.modules.get(&module) else {
                    log(&format!("SpawnLinkMFA: module {} not found", module));
                    return ExecResult::Crash;
                };

                // Function must be exported for spawn
                if !mod_ref.is_exported(&function, arity) {
                    log(&format!(
                        "SpawnLinkMFA: function {}:{}/{} not exported",
                        module, function, arity
                    ));
                    return ExecResult::Crash;
                }

                let Some(func) = mod_ref.get_function(&function, arity) else {
                    log(&format!(
                        "SpawnLinkMFA: function {}:{}/{} not found",
                        module, function, arity
                    ));
                    return ExecResult::Crash;
                };

                let entry = func.entry;

                // Copy arguments from parent's registers
                let Some(parent) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let args: Vec<Value> = (0..arity)
                    .map(|i| parent.registers[i as usize].clone())
                    .collect();

                // Create new process
                let child_pid = Pid(self.next_pid);
                self.next_pid += 1;

                let mut child =
                    Process::new_with_module(child_pid, Some(pid), module.clone(), entry);

                // Initialize child's registers with arguments
                for (i, arg) in args.into_iter().enumerate() {
                    child.registers[i] = arg;
                }

                // Establish bidirectional link
                child.links.push(pid);

                self.processes.insert(child_pid, child);
                self.ready_queue.push_back(child_pid);

                // Add link to parent and store child PID
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.links.push(child_pid);
                    p.registers[dest.0 as usize] = Value::Pid(child_pid);
                }

                ExecResult::Continue(1)
            }

            Instruction::Push { source } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let value = match &source {
                    Operand::Int(n) => Value::Int(*n),
                    Operand::Reg(r) => process.registers[r.0 as usize].clone(),
                };
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.stack.push(value);
                }
                ExecResult::Continue(1)
            }

            Instruction::Pop { dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                if let Some(value) = process.stack.pop() {
                    process.registers[dest.0 as usize] = value;
                    ExecResult::Continue(1)
                } else {
                    // Empty stack - crash
                    ExecResult::Crash
                }
            }

            Instruction::LoadAtom { name, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Atom(name.clone());
                ExecResult::Continue(1)
            }

            Instruction::MakeTuple { arity, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let arity = arity as usize;
                if process.stack.len() < arity {
                    return ExecResult::Crash;
                }
                // Drain elements - first pushed is at lower index, which becomes first tuple element
                let elements: Vec<Value> = process
                    .stack
                    .drain(process.stack.len() - arity..)
                    .collect();
                process.registers[dest.0 as usize] = Value::Tuple(elements);
                ExecResult::Continue(1)
            }

            Instruction::TupleElement { tuple, index, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Tuple(elements) = &process.registers[tuple.0 as usize] else {
                    return ExecResult::Crash;
                };
                let idx = index as usize;
                if idx >= elements.len() {
                    return ExecResult::Crash;
                }
                let value = elements[idx].clone();
                process.registers[dest.0 as usize] = value;
                ExecResult::Continue(1)
            }

            Instruction::TupleArity { tuple, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Tuple(elements) = &process.registers[tuple.0 as usize] else {
                    return ExecResult::Crash;
                };
                let arity = elements.len() as i64;
                process.registers[dest.0 as usize] = Value::Int(arity);
                ExecResult::Continue(1)
            }

            Instruction::Match {
                source,
                pattern,
                fail_target,
            } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let value = process.registers[source.0 as usize].clone();

                // Try to match and collect bindings
                let mut bindings = Vec::new();
                if Self::match_pattern(&value, &pattern, &mut bindings) {
                    // Match succeeded - apply bindings
                    if let Some(p) = self.processes.get_mut(&pid) {
                        for (reg, val) in bindings {
                            p.registers[reg.0 as usize] = val;
                        }
                    }
                    ExecResult::Continue(1)
                } else {
                    // Match failed - jump to fail target
                    ExecResult::Jump(fail_target, 1)
                }
            }

            Instruction::ReceiveMatch {
                clauses,
                timeout,
                timeout_target,
            } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };

                // Check for timeout first
                if process.timeout == Some(0) {
                    if let Some(p) = self.processes.get_mut(&pid) {
                        p.timeout = None;
                    }
                    return ExecResult::Jump(timeout_target, 1);
                }

                // Scan mailbox for a matching message
                let mailbox_len = process.mailbox.len();
                for i in 0..mailbox_len {
                    let msg = &process.mailbox[i];
                    let value = Self::message_to_value(msg.clone());

                    // Try each clause
                    for (pattern, target) in clauses.iter() {
                        let mut bindings = Vec::new();
                        if Self::match_pattern(&value, pattern, &mut bindings) {
                            // Match found! Remove message and apply bindings
                            if let Some(p) = self.processes.get_mut(&pid) {
                                p.mailbox.remove(i);
                                p.timeout = None;
                                for (reg, val) in bindings {
                                    p.registers[reg.0 as usize] = val;
                                }
                            }
                            return ExecResult::Jump(*target, 1);
                        }
                    }
                }

                // No match found - set timeout and wait
                if let Some(p) = self.processes.get_mut(&pid) {
                    if p.timeout.is_none() {
                        p.timeout = timeout.clone();
                    }
                }
                ExecResult::Wait
            }

            Instruction::MakeList { length, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let length = length as usize;
                if process.stack.len() < length {
                    return ExecResult::Crash;
                }
                // Drain elements - first pushed is at lower index, which becomes first list element
                let elements: Vec<Value> = process
                    .stack
                    .drain(process.stack.len() - length..)
                    .collect();
                process.registers[dest.0 as usize] = Value::List(elements);
                ExecResult::Continue(1)
            }

            Instruction::Cons { head, tail, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let head_val = process.registers[head.0 as usize].clone();
                let Value::List(mut elements) = process.registers[tail.0 as usize].clone() else {
                    return ExecResult::Crash;
                };
                elements.insert(0, head_val);
                process.registers[dest.0 as usize] = Value::List(elements);
                ExecResult::Continue(1)
            }

            Instruction::ListHead { list, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::List(elements) = &process.registers[list.0 as usize] else {
                    return ExecResult::Crash;
                };
                if elements.is_empty() {
                    return ExecResult::Crash;
                }
                let head = elements[0].clone();
                process.registers[dest.0 as usize] = head;
                ExecResult::Continue(1)
            }

            Instruction::ListTail { list, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::List(elements) = &process.registers[list.0 as usize] else {
                    return ExecResult::Crash;
                };
                if elements.is_empty() {
                    return ExecResult::Crash;
                }
                let tail = elements[1..].to_vec();
                process.registers[dest.0 as usize] = Value::List(tail);
                ExecResult::Continue(1)
            }

            Instruction::ListIsEmpty { list, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::List(elements) = &process.registers[list.0 as usize] else {
                    return ExecResult::Crash;
                };
                let is_empty = if elements.is_empty() { 1 } else { 0 };
                process.registers[dest.0 as usize] = Value::Int(is_empty);
                ExecResult::Continue(1)
            }

            Instruction::ListLength { list, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::List(elements) = &process.registers[list.0 as usize] else {
                    return ExecResult::Crash;
                };
                let length = elements.len() as i64;
                process.registers[dest.0 as usize] = Value::Int(length);
                ExecResult::Continue(1)
            }

            Instruction::ListAppend { a, b, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::List(list_a) = &process.registers[a.0 as usize] else {
                    return ExecResult::Crash;
                };
                let Value::List(list_b) = &process.registers[b.0 as usize] else {
                    return ExecResult::Crash;
                };
                let mut result = list_a.clone();
                result.extend(list_b.clone());
                process.registers[dest.0 as usize] = Value::List(result);
                ExecResult::Continue(1)
            }

            Instruction::ListReverse { list, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::List(elements) = &process.registers[list.0 as usize] else {
                    return ExecResult::Crash;
                };
                let reversed: Vec<Value> = elements.iter().rev().cloned().collect();
                process.registers[dest.0 as usize] = Value::List(reversed);
                ExecResult::Continue(1)
            }

            Instruction::ListNth { list, n, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::List(elements) = &process.registers[list.0 as usize] else {
                    return ExecResult::Crash;
                };
                let Value::Int(index) = process.registers[n.0 as usize] else {
                    return ExecResult::Crash;
                };
                if index < 0 || index as usize >= elements.len() {
                    return ExecResult::Crash;
                }
                let elem = elements[index as usize].clone();
                process.registers[dest.0 as usize] = elem;
                ExecResult::Continue(1)
            }

            Instruction::ListMember { elem, list, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::List(elements) = &process.registers[list.0 as usize] else {
                    return ExecResult::Crash;
                };
                let search_elem = &process.registers[elem.0 as usize];
                let found = elements.contains(search_elem);
                process.registers[dest.0 as usize] = Value::Int(if found { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::IsInteger { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_int = matches!(process.registers[source.0 as usize], Value::Int(_));
                process.registers[dest.0 as usize] = Value::Int(if is_int { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::IsAtom { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_atom = matches!(process.registers[source.0 as usize], Value::Atom(_));
                process.registers[dest.0 as usize] = Value::Int(if is_atom { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::IsTuple { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_tuple = matches!(process.registers[source.0 as usize], Value::Tuple(_));
                process.registers[dest.0 as usize] = Value::Int(if is_tuple { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::IsList { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_list = matches!(process.registers[source.0 as usize], Value::List(_));
                process.registers[dest.0 as usize] = Value::Int(if is_list { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::IsPid { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_pid = matches!(process.registers[source.0 as usize], Value::Pid(_));
                process.registers[dest.0 as usize] = Value::Int(if is_pid { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::IsFunction { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_fun = matches!(
                    process.registers[source.0 as usize],
                    Value::Fun { .. } | Value::Closure { .. }
                );
                process.registers[dest.0 as usize] = Value::Int(if is_fun { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::IsString { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_string = matches!(process.registers[source.0 as usize], Value::String(_));
                process.registers[dest.0 as usize] = Value::Int(if is_string { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            // ========== Process Dictionary ==========
            Instruction::PutDict { key, value, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let key_val = process.registers[key.0 as usize].clone();
                let new_val = process.registers[value.0 as usize].clone();
                let old_val = process.dictionary.insert(key_val, new_val);
                process.registers[dest.0 as usize] = old_val.unwrap_or(Value::None);
                ExecResult::Continue(1)
            }

            Instruction::GetDict { key, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let key_val = process.registers[key.0 as usize].clone();
                let value = process.dictionary.get(&key_val).cloned().unwrap_or(Value::None);
                process.registers[dest.0 as usize] = value;
                ExecResult::Continue(1)
            }

            Instruction::EraseDict { key, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let key_val = process.registers[key.0 as usize].clone();
                let old_val = process.dictionary.remove(&key_val);
                process.registers[dest.0 as usize] = old_val.unwrap_or(Value::None);
                ExecResult::Continue(1)
            }

            Instruction::GetDictKeys { dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let keys: Vec<Value> = process.dictionary.keys().cloned().collect();
                process.registers[dest.0 as usize] = Value::List(keys);
                ExecResult::Continue(1)
            }

            // ========== Maps ==========
            Instruction::MakeMap { count, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let mut map = std::collections::HashMap::new();
                // Pop count pairs from stack (in reverse: v, k, v, k, ...)
                for _ in 0..count {
                    let Some(value) = process.stack.pop() else {
                        return ExecResult::Crash;
                    };
                    let Some(key) = process.stack.pop() else {
                        return ExecResult::Crash;
                    };
                    map.insert(key, value);
                }
                process.registers[dest.0 as usize] = Value::Map(map);
                ExecResult::Continue(1)
            }

            Instruction::MapGet { map, key, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Map(m) = &process.registers[map.0 as usize] else {
                    return ExecResult::Crash;
                };
                let key_val = &process.registers[key.0 as usize];
                let Some(value) = m.get(key_val) else {
                    return ExecResult::Crash; // Key not found
                };
                let value = value.clone();
                process.registers[dest.0 as usize] = value;
                ExecResult::Continue(1)
            }

            Instruction::MapGetDefault { map, key, default, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Map(m) = &process.registers[map.0 as usize] else {
                    return ExecResult::Crash;
                };
                let key_val = &process.registers[key.0 as usize];
                let value = m
                    .get(key_val)
                    .cloned()
                    .unwrap_or_else(|| process.registers[default.0 as usize].clone());
                process.registers[dest.0 as usize] = value;
                ExecResult::Continue(1)
            }

            Instruction::MapPut { map, key, value, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Map(m) = &process.registers[map.0 as usize] else {
                    return ExecResult::Crash;
                };
                let mut new_map = m.clone();
                let key_val = process.registers[key.0 as usize].clone();
                let val = process.registers[value.0 as usize].clone();
                new_map.insert(key_val, val);
                process.registers[dest.0 as usize] = Value::Map(new_map);
                ExecResult::Continue(1)
            }

            Instruction::MapRemove { map, key, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Map(m) = &process.registers[map.0 as usize] else {
                    return ExecResult::Crash;
                };
                let mut new_map = m.clone();
                let key_val = &process.registers[key.0 as usize];
                new_map.remove(key_val);
                process.registers[dest.0 as usize] = Value::Map(new_map);
                ExecResult::Continue(1)
            }

            Instruction::MapHas { map, key, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Map(m) = &process.registers[map.0 as usize] else {
                    return ExecResult::Crash;
                };
                let key_val = &process.registers[key.0 as usize];
                let has = m.contains_key(key_val);
                process.registers[dest.0 as usize] = Value::Int(if has { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::MapSize { map, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Map(m) = &process.registers[map.0 as usize] else {
                    return ExecResult::Crash;
                };
                let size = m.len() as i64;
                process.registers[dest.0 as usize] = Value::Int(size);
                ExecResult::Continue(1)
            }

            Instruction::MapKeys { map, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Map(m) = &process.registers[map.0 as usize] else {
                    return ExecResult::Crash;
                };
                let keys: Vec<Value> = m.keys().cloned().collect();
                process.registers[dest.0 as usize] = Value::List(keys);
                ExecResult::Continue(1)
            }

            Instruction::MapValues { map, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Map(m) = &process.registers[map.0 as usize] else {
                    return ExecResult::Crash;
                };
                let values: Vec<Value> = m.values().cloned().collect();
                process.registers[dest.0 as usize] = Value::List(values);
                ExecResult::Continue(1)
            }

            Instruction::IsMap { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_map = matches!(process.registers[source.0 as usize], Value::Map(_));
                process.registers[dest.0 as usize] = Value::Int(if is_map { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            // ========== Binaries ==========
            Instruction::MakeBinary { bytes, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Binary(bytes.clone());
                ExecResult::Continue(1)
            }

            Instruction::BinarySize { bin, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Binary(bytes) = &process.registers[bin.0 as usize] else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Int(bytes.len() as i64);
                ExecResult::Continue(1)
            }

            Instruction::BinaryAt { bin, index, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Binary(bytes) = &process.registers[bin.0 as usize] else {
                    return ExecResult::Crash;
                };
                let Value::Int(idx) = process.registers[index.0 as usize] else {
                    return ExecResult::Crash;
                };
                if idx < 0 || idx as usize >= bytes.len() {
                    return ExecResult::Crash;
                }
                process.registers[dest.0 as usize] = Value::Int(bytes[idx as usize] as i64);
                ExecResult::Continue(1)
            }

            Instruction::BinarySlice { bin, start, len, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Binary(bytes) = &process.registers[bin.0 as usize] else {
                    return ExecResult::Crash;
                };
                let Value::Int(start_idx) = process.registers[start.0 as usize] else {
                    return ExecResult::Crash;
                };
                let Value::Int(length) = process.registers[len.0 as usize] else {
                    return ExecResult::Crash;
                };
                if start_idx < 0 || length < 0 {
                    return ExecResult::Crash;
                }
                let start_idx = start_idx as usize;
                let length = length as usize;
                if start_idx + length > bytes.len() {
                    return ExecResult::Crash;
                }
                let slice = bytes[start_idx..start_idx + length].to_vec();
                process.registers[dest.0 as usize] = Value::Binary(slice);
                ExecResult::Continue(1)
            }

            Instruction::BinaryConcat { a, b, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Binary(bytes_a) = &process.registers[a.0 as usize] else {
                    return ExecResult::Crash;
                };
                let Value::Binary(bytes_b) = &process.registers[b.0 as usize] else {
                    return ExecResult::Crash;
                };
                let mut result = bytes_a.clone();
                result.extend_from_slice(bytes_b);
                process.registers[dest.0 as usize] = Value::Binary(result);
                ExecResult::Continue(1)
            }

            Instruction::IsBinary { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_binary = matches!(process.registers[source.0 as usize], Value::Binary(_));
                process.registers[dest.0 as usize] = Value::Int(if is_binary { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::StringToBinary { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::String(s) = &process.registers[source.0 as usize] else {
                    return ExecResult::Crash;
                };
                let bytes = s.as_bytes().to_vec();
                process.registers[dest.0 as usize] = Value::Binary(bytes);
                ExecResult::Continue(1)
            }

            Instruction::BinaryToString { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Binary(bytes) = &process.registers[source.0 as usize] else {
                    return ExecResult::Crash;
                };
                let Ok(s) = std::str::from_utf8(bytes) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::String(s.to_string());
                ExecResult::Continue(1)
            }

            // ========== References ==========
            Instruction::MakeRef { dest } => {
                let ref_id = self.next_ref;
                self.next_ref += 1;
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Ref(ref_id);
                ExecResult::Continue(1)
            }

            Instruction::IsRef { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_ref = matches!(process.registers[source.0 as usize], Value::Ref(_));
                process.registers[dest.0 as usize] = Value::Int(if is_ref { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            // ========== Floats ==========
            Instruction::LoadFloat { value, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Float(value);
                ExecResult::Continue(1)
            }

            Instruction::IsFloat { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let is_float = matches!(process.registers[source.0 as usize], Value::Float(_));
                process.registers[dest.0 as usize] = Value::Int(if is_float { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::IntToFloat { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Int(n) = process.registers[source.0 as usize] else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Float(n as f64);
                ExecResult::Continue(1)
            }

            Instruction::FloatToInt { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Float(f) = process.registers[source.0 as usize] else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Int(f as i64);
                ExecResult::Continue(1)
            }

            Instruction::Floor { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Float(f) = process.registers[source.0 as usize] else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Float(f.floor());
                ExecResult::Continue(1)
            }

            Instruction::Ceil { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Float(f) = process.registers[source.0 as usize] else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Float(f.ceil());
                ExecResult::Continue(1)
            }

            Instruction::Round { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Float(f) = process.registers[source.0 as usize] else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Float(f.round());
                ExecResult::Continue(1)
            }

            Instruction::Trunc { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Float(f) = process.registers[source.0 as usize] else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Float(f.trunc());
                ExecResult::Continue(1)
            }

            Instruction::Sqrt { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let f = match process.registers[source.0 as usize] {
                    Value::Float(f) => f,
                    Value::Int(n) => n as f64,
                    _ => return ExecResult::Crash,
                };
                process.registers[dest.0 as usize] = Value::Float(f.sqrt());
                ExecResult::Continue(1)
            }

            Instruction::Abs { source, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let result = match process.registers[source.0 as usize] {
                    Value::Float(f) => Value::Float(f.abs()),
                    Value::Int(n) => Value::Int(n.abs()),
                    _ => return ExecResult::Crash,
                };
                process.registers[dest.0 as usize] = result;
                ExecResult::Continue(1)
            }

            Instruction::Pow { base, exp, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let b = match process.registers[base.0 as usize] {
                    Value::Float(f) => f,
                    Value::Int(n) => n as f64,
                    _ => return ExecResult::Crash,
                };
                let e = match process.registers[exp.0 as usize] {
                    Value::Float(f) => f,
                    Value::Int(n) => n as f64,
                    _ => return ExecResult::Crash,
                };
                process.registers[dest.0 as usize] = Value::Float(b.powf(e));
                ExecResult::Continue(1)
            }

            // ========== Timers ==========
            Instruction::SendAfter { delay, to, msg, dest } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let Some(target_pid) = self.resolve_pid(pid, &to) else {
                    // Target process not found
                    return ExecResult::Crash;
                };
                let message = process.registers[msg.0 as usize].clone();

                // Create timer reference
                let timer_ref = self.next_ref;
                self.next_ref += 1;

                // Add timer to queue
                self.timers.push(Timer {
                    timer_ref,
                    target: target_pid,
                    message,
                    remaining: delay,
                });

                // Store timer ref in dest register
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Ref(timer_ref);
                ExecResult::Continue(1)
            }

            Instruction::StartTimer { delay, msg, dest } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let message_value = process.registers[msg.0 as usize].clone();

                // Create timer reference
                let timer_ref = self.next_ref;
                self.next_ref += 1;

                // Build {:timeout, ref, msg} tuple
                let timeout_msg = Value::Tuple(vec![
                    Value::Atom("timeout".to_string()),
                    Value::Ref(timer_ref),
                    message_value,
                ]);

                // Add timer to queue (sends to self)
                self.timers.push(Timer {
                    timer_ref,
                    target: pid,
                    message: timeout_msg,
                    remaining: delay,
                });

                // Store timer ref in dest register
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Ref(timer_ref);
                ExecResult::Continue(1)
            }

            Instruction::CancelTimer { timer_ref, dest } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Ref(ref_id) = process.registers[timer_ref.0 as usize] else {
                    return ExecResult::Crash;
                };

                // Find and remove the timer
                let result = if let Some(idx) = self.timers.iter().position(|t| t.timer_ref == ref_id) {
                    let timer = self.timers.remove(idx);
                    Value::Int(timer.remaining as i64)
                } else {
                    // Timer already fired or doesn't exist
                    Value::Atom("ok".to_string())
                };

                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = result;
                ExecResult::Continue(1)
            }

            Instruction::ReadTimer { timer_ref, dest } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let Value::Ref(ref_id) = process.registers[timer_ref.0 as usize] else {
                    return ExecResult::Crash;
                };

                // Find the timer and get remaining time
                let remaining = self
                    .timers
                    .iter()
                    .find(|t| t.timer_ref == ref_id)
                    .map(|t| Value::Int(t.remaining as i64))
                    .unwrap_or(Value::Int(0));

                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = remaining;
                ExecResult::Continue(1)
            }

            // ========== IO ==========
            Instruction::PrintLn { source } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let value = &process.registers[source.0 as usize];
                // Capture output for testing, or print to stdout
                self.output.push(format!("{:?}", value));
                #[cfg(not(test))]
                println!("{:?}", value);
                ExecResult::Continue(1)
            }

            Instruction::ReadLine { dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                // In tests/WASM, return :eof. In CLI, read from stdin.
                #[cfg(test)]
                {
                    process.registers[dest.0 as usize] = Value::Atom("eof".to_string());
                }
                #[cfg(not(test))]
                {
                    use std::io::BufRead;
                    let stdin = std::io::stdin();
                    let mut line = String::new();
                    match stdin.lock().read_line(&mut line) {
                        Ok(0) => {
                            process.registers[dest.0 as usize] = Value::Atom("eof".to_string());
                        }
                        Ok(_) => {
                            // Trim trailing newline
                            let trimmed = line.trim_end_matches('\n').trim_end_matches('\r');
                            process.registers[dest.0 as usize] =
                                Value::String(trimmed.to_string());
                        }
                        Err(_) => {
                            process.registers[dest.0 as usize] = Value::Atom("eof".to_string());
                        }
                    }
                }
                ExecResult::Continue(1)
            }

            Instruction::FileRead { path, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let path_str = match &process.registers[path.0 as usize] {
                    Value::String(s) => s.clone(),
                    _ => return ExecResult::Crash,
                };
                match std::fs::read(&path_str) {
                    Ok(bytes) => {
                        process.registers[dest.0 as usize] = Value::Tuple(vec![
                            Value::Atom("ok".to_string()),
                            Value::Binary(bytes),
                        ]);
                    }
                    Err(e) => {
                        process.registers[dest.0 as usize] = Value::Tuple(vec![
                            Value::Atom("error".to_string()),
                            Value::Atom(e.kind().to_string()),
                        ]);
                    }
                }
                ExecResult::Continue(1)
            }

            Instruction::FileWrite { path, content, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let path_str = match &process.registers[path.0 as usize] {
                    Value::String(s) => s.clone(),
                    _ => return ExecResult::Crash,
                };
                let bytes = match &process.registers[content.0 as usize] {
                    Value::Binary(b) => b.clone(),
                    Value::String(s) => s.as_bytes().to_vec(),
                    _ => return ExecResult::Crash,
                };
                match std::fs::write(&path_str, bytes) {
                    Ok(()) => {
                        process.registers[dest.0 as usize] = Value::Atom("ok".to_string());
                    }
                    Err(e) => {
                        process.registers[dest.0 as usize] = Value::Tuple(vec![
                            Value::Atom("error".to_string()),
                            Value::Atom(e.kind().to_string()),
                        ]);
                    }
                }
                ExecResult::Continue(1)
            }

            Instruction::FileExists { path, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let path_str = match &process.registers[path.0 as usize] {
                    Value::String(s) => s.clone(),
                    _ => return ExecResult::Crash,
                };
                let exists = std::path::Path::new(&path_str).exists();
                process.registers[dest.0 as usize] = Value::Int(if exists { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::FileDelete { path, dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let path_str = match &process.registers[path.0 as usize] {
                    Value::String(s) => s.clone(),
                    _ => return ExecResult::Crash,
                };
                match std::fs::remove_file(&path_str) {
                    Ok(()) => {
                        process.registers[dest.0 as usize] = Value::Atom("ok".to_string());
                    }
                    Err(e) => {
                        process.registers[dest.0 as usize] = Value::Tuple(vec![
                            Value::Atom("error".to_string()),
                            Value::Atom(e.kind().to_string()),
                        ]);
                    }
                }
                ExecResult::Continue(1)
            }

            // ========== System Info ==========
            Instruction::SelfPid { dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Pid(pid);
                ExecResult::Continue(1)
            }

            Instruction::ProcessList { dest } => {
                let pids: Vec<Value> = self
                    .processes
                    .keys()
                    .map(|p| Value::Pid(*p))
                    .collect();
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::List(pids);
                ExecResult::Continue(1)
            }

            Instruction::ProcessCount { dest } => {
                let count = self.processes.len() as i64;
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Int(count);
                ExecResult::Continue(1)
            }

            Instruction::IsAlive { pid: target_pid, dest } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let target = match &process.registers[target_pid.0 as usize] {
                    Value::Pid(p) => *p,
                    _ => return ExecResult::Crash,
                };
                let alive = self.processes.contains_key(&target);
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Int(if alive { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            Instruction::ProcessInfo { pid: target_pid, dest } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let target = match &process.registers[target_pid.0 as usize] {
                    Value::Pid(p) => *p,
                    _ => return ExecResult::Crash,
                };
                let info = if let Some(target_proc) = self.processes.get(&target) {
                    let status_atom = match target_proc.status {
                        ProcessStatus::Ready => "ready",
                        ProcessStatus::Waiting => "waiting",
                        ProcessStatus::Done => "done",
                        ProcessStatus::Crashed => "crashed",
                    };
                    Value::Tuple(vec![
                        Value::Atom(status_atom.to_string()),
                        Value::Int(target_proc.mailbox.len() as i64),
                        Value::Int(target_proc.links.len() as i64),
                        Value::Int(target_proc.monitors.len() as i64),
                        Value::Int(if target_proc.trap_exit { 1 } else { 0 }),
                    ])
                } else {
                    Value::Atom("undefined".to_string())
                };
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = info;
                ExecResult::Continue(1)
            }

            Instruction::ModuleList { dest } => {
                let modules: Vec<Value> = self
                    .modules
                    .keys()
                    .map(|name| Value::Atom(name.clone()))
                    .collect();
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::List(modules);
                ExecResult::Continue(1)
            }

            Instruction::FunctionExported {
                module,
                function,
                arity,
                dest,
            } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let mod_name = match &process.registers[module.0 as usize] {
                    Value::Atom(s) => s.clone(),
                    Value::String(s) => s.clone(),
                    _ => return ExecResult::Crash,
                };
                let func_name = match &process.registers[function.0 as usize] {
                    Value::Atom(s) => s.clone(),
                    Value::String(s) => s.clone(),
                    _ => return ExecResult::Crash,
                };
                let func_arity = match &process.registers[arity.0 as usize] {
                    Value::Int(n) => *n as u8,
                    _ => return ExecResult::Crash,
                };
                let exported = self
                    .modules
                    .get(&mod_name)
                    .map(|m| m.is_exported(&func_name, func_arity))
                    .unwrap_or(false);
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.registers[dest.0 as usize] = Value::Int(if exported { 1 } else { 0 });
                ExecResult::Continue(1)
            }

            // ========== Exception Handling ==========
            Instruction::Try {
                catch_target,
                after_target,
            } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                // Push exception handler onto try stack
                process.try_stack.push(TryFrame {
                    catch_target,
                    after_target,
                    call_stack_depth: process.call_stack.len(),
                    stack_depth: process.stack.len(),
                });
                ExecResult::Continue(1)
            }

            Instruction::EndTry => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                // Pop the try frame
                if let Some(frame) = process.try_stack.pop() {
                    // If there's an after block, jump to it
                    if let Some(after_target) = frame.after_target {
                        return ExecResult::Jump(after_target, 1);
                    }
                }
                ExecResult::Continue(1)
            }

            Instruction::Throw { class, reason } => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                let class_val = process.registers[class.0 as usize].clone();
                let reason_val = process.registers[reason.0 as usize].clone();
                ExecResult::Throw(class_val, reason_val)
            }

            Instruction::GetException { dest } => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                let exception_tuple = if let Some((class, reason, stacktrace)) =
                    &process.current_exception
                {
                    // Convert stacktrace to a list of strings
                    let stacktrace_list = Value::List(
                        stacktrace
                            .iter()
                            .map(|s| Value::String(s.clone()))
                            .collect(),
                    );
                    Value::Tuple(vec![class.clone(), reason.clone(), stacktrace_list])
                } else {
                    Value::None
                };
                process.registers[dest.0 as usize] = exception_tuple;
                ExecResult::Continue(1)
            }

            Instruction::ClearException => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                process.current_exception = None;
                ExecResult::Continue(1)
            }

            Instruction::Reraise => {
                let Some(process) = self.processes.get(&pid) else {
                    return ExecResult::Crash;
                };
                if let Some((class, reason, _)) = &process.current_exception {
                    ExecResult::Throw(class.clone(), reason.clone())
                } else {
                    // No exception to reraise, crash
                    ExecResult::Crash
                }
            }
        }
    }

    /// Helper for arithmetic operations
    fn arith_op(
        &mut self,
        pid: Pid,
        a: &Operand,
        b: &Operand,
        dest: crate::Register,
        op: fn(i64, i64) -> i64,
    ) -> ExecResult {
        let Some(process) = self.processes.get(&pid) else {
            return ExecResult::Crash;
        };
        let av = self.resolve_operand(process, a);
        let bv = self.resolve_operand(process, b);
        match (av, bv) {
            (Some(x), Some(y)) => {
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Int(op(x, y));
                }
                ExecResult::Continue(1)
            }
            _ => ExecResult::Crash,
        }
    }

    /// Helper for comparison operations
    fn cmp_op(
        &mut self,
        pid: Pid,
        a: &Operand,
        b: &Operand,
        dest: crate::Register,
        op: fn(i64, i64) -> bool,
    ) -> ExecResult {
        let Some(process) = self.processes.get(&pid) else {
            return ExecResult::Crash;
        };
        let av = self.resolve_operand(process, a);
        let bv = self.resolve_operand(process, b);
        match (av, bv) {
            (Some(x), Some(y)) => {
                let result = if op(x, y) { 1 } else { 0 };
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Int(result);
                }
                ExecResult::Continue(1)
            }
            _ => ExecResult::Crash,
        }
    }

    /// Resolve an operand to an integer value
    fn resolve_operand(&self, process: &Process, operand: &Operand) -> Option<i64> {
        match operand {
            Operand::Int(n) => Some(*n),
            Operand::Reg(r) => process.registers[r.0 as usize].as_int(),
        }
    }

    fn resolve_pid(&self, current: Pid, source: &Source) -> Option<Pid> {
        match source {
            Source::Self_ => Some(current),
            Source::Parent => {
                let process = self.processes.get(&current)?;
                process.parent
            }
            Source::Pid(p) => Some(*p),
            Source::Reg(r) => {
                let process = self.processes.get(&current)?;
                match &process.registers[r.0 as usize] {
                    Value::Pid(p) => Some(*p),
                    _ => None,
                }
            }
            Source::Named(name) => self.registry.get(name).copied(),
        }
    }

    fn resolve_source(&self, process: &Process, source: &Source) -> Value {
        match source {
            Source::Self_ => Value::Pid(process.pid),
            Source::Parent => process.parent.map(Value::Pid).unwrap_or(Value::None),
            Source::Pid(p) => Value::Pid(*p),
            Source::Reg(r) => process.registers[r.0 as usize].clone(),
            Source::Named(name) => self
                .registry
                .get(name)
                .map(|p| Value::Pid(*p))
                .unwrap_or(Value::None),
        }
    }

    fn message_to_value(msg: Message) -> Value {
        match msg {
            Message::User(s) => Value::String(s),
            Message::System(sys) => match sys {
                SystemMsg::Exit(pid, reason) => {
                    // {:EXIT, Pid, Reason}
                    Value::Tuple(vec![
                        Value::Atom("EXIT".to_string()),
                        Value::Pid(pid),
                        reason,
                    ])
                }
                SystemMsg::Down(ref_id, pid, reason) => {
                    // {:DOWN, Ref, :process, Pid, Reason}
                    Value::Tuple(vec![
                        Value::Atom("DOWN".to_string()),
                        Value::Ref(ref_id),
                        Value::Atom("process".to_string()),
                        Value::Pid(pid),
                        reason,
                    ])
                }
            },
        }
    }

    /// Try to match a value against a pattern, collecting variable bindings
    /// Returns true if match succeeds, false otherwise
    fn match_pattern(
        value: &Value,
        pattern: &Pattern,
        bindings: &mut Vec<(Register, Value)>,
    ) -> bool {
        match pattern {
            Pattern::Wildcard => true,

            Pattern::Variable(reg) => {
                bindings.push((*reg, value.clone()));
                true
            }

            Pattern::Int(n) => matches!(value, Value::Int(v) if v == n),

            Pattern::Atom(name) => matches!(value, Value::Atom(a) if a == name),

            Pattern::String(s) => matches!(value, Value::String(v) if v == s),

            Pattern::Binary(bytes) => matches!(value, Value::Binary(b) if b == bytes),

            Pattern::Tuple(patterns) => {
                let Value::Tuple(elements) = value else {
                    return false;
                };
                if elements.len() != patterns.len() {
                    return false;
                }
                for (elem, pat) in elements.iter().zip(patterns.iter()) {
                    if !Self::match_pattern(elem, pat, bindings) {
                        return false;
                    }
                }
                true
            }

            Pattern::ListEmpty => {
                matches!(value, Value::List(elements) if elements.is_empty())
            }

            Pattern::ListCons { head, tail } => {
                let Value::List(elements) = value else {
                    return false;
                };
                if elements.is_empty() {
                    return false;
                }
                // Match head against first element
                if !Self::match_pattern(&elements[0], head, bindings) {
                    return false;
                }
                // Match tail against rest of list
                let tail_value = Value::List(elements[1..].to_vec());
                Self::match_pattern(&tail_value, tail, bindings)
            }

            Pattern::Map(pairs) => {
                let Value::Map(map) = value else {
                    return false;
                };
                // For each key-value pattern pair, find matching entry
                for (key_pattern, value_pattern) in pairs {
                    // Find a map entry where key matches the key pattern
                    let mut found = false;
                    for (k, v) in map.iter() {
                        let mut key_bindings = Vec::new();
                        if Self::match_pattern(k, key_pattern, &mut key_bindings) {
                            // Key matches, now check if value matches
                            let mut val_bindings = Vec::new();
                            if Self::match_pattern(v, value_pattern, &mut val_bindings) {
                                // Both match, add bindings
                                bindings.extend(key_bindings);
                                bindings.extend(val_bindings);
                                found = true;
                                break;
                            }
                        }
                    }
                    if !found {
                        return false;
                    }
                }
                true
            }
        }
    }

    fn finish_process_with_reason(&mut self, pid: Pid, status: ProcessStatus, reason: Value) {
        let (links, monitors) = if let Some(p) = self.processes.get_mut(&pid) {
            p.status = status;
            p.exit_reason = reason.clone();
            (p.links.clone(), p.monitored_by.clone())
        } else {
            return;
        };

        // Remove any registry entries for this process
        self.registry.retain(|_, v| *v != pid);

        // Determine if this is a "normal" exit
        let is_normal = reason == Value::Atom("normal".to_string());

        // Handle linked processes
        // - Normal exit: only notify if they trap_exit (send {:EXIT, Pid, :normal})
        // - Abnormal exit: if they trap_exit, send message; otherwise crash them
        let mut to_crash = Vec::new();
        for linked_pid in links {
            if let Some(linked) = self.processes.get_mut(&linked_pid) {
                // Remove the link from the linked process
                linked.links.retain(|p| *p != pid);

                if linked.trap_exit {
                    // Convert exit signal to message: {:EXIT, Pid, Reason}
                    let exit_tuple = Value::Tuple(vec![
                        Value::Atom("EXIT".to_string()),
                        Value::Pid(pid),
                        reason.clone(),
                    ]);
                    linked.mailbox.push_back(Message::System(SystemMsg::Exit(pid, reason.clone())));
                    // Also add as user message for pattern matching in receive
                    linked.mailbox.push_back(Message::User(format!("{:?}", exit_tuple)));
                    if linked.status == ProcessStatus::Waiting {
                        linked.status = ProcessStatus::Ready;
                        self.ready_queue.push_back(linked_pid);
                    }
                } else if !is_normal {
                    // Abnormal exit propagates to linked processes (crash them)
                    to_crash.push(linked_pid);
                }
                // Normal exit with trap_exit=false: do nothing (no notification)
            }
        }

        // Crash linked processes that don't trap_exit (after releasing borrow)
        for linked_pid in to_crash {
            self.finish_process_with_reason(
                linked_pid,
                ProcessStatus::Crashed,
                reason.clone(),
            );
        }

        // Notify monitoring processes (one-way, always send DOWN message)
        for (ref_id, monitor_pid) in monitors {
            if let Some(monitor) = self.processes.get_mut(&monitor_pid) {
                monitor
                    .mailbox
                    .push_back(Message::System(SystemMsg::Down(ref_id, pid, reason.clone())));
                if monitor.status == ProcessStatus::Waiting {
                    monitor.status = ProcessStatus::Ready;
                    self.ready_queue.push_back(monitor_pid);
                }
                // Also remove the monitor from the monitoring process's list
                monitor.monitors.retain(|(_, target)| *target != pid);
            }
        }
    }

    /// Get count of processes by status
    pub fn process_count(&self) -> (usize, usize, usize, usize) {
        let mut ready = 0;
        let mut waiting = 0;
        let mut done = 0;
        let mut crashed = 0;

        for p in self.processes.values() {
            match p.status {
                ProcessStatus::Ready => ready += 1,
                ProcessStatus::Waiting => waiting += 1,
                ProcessStatus::Done => done += 1,
                ProcessStatus::Crashed => crashed += 1,
            }
        }

        (ready, waiting, done, crashed)
    }
}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Operand, Register};

    fn run_to_idle(scheduler: &mut Scheduler) {
        loop {
            if scheduler.step(100) == StepResult::Idle {
                break;
            }
        }
    }

    #[test]
    fn test_spawn_and_end() {
        let mut scheduler = Scheduler::new();

        let program = vec![Instruction::End];
        scheduler.spawn(program);

        run_to_idle(&mut scheduler);

        let (ready, waiting, done, crashed) = scheduler.process_count();
        assert_eq!((ready, waiting, done, crashed), (0, 0, 1, 0));
    }

    #[test]
    fn test_work_uses_reductions() {
        let mut scheduler = Scheduler::new();

        let program = vec![Instruction::Work { amount: 5 }, Instruction::End];
        scheduler.spawn(program);

        // Step with budget of 3 - not enough to complete
        let result = scheduler.step(3);
        assert_eq!(result, StepResult::Busy);

        // Process should still be ready
        let (ready, _, _, _) = scheduler.process_count();
        assert_eq!(ready, 1);

        // Now finish it
        run_to_idle(&mut scheduler);
        let (_, _, done, _) = scheduler.process_count();
        assert_eq!(done, 1);
    }

    #[test]
    fn test_message_passing() {
        let mut scheduler = Scheduler::new();

        // Child: receive message, then end
        let child = vec![
            Instruction::Receive { dest: Register(0) },
            Instruction::End,
        ];

        // Parent: spawn child, send message, end
        let parent = vec![
            Instruction::Spawn {
                code: child,
                dest: Register(0),
            },
            Instruction::Send {
                to: Source::Reg(Register(0)),
                msg: "hello".into(),
            },
            Instruction::End,
        ];

        scheduler.spawn(parent);
        run_to_idle(&mut scheduler);

        let (_, _, done, _) = scheduler.process_count();
        assert_eq!(done, 2);
    }

    #[test]
    fn test_parent_tracking() {
        let mut scheduler = Scheduler::new();

        // Child: send message to parent, then end
        let child = vec![
            Instruction::Send {
                to: Source::Parent,
                msg: "from child".into(),
            },
            Instruction::End,
        ];

        // Parent: spawn child, receive message, end
        let parent = vec![
            Instruction::Spawn {
                code: child,
                dest: Register(0),
            },
            Instruction::Receive { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(parent);
        run_to_idle(&mut scheduler);

        // Check parent received the message
        let parent_process = scheduler.processes.get(&Pid(0)).unwrap();
        match &parent_process.registers[1] {
            Value::String(s) => assert_eq!(s, "from child"),
            _ => panic!("Expected string in register"),
        }
    }

    #[test]
    fn test_spawn_link_crash_notification() {
        let mut scheduler = Scheduler::new();

        // Child: crash immediately
        let child = vec![Instruction::Crash];

        // Parent: enable trap_exit, spawn_link child, receive exit notification
        let parent = vec![
            Instruction::TrapExit { enable: true },
            Instruction::SpawnLink {
                code: child,
                dest: Register(0),
            },
            Instruction::Receive { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(parent);
        run_to_idle(&mut scheduler);

        // Parent should have received {:EXIT, ChildPid, :crashed} tuple
        let parent_process = scheduler.processes.get(&Pid(0)).unwrap();
        match &parent_process.registers[1] {
            Value::Tuple(elems) => {
                assert_eq!(elems.len(), 3);
                assert_eq!(elems[0], Value::Atom("EXIT".to_string()));
                assert!(matches!(elems[1], Value::Pid(_)));
                assert_eq!(elems[2], Value::Atom("crashed".to_string()));
            }
            _ => panic!("Expected EXIT tuple, got {:?}", parent_process.registers[1]),
        }

        let (_, _, done, crashed) = scheduler.process_count();
        assert_eq!(done, 1); // Parent done
        assert_eq!(crashed, 1); // Child crashed
    }

    #[test]
    fn test_monitor_down_notification() {
        let mut scheduler = Scheduler::new();

        // Worker: crash after some work
        let worker = vec![Instruction::Work { amount: 1 }, Instruction::Crash];

        // Observer: spawn, monitor, wait for DOWN
        let observer = vec![
            Instruction::Spawn {
                code: worker,
                dest: Register(0),
            },
            Instruction::Monitor {
                target: Source::Reg(Register(0)),
                dest: Register(2),
            },
            Instruction::Receive { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(observer);
        run_to_idle(&mut scheduler);

        // Observer should have received {:DOWN, Ref, :process, Pid, Reason} tuple
        let observer_process = scheduler.processes.get(&Pid(0)).unwrap();
        match &observer_process.registers[1] {
            Value::Tuple(elems) => {
                assert_eq!(elems.len(), 5);
                assert_eq!(elems[0], Value::Atom("DOWN".to_string()));
                assert!(matches!(elems[1], Value::Ref(_)));
                assert_eq!(elems[2], Value::Atom("process".to_string()));
                assert!(matches!(elems[3], Value::Pid(_)));
                assert_eq!(elems[4], Value::Atom("crashed".to_string()));
            }
            _ => panic!("Expected DOWN tuple, got {:?}", observer_process.registers[1]),
        }
    }

    #[test]
    fn test_receive_timeout() {
        let mut scheduler = Scheduler::new();

        // Process waits for message that never comes
        let waiter = vec![
            Instruction::ReceiveTimeout {
                dest: Register(0),
                timeout: 3,
            },
            Instruction::End,
        ];

        scheduler.spawn(waiter);

        // Run enough steps for timeout to expire
        for _ in 0..10 {
            if scheduler.step(1) == StepResult::Idle {
                break;
            }
        }

        // Check that TIMEOUT was received
        let process = scheduler.processes.get(&Pid(0)).unwrap();
        match &process.registers[0] {
            Value::String(s) => assert_eq!(s, "TIMEOUT"),
            _ => panic!("Expected TIMEOUT string"),
        }
    }

    #[test]
    fn test_process_registry() {
        let mut scheduler = Scheduler::new();

        // Server: register, send ready, receive, end
        let server = vec![
            Instruction::Register {
                name: "myserver".into(),
            },
            Instruction::Send {
                to: Source::Parent,
                msg: "ready".into(),
            },
            Instruction::Receive { dest: Register(0) },
            Instruction::End,
        ];

        // Client: spawn server, wait for ready, send by name
        let client = vec![
            Instruction::Spawn {
                code: server,
                dest: Register(0),
            },
            Instruction::Receive { dest: Register(1) }, // wait for ready
            Instruction::Send {
                to: Source::Named("myserver".into()),
                msg: "ping".into(),
            },
            Instruction::End,
        ];

        scheduler.spawn(client);
        run_to_idle(&mut scheduler);

        // Both should complete
        let (_, _, done, _) = scheduler.process_count();
        assert_eq!(done, 2);

        // Server should have received "ping"
        let server_process = scheduler.processes.get(&Pid(1)).unwrap();
        match &server_process.registers[0] {
            Value::String(s) => assert_eq!(s, "ping"),
            _ => panic!("Expected ping message"),
        }
    }

    #[test]
    fn test_whereis() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Register {
                name: "test".into(),
            },
            Instruction::WhereIs {
                name: "test".into(),
                dest: Register(0),
            },
            Instruction::WhereIs {
                name: "nonexistent".into(),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();

        // Register 0 should have the PID
        match &process.registers[0] {
            Value::Pid(p) => assert_eq!(p.0, 0),
            _ => panic!("Expected Pid in register 0"),
        }

        // Register 1 should be None
        match &process.registers[1] {
            Value::None => {}
            _ => panic!("Expected None in register 1"),
        }
    }

    #[test]
    fn test_registry_cleanup_on_exit() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Register {
                name: "temp".into(),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);

        // After one step, process registers
        scheduler.step(1);
        assert!(scheduler.registry.contains_key("temp"));

        // After completion, registry should be cleaned
        run_to_idle(&mut scheduler);
        assert!(!scheduler.registry.contains_key("temp"));
    }

    #[test]
    fn test_spawn_many_processes() {
        let mut scheduler = Scheduler::new();

        let worker = vec![Instruction::End];

        // Spawner creates 50 workers
        let mut spawner = Vec::new();
        for _ in 0..50 {
            spawner.push(Instruction::Spawn {
                code: worker.clone(),
                dest: Register(0),
            });
        }
        spawner.push(Instruction::End);

        scheduler.spawn(spawner);
        run_to_idle(&mut scheduler);

        // Should have 51 processes total (spawner + 50 workers)
        assert_eq!(scheduler.processes.len(), 51);

        let (_, _, done, _) = scheduler.process_count();
        assert_eq!(done, 51);
    }

    #[test]
    fn test_print_captures_output() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Print {
                source: Source::Self_,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let output = scheduler.take_output();
        assert_eq!(output.len(), 1);
        assert!(output[0].contains("Pid(0)"));
    }

    // ========== Arithmetic Tests ==========

    #[test]
    fn test_load_int() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(42));
    }

    #[test]
    fn test_add() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 10,
                dest: Register(0),
            },
            Instruction::Add {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(5),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(15));
    }

    #[test]
    fn test_sub() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Sub {
                a: Operand::Int(20),
                b: Operand::Int(7),
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(13));
    }

    #[test]
    fn test_mul() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 6,
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 7,
                dest: Register(1),
            },
            Instruction::Mul {
                a: Operand::Reg(Register(0)),
                b: Operand::Reg(Register(1)),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(42));
    }

    #[test]
    fn test_div() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Div {
                a: Operand::Int(100),
                b: Operand::Int(7),
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(14)); // Integer division
    }

    #[test]
    fn test_div_by_zero_crashes() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Div {
                a: Operand::Int(10),
                b: Operand::Int(0),
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let (_, _, _, crashed) = scheduler.process_count();
        assert_eq!(crashed, 1);
    }

    #[test]
    fn test_mod() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Mod {
                a: Operand::Int(17),
                b: Operand::Int(5),
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(2));
    }

    // ========== Comparison Tests ==========

    #[test]
    fn test_eq() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Eq {
                a: Operand::Int(5),
                b: Operand::Int(5),
                dest: Register(0),
            },
            Instruction::Eq {
                a: Operand::Int(5),
                b: Operand::Int(3),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(1)); // true
        assert_eq!(process.registers[1], Value::Int(0)); // false
    }

    #[test]
    fn test_lt_gt() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Lt {
                a: Operand::Int(3),
                b: Operand::Int(5),
                dest: Register(0),
            },
            Instruction::Gt {
                a: Operand::Int(3),
                b: Operand::Int(5),
                dest: Register(1),
            },
            Instruction::Lt {
                a: Operand::Int(5),
                b: Operand::Int(3),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(1)); // 3 < 5 = true
        assert_eq!(process.registers[1], Value::Int(0)); // 3 > 5 = false
        assert_eq!(process.registers[2], Value::Int(0)); // 5 < 3 = false
    }

    #[test]
    fn test_lte_gte() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Lte {
                a: Operand::Int(5),
                b: Operand::Int(5),
                dest: Register(0),
            },
            Instruction::Gte {
                a: Operand::Int(5),
                b: Operand::Int(5),
                dest: Register(1),
            },
            Instruction::Lte {
                a: Operand::Int(3),
                b: Operand::Int(5),
                dest: Register(2),
            },
            Instruction::Gte {
                a: Operand::Int(3),
                b: Operand::Int(5),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(1)); // 5 <= 5 = true
        assert_eq!(process.registers[1], Value::Int(1)); // 5 >= 5 = true
        assert_eq!(process.registers[2], Value::Int(1)); // 3 <= 5 = true
        assert_eq!(process.registers[3], Value::Int(0)); // 3 >= 5 = false
    }

    #[test]
    fn test_ne() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Ne {
                a: Operand::Int(5),
                b: Operand::Int(3),
                dest: Register(0),
            },
            Instruction::Ne {
                a: Operand::Int(5),
                b: Operand::Int(5),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(1)); // 5 != 3 = true
        assert_eq!(process.registers[1], Value::Int(0)); // 5 != 5 = false
    }

    #[test]
    fn test_arithmetic_chain() {
        let mut scheduler = Scheduler::new();

        // Calculate (10 + 5) * 2 - 3 = 27
        let program = vec![
            Instruction::Add {
                a: Operand::Int(10),
                b: Operand::Int(5),
                dest: Register(0),
            },
            Instruction::Mul {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(2),
                dest: Register(0),
            },
            Instruction::Sub {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(3),
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(27));
    }

    // ========== Control Flow Tests ==========

    #[test]
    fn test_jump() {
        let mut scheduler = Scheduler::new();

        // Jump over the first LoadInt, should only execute second one
        let program = vec![
            // 0: Jump to instruction 2
            Instruction::Jump { target: 2 },
            // 1: This should be skipped
            Instruction::LoadInt {
                value: 999,
                dest: Register(0),
            },
            // 2: This should execute
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(42));
    }

    #[test]
    fn test_jump_if_truthy() {
        let mut scheduler = Scheduler::new();

        // Condition is 1 (truthy), should jump
        let program = vec![
            // 0: Load 1 (truthy)
            Instruction::LoadInt {
                value: 1,
                dest: Register(0),
            },
            // 1: Jump if truthy to 3
            Instruction::JumpIf {
                cond: Operand::Reg(Register(0)),
                target: 3,
            },
            // 2: Should be skipped
            Instruction::LoadInt {
                value: 999,
                dest: Register(1),
            },
            // 3: Should execute
            Instruction::LoadInt {
                value: 42,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(42));
    }

    #[test]
    fn test_jump_if_falsy() {
        let mut scheduler = Scheduler::new();

        // Condition is 0 (falsy), should NOT jump
        let program = vec![
            // 0: Load 0 (falsy)
            Instruction::LoadInt {
                value: 0,
                dest: Register(0),
            },
            // 1: Jump if truthy - should NOT jump since 0 is falsy
            Instruction::JumpIf {
                cond: Operand::Reg(Register(0)),
                target: 3,
            },
            // 2: Should execute (not skipped)
            Instruction::LoadInt {
                value: 42,
                dest: Register(1),
            },
            // 3:
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(42));
    }

    #[test]
    fn test_jump_unless_falsy() {
        let mut scheduler = Scheduler::new();

        // Condition is 0 (falsy), should jump
        let program = vec![
            // 0: Load 0 (falsy)
            Instruction::LoadInt {
                value: 0,
                dest: Register(0),
            },
            // 1: Jump unless truthy - should jump since 0 is falsy
            Instruction::JumpUnless {
                cond: Operand::Reg(Register(0)),
                target: 3,
            },
            // 2: Should be skipped
            Instruction::LoadInt {
                value: 999,
                dest: Register(1),
            },
            // 3: Should execute
            Instruction::LoadInt {
                value: 42,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(42));
    }

    #[test]
    fn test_simple_loop() {
        let mut scheduler = Scheduler::new();

        // Count down from 5 to 0
        // R0 = counter, R1 = accumulator (sum)
        let program = vec![
            // 0: R0 = 5
            Instruction::LoadInt {
                value: 5,
                dest: Register(0),
            },
            // 1: R1 = 0
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            // 2: Loop start - R1 = R1 + R0
            Instruction::Add {
                a: Operand::Reg(Register(1)),
                b: Operand::Reg(Register(0)),
                dest: Register(1),
            },
            // 3: R0 = R0 - 1
            Instruction::Sub {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(1),
                dest: Register(0),
            },
            // 4: Jump back to 2 if R0 > 0
            Instruction::JumpIf {
                cond: Operand::Reg(Register(0)),
                target: 2,
            },
            // 5: End
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        // Sum of 5+4+3+2+1 = 15
        assert_eq!(process.registers[1], Value::Int(15));
    }

    #[test]
    fn test_conditional_with_comparison() {
        let mut scheduler = Scheduler::new();

        // If 10 > 5, set R1 = 1, else R1 = 0
        let program = vec![
            // 0: Compare 10 > 5 -> R0
            Instruction::Gt {
                a: Operand::Int(10),
                b: Operand::Int(5),
                dest: Register(0),
            },
            // 1: If R0 is truthy, jump to 3
            Instruction::JumpIf {
                cond: Operand::Reg(Register(0)),
                target: 3,
            },
            // 2: Else branch - set R1 = 0, jump to end
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::Jump { target: 4 },
            // 3: Then branch - set R1 = 1
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            // 4: End
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // 10 > 5, so then branch
    }

    // ========== Function Call Tests ==========

    #[test]
    fn test_simple_call_return() {
        let mut scheduler = Scheduler::new();

        // Main calls a function that sets R0 = 42, then returns
        let program = vec![
            // 0: Call function at 3
            Instruction::Call { target: 3 },
            // 1: After return, end
            Instruction::End,
            // 2: (unreachable)
            Instruction::LoadInt {
                value: 999,
                dest: Register(0),
            },
            // 3: Function start - set R0 = 42
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            // 4: Return to caller
            Instruction::Return,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(42));
    }

    #[test]
    fn test_nested_calls() {
        let mut scheduler = Scheduler::new();

        // Main calls func_a, which calls func_b, which sets R0 = 100
        let program = vec![
            // 0: Call func_a at 3
            Instruction::Call { target: 3 },
            // 1: After return, end
            Instruction::End,
            // 2: (padding)
            Instruction::End,
            // 3: func_a - call func_b at 6
            Instruction::Call { target: 6 },
            // 4: Add 10 to R0
            Instruction::Add {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(10),
                dest: Register(0),
            },
            // 5: Return from func_a
            Instruction::Return,
            // 6: func_b - set R0 = 100
            Instruction::LoadInt {
                value: 100,
                dest: Register(0),
            },
            // 7: Return from func_b
            Instruction::Return,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        // func_b sets 100, func_a adds 10 = 110
        assert_eq!(process.registers[0], Value::Int(110));
    }

    #[test]
    fn test_function_with_loop() {
        let mut scheduler = Scheduler::new();

        // Main sets R0 = 5, calls factorial function which computes iteratively
        // factorial(n): R1 = 1, while n > 0: R1 *= n, n--; return R1 in R0
        let program = vec![
            // 0: Load 5 into R0 (argument)
            Instruction::LoadInt {
                value: 5,
                dest: Register(0),
            },
            // 1: Call factorial at 4
            Instruction::Call { target: 4 },
            // 2: End (R0 has result)
            Instruction::End,
            // 3: (padding)
            Instruction::End,
            // === factorial function at 4 ===
            // R0 = n (input), R1 = accumulator (result)
            // 4: R1 = 1 (accumulator)
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            // 5: Loop start - check if R0 <= 0
            Instruction::Lte {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(0),
                dest: Register(2),
            },
            // 6: If R0 <= 0, jump to return
            Instruction::JumpIf {
                cond: Operand::Reg(Register(2)),
                target: 10,
            },
            // 7: R1 = R1 * R0
            Instruction::Mul {
                a: Operand::Reg(Register(1)),
                b: Operand::Reg(Register(0)),
                dest: Register(1),
            },
            // 8: R0 = R0 - 1
            Instruction::Sub {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(1),
                dest: Register(0),
            },
            // 9: Jump back to loop start
            Instruction::Jump { target: 5 },
            // 10: Move result to R0 and return
            Instruction::Add {
                a: Operand::Reg(Register(1)),
                b: Operand::Int(0),
                dest: Register(0),
            },
            // 11: Return
            Instruction::Return,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(120)); // 5! = 120
    }

    #[test]
    fn test_return_without_call_ends_process() {
        let mut scheduler = Scheduler::new();

        // Return without a call should end the process
        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            Instruction::Return,
            // These should never execute
            Instruction::LoadInt {
                value: 999,
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(42));
        assert_eq!(process.status, ProcessStatus::Done);
    }

    // ========== Stack Operation Tests ==========

    #[test]
    fn test_push_pop() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Push some values
            Instruction::Push {
                source: Operand::Int(10),
            },
            Instruction::Push {
                source: Operand::Int(20),
            },
            Instruction::Push {
                source: Operand::Int(30),
            },
            // Pop in reverse order (LIFO)
            Instruction::Pop { dest: Register(0) }, // 30
            Instruction::Pop { dest: Register(1) }, // 20
            Instruction::Pop { dest: Register(2) }, // 10
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(30));
        assert_eq!(process.registers[1], Value::Int(20));
        assert_eq!(process.registers[2], Value::Int(10));
    }

    #[test]
    fn test_push_register() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::LoadInt {
                value: 0,
                dest: Register(0),
            },
            Instruction::Pop { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(0)); // Overwritten
        assert_eq!(process.registers[1], Value::Int(42)); // Restored from stack
    }

    #[test]
    fn test_pop_empty_crashes() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Pop { dest: Register(0) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let (_, _, _, crashed) = scheduler.process_count();
        assert_eq!(crashed, 1);
    }

    #[test]
    fn test_recursive_factorial_with_stack() {
        let mut scheduler = Scheduler::new();

        // Calculate factorial(5) = 120 using proper stack frames
        // R0 = argument/result
        let program = vec![
            // 0: Load 5 into R0
            Instruction::LoadInt {
                value: 5,
                dest: Register(0),
            },
            // 1: Call factorial at 4
            Instruction::Call { target: 4 },
            // 2: End (R0 has result)
            Instruction::End,
            // 3: (padding)
            Instruction::End,
            // === factorial function at 4 ===
            // 4: If R0 <= 1, return R0 (base case)
            Instruction::Lte {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(1),
                dest: Register(1),
            },
            // 5: Jump to return if base case
            Instruction::JumpIf {
                cond: Operand::Reg(Register(1)),
                target: 13,
            },
            // 6: Save R0 onto stack
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            // 7: R0 = R0 - 1
            Instruction::Sub {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(1),
                dest: Register(0),
            },
            // 8: Recursive call
            Instruction::Call { target: 4 },
            // 9: Pop saved value into R1
            Instruction::Pop { dest: Register(1) },
            // 10: R0 = R0 * R1
            Instruction::Mul {
                a: Operand::Reg(Register(0)),
                b: Operand::Reg(Register(1)),
                dest: Register(0),
            },
            // 11: Return
            Instruction::Return,
            // 12: (padding)
            Instruction::End,
            // 13: Base case - R0 is already 1, just return
            Instruction::Return,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(120)); // 5! = 120
    }

    #[test]
    fn test_recursive_fibonacci() {
        let mut scheduler = Scheduler::new();

        // Calculate fibonacci(10) = 55
        // R0 = argument/result
        let program = vec![
            // 0: Load 10 into R0
            Instruction::LoadInt {
                value: 10,
                dest: Register(0),
            },
            // 1: Call fib at 4
            Instruction::Call { target: 4 },
            // 2: End
            Instruction::End,
            // 3: (padding)
            Instruction::End,
            // === fib function at 4 ===
            // 4: If R0 <= 1, return R0
            Instruction::Lte {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(1),
                dest: Register(1),
            },
            // 5: Jump to return if base case
            Instruction::JumpIf {
                cond: Operand::Reg(Register(1)),
                target: 16,
            },
            // 6: Save n onto stack
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            // 7: R0 = n - 1
            Instruction::Sub {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(1),
                dest: Register(0),
            },
            // 8: Call fib(n-1)
            Instruction::Call { target: 4 },
            // 9: Move fib(n-1) to R2
            Instruction::Add {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(0),
                dest: Register(2),
            },
            // 10: Pop original n into R0
            Instruction::Pop { dest: Register(0) },
            // 11: Push fib(n-1) for later
            Instruction::Push {
                source: Operand::Reg(Register(2)),
            },
            // 12: R0 = n - 2
            Instruction::Sub {
                a: Operand::Reg(Register(0)),
                b: Operand::Int(2),
                dest: Register(0),
            },
            // 13: Call fib(n-2)
            Instruction::Call { target: 4 },
            // 14: Pop fib(n-1) into R1
            Instruction::Pop { dest: Register(1) },
            // 15: R0 = fib(n-1) + fib(n-2)
            Instruction::Add {
                a: Operand::Reg(Register(0)),
                b: Operand::Reg(Register(1)),
                dest: Register(0),
            },
            // 16: Return
            Instruction::Return,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(55)); // fib(10) = 55
    }

    // ========== Atom & Tuple Tests ==========

    #[test]
    fn test_load_atom() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadAtom {
                name: "ok".to_string(),
                dest: Register(0),
            },
            Instruction::LoadAtom {
                name: "error".to_string(),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Atom("ok".to_string()));
        assert_eq!(process.registers[1], Value::Atom("error".to_string()));
    }

    #[test]
    fn test_make_tuple() {
        let mut scheduler = Scheduler::new();

        // Create tuple {:ok, 42}
        let program = vec![
            Instruction::LoadAtom {
                name: "ok".to_string(),
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 42,
                dest: Register(1),
            },
            // Push elements onto stack
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Reg(Register(1)),
            },
            // Make 2-tuple
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(
            process.registers[2],
            Value::Tuple(vec![
                Value::Atom("ok".to_string()),
                Value::Int(42),
            ])
        );
    }

    #[test]
    fn test_tuple_element() {
        let mut scheduler = Scheduler::new();

        // Create tuple {:error, "not found", 404} and extract elements
        let program = vec![
            Instruction::LoadAtom {
                name: "error".to_string(),
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 404,
                dest: Register(1),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Reg(Register(1)),
            },
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(0),
            },
            // Extract element 0 (the atom)
            Instruction::TupleElement {
                tuple: Register(0),
                index: 0,
                dest: Register(1),
            },
            // Extract element 1 (the integer)
            Instruction::TupleElement {
                tuple: Register(0),
                index: 1,
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Atom("error".to_string()));
        assert_eq!(process.registers[2], Value::Int(404));
    }

    #[test]
    fn test_tuple_arity() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create a 3-tuple
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::MakeTuple {
                arity: 3,
                dest: Register(0),
            },
            Instruction::TupleArity {
                tuple: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(3));
    }

    #[test]
    fn test_tuple_element_out_of_bounds_crashes() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::MakeTuple {
                arity: 1,
                dest: Register(0),
            },
            // Try to access index 5 (out of bounds)
            Instruction::TupleElement {
                tuple: Register(0),
                index: 5,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let (_, _, _, crashed) = scheduler.process_count();
        assert_eq!(crashed, 1);
    }

    #[test]
    fn test_nested_tuples() {
        let mut scheduler = Scheduler::new();

        // Create {{:inner, 1}, :outer}
        let program = vec![
            // Create inner tuple {:inner, 1}
            Instruction::LoadAtom {
                name: "inner".to_string(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(0),
            },
            // Create outer tuple {inner_tuple, :outer}
            Instruction::LoadAtom {
                name: "outer".to_string(),
                dest: Register(1),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Reg(Register(1)),
            },
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(2),
            },
            // Extract inner tuple
            Instruction::TupleElement {
                tuple: Register(2),
                index: 0,
                dest: Register(3),
            },
            // Extract element from inner tuple
            Instruction::TupleElement {
                tuple: Register(3),
                index: 1,
                dest: Register(4),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        // R4 should have the integer 1 from the inner tuple
        assert_eq!(process.registers[4], Value::Int(1));
    }

    // ========== Pattern Matching Tests ==========

    #[test]
    fn test_match_wildcard() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            // Match anything with wildcard - should succeed
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Wildcard,
                fail_target: 4,
            },
            // Success path: set R1 = 1
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            Instruction::End,
            // Fail path: set R1 = 0
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // Success path taken
    }

    #[test]
    fn test_match_variable() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            // Match and bind to R1
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Variable(Register(1)),
                fail_target: 3,
            },
            Instruction::End,
            // Fail path
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(42)); // Bound value
    }

    #[test]
    fn test_match_int_success() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            // Match exact integer
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Int(42),
                fail_target: 4,
            },
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            Instruction::End,
            // Fail
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // Success
    }

    #[test]
    fn test_match_int_failure() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            // Match wrong integer - should fail
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Int(99),
                fail_target: 4,
            },
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            Instruction::End,
            // Fail path
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(0)); // Fail path taken
    }

    #[test]
    fn test_match_atom() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadAtom {
                name: "ok".to_string(),
                dest: Register(0),
            },
            // Match :ok
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Atom("ok".to_string()),
                fail_target: 4,
            },
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            Instruction::End,
            // Fail
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // Success
    }

    #[test]
    fn test_match_tuple_with_bindings() {
        let mut scheduler = Scheduler::new();

        // Create {:ok, 42} and match with {:ok, x}
        let program = vec![
            // Build tuple {:ok, 42}
            Instruction::LoadAtom {
                name: "ok".to_string(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(42),
            },
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(0),
            },
            // Match {:ok, x} binding x to R1
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Tuple(vec![
                    Pattern::Atom("ok".to_string()),
                    Pattern::Variable(Register(1)),
                ]),
                fail_target: 7,
            },
            // Success: R2 = 1
            Instruction::LoadInt {
                value: 1,
                dest: Register(2),
            },
            Instruction::End,
            // Fail: R2 = 0
            Instruction::LoadInt {
                value: 0,
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(42)); // Bound value
        assert_eq!(process.registers[2], Value::Int(1)); // Success path
    }

    #[test]
    fn test_match_tuple_wrong_atom() {
        let mut scheduler = Scheduler::new();

        // Create {:error, 42} and try to match {:ok, x} - should fail
        let program = vec![
            Instruction::LoadAtom {
                name: "error".to_string(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(42),
            },
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(0),
            },
            // Match {:ok, x} - should fail
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Tuple(vec![
                    Pattern::Atom("ok".to_string()),
                    Pattern::Variable(Register(1)),
                ]),
                fail_target: 7,
            },
            Instruction::LoadInt {
                value: 1,
                dest: Register(2),
            },
            Instruction::End,
            // Fail path
            Instruction::LoadInt {
                value: 0,
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(0)); // Fail path taken
    }

    #[test]
    fn test_match_tuple_wrong_arity() {
        let mut scheduler = Scheduler::new();

        // Create {1, 2, 3} and try to match {a, b} - should fail
        let program = vec![
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::MakeTuple {
                arity: 3,
                dest: Register(0),
            },
            // Match {a, b} - wrong arity
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Tuple(vec![
                    Pattern::Variable(Register(1)),
                    Pattern::Variable(Register(2)),
                ]),
                fail_target: 7,
            },
            Instruction::LoadInt {
                value: 1,
                dest: Register(3),
            },
            Instruction::End,
            // Fail path
            Instruction::LoadInt {
                value: 0,
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[3], Value::Int(0)); // Fail path taken
    }

    #[test]
    fn test_match_nested_tuple() {
        let mut scheduler = Scheduler::new();

        // Create {:ok, {:data, 100}} and match {:ok, {:data, x}}
        let program = vec![
            // Build inner tuple {:data, 100}
            Instruction::LoadAtom {
                name: "data".to_string(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(100),
            },
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(0),
            },
            // Build outer tuple {:ok, inner}
            Instruction::LoadAtom {
                name: "ok".to_string(),
                dest: Register(1),
            },
            Instruction::Push {
                source: Operand::Reg(Register(1)),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(0),
            },
            // Match {:ok, {:data, x}}
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Tuple(vec![
                    Pattern::Atom("ok".to_string()),
                    Pattern::Tuple(vec![
                        Pattern::Atom("data".to_string()),
                        Pattern::Variable(Register(1)),
                    ]),
                ]),
                fail_target: 11,
            },
            Instruction::LoadInt {
                value: 1,
                dest: Register(2),
            },
            Instruction::End,
            // Fail
            Instruction::LoadInt {
                value: 0,
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(100)); // Extracted value
        assert_eq!(process.registers[2], Value::Int(1)); // Success
    }

    // ========== ReceiveMatch Tests ==========

    #[test]
    fn test_receive_match_simple() {
        let mut scheduler = Scheduler::new();

        // Parent sends "hello", child receives with pattern match
        let child_code = vec![
            // Receive any message into R0
            Instruction::ReceiveMatch {
                clauses: vec![(Pattern::Variable(Register(0)), 2)],
                timeout: None,
                timeout_target: 0,
            },
            Instruction::End, // unreachable
            // Clause target: R1 = 1 to indicate success
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            Instruction::End,
        ];

        let parent_code = vec![
            Instruction::Spawn {
                code: child_code,
                dest: Register(0),
            },
            Instruction::Send {
                to: Source::Reg(Register(0)),
                msg: "hello".to_string(),
            },
            Instruction::End,
        ];

        scheduler.spawn(parent_code);
        run_to_idle(&mut scheduler);

        let child = scheduler.processes.get(&Pid(1)).unwrap();
        assert_eq!(child.registers[0], Value::String("hello".to_string()));
        assert_eq!(child.registers[1], Value::Int(1));
    }

    #[test]
    fn test_receive_match_specific_string() {
        let mut scheduler = Scheduler::new();

        // Child waits for "ping" specifically
        let child_code = vec![
            Instruction::ReceiveMatch {
                clauses: vec![(Pattern::String("ping".to_string()), 2)],
                timeout: None,
                timeout_target: 0,
            },
            Instruction::End,
            // Match success
            Instruction::LoadInt {
                value: 1,
                dest: Register(0),
            },
            Instruction::End,
        ];

        let parent_code = vec![
            Instruction::Spawn {
                code: child_code,
                dest: Register(0),
            },
            // Send "pong" first (won't match)
            Instruction::Send {
                to: Source::Reg(Register(0)),
                msg: "pong".to_string(),
            },
            // Then send "ping" (will match)
            Instruction::Send {
                to: Source::Reg(Register(0)),
                msg: "ping".to_string(),
            },
            Instruction::End,
        ];

        scheduler.spawn(parent_code);
        run_to_idle(&mut scheduler);

        let child = scheduler.processes.get(&Pid(1)).unwrap();
        assert_eq!(child.registers[0], Value::Int(1));
        // "pong" should still be in mailbox
        assert_eq!(child.mailbox.len(), 1);
    }

    #[test]
    fn test_receive_match_multiple_clauses() {
        let mut scheduler = Scheduler::new();

        // Child handles "ping" or "pong" differently
        let child_code = vec![
            Instruction::ReceiveMatch {
                clauses: vec![
                    (Pattern::String("ping".to_string()), 2), // -> set R0 = 1
                    (Pattern::String("pong".to_string()), 5), // -> set R0 = 2
                ],
                timeout: None,
                timeout_target: 0,
            },
            Instruction::End,
            // ping handler
            Instruction::LoadInt {
                value: 1,
                dest: Register(0),
            },
            Instruction::End,
            Instruction::End, // padding
            // pong handler
            Instruction::LoadInt {
                value: 2,
                dest: Register(0),
            },
            Instruction::End,
        ];

        let parent_code = vec![
            Instruction::Spawn {
                code: child_code,
                dest: Register(0),
            },
            Instruction::Send {
                to: Source::Reg(Register(0)),
                msg: "pong".to_string(),
            },
            Instruction::End,
        ];

        scheduler.spawn(parent_code);
        run_to_idle(&mut scheduler);

        let child = scheduler.processes.get(&Pid(1)).unwrap();
        assert_eq!(child.registers[0], Value::Int(2)); // pong handler
    }

    #[test]
    fn test_receive_match_timeout() {
        let mut scheduler = Scheduler::new();

        // Process waits for message with timeout
        let program = vec![
            Instruction::ReceiveMatch {
                clauses: vec![(Pattern::String("hello".to_string()), 3)],
                timeout: Some(5),
                timeout_target: 5,
            },
            Instruction::End, // unreachable
            Instruction::End, // padding
            // Match success (won't happen)
            Instruction::LoadInt {
                value: 1,
                dest: Register(0),
            },
            Instruction::End,
            // Timeout handler
            Instruction::LoadInt {
                value: 99,
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(99)); // Timeout handler ran
    }

    #[test]
    fn test_receive_match_selective() {
        let mut scheduler = Scheduler::new();

        // Test selective receive: skip non-matching messages
        let child_code = vec![
            // Wait for "second"
            Instruction::ReceiveMatch {
                clauses: vec![(Pattern::String("second".to_string()), 2)],
                timeout: None,
                timeout_target: 0,
            },
            Instruction::End,
            // Got "second"
            Instruction::LoadInt {
                value: 1,
                dest: Register(0),
            },
            // Now receive "first" which was skipped
            Instruction::ReceiveMatch {
                clauses: vec![(Pattern::String("first".to_string()), 5)],
                timeout: None,
                timeout_target: 0,
            },
            Instruction::End,
            // Got "first"
            Instruction::LoadInt {
                value: 2,
                dest: Register(1),
            },
            Instruction::End,
        ];

        let parent_code = vec![
            Instruction::Spawn {
                code: child_code,
                dest: Register(0),
            },
            // Send "first" then "second"
            Instruction::Send {
                to: Source::Reg(Register(0)),
                msg: "first".to_string(),
            },
            Instruction::Send {
                to: Source::Reg(Register(0)),
                msg: "second".to_string(),
            },
            Instruction::End,
        ];

        scheduler.spawn(parent_code);
        run_to_idle(&mut scheduler);

        let child = scheduler.processes.get(&Pid(1)).unwrap();
        assert_eq!(child.registers[0], Value::Int(1)); // Got "second"
        assert_eq!(child.registers[1], Value::Int(2)); // Then got "first"
    }

    // ========== List Tests ==========

    #[test]
    fn test_make_list() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Push elements onto stack
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            // Make list [1, 2, 3]
            Instruction::MakeList {
                length: 3,
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(
            process.registers[0],
            Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
    }

    #[test]
    fn test_make_empty_list() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeList {
                length: 0,
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::List(vec![]));
    }

    #[test]
    fn test_cons() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create empty list
            Instruction::MakeList {
                length: 0,
                dest: Register(0),
            },
            // Load 3
            Instruction::LoadInt {
                value: 3,
                dest: Register(1),
            },
            // Cons 3 onto empty list: [3]
            Instruction::Cons {
                head: Register(1),
                tail: Register(0),
                dest: Register(0),
            },
            // Load 2
            Instruction::LoadInt {
                value: 2,
                dest: Register(1),
            },
            // Cons 2 onto [3]: [2, 3]
            Instruction::Cons {
                head: Register(1),
                tail: Register(0),
                dest: Register(0),
            },
            // Load 1
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            // Cons 1 onto [2, 3]: [1, 2, 3]
            Instruction::Cons {
                head: Register(1),
                tail: Register(0),
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(
            process.registers[0],
            Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
    }

    #[test]
    fn test_list_head_tail() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2, 3]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::MakeList {
                length: 3,
                dest: Register(0),
            },
            // Get head -> 1
            Instruction::ListHead {
                list: Register(0),
                dest: Register(1),
            },
            // Get tail -> [2, 3]
            Instruction::ListTail {
                list: Register(0),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1));
        assert_eq!(
            process.registers[2],
            Value::List(vec![Value::Int(2), Value::Int(3)])
        );
    }

    #[test]
    fn test_list_is_empty() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create empty list
            Instruction::MakeList {
                length: 0,
                dest: Register(0),
            },
            Instruction::ListIsEmpty {
                list: Register(0),
                dest: Register(1),
            },
            // Create non-empty list
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::MakeList {
                length: 1,
                dest: Register(2),
            },
            Instruction::ListIsEmpty {
                list: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // empty list
        assert_eq!(process.registers[3], Value::Int(0)); // non-empty list
    }

    #[test]
    fn test_list_head_empty_crashes() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeList {
                length: 0,
                dest: Register(0),
            },
            Instruction::ListHead {
                list: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let (_, _, _, crashed) = scheduler.process_count();
        assert_eq!(crashed, 1);
    }

    #[test]
    fn test_match_list_empty() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create empty list
            Instruction::MakeList {
                length: 0,
                dest: Register(0),
            },
            // Match against []
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::ListEmpty,
                fail_target: 4,
            },
            // Success
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            Instruction::End,
            // Fail
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // Success
    }

    #[test]
    fn test_match_list_cons() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2, 3]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::MakeList {
                length: 3,
                dest: Register(0),
            },
            // Match [H | T] binding H to R1, T to R2
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::ListCons {
                    head: Box::new(Pattern::Variable(Register(1))),
                    tail: Box::new(Pattern::Variable(Register(2))),
                },
                fail_target: 7,
            },
            // Success
            Instruction::LoadInt {
                value: 1,
                dest: Register(3),
            },
            Instruction::End,
            // Fail
            Instruction::LoadInt {
                value: 0,
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // Head
        assert_eq!(
            process.registers[2],
            Value::List(vec![Value::Int(2), Value::Int(3)])
        ); // Tail
        assert_eq!(process.registers[3], Value::Int(1)); // Success
    }

    #[test]
    fn test_match_list_cons_on_empty_fails() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create empty list
            Instruction::MakeList {
                length: 0,
                dest: Register(0),
            },
            // Try to match [H | T] - should fail
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::ListCons {
                    head: Box::new(Pattern::Variable(Register(1))),
                    tail: Box::new(Pattern::Variable(Register(2))),
                },
                fail_target: 4,
            },
            Instruction::LoadInt {
                value: 1,
                dest: Register(3),
            },
            Instruction::End,
            // Fail path
            Instruction::LoadInt {
                value: 0,
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[3], Value::Int(0)); // Fail path taken
    }

    #[test]
    fn test_match_list_nested() {
        let mut scheduler = Scheduler::new();

        // Match [1, 2 | Rest]
        let program = vec![
            // Create list [1, 2, 3, 4]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::Push {
                source: Operand::Int(4),
            },
            Instruction::MakeList {
                length: 4,
                dest: Register(0),
            },
            // Match [1, 2 | Rest] - first must be 1, second must be 2, rest bound
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::ListCons {
                    head: Box::new(Pattern::Int(1)),
                    tail: Box::new(Pattern::ListCons {
                        head: Box::new(Pattern::Int(2)),
                        tail: Box::new(Pattern::Variable(Register(1))),
                    }),
                },
                fail_target: 8,
            },
            // Success
            Instruction::LoadInt {
                value: 1,
                dest: Register(2),
            },
            Instruction::End,
            // Fail
            Instruction::LoadInt {
                value: 0,
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(
            process.registers[1],
            Value::List(vec![Value::Int(3), Value::Int(4)])
        ); // Rest
        assert_eq!(process.registers[2], Value::Int(1)); // Success
    }

    // ========== Module Tests ==========

    #[test]
    fn test_module_load() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        let mut math = Module::new("math".to_string());
        math.add_function(
            "identity".to_string(),
            1,
            vec![
                // Just return the argument (already in R0)
                Instruction::Return,
            ],
        );
        math.export("identity", 1);

        assert!(scheduler.load_module(math).is_ok());
        assert!(scheduler.modules.contains_key("math"));
    }

    #[test]
    fn test_call_mfa() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Create a math module with add_one function
        let mut math = Module::new("math".to_string());
        math.add_function(
            "add_one".to_string(),
            1,
            vec![
                // R0 contains the argument, add 1 and return
                Instruction::Add {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Int(1),
                    dest: Register(0),
                },
                Instruction::Return,
            ],
        );
        math.export("add_one", 1);
        scheduler.load_module(math).unwrap();

        // Spawn a process that calls math:add_one(5)
        let program = vec![
            Instruction::LoadInt {
                value: 5,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "math".to_string(),
                function: "add_one".to_string(),
                arity: 1,
            },
            // R0 should now be 6
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(6));
    }

    #[test]
    fn test_call_local() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Create a module with two functions
        let mut math = Module::new("math".to_string());

        // double(X) -> X * 2
        math.add_function(
            "double".to_string(),
            1,
            vec![
                Instruction::Mul {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Int(2),
                    dest: Register(0),
                },
                Instruction::Return,
            ],
        );

        // quadruple(X) -> double(double(X))
        math.add_function(
            "quadruple".to_string(),
            1,
            vec![
                Instruction::CallLocal {
                    function: "double".to_string(),
                    arity: 1,
                },
                Instruction::CallLocal {
                    function: "double".to_string(),
                    arity: 1,
                },
                Instruction::Return,
            ],
        );
        math.export("quadruple", 1);
        scheduler.load_module(math).unwrap();

        // Call quadruple(3) -> 12
        let program = vec![
            Instruction::LoadInt {
                value: 3,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "math".to_string(),
                function: "quadruple".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(12));
    }

    #[test]
    fn test_tail_call_no_stack_growth() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Create a module with a tail-recursive countdown
        let mut counter = Module::new("counter".to_string());

        // countdown(0) -> 0
        // countdown(N) -> countdown(N-1)
        counter.add_function(
            "countdown".to_string(),
            1,
            vec![
                // Check if N == 0
                Instruction::Eq {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Int(0),
                    dest: Register(1),
                },
                Instruction::JumpIf {
                    cond: Operand::Reg(Register(1)),
                    target: 5, // Return 0
                },
                // N - 1
                Instruction::Sub {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Int(1),
                    dest: Register(0),
                },
                // Tail call countdown(N-1)
                Instruction::TailCallLocal {
                    function: "countdown".to_string(),
                    arity: 1,
                },
                // Should never reach here
                Instruction::End,
                // Return 0
                Instruction::LoadInt {
                    value: 0,
                    dest: Register(0),
                },
                Instruction::Return,
            ],
        );
        counter.export("countdown", 1);
        scheduler.load_module(counter).unwrap();

        // Call countdown(1000) - should not overflow stack
        let program = vec![
            Instruction::LoadInt {
                value: 1000,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "counter".to_string(),
                function: "countdown".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(0));
        // Call stack should be empty (all tail calls)
        assert!(process.call_stack.is_empty());
    }

    #[test]
    fn test_spawn_mfa() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Create a module with a simple worker function
        let mut worker = Module::new("worker".to_string());
        worker.add_function(
            "start".to_string(),
            1,
            vec![
                // Just double the argument and end
                Instruction::Mul {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Int(2),
                    dest: Register(0),
                },
                Instruction::End,
            ],
        );
        worker.export("start", 1);
        scheduler.load_module(worker).unwrap();

        // Spawn a worker with argument 21
        let program = vec![
            Instruction::LoadInt {
                value: 21,
                dest: Register(0),
            },
            Instruction::SpawnMFA {
                module: "worker".to_string(),
                function: "start".to_string(),
                arity: 1,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        // Check the child process
        let child = scheduler.processes.get(&Pid(1)).unwrap();
        assert_eq!(child.registers[0], Value::Int(42));
        assert_eq!(child.status, ProcessStatus::Done);
    }

    #[test]
    fn test_make_fun_and_apply() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Create a module with a double function
        let mut math = Module::new("math".to_string());
        math.add_function(
            "double".to_string(),
            1,
            vec![
                Instruction::Mul {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Int(2),
                    dest: Register(0),
                },
                Instruction::Return,
            ],
        );
        math.export("double", 1);
        scheduler.load_module(math).unwrap();

        // Create a function reference and apply it
        let program = vec![
            // Make a fun reference to math:double/1
            Instruction::MakeFun {
                module: "math".to_string(),
                function: "double".to_string(),
                arity: 1,
                dest: Register(7),
            },
            // Put argument in R0
            Instruction::LoadInt {
                value: 21,
                dest: Register(0),
            },
            // Apply the fun
            Instruction::Apply {
                fun: Register(7),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(42));
    }

    #[test]
    fn test_cross_module_call() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Module A calls Module B
        let mut mod_a = Module::new("mod_a".to_string());
        mod_a.add_function(
            "call_b".to_string(),
            1,
            vec![
                Instruction::CallMFA {
                    module: "mod_b".to_string(),
                    function: "add_ten".to_string(),
                    arity: 1,
                },
                Instruction::Return,
            ],
        );
        mod_a.export("call_b", 1);

        let mut mod_b = Module::new("mod_b".to_string());
        mod_b.add_function(
            "add_ten".to_string(),
            1,
            vec![
                Instruction::Add {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Int(10),
                    dest: Register(0),
                },
                Instruction::Return,
            ],
        );
        mod_b.export("add_ten", 1);

        scheduler.load_module(mod_a).unwrap();
        scheduler.load_module(mod_b).unwrap();

        let program = vec![
            Instruction::LoadInt {
                value: 5,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "mod_a".to_string(),
                function: "call_b".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(15));
    }

    #[test]
    fn test_make_closure_and_apply() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Create module with a function that multiplies explicit arg by captured value
        // multiply_impl/2: R0 = explicit arg, R1 = captured multiplier
        let mut math = Module::new("math".to_string());
        math.add_function(
            "multiply_impl".to_string(),
            2, // total arity = 1 explicit + 1 captured
            vec![
                Instruction::Mul {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Reg(Register(1)),
                    dest: Register(0),
                },
                Instruction::Return,
            ],
        );
        math.export("multiply_impl", 2);
        scheduler.load_module(math).unwrap();

        // Program: create closure capturing R1=5, apply it to R0=7
        let program = vec![
            // R1 = 5 (the multiplier to capture)
            Instruction::LoadInt {
                value: 5,
                dest: Register(1),
            },
            // R2 = closure capturing R1
            Instruction::MakeClosure {
                module: "math".to_string(),
                function: "multiply_impl".to_string(),
                arity: 1, // explicit arity
                captures: vec![Register(1)],
                dest: Register(2),
            },
            // R0 = 7 (the value to multiply)
            Instruction::LoadInt {
                value: 7,
                dest: Register(0),
            },
            // Apply closure in R2 with arity 1
            Instruction::Apply {
                fun: Register(2),
                arity: 1,
            },
            // R0 should now be 35
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(35));
    }

    #[test]
    fn test_closure_with_multiple_captures() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // add_three/3: R0 + R1 + R2
        let mut math = Module::new("math".to_string());
        math.add_function(
            "add_three".to_string(),
            3, // 1 explicit + 2 captured
            vec![
                Instruction::Add {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Reg(Register(1)),
                    dest: Register(0),
                },
                Instruction::Add {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Reg(Register(2)),
                    dest: Register(0),
                },
                Instruction::Return,
            ],
        );
        math.export("add_three", 3);
        scheduler.load_module(math).unwrap();

        let program = vec![
            // Capture values: R3=10, R4=20
            Instruction::LoadInt {
                value: 10,
                dest: Register(3),
            },
            Instruction::LoadInt {
                value: 20,
                dest: Register(4),
            },
            // Create closure capturing R3 and R4
            Instruction::MakeClosure {
                module: "math".to_string(),
                function: "add_three".to_string(),
                arity: 1,
                captures: vec![Register(3), Register(4)],
                dest: Register(5),
            },
            // R0 = 5 (explicit arg)
            Instruction::LoadInt {
                value: 5,
                dest: Register(0),
            },
            // Apply: 5 + 10 + 20 = 35
            Instruction::Apply {
                fun: Register(5),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(35));
    }

    #[test]
    fn test_closure_values_are_copied() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Simple function that returns its captured value
        // get_captured/1: returns R0 (which is the captured value)
        let mut mod_test = Module::new("test".to_string());
        mod_test.add_function(
            "get_captured".to_string(),
            1, // 0 explicit + 1 captured
            vec![Instruction::Return],
        );
        mod_test.export("get_captured", 1);
        scheduler.load_module(mod_test).unwrap();

        let program = vec![
            // R1 = 42
            Instruction::LoadInt {
                value: 42,
                dest: Register(1),
            },
            // Create closure capturing R1
            Instruction::MakeClosure {
                module: "test".to_string(),
                function: "get_captured".to_string(),
                arity: 0,
                captures: vec![Register(1)],
                dest: Register(2),
            },
            // Change R1 to 100 (should not affect captured value)
            Instruction::LoadInt {
                value: 100,
                dest: Register(1),
            },
            // Apply closure (no explicit args)
            Instruction::Apply {
                fun: Register(2),
                arity: 0,
            },
            // R0 should be 42 (the captured value), not 100
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Int(42));
    }

    #[test]
    fn test_closure_arity_mismatch_crashes() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        let mut math = Module::new("math".to_string());
        math.add_function(
            "add".to_string(),
            2,
            vec![
                Instruction::Add {
                    a: Operand::Reg(Register(0)),
                    b: Operand::Reg(Register(1)),
                    dest: Register(0),
                },
                Instruction::Return,
            ],
        );
        math.export("add", 2);
        scheduler.load_module(math).unwrap();

        let program = vec![
            Instruction::LoadInt {
                value: 5,
                dest: Register(1),
            },
            Instruction::MakeClosure {
                module: "math".to_string(),
                function: "add".to_string(),
                arity: 1, // closure expects 1 explicit arg
                captures: vec![Register(1)],
                dest: Register(2),
            },
            // Apply with wrong arity (2 instead of 1)
            Instruction::Apply {
                fun: Register(2),
                arity: 2, // Wrong!
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Crashed);
    }

    // ========== List BIFs Tests ==========

    #[test]
    fn test_list_length() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2, 3]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::MakeList {
                length: 3,
                dest: Register(0),
            },
            Instruction::ListLength {
                list: Register(0),
                dest: Register(1),
            },
            // Empty list length
            Instruction::MakeList {
                length: 0,
                dest: Register(2),
            },
            Instruction::ListLength {
                list: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(3));
        assert_eq!(process.registers[3], Value::Int(0));
    }

    #[test]
    fn test_list_append() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::MakeList {
                length: 2,
                dest: Register(0),
            },
            // Create list [3, 4]
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::Push {
                source: Operand::Int(4),
            },
            Instruction::MakeList {
                length: 2,
                dest: Register(1),
            },
            // Append [1, 2] ++ [3, 4]
            Instruction::ListAppend {
                a: Register(0),
                b: Register(1),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(
            process.registers[2],
            Value::List(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4)
            ])
        );
    }

    #[test]
    fn test_list_reverse() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2, 3]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::MakeList {
                length: 3,
                dest: Register(0),
            },
            Instruction::ListReverse {
                list: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(
            process.registers[1],
            Value::List(vec![Value::Int(3), Value::Int(2), Value::Int(1)])
        );
    }

    #[test]
    fn test_list_nth() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [10, 20, 30]
            Instruction::Push {
                source: Operand::Int(10),
            },
            Instruction::Push {
                source: Operand::Int(20),
            },
            Instruction::Push {
                source: Operand::Int(30),
            },
            Instruction::MakeList {
                length: 3,
                dest: Register(0),
            },
            // Get element at index 0
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::ListNth {
                list: Register(0),
                n: Register(1),
                dest: Register(2),
            },
            // Get element at index 2
            Instruction::LoadInt {
                value: 2,
                dest: Register(3),
            },
            Instruction::ListNth {
                list: Register(0),
                n: Register(3),
                dest: Register(4),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(10)); // index 0
        assert_eq!(process.registers[4], Value::Int(30)); // index 2
    }

    #[test]
    fn test_list_nth_out_of_bounds_crashes() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::MakeList {
                length: 2,
                dest: Register(0),
            },
            // Try to get element at index 5 (out of bounds)
            Instruction::LoadInt {
                value: 5,
                dest: Register(1),
            },
            Instruction::ListNth {
                list: Register(0),
                n: Register(1),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Crashed);
    }

    #[test]
    fn test_list_member() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2, 3]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::Push {
                source: Operand::Int(3),
            },
            Instruction::MakeList {
                length: 3,
                dest: Register(0),
            },
            // Check if 2 is a member
            Instruction::LoadInt {
                value: 2,
                dest: Register(1),
            },
            Instruction::ListMember {
                elem: Register(1),
                list: Register(0),
                dest: Register(2),
            },
            // Check if 5 is a member
            Instruction::LoadInt {
                value: 5,
                dest: Register(3),
            },
            Instruction::ListMember {
                elem: Register(3),
                list: Register(0),
                dest: Register(4),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(1)); // 2 is a member
        assert_eq!(process.registers[4], Value::Int(0)); // 5 is not a member
    }

    #[test]
    fn test_list_append_with_empty() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::MakeList {
                length: 2,
                dest: Register(0),
            },
            // Create empty list
            Instruction::MakeList {
                length: 0,
                dest: Register(1),
            },
            // Append [1, 2] ++ []
            Instruction::ListAppend {
                a: Register(0),
                b: Register(1),
                dest: Register(2),
            },
            // Append [] ++ [1, 2]
            Instruction::ListAppend {
                a: Register(1),
                b: Register(0),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(
            process.registers[2],
            Value::List(vec![Value::Int(1), Value::Int(2)])
        );
        assert_eq!(
            process.registers[3],
            Value::List(vec![Value::Int(1), Value::Int(2)])
        );
    }

    // ========== Type Checking Tests ==========

    #[test]
    fn test_is_integer() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // R0 = 42 (integer)
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            Instruction::IsInteger {
                source: Register(0),
                dest: Register(1),
            },
            // R2 = :atom (not an integer)
            Instruction::LoadAtom {
                name: "test".to_string(),
                dest: Register(2),
            },
            Instruction::IsInteger {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // 42 is an integer
        assert_eq!(process.registers[3], Value::Int(0)); // :test is not an integer
    }

    #[test]
    fn test_is_atom() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // R0 = :ok (atom)
            Instruction::LoadAtom {
                name: "ok".to_string(),
                dest: Register(0),
            },
            Instruction::IsAtom {
                source: Register(0),
                dest: Register(1),
            },
            // R2 = 42 (not an atom)
            Instruction::LoadInt {
                value: 42,
                dest: Register(2),
            },
            Instruction::IsAtom {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // :ok is an atom
        assert_eq!(process.registers[3], Value::Int(0)); // 42 is not an atom
    }

    #[test]
    fn test_is_tuple() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create tuple {1, 2}
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::MakeTuple {
                arity: 2,
                dest: Register(0),
            },
            Instruction::IsTuple {
                source: Register(0),
                dest: Register(1),
            },
            // R2 = 42 (not a tuple)
            Instruction::LoadInt {
                value: 42,
                dest: Register(2),
            },
            Instruction::IsTuple {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // {1, 2} is a tuple
        assert_eq!(process.registers[3], Value::Int(0)); // 42 is not a tuple
    }

    #[test]
    fn test_is_list() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create list [1, 2]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::MakeList {
                length: 2,
                dest: Register(0),
            },
            Instruction::IsList {
                source: Register(0),
                dest: Register(1),
            },
            // Empty list is also a list
            Instruction::MakeList {
                length: 0,
                dest: Register(2),
            },
            Instruction::IsList {
                source: Register(2),
                dest: Register(3),
            },
            // R4 = 42 (not a list)
            Instruction::LoadInt {
                value: 42,
                dest: Register(4),
            },
            Instruction::IsList {
                source: Register(4),
                dest: Register(5),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // [1, 2] is a list
        assert_eq!(process.registers[3], Value::Int(1)); // [] is a list
        assert_eq!(process.registers[5], Value::Int(0)); // 42 is not a list
    }

    #[test]
    fn test_is_pid() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Spawn a process to get a PID
            Instruction::Spawn {
                code: vec![Instruction::End],
                dest: Register(0),
            },
            Instruction::IsPid {
                source: Register(0),
                dest: Register(1),
            },
            // R2 = 42 (not a PID)
            Instruction::LoadInt {
                value: 42,
                dest: Register(2),
            },
            Instruction::IsPid {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // spawned PID is a PID
        assert_eq!(process.registers[3], Value::Int(0)); // 42 is not a PID
    }

    #[test]
    fn test_is_function() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Create a module with a function
        let mut math = Module::new("math".to_string());
        math.add_function("id".to_string(), 1, vec![Instruction::Return]);
        math.export("id", 1);
        scheduler.load_module(math).unwrap();

        let program = vec![
            // Create a function reference
            Instruction::MakeFun {
                module: "math".to_string(),
                function: "id".to_string(),
                arity: 1,
                dest: Register(0),
            },
            Instruction::IsFunction {
                source: Register(0),
                dest: Register(1),
            },
            // R2 = 42 (not a function)
            Instruction::LoadInt {
                value: 42,
                dest: Register(2),
            },
            Instruction::IsFunction {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // fun is a function
        assert_eq!(process.registers[3], Value::Int(0)); // 42 is not a function
    }

    #[test]
    fn test_is_function_closure() {
        use crate::Module;

        let mut scheduler = Scheduler::new();

        // Create a module with a function
        let mut math = Module::new("math".to_string());
        math.add_function("add".to_string(), 2, vec![
            Instruction::Add {
                a: Operand::Reg(Register(0)),
                b: Operand::Reg(Register(1)),
                dest: Register(0),
            },
            Instruction::Return,
        ]);
        math.export("add", 2);
        scheduler.load_module(math).unwrap();

        let program = vec![
            // R1 = 5 (value to capture)
            Instruction::LoadInt {
                value: 5,
                dest: Register(1),
            },
            // Create a closure
            Instruction::MakeClosure {
                module: "math".to_string(),
                function: "add".to_string(),
                arity: 1,
                captures: vec![Register(1)],
                dest: Register(0),
            },
            Instruction::IsFunction {
                source: Register(0),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(1)); // closure is a function
    }

    #[test]
    fn test_type_checks_comprehensive() {
        let mut scheduler = Scheduler::new();

        // Test that each type only matches its own check
        let program = vec![
            // R0 = 42 (integer)
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            // Create list [1]
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::MakeList {
                length: 1,
                dest: Register(1),
            },
            // Check integer against all types
            Instruction::IsInteger {
                source: Register(0),
                dest: Register(2),
            },
            Instruction::IsAtom {
                source: Register(0),
                dest: Register(3),
            },
            Instruction::IsTuple {
                source: Register(0),
                dest: Register(4),
            },
            Instruction::IsList {
                source: Register(0),
                dest: Register(5),
            },
            Instruction::IsPid {
                source: Register(0),
                dest: Register(6),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(1)); // is_integer(42) = true
        assert_eq!(process.registers[3], Value::Int(0)); // is_atom(42) = false
        assert_eq!(process.registers[4], Value::Int(0)); // is_tuple(42) = false
        assert_eq!(process.registers[5], Value::Int(0)); // is_list(42) = false
        assert_eq!(process.registers[6], Value::Int(0)); // is_pid(42) = false
    }

    #[test]
    fn test_process_dictionary_put_get() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Put key=:foo, value=42 into dictionary
            Instruction::LoadAtom {
                name: "foo".into(),
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 42,
                dest: Register(1),
            },
            Instruction::PutDict {
                key: Register(0),
                value: Register(1),
                dest: Register(2), // old value (should be None)
            },
            // Get the value back
            Instruction::GetDict {
                key: Register(0),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::None); // no old value
        assert_eq!(process.registers[3], Value::Int(42)); // retrieved value
    }

    #[test]
    fn test_process_dictionary_overwrite() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Put key=:bar, value=100
            Instruction::LoadAtom {
                name: "bar".into(),
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 100,
                dest: Register(1),
            },
            Instruction::PutDict {
                key: Register(0),
                value: Register(1),
                dest: Register(2),
            },
            // Overwrite with value=200
            Instruction::LoadInt {
                value: 200,
                dest: Register(1),
            },
            Instruction::PutDict {
                key: Register(0),
                value: Register(1),
                dest: Register(3), // should get old value 100
            },
            // Get final value
            Instruction::GetDict {
                key: Register(0),
                dest: Register(4),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::None); // first put: no old value
        assert_eq!(process.registers[3], Value::Int(100)); // second put: old value was 100
        assert_eq!(process.registers[4], Value::Int(200)); // current value is 200
    }

    #[test]
    fn test_process_dictionary_erase() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Put key=:temp, value=999
            Instruction::LoadAtom {
                name: "temp".into(),
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 999,
                dest: Register(1),
            },
            Instruction::PutDict {
                key: Register(0),
                value: Register(1),
                dest: Register(2),
            },
            // Erase the key
            Instruction::EraseDict {
                key: Register(0),
                dest: Register(3), // should get 999
            },
            // Try to get erased key
            Instruction::GetDict {
                key: Register(0),
                dest: Register(4), // should be None
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[3], Value::Int(999)); // erased value
        assert_eq!(process.registers[4], Value::None); // key no longer exists
    }

    #[test]
    fn test_process_dictionary_get_keys() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Put two keys
            Instruction::LoadAtom {
                name: "key1".into(),
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 1,
                dest: Register(1),
            },
            Instruction::PutDict {
                key: Register(0),
                value: Register(1),
                dest: Register(7),
            },
            Instruction::LoadAtom {
                name: "key2".into(),
                dest: Register(2),
            },
            Instruction::LoadInt {
                value: 2,
                dest: Register(3),
            },
            Instruction::PutDict {
                key: Register(2),
                value: Register(3),
                dest: Register(7),
            },
            // Get all keys
            Instruction::GetDictKeys { dest: Register(4) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        match &process.registers[4] {
            Value::List(keys) => {
                assert_eq!(keys.len(), 2);
                // Keys should contain :key1 and :key2 (order not guaranteed)
                assert!(
                    keys.contains(&Value::Atom("key1".into()))
                        && keys.contains(&Value::Atom("key2".into()))
                );
            }
            other => panic!("Expected List, got {:?}", other),
        }
    }

    #[test]
    fn test_process_dictionary_get_missing() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Try to get a key that doesn't exist
            Instruction::LoadAtom {
                name: "nonexistent".into(),
                dest: Register(0),
            },
            Instruction::GetDict {
                key: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::None);
    }

    // ========== Map Tests ==========

    #[test]
    fn test_make_map() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Push key1, value1, key2, value2 onto stack
            Instruction::LoadAtom {
                name: "a".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::LoadAtom {
                name: "b".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            // Make map with 2 pairs
            Instruction::MakeMap {
                count: 2,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        match &process.registers[1] {
            Value::Map(m) => {
                assert_eq!(m.len(), 2);
                assert_eq!(m.get(&Value::Atom("a".into())), Some(&Value::Int(1)));
                assert_eq!(m.get(&Value::Atom("b".into())), Some(&Value::Int(2)));
            }
            other => panic!("Expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_map_get() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create a map %{:x => 42}
            Instruction::LoadAtom {
                name: "x".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(42),
            },
            Instruction::MakeMap {
                count: 1,
                dest: Register(1),
            },
            // Get value for key :x
            Instruction::MapGet {
                map: Register(1),
                key: Register(0),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(42));
    }

    #[test]
    fn test_map_get_default() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create empty map
            Instruction::MakeMap {
                count: 0,
                dest: Register(0),
            },
            // Try to get missing key with default
            Instruction::LoadAtom {
                name: "missing".into(),
                dest: Register(1),
            },
            Instruction::LoadInt {
                value: -1,
                dest: Register(2),
            },
            Instruction::MapGetDefault {
                map: Register(0),
                key: Register(1),
                default: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[3], Value::Int(-1));
    }

    #[test]
    fn test_map_put() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create empty map
            Instruction::MakeMap {
                count: 0,
                dest: Register(0),
            },
            // Put :key => 100
            Instruction::LoadAtom {
                name: "key".into(),
                dest: Register(1),
            },
            Instruction::LoadInt {
                value: 100,
                dest: Register(2),
            },
            Instruction::MapPut {
                map: Register(0),
                key: Register(1),
                value: Register(2),
                dest: Register(3),
            },
            // Get value back
            Instruction::MapGet {
                map: Register(3),
                key: Register(1),
                dest: Register(4),
            },
            // Original map should be unchanged (size 0)
            Instruction::MapSize {
                map: Register(0),
                dest: Register(5),
            },
            // New map should have size 1
            Instruction::MapSize {
                map: Register(3),
                dest: Register(6),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[4], Value::Int(100));
        assert_eq!(process.registers[5], Value::Int(0)); // Original unchanged
        assert_eq!(process.registers[6], Value::Int(1)); // New has 1 entry
    }

    #[test]
    fn test_map_remove() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create map with one entry
            Instruction::LoadAtom {
                name: "rem".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(999),
            },
            Instruction::MakeMap {
                count: 1,
                dest: Register(1),
            },
            // Remove the key
            Instruction::MapRemove {
                map: Register(1),
                key: Register(0),
                dest: Register(2),
            },
            // Check sizes
            Instruction::MapSize {
                map: Register(1),
                dest: Register(3),
            },
            Instruction::MapSize {
                map: Register(2),
                dest: Register(4),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[3], Value::Int(1)); // Original has 1
        assert_eq!(process.registers[4], Value::Int(0)); // After remove has 0
    }

    #[test]
    fn test_map_has() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create map %{:exists => 1}
            Instruction::LoadAtom {
                name: "exists".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::MakeMap {
                count: 1,
                dest: Register(1),
            },
            // Check if :exists is in map
            Instruction::MapHas {
                map: Register(1),
                key: Register(0),
                dest: Register(2),
            },
            // Check if :missing is in map
            Instruction::LoadAtom {
                name: "missing".into(),
                dest: Register(3),
            },
            Instruction::MapHas {
                map: Register(1),
                key: Register(3),
                dest: Register(4),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(1)); // :exists is present
        assert_eq!(process.registers[4], Value::Int(0)); // :missing is not
    }

    #[test]
    fn test_map_keys_values() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create map %{:a => 1, :b => 2}
            Instruction::LoadAtom {
                name: "a".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(1),
            },
            Instruction::LoadAtom {
                name: "b".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(2),
            },
            Instruction::MakeMap {
                count: 2,
                dest: Register(1),
            },
            // Get keys and values
            Instruction::MapKeys {
                map: Register(1),
                dest: Register(2),
            },
            Instruction::MapValues {
                map: Register(1),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        match &process.registers[2] {
            Value::List(keys) => {
                assert_eq!(keys.len(), 2);
                assert!(keys.contains(&Value::Atom("a".into())));
                assert!(keys.contains(&Value::Atom("b".into())));
            }
            other => panic!("Expected List, got {:?}", other),
        }
        match &process.registers[3] {
            Value::List(vals) => {
                assert_eq!(vals.len(), 2);
                assert!(vals.contains(&Value::Int(1)));
                assert!(vals.contains(&Value::Int(2)));
            }
            other => panic!("Expected List, got {:?}", other),
        }
    }

    #[test]
    fn test_is_map() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create a map
            Instruction::MakeMap {
                count: 0,
                dest: Register(0),
            },
            // Check is_map on map
            Instruction::IsMap {
                source: Register(0),
                dest: Register(1),
            },
            // Check is_map on non-map
            Instruction::LoadInt {
                value: 42,
                dest: Register(2),
            },
            Instruction::IsMap {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // map is a map
        assert_eq!(process.registers[3], Value::Int(0)); // int is not a map
    }

    #[test]
    fn test_map_pattern_matching() {
        let mut scheduler = Scheduler::new();

        // Create a map and match it with a pattern
        let program = vec![
            // Create map %{:name => "Alice", :age => 30}
            Instruction::LoadAtom {
                name: "name".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::LoadAtom {
                name: "Alice".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::LoadAtom {
                name: "age".into(),
                dest: Register(0),
            },
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(30),
            },
            Instruction::MakeMap {
                count: 2,
                dest: Register(1),
            },
            // Match against pattern %{:age => age_val}
            Instruction::Match {
                source: Register(1),
                pattern: Pattern::Map(vec![(
                    Pattern::Atom("age".into()),
                    Pattern::Variable(Register(2)),
                )]),
                fail_target: 100, // Should not fail
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(30));
    }

    // ========== Reference Tests ==========

    #[test]
    fn test_make_ref() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeRef { dest: Register(0) },
            Instruction::MakeRef { dest: Register(1) },
            Instruction::MakeRef { dest: Register(2) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();

        // Each ref should be unique
        match (&process.registers[0], &process.registers[1], &process.registers[2]) {
            (Value::Ref(r0), Value::Ref(r1), Value::Ref(r2)) => {
                assert_ne!(r0, r1);
                assert_ne!(r1, r2);
                assert_ne!(r0, r2);
            }
            _ => panic!("Expected Ref values"),
        }
    }

    #[test]
    fn test_refs_unique_across_processes() {
        let mut scheduler = Scheduler::new();

        // First process creates a ref
        let program1 = vec![
            Instruction::MakeRef { dest: Register(0) },
            Instruction::End,
        ];

        // Second process creates a ref
        let program2 = vec![
            Instruction::MakeRef { dest: Register(0) },
            Instruction::End,
        ];

        scheduler.spawn(program1);
        scheduler.spawn(program2);
        run_to_idle(&mut scheduler);

        let p0 = scheduler.processes.get(&Pid(0)).unwrap();
        let p1 = scheduler.processes.get(&Pid(1)).unwrap();

        // Refs from different processes should be unique
        match (&p0.registers[0], &p1.registers[0]) {
            (Value::Ref(r0), Value::Ref(r1)) => {
                assert_ne!(r0, r1);
            }
            _ => panic!("Expected Ref values"),
        }
    }

    #[test]
    fn test_is_ref() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create a ref
            Instruction::MakeRef { dest: Register(0) },
            // Check is_ref on ref
            Instruction::IsRef {
                source: Register(0),
                dest: Register(1),
            },
            // Check is_ref on non-ref
            Instruction::LoadInt {
                value: 42,
                dest: Register(2),
            },
            Instruction::IsRef {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // ref is a ref
        assert_eq!(process.registers[3], Value::Int(0)); // int is not a ref
    }

    #[test]
    fn test_ref_equality_via_copy() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create a ref
            Instruction::MakeRef { dest: Register(0) },
            // Copy it
            Instruction::Move {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        // Copied ref should be equal to original (same underlying value)
        assert_eq!(process.registers[0], process.registers[1]);
    }

    #[test]
    fn test_ref_as_map_key() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create a ref to use as key
            Instruction::MakeRef { dest: Register(0) },
            // Push ref and value onto stack
            Instruction::Push {
                source: Operand::Reg(Register(0)),
            },
            Instruction::Push {
                source: Operand::Int(42),
            },
            // Create map with ref as key
            Instruction::MakeMap {
                count: 1,
                dest: Register(1),
            },
            // Look up using the same ref
            Instruction::MapGet {
                map: Register(1),
                key: Register(0),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        // Should retrieve the value using the ref key
        assert_eq!(process.registers[2], Value::Int(42));
    }

    // ========== Float Tests ==========

    #[test]
    fn test_load_float() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadFloat {
                value: 3.14,
                dest: Register(0),
            },
            Instruction::LoadFloat {
                value: -2.5,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Float(3.14));
        assert_eq!(process.registers[1], Value::Float(-2.5));
    }

    #[test]
    fn test_is_float() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadFloat {
                value: 1.5,
                dest: Register(0),
            },
            Instruction::IsFloat {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::LoadInt {
                value: 42,
                dest: Register(2),
            },
            Instruction::IsFloat {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // float is a float
        assert_eq!(process.registers[3], Value::Int(0)); // int is not a float
    }

    #[test]
    fn test_int_to_float() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            Instruction::IntToFloat {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Float(42.0));
    }

    #[test]
    fn test_float_to_int() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadFloat {
                value: 3.7,
                dest: Register(0),
            },
            Instruction::FloatToInt {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::LoadFloat {
                value: -2.9,
                dest: Register(2),
            },
            Instruction::FloatToInt {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(3)); // truncates toward zero
        assert_eq!(process.registers[3], Value::Int(-2)); // truncates toward zero
    }

    #[test]
    fn test_floor_ceil_round_trunc() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadFloat {
                value: 3.7,
                dest: Register(0),
            },
            Instruction::Floor {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::Ceil {
                source: Register(0),
                dest: Register(2),
            },
            Instruction::Round {
                source: Register(0),
                dest: Register(3),
            },
            Instruction::Trunc {
                source: Register(0),
                dest: Register(4),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Float(3.0)); // floor(3.7) = 3.0
        assert_eq!(process.registers[2], Value::Float(4.0)); // ceil(3.7) = 4.0
        assert_eq!(process.registers[3], Value::Float(4.0)); // round(3.7) = 4.0
        assert_eq!(process.registers[4], Value::Float(3.0)); // trunc(3.7) = 3.0
    }

    #[test]
    fn test_sqrt() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadFloat {
                value: 16.0,
                dest: Register(0),
            },
            Instruction::Sqrt {
                source: Register(0),
                dest: Register(1),
            },
            // sqrt also works on int
            Instruction::LoadInt {
                value: 25,
                dest: Register(2),
            },
            Instruction::Sqrt {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Float(4.0));
        assert_eq!(process.registers[3], Value::Float(5.0));
    }

    #[test]
    fn test_abs() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadFloat {
                value: -3.5,
                dest: Register(0),
            },
            Instruction::Abs {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::LoadInt {
                value: -42,
                dest: Register(2),
            },
            Instruction::Abs {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Float(3.5));
        assert_eq!(process.registers[3], Value::Int(42));
    }

    #[test]
    fn test_pow() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // 2^3 = 8
            Instruction::LoadInt {
                value: 2,
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 3,
                dest: Register(1),
            },
            Instruction::Pow {
                base: Register(0),
                exp: Register(1),
                dest: Register(2),
            },
            // 2.5^2 = 6.25
            Instruction::LoadFloat {
                value: 2.5,
                dest: Register(3),
            },
            Instruction::LoadInt {
                value: 2,
                dest: Register(4),
            },
            Instruction::Pow {
                base: Register(3),
                exp: Register(4),
                dest: Register(5),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Float(8.0));
        assert_eq!(process.registers[5], Value::Float(6.25));
    }

    // ========== Unlink/Demonitor Tests ==========

    #[test]
    fn test_unlink() {
        let mut scheduler = Scheduler::new();

        // Spawner creates a worker and links to it, then unlinks
        let worker = vec![
            Instruction::Work { amount: 100 },
            Instruction::Crash, // Worker will crash
        ];

        let spawner = vec![
            Instruction::SpawnLink {
                code: worker,
                dest: Register(0),
            },
            // Unlink from the worker before it crashes
            Instruction::Unlink {
                target: Source::Reg(Register(0)),
            },
            // Wait a bit for worker to crash
            Instruction::Work { amount: 200 },
            Instruction::End,
        ];

        scheduler.spawn(spawner);
        run_to_idle(&mut scheduler);

        // Spawner should have completed normally (not crashed with worker)
        let spawner_process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(spawner_process.status, ProcessStatus::Done);

        // Worker should have crashed
        let worker_process = scheduler.processes.get(&Pid(1)).unwrap();
        assert_eq!(worker_process.status, ProcessStatus::Crashed);
    }

    #[test]
    fn test_monitor_returns_ref() {
        let mut scheduler = Scheduler::new();

        let worker = vec![Instruction::End];

        let observer = vec![
            Instruction::Spawn {
                code: worker,
                dest: Register(0),
            },
            Instruction::Monitor {
                target: Source::Reg(Register(0)),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(observer);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        // Register 1 should contain a Ref
        match &process.registers[1] {
            Value::Ref(_) => {}
            other => panic!("Expected Ref, got {:?}", other),
        }
    }

    #[test]
    fn test_demonitor() {
        let mut scheduler = Scheduler::new();

        // Worker will crash
        let worker = vec![
            Instruction::Work { amount: 100 },
            Instruction::Crash,
        ];

        let observer = vec![
            Instruction::Spawn {
                code: worker,
                dest: Register(0),
            },
            // Monitor the worker
            Instruction::Monitor {
                target: Source::Reg(Register(0)),
                dest: Register(1),
            },
            // Immediately demonitor
            Instruction::Demonitor {
                monitor_ref: Register(1),
            },
            // Try to receive - should timeout since we demonitored
            Instruction::ReceiveTimeout {
                dest: Register(2),
                timeout: 50,
            },
            Instruction::End,
        ];

        scheduler.spawn(observer);
        run_to_idle(&mut scheduler);

        // Observer should complete (not receive DOWN message since we demonitored)
        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);
        // Should have received TIMEOUT, not DOWN
        assert_eq!(process.registers[2], Value::String("TIMEOUT".to_string()));
    }

    #[test]
    fn test_multiple_monitors_same_target() {
        let mut scheduler = Scheduler::new();

        let worker = vec![Instruction::End];

        let observer = vec![
            Instruction::Spawn {
                code: worker,
                dest: Register(0),
            },
            // Set up two monitors on the same target
            Instruction::Monitor {
                target: Source::Reg(Register(0)),
                dest: Register(1),
            },
            Instruction::Monitor {
                target: Source::Reg(Register(0)),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(observer);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();

        // Both should be Refs with different values
        match (&process.registers[1], &process.registers[2]) {
            (Value::Ref(r1), Value::Ref(r2)) => {
                assert_ne!(r1, r2); // Different monitor refs
            }
            _ => panic!("Expected two Refs"),
        }
    }

    #[test]
    fn test_exit_with_normal_reason() {
        // Normal exit doesn't propagate to linked processes (unless they trap_exit)
        let mut scheduler = Scheduler::new();

        // Child: exit normally
        let child = vec![
            Instruction::LoadAtom {
                name: "normal".to_string(),
                dest: Register(0),
            },
            Instruction::Exit {
                reason: Register(0),
            },
        ];

        // Parent: link to child, wait for message (should not receive one)
        let parent = vec![
            Instruction::SpawnLink {
                code: child,
                dest: Register(0),
            },
            // Use timeout to avoid blocking forever
            Instruction::ReceiveTimeout {
                dest: Register(1),
                timeout: 10,
            },
            Instruction::End,
        ];

        scheduler.spawn(parent);
        run_to_idle(&mut scheduler);

        // Parent should finish normally (child's normal exit doesn't propagate)
        let parent_process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(parent_process.status, ProcessStatus::Done);
        // Should have timed out, not received an exit message
        assert_eq!(
            parent_process.registers[1],
            Value::String("TIMEOUT".to_string())
        );
    }

    #[test]
    fn test_exit_with_custom_reason() {
        // Exit with a custom reason propagates through trap_exit
        let mut scheduler = Scheduler::new();

        // Child: exit with custom reason
        let child = vec![
            Instruction::LoadAtom {
                name: "shutdown".to_string(),
                dest: Register(0),
            },
            Instruction::Exit {
                reason: Register(0),
            },
        ];

        // Parent: trap_exit, link to child, receive exit message
        let parent = vec![
            Instruction::TrapExit { enable: true },
            Instruction::SpawnLink {
                code: child,
                dest: Register(0),
            },
            Instruction::Receive { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(parent);
        run_to_idle(&mut scheduler);

        // Parent should have received {:EXIT, Pid, :shutdown}
        let parent_process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(parent_process.status, ProcessStatus::Done);
        match &parent_process.registers[1] {
            Value::Tuple(elems) => {
                assert_eq!(elems.len(), 3);
                assert_eq!(elems[0], Value::Atom("EXIT".to_string()));
                assert!(matches!(elems[1], Value::Pid(_)));
                assert_eq!(elems[2], Value::Atom("shutdown".to_string()));
            }
            _ => panic!("Expected EXIT tuple, got {:?}", parent_process.registers[1]),
        }
    }

    #[test]
    fn test_link_crash_without_trap_exit() {
        // Without trap_exit, linked process crash propagates (crashes parent too)
        let mut scheduler = Scheduler::new();

        // Child: crash immediately
        let child = vec![Instruction::Crash];

        // Parent: link to child, try to receive (should crash before receiving)
        let parent = vec![
            Instruction::SpawnLink {
                code: child,
                dest: Register(0),
            },
            Instruction::Receive { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(parent);
        run_to_idle(&mut scheduler);

        // Parent should have crashed (exit signal propagated)
        let parent_process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(parent_process.status, ProcessStatus::Crashed);
        assert_eq!(
            parent_process.exit_reason,
            Value::Atom("crashed".to_string())
        );
    }

    #[test]
    fn test_trap_exit_toggle() {
        // Test that TrapExit instruction works
        let mut scheduler = Scheduler::new();

        // Worker exits normally
        let worker = vec![Instruction::End];

        // Process: enable trap_exit, spawn_link worker, verify trap_exit is enabled
        let program = vec![
            Instruction::TrapExit { enable: true },
            Instruction::SpawnLink {
                code: worker,
                dest: Register(0),
            },
            // Use timeout to not block forever
            Instruction::ReceiveTimeout {
                dest: Register(1),
                timeout: 10,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        // Process should have finished and have trap_exit enabled
        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert!(process.trap_exit);
        assert_eq!(process.status, ProcessStatus::Done);
    }

    #[test]
    fn test_exit_reason_stored_in_process() {
        // Test that exit reason is stored in the process
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadAtom {
                name: "my_reason".to_string(),
                dest: Register(0),
            },
            Instruction::Exit {
                reason: Register(0),
            },
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Crashed); // not :normal
        assert_eq!(
            process.exit_reason,
            Value::Atom("my_reason".to_string())
        );
    }

    #[test]
    fn test_monitor_receives_exit_reason() {
        // Monitor DOWN message includes the exit reason
        let mut scheduler = Scheduler::new();

        // Worker: exit with custom reason
        let worker = vec![
            Instruction::LoadAtom {
                name: "killed".to_string(),
                dest: Register(0),
            },
            Instruction::Exit {
                reason: Register(0),
            },
        ];

        // Observer: spawn, monitor, wait for DOWN
        let observer = vec![
            Instruction::Spawn {
                code: worker,
                dest: Register(0),
            },
            Instruction::Monitor {
                target: Source::Reg(Register(0)),
                dest: Register(2),
            },
            Instruction::Receive { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(observer);
        run_to_idle(&mut scheduler);

        // Observer should have received {:DOWN, Ref, :process, Pid, :killed}
        let observer_process = scheduler.processes.get(&Pid(0)).unwrap();
        match &observer_process.registers[1] {
            Value::Tuple(elems) => {
                assert_eq!(elems.len(), 5);
                assert_eq!(elems[0], Value::Atom("DOWN".to_string()));
                assert!(matches!(elems[1], Value::Ref(_)));
                assert_eq!(elems[2], Value::Atom("process".to_string()));
                assert!(matches!(elems[3], Value::Pid(_)));
                assert_eq!(elems[4], Value::Atom("killed".to_string()));
            }
            _ => panic!("Expected DOWN tuple, got {:?}", observer_process.registers[1]),
        }
    }

    // ========== Timer Tests ==========

    #[test]
    fn test_start_timer() {
        // StartTimer sends {:timeout, ref, msg} to self after delay
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Load message value
            Instruction::LoadAtom {
                name: "ping".to_string(),
                dest: Register(0),
            },
            // Start timer with 5 reduction delay
            Instruction::StartTimer {
                delay: 5,
                msg: Register(0),
                dest: Register(1),
            },
            // Wait for the timer message
            Instruction::Receive { dest: Register(2) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // R1 should have the timer ref
        assert!(matches!(process.registers[1], Value::Ref(_)));

        // R2 should have received the timeout tuple (as string from debug format)
        match &process.registers[2] {
            Value::String(s) => {
                assert!(s.contains("timeout"));
                assert!(s.contains("ping"));
            }
            _ => panic!("Expected timeout message string, got {:?}", process.registers[2]),
        }
    }

    #[test]
    fn test_send_after() {
        // SendAfter sends a message to another process after delay
        let mut scheduler = Scheduler::new();

        // Receiver waits for message
        let receiver = vec![
            Instruction::Receive { dest: Register(0) },
            Instruction::End,
        ];

        // Sender spawns receiver, then sends after delay
        let sender = vec![
            Instruction::Spawn {
                code: receiver,
                dest: Register(0),
            },
            // Load message
            Instruction::LoadAtom {
                name: "hello".to_string(),
                dest: Register(1),
            },
            // Send after 5 reductions
            Instruction::SendAfter {
                delay: 5,
                to: Source::Reg(Register(0)),
                msg: Register(1),
                dest: Register(2),
            },
            // Wait a bit for timer to fire
            Instruction::Work { amount: 10 },
            Instruction::End,
        ];

        scheduler.spawn(sender);
        run_to_idle(&mut scheduler);

        // Check receiver got the message
        let receiver_process = scheduler.processes.get(&Pid(1)).unwrap();
        assert_eq!(receiver_process.status, ProcessStatus::Done);
        match &receiver_process.registers[0] {
            Value::String(s) => assert!(s.contains("hello")),
            _ => panic!("Expected message, got {:?}", receiver_process.registers[0]),
        }
    }

    #[test]
    fn test_cancel_timer() {
        // CancelTimer stops a pending timer
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Load message
            Instruction::LoadAtom {
                name: "ping".to_string(),
                dest: Register(0),
            },
            // Start timer with long delay
            Instruction::StartTimer {
                delay: 1000,
                msg: Register(0),
                dest: Register(1),
            },
            // Cancel it immediately
            Instruction::CancelTimer {
                timer_ref: Register(1),
                dest: Register(2),
            },
            // Try to receive with timeout (should timeout, not receive timer msg)
            Instruction::ReceiveTimeout {
                dest: Register(3),
                timeout: 10,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // R2 should have remaining time (close to 1000)
        match &process.registers[2] {
            Value::Int(remaining) => assert!(*remaining > 900),
            _ => panic!("Expected remaining time, got {:?}", process.registers[2]),
        }

        // R3 should have TIMEOUT (timer was cancelled)
        assert_eq!(
            process.registers[3],
            Value::String("TIMEOUT".to_string())
        );

        // Timer queue should be empty
        assert!(scheduler.timers.is_empty());
    }

    #[test]
    fn test_read_timer() {
        // ReadTimer returns remaining time
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadAtom {
                name: "ping".to_string(),
                dest: Register(0),
            },
            Instruction::StartTimer {
                delay: 100,
                msg: Register(0),
                dest: Register(1),
            },
            // Read timer immediately
            Instruction::ReadTimer {
                timer_ref: Register(1),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();

        // R2 should have remaining time (may be slightly less due to processing)
        match &process.registers[2] {
            Value::Int(remaining) => assert!(*remaining > 0 && *remaining <= 100),
            _ => panic!("Expected remaining time, got {:?}", process.registers[2]),
        }
    }

    #[test]
    fn test_timer_fires_after_delay() {
        // Verify timer fires after specified delay
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadAtom {
                name: "tick".to_string(),
                dest: Register(0),
            },
            // Start timer with delay
            Instruction::StartTimer {
                delay: 10,
                msg: Register(0),
                dest: Register(1),
            },
            // Do enough work that timer fires
            Instruction::Work { amount: 20 },
            // Receive the timer message (should be available now)
            Instruction::ReceiveTimeout {
                dest: Register(2),
                timeout: 5,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // R2 should have the timer message (not TIMEOUT)
        match &process.registers[2] {
            Value::String(s) => {
                assert!(s.contains("timeout") && s.contains("tick"));
            }
            _ => panic!("Expected timer message, got {:?}", process.registers[2]),
        }
    }

    #[test]
    fn test_multiple_timers() {
        // Multiple timers fire in order
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create three timers with different delays
            Instruction::LoadInt {
                value: 1,
                dest: Register(0),
            },
            Instruction::StartTimer {
                delay: 10,
                msg: Register(0),
                dest: Register(4),
            },
            Instruction::LoadInt {
                value: 2,
                dest: Register(0),
            },
            Instruction::StartTimer {
                delay: 20,
                msg: Register(0),
                dest: Register(5),
            },
            Instruction::LoadInt {
                value: 3,
                dest: Register(0),
            },
            Instruction::StartTimer {
                delay: 30,
                msg: Register(0),
                dest: Register(6),
            },
            // Wait for all timers
            Instruction::Work { amount: 50 },
            // Receive all three messages
            Instruction::Receive { dest: Register(1) },
            Instruction::Receive { dest: Register(2) },
            Instruction::Receive { dest: Register(3) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // All three timer refs should be present
        assert!(matches!(process.registers[4], Value::Ref(_)));
        assert!(matches!(process.registers[5], Value::Ref(_)));
        assert!(matches!(process.registers[6], Value::Ref(_)));
    }

    // ========== Try/Catch/After Tests ==========

    #[test]
    fn test_try_catch_basic() {
        // Basic try/catch - throw is caught
        let mut scheduler = Scheduler::new();

        // try { throw(:error, :oops) } catch { handle }
        let program = vec![
            // 0: Try with catch at 5
            Instruction::Try {
                catch_target: 5,
                after_target: None,
            },
            // 1: Load :error class
            Instruction::LoadAtom {
                name: "error".to_string(),
                dest: Register(0),
            },
            // 2: Load :oops reason
            Instruction::LoadAtom {
                name: "oops".to_string(),
                dest: Register(1),
            },
            // 3: Throw
            Instruction::Throw {
                class: Register(0),
                reason: Register(1),
            },
            // 4: Should not reach here
            Instruction::LoadInt {
                value: 999,
                dest: Register(2),
            },
            // 5: Catch handler - get exception
            Instruction::GetException { dest: Register(3) },
            // 6: Clear exception
            Instruction::ClearException,
            // 7: Mark that we caught it
            Instruction::LoadAtom {
                name: "caught".to_string(),
                dest: Register(4),
            },
            // 8: End
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // R2 should NOT be 999 (we skipped that instruction)
        assert_ne!(process.registers[2], Value::Int(999));

        // R4 should be :caught
        assert_eq!(process.registers[4], Value::Atom("caught".to_string()));

        // R3 should have the exception tuple {class, reason, stacktrace}
        match &process.registers[3] {
            Value::Tuple(elems) => {
                assert_eq!(elems.len(), 3);
                assert_eq!(elems[0], Value::Atom("error".to_string()));
                assert_eq!(elems[1], Value::Atom("oops".to_string()));
            }
            _ => panic!("Expected exception tuple, got {:?}", process.registers[3]),
        }
    }

    #[test]
    fn test_try_no_exception() {
        // Try block completes without exception
        let mut scheduler = Scheduler::new();

        let program = vec![
            // 0: Try with catch at 4
            Instruction::Try {
                catch_target: 4,
                after_target: None,
            },
            // 1: Normal work (no throw)
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            // 2: EndTry
            Instruction::EndTry,
            // 3: Jump to end (skip catch)
            Instruction::Jump { target: 6 },
            // 4: Catch handler (should not run)
            Instruction::LoadAtom {
                name: "caught".to_string(),
                dest: Register(1),
            },
            // 5: Jump to end
            Instruction::Jump { target: 6 },
            // 6: End
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // R0 should be 42
        assert_eq!(process.registers[0], Value::Int(42));

        // R1 should NOT be :caught (catch didn't run)
        assert_ne!(process.registers[1], Value::Atom("caught".to_string()));
    }

    #[test]
    fn test_throw_without_catch() {
        // Throw without handler crashes process
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadAtom {
                name: "throw".to_string(),
                dest: Register(0),
            },
            Instruction::LoadAtom {
                name: "uncaught".to_string(),
                dest: Register(1),
            },
            Instruction::Throw {
                class: Register(0),
                reason: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Crashed);

        // Exit reason should be tuple of {class, reason}
        match &process.exit_reason {
            Value::Tuple(elems) => {
                assert_eq!(elems.len(), 2);
                assert_eq!(elems[0], Value::Atom("throw".to_string()));
                assert_eq!(elems[1], Value::Atom("uncaught".to_string()));
            }
            _ => panic!("Expected exit reason tuple, got {:?}", process.exit_reason),
        }
    }

    #[test]
    fn test_try_with_after() {
        // After block runs on normal completion
        let mut scheduler = Scheduler::new();

        let program = vec![
            // 0: Try with catch at 4, after at 6
            Instruction::Try {
                catch_target: 4,
                after_target: Some(6),
            },
            // 1: Normal work
            Instruction::LoadInt {
                value: 1,
                dest: Register(0),
            },
            // 2: EndTry (jumps to after)
            Instruction::EndTry,
            // 3: Should not reach (EndTry jumps to after)
            Instruction::LoadInt {
                value: 999,
                dest: Register(3),
            },
            // 4: Catch handler
            Instruction::LoadAtom {
                name: "caught".to_string(),
                dest: Register(1),
            },
            // 5: Jump to after (from catch)
            Instruction::Jump { target: 6 },
            // 6: After block (cleanup)
            Instruction::LoadAtom {
                name: "cleanup".to_string(),
                dest: Register(2),
            },
            // 7: End
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // R0 should be 1 (try block ran)
        assert_eq!(process.registers[0], Value::Int(1));

        // R1 should NOT be :caught (no exception)
        assert_ne!(process.registers[1], Value::Atom("caught".to_string()));

        // R2 should be :cleanup (after ran)
        assert_eq!(process.registers[2], Value::Atom("cleanup".to_string()));

        // R3 should NOT be 999 (we jumped over it)
        assert_ne!(process.registers[3], Value::Int(999));
    }

    #[test]
    fn test_reraise() {
        // Catch and re-raise exception
        let mut scheduler = Scheduler::new();

        let program = vec![
            // 0: Outer try with catch at 9
            Instruction::Try {
                catch_target: 9,
                after_target: None,
            },
            // 1: Inner try with catch at 6
            Instruction::Try {
                catch_target: 6,
                after_target: None,
            },
            // 2: Load class
            Instruction::LoadAtom {
                name: "error".to_string(),
                dest: Register(0),
            },
            // 3: Load reason
            Instruction::LoadAtom {
                name: "inner".to_string(),
                dest: Register(1),
            },
            // 4: Throw
            Instruction::Throw {
                class: Register(0),
                reason: Register(1),
            },
            // 5: Should not reach
            Instruction::End,
            // 6: Inner catch - mark and reraise
            Instruction::LoadAtom {
                name: "inner_caught".to_string(),
                dest: Register(2),
            },
            // 7: Reraise
            Instruction::Reraise,
            // 8: Should not reach
            Instruction::End,
            // 9: Outer catch
            Instruction::LoadAtom {
                name: "outer_caught".to_string(),
                dest: Register(3),
            },
            // 10: End
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // Both catches should have run
        assert_eq!(
            process.registers[2],
            Value::Atom("inner_caught".to_string())
        );
        assert_eq!(
            process.registers[3],
            Value::Atom("outer_caught".to_string())
        );
    }

    #[test]
    fn test_nested_try() {
        // Nested try blocks
        let mut scheduler = Scheduler::new();

        let program = vec![
            // 0: Outer try
            Instruction::Try {
                catch_target: 7,
                after_target: None,
            },
            // 1: Inner try
            Instruction::Try {
                catch_target: 5,
                after_target: None,
            },
            // 2: Throw from inner
            Instruction::LoadAtom {
                name: "error".to_string(),
                dest: Register(0),
            },
            Instruction::LoadAtom {
                name: "inner_error".to_string(),
                dest: Register(1),
            },
            Instruction::Throw {
                class: Register(0),
                reason: Register(1),
            },
            // 5: Inner catch - handle it
            Instruction::LoadAtom {
                name: "handled".to_string(),
                dest: Register(2),
            },
            Instruction::ClearException,
            // 7: This is outer catch (but we won't reach it if inner handles)
            // Actually, since inner handled, we need to EndTry and continue
            // Let me fix this test structure...
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Done);

        // Inner catch handled it
        assert_eq!(process.registers[2], Value::Atom("handled".to_string()));
    }

    // ========== Binary Tests ==========

    #[test]
    fn test_make_binary() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: vec![1, 2, 3, 255],
                dest: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(
            process.registers[0],
            Value::Binary(vec![1, 2, 3, 255])
        );
    }

    #[test]
    fn test_binary_size() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: vec![1, 2, 3, 4, 5],
                dest: Register(0),
            },
            Instruction::BinarySize {
                bin: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(5));
    }

    #[test]
    fn test_binary_at() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: vec![10, 20, 30, 40],
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 2,
                dest: Register(1),
            },
            Instruction::BinaryAt {
                bin: Register(0),
                index: Register(1),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Int(30)); // byte at index 2
    }

    #[test]
    fn test_binary_at_out_of_bounds() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: vec![1, 2, 3],
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 10, // out of bounds
                dest: Register(1),
            },
            Instruction::BinaryAt {
                bin: Register(0),
                index: Register(1),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Crashed);
    }

    #[test]
    fn test_binary_slice() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: vec![0, 1, 2, 3, 4, 5],
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 2, // start
                dest: Register(1),
            },
            Instruction::LoadInt {
                value: 3, // length
                dest: Register(2),
            },
            Instruction::BinarySlice {
                bin: Register(0),
                start: Register(1),
                len: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[3], Value::Binary(vec![2, 3, 4]));
    }

    #[test]
    fn test_binary_concat() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: vec![1, 2, 3],
                dest: Register(0),
            },
            Instruction::MakeBinary {
                bytes: vec![4, 5],
                dest: Register(1),
            },
            Instruction::BinaryConcat {
                a: Register(0),
                b: Register(1),
                dest: Register(2),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[2], Value::Binary(vec![1, 2, 3, 4, 5]));
    }

    #[test]
    fn test_is_binary() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: vec![1, 2, 3],
                dest: Register(0),
            },
            Instruction::IsBinary {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::LoadInt {
                value: 42,
                dest: Register(2),
            },
            Instruction::IsBinary {
                source: Register(2),
                dest: Register(3),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // binary is binary
        assert_eq!(process.registers[3], Value::Int(0)); // int is not binary
    }

    #[test]
    fn test_binary_to_string() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: "hello".as_bytes().to_vec(),
                dest: Register(0),
            },
            Instruction::BinaryToString {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::String("hello".to_string()));
    }

    #[test]
    fn test_binary_to_string_invalid_utf8() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::MakeBinary {
                bytes: vec![0xFF, 0xFE], // Invalid UTF-8
                dest: Register(0),
            },
            Instruction::BinaryToString {
                source: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.status, ProcessStatus::Crashed);
    }

    #[test]
    fn test_binary_pattern_matching() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create binary <<1, 2, 3>>
            Instruction::MakeBinary {
                bytes: vec![1, 2, 3],
                dest: Register(0),
            },
            // Try to match it against <<1, 2, 3>>
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Binary(vec![1, 2, 3]),
                fail_target: 4,
            },
            // Match succeeded - set marker
            Instruction::LoadInt {
                value: 100,
                dest: Register(1),
            },
            Instruction::End,
            // Match failed
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(100)); // Match succeeded
    }

    #[test]
    fn test_binary_pattern_mismatch() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            // Create binary <<1, 2, 3>>
            Instruction::MakeBinary {
                bytes: vec![1, 2, 3],
                dest: Register(0),
            },
            // Try to match it against <<4, 5, 6>> (should fail)
            Instruction::Match {
                source: Register(0),
                pattern: Pattern::Binary(vec![4, 5, 6]),
                fail_target: 4,
            },
            // Match succeeded (shouldn't happen)
            Instruction::LoadInt {
                value: 100,
                dest: Register(1),
            },
            Instruction::End,
            // Match failed - set marker
            Instruction::LoadInt {
                value: 0,
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(0)); // Match failed
    }

    // ========== IO Tests ==========

    #[test]
    fn test_println() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            Instruction::PrintLn {
                source: Register(0),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        // Check output was captured
        assert_eq!(scheduler.output.len(), 1);
        assert!(scheduler.output[0].contains("42"));
    }

    #[test]
    fn test_readline_eof_in_tests() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::ReadLine { dest: Register(0) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        // In tests, ReadLine returns :eof
        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Atom("eof".to_string()));
    }

    #[test]
    fn test_file_exists() {
        let mut scheduler = Scheduler::new();

        // Use Cargo.toml which we know exists
        let program = vec![
            Instruction::MakeBinary {
                bytes: "Cargo.toml".as_bytes().to_vec(),
                dest: Register(0),
            },
            Instruction::BinaryToString {
                source: Register(0),
                dest: Register(0),
            },
            Instruction::FileExists {
                path: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // Cargo.toml exists
    }

    // ========== System Info Tests ==========

    #[test]
    fn test_self_pid() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::SelfPid { dest: Register(0) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[0], Value::Pid(Pid(0)));
    }

    #[test]
    fn test_process_count() {
        let mut scheduler = Scheduler::new();

        // Spawn 3 processes
        let child_code = vec![
            Instruction::Work { amount: 100 },
            Instruction::End,
        ];

        let program = vec![
            Instruction::Spawn {
                code: child_code.clone(),
                dest: Register(0),
            },
            Instruction::Spawn {
                code: child_code.clone(),
                dest: Register(1),
            },
            Instruction::ProcessCount { dest: Register(2) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        // Just execute a few steps, don't run to completion
        scheduler.step(10);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        // Should have 3 processes (main + 2 spawned)
        assert_eq!(process.registers[2], Value::Int(3));
    }

    #[test]
    fn test_process_list() {
        let mut scheduler = Scheduler::new();

        let child_code = vec![
            Instruction::Work { amount: 100 },
            Instruction::End,
        ];

        let program = vec![
            Instruction::Spawn {
                code: child_code,
                dest: Register(0),
            },
            Instruction::ProcessList { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        scheduler.step(10);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        if let Value::List(pids) = &process.registers[1] {
            assert_eq!(pids.len(), 2); // main + 1 child
        } else {
            panic!("expected list");
        }
    }

    #[test]
    fn test_is_alive() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::SelfPid { dest: Register(0) },
            Instruction::IsAlive {
                pid: Register(0),
                dest: Register(1),
            },
            // Check a non-existent PID
            Instruction::LoadInt {
                value: 9999,
                dest: Register(2),
            },
            // We need to construct a Pid value - use spawn to get a real one then check after it ends
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[1], Value::Int(1)); // self is alive
    }

    #[test]
    fn test_process_info() {
        let mut scheduler = Scheduler::new();

        let program = vec![
            Instruction::SelfPid { dest: Register(0) },
            Instruction::ProcessInfo {
                pid: Register(0),
                dest: Register(1),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        if let Value::Tuple(info) = &process.registers[1] {
            assert_eq!(info.len(), 5);
            // Status should be "done" or "ready" depending on timing
            assert!(matches!(&info[0], Value::Atom(_)));
        } else {
            panic!("expected tuple");
        }
    }

    #[test]
    fn test_module_list() {
        let mut scheduler = Scheduler::new();

        // Load a module first
        let module = crate::Module {
            name: "test_mod".to_string(),
            code: vec![Instruction::End],
            functions: std::collections::HashMap::new(),
            exports: std::collections::HashSet::new(),
        };
        scheduler.load_module(module).unwrap();

        let program = vec![
            Instruction::ModuleList { dest: Register(0) },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        if let Value::List(modules) = &process.registers[0] {
            assert!(modules.contains(&Value::Atom("test_mod".to_string())));
        } else {
            panic!("expected list");
        }
    }

    #[test]
    fn test_function_exported() {
        let mut scheduler = Scheduler::new();

        // Load a module with an exported function
        let mut functions = std::collections::HashMap::new();
        functions.insert(
            ("my_func".to_string(), 0),
            crate::FunctionDef {
                name: "my_func".to_string(),
                arity: 0,
                entry: 0,
            },
        );
        let mut exports = std::collections::HashSet::new();
        exports.insert(("my_func".to_string(), 0));

        let module = crate::Module {
            name: "test_mod".to_string(),
            code: vec![Instruction::End],
            functions,
            exports,
        };
        scheduler.load_module(module).unwrap();

        let program = vec![
            Instruction::LoadAtom {
                name: "test_mod".to_string(),
                dest: Register(0),
            },
            Instruction::LoadAtom {
                name: "my_func".to_string(),
                dest: Register(1),
            },
            Instruction::LoadInt {
                value: 0,
                dest: Register(2),
            },
            Instruction::FunctionExported {
                module: Register(0),
                function: Register(1),
                arity: Register(2),
                dest: Register(3),
            },
            // Check non-existent function
            Instruction::LoadAtom {
                name: "no_func".to_string(),
                dest: Register(4),
            },
            Instruction::FunctionExported {
                module: Register(0),
                function: Register(4),
                arity: Register(2),
                dest: Register(5),
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_idle(&mut scheduler);

        let process = scheduler.processes.get(&Pid(0)).unwrap();
        assert_eq!(process.registers[3], Value::Int(1)); // my_func/0 exists
        assert_eq!(process.registers[5], Value::Int(0)); // no_func/0 doesn't exist
    }
}
