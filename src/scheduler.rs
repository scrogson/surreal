//! The scheduler / VM execution engine.

use std::collections::{HashMap, VecDeque};

use crate::{
    DownReason, Instruction, Message, Operand, Pid, Process, ProcessStatus, Source, SystemMsg,
    Value,
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
    /// Process finished normally
    Done,
    /// Process crashed
    Crash,
}

/// Platform-specific logging
fn log(msg: &str) {
    #[cfg(target_arch = "wasm32")]
    web_sys::console::log_1(&msg.into());

    #[cfg(not(target_arch = "wasm32"))]
    println!("{}", msg);
}

/// The scheduler / VM state
#[derive(Debug)]
pub struct Scheduler {
    pub processes: HashMap<Pid, Process>,
    pub ready_queue: VecDeque<Pid>,
    pub next_pid: u64,
    /// Process registry: name -> pid
    pub registry: HashMap<String, Pid>,
    /// Output buffer from print instructions
    pub output: Vec<String>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            processes: HashMap::new(),
            ready_queue: VecDeque::new(),
            next_pid: 0,
            registry: HashMap::new(),
            output: Vec::new(),
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

        while remaining > 0 {
            // Get next ready process
            let Some(pid) = self.ready_queue.pop_front() else {
                // No ready processes - check if any are waiting with timeouts
                if self.has_pending_timeouts() {
                    return StepResult::Busy; // Keep ticking
                }
                return StepResult::Idle;
            };

            // Run it for up to `remaining` reductions
            let used = self.run_process(pid, remaining);
            remaining = remaining.saturating_sub(used);
        }

        if self.ready_queue.is_empty() && !self.has_pending_timeouts() {
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

    /// Run a single process for up to `budget` reductions
    /// Returns how many reductions were used
    fn run_process(&mut self, pid: Pid, budget: u32) -> u32 {
        let mut used = 0;

        while used < budget {
            let Some(process) = self.processes.get(&pid) else {
                break;
            };

            if process.pc >= process.code.len() {
                // Process finished
                self.finish_process(pid, ProcessStatus::Done);
                break;
            }

            // Clone instruction to avoid borrow issues
            let instruction = process.code[process.pc].clone();

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
                    self.finish_process(pid, ProcessStatus::Done);
                    break;
                }
                ExecResult::Crash => {
                    self.finish_process(pid, ProcessStatus::Crashed);
                    break;
                }
            }
        }

        // If we used full budget but process isn't done, requeue it
        if used >= budget {
            if let Some(p) = self.processes.get(&pid) {
                if p.status == ProcessStatus::Ready && p.pc < p.code.len() {
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

            Instruction::Monitor { target } => {
                let Some(target_pid) = self.resolve_pid(pid, &target) else {
                    return ExecResult::Crash;
                };

                // Add one-way monitor: caller monitors target
                if let Some(t) = self.processes.get_mut(&target_pid) {
                    if !t.monitored_by.contains(&pid) {
                        t.monitored_by.push(pid);
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

            // ========== Arithmetic ==========
            Instruction::LoadInt { value, dest } => {
                if let Some(p) = self.processes.get_mut(&pid) {
                    p.registers[dest.0 as usize] = Value::Int(value);
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
                    // Push return address (next instruction after call)
                    p.call_stack.push(p.pc + 1);
                }
                ExecResult::Jump(target, 1)
            }

            Instruction::Return => {
                let Some(process) = self.processes.get_mut(&pid) else {
                    return ExecResult::Crash;
                };
                if let Some(return_addr) = process.call_stack.pop() {
                    ExecResult::Jump(return_addr, 1)
                } else {
                    // Empty call stack - end the process
                    ExecResult::Done
                }
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
                SystemMsg::Exit(p) => Value::String(format!("EXIT<{}>", p.0)),
                SystemMsg::Crash(p) => Value::String(format!("CRASH<{}>", p.0)),
                SystemMsg::Down(p, reason) => {
                    let reason_str = match reason {
                        DownReason::Normal => "normal",
                        DownReason::Crashed => "crashed",
                    };
                    Value::String(format!("DOWN<{},{}>", p.0, reason_str))
                }
            },
        }
    }

    fn finish_process(&mut self, pid: Pid, status: ProcessStatus) {
        let (links, monitors) = if let Some(p) = self.processes.get_mut(&pid) {
            p.status = status;
            (p.links.clone(), p.monitored_by.clone())
        } else {
            return;
        };

        // Remove any registry entries for this process
        self.registry.retain(|_, v| *v != pid);

        // Notify linked processes (bidirectional)
        let link_msg = match status {
            ProcessStatus::Done => Message::System(SystemMsg::Exit(pid)),
            ProcessStatus::Crashed => Message::System(SystemMsg::Crash(pid)),
            _ => return,
        };

        for linked_pid in links {
            if let Some(linked) = self.processes.get_mut(&linked_pid) {
                linked.mailbox.push_back(link_msg.clone());
                if linked.status == ProcessStatus::Waiting {
                    linked.status = ProcessStatus::Ready;
                    self.ready_queue.push_back(linked_pid);
                }
            }
        }

        // Notify monitoring processes (one-way)
        let down_reason = match status {
            ProcessStatus::Done => DownReason::Normal,
            ProcessStatus::Crashed => DownReason::Crashed,
            _ => return,
        };
        let monitor_msg = Message::System(SystemMsg::Down(pid, down_reason));

        for monitor_pid in monitors {
            if let Some(monitor) = self.processes.get_mut(&monitor_pid) {
                monitor.mailbox.push_back(monitor_msg.clone());
                if monitor.status == ProcessStatus::Waiting {
                    monitor.status = ProcessStatus::Ready;
                    self.ready_queue.push_back(monitor_pid);
                }
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

        // Parent: spawn_link child, receive crash notification
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

        // Parent should have received crash notification
        let parent_process = scheduler.processes.get(&Pid(0)).unwrap();
        match &parent_process.registers[1] {
            Value::String(s) => assert!(s.starts_with("CRASH<")),
            _ => panic!("Expected crash notification string"),
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
            },
            Instruction::Receive { dest: Register(1) },
            Instruction::End,
        ];

        scheduler.spawn(observer);
        run_to_idle(&mut scheduler);

        // Observer should have received DOWN notification
        let observer_process = scheduler.processes.get(&Pid(0)).unwrap();
        match &observer_process.registers[1] {
            Value::String(s) => assert!(s.starts_with("DOWN<") && s.contains("crashed")),
            _ => panic!("Expected DOWN notification string"),
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
}
