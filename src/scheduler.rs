//! The scheduler / VM execution engine.

use std::collections::{HashMap, VecDeque};

use crate::{
    DownReason, Instruction, Message, Pid, Process, ProcessStatus, Source, SystemMsg, Value,
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
    use crate::Register;

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
}
