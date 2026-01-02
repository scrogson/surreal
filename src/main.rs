use toybeam::{Instruction, Register, Scheduler, Source, StepResult};

fn main() {
    let mut scheduler = Scheduler::new();

    // Demo: process registry with proper synchronization

    let server = vec![
        Instruction::Register { name: "myserver".into() },
        Instruction::Send { to: Source::Parent, msg: "ready".into() },
        Instruction::Receive { dest: Register(0) },
        Instruction::Print { source: Source::Reg(Register(0)) },
        Instruction::Send { to: Source::Parent, msg: "handled!".into() },
        Instruction::End,
    ];

    let client = vec![
        Instruction::Spawn { code: server, dest: Register(0) },
        Instruction::Receive { dest: Register(1) },  // wait for "ready"
        Instruction::Send { to: Source::Named("myserver".into()), msg: "ping".into() },
        Instruction::Receive { dest: Register(1) },
        Instruction::Print { source: Source::Reg(Register(1)) },
        Instruction::End,
    ];

    scheduler.spawn(client);

    // Run until idle
    loop {
        let result = scheduler.step(10);
        println!("Step result: {:?}", result);

        if result == StepResult::Idle {
            break;
        }
    }

    println!("\nFinal state:");
    for (pid, process) in &scheduler.processes {
        println!("  {:?}: {:?}", pid, process.status);
    }
}
