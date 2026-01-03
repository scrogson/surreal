//! ToyBEAM CLI - Run ToyBEAM programs.

use std::env;
use std::fs;
use std::process::ExitCode;

use toybeam::{compiler::compile, Instruction, Register, Scheduler, StepResult, Value};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: toybeam <file.tb> [function] [args...]");
        eprintln!();
        eprintln!("Examples:");
        eprintln!("  toybeam examples/hello.tb");
        eprintln!("  toybeam examples/math.tb factorial 5");
        return ExitCode::from(1);
    }

    let filename = &args[1];

    // Read source file
    let source = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", filename, e);
            return ExitCode::from(1);
        }
    };

    // Compile
    let module = match compile(&source) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Compile error: {}", e);
            return ExitCode::from(1);
        }
    };

    let module_name = module.name.clone();

    // Determine function to call
    let (func_name, func_args) = if args.len() >= 3 {
        let func = args[2].clone();
        let func_args: Vec<i64> = args[3..]
            .iter()
            .filter_map(|s| s.parse().ok())
            .collect();
        (func, func_args)
    } else {
        // Default to main/0
        ("main".to_string(), vec![])
    };

    let arity = func_args.len() as u8;

    // Check function exists
    if module.get_function(&func_name, arity).is_none() {
        eprintln!(
            "Error: function {}/{}  not found in module {}",
            func_name, arity, module_name
        );
        eprintln!();
        eprintln!("Available functions:");
        for ((name, ar), _) in &module.functions {
            if module.is_exported(name, *ar) {
                eprintln!("  {}/{}", name, ar);
            }
        }
        return ExitCode::from(1);
    }

    // Load module
    let mut scheduler = Scheduler::new();
    if let Err(e) = scheduler.load_module(module) {
        eprintln!("Error loading module: {}", e);
        return ExitCode::from(1);
    }

    // Build program to call the function
    let mut program = Vec::new();

    // Load arguments into registers
    for (i, arg) in func_args.iter().enumerate() {
        program.push(Instruction::LoadInt {
            value: *arg,
            dest: Register(i as u8),
        });
    }

    // Call the function
    program.push(Instruction::CallMFA {
        module: module_name,
        function: func_name,
        arity,
    });

    program.push(Instruction::End);

    // Spawn and run
    scheduler.spawn(program);

    loop {
        let result = scheduler.step(1000);
        if result == StepResult::Idle {
            break;
        }
    }

    // Get result from R0
    if let Some(process) = scheduler.processes.values().next() {
        let result = &process.registers[0];
        println!("{}", format_value(result));
    }

    ExitCode::SUCCESS
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Int(n) => n.to_string(),
        Value::Float(f) => format!("{}", f),
        Value::Atom(a) => format!(":{}", a),
        Value::String(s) => format!("\"{}\"", s),
        Value::Binary(bytes) => {
            let inner: Vec<String> = bytes.iter().map(|b| b.to_string()).collect();
            format!("<<{}>>", inner.join(", "))
        }
        Value::Pid(p) => format!("#PID<{}>", p.0),
        Value::Ref(r) => format!("#Ref<{}>", r),
        Value::Tuple(elems) => {
            let inner: Vec<String> = elems.iter().map(format_value).collect();
            format!("{{{}}}", inner.join(", "))
        }
        Value::List(elems) => {
            let inner: Vec<String> = elems.iter().map(format_value).collect();
            format!("[{}]", inner.join(", "))
        }
        Value::Map(entries) => {
            let inner: Vec<String> = entries
                .iter()
                .map(|(k, v)| format!("{} => {}", format_value(k), format_value(v)))
                .collect();
            format!("%{{{}}}", inner.join(", "))
        }
        Value::Fun { module, function, arity } => {
            format!("&{}:{}/{}", module, function, arity)
        }
        Value::Closure { module, function, arity, .. } => {
            format!("#Closure<{}:{}/{}>", module, function, arity)
        }
        Value::None => "none".to_string(),
    }
}
