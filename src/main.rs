//! ToyBEAM CLI - Run ToyBEAM programs.

use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

use toybeam::{
    compiler::{compile, CoreErlangEmitter, ModuleLoader},
    Instruction, Register, Scheduler, StepResult, Value,
};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        return ExitCode::from(1);
    }

    // Check for --target flag
    if args.len() >= 3 && args[1] == "--target" {
        let target = &args[2];
        if target == "beam" || target == "core" {
            if args.len() < 4 {
                eprintln!("Error: missing source file");
                eprintln!("Usage: toybeam --target beam <file.tb>");
                return ExitCode::from(1);
            }
            return compile_to_core_erlang(&args[3]);
        } else {
            eprintln!("Error: unknown target '{}'", target);
            eprintln!("Available targets: beam, core");
            return ExitCode::from(1);
        }
    }

    // Normal execution mode
    run_program(&args)
}

fn print_usage() {
    eprintln!("Usage: toybeam <file.tb> [function] [args...]");
    eprintln!("       toybeam --target beam <file.tb>");
    eprintln!();
    eprintln!("Targets:");
    eprintln!("  beam, core  Emit Core Erlang (.core file) for BEAM compilation");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  toybeam examples/hello.tb              # Run main/0");
    eprintln!("  toybeam examples/math.tb factorial 5   # Run factorial(5)");
    eprintln!("  toybeam --target beam examples/math.tb # Emit math.core");
}

fn compile_to_core_erlang(filename: &str) -> ExitCode {
    let input_path = Path::new(filename);

    // Use ModuleLoader for multi-file projects
    let mut loader = ModuleLoader::new();
    let main_module = match loader.load_project(input_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Error: {}", e);
            return ExitCode::from(1);
        }
    };

    // Emit Core Erlang for all loaded modules
    let mut output_files = Vec::new();

    for module in loader.modules() {
        let mut emitter = CoreErlangEmitter::new();
        let core_erlang = match emitter.emit_module(module) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Compile error in {}: {}", module.name, e);
                return ExitCode::from(1);
            }
        };

        let output_filename = format!("{}.core", module.name);
        match fs::write(&output_filename, &core_erlang) {
            Ok(_) => {
                println!("Wrote {}", output_filename);
                output_files.push(output_filename);
            }
            Err(e) => {
                eprintln!("Error writing {}.core: {}", module.name, e);
                return ExitCode::from(1);
            }
        }
    }

    if !output_files.is_empty() {
        println!();
        println!("To compile to BEAM:");
        println!("  erlc +from_core {}", output_files.join(" "));
    }

    // Return the main module name for reference
    let _ = main_module;
    ExitCode::SUCCESS
}

fn run_program(args: &[String]) -> ExitCode {
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
        Value::BigInt(n) => n.to_string(),
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
        Value::Fun {
            module,
            function,
            arity,
        } => {
            format!("&{}:{}/{}", module, function, arity)
        }
        Value::Closure {
            module,
            function,
            arity,
            ..
        } => {
            format!("#Closure<{}:{}/{}>", module, function, arity)
        }
        Value::None => "none".to_string(),
    }
}
