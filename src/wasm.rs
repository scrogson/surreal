//! WASM bindings - provides a JS-friendly API.

use js_sys::{Array, Object, Reflect};
use wasm_bindgen::prelude::{wasm_bindgen, JsError, JsValue};

use crate::{Instruction, Operand, Pid, Register, Scheduler, Source, StepResult};

/// JS-friendly wrapper around the scheduler
#[wasm_bindgen]
pub struct VM {
    scheduler: Scheduler,
}

#[wasm_bindgen]
impl VM {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            scheduler: Scheduler::new(),
        }
    }

    /// Spawn a process from a program (array of instruction objects)
    #[wasm_bindgen]
    pub fn spawn(&mut self, program: JsValue) -> Result<u64, JsError> {
        let instructions = parse_program(program)?;
        let pid = self.scheduler.spawn(instructions);
        Ok(pid.0)
    }

    /// Run up to `budget` reductions
    /// Returns "busy" or "idle"
    #[wasm_bindgen]
    pub fn step(&mut self, budget: u32) -> String {
        match self.scheduler.step(budget) {
            StepResult::Busy => "busy".into(),
            StepResult::Idle => "idle".into(),
        }
    }

    /// Run until idle
    #[wasm_bindgen]
    pub fn run(&mut self) {
        loop {
            if self.scheduler.step(1000) == StepResult::Idle {
                break;
            }
        }
    }

    /// Get process counts as [ready, waiting, done, crashed]
    #[wasm_bindgen]
    pub fn stats(&self) -> Vec<usize> {
        let (r, w, d, c) = self.scheduler.process_count();
        vec![r, w, d, c]
    }

    /// Get total process count
    #[wasm_bindgen]
    pub fn process_count(&self) -> usize {
        self.scheduler.processes.len()
    }

    /// Get and clear output buffer
    #[wasm_bindgen]
    pub fn take_output(&mut self) -> Vec<String> {
        self.scheduler.take_output()
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse a JS program description into instructions
fn parse_program(program: JsValue) -> Result<Vec<Instruction>, JsError> {
    let array = Array::from(&program);
    let mut instructions = Vec::with_capacity(array.length() as usize);

    for i in 0..array.length() {
        let item = array.get(i);
        let obj = Object::from(item);

        let op = Reflect::get(&obj, &"op".into())
            .map_err(|_| JsError::new("missing 'op' field"))?
            .as_string()
            .ok_or_else(|| JsError::new("'op' must be a string"))?;

        let instruction = match op.as_str() {
            "end" => Instruction::End,

            "work" => {
                let amount = Reflect::get(&obj, &"amount".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(1.0) as u32;
                Instruction::Work { amount }
            }

            "spawn" => {
                let code_val = Reflect::get(&obj, &"code".into())
                    .map_err(|_| JsError::new("spawn: missing 'code'"))?;
                let code = parse_program(code_val)?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Spawn { code, dest }
            }

            "spawn_link" => {
                let code_val = Reflect::get(&obj, &"code".into())
                    .map_err(|_| JsError::new("spawn_link: missing 'code'"))?;
                let code = parse_program(code_val)?;
                let dest = get_register(&obj, "dest")?;
                Instruction::SpawnLink { code, dest }
            }

            "send" => {
                let to = get_source(&obj, "to")?;
                let msg = Reflect::get(&obj, &"msg".into())
                    .ok()
                    .and_then(|v| v.as_string())
                    .unwrap_or_default();
                Instruction::Send { to, msg }
            }

            "receive" => {
                let dest = get_register(&obj, "dest")?;
                Instruction::Receive { dest }
            }

            "receive_timeout" => {
                let dest = get_register(&obj, "dest")?;
                let timeout = Reflect::get(&obj, &"timeout".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(10.0) as u32;
                Instruction::ReceiveTimeout { dest, timeout }
            }

            "link" => {
                let target = get_source(&obj, "target")?;
                Instruction::Link { target }
            }

            "monitor" => {
                let target = get_source(&obj, "target")?;
                Instruction::Monitor { target }
            }

            "register" => {
                let name = Reflect::get(&obj, &"name".into())
                    .ok()
                    .and_then(|v| v.as_string())
                    .unwrap_or_default();
                Instruction::Register { name }
            }

            "unregister" => {
                let name = Reflect::get(&obj, &"name".into())
                    .ok()
                    .and_then(|v| v.as_string())
                    .unwrap_or_default();
                Instruction::Unregister { name }
            }

            "whereis" => {
                let name = Reflect::get(&obj, &"name".into())
                    .ok()
                    .and_then(|v| v.as_string())
                    .unwrap_or_default();
                let dest = get_register(&obj, "dest")?;
                Instruction::WhereIs { name, dest }
            }

            "print" => {
                let source = get_source(&obj, "source")?;
                Instruction::Print { source }
            }

            "crash" => Instruction::Crash,

            // Arithmetic
            "load_int" => {
                let value = Reflect::get(&obj, &"value".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0) as i64;
                let dest = get_register(&obj, "dest")?;
                Instruction::LoadInt { value, dest }
            }

            "add" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Add { a, b, dest }
            }

            "sub" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Sub { a, b, dest }
            }

            "mul" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Mul { a, b, dest }
            }

            "div" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Div { a, b, dest }
            }

            "mod" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Mod { a, b, dest }
            }

            // Comparisons
            "eq" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Eq { a, b, dest }
            }

            "ne" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Ne { a, b, dest }
            }

            "lt" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Lt { a, b, dest }
            }

            "lte" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Lte { a, b, dest }
            }

            "gt" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Gt { a, b, dest }
            }

            "gte" => {
                let a = get_operand(&obj, "a")?;
                let b = get_operand(&obj, "b")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::Gte { a, b, dest }
            }

            // Control flow
            "jump" => {
                let target = Reflect::get(&obj, &"target".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0) as usize;
                Instruction::Jump { target }
            }

            "jump_if" => {
                let cond = get_operand(&obj, "cond")?;
                let target = Reflect::get(&obj, &"target".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0) as usize;
                Instruction::JumpIf { cond, target }
            }

            "jump_unless" => {
                let cond = get_operand(&obj, "cond")?;
                let target = Reflect::get(&obj, &"target".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0) as usize;
                Instruction::JumpUnless { cond, target }
            }

            "call" => {
                let target = Reflect::get(&obj, &"target".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0) as usize;
                Instruction::Call { target }
            }

            "return" => Instruction::Return,

            // Stack operations
            "push" => {
                let source = get_operand(&obj, "source")?;
                Instruction::Push { source }
            }

            "pop" => {
                let dest = get_register(&obj, "dest")?;
                Instruction::Pop { dest }
            }

            // Atoms & Tuples
            "load_atom" => {
                let name = Reflect::get(&obj, &"name".into())
                    .ok()
                    .and_then(|v| v.as_string())
                    .unwrap_or_default();
                let dest = get_register(&obj, "dest")?;
                Instruction::LoadAtom { name, dest }
            }

            "make_tuple" => {
                let arity = Reflect::get(&obj, &"arity".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0) as u8;
                let dest = get_register(&obj, "dest")?;
                Instruction::MakeTuple { arity, dest }
            }

            "tuple_element" => {
                let tuple = get_register(&obj, "tuple")?;
                let index = Reflect::get(&obj, &"index".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0) as u8;
                let dest = get_register(&obj, "dest")?;
                Instruction::TupleElement { tuple, index, dest }
            }

            "tuple_arity" => {
                let tuple = get_register(&obj, "tuple")?;
                let dest = get_register(&obj, "dest")?;
                Instruction::TupleArity { tuple, dest }
            }

            other => return Err(JsError::new(&format!("unknown op: {}", other))),
        };

        instructions.push(instruction);
    }

    Ok(instructions)
}

fn get_register(obj: &Object, field: &str) -> Result<Register, JsError> {
    let val = Reflect::get(obj, &field.into())
        .ok()
        .and_then(|v| v.as_f64())
        .unwrap_or(0.0) as u8;

    Ok(Register(val))
}

fn get_source(obj: &Object, field: &str) -> Result<Source, JsError> {
    let val = Reflect::get(obj, &field.into())
        .map_err(|_| JsError::new(&format!("missing '{}'", field)))?;

    // Check if it's a string like "self" or "parent"
    if let Some(s) = val.as_string() {
        match s.as_str() {
            "self" => return Ok(Source::Self_),
            "parent" => return Ok(Source::Parent),
            _ => {}
        }
    }

    // Check if it's an object with type field
    if val.is_object() {
        let obj = Object::from(val);
        let type_val = Reflect::get(&obj, &"type".into()).ok();

        if let Some(t) = type_val.and_then(|v| v.as_string()) {
            match t.as_str() {
                "reg" => {
                    let idx = Reflect::get(&obj, &"reg".into())
                        .ok()
                        .and_then(|v| v.as_f64())
                        .unwrap_or(0.0) as u8;
                    return Ok(Source::Reg(Register(idx)));
                }
                "pid" => {
                    let id = Reflect::get(&obj, &"pid".into())
                        .ok()
                        .and_then(|v| v.as_f64())
                        .unwrap_or(0.0) as u64;
                    return Ok(Source::Pid(Pid(id)));
                }
                "self" => return Ok(Source::Self_),
                "parent" => return Ok(Source::Parent),
                "named" => {
                    let name = Reflect::get(&obj, &"name".into())
                        .ok()
                        .and_then(|v| v.as_string())
                        .unwrap_or_default();
                    return Ok(Source::Named(name));
                }
                _ => {}
            }
        }
    }

    // Default to register 0
    Ok(Source::Reg(Register(0)))
}

/// Parse an operand from a JS object field.
/// Operands can be:
/// - A number (immediate integer): `{ a: 42 }`
/// - An object with type "reg": `{ a: { type: "reg", reg: 0 } }`
fn get_operand(obj: &Object, field: &str) -> Result<Operand, JsError> {
    let val = Reflect::get(obj, &field.into())
        .map_err(|_| JsError::new(&format!("missing '{}'", field)))?;

    // Check if it's a number (immediate value)
    if let Some(n) = val.as_f64() {
        return Ok(Operand::Int(n as i64));
    }

    // Check if it's an object with type field
    if val.is_object() {
        let obj = Object::from(val);
        let type_val = Reflect::get(&obj, &"type".into()).ok();

        if let Some(t) = type_val.and_then(|v| v.as_string()) {
            if t == "reg" {
                let idx = Reflect::get(&obj, &"reg".into())
                    .ok()
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0) as u8;
                return Ok(Operand::Reg(Register(idx)));
            }
        }
    }

    // Default to immediate 0
    Ok(Operand::Int(0))
}
