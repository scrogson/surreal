//! Code generation from AST to ToyBEAM bytecode.

use std::collections::HashMap;

use crate::compiler::ast::{
    BinOp, Block, Expr, Function, Item, Module as AstModule, Pattern as AstPattern, Stmt, UnaryOp,
};
use crate::instruction::{Instruction, Operand, Pattern as VmPattern, Register, Source};
use crate::Module;

/// Code generation error.
#[derive(Debug, Clone)]
pub struct CodegenError {
    pub message: String,
}

impl CodegenError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "codegen error: {}", self.message)
    }
}

impl std::error::Error for CodegenError {}

pub type CodegenResult<T> = Result<T, CodegenError>;

/// Simple register allocator.
struct RegisterAllocator {
    /// Next available register.
    next_reg: u8,
    /// Variable bindings: name → register.
    locals: HashMap<String, u8>,
    /// Saved register state for scopes.
    saved_states: Vec<(u8, HashMap<String, u8>)>,
}

impl RegisterAllocator {
    fn new() -> Self {
        Self {
            next_reg: 0,
            locals: HashMap::new(),
            saved_states: Vec::new(),
        }
    }

    /// Reset for a new function.
    fn reset(&mut self) {
        self.next_reg = 0;
        self.locals.clear();
        self.saved_states.clear();
    }

    /// Allocate a register.
    fn alloc(&mut self) -> Register {
        let reg = Register(self.next_reg);
        self.next_reg += 1;
        reg
    }

    /// Bind a variable to a register.
    fn bind(&mut self, name: &str, reg: u8) {
        self.locals.insert(name.to_string(), reg);
    }

    /// Look up a variable's register.
    fn lookup(&self, name: &str) -> Option<Register> {
        self.locals.get(name).map(|&r| Register(r))
    }

    /// Save current state (for nested scopes).
    fn save(&mut self) {
        self.saved_states
            .push((self.next_reg, self.locals.clone()));
    }

    /// Restore previous state.
    fn restore(&mut self) {
        if let Some((next, locals)) = self.saved_states.pop() {
            self.next_reg = next;
            self.locals = locals;
        }
    }
}

/// Code generator.
pub struct Codegen {
    /// Module name.
    module_name: String,
    /// Emitted instructions.
    code: Vec<Instruction>,
    /// Function entry points.
    functions: HashMap<(String, u8), usize>,
    /// Exported functions.
    exports: Vec<(String, u8)>,
    /// Register allocator.
    regs: RegisterAllocator,
    /// Labels that need patching (instruction index → placeholder).
    pending_jumps: Vec<usize>,
}

impl Codegen {
    /// Create a new code generator.
    pub fn new() -> Self {
        Self {
            module_name: String::new(),
            code: Vec::new(),
            functions: HashMap::new(),
            exports: Vec::new(),
            regs: RegisterAllocator::new(),
            pending_jumps: Vec::new(),
        }
    }

    /// Compile an AST module to a VM module.
    pub fn compile_module(ast: &AstModule) -> CodegenResult<Module> {
        let mut codegen = Codegen::new();
        codegen.module_name = ast.name.clone();

        // Compile all functions
        for item in &ast.items {
            match item {
                Item::Function(func) => {
                    codegen.compile_function(func)?;
                }
                Item::Struct(_) | Item::Enum(_) => {
                    // Structs and enums don't generate code directly
                    // They're used for pattern matching at runtime
                }
            }
        }

        // Build the VM module
        let mut module = Module::new(ast.name.clone());
        module.code = codegen.code;

        for ((name, arity), entry) in codegen.functions {
            module.add_function_at(name.clone(), arity, entry);

            // Check if exported
            if codegen.exports.contains(&(name.clone(), arity)) {
                module.export(&name, arity);
            }
        }

        Ok(module)
    }

    /// Compile a function.
    fn compile_function(&mut self, func: &Function) -> CodegenResult<()> {
        let entry = self.code.len();
        let arity = func.params.len() as u8;

        // Reset register allocator for this function
        self.regs.reset();

        // Bind parameters to registers R0, R1, ...
        for (i, param) in func.params.iter().enumerate() {
            self.bind_pattern(&param.pattern, Register(i as u8))?;
            self.regs.next_reg = (i + 1) as u8;
        }

        // Compile function body
        let result_reg = self.compile_block(&func.body)?;

        // Move result to R0 if not already there
        if result_reg.0 != 0 {
            self.emit(Instruction::Move {
                source: result_reg,
                dest: Register(0),
            });
        }

        // Emit return
        self.emit(Instruction::Return);

        // Record function
        self.functions
            .insert((func.name.clone(), arity), entry);

        if func.is_pub {
            self.exports.push((func.name.clone(), arity));
        }

        Ok(())
    }

    /// Compile a block, returning the register containing the result.
    fn compile_block(&mut self, block: &Block) -> CodegenResult<Register> {
        self.regs.save();

        // Compile statements
        for stmt in &block.stmts {
            self.compile_stmt(stmt)?;
        }

        // Compile trailing expression or return unit
        let result = if let Some(expr) = &block.expr {
            self.compile_expr(expr)?
        } else {
            // Return unit (represented as empty tuple or 0)
            let reg = self.regs.alloc();
            self.emit(Instruction::LoadInt { value: 0, dest: reg });
            reg
        };

        self.regs.restore();
        Ok(result)
    }

    /// Check if an expression contains a function call (which could clobber registers).
    fn contains_call(expr: &Expr) -> bool {
        match expr {
            Expr::Call { .. } | Expr::Spawn(_) | Expr::SpawnClosure(_) => true,
            Expr::Binary { left, right, .. } => Self::contains_call(left) || Self::contains_call(right),
            Expr::Unary { expr, .. } => Self::contains_call(expr),
            Expr::If { cond, then_block, else_block } => {
                Self::contains_call(cond)
                    || then_block.stmts.iter().any(|s| match s {
                        Stmt::Let { value, .. } => Self::contains_call(value),
                        Stmt::Expr(e) => Self::contains_call(e),
                    })
                    || then_block.expr.as_ref().map_or(false, |e| Self::contains_call(e))
                    || else_block.as_ref().map_or(false, |b| {
                        b.stmts.iter().any(|s| match s {
                            Stmt::Let { value, .. } => Self::contains_call(value),
                            Stmt::Expr(e) => Self::contains_call(e),
                        }) || b.expr.as_ref().map_or(false, |e| Self::contains_call(e))
                    })
            }
            Expr::Match { expr, arms } => {
                Self::contains_call(expr)
                    || arms.iter().any(|arm| Self::contains_call(&arm.body))
            }
            Expr::Block(block) => {
                block.stmts.iter().any(|s| match s {
                    Stmt::Let { value, .. } => Self::contains_call(value),
                    Stmt::Expr(e) => Self::contains_call(e),
                }) || block.expr.as_ref().map_or(false, |e| Self::contains_call(e))
            }
            Expr::Tuple(elems) | Expr::List(elems) => elems.iter().any(|e| Self::contains_call(e)),
            Expr::StructInit { fields, .. } => fields.iter().any(|(_, e)| Self::contains_call(e)),
            Expr::EnumVariant { args, .. } => args.iter().any(|e| Self::contains_call(e)),
            Expr::FieldAccess { expr, .. } => Self::contains_call(expr),
            Expr::MethodCall { receiver, args, .. } => {
                Self::contains_call(receiver) || args.iter().any(|a| Self::contains_call(a))
            }
            Expr::Send { to, msg } => Self::contains_call(to) || Self::contains_call(msg),
            Expr::Return(Some(e)) => Self::contains_call(e),
            _ => false,
        }
    }

    /// Compile a statement.
    fn compile_stmt(&mut self, stmt: &Stmt) -> CodegenResult<()> {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                let value_reg = self.compile_expr(value)?;
                self.bind_pattern(pattern, value_reg)?;
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                Ok(())
            }
        }
    }

    /// Compile an expression, returning the register containing the result.
    fn compile_expr(&mut self, expr: &Expr) -> CodegenResult<Register> {
        match expr {
            Expr::Int(n) => {
                let dest = self.regs.alloc();
                self.emit(Instruction::LoadInt { value: *n, dest });
                Ok(dest)
            }

            Expr::String(s) => {
                // Strings are represented as Value::String, but we don't have
                // a LoadString instruction. For now, use a workaround with atoms.
                // TODO: Add LoadString instruction
                let dest = self.regs.alloc();
                self.emit(Instruction::LoadAtom {
                    name: format!("__str_{}", s.replace(' ', "_")),
                    dest,
                });
                Ok(dest)
            }

            Expr::Atom(a) => {
                let dest = self.regs.alloc();
                self.emit(Instruction::LoadAtom {
                    name: a.clone(),
                    dest,
                });
                Ok(dest)
            }

            Expr::Bool(b) => {
                let dest = self.regs.alloc();
                self.emit(Instruction::LoadInt {
                    value: if *b { 1 } else { 0 },
                    dest,
                });
                Ok(dest)
            }

            Expr::Ident(name) => {
                if let Some(reg) = self.regs.lookup(name) {
                    Ok(reg)
                } else if name == "self" {
                    // self() returns current process PID
                    let dest = self.regs.alloc();
                    self.emit(Instruction::Print {
                        source: Source::Self_,
                    });
                    // TODO: Need a proper instruction to get self PID into register
                    Ok(dest)
                } else {
                    Err(CodegenError::new(format!("undefined variable: {}", name)))
                }
            }

            Expr::Binary { op, left, right } => {
                let left_reg = self.compile_expr(left)?;

                // If right side contains a call, save left to stack (calls clobber registers)
                let needs_stack_save = Self::contains_call(right);
                if needs_stack_save {
                    self.emit(Instruction::Push {
                        source: Operand::Reg(left_reg),
                    });
                }

                let right_reg = self.compile_expr(right)?;

                // Restore left operand from stack if needed
                let actual_left = if needs_stack_save {
                    let restored = self.regs.alloc();
                    self.emit(Instruction::Pop { dest: restored });
                    restored
                } else {
                    left_reg
                };

                let dest = self.regs.alloc();

                let instr = match op {
                    BinOp::Add => Instruction::Add {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Sub => Instruction::Sub {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Mul => Instruction::Mul {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Div => Instruction::Div {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Mod => Instruction::Mod {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Eq => Instruction::Eq {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Ne => Instruction::Ne {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Lt => Instruction::Lt {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Le => Instruction::Lte {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Gt => Instruction::Gt {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::Ge => Instruction::Gte {
                        a: Operand::Reg(actual_left),
                        b: Operand::Reg(right_reg),
                        dest,
                    },
                    BinOp::And => {
                        // Logical AND: both must be truthy (non-zero)
                        // Result: 1 if both non-zero, 0 otherwise
                        // For now, simple multiplication works for 0/1 values
                        Instruction::Mul {
                            a: Operand::Reg(actual_left),
                            b: Operand::Reg(right_reg),
                            dest,
                        }
                    }
                    BinOp::Or => {
                        // Logical OR: either must be truthy
                        // For now, use addition and then normalize
                        // TODO: Proper short-circuit evaluation
                        Instruction::Add {
                            a: Operand::Reg(actual_left),
                            b: Operand::Reg(right_reg),
                            dest,
                        }
                    }
                };

                self.emit(instr);
                Ok(dest)
            }

            Expr::Unary { op, expr } => {
                let expr_reg = self.compile_expr(expr)?;
                let dest = self.regs.alloc();

                match op {
                    UnaryOp::Neg => {
                        self.emit(Instruction::Sub {
                            a: Operand::Int(0),
                            b: Operand::Reg(expr_reg),
                            dest,
                        });
                    }
                    UnaryOp::Not => {
                        // !x = 1 if x == 0, else 0
                        self.emit(Instruction::Eq {
                            a: Operand::Reg(expr_reg),
                            b: Operand::Int(0),
                            dest,
                        });
                    }
                }

                Ok(dest)
            }

            Expr::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_reg = self.compile_expr(cond)?;
                let result_reg = self.regs.alloc();

                // JumpUnless to else branch
                let jump_to_else = self.code.len();
                self.emit(Instruction::JumpUnless {
                    cond: Operand::Reg(cond_reg),
                    target: 0, // Placeholder
                });

                // Then branch
                let then_result = self.compile_block(then_block)?;
                self.emit(Instruction::Move {
                    source: then_result,
                    dest: result_reg,
                });

                let jump_to_end = self.code.len();
                self.emit(Instruction::Jump { target: 0 }); // Placeholder

                // Else branch
                let else_start = self.code.len();
                self.patch_jump(jump_to_else, else_start);

                if let Some(else_blk) = else_block {
                    let else_result = self.compile_block(else_blk)?;
                    self.emit(Instruction::Move {
                        source: else_result,
                        dest: result_reg,
                    });
                } else {
                    // No else: result is unit (0)
                    self.emit(Instruction::LoadInt {
                        value: 0,
                        dest: result_reg,
                    });
                }

                let end = self.code.len();
                self.patch_jump(jump_to_end, end);

                Ok(result_reg)
            }

            Expr::Match { expr, arms } => {
                let expr_reg = self.compile_expr(expr)?;
                let result_reg = self.regs.alloc();
                let mut end_jumps = Vec::new();

                // Save register state before processing arms
                // Each arm can allocate temporaries that should be reset for the next arm
                let base_next_reg = self.regs.next_reg;
                let base_locals = self.regs.locals.clone();

                for (i, arm) in arms.iter().enumerate() {
                    let is_last = i == arms.len() - 1;
                    let fail_target = if is_last {
                        // Last arm: crash if no match
                        self.code.len() + 2 // Skip Match and body
                    } else {
                        0 // Placeholder, will be patched
                    };

                    // Compile pattern
                    let vm_pattern = self.compile_pattern(&arm.pattern)?;
                    let match_idx = self.code.len();
                    self.emit(Instruction::Match {
                        source: expr_reg,
                        pattern: vm_pattern,
                        fail_target,
                    });

                    // Compile guard if present
                    if let Some(guard) = &arm.guard {
                        let guard_reg = self.compile_expr(guard)?;
                        let guard_jump = self.code.len();
                        self.emit(Instruction::JumpUnless {
                            cond: Operand::Reg(guard_reg),
                            target: 0, // Placeholder for next arm
                        });
                        self.pending_jumps.push(guard_jump);
                    }

                    // Compile body
                    let body_result = self.compile_expr(&arm.body)?;
                    self.emit(Instruction::Move {
                        source: body_result,
                        dest: result_reg,
                    });

                    // Jump to end
                    let jump_idx = self.code.len();
                    self.emit(Instruction::Jump { target: 0 });
                    end_jumps.push(jump_idx);

                    // Patch fail target for non-last arms
                    if !is_last {
                        let next_arm = self.code.len();
                        self.patch_match_fail(match_idx, next_arm);

                        // Patch guard jump if present
                        if arm.guard.is_some() {
                            if let Some(guard_jump) = self.pending_jumps.pop() {
                                self.patch_jump(guard_jump, next_arm);
                            }
                        }

                        // Reset register allocation for next arm
                        // Keep result_reg but reset temporaries and restore base locals
                        self.regs.next_reg = base_next_reg;
                        self.regs.locals = base_locals.clone();
                    }
                }

                // Add crash instruction for exhaustive match failure
                self.emit(Instruction::Crash);

                // Patch all end jumps
                let end = self.code.len();
                for jump_idx in end_jumps {
                    self.patch_jump(jump_idx, end);
                }

                Ok(result_reg)
            }

            Expr::Block(block) => self.compile_block(block),

            Expr::Tuple(elements) => {
                // Push elements onto stack, then MakeTuple
                for elem in elements {
                    let elem_reg = self.compile_expr(elem)?;
                    self.emit(Instruction::Push {
                        source: Operand::Reg(elem_reg),
                    });
                }

                let dest = self.regs.alloc();
                self.emit(Instruction::MakeTuple {
                    arity: elements.len() as u8,
                    dest,
                });

                Ok(dest)
            }

            Expr::List(elements) => {
                if elements.is_empty() {
                    let dest = self.regs.alloc();
                    self.emit(Instruction::MakeList { length: 0, dest });
                    Ok(dest)
                } else {
                    // Push elements onto stack, then MakeList
                    for elem in elements {
                        let elem_reg = self.compile_expr(elem)?;
                        self.emit(Instruction::Push {
                            source: Operand::Reg(elem_reg),
                        });
                    }

                    let dest = self.regs.alloc();
                    self.emit(Instruction::MakeList {
                        length: elements.len() as u8,
                        dest,
                    });

                    Ok(dest)
                }
            }

            Expr::StructInit { name, fields } => {
                // Represent as tagged tuple: {:StructName, field1, field2, ...}
                // First push the tag atom
                let tag_reg = self.regs.alloc();
                self.emit(Instruction::LoadAtom {
                    name: name.clone(),
                    dest: tag_reg,
                });
                self.emit(Instruction::Push {
                    source: Operand::Reg(tag_reg),
                });

                // Push fields in order
                for (_, value) in fields {
                    let value_reg = self.compile_expr(value)?;
                    self.emit(Instruction::Push {
                        source: Operand::Reg(value_reg),
                    });
                }

                let dest = self.regs.alloc();
                self.emit(Instruction::MakeTuple {
                    arity: (fields.len() + 1) as u8, // +1 for tag
                    dest,
                });

                Ok(dest)
            }

            Expr::EnumVariant {
                type_name: _,
                variant,
                args,
            } => {
                if args.is_empty() {
                    // Unit variant: just an atom
                    let dest = self.regs.alloc();
                    self.emit(Instruction::LoadAtom {
                        name: variant.clone(),
                        dest,
                    });
                    Ok(dest)
                } else {
                    // Tuple variant: {:Variant, arg1, arg2, ...}
                    let tag_reg = self.regs.alloc();
                    self.emit(Instruction::LoadAtom {
                        name: variant.clone(),
                        dest: tag_reg,
                    });
                    self.emit(Instruction::Push {
                        source: Operand::Reg(tag_reg),
                    });

                    for arg in args {
                        let arg_reg = self.compile_expr(arg)?;
                        self.emit(Instruction::Push {
                            source: Operand::Reg(arg_reg),
                        });
                    }

                    let dest = self.regs.alloc();
                    self.emit(Instruction::MakeTuple {
                        arity: (args.len() + 1) as u8, // +1 for tag
                        dest,
                    });

                    Ok(dest)
                }
            }

            Expr::FieldAccess { expr, field } => {
                // For now, assume expr is a struct (tagged tuple)
                // Field access becomes tuple element access
                // TODO: Need struct metadata to know field indices
                let _ = self.compile_expr(expr)?;
                Err(CodegenError::new(format!(
                    "field access not yet implemented: .{}",
                    field
                )))
            }

            Expr::Call { func, args } => {
                // Save current registers before call
                let saved_next = self.regs.next_reg;

                // Compile arguments into R0, R1, ...
                for (i, arg) in args.iter().enumerate() {
                    let arg_reg = self.compile_expr(arg)?;
                    if arg_reg.0 != i as u8 {
                        // Move to correct register
                        self.emit(Instruction::Move {
                            source: arg_reg,
                            dest: Register(i as u8),
                        });
                    }
                }

                // Determine call target
                match func.as_ref() {
                    Expr::Ident(name) => {
                        // Local function call
                        self.emit(Instruction::CallLocal {
                            function: name.clone(),
                            arity: args.len() as u8,
                        });
                    }
                    Expr::Path { segments } => {
                        if segments.len() == 2 {
                            // Module::function call
                            self.emit(Instruction::CallMFA {
                                module: segments[0].clone(),
                                function: segments[1].clone(),
                                arity: args.len() as u8,
                            });
                        } else {
                            return Err(CodegenError::new("invalid call path"));
                        }
                    }
                    _ => {
                        return Err(CodegenError::new("unsupported call target"));
                    }
                }

                // Result is in R0
                self.regs.next_reg = saved_next.max(1);
                Ok(Register(0))
            }

            Expr::MethodCall { .. } => {
                Err(CodegenError::new("method calls not yet implemented"))
            }

            Expr::Path { segments } => {
                // Path expression (like Module::function) - return as identifier for now
                if segments.len() == 1 {
                    if let Some(reg) = self.regs.lookup(&segments[0]) {
                        return Ok(reg);
                    }
                }
                Err(CodegenError::new(format!(
                    "cannot use path as value: {}",
                    segments.join("::")
                )))
            }

            Expr::Send { to, msg } => {
                let to_reg = self.compile_expr(to)?;
                let msg_reg = self.compile_expr(msg)?;

                // Convert message to string for Send instruction
                // TODO: Need proper message value support
                self.emit(Instruction::Send {
                    to: Source::Reg(to_reg),
                    msg: format!("msg_{}", msg_reg.0), // Placeholder
                });

                // Send returns the message
                Ok(msg_reg)
            }

            Expr::Receive { arms, timeout } => {
                let result_reg = self.regs.alloc();

                // Build receive clauses with their target addresses
                let mut clauses = Vec::new();
                let receive_idx = self.code.len();

                // First pass: compile patterns and calculate targets
                // We'll emit a placeholder ReceiveMatch, then compile bodies
                let timeout_val = timeout.as_ref().map(|(t, _)| {
                    if let Expr::Int(n) = t.as_ref() {
                        *n as u32
                    } else {
                        1000
                    }
                });

                // Emit placeholder - we'll build clauses after compiling bodies
                self.emit(Instruction::ReceiveMatch {
                    clauses: Vec::new(),
                    timeout: timeout_val,
                    timeout_target: 0,
                });

                let mut end_jumps = Vec::new();
                let mut arm_entries = Vec::new();

                // Compile each arm body and record entry points
                for arm in arms {
                    let arm_start = self.code.len();
                    arm_entries.push(arm_start);

                    let body_result = self.compile_expr(&arm.body)?;
                    self.emit(Instruction::Move {
                        source: body_result,
                        dest: result_reg,
                    });

                    let jump_idx = self.code.len();
                    self.emit(Instruction::Jump { target: 0 });
                    end_jumps.push(jump_idx);
                }

                // Compile timeout body if present
                let timeout_entry = self.code.len();
                if let Some((_, timeout_block)) = timeout {
                    let timeout_result = self.compile_block(timeout_block)?;
                    self.emit(Instruction::Move {
                        source: timeout_result,
                        dest: result_reg,
                    });
                }

                let end = self.code.len();

                // Now build the clauses with correct targets
                for (i, arm) in arms.iter().enumerate() {
                    let pattern = self.compile_pattern(&arm.pattern)?;
                    clauses.push((pattern, arm_entries[i]));
                }

                // Patch the ReceiveMatch instruction
                self.code[receive_idx] = Instruction::ReceiveMatch {
                    clauses,
                    timeout: timeout_val,
                    timeout_target: if timeout.is_some() {
                        timeout_entry
                    } else {
                        end
                    },
                };

                // Patch end jumps
                for jump_idx in end_jumps {
                    self.patch_jump(jump_idx, end);
                }

                Ok(result_reg)
            }

            Expr::Spawn(expr) => {
                // Spawn a function call
                match expr.as_ref() {
                    Expr::Call { func, args } => {
                        // Compile arguments
                        for (i, arg) in args.iter().enumerate() {
                            let arg_reg = self.compile_expr(arg)?;
                            if arg_reg.0 != i as u8 {
                                self.emit(Instruction::Move {
                                    source: arg_reg,
                                    dest: Register(i as u8),
                                });
                            }
                        }

                        let dest = self.regs.alloc();

                        match func.as_ref() {
                            Expr::Ident(name) => {
                                self.emit(Instruction::SpawnMFA {
                                    module: self.module_name.clone(),
                                    function: name.clone(),
                                    arity: args.len() as u8,
                                    dest,
                                });
                            }
                            Expr::Path { segments } if segments.len() == 2 => {
                                self.emit(Instruction::SpawnMFA {
                                    module: segments[0].clone(),
                                    function: segments[1].clone(),
                                    arity: args.len() as u8,
                                    dest,
                                });
                            }
                            _ => {
                                return Err(CodegenError::new("invalid spawn target"));
                            }
                        }

                        Ok(dest)
                    }
                    _ => Err(CodegenError::new("spawn requires a function call")),
                }
            }

            Expr::SpawnClosure(block) => {
                // Compile block as inline code for Spawn instruction
                // This is more complex - need to generate separate code
                // For now, generate a synthetic function

                // TODO: Implement proper closure compilation
                // For now, just emit an error or placeholder
                let _ = block;
                Err(CodegenError::new(
                    "spawn closures not yet implemented - use spawn function_name() instead",
                ))
            }

            Expr::Return(value) => {
                let result = if let Some(v) = value {
                    self.compile_expr(v)?
                } else {
                    let reg = self.regs.alloc();
                    self.emit(Instruction::LoadInt { value: 0, dest: reg });
                    reg
                };

                // Move to R0 if needed
                if result.0 != 0 {
                    self.emit(Instruction::Move {
                        source: result,
                        dest: Register(0),
                    });
                }

                self.emit(Instruction::Return);
                Ok(Register(0))
            }

            Expr::Unit => {
                let dest = self.regs.alloc();
                self.emit(Instruction::LoadInt { value: 0, dest });
                Ok(dest)
            }
        }
    }

    /// Compile a pattern to VM pattern.
    fn compile_pattern(&mut self, pattern: &AstPattern) -> CodegenResult<VmPattern> {
        match pattern {
            AstPattern::Wildcard => Ok(VmPattern::Wildcard),

            AstPattern::Ident(name) => {
                let reg = self.regs.alloc();
                self.regs.bind(name, reg.0);
                Ok(VmPattern::Variable(reg))
            }

            AstPattern::Int(n) => Ok(VmPattern::Int(*n)),

            AstPattern::String(s) => Ok(VmPattern::String(s.clone())),

            AstPattern::Atom(a) => Ok(VmPattern::Atom(a.clone())),

            AstPattern::Bool(b) => Ok(VmPattern::Int(if *b { 1 } else { 0 })),

            AstPattern::Tuple(elements) => {
                let mut vm_patterns = Vec::new();
                for elem in elements {
                    vm_patterns.push(self.compile_pattern(elem)?);
                }
                Ok(VmPattern::Tuple(vm_patterns))
            }

            AstPattern::List(elements) => {
                if elements.is_empty() {
                    Ok(VmPattern::ListEmpty)
                } else {
                    // Build nested cons pattern
                    let mut result = VmPattern::ListEmpty;
                    for elem in elements.iter().rev() {
                        let elem_pattern = self.compile_pattern(elem)?;
                        result = VmPattern::ListCons {
                            head: Box::new(elem_pattern),
                            tail: Box::new(result),
                        };
                    }
                    Ok(result)
                }
            }

            AstPattern::ListCons { head, tail } => {
                let head_pattern = self.compile_pattern(head)?;
                let tail_pattern = self.compile_pattern(tail)?;
                Ok(VmPattern::ListCons {
                    head: Box::new(head_pattern),
                    tail: Box::new(tail_pattern),
                })
            }

            AstPattern::Struct { name, fields } => {
                // Struct pattern becomes tuple pattern: {:Name, field1, field2, ...}
                let mut vm_patterns = vec![VmPattern::Atom(name.clone())];
                for (_, field_pattern) in fields {
                    vm_patterns.push(self.compile_pattern(field_pattern)?);
                }
                Ok(VmPattern::Tuple(vm_patterns))
            }

            AstPattern::Enum {
                name: _,
                variant,
                fields,
            } => {
                if fields.is_empty() {
                    // Unit variant: just the atom
                    Ok(VmPattern::Atom(variant.clone()))
                } else {
                    // Tuple variant: {:Variant, field1, ...}
                    let mut vm_patterns = vec![VmPattern::Atom(variant.clone())];
                    for field_pattern in fields {
                        vm_patterns.push(self.compile_pattern(field_pattern)?);
                    }
                    Ok(VmPattern::Tuple(vm_patterns))
                }
            }
        }
    }

    /// Bind variables in a pattern to registers.
    fn bind_pattern(&mut self, pattern: &AstPattern, reg: Register) -> CodegenResult<()> {
        match pattern {
            AstPattern::Ident(name) => {
                self.regs.bind(name, reg.0);
                Ok(())
            }
            AstPattern::Wildcard => Ok(()),
            AstPattern::Tuple(elements) => {
                for (i, elem) in elements.iter().enumerate() {
                    // Would need TupleElement to extract each field
                    let elem_reg = self.regs.alloc();
                    self.emit(Instruction::TupleElement {
                        tuple: reg,
                        index: i as u8,
                        dest: elem_reg,
                    });
                    self.bind_pattern(elem, elem_reg)?;
                }
                Ok(())
            }
            _ => {
                // For complex patterns in parameters, just bind the whole thing
                // Real pattern matching happens via Match instruction
                Ok(())
            }
        }
    }

    /// Emit an instruction.
    fn emit(&mut self, instr: Instruction) {
        self.code.push(instr);
    }

    /// Patch a Jump instruction target.
    fn patch_jump(&mut self, idx: usize, target: usize) {
        match &mut self.code[idx] {
            Instruction::Jump { target: t } => *t = target,
            Instruction::JumpIf { target: t, .. } => *t = target,
            Instruction::JumpUnless { target: t, .. } => *t = target,
            _ => {}
        }
    }

    /// Patch a Match instruction fail target.
    fn patch_match_fail(&mut self, idx: usize, target: usize) {
        if let Instruction::Match { fail_target, .. } = &mut self.code[idx] {
            *fail_target = target;
        }
    }
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to compile source code to a VM module.
pub fn compile(source: &str) -> CodegenResult<Module> {
    use crate::compiler::Parser;

    let mut parser = Parser::new(source);
    let ast = parser
        .parse_module()
        .map_err(|e| CodegenError::new(e.to_string()))?;

    Codegen::compile_module(&ast)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Scheduler, StepResult};

    fn run_to_completion(scheduler: &mut Scheduler) {
        for _ in 0..1000 {
            if scheduler.step(100) == StepResult::Idle {
                break;
            }
        }
    }

    #[test]
    fn test_compile_simple_add() {
        let source = r#"
            mod math {
                pub fn add(x: int, y: int) -> int {
                    x + y
                }
            }
        "#;

        let module = compile(source).unwrap();
        assert_eq!(module.name, "math");
        assert!(module.get_function("add", 2).is_some());
    }

    #[test]
    fn test_compile_and_run_add() {
        let source = r#"
            mod math {
                pub fn add(x: int, y: int) -> int {
                    x + y
                }
            }
        "#;

        let module = compile(source).unwrap();

        let mut scheduler = Scheduler::new();
        scheduler.load_module(module).unwrap();

        // Spawn a process that calls add(3, 4)
        use crate::instruction::{Instruction, Register};
        let program = vec![
            Instruction::LoadInt {
                value: 3,
                dest: Register(0),
            },
            Instruction::LoadInt {
                value: 4,
                dest: Register(1),
            },
            Instruction::CallMFA {
                module: "math".to_string(),
                function: "add".to_string(),
                arity: 2,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_completion(&mut scheduler);

        let process = scheduler.processes.get(&crate::Pid(0)).unwrap();
        assert_eq!(process.registers[0], crate::Value::Int(7));
    }

    #[test]
    fn test_compile_if_expression() {
        let source = r#"
            mod test {
                pub fn abs(x: int) -> int {
                    if x < 0 {
                        0 - x
                    } else {
                        x
                    }
                }
            }
        "#;

        let module = compile(source).unwrap();

        let mut scheduler = Scheduler::new();
        scheduler.load_module(module).unwrap();

        // Test abs(-5) = 5
        use crate::instruction::{Instruction, Register};
        let program = vec![
            Instruction::LoadInt {
                value: -5,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "test".to_string(),
                function: "abs".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_completion(&mut scheduler);

        let process = scheduler.processes.get(&crate::Pid(0)).unwrap();
        assert_eq!(process.registers[0], crate::Value::Int(5));
    }

    #[test]
    fn test_compile_match_expression() {
        let source = r#"
            mod test {
                pub fn is_zero(x: int) -> int {
                    match x {
                        0 => 1,
                        _ => 0,
                    }
                }
            }
        "#;

        let module = compile(source).unwrap();

        let mut scheduler = Scheduler::new();
        scheduler.load_module(module).unwrap();

        // Test is_zero(0) = 1
        use crate::instruction::{Instruction, Register};
        let program = vec![
            Instruction::LoadInt {
                value: 0,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "test".to_string(),
                function: "is_zero".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_completion(&mut scheduler);

        let process = scheduler.processes.get(&crate::Pid(0)).unwrap();
        assert_eq!(process.registers[0], crate::Value::Int(1));
    }

    #[test]
    fn test_compile_recursive_factorial() {
        let source = r#"
            mod math {
                pub fn factorial(n: int) -> int {
                    if n == 0 {
                        1
                    } else {
                        n * factorial(n - 1)
                    }
                }
            }
        "#;

        let module = compile(source).unwrap();

        let mut scheduler = Scheduler::new();
        scheduler.load_module(module).unwrap();

        // Test factorial(5) = 120
        use crate::instruction::{Instruction, Register};
        let program = vec![
            Instruction::LoadInt {
                value: 5,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "math".to_string(),
                function: "factorial".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_completion(&mut scheduler);

        let process = scheduler.processes.get(&crate::Pid(0)).unwrap();
        assert_eq!(process.registers[0], crate::Value::Int(120));
    }

    #[test]
    fn test_compile_let_binding() {
        let source = r#"
            mod test {
                pub fn double(x: int) -> int {
                    let y = x * 2;
                    y
                }
            }
        "#;

        let module = compile(source).unwrap();

        let mut scheduler = Scheduler::new();
        scheduler.load_module(module).unwrap();

        use crate::instruction::{Instruction, Register};
        let program = vec![
            Instruction::LoadInt {
                value: 7,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "test".to_string(),
                function: "double".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_completion(&mut scheduler);

        let process = scheduler.processes.get(&crate::Pid(0)).unwrap();
        assert_eq!(process.registers[0], crate::Value::Int(14));
    }

    #[test]
    fn test_compile_match_with_guard() {
        let source = r#"
            mod test {
                pub fn classify(n: int) -> int {
                    match n {
                        x if x < 0 => 0 - 1,
                        x if x == 0 => 0,
                        x if x > 0 => 1,
                        _ => 99,
                    }
                }
            }
        "#;

        let module = compile(source).unwrap();

        let mut scheduler = Scheduler::new();
        scheduler.load_module(module).unwrap();

        // Test negative: classify(-5) = -1
        use crate::instruction::{Instruction, Register};
        let program = vec![
            Instruction::LoadInt {
                value: -5,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "test".to_string(),
                function: "classify".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_completion(&mut scheduler);

        let process = scheduler.processes.get(&crate::Pid(0)).unwrap();
        assert_eq!(process.registers[0], crate::Value::Int(-1));
    }

    #[test]
    fn test_compile_match_guard_zero() {
        let source = r#"
            mod test {
                pub fn classify(n: int) -> int {
                    match n {
                        x if x < 0 => 0 - 1,
                        x if x == 0 => 0,
                        x if x > 0 => 1,
                        _ => 99,
                    }
                }
            }
        "#;

        let module = compile(source).unwrap();

        let mut scheduler = Scheduler::new();
        scheduler.load_module(module).unwrap();

        // Test zero: classify(0) = 0
        use crate::instruction::{Instruction, Register};
        let program = vec![
            Instruction::LoadInt {
                value: 0,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "test".to_string(),
                function: "classify".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_completion(&mut scheduler);

        let process = scheduler.processes.get(&crate::Pid(0)).unwrap();
        assert_eq!(process.registers[0], crate::Value::Int(0));
    }

    #[test]
    fn test_compile_match_guard_positive() {
        let source = r#"
            mod test {
                pub fn classify(n: int) -> int {
                    match n {
                        x if x < 0 => 0 - 1,
                        x if x == 0 => 0,
                        x if x > 0 => 1,
                        _ => 99,
                    }
                }
            }
        "#;

        let module = compile(source).unwrap();

        let mut scheduler = Scheduler::new();
        scheduler.load_module(module).unwrap();

        // Test positive: classify(42) = 1
        use crate::instruction::{Instruction, Register};
        let program = vec![
            Instruction::LoadInt {
                value: 42,
                dest: Register(0),
            },
            Instruction::CallMFA {
                module: "test".to_string(),
                function: "classify".to_string(),
                arity: 1,
            },
            Instruction::End,
        ];

        scheduler.spawn(program);
        run_to_completion(&mut scheduler);

        let process = scheduler.processes.get(&crate::Pid(0)).unwrap();
        assert_eq!(process.registers[0], crate::Value::Int(1));
    }
}
