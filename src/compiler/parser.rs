//! Recursive descent parser.

use crate::compiler::ast::*;
use crate::compiler::error::{ParseError, ParseResult};
use crate::compiler::lexer::{Lexer, Span, SpannedToken};
use crate::compiler::prelude::prelude_items_for_module;
use crate::compiler::token::Token;

/// Recursive descent parser.
pub struct Parser<'source> {
    tokens: Vec<SpannedToken>,
    pos: usize,
    source: &'source str,
}

impl<'source> Parser<'source> {
    /// Create a new parser for the given source code.
    pub fn new(source: &'source str) -> Self {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.collect_tokens();
        Self {
            tokens,
            pos: 0,
            source,
        }
    }

    /// Parse a complete module.
    pub fn parse_module(&mut self) -> ParseResult<Module> {
        self.expect(&Token::Mod)?;
        let name = self.expect_ident()?;
        self.expect(&Token::LBrace)?;

        let mut items = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            items.push(self.parse_item()?);
        }

        self.expect(&Token::RBrace)?;

        // Inject prelude items (Option, Result) at the beginning
        let prelude = prelude_items_for_module(&items);
        let mut all_items = prelude;
        all_items.extend(items);

        Ok(Module {
            name,
            items: all_items,
            source: Some(self.source.to_string()),
        })
    }

    /// Parse a source file as a module.
    /// Handles both wrapped modules (`mod name { ... }`) and file-based modules (items directly).
    /// For wrapped modules, the module name comes from the source.
    /// For file-based modules, the name is derived from the filename.
    /// Note: For files with multiple wrapped modules, use parse_file_modules instead.
    pub fn parse_file(&mut self, module_name: &str) -> ParseResult<Module> {
        // Check if this is a wrapped module: `mod name { ... }`
        if self.check(&Token::Mod) {
            // Peek ahead to see if it's `mod name {` (wrapped) or `mod name;` (declaration)
            if self.peek_is_wrapped_module() {
                return self.parse_module();
            }
        }

        // File-based module: parse items directly
        let mut items = Vec::new();

        while !self.is_at_end() {
            items.push(self.parse_item()?);
        }

        // Inject prelude items (Option, Result) at the beginning
        let prelude = prelude_items_for_module(&items);
        let mut all_items = prelude;
        all_items.extend(items);

        Ok(Module {
            name: module_name.to_string(),
            items: all_items,
            source: Some(self.source.to_string()),
        })
    }

    /// Parse a source file that may contain multiple wrapped modules.
    /// Returns all modules found in the file.
    /// If the file contains file-based items (not wrapped in mod {}), returns a single module.
    pub fn parse_file_modules(&mut self, fallback_name: &str) -> ParseResult<Vec<Module>> {
        let mut modules = Vec::new();

        // Check if this file has wrapped modules
        if self.check(&Token::Mod) && self.peek_is_wrapped_module() {
            // Parse all wrapped modules in the file
            while !self.is_at_end() {
                if self.check(&Token::Mod) && self.peek_is_wrapped_module() {
                    modules.push(self.parse_module()?);
                } else {
                    // Non-module item at top level - this is an error in a multi-module file
                    let span = self.current_span();
                    return Err(ParseError::new(
                        "expected `mod` block in multi-module file",
                        span,
                    ));
                }
            }
        } else {
            // File-based module: parse items directly
            let module = self.parse_file(fallback_name)?;
            modules.push(module);
        }

        Ok(modules)
    }

    /// Check if the next tokens are `mod <ident> {` (wrapped module).
    fn peek_is_wrapped_module(&self) -> bool {
        // Check: mod <ident> {
        if self.pos + 2 < self.tokens.len() {
            let is_mod = matches!(self.tokens[self.pos].token, Token::Mod);
            let is_ident = matches!(self.tokens[self.pos + 1].token, Token::Ident(_));
            let is_lbrace = matches!(self.tokens[self.pos + 2].token, Token::LBrace);
            return is_mod && is_ident && is_lbrace;
        }
        false
    }

    /// Parse a top-level item.
    pub fn parse_item(&mut self) -> ParseResult<Item> {
        // Use statements don't have pub modifier
        if self.check(&Token::Use) {
            return self.parse_use_decl();
        }

        // Impl blocks don't have pub modifier (methods inside can be pub)
        if self.check(&Token::Impl) {
            return self.parse_impl_or_trait_impl();
        }

        // Trait definitions (with optional pub modifier)
        if self.check(&Token::Trait) {
            return self.parse_trait_def();
        }

        let is_pub = self.check(&Token::Pub);
        if is_pub {
            self.advance();
        }

        // Check for trait after pub
        if self.check(&Token::Trait) {
            // pub trait - parse trait definition
            // Note: Currently traits don't track pub visibility, but we accept the syntax
            return self.parse_trait_def();
        }

        if self.check(&Token::Fn) {
            Ok(Item::Function(self.parse_function(is_pub)?))
        } else if self.check(&Token::Struct) {
            Ok(Item::Struct(self.parse_struct(is_pub)?))
        } else if self.check(&Token::Enum) {
            Ok(Item::Enum(self.parse_enum(is_pub)?))
        } else if self.check(&Token::Mod) {
            self.parse_mod_decl(is_pub)
        } else {
            let span = self.current_span();
            Err(ParseError::new(
                "expected `fn`, `struct`, `enum`, `mod`, `impl`, `trait`, or `use`",
                span,
            ))
        }
    }

    /// Parse a module declaration: `mod foo;`
    fn parse_mod_decl(&mut self, is_pub: bool) -> ParseResult<Item> {
        self.expect(&Token::Mod)?;
        let name = self.expect_ident()?;
        self.expect(&Token::Semi)?;
        Ok(Item::ModDecl(ModDecl { name, is_pub }))
    }

    /// Parse an impl block, trait implementation, or module-level trait declaration.
    /// `impl Point { ... }` or `impl Display for Point { ... }` or `impl mod::Trait;`
    fn parse_impl_or_trait_impl(&mut self) -> ParseResult<Item> {
        self.expect(&Token::Impl)?;

        // Parse the first type name (could be module-qualified like genserver::GenServer)
        let first_name = self.parse_possibly_qualified_type_name()?;

        // Check for module-level trait declaration: `impl Trait;`
        if self.check(&Token::Semi) {
            self.advance();
            return Ok(Item::TraitDecl(TraitDecl {
                trait_name: first_name,
            }));
        }

        // Check if this is a trait impl: `impl Trait for Type { ... }`
        if self.check(&Token::For) {
            self.advance();
            let type_name = self.expect_type_ident()?;
            self.expect(&Token::LBrace)?;

            // Parse type bindings and methods inside the trait impl
            let mut type_bindings = Vec::new();
            let mut methods = Vec::new();
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                // Check for type binding: `type Name = ConcreteType;`
                if self.check_ident("type") {
                    type_bindings.push(self.parse_type_binding()?);
                    continue;
                }

                // Methods can have pub modifier
                let is_pub = self.check(&Token::Pub);
                if is_pub {
                    self.advance();
                }

                if self.check(&Token::Fn) {
                    methods.push(self.parse_function(is_pub)?);
                } else {
                    let span = self.current_span();
                    return Err(ParseError::new("expected `fn` or `type` in trait impl", span));
                }
            }

            self.expect(&Token::RBrace)?;

            Ok(Item::TraitImpl(TraitImpl {
                trait_name: first_name,
                type_name,
                type_bindings,
                methods,
            }))
        } else {
            // Regular impl block: `impl Type { ... }`
            self.expect(&Token::LBrace)?;

            // Parse methods inside the impl block
            let mut methods = Vec::new();
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                // Methods can have pub modifier
                let is_pub = self.check(&Token::Pub);
                if is_pub {
                    self.advance();
                }

                if self.check(&Token::Fn) {
                    methods.push(self.parse_function(is_pub)?);
                } else {
                    let span = self.current_span();
                    return Err(ParseError::new("expected `fn` in impl block", span));
                }
            }

            self.expect(&Token::RBrace)?;

            Ok(Item::Impl(ImplBlock {
                type_name: first_name,
                methods,
            }))
        }
    }

    /// Parse a type name that may be module-qualified: `Point` or `genserver::GenServer`
    fn parse_possibly_qualified_type_name(&mut self) -> ParseResult<String> {
        // Check for module-qualified name: module::Type
        if let Some(Token::Ident(module_name)) = self.peek().cloned() {
            if self.peek_next() == Some(&Token::ColonColon) {
                self.advance(); // consume module name
                self.advance(); // consume ::
                let type_name = self.expect_type_ident()?;
                return Ok(format!("{}::{}", module_name, type_name));
            }
        }
        // Otherwise, just a simple type identifier
        self.expect_type_ident()
    }

    /// Parse a trait definition: `trait Display { fn display(self) -> String; }`
    fn parse_trait_def(&mut self) -> ParseResult<Item> {
        self.expect(&Token::Trait)?;
        let name = self.expect_type_ident()?;
        self.expect(&Token::LBrace)?;

        let mut associated_types = Vec::new();
        let mut methods = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            // Check for associated type declaration: `type Name;`
            if self.check_ident("type") {
                associated_types.push(self.parse_associated_type_decl()?);
            } else {
                methods.push(self.parse_trait_method()?);
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Item::Trait(TraitDef { name, associated_types, methods }))
    }

    /// Parse an associated type declaration in a trait: `type Name;`
    fn parse_associated_type_decl(&mut self) -> ParseResult<String> {
        self.expect_ident_value("type")?;
        let name = self.expect_type_ident()?;
        self.expect(&Token::Semi)?;
        Ok(name)
    }

    /// Parse a type binding in a trait impl: `type State = int;`
    fn parse_type_binding(&mut self) -> ParseResult<(String, Type)> {
        self.expect_ident_value("type")?;
        let name = self.expect_type_ident()?;
        self.expect(&Token::Eq)?;
        let ty = self.parse_type()?;
        self.expect(&Token::Semi)?;
        Ok((name, ty))
    }

    /// Parse a trait method: `fn method<T: Bound>(self, arg: Type) -> ReturnType;`
    /// or with default body: `fn method(self) -> Type { ... }`
    fn parse_trait_method(&mut self) -> ParseResult<TraitMethod> {
        self.expect(&Token::Fn)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;

        self.expect(&Token::LParen)?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                params.push(self.parse_param()?);
                if !self.check(&Token::Comma) {
                    break;
                }
                self.advance();
            }
        }
        self.expect(&Token::RParen)?;

        let return_type = if self.check(&Token::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Check for default implementation body or semicolon
        let body = if self.check(&Token::LBrace) {
            Some(self.parse_block()?)
        } else {
            self.expect(&Token::Semi)?;
            None
        };

        Ok(TraitMethod {
            name,
            type_params,
            params,
            return_type,
            body,
        })
    }

    /// Parse a use declaration: `use foo::bar;` or `use foo::{a, b};` or `use foo::*;`
    fn parse_use_decl(&mut self) -> ParseResult<Item> {
        self.expect(&Token::Use)?;
        let module = self.expect_ident()?;
        self.expect(&Token::ColonColon)?;

        let tree = if self.check(&Token::Star) {
            // Glob import: use foo::*;
            self.advance();
            self.expect(&Token::Semi)?;
            UseTree::Glob { module }
        } else if self.check(&Token::LBrace) {
            // Group import: use foo::{a, b as c};
            self.advance();
            let mut items = Vec::new();

            if !self.check(&Token::RBrace) {
                loop {
                    let name = self.expect_ident()?;
                    let rename = if self.check(&Token::As) {
                        self.advance();
                        Some(self.expect_ident()?)
                    } else {
                        None
                    };
                    items.push(UseTreeItem { name, rename });

                    if !self.check(&Token::Comma) {
                        break;
                    }
                    self.advance();
                }
            }

            self.expect(&Token::RBrace)?;
            self.expect(&Token::Semi)?;
            UseTree::Group { module, items }
        } else {
            // Single import: use foo::bar; or use foo::bar as baz;
            let name = self.expect_ident()?;
            let rename = if self.check(&Token::As) {
                self.advance();
                Some(self.expect_ident()?)
            } else {
                None
            };
            self.expect(&Token::Semi)?;
            UseTree::Path { module, name, rename }
        };

        Ok(Item::Use(UseDecl { tree }))
    }

    /// Parse a function definition.
    fn parse_function(&mut self, is_pub: bool) -> ParseResult<Function> {
        let start = self.current_span().start;
        self.expect(&Token::Fn)?;
        let name = self.expect_ident()?;

        // Parse optional type parameters: <T, U>
        let type_params = self.parse_type_params()?;

        self.expect(&Token::LParen)?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                params.push(self.parse_param()?);
                if !self.check(&Token::Comma) {
                    break;
                }
                self.advance(); // consume comma
            }
        }
        self.expect(&Token::RParen)?;

        // Parse optional guard clause: `when <expr>`
        let guard = if self.check(&Token::When) {
            self.advance();
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        let return_type = if self.check(&Token::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;
        let end = self.tokens.get(self.pos.saturating_sub(1))
            .map(|t| t.span.end)
            .unwrap_or(start);

        Ok(Function {
            name,
            type_params,
            params,
            guard,
            return_type,
            body,
            is_pub,
            span: start..end,
        })
    }

    /// Parse a function parameter.
    fn parse_param(&mut self) -> ParseResult<Param> {
        // Special handling for `self` without type annotation
        if self.check(&Token::SelfKw) {
            self.advance();
            let pattern = Pattern::Ident("self".to_string());

            // Optional type annotation for self
            let ty = if self.check(&Token::Colon) {
                self.advance();
                self.parse_type()?
            } else {
                // Use `Self` as placeholder type (resolved during codegen)
                Type::Named {
                    name: "Self".to_string(),
                    type_args: vec![],
                }
            };

            return Ok(Param { pattern, ty });
        }

        let pattern = self.parse_pattern()?;

        // Type is optional for literal patterns (type can be inferred)
        let ty = if self.check(&Token::Colon) {
            self.advance();
            self.parse_type()?
        } else {
            // Infer type from pattern for literals
            match &pattern {
                Pattern::Int(_) => Type::Named {
                    name: "int".to_string(),
                    type_args: vec![],
                },
                Pattern::Atom(_) => Type::Named {
                    name: "atom".to_string(),
                    type_args: vec![],
                },
                Pattern::Bool(_) => Type::Named {
                    name: "bool".to_string(),
                    type_args: vec![],
                },
                Pattern::String(_) => Type::Named {
                    name: "string".to_string(),
                    type_args: vec![],
                },
                Pattern::Wildcard => Type::Named {
                    name: "any".to_string(),
                    type_args: vec![],
                },
                _ => {
                    let span = self.current_span();
                    return Err(ParseError::new(
                        "type annotation required for this pattern",
                        span,
                    ));
                }
            }
        };

        Ok(Param { pattern, ty })
    }

    /// Parse a struct definition.
    fn parse_struct(&mut self, is_pub: bool) -> ParseResult<StructDef> {
        self.expect(&Token::Struct)?;
        let name = self.expect_type_ident()?;

        // Parse optional type parameters: <T, U>
        let type_params = self.parse_type_params()?;

        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let field_type = self.parse_type()?;
            fields.push((field_name, field_type));

            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(StructDef {
            name,
            type_params,
            fields,
            is_pub,
        })
    }

    /// Parse an enum definition.
    fn parse_enum(&mut self, is_pub: bool) -> ParseResult<EnumDef> {
        self.expect(&Token::Enum)?;
        let name = self.expect_type_ident()?;

        // Parse optional type parameters: <T, E>
        let type_params = self.parse_type_params()?;

        self.expect(&Token::LBrace)?;

        let mut variants = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            let variant_name = self.expect_type_ident()?;

            let fields = if self.check(&Token::LParen) {
                self.advance();
                let mut fs = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        fs.push(self.parse_type()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(&Token::RParen)?;
                fs
            } else {
                Vec::new()
            };

            variants.push(EnumVariant {
                name: variant_name,
                fields,
            });

            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(EnumDef {
            name,
            type_params,
            variants,
            is_pub,
        })
    }

    /// Parse a block.
    fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect(&Token::LBrace)?;

        let mut stmts = Vec::new();
        let mut expr = None;

        while !self.check(&Token::RBrace) && !self.is_at_end() {
            // Check if this is a let statement
            if self.check(&Token::Let) {
                stmts.push(self.parse_let_stmt()?);
            } else {
                // Parse expression
                let e = self.parse_expr()?;

                // Check if followed by semicolon (statement) or not (trailing expr)
                if self.check(&Token::Semi) {
                    self.advance();
                    stmts.push(Stmt::Expr(e));
                } else if self.check(&Token::RBrace) {
                    // Trailing expression
                    expr = Some(Box::new(e));
                } else {
                    // Expression statements that don't need semicolons
                    // (if, match, block, etc.)
                    if Self::is_block_expr(&e) {
                        stmts.push(Stmt::Expr(e));
                    } else {
                        let span = self.current_span();
                        return Err(ParseError::new("expected `;` or `}`", span));
                    }
                }
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(Block { stmts, expr })
    }

    /// Check if an expression is a "block expression" that doesn't need a semicolon.
    fn is_block_expr(e: &Expr) -> bool {
        matches!(
            e,
            Expr::If { .. }
                | Expr::Match { .. }
                | Expr::Block(_)
                | Expr::Receive { .. }
        )
    }

    /// Parse a let statement.
    fn parse_let_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect(&Token::Let)?;
        let pattern = self.parse_pattern()?;

        let ty = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        self.expect(&Token::Semi)?;

        Ok(Stmt::Let { pattern, ty, value })
    }

    /// Parse an expression.
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_pipe_expr()
    }

    /// Parse pipe expressions: `expr |> func(args)`.
    /// Pipe has lowest precedence and is left-associative.
    fn parse_pipe_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_or_expr()?;

        while self.check(&Token::PipeRight) {
            self.advance();
            let right = self.parse_or_expr()?;
            left = Expr::Pipe {
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse || expressions.
    fn parse_or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_and_expr()?;

        while self.check(&Token::OrOr) {
            self.advance();
            let right = self.parse_and_expr()?;
            left = Expr::Binary {
                op: BinOp::Or,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse && expressions.
    fn parse_and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_cmp_expr()?;

        while self.check(&Token::AndAnd) {
            self.advance();
            let right = self.parse_cmp_expr()?;
            left = Expr::Binary {
                op: BinOp::And,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse comparison expressions.
    fn parse_cmp_expr(&mut self) -> ParseResult<Expr> {
        let left = self.parse_add_expr()?;

        let op = if self.check(&Token::EqEq) {
            Some(BinOp::Eq)
        } else if self.check(&Token::BangEq) {
            Some(BinOp::Ne)
        } else if self.check(&Token::Lt) {
            Some(BinOp::Lt)
        } else if self.check(&Token::LtEq) {
            Some(BinOp::Le)
        } else if self.check(&Token::Gt) {
            Some(BinOp::Gt)
        } else if self.check(&Token::GtEq) {
            Some(BinOp::Ge)
        } else {
            None
        };

        if let Some(op) = op {
            self.advance();
            let right = self.parse_add_expr()?;
            Ok(Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            })
        } else {
            Ok(left)
        }
    }

    /// Parse addition/subtraction expressions.
    fn parse_add_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_mul_expr()?;

        loop {
            let op = if self.check(&Token::Plus) {
                BinOp::Add
            } else if self.check(&Token::Minus) {
                BinOp::Sub
            } else {
                break;
            };

            self.advance();
            let right = self.parse_mul_expr()?;
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse multiplication/division expressions.
    fn parse_mul_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_unary_expr()?;

        loop {
            let op = if self.check(&Token::Star) {
                BinOp::Mul
            } else if self.check(&Token::Slash) {
                BinOp::Div
            } else if self.check(&Token::Percent) {
                BinOp::Mod
            } else {
                break;
            };

            self.advance();
            let right = self.parse_unary_expr()?;
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse unary expressions.
    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        if self.check(&Token::Bang) {
            self.advance();
            let expr = self.parse_unary_expr()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(expr),
            });
        }

        if self.check(&Token::Minus) {
            self.advance();
            let expr = self.parse_unary_expr()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(expr),
            });
        }

        self.parse_postfix_expr()
    }

    /// Parse postfix expressions (calls, field access).
    fn parse_postfix_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            // Check for turbofish type args before function call: func::<T>(args)
            let type_args = if self.check(&Token::ColonColon) && self.peek_is_lt() {
                self.advance(); // consume '::'
                self.parse_type_args()?
            } else {
                vec![]
            };

            if self.check(&Token::LParen) {
                // Function call
                self.advance();
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(&Token::RParen)?;
                expr = Expr::Call {
                    func: Box::new(expr),
                    type_args,
                    args,
                };
            } else if !type_args.is_empty() {
                // Had turbofish but no function call - error
                let span = self.current_span();
                return Err(ParseError::new("expected function call after type arguments", span));
            } else if self.check(&Token::Dot) {
                // Field access or method call
                self.advance();
                let field = self.expect_ident()?;

                if self.check(&Token::LParen) {
                    // Method call
                    self.advance();
                    let mut args = Vec::new();
                    if !self.check(&Token::RParen) {
                        loop {
                            args.push(self.parse_expr()?);
                            if !self.check(&Token::Comma) {
                                break;
                            }
                            self.advance();
                        }
                    }
                    self.expect(&Token::RParen)?;
                    expr = Expr::MethodCall {
                        receiver: Box::new(expr),
                        method: field,
                        args,
                        resolved_module: None,
                    };
                } else {
                    expr = Expr::FieldAccess {
                        expr: Box::new(expr),
                        field,
                    };
                }
            } else if self.check(&Token::ColonColon) {
                // Path access - only valid if expr is Ident or Path
                self.advance();
                let segment = self.expect_ident_or_type_ident()?;
                let is_type_segment = segment.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);

                expr = match expr {
                    Expr::Ident(first) => Expr::Path {
                        segments: vec![first, segment.clone()],
                    },
                    Expr::Path { mut segments } => {
                        segments.push(segment.clone());
                        Expr::Path { segments }
                    }
                    _ => {
                        let span = self.current_span();
                        return Err(ParseError::new("invalid path expression", span));
                    }
                };

                // Check for struct init after module-qualified path: mod::Type { ... }
                if is_type_segment && self.check(&Token::LBrace) {
                    // This is a struct init with qualified path
                    if let Expr::Path { segments } = expr {
                        let name = segments.join("::");
                        self.advance(); // consume '{'
                        let mut fields = Vec::new();
                        while !self.check(&Token::RBrace) && !self.is_at_end() {
                            let field_name = self.expect_ident()?;
                            self.expect(&Token::Colon)?;
                            let field_value = self.parse_expr()?;
                            fields.push((field_name, field_value));

                            if self.check(&Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.expect(&Token::RBrace)?;
                        expr = Expr::StructInit { name, fields };
                    }
                }

                // Check for enum variant after module-qualified path: mod::Type::Variant or mod::Type::Variant(args)
                // The path looks like ["mod", "Type", "Variant"] where Type and Variant are both uppercase
                if is_type_segment {
                    if let Expr::Path { ref segments } = expr {
                        // Check if this looks like mod::Type::Variant (at least 3 segments, last two uppercase)
                        if segments.len() >= 3 {
                            let type_idx = segments.len() - 2;
                            let variant_idx = segments.len() - 1;
                            let type_is_upper = segments[type_idx].chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                            let variant_is_upper = segments[variant_idx].chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                            if type_is_upper && variant_is_upper {
                                // This is mod::Type::Variant - treat as enum variant
                                let variant = segments[variant_idx].clone();
                                let type_path = segments[..variant_idx].join("::");

                                // Check for args
                                let args = if self.check(&Token::LParen) {
                                    self.advance();
                                    let mut args = Vec::new();
                                    if !self.check(&Token::RParen) {
                                        loop {
                                            args.push(self.parse_expr()?);
                                            if !self.check(&Token::Comma) {
                                                break;
                                            }
                                            self.advance();
                                        }
                                    }
                                    self.expect(&Token::RParen)?;
                                    args
                                } else {
                                    vec![]
                                };

                                expr = Expr::EnumVariant {
                                    type_name: Some(type_path),
                                    variant,
                                    args,
                                };
                            }
                        }
                    }
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse primary expressions.
    fn parse_primary(&mut self) -> ParseResult<Expr> {
        // Literals
        if let Some(Token::Int(n)) = self.peek().cloned() {
            self.advance();
            return Ok(Expr::Int(n));
        }

        if let Some(Token::String(s)) = self.peek().cloned() {
            self.advance();
            return Ok(Expr::String(s));
        }

        // Check for atom or quoted atom (for extern calls or literal atoms)
        let atom_name = match self.peek().cloned() {
            Some(Token::Atom(a)) => {
                self.advance();
                Some(a)
            }
            Some(Token::QuotedAtom(a)) => {
                self.advance();
                Some(a)
            }
            _ => None,
        };

        if let Some(a) = atom_name {
            // Check for extern call: :module::function(args)
            if self.check(&Token::ColonColon) {
                self.advance();
                let function = self.expect_ident()?;
                self.expect(&Token::LParen)?;
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    args.push(self.parse_expr()?);
                    while self.check(&Token::Comma) {
                        self.advance();
                        args.push(self.parse_expr()?);
                    }
                }
                self.expect(&Token::RParen)?;
                return Ok(Expr::ExternCall {
                    module: a,
                    function,
                    args,
                });
            }
            return Ok(Expr::Atom(a));
        }

        if self.check(&Token::True) {
            self.advance();
            return Ok(Expr::Bool(true));
        }

        if self.check(&Token::False) {
            self.advance();
            return Ok(Expr::Bool(false));
        }

        // Self keyword as identifier (for impl methods)
        if self.check(&Token::SelfKw) {
            self.advance();
            return Ok(Expr::Ident("self".to_string()));
        }

        // Identifier or type identifier (for struct init or enum)
        if let Some(Token::Ident(name)) = self.peek().cloned() {
            self.advance();
            return Ok(Expr::Ident(name));
        }

        if let Some(Token::TypeIdent(name)) = self.peek().cloned() {
            self.advance();

            // Check for struct init: TypeIdent { ... }
            if self.check(&Token::LBrace) {
                self.advance();
                let mut fields = Vec::new();
                while !self.check(&Token::RBrace) && !self.is_at_end() {
                    let field_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let field_value = self.parse_expr()?;
                    fields.push((field_name, field_value));

                    if self.check(&Token::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                self.expect(&Token::RBrace)?;
                return Ok(Expr::StructInit { name, fields });
            }

            // Check for qualified path: Type::variant or Type::method
            if self.check(&Token::ColonColon) {
                self.advance();

                // Could be TypeIdent (enum variant) or Ident (static method)
                if let Some(Token::TypeIdent(variant)) = self.peek().cloned() {
                    self.advance();

                    // Check for args: TypeIdent::Variant(args)
                    if self.check(&Token::LParen) {
                        self.advance();
                        let mut args = Vec::new();
                        if !self.check(&Token::RParen) {
                            loop {
                                args.push(self.parse_expr()?);
                                if !self.check(&Token::Comma) {
                                    break;
                                }
                                self.advance();
                            }
                        }
                        self.expect(&Token::RParen)?;
                        return Ok(Expr::EnumVariant {
                            type_name: Some(name),
                            variant,
                            args,
                        });
                    }

                    // Unit variant: TypeIdent::Variant
                    return Ok(Expr::EnumVariant {
                        type_name: Some(name),
                        variant,
                        args: vec![],
                    });
                } else if let Some(Token::Ident(method)) = self.peek().cloned() {
                    // Static method path: Type::method
                    self.advance();
                    return Ok(Expr::Path {
                        segments: vec![name, method],
                    });
                } else {
                    let span = self.current_span();
                    return Err(ParseError::new(
                        "expected identifier or type identifier after `::`",
                        span,
                    ));
                }
            }

            // Check for enum variant without type: Variant(args)
            if self.check(&Token::LParen) {
                self.advance();
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(&Token::RParen)?;
                return Ok(Expr::EnumVariant {
                    type_name: None,
                    variant: name,
                    args,
                });
            }

            // Unit variant without type qualifier: Variant (just an atom)
            return Ok(Expr::EnumVariant {
                type_name: None,
                variant: name,
                args: vec![],
            });
        }

        // Parenthesized expression or tuple
        if self.check(&Token::LParen) {
            self.advance();

            if self.check(&Token::RParen) {
                self.advance();
                return Ok(Expr::Unit);
            }

            let first = self.parse_expr()?;

            if self.check(&Token::Comma) {
                // Tuple
                let mut elements = vec![first];
                while self.check(&Token::Comma) {
                    self.advance();
                    if self.check(&Token::RParen) {
                        break;
                    }
                    elements.push(self.parse_expr()?);
                }
                self.expect(&Token::RParen)?;
                return Ok(Expr::Tuple(elements));
            }

            self.expect(&Token::RParen)?;
            return Ok(first);
        }

        // List
        if self.check(&Token::LBracket) {
            self.advance();
            let mut elements = Vec::new();
            if !self.check(&Token::RBracket) {
                loop {
                    elements.push(self.parse_expr()?);
                    if !self.check(&Token::Comma) {
                        break;
                    }
                    self.advance();
                }
            }
            self.expect(&Token::RBracket)?;
            return Ok(Expr::List(elements));
        }

        // Block expression
        if self.check(&Token::LBrace) {
            return Ok(Expr::Block(self.parse_block()?));
        }

        // If expression
        if self.check(&Token::If) {
            return self.parse_if_expr();
        }

        // Match expression
        if self.check(&Token::Match) {
            return self.parse_match_expr();
        }

        // Receive expression
        if self.check(&Token::Receive) {
            return self.parse_receive_expr();
        }

        // Spawn expression
        if self.check(&Token::Spawn) {
            return self.parse_spawn_expr();
        }

        // Return expression
        if self.check(&Token::Return) {
            self.advance();
            let value = if self.check(&Token::Semi)
                || self.check(&Token::RBrace)
                || self.check(&Token::Comma)
            {
                None
            } else {
                Some(Box::new(self.parse_expr()?))
            };
            return Ok(Expr::Return(value));
        }

        // Self keyword
        if self.check(&Token::SelfKw) {
            self.advance();
            return Ok(Expr::Ident("self".to_string()));
        }

        // Binary/bit string: <<segments>>
        if self.check(&Token::LtLt) {
            return self.parse_bitstring_expr();
        }

        // Closure: |x, y| { body } or || { body }
        if self.check(&Token::Pipe) || self.check(&Token::OrOr) {
            let mut params = Vec::new();

            if self.check(&Token::OrOr) {
                // Empty params: || { body }
                self.advance();
            } else {
                // |x, y| { body }
                self.advance(); // consume opening |
                if !self.check(&Token::Pipe) {
                    params.push(self.expect_ident()?);
                    while self.check(&Token::Comma) {
                        self.advance();
                        if self.check(&Token::Pipe) {
                            break;
                        }
                        params.push(self.expect_ident()?);
                    }
                }
                self.expect(&Token::Pipe)?; // consume closing |
            }

            let body = self.parse_block()?;
            return Ok(Expr::Closure { params, body });
        }

        let span = self.current_span();
        Err(ParseError::new("expected expression", span))
    }

    /// Parse a bit string expression: `<<1, 2, X:16/little>>`
    fn parse_bitstring_expr(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::LtLt)?;

        let mut segments = Vec::new();

        // Handle empty binary: <<>>
        if self.check(&Token::GtGt) {
            self.advance();
            return Ok(Expr::BitString(segments));
        }

        loop {
            let segment = self.parse_bitstring_segment_expr()?;
            segments.push(segment);

            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(&Token::GtGt)?;
        Ok(Expr::BitString(segments))
    }

    /// Parse a single bit string segment: `value:size/specifiers`
    fn parse_bitstring_segment_expr(&mut self) -> ParseResult<BitStringSegment<Box<Expr>>> {
        // Parse the value expression
        let value = Box::new(self.parse_unary_expr()?);

        let mut segment = BitStringSegment::new(value);

        // Parse optional size: `:size`
        if self.check(&Token::Colon) {
            self.advance();
            segment.size = Some(Box::new(self.parse_unary_expr()?));
        }

        // Parse optional type specifiers: `/specifier-specifier-...`
        if self.check(&Token::Slash) {
            self.advance();
            self.parse_bitstring_specifiers(&mut segment)?;
        }

        Ok(segment)
    }

    /// Parse bit string type specifiers: `big-signed-integer`
    fn parse_bitstring_specifiers<T>(&mut self, segment: &mut BitStringSegment<T>) -> ParseResult<()> {
        loop {
            match self.peek() {
                Some(Token::Big) => {
                    self.advance();
                    segment.endianness = BitEndianness::Big;
                }
                Some(Token::Little) => {
                    self.advance();
                    segment.endianness = BitEndianness::Little;
                }
                Some(Token::Signed) => {
                    self.advance();
                    segment.signedness = BitSignedness::Signed;
                }
                Some(Token::Unsigned) => {
                    self.advance();
                    segment.signedness = BitSignedness::Unsigned;
                }
                Some(Token::Integer) => {
                    self.advance();
                    segment.segment_type = BitSegmentType::Integer;
                }
                Some(Token::Float) => {
                    self.advance();
                    segment.segment_type = BitSegmentType::Float;
                }
                Some(Token::BinaryKw) | Some(Token::Bytes) => {
                    self.advance();
                    segment.segment_type = BitSegmentType::Binary;
                }
                Some(Token::Utf8) => {
                    self.advance();
                    segment.segment_type = BitSegmentType::Utf8;
                }
                _ => break,
            }

            // Check for more specifiers separated by `-`
            if self.check(&Token::Minus) {
                self.advance();
            } else {
                break;
            }
        }
        Ok(())
    }

    /// Parse an if expression.
    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::If)?;
        let cond = self.parse_expr()?;
        let then_block = self.parse_block()?;

        let else_block = if self.check(&Token::Else) {
            self.advance();
            if self.check(&Token::If) {
                // else if
                let else_if = self.parse_if_expr()?;
                Some(Block {
                    stmts: Vec::new(),
                    expr: Some(Box::new(else_if)),
                })
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };

        Ok(Expr::If {
            cond: Box::new(cond),
            then_block,
            else_block,
        })
    }

    /// Parse a match expression.
    fn parse_match_expr(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::Match)?;
        let expr = self.parse_expr()?;
        self.expect(&Token::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            arms.push(self.parse_match_arm()?);

            // Optional comma between arms
            if self.check(&Token::Comma) {
                self.advance();
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(Expr::Match {
            expr: Box::new(expr),
            arms,
        })
    }

    /// Parse a receive expression.
    fn parse_receive_expr(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::Receive)?;
        self.expect(&Token::LBrace)?;

        let mut arms = Vec::new();
        let mut timeout = None;

        while !self.check(&Token::RBrace) && !self.is_at_end() {
            // Check for 'after' timeout clause
            if self.check(&Token::After) {
                self.advance();
                let timeout_expr = self.parse_expr()?;
                self.expect(&Token::FatArrow)?;
                let timeout_block = self.parse_block()?;
                timeout = Some((Box::new(timeout_expr), timeout_block));
                break;
            }

            arms.push(self.parse_match_arm()?);

            if self.check(&Token::Comma) {
                self.advance();
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(Expr::Receive { arms, timeout })
    }

    /// Parse a spawn expression.
    fn parse_spawn_expr(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::Spawn)?;

        // Check for closure syntax: spawn || { ... }
        if self.check(&Token::OrOr) {
            self.advance();
            let block = self.parse_block()?;
            return Ok(Expr::SpawnClosure(block));
        }

        // Otherwise it's spawn expr
        let expr = self.parse_postfix_expr()?;
        Ok(Expr::Spawn(Box::new(expr)))
    }

    /// Parse a match arm.
    fn parse_match_arm(&mut self) -> ParseResult<MatchArm> {
        let pattern = self.parse_pattern()?;

        let guard = if self.check(&Token::If) {
            self.advance();
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        self.expect(&Token::FatArrow)?;
        let body = self.parse_expr()?;

        Ok(MatchArm {
            pattern,
            guard,
            body,
        })
    }

    /// Parse a pattern.
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        // Wildcard
        if self.check(&Token::Underscore) {
            self.advance();
            return Ok(Pattern::Wildcard);
        }

        // Literals
        if let Some(Token::Int(n)) = self.peek().cloned() {
            self.advance();
            return Ok(Pattern::Int(n));
        }

        if let Some(Token::String(s)) = self.peek().cloned() {
            self.advance();
            return Ok(Pattern::String(s));
        }

        if let Some(Token::Atom(a)) = self.peek().cloned() {
            self.advance();
            return Ok(Pattern::Atom(a));
        }

        if let Some(Token::QuotedAtom(a)) = self.peek().cloned() {
            self.advance();
            return Ok(Pattern::Atom(a));
        }

        if self.check(&Token::True) {
            self.advance();
            return Ok(Pattern::Bool(true));
        }

        if self.check(&Token::False) {
            self.advance();
            return Ok(Pattern::Bool(false));
        }

        // Identifier pattern or module-qualified enum pattern
        if let Some(Token::Ident(name)) = self.peek().cloned() {
            self.advance();

            // Check for module-qualified path: mod::Type::Variant
            if self.check(&Token::ColonColon) {
                self.advance();
                let type_name = self.expect_type_ident()?;

                // Now we have mod::Type, expect ::Variant
                if self.check(&Token::ColonColon) {
                    self.advance();
                    let variant = self.expect_type_ident()?;

                    let fields = if self.check(&Token::LParen) {
                        self.advance();
                        let mut fs = Vec::new();
                        if !self.check(&Token::RParen) {
                            loop {
                                fs.push(self.parse_pattern()?);
                                if !self.check(&Token::Comma) {
                                    break;
                                }
                                self.advance();
                            }
                        }
                        self.expect(&Token::RParen)?;
                        fs
                    } else {
                        Vec::new()
                    };

                    return Ok(Pattern::Enum {
                        name: format!("{}::{}", name, type_name),
                        variant,
                        fields,
                    });
                } else {
                    // Just mod::Type without variant - could be struct pattern
                    let full_name = format!("{}::{}", name, type_name);
                    if self.check(&Token::LBrace) {
                        // Module-qualified struct pattern
                        self.advance();
                        let mut fields = Vec::new();
                        while !self.check(&Token::RBrace) && !self.is_at_end() {
                            if self.check(&Token::DotDot) {
                                self.advance();
                                break;
                            }
                            let field_name = self.expect_ident()?;
                            let field_pattern = if self.check(&Token::Colon) {
                                self.advance();
                                self.parse_pattern()?
                            } else {
                                Pattern::Ident(field_name.clone())
                            };
                            fields.push((field_name, field_pattern));
                            if self.check(&Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.expect(&Token::RBrace)?;
                        return Ok(Pattern::Struct { name: full_name, fields });
                    }
                    // Just a type path without variant - treat as unit enum variant
                    let span = self.current_span();
                    return Err(ParseError::new("expected `::` or `{` after module-qualified type", span));
                }
            }

            return Ok(Pattern::Ident(name));
        }

        // Self keyword as identifier pattern (for impl methods)
        if self.check(&Token::SelfKw) {
            self.advance();
            return Ok(Pattern::Ident("self".to_string()));
        }

        // Type identifier (struct or enum pattern)
        if let Some(Token::TypeIdent(name)) = self.peek().cloned() {
            self.advance();

            // Enum variant: Name::Variant or Name::Variant(...)
            if self.check(&Token::ColonColon) {
                self.advance();
                let variant = self.expect_type_ident()?;

                let fields = if self.check(&Token::LParen) {
                    self.advance();
                    let mut fs = Vec::new();
                    if !self.check(&Token::RParen) {
                        loop {
                            fs.push(self.parse_pattern()?);
                            if !self.check(&Token::Comma) {
                                break;
                            }
                            self.advance();
                        }
                    }
                    self.expect(&Token::RParen)?;
                    fs
                } else {
                    Vec::new()
                };

                return Ok(Pattern::Enum {
                    name,
                    variant,
                    fields,
                });
            }

            // Struct pattern: Name { field: pattern, ... } or Name { field, ... } or Name { field, .. }
            if self.check(&Token::LBrace) {
                self.advance();
                let mut fields = Vec::new();
                while !self.check(&Token::RBrace) && !self.is_at_end() {
                    // Check for rest pattern `..` (ignores remaining fields)
                    if self.check(&Token::DotDot) {
                        self.advance();
                        // `..` must be last, skip to closing brace
                        break;
                    }

                    let field_name = self.expect_ident()?;

                    // Support shorthand: `x` is equivalent to `x: x`
                    let field_pattern = if self.check(&Token::Colon) {
                        self.advance();
                        self.parse_pattern()?
                    } else {
                        // Shorthand: field name becomes binding pattern
                        Pattern::Ident(field_name.clone())
                    };

                    fields.push((field_name, field_pattern));

                    if self.check(&Token::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                self.expect(&Token::RBrace)?;
                return Ok(Pattern::Struct { name, fields });
            }

            // Unqualified enum variant pattern: Variant(...)
            if self.check(&Token::LParen) {
                self.advance();
                let mut fields = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        fields.push(self.parse_pattern()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(&Token::RParen)?;
                return Ok(Pattern::Enum {
                    name: String::new(), // No type qualifier
                    variant: name,
                    fields,
                });
            }

            // Just a type name as pattern (unit enum variant)
            return Ok(Pattern::Enum {
                name: String::new(),
                variant: name,
                fields: vec![],
            });
        }

        // Tuple pattern
        if self.check(&Token::LParen) {
            self.advance();
            if self.check(&Token::RParen) {
                self.advance();
                return Ok(Pattern::Tuple(Vec::new()));
            }

            let mut elements = vec![self.parse_pattern()?];
            while self.check(&Token::Comma) {
                self.advance();
                if self.check(&Token::RParen) {
                    break;
                }
                elements.push(self.parse_pattern()?);
            }
            self.expect(&Token::RParen)?;
            return Ok(Pattern::Tuple(elements));
        }

        // List pattern
        if self.check(&Token::LBracket) {
            self.advance();
            if self.check(&Token::RBracket) {
                self.advance();
                return Ok(Pattern::List(Vec::new()));
            }

            let first = self.parse_pattern()?;

            // Check for cons pattern: [head | tail]
            if self.check(&Token::Pipe) {
                self.advance();
                let tail = self.parse_pattern()?;
                self.expect(&Token::RBracket)?;
                return Ok(Pattern::ListCons {
                    head: Box::new(first),
                    tail: Box::new(tail),
                });
            }

            // Regular list pattern
            let mut elements = vec![first];
            while self.check(&Token::Comma) {
                self.advance();
                if self.check(&Token::RBracket) {
                    break;
                }
                elements.push(self.parse_pattern()?);
            }
            self.expect(&Token::RBracket)?;
            return Ok(Pattern::List(elements));
        }

        // Binary/bit string pattern: <<segments>>
        if self.check(&Token::LtLt) {
            return self.parse_bitstring_pattern();
        }

        let span = self.current_span();
        Err(ParseError::new("expected pattern", span))
    }

    /// Parse a bit string pattern: `<<A:8, B:16/little, Rest/binary>>`
    fn parse_bitstring_pattern(&mut self) -> ParseResult<Pattern> {
        self.expect(&Token::LtLt)?;

        let mut segments = Vec::new();

        // Handle empty binary pattern: <<>>
        if self.check(&Token::GtGt) {
            self.advance();
            return Ok(Pattern::BitString(segments));
        }

        loop {
            let segment = self.parse_bitstring_segment_pattern()?;
            segments.push(segment);

            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(&Token::GtGt)?;
        Ok(Pattern::BitString(segments))
    }

    /// Parse a single bit string pattern segment: `pattern:size/specifiers`
    fn parse_bitstring_segment_pattern(&mut self) -> ParseResult<BitStringSegment<Box<Pattern>>> {
        // Parse the pattern (simplified: just literals, identifiers, and wildcards)
        let pattern = if self.check(&Token::Underscore) {
            self.advance();
            Pattern::Wildcard
        } else if let Some(Token::Int(n)) = self.peek().cloned() {
            self.advance();
            Pattern::Int(n)
        } else if let Some(Token::Ident(name)) = self.peek().cloned() {
            self.advance();
            Pattern::Ident(name)
        } else {
            let span = self.current_span();
            return Err(ParseError::new("expected pattern in bit string segment", span));
        };

        let mut segment = BitStringSegment::new(Box::new(pattern));

        // Parse optional size: `:size`
        if self.check(&Token::Colon) {
            self.advance();
            // Size in patterns must be a literal integer
            if let Some(Token::Int(n)) = self.peek().cloned() {
                self.advance();
                segment.size = Some(Box::new(Expr::Int(n)));
            } else if let Some(Token::Ident(name)) = self.peek().cloned() {
                self.advance();
                segment.size = Some(Box::new(Expr::Ident(name)));
            } else {
                let span = self.current_span();
                return Err(ParseError::new("expected size in bit string segment", span));
            }
        }

        // Parse optional type specifiers: `/specifier-specifier-...`
        if self.check(&Token::Slash) {
            self.advance();
            self.parse_bitstring_specifiers(&mut segment)?;
        }

        Ok(segment)
    }

    /// Parse optional type parameters with bounds: `<T, U: Display, V: Clone + Debug>`.
    /// Returns empty Vec if no type parameters present.
    fn parse_type_params(&mut self) -> ParseResult<Vec<TypeParam>> {
        if !self.check(&Token::Lt) {
            return Ok(Vec::new());
        }
        self.advance(); // consume '<'

        let mut params = Vec::new();
        loop {
            let name = self.expect_type_ident()?;
            let bounds = if self.check(&Token::Colon) {
                self.advance(); // consume ':'
                self.parse_trait_bounds()?
            } else {
                Vec::new()
            };
            params.push(TypeParam { name, bounds });

            if !self.check(&Token::Comma) {
                break;
            }
            self.advance(); // consume ','
        }

        self.expect(&Token::Gt)?;
        Ok(params)
    }

    /// Parse trait bounds: `Trait1 + Trait2 + Trait3`
    fn parse_trait_bounds(&mut self) -> ParseResult<Vec<String>> {
        let mut bounds = vec![self.expect_type_ident()?];
        while self.check(&Token::Plus) {
            self.advance(); // consume '+'
            bounds.push(self.expect_type_ident()?);
        }
        Ok(bounds)
    }

    /// Parse optional type arguments: `<int, String>`.
    /// Returns empty Vec if no type arguments present.
    fn parse_type_args(&mut self) -> ParseResult<Vec<Type>> {
        if !self.check(&Token::Lt) {
            return Ok(Vec::new());
        }
        self.advance(); // consume '<'

        let mut args = Vec::new();
        loop {
            args.push(self.parse_type()?);

            if !self.check(&Token::Comma) {
                break;
            }
            self.advance(); // consume ','
        }

        self.expect(&Token::Gt)?;
        Ok(args)
    }

    /// Parse a type.
    fn parse_type(&mut self) -> ParseResult<Type> {
        // Primitive types (recognized as identifiers)
        if let Some(Token::Ident(name)) = self.peek().cloned() {
            match name.as_str() {
                "int" => {
                    self.advance();
                    return Ok(Type::Int);
                }
                "string" => {
                    self.advance();
                    return Ok(Type::String);
                }
                "atom" => {
                    self.advance();
                    return Ok(Type::Atom);
                }
                "pid" => {
                    self.advance();
                    return Ok(Type::Pid);
                }
                "bool" => {
                    self.advance();
                    return Ok(Type::Bool);
                }
                "map" => {
                    self.advance();
                    return Ok(Type::Map);
                }
                "any" => {
                    self.advance();
                    return Ok(Type::Any);
                }
                _ => {
                    // Check for module-qualified types: module::Type or module::Type<Args>
                    if self.peek_next() == Some(&Token::ColonColon) {
                        self.advance(); // consume module name
                        self.advance(); // consume ::
                        let type_name = self.expect_type_ident()?;
                        let type_args = self.parse_type_args()?;
                        return Ok(Type::Named {
                            name: format!("{}::{}", name, type_name),
                            type_args,
                        });
                    }
                }
            }
        }

        // Binary keyword as type (since "binary" is tokenized as BinaryKw, not Ident)
        if self.check(&Token::BinaryKw) {
            self.advance();
            return Ok(Type::Binary);
        }

        // Named type (uppercase identifier) with optional type arguments
        if let Some(Token::TypeIdent(name)) = self.peek().cloned() {
            self.advance();

            // Check for Self::AssociatedType syntax
            if name == "Self" && self.check(&Token::ColonColon) {
                self.advance();
                let assoc_name = self.expect_type_ident()?;
                return Ok(Type::AssociatedType {
                    base: "Self".to_string(),
                    name: assoc_name,
                });
            }

            let type_args = self.parse_type_args()?;
            return Ok(Type::Named { name, type_args });
        }

        // Function type: fn(T, U) -> R
        if self.check(&Token::Fn) {
            self.advance();
            self.expect(&Token::LParen)?;

            let mut params = Vec::new();
            if !self.check(&Token::RParen) {
                params.push(self.parse_type()?);
                while self.check(&Token::Comma) {
                    self.advance();
                    if self.check(&Token::RParen) {
                        break;
                    }
                    params.push(self.parse_type()?);
                }
            }
            self.expect(&Token::RParen)?;

            self.expect(&Token::Arrow)?;
            let ret = self.parse_type()?;

            return Ok(Type::Fn {
                params,
                ret: Box::new(ret),
            });
        }

        // Tuple type: ()
        if self.check(&Token::LParen) {
            self.advance();
            if self.check(&Token::RParen) {
                self.advance();
                return Ok(Type::Unit);
            }

            let first = self.parse_type()?;
            if self.check(&Token::Comma) {
                let mut elements = vec![first];
                while self.check(&Token::Comma) {
                    self.advance();
                    if self.check(&Token::RParen) {
                        break;
                    }
                    elements.push(self.parse_type()?);
                }
                self.expect(&Token::RParen)?;
                return Ok(Type::Tuple(elements));
            }

            self.expect(&Token::RParen)?;
            return Ok(first);
        }

        // List type: [T]
        if self.check(&Token::LBracket) {
            self.advance();
            let inner = self.parse_type()?;
            self.expect(&Token::RBracket)?;
            return Ok(Type::List(Box::new(inner)));
        }

        let span = self.current_span();
        Err(ParseError::new("expected type", span))
    }

    // === Helper methods ===

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    /// Peek at the next token (after current).
    fn peek_next(&self) -> Option<&Token> {
        self.tokens.get(self.pos + 1).map(|t| &t.token)
    }

    /// Check if the next token is `<` (for turbofish disambiguation).
    fn peek_is_lt(&self) -> bool {
        self.peek_next() == Some(&Token::Lt)
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.pos < self.tokens.len() {
            self.pos += 1;
            self.tokens.get(self.pos - 1).map(|t| &t.token)
        } else {
            None
        }
    }

    fn check(&self, expected: &Token) -> bool {
        self.peek() == Some(expected)
    }

    /// Check if current token is an identifier with specific value (for contextual keywords like `type`)
    fn check_ident(&self, value: &str) -> bool {
        matches!(self.peek(), Some(Token::Ident(s)) if s == value)
    }

    /// Expect a specific identifier value (for contextual keywords)
    fn expect_ident_value(&mut self, value: &str) -> ParseResult<()> {
        if self.check_ident(value) {
            self.advance();
            Ok(())
        } else {
            let span = self.current_span();
            Err(ParseError::new(format!("expected `{}`", value), span))
        }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|t| t.span.clone())
            .unwrap_or(self.source.len()..self.source.len())
    }

    fn expect(&mut self, expected: &Token) -> ParseResult<()> {
        if self.check(expected) {
            self.advance();
            Ok(())
        } else if self.is_at_end() {
            Err(ParseError::unexpected_eof(&expected.to_string()))
        } else {
            let span = self.current_span();
            Err(ParseError::unexpected_token(
                self.peek().unwrap(),
                &expected.to_string(),
                span,
            ))
        }
    }

    fn expect_ident(&mut self) -> ParseResult<String> {
        if let Some(Token::Ident(name)) = self.peek().cloned() {
            self.advance();
            Ok(name)
        } else if self.is_at_end() {
            Err(ParseError::unexpected_eof("identifier"))
        } else {
            let span = self.current_span();
            Err(ParseError::unexpected_token(
                self.peek().unwrap(),
                "identifier",
                span,
            ))
        }
    }

    fn expect_type_ident(&mut self) -> ParseResult<String> {
        if let Some(Token::TypeIdent(name)) = self.peek().cloned() {
            self.advance();
            Ok(name)
        } else if self.is_at_end() {
            Err(ParseError::unexpected_eof("type identifier"))
        } else {
            let span = self.current_span();
            Err(ParseError::unexpected_token(
                self.peek().unwrap(),
                "type identifier",
                span,
            ))
        }
    }

    fn expect_ident_or_type_ident(&mut self) -> ParseResult<String> {
        if let Some(Token::Ident(name)) = self.peek().cloned() {
            self.advance();
            Ok(name)
        } else if let Some(Token::TypeIdent(name)) = self.peek().cloned() {
            self.advance();
            Ok(name)
        } else if self.is_at_end() {
            Err(ParseError::unexpected_eof("identifier"))
        } else {
            let span = self.current_span();
            Err(ParseError::unexpected_token(
                self.peek().unwrap(),
                "identifier",
                span,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Count the number of prelude items (Option, Result) injected into a module.
    /// Prelude items are always at the start, in order: Option (if present), Result (if present).
    fn prelude_count(module: &Module) -> usize {
        let mut count = 0;
        let mut iter = module.items.iter();

        // Helper to check type param names
        fn type_param_names(params: &[TypeParam]) -> Vec<&str> {
            params.iter().map(|p| p.name.as_str()).collect()
        }

        // Check for prelude Option at position 0
        if let Some(Item::Enum(e)) = iter.next() {
            if e.name == "Option"
                && !e.is_pub
                && e.variants.len() == 2
                && e.variants[0].name == "Some"
                && e.variants[1].name == "None"
                && type_param_names(&e.type_params) == vec!["T"]
            {
                count += 1;
                // Check for prelude Result at position 1
                if let Some(Item::Enum(e)) = iter.next() {
                    if e.name == "Result"
                        && !e.is_pub
                        && e.variants.len() == 2
                        && e.variants[0].name == "Ok"
                        && e.variants[1].name == "Err"
                        && type_param_names(&e.type_params) == vec!["T", "E"]
                    {
                        count += 1;
                    }
                }
            } else if e.name == "Result"
                && !e.is_pub
                && e.variants.len() == 2
                && e.variants[0].name == "Ok"
                && e.variants[1].name == "Err"
                && type_param_names(&e.type_params) == vec!["T", "E"]
            {
                // Only Result was injected (module already defines Option)
                count += 1;
            }
        }
        count
    }

    /// Get the first non-prelude item from a module
    fn first_user_item(module: &Module) -> &Item {
        let skip = prelude_count(module);
        &module.items[skip]
    }

    /// Get user items (non-prelude) from a module
    fn user_items(module: &Module) -> &[Item] {
        let skip = prelude_count(module);
        &module.items[skip..]
    }

    #[test]
    fn test_parse_simple_function() {
        let source = r#"
            mod test {
                fn add(x: int, y: int) -> int {
                    x + y
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        assert_eq!(module.name, "test");
        assert_eq!(user_items(&module).len(), 1);

        if let Item::Function(f) = first_user_item(&module) {
            assert_eq!(f.name, "add");
            assert_eq!(f.params.len(), 2);
            assert!(!f.is_pub);
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_struct() {
        let source = r#"
            mod test {
                pub struct Point {
                    x: int,
                    y: int,
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Struct(s) = first_user_item(&module) {
            assert_eq!(s.name, "Point");
            assert_eq!(s.fields.len(), 2);
            assert!(s.is_pub);
        } else {
            panic!("expected struct");
        }
    }

    #[test]
    fn test_parse_enum() {
        let source = r#"
            mod test {
                enum Option {
                    Some(int),
                    None,
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Note: prelude injects Option, so test module has a duplicate
        // We use index 2 to skip prelude Option/Result
        if let Item::Enum(e) = first_user_item(&module) {
            assert_eq!(e.name, "Option");
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].name, "Some");
            assert_eq!(e.variants[0].fields.len(), 1);
            assert_eq!(e.variants[1].name, "None");
            assert_eq!(e.variants[1].fields.len(), 0);
        } else {
            panic!("expected enum");
        }
    }

    #[test]
    fn test_parse_let_and_if() {
        let source = r#"
            mod test {
                fn foo() {
                    let x = 42;
                    if x > 0 {
                        x
                    } else {
                        0
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            assert_eq!(f.body.stmts.len(), 1);
            assert!(f.body.expr.is_some());
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_match() {
        let source = r#"
            mod test {
                fn foo(x: int) {
                    match x {
                        0 => :zero,
                        n => :other,
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::Match { arms, .. }) = f.body.expr.as_deref() {
                assert_eq!(arms.len(), 2);
            } else {
                panic!("expected match expr");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_receive() {
        let source = r#"
            mod test {
                fn loop_() {
                    receive {
                        :ping => :pong,
                        after 1000 => {
                            :timeout
                        }
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::Receive { arms, timeout }) = f.body.expr.as_deref() {
                assert_eq!(arms.len(), 1);
                assert!(timeout.is_some());
            } else {
                panic!("expected receive expr");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_spawn() {
        let source = r#"
            mod test {
                fn start() {
                    spawn || {
                        loop_()
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::SpawnClosure(_)) = f.body.expr.as_deref() {
                // ok
            } else {
                panic!("expected spawn closure");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_list_cons_pattern() {
        let source = r#"
            mod test {
                fn head(list: [int]) -> int {
                    match list {
                        [h | _] => h,
                        [] => 0,
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::Match { arms, .. }) = f.body.expr.as_deref() {
                if let Pattern::ListCons { .. } = &arms[0].pattern {
                    // ok
                } else {
                    panic!("expected list cons pattern");
                }
            } else {
                panic!("expected match");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_struct_pattern() {
        let source = r#"
            mod test {
                struct Point { x: int, y: int }

                fn get_x(p: Point) -> int {
                    match p {
                        Point { x, y: _ } => x,
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = &user_items(&module)[1] {
            if let Some(Expr::Match { arms, .. }) = f.body.expr.as_deref() {
                if let Pattern::Struct { name, fields } = &arms[0].pattern {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].0, "x");
                    assert_eq!(fields[1].0, "y");
                    // x uses shorthand (becomes Ident("x"))
                    if let Pattern::Ident(n) = &fields[0].1 {
                        assert_eq!(n, "x");
                    } else {
                        panic!("expected ident pattern for shorthand");
                    }
                    // y: _ uses wildcard
                    assert!(matches!(fields[1].1, Pattern::Wildcard));
                } else {
                    panic!("expected struct pattern");
                }
            } else {
                panic!("expected match");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_struct_pattern_rest() {
        // Test the `..` rest pattern
        let source = r#"
            mod test {
                struct Point { x: int, y: int, z: int }

                fn get_x(p: Point) -> int {
                    match p {
                        Point { x, .. } => x,
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = &user_items(&module)[1] {
            if let Some(Expr::Match { arms, .. }) = f.body.expr.as_deref() {
                if let Pattern::Struct { name, fields } = &arms[0].pattern {
                    assert_eq!(name, "Point");
                    // Only x is captured, .. ignores y and z
                    assert_eq!(fields.len(), 1);
                    assert_eq!(fields[0].0, "x");
                } else {
                    panic!("expected struct pattern");
                }
            } else {
                panic!("expected match");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_rust_keywords_as_function_names() {
        // This isn't Rust - we can use Rust keywords freely
        let source = r#"
            mod test {
                fn loop(n: int) -> int {
                    if n == 0 {
                        0
                    } else {
                        loop(n - 1)
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            assert_eq!(f.name, "loop");
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_generic_enum() {
        let source = r#"
            mod test {
                enum Option<T> {
                    Some(T),
                    None,
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Use first_user_item since prelude Option won't be added (module defines Option)
        if let Item::Enum(e) = first_user_item(&module) {
            assert_eq!(e.name, "Option");
            assert_eq!(e.type_params.len(), 1);
            assert_eq!(e.type_params[0].name, "T");
            assert!(e.type_params[0].bounds.is_empty());
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].name, "Some");
            // The field type should be a TypeVar "T"
            if let Type::Named { name, type_args } = &e.variants[0].fields[0] {
                assert_eq!(name, "T");
                assert!(type_args.is_empty());
            } else {
                panic!("expected Named type for T");
            }
        } else {
            panic!("expected enum");
        }
    }

    #[test]
    fn test_parse_generic_result() {
        let source = r#"
            mod test {
                enum Result<T, E> {
                    Ok(T),
                    Err(E),
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Prelude adds Option at index 0, user's Result is at index 1
        // (Can't use first_user_item because user's Result matches prelude signature)
        if let Item::Enum(e) = &module.items[1] {
            assert_eq!(e.name, "Result");
            assert_eq!(e.type_params.len(), 2);
            assert_eq!(e.type_params[0].name, "T");
            assert_eq!(e.type_params[1].name, "E");
            assert_eq!(e.variants.len(), 2);
        } else {
            panic!("expected enum");
        }
    }

    #[test]
    fn test_parse_generic_function() {
        let source = r#"
            mod test {
                fn map<T, U>(opt: Option<T>, f: Fn) -> Option<U> {
                    match opt {
                        Some(x) => Some(f(x)),
                        None => None,
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            assert_eq!(f.name, "map");
            assert_eq!(f.type_params.len(), 2);
            assert_eq!(f.type_params[0].name, "T");
            assert_eq!(f.type_params[1].name, "U");
            assert_eq!(f.params.len(), 2);

            // First param should have type Option<T>
            if let Type::Named { name, type_args } = &f.params[0].ty {
                assert_eq!(name, "Option");
                assert_eq!(type_args.len(), 1);
            } else {
                panic!("expected Named type");
            }

            // Return type should be Option<U>
            if let Some(Type::Named { name, type_args }) = &f.return_type {
                assert_eq!(name, "Option");
                assert_eq!(type_args.len(), 1);
            } else {
                panic!("expected return type");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_trait_bounds() {
        let source = r#"
            mod test {
                fn bounded<T: Display>(x: T) -> T {
                    x
                }

                fn multi_bound<T: Clone + Debug, U: Display>(a: T, b: U) -> T {
                    a
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // First function: single bound
        if let Item::Function(f) = first_user_item(&module) {
            assert_eq!(f.name, "bounded");
            assert_eq!(f.type_params.len(), 1);
            assert_eq!(f.type_params[0].name, "T");
            assert_eq!(f.type_params[0].bounds, vec!["Display"]);
        } else {
            panic!("expected function");
        }

        // Second function: multiple bounds
        if let Item::Function(f) = &module.items[prelude_count(&module) + 1] {
            assert_eq!(f.name, "multi_bound");
            assert_eq!(f.type_params.len(), 2);
            assert_eq!(f.type_params[0].name, "T");
            assert_eq!(f.type_params[0].bounds, vec!["Clone", "Debug"]);
            assert_eq!(f.type_params[1].name, "U");
            assert_eq!(f.type_params[1].bounds, vec!["Display"]);
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_turbofish() {
        let source = r#"
            mod test {
                fn main() -> int {
                    let x = start::<Counter>();
                    let y = call::<int, String>(x);
                    0
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            assert_eq!(f.name, "main");

            // Check first statement: let x = start::<Counter>();
            if let Stmt::Let { value, .. } = &f.body.stmts[0] {
                if let Expr::Call {
                    func, type_args, ..
                } = value
                {
                    if let Expr::Ident(name) = func.as_ref() {
                        assert_eq!(name, "start");
                    } else {
                        panic!("expected ident");
                    }
                    assert_eq!(type_args.len(), 1);
                    if let Type::Named { name, .. } = &type_args[0] {
                        assert_eq!(name, "Counter");
                    }
                } else {
                    panic!("expected call");
                }
            }

            // Check second statement: let y = call::<int, String>(x);
            if let Stmt::Let { value, .. } = &f.body.stmts[1] {
                if let Expr::Call { type_args, .. } = value {
                    assert_eq!(type_args.len(), 2);
                }
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_generic_struct() {
        let source = r#"
            mod test {
                struct Pair<A, B> {
                    first: A,
                    second: B,
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Struct(s) = first_user_item(&module) {
            assert_eq!(s.name, "Pair");
            assert_eq!(s.type_params.len(), 2);
            assert_eq!(s.type_params[0].name, "A");
            assert_eq!(s.type_params[1].name, "B");
            assert_eq!(s.fields.len(), 2);
        } else {
            panic!("expected struct");
        }
    }

    #[test]
    fn test_parse_nested_generic_type() {
        let source = r#"
            mod test {
                fn nested(x: Result<Option<int>, String>) -> int {
                    0
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            // Param type should be Result<Option<int>, String>
            if let Type::Named { name, type_args } = &f.params[0].ty {
                assert_eq!(name, "Result");
                assert_eq!(type_args.len(), 2);

                // First arg should be Option<int>
                if let Type::Named { name, type_args } = &type_args[0] {
                    assert_eq!(name, "Option");
                    assert_eq!(type_args.len(), 1);
                    assert_eq!(type_args[0], Type::Int);
                } else {
                    panic!("expected Option<int>");
                }
            } else {
                panic!("expected Result type");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_binary_literal() {
        let source = r#"
        mod test {
            pub fn make_binary() -> int {
                let x = <<1, 2, 3>>;
                0
            }
        }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Stmt::Let { value, .. } = &f.body.stmts[0] {
                if let Expr::BitString(segments) = value {
                    assert_eq!(segments.len(), 3);
                    // First segment should be Int(1)
                    if let Expr::Int(1) = segments[0].value.as_ref() {
                        // ok
                    } else {
                        panic!("expected Int(1)");
                    }
                } else {
                    panic!("expected BitString");
                }
            } else {
                panic!("expected Let");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_binary_with_size() {
        let source = r#"
        mod test {
            pub fn sized_binary() -> int {
                let x = <<4660:16>>;
                0
            }
        }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Stmt::Let { value, .. } = &f.body.stmts[0] {
                if let Expr::BitString(segments) = value {
                    assert_eq!(segments.len(), 1);
                    // Check that size is 16
                    if let Some(size) = &segments[0].size {
                        if let Expr::Int(16) = size.as_ref() {
                            // ok
                        } else {
                            panic!("expected size 16");
                        }
                    } else {
                        panic!("expected size");
                    }
                } else {
                    panic!("expected BitString");
                }
            } else {
                panic!("expected Let");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_binary_with_specifiers() {
        let source = r#"
        mod test {
            pub fn specified_binary() -> int {
                let x = <<value:16/little-signed>>;
                0
            }
        }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Stmt::Let { value, .. } = &f.body.stmts[0] {
                if let Expr::BitString(segments) = value {
                    assert_eq!(segments.len(), 1);
                    assert_eq!(segments[0].endianness, BitEndianness::Little);
                    assert_eq!(segments[0].signedness, BitSignedness::Signed);
                } else {
                    panic!("expected BitString");
                }
            } else {
                panic!("expected Let");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_binary_pattern() {
        let source = r#"
        mod test {
            pub fn match_binary(data: int) -> int {
                match data {
                    <<a:8, b:16>> => a,
                    _ => 0,
                }
            }
        }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        let Item::Function(f) = first_user_item(&module) else {
            panic!("expected function");
        };
        let Some(ref expr) = f.body.expr else {
            panic!("expected expression");
        };
        let Expr::Match { arms, .. } = expr.as_ref() else {
            panic!("expected Match");
        };
        let Pattern::BitString(segments) = &arms[0].pattern else {
            panic!("expected BitString pattern");
        };

        assert_eq!(segments.len(), 2);

        // First segment binds 'a' with 8 bits
        let Pattern::Ident(name) = segments[0].value.as_ref() else {
            panic!("expected Ident pattern");
        };
        assert_eq!(name, "a");

        let Some(size) = &segments[0].size else {
            panic!("expected size");
        };
        let Expr::Int(8) = size.as_ref() else {
            panic!("expected size 8");
        };
    }

    #[test]
    fn test_parse_empty_binary() {
        let source = r#"
        mod test {
            pub fn empty() -> int {
                let x = <<>>;
                0
            }
        }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Stmt::Let { value, .. } = &f.body.stmts[0] {
                if let Expr::BitString(segments) = value {
                    assert_eq!(segments.len(), 0);
                } else {
                    panic!("expected BitString");
                }
            } else {
                panic!("expected Let");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_binary_example_file() {
        let source = include_str!("../../examples/binary_example.dream");
        let mut parser = Parser::new(source);
        let module = parser.parse_module().expect("binary_example.dream should parse successfully");

        // Should have 10 functions + 2 prelude items
        assert_eq!(user_items(&module).len(), 10);
        assert_eq!(module.name, "binary_example");
    }

    #[test]
    fn test_parse_impl_block() {
        let source = r#"
            mod test {
                struct Point {
                    x: int,
                    y: int,
                }

                impl Point {
                    pub fn new(x: int, y: int) -> Point {
                        Point { x: x, y: y }
                    }

                    fn private_method() -> int {
                        42
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().expect("impl block should parse");

        // Should have struct and impl (plus prelude items)
        assert_eq!(user_items(&module).len(), 2);

        // Check the impl block (index 1 in user items)
        if let Item::Impl(impl_block) = &user_items(&module)[1] {
            assert_eq!(impl_block.type_name, "Point");
            assert_eq!(impl_block.methods.len(), 2);
            assert_eq!(impl_block.methods[0].name, "new");
            assert!(impl_block.methods[0].is_pub);
            assert_eq!(impl_block.methods[1].name, "private_method");
            assert!(!impl_block.methods[1].is_pub);
        } else {
            panic!("expected impl block");
        }
    }

    #[test]
    fn test_parse_trait_definition() {
        let source = r#"
            mod test {
                trait Display {
                    fn display(self) -> String;
                    fn format(self, prefix: String) -> String;
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().expect("trait def should parse");

        assert_eq!(user_items(&module).len(), 1);

        if let Item::Trait(trait_def) = first_user_item(&module) {
            assert_eq!(trait_def.name, "Display");
            assert_eq!(trait_def.methods.len(), 2);
            assert_eq!(trait_def.methods[0].name, "display");
            assert_eq!(trait_def.methods[0].params.len(), 1);
            assert!(trait_def.methods[0].return_type.is_some());
            assert_eq!(trait_def.methods[1].name, "format");
            assert_eq!(trait_def.methods[1].params.len(), 2);
        } else {
            panic!("expected trait definition");
        }
    }

    #[test]
    fn test_parse_trait_impl() {
        let source = r#"
            mod test {
                struct Point {
                    x: int,
                    y: int,
                }

                trait Display {
                    fn display(self) -> String;
                }

                impl Display for Point {
                    pub fn display(self) -> String {
                        "Point"
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().expect("trait impl should parse");

        assert_eq!(user_items(&module).len(), 3);

        if let Item::TraitImpl(trait_impl) = &user_items(&module)[2] {
            assert_eq!(trait_impl.trait_name, "Display");
            assert_eq!(trait_impl.type_name, "Point");
            assert_eq!(trait_impl.methods.len(), 1);
            assert_eq!(trait_impl.methods[0].name, "display");
            assert!(trait_impl.methods[0].is_pub);
        } else {
            panic!("expected trait impl");
        }
    }
}
