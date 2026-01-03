//! Recursive descent parser.

use crate::compiler::ast::*;
use crate::compiler::error::{ParseError, ParseResult};
use crate::compiler::lexer::{Lexer, Span, SpannedToken};
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
        Ok(Module { name, items })
    }

    /// Parse a source file as an implicit module.
    /// Used for lib.tb, main.tb, or any file loaded via `mod foo;`.
    /// The module name is derived from the filename.
    pub fn parse_file(&mut self, module_name: &str) -> ParseResult<Module> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            items.push(self.parse_item()?);
        }

        Ok(Module {
            name: module_name.to_string(),
            items,
        })
    }

    /// Parse a top-level item.
    pub fn parse_item(&mut self) -> ParseResult<Item> {
        // Use statements don't have pub modifier
        if self.check(&Token::Use) {
            return self.parse_use_decl();
        }

        // Impl blocks don't have pub modifier (methods inside can be pub)
        if self.check(&Token::Impl) {
            return self.parse_impl_block();
        }

        let is_pub = self.check(&Token::Pub);
        if is_pub {
            self.advance();
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
                "expected `fn`, `struct`, `enum`, `mod`, `impl`, or `use`",
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

    /// Parse an impl block: `impl Point { fn new() -> Point { ... } }`
    fn parse_impl_block(&mut self) -> ParseResult<Item> {
        use crate::compiler::ast::ImplBlock;

        self.expect(&Token::Impl)?;

        // Parse the type name (must be a TypeIdent like Point, not lowercase)
        let type_name = self.expect_type_ident()?;

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

        Ok(Item::Impl(ImplBlock { type_name, methods }))
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

        let return_type = if self.check(&Token::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(Function {
            name,
            type_params,
            params,
            return_type,
            body,
            is_pub,
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
        self.expect(&Token::Colon)?;
        let ty = self.parse_type()?;
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

        self.parse_send_expr()
    }

    /// Parse send expressions (pid ! message).
    fn parse_send_expr(&mut self) -> ParseResult<Expr> {
        let left = self.parse_postfix_expr()?;

        if self.check(&Token::Bang) {
            self.advance();
            let msg = self.parse_postfix_expr()?;
            return Ok(Expr::Send {
                to: Box::new(left),
                msg: Box::new(msg),
            });
        }

        Ok(left)
    }

    /// Parse postfix expressions (calls, field access).
    fn parse_postfix_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
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
                    args,
                };
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

                expr = match expr {
                    Expr::Ident(first) => Expr::Path {
                        segments: vec![first, segment],
                    },
                    Expr::Path { mut segments } => {
                        segments.push(segment);
                        Expr::Path { segments }
                    }
                    _ => {
                        let span = self.current_span();
                        return Err(ParseError::new("invalid path expression", span));
                    }
                };
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

        if let Some(Token::Atom(a)) = self.peek().cloned() {
            self.advance();
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

        if self.check(&Token::True) {
            self.advance();
            return Ok(Pattern::Bool(true));
        }

        if self.check(&Token::False) {
            self.advance();
            return Ok(Pattern::Bool(false));
        }

        // Identifier pattern
        if let Some(Token::Ident(name)) = self.peek().cloned() {
            self.advance();
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

            // Struct pattern: Name { field: pattern, ... }
            if self.check(&Token::LBrace) {
                self.advance();
                let mut fields = Vec::new();
                while !self.check(&Token::RBrace) && !self.is_at_end() {
                    let field_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let field_pattern = self.parse_pattern()?;
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

    /// Parse optional type parameters: `<T, U>`.
    /// Returns empty Vec if no type parameters present.
    fn parse_type_params(&mut self) -> ParseResult<Vec<String>> {
        if !self.check(&Token::Lt) {
            return Ok(Vec::new());
        }
        self.advance(); // consume '<'

        let mut params = Vec::new();
        loop {
            let name = self.expect_type_ident()?;
            params.push(name);

            if !self.check(&Token::Comma) {
                break;
            }
            self.advance(); // consume ','
        }

        self.expect(&Token::Gt)?;
        Ok(params)
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
                _ => {}
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
            let type_args = self.parse_type_args()?;
            return Ok(Type::Named { name, type_args });
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
        assert_eq!(module.items.len(), 1);

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Struct(s) = &module.items[0] {
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

        if let Item::Enum(e) = &module.items[0] {
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

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Function(f) = &module.items[0] {
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
    fn test_parse_send() {
        let source = r#"
            mod test {
                fn send_msg(pid: pid) {
                    pid ! :hello;
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = &module.items[0] {
            if let Stmt::Expr(Expr::Send { .. }) = &f.body.stmts[0] {
                // ok
            } else {
                panic!("expected send expr");
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

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Enum(e) = &module.items[0] {
            assert_eq!(e.name, "Option");
            assert_eq!(e.type_params, vec!["T".to_string()]);
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

        if let Item::Enum(e) = &module.items[0] {
            assert_eq!(e.name, "Result");
            assert_eq!(e.type_params, vec!["T".to_string(), "E".to_string()]);
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

        if let Item::Function(f) = &module.items[0] {
            assert_eq!(f.name, "map");
            assert_eq!(f.type_params, vec!["T".to_string(), "U".to_string()]);
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

        if let Item::Struct(s) = &module.items[0] {
            assert_eq!(s.name, "Pair");
            assert_eq!(s.type_params, vec!["A".to_string(), "B".to_string()]);
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

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Function(f) = &module.items[0] {
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

        if let Item::Function(f) = &module.items[0] {
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

        let Item::Function(f) = &module.items[0] else {
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

        if let Item::Function(f) = &module.items[0] {
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
        let source = include_str!("../../examples/binary.tb");
        let mut parser = Parser::new(source);
        let module = parser.parse_module().expect("binary.tb should parse successfully");

        // Should have 10 functions
        assert_eq!(module.items.len(), 10);
        assert_eq!(module.name, "binaries");
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

        // Should have struct and impl
        assert_eq!(module.items.len(), 2);

        // Check the impl block
        if let Item::Impl(impl_block) = &module.items[1] {
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
}
