//! Recursive descent parser.

use crate::compiler::ast::*;
use crate::compiler::error::{ParseError, ParseResult};
use crate::compiler::lexer::{Lexer, Span, SpannedToken};
use crate::compiler::prelude::prelude_items_for_module;
use crate::compiler::token::{
    has_interpolation, parse_interpolated_string, process_escapes, LexStringPart, Token,
};

/// Recursive descent parser.
pub struct Parser<'source> {
    tokens: Vec<SpannedToken>,
    pos: usize,
    source: &'source str,
    /// Tracks if we have a pending `>` from splitting a `>>` token.
    /// This enables parsing nested generics like `Option<Option<T>>`.
    pending_gt: bool,
    /// Tracks if we're currently parsing inside a quote block.
    /// When true, allows `#ident` (unquote) in type name positions.
    in_quote: bool,
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
            pending_gt: false,
            in_quote: false,
        }
    }

    /// Parse a complete module.
    /// Supports fully qualified module paths like `mod my_app::users::auth { }`.
    pub fn parse_module(&mut self) -> ParseResult<Module> {
        self.expect(&Token::Mod)?;
        let name = self.parse_module_path()?;
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
            attrs: vec![],
            name,
            items: all_items,
            source: Some(self.source.to_string()),
            source_path: None,
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
            attrs: vec![],
            name: module_name.to_string(),
            items: all_items,
            source: Some(self.source.to_string()),
            source_path: None,
        })
    }

    /// Parse a source file that may contain multiple wrapped modules.
    /// Returns all modules found in the file.
    /// If the file contains file-based items (not wrapped in mod {}), returns a single module.
    /// If the file contains top-level expressions after modules, creates a `__script__` module.
    pub fn parse_file_modules(&mut self, fallback_name: &str) -> ParseResult<Vec<Module>> {
        let mut modules = Vec::new();

        // Check if this file has wrapped modules
        if self.check(&Token::Mod) && self.peek_is_wrapped_module() {
            // Parse all wrapped modules in the file
            while !self.is_at_end() {
                if self.check(&Token::Mod) && self.peek_is_wrapped_module() {
                    modules.push(self.parse_module()?);
                } else {
                    // Non-module tokens - parse as script expressions
                    break;
                }
            }

            // If there are remaining tokens, parse them as script statements
            if !self.is_at_end() {
                let script_module = self.parse_script_module()?;
                modules.push(script_module);
            }
        } else {
            // File-based module: parse items directly
            let module = self.parse_file(fallback_name)?;
            modules.push(module);
        }

        Ok(modules)
    }

    /// Parse top-level statements/expressions into a synthetic `__script__` module.
    /// This enables script-style execution: the `__main__` function is called when the file runs.
    fn parse_script_module(&mut self) -> ParseResult<Module> {
        let start_span = self.current_span();
        let mut stmts = Vec::new();
        let mut expr = None;

        while !self.is_at_end() {
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
                } else if self.is_at_end() {
                    // Trailing expression (final result)
                    expr = Some(Box::new(e));
                } else {
                    // Expression statements that don't need semicolons
                    if Self::is_block_expr(&e) {
                        stmts.push(Stmt::Expr(e));
                    } else {
                        let span = self.current_span();
                        return Err(ParseError::new("expected `;` or end of file", span));
                    }
                }
            }
        }

        // Create the __main__ function with return type `any`
        // so it accepts any expression as the final result
        let main_fn = Function {
            attrs: vec![],
            name: "__main__".to_string(),
            type_params: vec![],
            params: vec![],
            guard: None,
            return_type: Some(Type::Any),
            body: Block { stmts, expr },
            is_pub: true,
            span: Span {
                start: start_span.start,
                end: self.current_span().end,
            },
        };

        Ok(Module {
            attrs: vec![],
            name: "__script__".to_string(),
            items: vec![Item::Function(main_fn)],
            source: Some(self.source.to_string()),
            source_path: None,
        })
    }

    /// Check if the next tokens are `mod <path> {` (wrapped module).
    /// Supports both simple `mod name {` and qualified `mod a::b::c {`.
    fn peek_is_wrapped_module(&self) -> bool {
        // Check: mod <ident> (:: <ident>)* {
        if self.pos >= self.tokens.len() {
            return false;
        }

        // First token must be `mod`
        if !matches!(self.tokens[self.pos].token, Token::Mod) {
            return false;
        }

        // Second token must be an identifier
        let mut idx = self.pos + 1;
        if idx >= self.tokens.len() || !matches!(self.tokens[idx].token, Token::Ident(_)) {
            return false;
        }
        idx += 1;

        // Skip over any `::ident` pairs
        while idx + 1 < self.tokens.len() {
            if matches!(self.tokens[idx].token, Token::ColonColon) {
                if matches!(self.tokens[idx + 1].token, Token::Ident(_)) {
                    idx += 2;
                } else {
                    return false;
                }
            } else {
                break;
            }
        }

        // After the path, we should find `{` for a wrapped module
        idx < self.tokens.len() && matches!(self.tokens[idx].token, Token::LBrace)
    }

    // =========================================================================
    // Attribute Parsing
    // =========================================================================

    /// Parse zero or more attributes: `#[attr1] #[attr2] ...`
    fn parse_attributes(&mut self) -> ParseResult<Vec<Attribute>> {
        let mut attrs = Vec::new();
        while self.check(&Token::HashBracket) {
            attrs.push(self.parse_attribute()?);
        }
        Ok(attrs)
    }

    /// Parse a single attribute: `#[name]`, `#[name(args)]`, or `#[name = "value"]`
    fn parse_attribute(&mut self) -> ParseResult<Attribute> {
        let start = self.current_span().start;
        self.expect(&Token::HashBracket)?;

        let name = self.expect_ident()?;
        let args = self.parse_attribute_args()?;

        let end = self.current_span().end;
        self.expect(&Token::RBracket)?;

        Ok(Attribute {
            name,
            args,
            span: start..end,
        })
    }

    /// Parse attribute arguments after the name: `(args)`, `= "value"`, or nothing
    fn parse_attribute_args(&mut self) -> ParseResult<AttributeArgs> {
        if self.check(&Token::LParen) {
            self.advance();
            let args = self.parse_attribute_arg_list()?;
            self.expect(&Token::RParen)?;
            Ok(AttributeArgs::Parenthesized(args))
        } else if self.check(&Token::Eq) {
            self.advance();
            // Expect a string literal
            if let Some(SpannedToken {
                token: Token::String(s),
                ..
            }) = self.tokens.get(self.pos)
            {
                let value = s.clone();
                self.advance();
                Ok(AttributeArgs::Eq(value))
            } else {
                Err(ParseError::new(
                    "expected string literal after `=` in attribute",
                    self.current_span(),
                ))
            }
        } else {
            Ok(AttributeArgs::None)
        }
    }

    /// Parse a comma-separated list of attribute arguments
    fn parse_attribute_arg_list(&mut self) -> ParseResult<Vec<AttributeArg>> {
        let mut args = Vec::new();

        // Handle empty parens
        if self.check(&Token::RParen) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_attribute_arg()?);

            if self.check(&Token::Comma) {
                self.advance();
                // Allow trailing comma
                if self.check(&Token::RParen) {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(args)
    }

    /// Parse a single attribute argument: `ident`, `key = "value"`, or `func(args)`
    fn parse_attribute_arg(&mut self) -> ParseResult<AttributeArg> {
        // Accept both lowercase identifiers and type identifiers (for derives like Debug, Clone)
        let name = self.expect_ident_or_type_ident()?;

        if self.check(&Token::Eq) {
            // Key-value: `feature = "json"`
            self.advance();
            if let Some(SpannedToken {
                token: Token::String(s),
                ..
            }) = self.tokens.get(self.pos)
            {
                let value = s.clone();
                self.advance();
                Ok(AttributeArg::KeyValue(name, value))
            } else {
                Err(ParseError::new(
                    "expected string literal after `=` in attribute argument",
                    self.current_span(),
                ))
            }
        } else if self.check(&Token::LParen) {
            // Nested: `not(test)` or `all(feature = "a", feature = "b")`
            self.advance();
            let nested_args = self.parse_attribute_arg_list()?;
            self.expect(&Token::RParen)?;
            Ok(AttributeArg::Nested(name, nested_args))
        } else {
            // Simple identifier: `test`
            Ok(AttributeArg::Ident(name))
        }
    }

    /// Parse a top-level item.
    pub fn parse_item(&mut self) -> ParseResult<Item> {
        // Parse any attributes before the item
        let attrs = self.parse_attributes()?;

        // Use statements don't have pub modifier (and don't support attributes currently)
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

        if self.check(&Token::Extern) {
            self.parse_extern_mod(attrs)
        } else if self.check(&Token::Fn) {
            Ok(Item::Function(self.parse_function(is_pub, attrs)?))
        } else if self.check(&Token::Struct) {
            Ok(Item::Struct(self.parse_struct(is_pub, attrs)?))
        } else if self.check(&Token::Enum) {
            Ok(Item::Enum(self.parse_enum(is_pub, attrs)?))
        } else if self.check(&Token::Type) {
            Ok(Item::TypeAlias(self.parse_type_alias(is_pub, attrs)?))
        } else if self.check(&Token::Mod) {
            self.parse_mod_decl(is_pub)
        } else {
            let span = self.current_span();
            Err(ParseError::new(
                "expected `fn`, `struct`, `enum`, `type`, `mod`, `impl`, `trait`, `extern`, or `use`",
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

    /// Parse a type alias: `type Result = :ok | :error;` or `type Result<T> = (:ok, T) | :error;`
    fn parse_type_alias(&mut self, is_pub: bool, attrs: Vec<Attribute>) -> ParseResult<TypeAlias> {
        self.expect(&Token::Type)?;
        let name = self.expect_type_ident()?;

        // Optional type parameters: <T, E>
        let type_params = if self.check(&Token::Lt) {
            self.parse_type_params()?
        } else {
            vec![]
        };

        self.expect(&Token::Eq)?;
        let ty = self.parse_type()?;
        self.expect(&Token::Semi)?;

        Ok(TypeAlias {
            attrs,
            name,
            type_params,
            ty,
            is_pub,
        })
    }

    /// Parse an external module declaration: `extern mod erlang { ... }`
    /// Used in .dreamt files to declare types for FFI modules.
    /// Supports `#[name = "Actual.Module.Name"]` attribute for module name mapping.
    fn parse_extern_mod(&mut self, attrs: Vec<Attribute>) -> ParseResult<Item> {
        self.expect(&Token::Extern)?;
        self.expect(&Token::Mod)?;
        let name = self.expect_ident()?;
        self.expect(&Token::LBrace)?;

        let mut items = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            items.push(self.parse_extern_item()?);
        }
        self.expect(&Token::RBrace)?;

        Ok(Item::ExternMod(ExternMod { attrs, name, items }))
    }

    /// Parse an item inside an extern mod block.
    /// Supports attributes on nested modules and functions.
    fn parse_extern_item(&mut self) -> ParseResult<ExternItem> {
        // Parse any attributes before the item
        let attrs = self.parse_attributes()?;

        if self.check(&Token::Mod) {
            // Nested module: `mod socket { ... }`
            self.expect(&Token::Mod)?;
            let name = self.expect_ident()?;
            self.expect(&Token::LBrace)?;

            let mut items = Vec::new();
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                items.push(self.parse_extern_item()?);
            }
            self.expect(&Token::RBrace)?;

            Ok(ExternItem::Mod(ExternMod { attrs, name, items }))
        } else if self.check(&Token::Type) {
            // Opaque type: `type Socket;` or `type Map<K, V>;`
            // Note: attrs parsed but not stored on ExternType (could be added if needed)
            self.expect(&Token::Type)?;
            let name = self.expect_type_ident()?;
            let type_params = if self.check(&Token::Lt) {
                self.parse_type_params()?
            } else {
                vec![]
            };
            self.expect(&Token::Semi)?;

            Ok(ExternItem::Type(ExternType { name, type_params }))
        } else if self.check(&Token::Fn) {
            // Function declaration: `fn get<K, V>(key: K, map: Map<K, V>) -> V;`
            self.expect(&Token::Fn)?;
            let name = self.expect_ident_or_keyword()?;

            let type_params = if self.check(&Token::Lt) {
                self.parse_type_params()?
            } else {
                vec![]
            };

            self.expect(&Token::LParen)?;
            let mut params = Vec::new();
            while !self.check(&Token::RParen) {
                let param_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let param_type = self.parse_type()?;
                params.push((param_name, param_type));
                if !self.check(&Token::RParen) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RParen)?;

            let return_type = if self.check(&Token::Arrow) {
                self.advance();
                self.parse_type()?
            } else {
                Type::Unit
            };

            self.expect(&Token::Semi)?;

            Ok(ExternItem::Function(ExternFn {
                attrs,
                name,
                type_params,
                params,
                return_type,
            }))
        } else {
            let span = self.current_span();
            Err(ParseError::new(
                "expected `fn`, `type`, or `mod` in extern block",
                span,
            ))
        }
    }

    /// Parse an impl block, trait implementation, or module-level trait declaration.
    /// `impl Point { ... }` or `impl Display for Point { ... }` or `impl mod::Trait;`
    fn parse_impl_or_trait_impl(&mut self) -> ParseResult<Item> {
        self.expect(&Token::Impl)?;

        // Parse the first type name (could be module-qualified like genserver::GenServer)
        let first_name = self.parse_possibly_qualified_type_name()?;

        // Parse optional type arguments for parameterized traits: impl From<int> for ...
        let trait_type_args = self.parse_type_args()?;

        // Check for module-level trait declaration: `impl Trait;` or `impl Trait { type X = T; }`
        if self.check(&Token::Semi) {
            self.advance();
            return Ok(Item::TraitDecl(TraitDecl {
                trait_name: first_name,
                type_bindings: vec![],
            }));
        }

        // Check if this is a trait impl: `impl Trait for Type { ... }` or `impl Trait<T> for Type { ... }`
        if self.check(&Token::For) {
            self.advance();
            let type_name = self.expect_type_ident()?;
            self.expect(&Token::LBrace)?;

            // Parse type bindings and methods inside the trait impl
            let mut type_bindings = Vec::new();
            let mut methods = Vec::new();
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                // Check for type binding: `type Name = ConcreteType;`
                if self.check(&Token::Type) {
                    type_bindings.push(self.parse_type_binding()?);
                    continue;
                }

                // Parse attributes for methods
                let attrs = self.parse_attributes()?;

                // Methods can have pub modifier
                let is_pub = self.check(&Token::Pub);
                if is_pub {
                    self.advance();
                }

                if self.check(&Token::Fn) {
                    methods.push(self.parse_function(is_pub, attrs)?);
                } else {
                    let span = self.current_span();
                    return Err(ParseError::new("expected `fn` or `type` in trait impl", span));
                }
            }

            self.expect(&Token::RBrace)?;

            Ok(Item::TraitImpl(TraitImpl {
                trait_name: first_name,
                trait_type_args,
                type_name,
                type_bindings,
                methods,
            }))
        } else {
            // Could be:
            // - Regular impl block: `impl Type { pub fn ... }`
            // - Module-level trait declaration: `impl Trait { type X = T; }`
            self.expect(&Token::LBrace)?;

            // Peek to see if first item is a type binding (module-level trait declaration)
            // or a function (regular impl block)
            if self.check(&Token::Type) {
                // Module-level trait declaration with type bindings
                let mut type_bindings = Vec::new();
                while !self.check(&Token::RBrace) && !self.is_at_end() {
                    if self.check(&Token::Type) {
                        type_bindings.push(self.parse_type_binding()?);
                    } else {
                        let span = self.current_span();
                        return Err(ParseError::new(
                            "expected `type` binding in module-level trait declaration",
                            span,
                        ));
                    }
                }

                self.expect(&Token::RBrace)?;

                Ok(Item::TraitDecl(TraitDecl {
                    trait_name: first_name,
                    type_bindings,
                }))
            } else {
                // Regular impl block
                let mut methods = Vec::new();
                while !self.check(&Token::RBrace) && !self.is_at_end() {
                    // Parse attributes for methods
                    let attrs = self.parse_attributes()?;

                    // Methods can have pub modifier
                    let is_pub = self.check(&Token::Pub);
                    if is_pub {
                        self.advance();
                    }

                    if self.check(&Token::Fn) {
                        methods.push(self.parse_function(is_pub, attrs)?);
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
    }

    /// Parse a type name that may be module-qualified: `Point` or `genserver::GenServer`
    fn parse_possibly_qualified_type_name(&mut self) -> ParseResult<String> {
        // In quote mode, allow #ident for unquote in type position
        if self.in_quote && self.check(&Token::Hash) {
            self.advance(); // consume #
            let var_name = self.expect_ident()?;
            // Use special marker prefix that codegen recognizes
            return Ok(format!("$UNQUOTE:{}", var_name));
        }

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

    /// Parse a trait definition: `trait Display { ... }` or `trait From<T> { ... }`
    fn parse_trait_def(&mut self) -> ParseResult<Item> {
        self.expect(&Token::Trait)?;
        let name = self.expect_type_ident()?;

        // Parse optional type parameters: trait From<T> { ... }
        let type_params = self.parse_type_params()?;

        self.expect(&Token::LBrace)?;

        let mut associated_types = Vec::new();
        let mut methods = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            // Check for associated type declaration: `type Name;`
            if self.check(&Token::Type) {
                associated_types.push(self.parse_associated_type_decl()?);
            } else {
                methods.push(self.parse_trait_method()?);
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Item::Trait(TraitDef { name, type_params, associated_types, methods }))
    }

    /// Parse an associated type declaration in a trait: `type Name;`
    fn parse_associated_type_decl(&mut self) -> ParseResult<String> {
        self.expect(&Token::Type)?;
        let name = self.expect_type_ident()?;
        self.expect(&Token::Semi)?;
        Ok(name)
    }

    /// Parse a type binding in a trait impl: `type State = int;`
    fn parse_type_binding(&mut self) -> ParseResult<(String, Type)> {
        self.expect(&Token::Type)?;
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

    /// Parse a module path for use declarations.
    /// Handles: `foo`, `foo::bar`, `crate::foo`, `super::foo`, `self::foo`
    fn parse_use_module_path(&mut self) -> ParseResult<ModulePath> {
        // Check for path prefix keywords
        let prefix = if self.check(&Token::Crate) {
            self.advance();
            self.expect(&Token::ColonColon)?;
            PathPrefix::Crate
        } else if self.check(&Token::Super) {
            self.advance();
            self.expect(&Token::ColonColon)?;
            PathPrefix::Super
        } else if self.check(&Token::SelfKw) {
            self.advance();
            self.expect(&Token::ColonColon)?;
            PathPrefix::SelfMod
        } else {
            PathPrefix::None
        };

        // For prefixed paths, segments can be empty (e.g., `use super::foo;` where foo is the item)
        // For non-prefixed paths, we need at least one segment (the module name)
        let mut segments = Vec::new();

        // Check if we should parse the first segment
        // We need lookahead to determine if the current ident is a module segment or the item name
        // It's a module segment if followed by ::ident (another segment or item)
        // It's an item name if followed by ; or as or {
        let should_parse_first = if prefix == PathPrefix::None {
            // Non-prefixed: always need at least one segment
            true
        } else {
            // Prefixed: check if current ident is followed by :: and more
            self.is_module_segment_ahead()
        };

        if should_parse_first {
            segments.push(self.expect_ident()?);

            // Continue parsing segments until we hit the item name
            // After consuming a segment, we're at ::. We need to check if the next
            // ident is a segment (followed by ::) or the final item (followed by ; or as or {)
            while self.check(&Token::ColonColon) && self.has_more_path_after_colon() {
                self.advance(); // consume ::
                segments.push(self.expect_ident()?);
            }
        }

        Ok(ModulePath { prefix, segments })
    }

    /// Check if there's more path segments after the current ::.
    /// Assumes we're positioned at ::. Looks at the ident after :: and what follows it.
    /// Returns true if: :: ident :: (more path)
    /// Returns false if: :: ident (end - item name)
    fn has_more_path_after_colon(&self) -> bool {
        // Current should be ::
        if self.pos >= self.tokens.len() {
            return false;
        }
        if self.tokens[self.pos].token != Token::ColonColon {
            return false;
        }

        // pos+1 should be an ident
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        match &self.tokens[self.pos + 1].token {
            Token::Ident(_) | Token::TypeIdent(_) => {}
            _ => return false,
        }

        // pos+2 should be :: for this to be a segment (not the final item)
        if self.pos + 2 >= self.tokens.len() {
            return false;
        }
        self.tokens[self.pos + 2].token == Token::ColonColon
    }

    /// Check if the current position has a module segment followed by more path.
    /// Returns true if we see: ident :: (ident | * | {)
    /// Returns false if we see: ident ; or ident as
    fn is_module_segment_ahead(&self) -> bool {
        // We need: ident :: something
        // Current should be ident
        if self.pos >= self.tokens.len() {
            return false;
        }
        match &self.tokens[self.pos].token {
            Token::Ident(_) | Token::TypeIdent(_) => {}
            _ => return false,
        }

        // Next should be ::
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        if self.tokens[self.pos + 1].token != Token::ColonColon {
            return false;
        }

        // After :: should be another ident, *, or {
        if self.pos + 2 >= self.tokens.len() {
            return false;
        }
        match &self.tokens[self.pos + 2].token {
            Token::Ident(_) | Token::TypeIdent(_) | Token::Star | Token::LBrace => true,
            _ => false,
        }
    }

    /// Parse a use declaration: `use foo::bar;` or `use foo::{a, b};` or `use foo::*;`
    /// Also handles: `use crate::db::query;`, `use super::helpers::{a, b};`
    fn parse_use_decl(&mut self) -> ParseResult<Item> {
        self.expect(&Token::Use)?;

        let module = self.parse_use_module_path()?;

        // Only expect :: if we have segments (or no prefix)
        // When we have a prefix with no segments (e.g., `super::item`), the :: was already consumed
        if !module.segments.is_empty() || module.prefix == PathPrefix::None {
            self.expect(&Token::ColonColon)?;
        }

        let tree = if self.check(&Token::Star) {
            // Glob import: use foo::*;
            self.advance();
            self.expect(&Token::Semi)?;
            UseTree::Glob { module }
        } else if self.check(&Token::LBrace) {
            // Group import: use foo::{a, b as c, SomeType};
            self.advance();
            let mut items = Vec::new();

            if !self.check(&Token::RBrace) {
                loop {
                    // Accept both lowercase idents and TypeIdents (for types/traits)
                    let name = self.expect_ident_or_type_ident()?;
                    let rename = if self.check(&Token::As) {
                        self.advance();
                        Some(self.expect_ident_or_type_ident()?)
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
            // Single import: use foo::bar; or use foo::Bar; (types/traits)
            let name = self.expect_ident_or_type_ident()?;
            let rename = if self.check(&Token::As) {
                self.advance();
                Some(self.expect_ident_or_type_ident()?)
            } else {
                None
            };
            self.expect(&Token::Semi)?;
            UseTree::Path { module, name, rename }
        };

        Ok(Item::Use(UseDecl { tree }))
    }

    /// Parse a function definition.
    fn parse_function(&mut self, is_pub: bool, attrs: Vec<Attribute>) -> ParseResult<Function> {
        let start = self.current_span().start;
        self.expect(&Token::Fn)?;

        // In quote mode, allow #ident for unquote in function name position
        let name = if self.in_quote && self.check(&Token::Hash) {
            self.advance(); // consume #
            let var_name = self.expect_ident()?;
            format!("$UNQUOTE:{}", var_name)
        } else {
            self.expect_ident()?
        };

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
            attrs,
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
                Pattern::Charlist(_) => Type::Named {
                    name: "string".to_string(),
                    type_args: vec![],
                },
                Pattern::Wildcard => Type::Named {
                    name: "any".to_string(),
                    type_args: vec![],
                },
                // Infer type from enum variant pattern (e.g., Msg::Get → Msg)
                Pattern::Enum { name, .. } => Type::Named {
                    name: name.clone(),
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
    fn parse_struct(&mut self, is_pub: bool, attrs: Vec<Attribute>) -> ParseResult<StructDef> {
        self.expect(&Token::Struct)?;

        // In quote mode, allow #ident for unquote in struct name position
        let name = if self.in_quote && self.check(&Token::Hash) {
            self.advance(); // consume #
            let var_name = self.expect_ident()?;
            format!("$UNQUOTE:{}", var_name)
        } else {
            self.expect_type_ident()?
        };

        // Parse optional type parameters: <T, U>
        let type_params = self.parse_type_params()?;

        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            // In quote mode, allow #ident for unquote in field name position
            let field_name = if self.in_quote && self.check(&Token::Hash) {
                self.advance(); // consume #
                let var_name = self.expect_ident()?;
                format!("$UNQUOTE:{}", var_name)
            } else {
                self.expect_ident()?
            };
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
            attrs,
            name,
            type_params,
            fields,
            is_pub,
        })
    }

    /// Parse an enum definition.
    fn parse_enum(&mut self, is_pub: bool, attrs: Vec<Attribute>) -> ParseResult<EnumDef> {
        self.expect(&Token::Enum)?;
        let name = self.expect_type_ident()?;

        // Parse optional type parameters: <T, E>
        let type_params = self.parse_type_params()?;

        self.expect(&Token::LBrace)?;

        let mut variants = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            let variant_name = self.expect_type_ident()?;

            let kind = if self.check(&Token::LParen) {
                // Tuple variant: Some(T)
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
                VariantKind::Tuple(fs)
            } else if self.check(&Token::LBrace) {
                // Struct variant: Move { x: Int, y: Int }
                self.advance();
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
                VariantKind::Struct(fields)
            } else {
                // Unit variant: None
                VariantKind::Unit
            };

            variants.push(EnumVariant {
                name: variant_name,
                kind,
            });

            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(&Token::RBrace)?;
        Ok(EnumDef {
            attrs,
            name,
            type_params,
            variants,
            is_pub,
        })
    }

    /// Parse a block.
    fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect(&Token::LBrace)?;
        let block = self.parse_block_contents()?;
        self.expect(&Token::RBrace)?;
        Ok(block)
    }

    /// Parse block contents (statements and optional trailing expression).
    /// Does not consume the surrounding braces.
    fn parse_block_contents(&mut self) -> ParseResult<Block> {
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
                    inferred_type_args: vec![],
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

                // Check for turbofish: .method::<Type>() or just .method()
                let type_args = if self.check(&Token::ColonColon) {
                    // Look ahead to see if this is turbofish (::< ) or path (::ident)
                    if self.peek_next() == Some(&Token::Lt) {
                        self.advance(); // consume ::
                        self.parse_type_args()?
                    } else {
                        vec![]
                    }
                } else {
                    vec![]
                };

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
                        type_args,
                        args,
                        resolved_module: None,
                        inferred_type_args: vec![],
                    };
                } else if !type_args.is_empty() {
                    // Turbofish without parens is an error
                    let span = self.current_span();
                    return Err(ParseError::new("expected '(' after turbofish type arguments", span));
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
                        let mut base = None;
                        while !self.check(&Token::RBrace) && !self.is_at_end() {
                            // Check for struct update syntax: ..base
                            if self.check(&Token::DotDot) {
                                self.advance();
                                base = Some(Box::new(self.parse_expr()?));
                                // After ..base, only } is allowed
                                break;
                            }

                            let field_name = self.expect_ident()?;
                            // Support shorthand: `{ x }` is equivalent to `{ x: x }`
                            let field_value = if self.check(&Token::Colon) {
                                self.advance();
                                self.parse_expr()?
                            } else {
                                Expr::Ident(field_name.clone())
                            };
                            fields.push((field_name, field_value));

                            if self.check(&Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.expect(&Token::RBrace)?;
                        expr = Expr::StructInit { name, fields, base };
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

                                // Check for tuple args
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
                                    EnumVariantArgs::Tuple(args)
                                } else if self.check(&Token::LBrace) {
                                    // Struct variant: mod::Type::Variant { field: value } or { field } shorthand
                                    self.advance();
                                    let mut fields = Vec::new();
                                    if !self.check(&Token::RBrace) {
                                        loop {
                                            let field_name = self.expect_ident()?;
                                            // Support shorthand: `{ x }` is equivalent to `{ x: x }`
                                            let field_value = if self.check(&Token::Colon) {
                                                self.advance();
                                                self.parse_expr()?
                                            } else {
                                                Expr::Ident(field_name.clone())
                                            };
                                            fields.push((field_name, field_value));
                                            if !self.check(&Token::Comma) {
                                                break;
                                            }
                                            self.advance();
                                        }
                                    }
                                    self.expect(&Token::RBrace)?;
                                    EnumVariantArgs::Struct(fields)
                                } else {
                                    EnumVariantArgs::Unit
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
            } else if self.check(&Token::Question) {
                // Try operator: expr?
                self.advance();
                expr = Expr::Try {
                    expr: Box::new(expr),
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

        if let Some(Token::String(raw)) = self.peek().cloned() {
            self.advance();
            // Check for string interpolation
            if has_interpolation(&raw) {
                let parts = parse_interpolated_string(&raw);
                let ast_parts = self.parse_string_interpolation_parts(parts)?;
                return Ok(Expr::StringInterpolation(ast_parts));
            } else {
                // Plain binary string - process escapes
                return Ok(Expr::String(process_escapes(&raw)));
            }
        }

        // Charlist (single-quoted string) - Elixir-style
        if let Some(Token::Charlist(raw)) = self.peek().cloned() {
            self.advance();
            // Process escapes for charlist
            return Ok(Expr::Charlist(process_escapes(&raw)));
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

            // Check for struct init: TypeIdent { ... } with field shorthand support
            if self.check(&Token::LBrace) {
                self.advance();
                let mut fields = Vec::new();
                let mut base = None;
                while !self.check(&Token::RBrace) && !self.is_at_end() {
                    // Check for struct update syntax: ..base
                    if self.check(&Token::DotDot) {
                        self.advance();
                        base = Some(Box::new(self.parse_expr()?));
                        // After ..base, only } is allowed
                        break;
                    }

                    let field_name = self.expect_ident()?;
                    // Support shorthand: `{ x }` is equivalent to `{ x: x }`
                    let field_value = if self.check(&Token::Colon) {
                        self.advance();
                        self.parse_expr()?
                    } else {
                        Expr::Ident(field_name.clone())
                    };
                    fields.push((field_name, field_value));

                    if self.check(&Token::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                self.expect(&Token::RBrace)?;
                return Ok(Expr::StructInit { name, fields, base });
            }

            // Check for qualified path: Type::variant or Type::method
            if self.check(&Token::ColonColon) {
                self.advance();

                // Could be TypeIdent (enum variant) or Ident (static method)
                if let Some(Token::TypeIdent(variant)) = self.peek().cloned() {
                    self.advance();

                    // Check for tuple args: TypeIdent::Variant(args)
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
                            args: EnumVariantArgs::Tuple(args),
                        });
                    }

                    // Check for struct fields: TypeIdent::Variant { field: value } or { field } shorthand
                    if self.check(&Token::LBrace) {
                        self.advance();
                        let mut fields = Vec::new();
                        if !self.check(&Token::RBrace) {
                            loop {
                                let field_name = self.expect_ident()?;
                                // Support shorthand: `{ x }` is equivalent to `{ x: x }`
                                let field_value = if self.check(&Token::Colon) {
                                    self.advance();
                                    self.parse_expr()?
                                } else {
                                    // Shorthand: field name becomes the value (identifier)
                                    Expr::Ident(field_name.clone())
                                };
                                fields.push((field_name, field_value));
                                if !self.check(&Token::Comma) {
                                    break;
                                }
                                self.advance();
                            }
                        }
                        self.expect(&Token::RBrace)?;
                        return Ok(Expr::EnumVariant {
                            type_name: Some(name),
                            variant,
                            args: EnumVariantArgs::Struct(fields),
                        });
                    }

                    // Unit variant: TypeIdent::Variant
                    return Ok(Expr::EnumVariant {
                        type_name: Some(name),
                        variant,
                        args: EnumVariantArgs::Unit,
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
                    args: EnumVariantArgs::Tuple(args),
                });
            }

            // Unit variant without type qualifier: Variant (just an atom)
            return Ok(Expr::EnumVariant {
                type_name: None,
                variant: name,
                args: EnumVariantArgs::Unit,
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

        // Quote expression: quote { ... }
        if self.check(&Token::Quote) {
            return self.parse_quote_expr();
        }

        // Unquote expression: #expr (for use inside quote blocks)
        if self.check(&Token::Hash) {
            self.advance();
            // Check for unquote-splicing: #...expr
            if self.check(&Token::DotDot) {
                self.advance();
                let expr = self.parse_primary()?;
                return Ok(Expr::UnquoteSplice(Box::new(expr)));
            }
            let expr = self.parse_primary()?;
            return Ok(Expr::Unquote(Box::new(expr)));
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

    /// Parse a quote expression: `quote { ... }`.
    /// The contents are captured as AST for macro processing.
    /// If the quote block contains an item (impl, fn, struct, enum, trait),
    /// it returns QuoteItem; otherwise Quote with a block expression.
    fn parse_quote_expr(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::Quote)?;
        self.expect(&Token::LBrace)?;

        // Enter quote mode - allows #ident in type positions
        let was_in_quote = self.in_quote;
        self.in_quote = true;

        // Check if the content is an item (impl, fn, struct, enum, trait)
        let is_item = matches!(
            self.peek(),
            Some(Token::Impl) | Some(Token::Fn) | Some(Token::Struct) | Some(Token::Enum) | Some(Token::Trait)
        );

        let result = if is_item {
            // Parse as a quoted item - parse_item handles its own attributes
            let item = self.parse_item()?;
            self.expect(&Token::RBrace)?;
            Ok(Expr::QuoteItem(Box::new(item)))
        } else {
            // Parse as a quoted block expression
            let block = self.parse_block_contents()?;
            self.expect(&Token::RBrace)?;
            Ok(Expr::Quote(Box::new(Expr::Block(block))))
        };

        // Restore quote mode
        self.in_quote = was_in_quote;
        result
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

        if let Some(Token::String(raw)) = self.peek().cloned() {
            self.advance();
            // Patterns don't support interpolation - just process escapes
            return Ok(Pattern::String(process_escapes(&raw)));
        }

        if let Some(Token::Charlist(raw)) = self.peek().cloned() {
            self.advance();
            return Ok(Pattern::Charlist(process_escapes(&raw)));
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
                        // Tuple variant pattern: mod::Type::Variant(...)
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
                        EnumPatternFields::Tuple(fs)
                    } else if self.check(&Token::LBrace) {
                        // Struct variant pattern: mod::Type::Variant { field, field: pattern, .. }
                        self.advance();
                        let mut fs = Vec::new();
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
                            fs.push((field_name, field_pattern));
                            if self.check(&Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.expect(&Token::RBrace)?;
                        EnumPatternFields::Struct(fs)
                    } else {
                        EnumPatternFields::Unit
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

            // Enum variant: Name::Variant or Name::Variant(...) or Name::Variant { ... }
            if self.check(&Token::ColonColon) {
                self.advance();
                let variant = self.expect_type_ident()?;

                let fields = if self.check(&Token::LParen) {
                    // Tuple variant pattern
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
                    EnumPatternFields::Tuple(fs)
                } else if self.check(&Token::LBrace) {
                    // Struct variant pattern: Type::Variant { field, field: pattern, .. }
                    self.advance();
                    let mut fs = Vec::new();
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
                        fs.push((field_name, field_pattern));
                        if self.check(&Token::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.expect(&Token::RBrace)?;
                    EnumPatternFields::Struct(fs)
                } else {
                    EnumPatternFields::Unit
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

            // Unqualified enum variant pattern: Variant(...) or Variant { ... }
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
                    fields: EnumPatternFields::Tuple(fields),
                });
            }

            // Just a type name as pattern (unit enum variant)
            return Ok(Pattern::Enum {
                name: String::new(),
                variant: name,
                fields: EnumPatternFields::Unit,
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
    /// Uses `expect_gt()` to handle nested generics like `Option<Option<T>>`.
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

        self.expect_gt()?; // Use expect_gt to handle >> as two > tokens
        Ok(args)
    }

    /// Parse a type.
    fn parse_type(&mut self) -> ParseResult<Type> {
        // Parse primary type, then check for union (|)
        let first = self.parse_primary_type()?;

        // Check for union type: T | U | V
        if self.check(&Token::Pipe) {
            let mut variants = vec![first];
            while self.check(&Token::Pipe) {
                self.advance();
                variants.push(self.parse_primary_type()?);
            }
            return Ok(Type::Union(variants));
        }

        Ok(first)
    }

    /// Parse a single (non-union) type.
    fn parse_primary_type(&mut self) -> ParseResult<Type> {
        // In quote mode, allow #ident for unquote in type position
        if self.in_quote && self.check(&Token::Hash) {
            self.advance(); // consume #
            let var_name = self.expect_ident()?;
            return Ok(Type::Named {
                name: format!("$UNQUOTE:{}", var_name),
                type_args: vec![],
            });
        }

        // Atom literal type: :ok, :error
        if let Some(Token::Atom(name)) = self.peek().cloned() {
            self.advance();
            return Ok(Type::AtomLiteral(name));
        }

        // Handle 'float' which is tokenized as Token::Float (a binary segment keyword)
        if self.check(&Token::Float) {
            self.advance();
            return Ok(Type::Float);
        }

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
                "ref" => {
                    self.advance();
                    return Ok(Type::Ref);
                }
                "bool" => {
                    self.advance();
                    return Ok(Type::Bool);
                }
                "float" => {
                    self.advance();
                    return Ok(Type::Float);
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

    /// Expect a `>` token, handling the case where `>>` needs to be split.
    /// When we see `>>` and expect `>`, we consume it but set `pending_gt`
    /// so the next `>` expectation is satisfied without consuming another token.
    fn expect_gt(&mut self) -> ParseResult<()> {
        if self.pending_gt {
            self.pending_gt = false;
            return Ok(());
        }
        if self.check(&Token::Gt) {
            self.advance();
            Ok(())
        } else if self.check(&Token::GtGt) {
            self.advance();
            self.pending_gt = true; // We've used one `>`, save the other
            Ok(())
        } else if self.is_at_end() {
            Err(ParseError::unexpected_eof(">"))
        } else {
            let span = self.current_span();
            Err(ParseError::unexpected_token(
                self.peek().unwrap(),
                ">",
                span,
            ))
        }
    }

    // TODO: Use these for parsing contextual keywords like `type` in type aliases
    /// Check if current token is an identifier with specific value (for contextual keywords like `type`)
    #[allow(dead_code)]
    fn check_ident(&self, value: &str) -> bool {
        matches!(self.peek(), Some(Token::Ident(s)) if s == value)
    }

    /// Expect a specific identifier value (for contextual keywords)
    #[allow(dead_code)]
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

    /// Parse a module path like `my_app::users::auth`.
    /// Returns the full path joined with `::`.
    fn parse_module_path(&mut self) -> ParseResult<String> {
        let mut parts = vec![self.expect_ident()?];

        while self.check(&Token::ColonColon) {
            self.advance(); // consume ::
            parts.push(self.expect_ident()?);
        }

        Ok(parts.join("::"))
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

    /// Expect an identifier or a keyword that can be used as a function name.
    /// This is needed for extern blocks where Erlang/Elixir function names may
    /// be Dream keywords (spawn, receive, self, etc.)
    fn expect_ident_or_keyword(&mut self) -> ParseResult<String> {
        let name = match self.peek().cloned() {
            Some(Token::Ident(name)) => name,
            Some(Token::Spawn) => "spawn".to_string(),
            Some(Token::Receive) => "receive".to_string(),
            Some(Token::SelfKw) => "self".to_string(),
            Some(Token::After) => "after".to_string(),
            Some(Token::Match) => "match".to_string(),
            Some(Token::If) => "if".to_string(),
            Some(Token::Else) => "else".to_string(),
            Some(Token::For) => "for".to_string(),
            Some(Token::When) => "when".to_string(),
            Some(Token::Type) => "type".to_string(),
            Some(Token::True) => "true".to_string(),
            Some(Token::False) => "false".to_string(),
            // Binary segment type keywords that might be function names
            Some(Token::Float) => "float".to_string(),
            Some(Token::Integer) => "integer".to_string(),
            Some(Token::BinaryKw) => "binary".to_string(),
            None => return Err(ParseError::unexpected_eof("identifier")),
            _ => {
                let span = self.current_span();
                return Err(ParseError::unexpected_token(
                    self.peek().unwrap(),
                    "identifier",
                    span,
                ));
            }
        };
        self.advance();
        Ok(name)
    }

    /// Parse string interpolation parts into AST StringParts.
    /// Converts LexStringPart::Interpolation to parsed expressions.
    fn parse_string_interpolation_parts(
        &mut self,
        parts: Vec<LexStringPart>,
    ) -> ParseResult<Vec<StringPart>> {
        let mut result = Vec::new();

        for part in parts {
            match part {
                LexStringPart::Literal(s) => {
                    result.push(StringPart::Literal(s));
                }
                LexStringPart::Interpolation(expr_str) => {
                    // Create a new parser for the expression string
                    let mut sub_parser = Parser::new(&expr_str);
                    let expr = sub_parser.parse_expr()?;

                    // Ensure the entire expression was consumed
                    if sub_parser.peek().is_some() {
                        return Err(ParseError::unexpected_token(
                            sub_parser.peek().unwrap(),
                            "end of interpolation",
                            sub_parser.current_span(),
                        ));
                    }

                    result.push(StringPart::Expr(Box::new(expr)));
                }
            }
        }

        Ok(result)
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
            assert!(matches!(&e.variants[0].kind, VariantKind::Tuple(fields) if fields.len() == 1));
            assert_eq!(e.variants[1].name, "None");
            assert!(matches!(&e.variants[1].kind, VariantKind::Unit));
        } else {
            panic!("expected enum");
        }
    }

    #[test]
    fn test_parse_enum_struct_variants() {
        let source = r#"
            mod test {
                enum Message {
                    Quit,
                    Move { x: Int, y: Int },
                    Write(String),
                    ChangeColor { r: Int, g: Int, b: Int },
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Enum(e) = first_user_item(&module) {
            assert_eq!(e.name, "Message");
            assert_eq!(e.variants.len(), 4);

            // Quit - unit variant
            assert_eq!(e.variants[0].name, "Quit");
            assert!(matches!(&e.variants[0].kind, VariantKind::Unit));

            // Move { x: Int, y: Int } - struct variant
            assert_eq!(e.variants[1].name, "Move");
            if let VariantKind::Struct(fields) = &e.variants[1].kind {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0, "x");
                assert_eq!(fields[1].0, "y");
            } else {
                panic!("expected Struct variant for Move");
            }

            // Write(String) - tuple variant
            assert_eq!(e.variants[2].name, "Write");
            assert!(matches!(&e.variants[2].kind, VariantKind::Tuple(fields) if fields.len() == 1));

            // ChangeColor { r: Int, g: Int, b: Int } - struct variant
            assert_eq!(e.variants[3].name, "ChangeColor");
            if let VariantKind::Struct(fields) = &e.variants[3].kind {
                assert_eq!(fields.len(), 3);
                assert_eq!(fields[0].0, "r");
                assert_eq!(fields[1].0, "g");
                assert_eq!(fields[2].0, "b");
            } else {
                panic!("expected Struct variant for ChangeColor");
            }
        } else {
            panic!("expected enum");
        }
    }

    #[test]
    fn test_parse_struct_variant_construction() {
        let source = r#"
            mod test {
                enum Message {
                    Move { x: int, y: int },
                }

                fn main() {
                    let msg = Message::Move { x: 10, y: 20 };
                    msg
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find the function
        let func = module.items.iter().find_map(|item| {
            if let Item::Function(f) = item { Some(f) } else { None }
        }).expect("expected function");

        // Check the let statement
        if let Some(Stmt::Let { pattern: _, ty: _, value: init }) = func.body.stmts.first() {
            if let Expr::EnumVariant { type_name, variant, args } = init {
                assert_eq!(type_name.as_deref(), Some("Message"));
                assert_eq!(variant, "Move");
                if let EnumVariantArgs::Struct(fields) = args {
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].0, "x");
                    assert_eq!(fields[1].0, "y");
                } else {
                    panic!("expected struct variant args");
                }
            } else {
                panic!("expected EnumVariant expression");
            }
        } else {
            panic!("expected let statement");
        }
    }

    #[test]
    fn test_parse_struct_variant_shorthand() {
        let source = r#"
            mod test {
                enum Point {
                    Coord { x: int, y: int },
                }

                fn main() {
                    let x = 10;
                    let y = 20;
                    let pt = Point::Coord { x, y };
                    pt
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find the function
        let func = module.items.iter().find_map(|item| {
            if let Item::Function(f) = item { Some(f) } else { None }
        }).expect("expected function");

        // Check the third let statement (let pt = ...)
        if let Some(Stmt::Let { value: init, .. }) = func.body.stmts.get(2) {
            if let Expr::EnumVariant { args, .. } = init {
                if let EnumVariantArgs::Struct(fields) = args {
                    assert_eq!(fields.len(), 2);
                    // Check that shorthand expanded to Ident
                    assert_eq!(fields[0].0, "x");
                    assert!(matches!(&fields[0].1, Expr::Ident(n) if n == "x"));
                    assert_eq!(fields[1].0, "y");
                    assert!(matches!(&fields[1].1, Expr::Ident(n) if n == "y"));
                } else {
                    panic!("expected struct variant args");
                }
            } else {
                panic!("expected EnumVariant expression");
            }
        } else {
            panic!("expected let statement");
        }
    }

    #[test]
    fn test_parse_struct_init_shorthand() {
        let source = r#"
            mod test {
                struct Point {
                    x: int,
                    y: int,
                }

                fn main() {
                    let x = 10;
                    let y = 20;
                    let pt = Point { x, y };
                    pt
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find the function
        let func = module.items.iter().find_map(|item| {
            if let Item::Function(f) = item { Some(f) } else { None }
        }).expect("expected function");

        // Check the third let statement (let pt = ...)
        if let Some(Stmt::Let { value: init, .. }) = func.body.stmts.get(2) {
            if let Expr::StructInit { name, fields, .. } = init {
                assert_eq!(name, "Point");
                assert_eq!(fields.len(), 2);
                // Check that shorthand expanded to Ident
                assert_eq!(fields[0].0, "x");
                assert!(matches!(&fields[0].1, Expr::Ident(n) if n == "x"));
                assert_eq!(fields[1].0, "y");
                assert!(matches!(&fields[1].1, Expr::Ident(n) if n == "y"));
            } else {
                panic!("expected StructInit expression");
            }
        } else {
            panic!("expected let statement");
        }
    }

    #[test]
    fn test_parse_struct_variant_pattern() {
        let source = r#"
            mod test {
                enum Message {
                    Move { x: int, y: int },
                }

                fn main(msg: Message) {
                    match msg {
                        Message::Move { x, y } => x + y,
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Find the function
        let func = module.items.iter().find_map(|item| {
            if let Item::Function(f) = item { Some(f) } else { None }
        }).expect("expected function");

        // Check the match expression
        if let Some(expr) = &func.body.expr {
            if let Expr::Match { arms, .. } = expr.as_ref() {
                let arm = &arms[0];
                if let Pattern::Enum { name, variant, fields } = &arm.pattern {
                    assert_eq!(name, "Message");
                    assert_eq!(variant, "Move");
                    if let EnumPatternFields::Struct(field_patterns) = fields {
                        assert_eq!(field_patterns.len(), 2);
                        assert_eq!(field_patterns[0].0, "x");
                        assert_eq!(field_patterns[1].0, "y");
                    } else {
                        panic!("expected struct pattern fields");
                    }
                } else {
                    panic!("expected Enum pattern");
                }
            } else {
                panic!("expected match expression");
            }
        } else {
            panic!("expected expression in body");
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
            if let VariantKind::Tuple(fields) = &e.variants[0].kind {
                if let Type::Named { name, type_args } = &fields[0] {
                    assert_eq!(name, "T");
                    assert!(type_args.is_empty());
                } else {
                    panic!("expected Named type for T");
                }
            } else {
                panic!("expected Tuple variant");
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
        let modules = parser.parse_file_modules("binary_example").expect("binary_example.dream should parse successfully");

        // Should have 1 module with 2 items (1 use import + 1 main function)
        assert_eq!(modules.len(), 1);
        assert_eq!(modules[0].name, "binary_example");
        assert_eq!(user_items(&modules[0]).len(), 2);
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

    #[test]
    fn test_parse_extern_mod() {
        let source = r#"
            mod test {
                extern mod erlang {
                    fn spawn(fun: fn() -> any) -> pid;
                    fn self() -> pid;
                    fn send(dest: pid, msg: any) -> any;

                    type Ref;

                    mod maps {
                        fn get(key: any, map: any) -> any;
                        fn put(key: any, value: any, map: any) -> any;
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().expect("extern mod should parse");

        // Find the ExternMod item
        let extern_mod = user_items(&module)
            .iter()
            .find_map(|item| {
                if let Item::ExternMod(em) = item {
                    Some(em)
                } else {
                    None
                }
            })
            .expect("should have extern mod");

        assert_eq!(extern_mod.name, "erlang");
        assert_eq!(extern_mod.items.len(), 5); // 3 fns + 1 type + 1 nested mod

        // Check nested mod
        let nested_mod = extern_mod.items.iter().find_map(|item| {
            if let ExternItem::Mod(m) = item {
                Some(m)
            } else {
                None
            }
        });
        assert!(nested_mod.is_some());
        assert_eq!(nested_mod.unwrap().name, "maps");
        assert_eq!(nested_mod.unwrap().items.len(), 2);
    }

    #[test]
    fn test_parse_erlang_stub_file() {
        // Test parsing the actual erlang.dreamt stub file (generated by bindgen)
        let stub_source = include_str!("../../stubs/erlang.dreamt");
        // Wrap in a module since parser expects that
        let source = format!("mod stubs {{\n{}\n}}", stub_source);
        let mut parser = Parser::new(&source);
        let module = parser.parse_module().expect("erlang.dreamt should parse");

        // Count extern mods
        let extern_mods: Vec<_> = user_items(&module)
            .iter()
            .filter_map(|item| {
                if let Item::ExternMod(em) = item {
                    Some(em)
                } else {
                    None
                }
            })
            .collect();

        // Generated by bindgen - should have at least 1 extern mod (erlang)
        assert!(extern_mods.len() >= 1, "expected at least 1 extern mod, got {}", extern_mods.len());

        // Verify erlang module exists and has expected functions
        let erlang_mod = extern_mods.iter().find(|m| m.name == "erlang");
        assert!(erlang_mod.is_some(), "should have erlang extern mod");
        let erlang = erlang_mod.unwrap();

        // Count functions in erlang mod - bindgen generates many functions
        let fn_count = erlang.items.iter().filter(|item| matches!(item, ExternItem::Function(_))).count();
        assert!(fn_count >= 30, "erlang mod should have at least 30 functions, got {}", fn_count);
    }

    // ========== Union Type Parser Tests ==========

    #[test]
    fn test_parse_atom_literal_type() {
        let source = r#"
            mod test {
                fn returns_ok() -> :ok {
                    :ok
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            assert_eq!(f.name, "returns_ok");
            assert!(matches!(f.return_type, Some(Type::AtomLiteral(ref s)) if s == "ok"));
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_union_type() {
        let source = r#"
            mod test {
                fn maybe_fail() -> :ok | :error {
                    :ok
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            assert_eq!(f.name, "maybe_fail");
            if let Some(Type::Union(variants)) = &f.return_type {
                assert_eq!(variants.len(), 2);
                assert!(matches!(&variants[0], Type::AtomLiteral(s) if s == "ok"));
                assert!(matches!(&variants[1], Type::AtomLiteral(s) if s == "error"));
            } else {
                panic!("expected union type, got {:?}", f.return_type);
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_union_with_primitive_types() {
        let source = r#"
            mod test {
                fn get_value() -> int | string {
                    42
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Type::Union(variants)) = &f.return_type {
                assert_eq!(variants.len(), 2);
                assert!(matches!(&variants[0], Type::Int));
                assert!(matches!(&variants[1], Type::String));
            } else {
                panic!("expected union type, got {:?}", f.return_type);
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_three_way_union() {
        let source = r#"
            mod test {
                fn choice() -> :a | :b | :c {
                    :a
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Type::Union(variants)) = &f.return_type {
                assert_eq!(variants.len(), 3);
                assert!(matches!(&variants[0], Type::AtomLiteral(s) if s == "a"));
                assert!(matches!(&variants[1], Type::AtomLiteral(s) if s == "b"));
                assert!(matches!(&variants[2], Type::AtomLiteral(s) if s == "c"));
            } else {
                panic!("expected union type, got {:?}", f.return_type);
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_struct_with_union_field() {
        let source = r#"
            mod test {
                struct Response {
                    status: :success | :failure,
                    code: int,
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Struct(s) = first_user_item(&module) {
            assert_eq!(s.name, "Response");
            assert_eq!(s.fields.len(), 2);

            let (status_name, status_type) = &s.fields[0];
            assert_eq!(status_name, "status");
            if let Type::Union(variants) = status_type {
                assert_eq!(variants.len(), 2);
            } else {
                panic!("expected union type for status field");
            }
        } else {
            panic!("expected struct");
        }
    }

    // ========== Type Alias Tests ==========

    #[test]
    fn test_parse_simple_type_alias() {
        let source = r#"
            mod test {
                type UserId = int;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::TypeAlias(alias) = first_user_item(&module) {
            assert_eq!(alias.name, "UserId");
            assert!(!alias.is_pub);
            assert!(alias.type_params.is_empty());
            // int is a primitive type, not a Named type
            assert_eq!(alias.ty, Type::Int);
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_pub_type_alias() {
        let source = r#"
            mod test {
                pub type ApiResponse = :success | :failure;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::TypeAlias(alias) = first_user_item(&module) {
            assert_eq!(alias.name, "ApiResponse");
            assert!(alias.is_pub);
            if let Type::Union(variants) = &alias.ty {
                assert_eq!(variants.len(), 2);
            } else {
                panic!("expected union type");
            }
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_type_alias_union() {
        let source = r#"
            mod test {
                type Status = :ok | :error | :pending;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::TypeAlias(alias) = first_user_item(&module) {
            assert_eq!(alias.name, "Status");
            if let Type::Union(variants) = &alias.ty {
                assert_eq!(variants.len(), 3);
            } else {
                panic!("expected union type");
            }
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_type_alias_tuple() {
        let source = r#"
            mod test {
                type Point = (int, int);
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::TypeAlias(alias) = first_user_item(&module) {
            assert_eq!(alias.name, "Point");
            if let Type::Tuple(elems) = &alias.ty {
                assert_eq!(elems.len(), 2);
            } else {
                panic!("expected tuple type");
            }
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_generic_type_alias() {
        let source = r#"
            mod test {
                type MyResult<T> = (:ok, T) | :error;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::TypeAlias(alias) = first_user_item(&module) {
            assert_eq!(alias.name, "MyResult");
            assert_eq!(alias.type_params.len(), 1);
            assert_eq!(alias.type_params[0].name, "T");
            if let Type::Union(_) = &alias.ty {
                // Good - it's a union type
            } else {
                panic!("expected union type");
            }
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_generic_type_alias_multiple_params() {
        let source = r#"
            mod test {
                type Either<L, R> = (:left, L) | (:right, R);
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::TypeAlias(alias) = first_user_item(&module) {
            assert_eq!(alias.name, "Either");
            assert_eq!(alias.type_params.len(), 2);
            assert_eq!(alias.type_params[0].name, "L");
            assert_eq!(alias.type_params[1].name, "R");
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_type_alias_function_type() {
        let source = r#"
            mod test {
                type Handler = fn(int) -> string;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::TypeAlias(alias) = first_user_item(&module) {
            assert_eq!(alias.name, "Handler");
            if let Type::Fn { params, ret } = &alias.ty {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], Type::Int);
                // string is a primitive type
                assert_eq!(ret.as_ref(), &Type::String);
            } else {
                panic!("expected function type, got {:?}", alias.ty);
            }
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_use_with_crate_path() {
        use crate::compiler::ast::{PathPrefix, UseTree};

        let source = r#"
            mod test {
                use crate::db::query;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Use(use_decl) = first_user_item(&module) {
            if let UseTree::Path { module, name, .. } = &use_decl.tree {
                assert_eq!(module.prefix, PathPrefix::Crate);
                assert_eq!(module.segments, vec!["db".to_string()]);
                assert_eq!(name, "query");
            } else {
                panic!("expected path use tree");
            }
        } else {
            panic!("expected use declaration");
        }
    }

    #[test]
    fn test_parse_use_with_super_path() {
        use crate::compiler::ast::{PathPrefix, UseTree};

        let source = r#"
            mod test {
                use super::helpers::{foo, bar};
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Use(use_decl) = first_user_item(&module) {
            if let UseTree::Group { module, items } = &use_decl.tree {
                assert_eq!(module.prefix, PathPrefix::Super);
                assert_eq!(module.segments, vec!["helpers".to_string()]);
                assert_eq!(items.len(), 2);
                assert_eq!(items[0].name, "foo");
                assert_eq!(items[1].name, "bar");
            } else {
                panic!("expected group use tree");
            }
        } else {
            panic!("expected use declaration");
        }
    }

    #[test]
    fn test_parse_use_with_self_path() {
        use crate::compiler::ast::{PathPrefix, UseTree};

        let source = r#"
            mod test {
                use self::submod::Thing;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Use(use_decl) = first_user_item(&module) {
            if let UseTree::Path { module, name, .. } = &use_decl.tree {
                assert_eq!(module.prefix, PathPrefix::SelfMod);
                assert_eq!(module.segments, vec!["submod".to_string()]);
                assert_eq!(name, "Thing");
            } else {
                panic!("expected path use tree");
            }
        } else {
            panic!("expected use declaration");
        }
    }

    #[test]
    fn test_parse_use_with_super_direct_import() {
        use crate::compiler::ast::{PathPrefix, UseTree};

        // Test `use super::item;` where there are no module segments
        let source = r#"
            mod test {
                use super::get_user_id;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Use(use_decl) = first_user_item(&module) {
            if let UseTree::Path { module, name, .. } = &use_decl.tree {
                assert_eq!(module.prefix, PathPrefix::Super);
                assert!(module.segments.is_empty(), "expected empty segments for direct super:: import");
                assert_eq!(name, "get_user_id");
            } else {
                panic!("expected path use tree");
            }
        } else {
            panic!("expected use declaration");
        }
    }

    #[test]
    fn test_parse_use_with_crate_direct_import() {
        use crate::compiler::ast::{PathPrefix, UseTree};

        // Test `use crate::item;` where there are no module segments
        let source = r#"
            mod test {
                use crate::main_func;
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Use(use_decl) = first_user_item(&module) {
            if let UseTree::Path { module, name, .. } = &use_decl.tree {
                assert_eq!(module.prefix, PathPrefix::Crate);
                assert!(module.segments.is_empty(), "expected empty segments for direct crate:: import");
                assert_eq!(name, "main_func");
            } else {
                panic!("expected path use tree");
            }
        } else {
            panic!("expected use declaration");
        }
    }

    // =========================================================================
    // Attribute Parsing Tests
    // =========================================================================

    #[test]
    fn test_parse_simple_attribute() {
        let source = r#"
            mod test {
                #[test]
                fn test_something() {}
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(func) = first_user_item(&module) {
            assert_eq!(func.name, "test_something");
            assert_eq!(func.attrs.len(), 1);
            assert_eq!(func.attrs[0].name, "test");
            assert!(matches!(func.attrs[0].args, AttributeArgs::None));
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_cfg_attribute_simple() {
        let source = r#"
            mod test {
                #[cfg(test)]
                fn test_only() {}
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(func) = first_user_item(&module) {
            assert_eq!(func.name, "test_only");
            assert_eq!(func.attrs.len(), 1);
            assert_eq!(func.attrs[0].name, "cfg");
            if let AttributeArgs::Parenthesized(args) = &func.attrs[0].args {
                assert_eq!(args.len(), 1);
                if let AttributeArg::Ident(ident) = &args[0] {
                    assert_eq!(ident, "test");
                } else {
                    panic!("expected ident argument");
                }
            } else {
                panic!("expected parenthesized args");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_cfg_attribute_feature() {
        let source = r#"
            mod test {
                #[cfg(feature = "json")]
                fn with_json() {}
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(func) = first_user_item(&module) {
            assert_eq!(func.name, "with_json");
            if let AttributeArgs::Parenthesized(args) = &func.attrs[0].args {
                if let AttributeArg::KeyValue(key, value) = &args[0] {
                    assert_eq!(key, "feature");
                    assert_eq!(value, "json");
                } else {
                    panic!("expected key-value argument");
                }
            } else {
                panic!("expected parenthesized args");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_cfg_attribute_not() {
        let source = r#"
            mod test {
                #[cfg(not(test))]
                fn production_only() {}
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(func) = first_user_item(&module) {
            assert_eq!(func.name, "production_only");
            if let AttributeArgs::Parenthesized(args) = &func.attrs[0].args {
                if let AttributeArg::Nested(name, nested) = &args[0] {
                    assert_eq!(name, "not");
                    assert_eq!(nested.len(), 1);
                    if let AttributeArg::Ident(inner) = &nested[0] {
                        assert_eq!(inner, "test");
                    } else {
                        panic!("expected ident in nested");
                    }
                } else {
                    panic!("expected nested argument");
                }
            } else {
                panic!("expected parenthesized args");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_multiple_attributes() {
        let source = r#"
            mod test {
                #[test]
                #[cfg(feature = "json")]
                fn test_with_json() {}
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(func) = first_user_item(&module) {
            assert_eq!(func.name, "test_with_json");
            assert_eq!(func.attrs.len(), 2);
            assert_eq!(func.attrs[0].name, "test");
            assert_eq!(func.attrs[1].name, "cfg");
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_attribute_on_struct() {
        let source = r#"
            mod test {
                #[cfg(feature = "serde")]
                struct User {
                    name: string
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Struct(struct_def) = first_user_item(&module) {
            assert_eq!(struct_def.name, "User");
            assert_eq!(struct_def.attrs.len(), 1);
            assert_eq!(struct_def.attrs[0].name, "cfg");
        } else {
            panic!("expected struct");
        }
    }

    #[test]
    fn test_parse_attribute_on_enum() {
        let source = r#"
            mod test {
                #[cfg(test)]
                enum TestResult {
                    Pass,
                    Fail
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Enum(enum_def) = first_user_item(&module) {
            assert_eq!(enum_def.name, "TestResult");
            assert_eq!(enum_def.attrs.len(), 1);
            assert_eq!(enum_def.attrs[0].name, "cfg");
        } else {
            panic!("expected enum");
        }
    }

    #[test]
    fn test_parse_attribute_doc_equals() {
        let source = r#"
            mod test {
                #[doc = "This is a function"]
                fn documented() {}
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(func) = first_user_item(&module) {
            assert_eq!(func.name, "documented");
            assert_eq!(func.attrs.len(), 1);
            assert_eq!(func.attrs[0].name, "doc");
            if let AttributeArgs::Eq(value) = &func.attrs[0].args {
                assert_eq!(value, "This is a function");
            } else {
                panic!("expected eq args");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_attribute_cfg_all() {
        let source = r#"
            mod test {
                #[cfg(all(feature = "a", feature = "b"))]
                fn needs_both() {}
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(func) = first_user_item(&module) {
            if let AttributeArgs::Parenthesized(args) = &func.attrs[0].args {
                if let AttributeArg::Nested(name, nested) = &args[0] {
                    assert_eq!(name, "all");
                    assert_eq!(nested.len(), 2);
                } else {
                    panic!("expected nested argument");
                }
            } else {
                panic!("expected parenthesized args");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_attribute_on_impl_method() {
        let source = r#"
            mod test {
                struct Foo {}

                impl Foo {
                    #[test]
                    fn test_method(self) {}
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        // Skip prelude items and struct to get to impl
        let items = user_items(&module);
        if let Item::Impl(impl_block) = &items[1] {
            assert_eq!(impl_block.methods.len(), 1);
            assert_eq!(impl_block.methods[0].attrs.len(), 1);
            assert_eq!(impl_block.methods[0].attrs[0].name, "test");
        } else {
            panic!("expected impl block");
        }
    }

    #[test]
    fn test_parse_wrapped_module_with_leading_comments() {
        let source = r#"// Define modules here. All code must be inside mod blocks.
// After saving, call functions from the REPL prompt.

mod repl_edit {
    pub fn add(a: int, b: int) -> int {
        a + b
    }
}
"#;
        let mut parser = Parser::new(source);
        let modules = parser
            .parse_file_modules("repl_edit")
            .expect("should parse wrapped module with leading comments");

        assert_eq!(modules.len(), 1);
        assert_eq!(modules[0].name, "repl_edit");
        // Should have Option, Result (prelude) + add function
        assert!(user_items(&modules[0]).len() >= 1);
    }

    #[test]
    fn test_parse_wrapped_module_no_comments() {
        let source = r#"mod repl_edit {
    struct User {
        name: String,
    }
}
"#;
        let mut parser = Parser::new(source);
        let modules = parser
            .parse_file_modules("repl_edit")
            .expect("should parse wrapped module without comments");

        assert_eq!(modules.len(), 1);
        assert_eq!(modules[0].name, "repl_edit");
    }

    #[test]
    fn test_parse_quote_unquote() {
        let source = r#"
            mod test {
                fn make_add(x: int) {
                    quote {
                        let val = #x;
                        val + 1
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::Quote(inner)) = f.body.expr.as_deref() {
                // The quote contains a block
                if let Expr::Block(block) = inner.as_ref() {
                    assert_eq!(block.stmts.len(), 1); // let val = #x
                    // The block expression should be the addition
                    assert!(block.expr.is_some());
                } else {
                    panic!("expected block inside quote");
                }
            } else {
                panic!("expected quote expr, got {:?}", f.body.expr);
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_unquote_splice() {
        let source = r#"
            mod test {
                fn make_list(items: [Ast]) {
                    quote {
                        [1, #..items, 2]
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::Quote(_)) = f.body.expr.as_deref() {
                // Just verify it parses - the inner structure is complex
            } else {
                panic!("expected quote expr");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_quote_unquote_function_name() {
        // TDD: Test that #func_name works in quoted function definitions
        let source = r#"
            mod test {
                fn make_getter(func_name: Atom) {
                    quote {
                        fn #func_name(self) -> int { 42 }
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::QuoteItem(item)) = f.body.expr.as_deref() {
                if let Item::Function(quoted_func) = item.as_ref() {
                    // The function name should be the unquote marker
                    assert_eq!(quoted_func.name, "$UNQUOTE:func_name");
                } else {
                    panic!("expected quoted function, got {:?}", item);
                }
            } else {
                panic!("expected quote item expr, got {:?}", f.body.expr);
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_quote_unquote_struct_name() {
        // TDD: Test that #name works in quoted struct definitions
        let source = r#"
            mod test {
                fn make_struct(name: atom) {
                    quote {
                        struct #name {
                            value: int,
                        }
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::QuoteItem(item)) = f.body.expr.as_deref() {
                if let Item::Struct(s) = item.as_ref() {
                    // The struct name should be the unquote marker
                    assert_eq!(s.name, "$UNQUOTE:name");
                } else {
                    panic!("expected quoted struct, got {:?}", item);
                }
            } else {
                panic!("expected quote item expr, got {:?}", f.body.expr);
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_quote_unquote_struct_field_name() {
        // TDD: Test that #field_name works in quoted struct field definitions
        let source = r#"
            mod test {
                fn make_struct(field_name: atom) {
                    quote {
                        struct Foo {
                            #field_name: int,
                        }
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::QuoteItem(item)) = f.body.expr.as_deref() {
                if let Item::Struct(s) = item.as_ref() {
                    // The first field name should be the unquote marker
                    assert_eq!(s.fields[0].0, "$UNQUOTE:field_name");
                } else {
                    panic!("expected quoted struct, got {:?}", item);
                }
            } else {
                panic!("expected quote item expr, got {:?}", f.body.expr);
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_quote_unquote_return_type() {
        use crate::compiler::ast::Type;
        // TDD: Test that #ret_type works in quoted function return types
        let source = r#"
            mod test {
                fn make_fn(ret_type: Type) {
                    quote {
                        fn foo() -> #ret_type { }
                    }
                }
            }
        "#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module().unwrap();

        if let Item::Function(f) = first_user_item(&module) {
            if let Some(Expr::QuoteItem(item)) = f.body.expr.as_deref() {
                if let Item::Function(quoted_func) = item.as_ref() {
                    // The return type should be an unquote marker
                    if let Some(Type::Named { name, .. }) = &quoted_func.return_type {
                        assert_eq!(name, "$UNQUOTE:ret_type");
                    } else {
                        panic!("expected named type with unquote marker, got {:?}", quoted_func.return_type);
                    }
                } else {
                    panic!("expected quoted function, got {:?}", item);
                }
            } else {
                panic!("expected quote item expr, got {:?}", f.body.expr);
            }
        } else {
            panic!("expected function");
        }
    }
}
