//! Token definitions for the lexer.

use logos::Logos;

/// Parts of an interpolated string from the lexer.
/// The parser will convert `Interpolation(String)` to actual parsed expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum LexStringPart {
    /// A literal string segment (escapes already processed)
    Literal(String),
    /// Raw expression text to be parsed (content between { and })
    Interpolation(String),
}

/// Process escape sequences in a string literal.
/// Handles: \n, \r, \t, \\, \", \0, {{ → {, }} → }
pub fn process_escapes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some('{') => result.push('{'),
                Some('}') => result.push('}'),
                Some(other) => {
                    // Unknown escape - keep as-is
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else if c == '{' && chars.peek() == Some(&'{') {
            // {{ -> {
            chars.next();
            result.push('{');
        } else if c == '}' && chars.peek() == Some(&'}') {
            // }} -> }
            chars.next();
            result.push('}');
        } else {
            result.push(c);
        }
    }

    result
}

/// Check if a string contains interpolation (unescaped `{`).
pub fn has_interpolation(s: &str) -> bool {
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            // Skip escaped character
            chars.next();
        } else if c == '{' {
            // Check if it's escaped as {{
            if chars.peek() == Some(&'{') {
                chars.next(); // Skip the second {
            } else {
                return true;
            }
        }
    }
    false
}

/// Parse a string with interpolation into parts.
/// Handles `{{` and `}}` escapes.
pub fn parse_interpolated_string(s: &str) -> Vec<LexStringPart> {
    let mut parts = Vec::new();
    let mut current_literal = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // Handle escape sequences in literal parts
            match chars.next() {
                Some('n') => current_literal.push('\n'),
                Some('r') => current_literal.push('\r'),
                Some('t') => current_literal.push('\t'),
                Some('\\') => current_literal.push('\\'),
                Some('"') => current_literal.push('"'),
                Some('0') => current_literal.push('\0'),
                Some('{') => current_literal.push('{'),
                Some('}') => current_literal.push('}'),
                Some(other) => {
                    current_literal.push('\\');
                    current_literal.push(other);
                }
                None => current_literal.push('\\'),
            }
        } else if c == '{' {
            // Check for escaped {{ -> literal {
            if chars.peek() == Some(&'{') {
                chars.next();
                current_literal.push('{');
            } else {
                // Start of interpolation
                // Save current literal if non-empty
                if !current_literal.is_empty() {
                    parts.push(LexStringPart::Literal(current_literal));
                    current_literal = String::new();
                }

                // Parse until matching }
                let mut expr = String::new();
                let mut brace_depth = 1;

                while let Some(ic) = chars.next() {
                    if ic == '{' {
                        brace_depth += 1;
                        expr.push(ic);
                    } else if ic == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                        expr.push(ic);
                    } else if ic == '"' {
                        // Handle nested string in expression
                        expr.push(ic);
                        while let Some(sc) = chars.next() {
                            expr.push(sc);
                            if sc == '\\' {
                                if let Some(esc) = chars.next() {
                                    expr.push(esc);
                                }
                            } else if sc == '"' {
                                break;
                            }
                        }
                    } else {
                        expr.push(ic);
                    }
                }

                parts.push(LexStringPart::Interpolation(expr));
            }
        } else if c == '}' {
            // Check for escaped }} -> literal }
            if chars.peek() == Some(&'}') {
                chars.next();
                current_literal.push('}');
            } else {
                // Stray } - just include it
                current_literal.push('}');
            }
        } else {
            current_literal.push(c);
        }
    }

    // Don't forget the trailing literal
    if !current_literal.is_empty() {
        parts.push(LexStringPart::Literal(current_literal));
    }

    parts
}

/// Tokens produced by the lexer.
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r]+")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    // Keywords
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("mod")]
    Mod,
    #[token("pub")]
    Pub,
    #[token("self")]
    SelfKw,
    #[token("crate")]
    Crate,
    #[token("super")]
    Super,
    #[token("spawn")]
    Spawn,
    #[token("receive")]
    Receive,
    #[token("after")]
    After,
    #[token("return")]
    Return,
    #[token("use")]
    Use,
    #[token("as")]
    As,
    #[token("impl")]
    Impl,
    #[token("trait")]
    Trait,
    #[token("for")]
    For,
    #[token("when")]
    When,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("extern")]
    Extern,
    #[token("type")]
    Type,
    #[token("quote")]
    Quote,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Int(i64),

    /// Binary string literal (double quotes) - Elixir-style.
    /// Stores RAW content (quotes stripped, escapes NOT processed).
    /// The parser will:
    /// 1. Check for interpolation (`{expr}` syntax)
    /// 2. If found: parse as InterpolatedString
    /// 3. If not: process escapes and create Expr::Binary
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Strip quotes but keep raw content for interpolation detection
        Some(s[1..s.len()-1].to_string())
    })]
    String(String),

    /// Charlist literal (single quotes) - Elixir-style.
    /// Produces a list of integers (codepoints).
    #[regex(r#"'([^'\\]|\\.)*'"#, |lex| {
        let s = lex.slice();
        // Strip quotes but keep raw content
        Some(s[1..s.len()-1].to_string())
    }, priority = 2)]
    Charlist(String),

    // Simple atoms: :ok, :error, :my_atom
    #[regex(r":[a-z_][a-z0-9_]*", |lex| Some(lex.slice()[1..].to_string()))]
    Atom(String),

    // Quoted atoms for Elixir modules: :'Elixir.Enum', :'my-atom'
    #[regex(r":'[^']*'", |lex| {
        let s = lex.slice();
        // Extract content between :' and '
        Some(s[2..s.len()-1].to_string())
    })]
    // Also support double-quote syntax: :"Elixir.Enum"
    #[regex(r#":"[^"]*""#, |lex| {
        let s = lex.slice();
        // Extract content between :" and "
        Some(s[2..s.len()-1].to_string())
    })]
    QuotedAtom(String),

    #[regex(r"[a-z_][a-z0-9_]*", |lex| Some(lex.slice().to_string()), priority = 1)]
    Ident(String),

    #[regex(r"[A-Z][a-zA-Z0-9_]*", |lex| Some(lex.slice().to_string()))]
    TypeIdent(String),

    // Binary/bit syntax keywords
    #[token("big")]
    Big,
    #[token("little")]
    Little,
    #[token("signed")]
    Signed,
    #[token("unsigned")]
    Unsigned,
    #[token("integer")]
    Integer,
    #[token("float")]
    Float,
    #[token("binary")]
    BinaryKw,
    #[token("bytes")]
    Bytes,
    #[token("utf8")]
    Utf8,

    // Operators (order matters for multi-char operators)
    #[token("<<")]
    LtLt,
    #[token(">>")]
    GtGt,
    #[token("==")]
    EqEq,
    #[token("!=")]
    BangEq,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("|>")]
    PipeRight,
    #[token("::")]
    ColonColon,
    #[token("#[")]
    HashBracket,
    #[token("#")]
    Hash,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("!")]
    Bang,
    #[token("=")]
    Eq,
    #[token("|")]
    Pipe,

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token("..")]
    DotDot,
    #[token(".")]
    Dot,
    #[token("_")]
    Underscore,
    #[token("?")]
    Question,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Mut => write!(f, "mut"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Match => write!(f, "match"),
            Token::Struct => write!(f, "struct"),
            Token::Enum => write!(f, "enum"),
            Token::Mod => write!(f, "mod"),
            Token::Pub => write!(f, "pub"),
            Token::SelfKw => write!(f, "self"),
            Token::Spawn => write!(f, "spawn"),
            Token::Receive => write!(f, "receive"),
            Token::After => write!(f, "after"),
            Token::Return => write!(f, "return"),
            Token::Use => write!(f, "use"),
            Token::As => write!(f, "as"),
            Token::Impl => write!(f, "impl"),
            Token::Trait => write!(f, "trait"),
            Token::For => write!(f, "for"),
            Token::When => write!(f, "when"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Extern => write!(f, "extern"),
            Token::Type => write!(f, "type"),
            Token::Int(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Charlist(s) => write!(f, "'{}'", s),
            Token::Atom(a) => write!(f, ":{}", a),
            Token::QuotedAtom(a) => write!(f, ":'{}'", a),
            Token::Ident(s) => write!(f, "{}", s),
            Token::TypeIdent(s) => write!(f, "{}", s),
            Token::Big => write!(f, "big"),
            Token::Little => write!(f, "little"),
            Token::Signed => write!(f, "signed"),
            Token::Unsigned => write!(f, "unsigned"),
            Token::Integer => write!(f, "integer"),
            Token::Float => write!(f, "float"),
            Token::BinaryKw => write!(f, "binary"),
            Token::Bytes => write!(f, "bytes"),
            Token::Utf8 => write!(f, "utf8"),
            Token::LtLt => write!(f, "<<"),
            Token::GtGt => write!(f, ">>"),
            Token::EqEq => write!(f, "=="),
            Token::BangEq => write!(f, "!="),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::AndAnd => write!(f, "&&"),
            Token::OrOr => write!(f, "||"),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::PipeRight => write!(f, "|>"),
            Token::ColonColon => write!(f, "::"),
            Token::HashBracket => write!(f, "#["),
            Token::Hash => write!(f, "#"),
            Token::Quote => write!(f, "quote"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Bang => write!(f, "!"),
            Token::Eq => write!(f, "="),
            Token::Pipe => write!(f, "|"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semi => write!(f, ";"),
            Token::DotDot => write!(f, ".."),
            Token::Dot => write!(f, "."),
            Token::Underscore => write!(f, "_"),
            Token::Question => write!(f, "?"),
            Token::Crate => write!(f, "crate"),
            Token::Super => write!(f, "super"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lex = Token::lexer("fn let if else match struct enum mod pub");
        assert_eq!(lex.next(), Some(Ok(Token::Fn)));
        assert_eq!(lex.next(), Some(Ok(Token::Let)));
        assert_eq!(lex.next(), Some(Ok(Token::If)));
        assert_eq!(lex.next(), Some(Ok(Token::Else)));
        assert_eq!(lex.next(), Some(Ok(Token::Match)));
        assert_eq!(lex.next(), Some(Ok(Token::Struct)));
        assert_eq!(lex.next(), Some(Ok(Token::Enum)));
        assert_eq!(lex.next(), Some(Ok(Token::Mod)));
        assert_eq!(lex.next(), Some(Ok(Token::Pub)));
    }

    #[test]
    fn test_literals() {
        let mut lex = Token::lexer("42 \"hello\" :ok foo Bar");
        assert_eq!(lex.next(), Some(Ok(Token::Int(42))));
        assert_eq!(lex.next(), Some(Ok(Token::String("hello".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::Atom("ok".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::Ident("foo".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::TypeIdent("Bar".to_string()))));
    }

    #[test]
    fn test_operators() {
        let mut lex = Token::lexer("+ - * / == != < <= > >= && || -> =>");
        assert_eq!(lex.next(), Some(Ok(Token::Plus)));
        assert_eq!(lex.next(), Some(Ok(Token::Minus)));
        assert_eq!(lex.next(), Some(Ok(Token::Star)));
        assert_eq!(lex.next(), Some(Ok(Token::Slash)));
        assert_eq!(lex.next(), Some(Ok(Token::EqEq)));
        assert_eq!(lex.next(), Some(Ok(Token::BangEq)));
        assert_eq!(lex.next(), Some(Ok(Token::Lt)));
        assert_eq!(lex.next(), Some(Ok(Token::LtEq)));
        assert_eq!(lex.next(), Some(Ok(Token::Gt)));
        assert_eq!(lex.next(), Some(Ok(Token::GtEq)));
        assert_eq!(lex.next(), Some(Ok(Token::AndAnd)));
        assert_eq!(lex.next(), Some(Ok(Token::OrOr)));
        assert_eq!(lex.next(), Some(Ok(Token::Arrow)));
        assert_eq!(lex.next(), Some(Ok(Token::FatArrow)));
    }

    #[test]
    fn test_delimiters() {
        let mut lex = Token::lexer("( ) { } [ ] , : ; . _ ::");
        assert_eq!(lex.next(), Some(Ok(Token::LParen)));
        assert_eq!(lex.next(), Some(Ok(Token::RParen)));
        assert_eq!(lex.next(), Some(Ok(Token::LBrace)));
        assert_eq!(lex.next(), Some(Ok(Token::RBrace)));
        assert_eq!(lex.next(), Some(Ok(Token::LBracket)));
        assert_eq!(lex.next(), Some(Ok(Token::RBracket)));
        assert_eq!(lex.next(), Some(Ok(Token::Comma)));
        assert_eq!(lex.next(), Some(Ok(Token::Colon)));
        assert_eq!(lex.next(), Some(Ok(Token::Semi)));
        assert_eq!(lex.next(), Some(Ok(Token::Dot)));
        assert_eq!(lex.next(), Some(Ok(Token::Underscore)));
        assert_eq!(lex.next(), Some(Ok(Token::ColonColon)));
    }

    #[test]
    fn test_comments() {
        let mut lex = Token::lexer("foo // comment\nbar /* block */ baz");
        assert_eq!(lex.next(), Some(Ok(Token::Ident("foo".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::Ident("bar".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::Ident("baz".to_string()))));
    }

    #[test]
    fn test_process_syntax() {
        let mut lex = Token::lexer("spawn receive after my_pid ! :message");
        assert_eq!(lex.next(), Some(Ok(Token::Spawn)));
        assert_eq!(lex.next(), Some(Ok(Token::Receive)));
        assert_eq!(lex.next(), Some(Ok(Token::After)));
        assert_eq!(lex.next(), Some(Ok(Token::Ident("my_pid".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::Bang)));
        assert_eq!(lex.next(), Some(Ok(Token::Atom("message".to_string()))));
    }

    #[test]
    fn test_rust_keywords_are_valid_idents() {
        // This isn't Rust - we can use most Rust keywords as identifiers
        // (except `use`, `as`, `impl`, `trait`, `for`, `type`, `extern`, `crate`, `super` which are now keywords in Dream)
        let mut lex = Token::lexer("loop while");
        assert_eq!(lex.next(), Some(Ok(Token::Ident("loop".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::Ident("while".to_string()))));
    }

    #[test]
    fn test_crate_and_super_keywords() {
        // crate and super are now keywords for Rust-style module paths
        let mut lex = Token::lexer("crate super");
        assert_eq!(lex.next(), Some(Ok(Token::Crate)));
        assert_eq!(lex.next(), Some(Ok(Token::Super)));
    }

    #[test]
    fn test_extern_and_type_keywords() {
        // extern and type are now keywords for FFI type stubs
        let mut lex = Token::lexer("extern type");
        assert_eq!(lex.next(), Some(Ok(Token::Extern)));
        assert_eq!(lex.next(), Some(Ok(Token::Type)));
    }

    #[test]
    fn test_quoted_atoms() {
        // Single-quoted atoms
        let mut lex = Token::lexer(":'Elixir.Enum' :'my-atom'");
        assert_eq!(
            lex.next(),
            Some(Ok(Token::QuotedAtom("Elixir.Enum".to_string())))
        );
        assert_eq!(
            lex.next(),
            Some(Ok(Token::QuotedAtom("my-atom".to_string())))
        );

        // Double-quoted atoms
        let mut lex = Token::lexer(r#":"Elixir.Jason" :"with.dots""#);
        assert_eq!(
            lex.next(),
            Some(Ok(Token::QuotedAtom("Elixir.Jason".to_string())))
        );
        assert_eq!(
            lex.next(),
            Some(Ok(Token::QuotedAtom("with.dots".to_string())))
        );
    }

    #[test]
    fn test_attributes() {
        // Attribute syntax: #[name]
        let mut lex = Token::lexer("#[test]");
        assert_eq!(lex.next(), Some(Ok(Token::HashBracket)));
        assert_eq!(lex.next(), Some(Ok(Token::Ident("test".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::RBracket)));

        // Attribute with args: #[cfg(test)]
        let mut lex = Token::lexer("#[cfg(test)]");
        assert_eq!(lex.next(), Some(Ok(Token::HashBracket)));
        assert_eq!(lex.next(), Some(Ok(Token::Ident("cfg".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::LParen)));
        assert_eq!(lex.next(), Some(Ok(Token::Ident("test".to_string()))));
        assert_eq!(lex.next(), Some(Ok(Token::RParen)));
        assert_eq!(lex.next(), Some(Ok(Token::RBracket)));
    }
}
