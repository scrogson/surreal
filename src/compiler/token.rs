//! Token definitions for the lexer.

use logos::Logos;

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
    #[token("spawn")]
    Spawn,
    #[token("receive")]
    Receive,
    #[token("after")]
    After,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("false")]
    False,


    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Int(i64),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        Some(s[1..s.len()-1].to_string())
    })]
    String(String),

    #[regex(r":[a-z_][a-z0-9_]*", |lex| Some(lex.slice()[1..].to_string()))]
    Atom(String),

    #[regex(r"[a-z_][a-z0-9_]*", |lex| Some(lex.slice().to_string()), priority = 1)]
    Ident(String),

    #[regex(r"[A-Z][a-zA-Z0-9_]*", |lex| Some(lex.slice().to_string()))]
    TypeIdent(String),

    // Operators (order matters for multi-char operators)
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
    #[token("::")]
    ColonColon,

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
    #[token(".")]
    Dot,
    #[token("_")]
    Underscore,
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
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Int(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Atom(a) => write!(f, ":{}", a),
            Token::Ident(s) => write!(f, "{}", s),
            Token::TypeIdent(s) => write!(f, "{}", s),
            Token::EqEq => write!(f, "=="),
            Token::BangEq => write!(f, "!="),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::AndAnd => write!(f, "&&"),
            Token::OrOr => write!(f, "||"),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::ColonColon => write!(f, "::"),
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
            Token::Dot => write!(f, "."),
            Token::Underscore => write!(f, "_"),
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
}
