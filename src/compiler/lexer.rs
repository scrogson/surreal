//! Lexer wrapper with span tracking.

use logos::{Logos, SpannedIter};

use crate::compiler::token::Token;

/// A span in the source code.
pub type Span = std::ops::Range<usize>;

/// A token with its span in the source.
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

/// Lexer that wraps logos and provides span information.
pub struct Lexer<'source> {
    inner: SpannedIter<'source, Token>,
    source: &'source str,
}

impl<'source> Lexer<'source> {
    /// Create a new lexer for the given source code.
    pub fn new(source: &'source str) -> Self {
        Self {
            inner: Token::lexer(source).spanned(),
            source,
        }
    }

    /// Get the source code.
    pub fn source(&self) -> &'source str {
        self.source
    }

    /// Collect all tokens into a vector.
    pub fn collect_tokens(&mut self) -> Vec<SpannedToken> {
        let mut tokens = Vec::new();
        while let Some(tok) = self.next() {
            tokens.push(tok);
        }
        tokens
    }
}

impl Iterator for Lexer<'_> {
    type Item = SpannedToken;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.inner.next() {
                Some((Ok(token), span)) => {
                    return Some(SpannedToken { token, span });
                }
                Some((Err(()), span)) => {
                    // Skip invalid tokens for now, parser will handle errors
                    eprintln!("Lexer error at {:?}: {:?}", span, &self.source[span.clone()]);
                    continue;
                }
                None => return None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_spans() {
        let source = "fn foo() {}";
        let mut lexer = Lexer::new(source);

        let tok = lexer.next().unwrap();
        assert_eq!(tok.token, Token::Fn);
        assert_eq!(tok.span, 0..2);

        let tok = lexer.next().unwrap();
        assert_eq!(tok.token, Token::Ident("foo".to_string()));
        assert_eq!(tok.span, 3..6);
    }

    #[test]
    fn test_collect_tokens() {
        let source = "let x = 42;";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.collect_tokens();

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].token, Token::Let);
        assert_eq!(tokens[1].token, Token::Ident("x".to_string()));
        assert_eq!(tokens[2].token, Token::Eq);
        assert_eq!(tokens[3].token, Token::Int(42));
        assert_eq!(tokens[4].token, Token::Semi);
    }
}
