use std::usize;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use logos::{Logos, SpannedIter};

#[derive(Debug, Clone)]
pub enum LexicalError<'source> {
    InvalidSymbol(Token<'source>),
}

impl<'source> LexicalError<'source> {
    pub fn to_diagnostic(&self) -> Diagnostic<usize> {
        match self.clone() {
            LexicalError::InvalidSymbol(token) => Diagnostic::error()
                .with_message("lexical error")
                .with_labels(vec![Label::primary(token.loc.file_id, token.loc.span)
                    .with_message("invalid symbol")]),
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum TokenKind<'a> {
    #[token("def")]
    FunctionKeyword,
    #[token("int")]
    IntType,
    #[token("float")]
    FloatType,
    #[token("string")]
    StringType,
    #[token("print")]
    Print,
    #[token("read")]
    Read,
    #[token("return")]
    Return,
    #[token("for")]
    For,
    #[token("break")]
    Break,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("new")]
    New,
    #[token("null")]
    Null,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token("(")]
    OpenParenthesis,
    #[token(")")]
    CloseParenthesis,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("{")]
    OpenBraces,
    #[token("}")]
    CloseBraces,
    #[token("<=")]
    LessThanEqual,
    #[token(">=")]
    GreaterThanEqual,
    #[token("==")]
    Equals,
    #[token("!=")]
    NotEquals,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("=")]
    Assignment,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiplication,
    #[token("/")]
    Division,
    #[token("%")]
    Mod,
    #[regex("[[:alpha:]_][[:word:]]*")]
    Identifier(&'a str),
    #[regex("\\d\\d*\\.\\d\\d*", |lex| lex.slice().parse())]
    FloatConstant(f64),
    #[regex("\\d\\d*", |lex| lex.slice().parse())]
    IntegerConstant(i64),
    #[regex("\"[^\"]*\"")]
    StringConstant(&'a str),

    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'source> {
    pub kind: TokenKind<'source>,
    pub loc: Location<'source>,
}
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Location<'source> {
    pub span: core::ops::Range<usize>,
    pub file_id: usize,
    pub text: &'source str,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'source> Token<'source> {
    pub fn to_error(self) -> Option<LexicalError<'source>> {
        match self.kind {
            TokenKind::Error => Some(LexicalError::InvalidSymbol(self)),
            _ => None,
        }
    }

    pub fn to_spanned(
        self,
    ) -> Spanned<TokenKind<'source>, Location<'source>, LexicalError<'source>> {
        match self.kind {
            TokenKind::Error => Err(self.to_error().unwrap()),
            kind => Ok((self.loc.clone(), kind, self.loc)),
        }
    }
}

pub struct Lexer<'source> {
    file_id: usize,
    iter: SpannedIter<'source, TokenKind<'source>>,
    source: &'source str,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str, file_id: usize) -> Self {
        let iter = TokenKind::lexer(source).spanned();
        Lexer {
            file_id,
            iter,
            source,
        }
    }
}
impl<'source> Iterator for Lexer<'source> {
    type Item = Token<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(token, span)| Token {
            kind: token,
            loc: Location {
                file_id: self.file_id,
                text: &self.source[span.clone()],
                span,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind::*;

    #[test]
    fn lex_function() {
        let result: Vec<_> = TokenKind::lexer("def foo() {}").collect();
        assert_eq!(
            result,
            &[
                FunctionKeyword,
                Identifier("foo"),
                OpenParenthesis,
                CloseParenthesis,
                OpenBraces,
                CloseBraces
            ]
        );
    }

    #[test]
    fn lex_function_error() {
        let result: Vec<_> = TokenKind::lexer("def foo^").collect();

        assert_eq!(result, &[FunctionKeyword, Identifier("foo"), Error]);
    }

    #[test]
    fn lex_int() {
        let result: Vec<_> = TokenKind::lexer("123").collect();

        assert_eq!(result, &[IntegerConstant(123)]);
    }

    #[test]
    fn lex_float() {
        let result: Vec<_> = TokenKind::lexer("123.22").collect();
        assert_eq!(result, &[FloatConstant(123.22)]);
    }

    #[test]
    fn lex_semi_keyword() {
        let result: Vec<_> = TokenKind::lexer("defined").collect();
        assert_eq!(result, &[Identifier("defined")]);
    }
}
