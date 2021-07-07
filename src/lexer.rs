use logos::{Logos, SpannedIter};

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'a> {
    #[token("def")]
    FunctionKeyword,
    #[token("int")]
    IntType,
    #[token("float")]
    FloatType,
    #[token("string")]
    String,
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
    BiggerThanEqual,
    #[token("==")]
    Equals,
    #[token("!=")]
    DiffentThan,
    #[token("<")]
    LessThan,
    #[token(">")]
    BiggerThan,
    #[token("=")]
    Atribuition,
    #[token("+")]
    Addition,
    #[token("-")]
    Subtraction,
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

pub fn lex<'source>(source: &'source str) -> SpannedIter<'source, Token> {
    Token::lexer(source).spanned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn lex_function() {
        let result: Vec<_> = Token::lexer("def foo() {}").collect();
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
        let result: Vec<_> = Token::lexer("def foo^").collect();

        assert_eq!(result, &[FunctionKeyword, Identifier("foo"), Error]);
    }

    #[test]
    fn lex_int() {
        let result: Vec<_> = Token::lexer("123").collect();

        assert_eq!(result, &[IntegerConstant(123)]);
    }

    #[test]
    fn lex_float() {
        let result: Vec<_> = Token::lexer("123.22").collect();

        assert_eq!(result, &[FloatConstant(123.22)]);
    }
}
