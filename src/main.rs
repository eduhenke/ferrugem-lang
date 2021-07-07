use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
enum Token {
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
    #[token("{")]
    OpenBraces,
    #[token("}")]
    CloseBraces,
    #[token("=")]
    Equals,
    #[token("break")]
    Break,
    #[token("def")]
    FunctionKeyword,
    #[regex("[[:alpha:]_][[:word:]]*")]
    Identifier,

    #[regex("AAAAAA")]
    FloatConstant,
    #[regex("[[:digit:]][[:digit:]_]*([[:alpha:]][[:word:]]*)?")]
    IntegerConstant,
    
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

fn main() {
    let lex = Token::lexer("Create ridiculously fast Lexers.");

    for token in lex {
        println!("{:?}", token);
    }
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
            &[FunctionKeyword, Identifier, OpenParenthesis, CloseParenthesis, OpenBraces, CloseBraces]
        );
    }

    #[test]
    fn lex_function_error() {    
        let result: Vec<_> = Token::lexer("def foo^").collect();

        assert_eq!(
            result,
            &[
                FunctionKeyword,
                Identifier,
                Error
            ]
        );
    }

    #[test]
    fn lex_int() {    
        let result: Vec<_> = Token::lexer("123").collect();

        assert_eq!(
            result,
            &[IntegerConstant]
        );
    }

    #[test]
    fn lex_float() {    
        let result: Vec<_> = Token::lexer("123.22").collect();

        assert_eq!(
            result,
            &[FloatConstant]
        );
    }
}