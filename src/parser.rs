use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ast::Program,
    lexer::{LexicalError, Location, Token, TokenKind},
};

lalrpop_mod!(pub grammar); // synthesized by LALRPOP

#[derive(Debug, Clone)]
pub struct SyntaxError<'source>(
    lalrpop_util::ParseError<Location<'source>, TokenKind<'source>, LexicalError<'source>>,
);

fn error_diagnostic_from_location(loc: &Location) -> Diagnostic<usize> {
    Diagnostic::error()
        .with_message("syntax error")
        .with_labels(vec![Label::primary(loc.file_id, loc.span.clone())])
}
impl<'source> SyntaxError<'source> {
    pub fn to_diagnostic(&self) -> Diagnostic<usize> {
        use lalrpop_util::ParseError::*;
        match &self.0 {
            ExtraToken {
                token: (loc, token, _),
            } => error_diagnostic_from_location(loc)
                .with_message(format!("extra token: {:?}", token)),
            InvalidToken { location } => error_diagnostic_from_location(location)
                .with_message(format!("invalid token: {:?}", location.text,)),
            UnrecognizedEOF { location, expected } => error_diagnostic_from_location(location)
                .with_message(format!(
                    "file ended too early, expected: {:?}",
                    expected.join(",").replace('"', "")
                )),
            UnrecognizedToken {
                token: (loc, _, _),
                expected,
            } => error_diagnostic_from_location(loc).with_message(format!(
                "found token: {:?}, expected: {:?}",
                loc.text,
                expected.join(",").replace('"', "")
            )),
            User { error } => error.to_diagnostic(),
        }
    }
}

pub fn parse<'a, TokenIter: Iterator<Item = Token<'a>>>(
    tokens: TokenIter,
) -> Result<Program<'a>, SyntaxError<'a>> {
    grammar::PROGRAMParser::new()
        .parse(tokens.map(Token::to_spanned))
        .map_err(SyntaxError)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        BinOp::*, Expression::*, LValue::*, Program::*, Statement::*, Type::*, UnaryOp::*, *,
    };
    use crate::lexer::Lexer;

    fn parse_str(input: &str) -> Program {
        parse(Lexer::new(input, 1)).expect("parsing failed")
    }

    #[test]
    fn parse_statement() {
        assert_eq!(
            parse_str("int a;"),
            Statement(VariableDeclaration(Int, "a"))
        );
        assert_eq!(
            parse_str("int a[3][4];"),
            Statement(VariableDeclaration(
                Array(
                    Box::new(Array(Box::new(Int), Box::new(IntLiteral(3)))),
                    Box::new(IntLiteral(4))
                ),
                "a"
            ))
        );
    }

    #[test]
    fn parse_funclist() {
        assert_eq!(
            parse_str("def foo() { int a; }"),
            FuncList(vec![FunctionDefinition {
                name: "foo",
                parameters: vec![],
                body: vec![VariableDeclaration(Int, "a")]
            }])
        );
    }

    #[test]
    fn parse_ifstat() {
        assert_eq!(
            parse_str("if (-3 >= 0) { int a; }"),
            Statement(If {
                condition: (Binary(
                    Box::new(Unary(Negative, Box::new(IntLiteral(3)))),
                    GreaterThanEqual,
                    Box::new(IntLiteral(0))
                )),
                true_path: Box::new(StatementList(vec![VariableDeclaration(Int, "a")])),
                false_path: None,
            })
        );
        assert_eq!(
            parse_str("if (1) { a = 1; } else { a = 2; }"),
            Statement(If {
                condition: IntLiteral(1),
                true_path: Box::new(StatementList(vec![Assignment(
                    NameReference("a"),
                    IntLiteral(1)
                )])),
                false_path: Some(Box::new(StatementList(vec![Assignment(
                    NameReference("a"),
                    IntLiteral(2)
                )]))),
            })
        );
    }

    #[test]
    fn parse_read() {
        assert_eq!(
            parse_str("read nome;"),
            Statement(Read(NameReference("nome")))
        );
    }
    #[test]
    fn parse_return() {
        assert_eq!(parse_str("return;"), Statement(Return));
    }
    #[test]
    fn parse_print() {
        assert_eq!(
            parse_str("print (3 * 4);"),
            Statement(Print(Binary(
                Box::new(IntLiteral(3)),
                Mul,
                Box::new(IntLiteral(4))
            )))
        );
    }
    #[test]
    fn parse_attrib() {
        assert_eq!(
            parse_str("a = 3;"),
            Statement(Assignment(NameReference("a"), IntLiteral(3)))
        );
    }
    #[test]
    fn parse_for() {
        let program = r"
        for (i = 0; i < 10; i = i + 1) {
            int a;
            a = i * i;
        }";
        assert_eq!(
            parse_str(program),
            Statement(For {
                initial_assignment: Box::new(Assignment(NameReference("i"), IntLiteral(0))),
                condition: Binary(
                    Box::new(LValue(Box::new(NameReference("i")))),
                    LessThan,
                    Box::new(IntLiteral(10))
                ),
                post_assignment: Box::new(Assignment(
                    NameReference("i"),
                    Binary(
                        Box::new(LValue(Box::new(NameReference("i")))),
                        Add,
                        Box::new(IntLiteral(1))
                    )
                )),
                body: Box::new(StatementList(vec![
                    VariableDeclaration(Int, "a"),
                    Assignment(
                        NameReference("a"),
                        Binary(
                            Box::new(LValue(Box::new(NameReference("i")))),
                            Mul,
                            Box::new(LValue(Box::new(NameReference("i"))))
                        )
                    )
                ]))
            }),
        );
    }
    #[test]
    fn parse_funccall() {
        assert_eq!(
            parse_str("a = foo(a, b);"),
            Statement(Assignment(
                NameReference("a"),
                FunctionCall("foo", vec!["a", "b"])
            ))
        );
    }

    #[test]
    fn parse_array_access() {
        assert_eq!(
            parse_str("print arr[3][2 * 4];"),
            Statement(Print(LValue(Box::new(ArrayAccess(
                Box::new(ArrayAccess(
                    Box::new(NameReference("arr")),
                    Box::new(IntLiteral(3))
                )),
                Box::new(Binary(
                    Box::new(IntLiteral(2)),
                    Mul,
                    Box::new(IntLiteral(4))
                ))
            )))))
        );
    }
    #[test]
    fn parse_array_element_assignment() {
        assert_eq!(
            parse_str("arr[3] = foo();"),
            Statement(Assignment(
                ArrayAccess(Box::new(NameReference("arr")), Box::new(IntLiteral(3))),
                FunctionCall("foo", vec![])
            ))
        );
    }
    #[test]
    fn parse_array_allocation() {
        assert_eq!(
            parse_str("arr = new int[10];"),
            Statement(Assignment(
                NameReference("arr"),
                Alloc(Array(Box::new(Int), Box::new(IntLiteral(10))))
            ))
        );
    }
    #[test]
    fn parse_complex_expression() {
        assert_eq!(
            parse_str("print 1 + 2 + 3;"),
            Statement(Print(Binary(
                Box::new(Binary(
                    Box::new(IntLiteral(1)),
                    Add,
                    Box::new(IntLiteral(2))
                )),
                Add,
                Box::new(IntLiteral(3))
            )))
        )
    }
}
