lalrpop_mod!(pub grammar); // synthesized by LALRPOP

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp::*, Expression::*, Program::*, Statement::*, Type::*, UnaryOp::*, *};
    use crate::lexer::{Lexer, Token};

    fn parse_str(input: &str) -> Program {
        let lexer = Lexer::new(input, 1);
        grammar::PROGRAMParser::new()
            .parse(lexer.map(Token::to_spanned))
            .expect("parsing failed")
    }

    #[test]
    fn parse_statement() {
        assert_eq!(
            parse_str("int a;"),
            Statement(VariableDeclaration(Int, "a", None))
        );
    }

    #[test]
    fn parse_funclist() {
        assert_eq!(
            parse_str("def foo() { int a; }"),
            FuncList(vec![FunctionDefinition {
                name: "foo",
                parameters: vec![],
                body: vec![VariableDeclaration(Int, "a", None)]
            }])
        );
    }

    #[test]
    fn parse_ifstat() {
        assert_eq!(
            parse_str("if (-3 >= 0) int a;"),
            Statement(If {
                condition: (Binary(
                    Box::new(Unary(Negative, Box::new(IntLiteral(3)))),
                    GreaterThanEqual,
                    Box::new(IntLiteral(0))
                )),
                true_path: Box::new(VariableDeclaration(Int, "a", None)),
                false_path: None,
            })
        );
    }

    #[test]
    fn parse_read() {
        assert_eq!(parse_str("read nome;"), Statement(Read("nome")));
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
            Statement(Assignment("a", IntLiteral(3)))
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
                initial_assignment: Box::new(Assignment("i", IntLiteral(0))),
                condition: Binary(
                    Box::new(NameReference("i")),
                    LessThan,
                    Box::new(IntLiteral(10))
                ),
                post_assignment: Box::new(Assignment(
                    "i",
                    Binary(Box::new(NameReference("i")), Add, Box::new(IntLiteral(1)))
                )),
                body: Box::new(StatementList(vec![
                    VariableDeclaration(Int, "a", None),
                    Assignment(
                        "a",
                        Binary(
                            Box::new(NameReference("i")),
                            Mul,
                            Box::new(NameReference("i"))
                        )
                    )
                ]))
            }),
        );
    }
}
