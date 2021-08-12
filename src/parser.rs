lalrpop_mod!(pub grammar); // synthesized by LALRPOP


(3 + (-4)) >= 0

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
            parse_str("if ((3 + (-4)) >= 0) int a;"),
            Statement(IfStatement {
                condition: (Binary(
                    Box::new(Binary(
                        Box::new(IntLiteral(3)),
                        Add,
                        Box::new(Unary(Negative, Box::new(IntLiteral(4))))
                    )),
                    GreaterThanEqual,
                    Box::new(IntLiteral(0))
                )),
                true_path: Box::new(VariableDeclaration(Int, "a", None)),
                false_path: None,
            })
        );
    }
}
