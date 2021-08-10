#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'input> {
    Str(&'input str),
    Number(i64),
    Op {
        lhs: Box<Expr<'input>>, // left-hand side
        op: Opcode,
        rhs: Box<Expr<'input>>, // right-hand side
    },
    If {
        condition: Box<Expr<'input>>,
        then_expr: Box<Expr<'input>>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
}

lalrpop_mod!(pub grammar); // synthesized by LALRPOP

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Lexer, Token};
    use Expr::*;

    fn parse_str(input: &str) -> Expr {
        let lexer = Lexer::new(input, 1);
        *grammar::ExprParser::new()
            .parse(input, lexer.map(Token::to_spanned))
            .expect("parsing failed")
    }

    #[test]
    fn parse_if_expr() {
        assert_eq!(
            parse_str("if 1 { 123 }"),
            If {
                condition: Box::new(Number(1)),
                then_expr: Box::new(Number(123)),
            }
        );
    }

    #[test]
    fn parse_num_expr() {
        assert_eq!(
            parse_str("1 + 2 * 4"),
            Op {
                lhs: Box::new(Number(1)),
                op: Opcode::Add,
                rhs: Box::new(Op {
                    lhs: Box::new(Number(2)),
                    op: Opcode::Mul,
                    rhs: Box::new(Number(4)),
                }),
            }
        );
    }
}
