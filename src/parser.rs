use crate::lexer::{Token, TokenKind};

lalrpop_mod!(pub calculator1); // synthesized by LALRPOP

#[test]
fn calculator1() {
    assert!(calculator1::TermParser::new().parse("22").is_ok());
    assert!(calculator1::TermParser::new().parse("(22)").is_ok());
    assert!(calculator1::TermParser::new().parse("((((22))))").is_ok());
    assert!(calculator1::TermParser::new().parse("((22)").is_err());

    // assert!(calculator1::TermParser::new().parse([
    //     TokenKind::OpenParenthesis,
    //     TokenKind::IntegerConstant(22),
    //     TokenKind::CloseParenthesis
    // ]).is_ok());
}
