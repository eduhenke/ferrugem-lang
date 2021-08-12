#[derive(Debug, PartialEq, Clone)]
pub enum Program<'a> {
    Statement(Statement<'a>),
    FuncList(Vec<FunctionDefinition<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub parameters: Vec<(Type, &'a str)>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int,
    Float,
    String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    VariableDeclaration(Type, &'a str, Option<i64>),
    If {
        condition: Expression<'a>,
        true_path: Box<Statement<'a>>,
        false_path: Option<Box<Statement<'a>>>,
    },
    StatementList(Vec<Statement<'a>>),
    Read(&'a str),
    Print(Expression<'a>),
    Return,
    Break,
    Assignment(&'a str, Expression<'a>),
    For {
        initial_assignment: Box<Statement<'a>>,
        condition: Expression<'a>,
        post_assignment: Box<Statement<'a>>,
        body: Box<Statement<'a>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Binary(Box<Expression<'a>>, BinOp, Box<Expression<'a>>),
    Unary(UnaryOp, Box<Expression<'a>>),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(&'a str),
    Null,
    NameReference(&'a str),
    // TODO: missing array index? ident[num]
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOp {
    // comparison
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equals,
    NotEquals,

    // numeric
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp {
    Negative,
    Positive,
}
