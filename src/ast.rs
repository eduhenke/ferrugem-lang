#[derive(Debug, PartialEq, Clone)]
pub enum Program<'a> {
    Statement(Statement<'a>),
    FuncList(Vec<FunctionDefinition<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub parameters: Vec<(Type<'a>, &'a str)>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type<'a> {
    Int,
    Float,
    String,
    Array(Box<Type<'a>>, Box<Expression<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    VariableDeclaration(Type<'a>, &'a str),
    If {
        condition: Expression<'a>,
        true_path: Box<Statement<'a>>,
        false_path: Option<Box<Statement<'a>>>,
    },
    StatementList(Vec<Statement<'a>>),
    Read(LValue<'a>),
    Print(Expression<'a>),
    Return,
    Break,
    Assignment(LValue<'a>, Expression<'a>),
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
    LValue(Box<LValue<'a>>),
    FunctionCall(&'a str, Vec<&'a str>),
    Alloc(Type<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LValue<'a> {
    NameReference(&'a str),
    ArrayAccess(Box<LValue<'a>>, Box<Expression<'a>>),
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
