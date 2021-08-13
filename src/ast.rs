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
    // e.g. 3 + 4
    Binary(Box<Expression<'a>>, BinOp, Box<Expression<'a>>),
    // e.g. -4
    Unary(UnaryOp, Box<Expression<'a>>),
    // e.g. 4
    IntLiteral(i64),
    // e.g. 5.3
    FloatLiteral(f64),
    // e.g. "asd"
    StringLiteral(&'a str),
    // e.g. null
    Null,
    // e.g. a[0]
    LValue(Box<LValue<'a>>),
    // e.g. foo()
    FunctionCall(&'a str, Vec<&'a str>),
    // e.g. new int[10]
    Alloc(Type<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LValue<'a> {
    // int a; a
    NameReference(&'a str),
    // int a[10]; a[2]
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
