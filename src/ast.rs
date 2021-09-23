use std::{borrow::Borrow, fmt::Display, ops::Deref};

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
    Null,
    Void,
    String,
    Array(Box<Type<'a>>, Box<Expression<'a>>),
    Function {
        return_type: Box<Type<'a>>,
        params_types: Vec<Type<'a>>,
    },
}

impl Type<'_> {
    pub fn can_be_casted_to(&self, other: &Type) -> bool {
        if self.is_numeric() && other.is_numeric() {
            true
        } else {
            if let Type::Array(a_base, _a_size) = self {
                if let Type::Array(b_base, _b_size) = other {
                    return a_base.can_be_casted_to(b_base);
                }
            }
            self == other
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Int => true,
            Type::Float => true,
            _ => false,
        }
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Null => write!(f, "null"),
            Type::Void => write!(f, "void"),
            Type::String => write!(f, "string"),
            Type::Array(ty, size) => write!(
                f,
                "{}[{}]",
                ty,
                match size.deref() {
                    Expression::IntLiteral(num) => num.to_string(),
                    Expression::StringLiteral(var) => var.to_string(),
                    Expression::LValue(lval) => match lval.borrow() {
                        LValue::NameReference(name) => name.to_string(),
                        LValue::ArrayAccess(_, _) => todo!(),
                    },
                    x => panic!("{:?}", x),
                }
            ),
            Type::Function {
                return_type: _,
                params_types: _,
            } => todo!(),
        }
    }
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::LessThan => write!(f, "<"),
            BinOp::GreaterThan => write!(f, ">"),
            BinOp::LessThanEqual => write!(f, "<="),
            BinOp::GreaterThanEqual => write!(f, ">="),
            BinOp::Equals => write!(f, "=="),
            BinOp::NotEquals => write!(f, "!="),
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOp {
    Negative,
    Positive,
}
impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Negative => write!(f, "-"),
            UnaryOp::Positive => write!(f, "+"),
        }
    }
}
