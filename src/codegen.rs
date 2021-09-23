use std::fmt::{self, Display};

use crate::ast::{BinOp, Expression, LValue, Program, Statement, UnaryOp};

#[derive(Debug, Clone)]
pub enum Operand {
    Literal(String),
    Variable(String),
}

impl Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Literal(str) => write!(f, "{}", str),
            Operand::Variable(str) => write!(f, "{}", str),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Context(pub i32, pub i32);

impl Context {
    pub fn new() -> Self {
        Context(0, 0)
    }

    fn get_new_name(&mut self) -> String {
        self.0 += 1;
        format!("t{}", self.0)
    }
    fn get_new_label(&mut self) -> String {
        self.1 += 10;
        format!("L{}", self.1)
    }
}

// Um código de representação intermediaŕio de 3 endereços
#[derive(Debug, Clone)]
pub enum IR {
    Label(String),
    Operand(Operand),
    BinOp(String, Operand, BinOp, Operand),
    UnaryOp(String, UnaryOp, Operand),
    Assignment(String, Operand),
    AccessArray(String, Operand, Operand),
    Alloc(String, Operand),
    Declare(String, Operand),
    Command(String, Operand),
    IfTrue(String, String),
    IfFalse(String, String),
    Goto(String),
}

fn get_last_var_name(instructions: &Vec<IR>) -> Operand {
    instructions.last().unwrap().get_var_name()
}

impl Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IR::BinOp(var, rhs, op, lhs) => write!(f, "\t{} = {}{}{}", var, rhs, op, lhs),
            IR::UnaryOp(var, op, operand) => write!(f, "\t{} = {}{}", var, op, operand),
            IR::Assignment(var, operand) => write!(f, "\t{} = {}", var, operand),
            IR::Command(command, operand) => write!(f, "\t{} {}", command, operand),
            IR::Alloc(var, operand) => write!(f, "\t{} = alloc {}", var, operand),
            IR::Label(label) => write!(f, "{}:", label),
            IR::Operand(_operand) => write!(f, ""),
            IR::IfTrue(cond, label) => write!(f, "\tif {} goto {}", cond, label),
            IR::IfFalse(cond, label) => write!(f, "\tif False {} goto {}", cond, label),
            IR::Goto(label) => write!(f, "\tgoto {}", label),
            IR::Declare(ty, name) => write!(f, "\t{} {}", ty, name),
            IR::AccessArray(var, operand, index) => write!(f, "\t{} = {}[{}]", var, operand, index),
        }
    }
}

impl IR {
    fn get_var_name(&self) -> Operand {
        match self {
            IR::BinOp(var, _, _, _) => Operand::Variable(var.to_string()),
            IR::UnaryOp(var, _, _) => Operand::Variable(var.to_string()),
            IR::Assignment(var, _) => Operand::Variable(var.to_string()),
            IR::Alloc(var, _) => Operand::Variable(var.to_string()),
            IR::Operand(var) => Operand::Variable(var.to_string()),
            IR::AccessArray(var, _, _) => Operand::Variable(var.to_string()),
            _ => panic!("there is no variable name for this variant"),
        }
    }
}

pub(crate) trait CodeGeneratable {
    fn generate_code(&self, ctx: &mut Context) -> Vec<IR>;
}

impl CodeGeneratable for Expression<'_> {
    fn generate_code(&self, ctx: &mut Context) -> Vec<IR> {
        match self {
            Expression::Binary(rhs, op, lhs) => {
                let r_code = rhs.generate_code(ctx);
                let l_code = lhs.generate_code(ctx);
                let var = ctx.get_new_name();
                let code = IR::BinOp(
                    var.clone(),
                    get_last_var_name(&r_code),
                    *op,
                    get_last_var_name(&l_code),
                );
                [r_code, l_code, vec![code]].concat()
            }
            Expression::Unary(op, expr) => {
                let expr_code = expr.generate_code(ctx);
                let var = ctx.get_new_name();
                let code = IR::UnaryOp(var.clone(), *op, get_last_var_name(&expr_code));
                [expr_code, vec![code]].concat()
            }
            Expression::IntLiteral(val) => vec![IR::Operand(Operand::Literal(val.to_string()))],
            Expression::FloatLiteral(val) => vec![IR::Operand(Operand::Literal(val.to_string()))],
            Expression::StringLiteral(val) => vec![IR::Operand(Operand::Literal(val.to_string()))],
            Expression::Null => vec![IR::Operand(Operand::Literal("null".to_string()))],
            Expression::LValue(lval) => lval.generate_code(ctx),
            Expression::FunctionCall(_, _) => todo!(),
            Expression::Alloc(ty) => vec![IR::Alloc(
                ctx.get_new_name(),
                Operand::Variable(ty.to_string()),
            )],
        }
    }
}

impl CodeGeneratable for Statement<'_> {
    fn generate_code(&self, ctx: &mut Context) -> Vec<IR> {
        match self {
            Statement::VariableDeclaration(ty, name) => {
                vec![IR::Declare(
                    ty.to_string(),
                    Operand::Variable(name.to_string()),
                )]
            }
            Statement::If {
                condition,
                true_path,
                false_path,
            } => {
                let l_begin = ctx.get_new_label();
                let l_true = ctx.get_new_label();
                let l_false = ctx.get_new_label();
                let l_end = ctx.get_new_label();
                let cond_code = condition.generate_code(ctx);
                let cond_var = get_last_var_name(&cond_code);
                [
                    vec![IR::Label(l_begin)],
                    cond_code,
                    vec![
                        IR::IfTrue(cond_var.to_string(), l_true.clone()),
                        IR::IfFalse(cond_var.to_string(), l_false.clone()),
                    ],
                    vec![IR::Label(l_true)],
                    true_path.generate_code(ctx),
                    vec![IR::Goto(l_end.clone()), IR::Label(l_false)],
                    false_path
                        .as_ref()
                        .map(|stmt| stmt.generate_code(ctx))
                        .unwrap_or(vec![]),
                    vec![IR::Label(l_end)],
                ]
                .concat()
            }
            Statement::StatementList(stmts) => stmts.into_iter().fold(vec![], |code, stmt| {
                let n_code = stmt.generate_code(ctx);
                [code, n_code].concat()
            }),
            Statement::Read(lval) => {
                let l_code = lval.generate_code(ctx);
                [
                    l_code.clone(),
                    vec![IR::Command("read".to_string(), get_last_var_name(&l_code))],
                ]
                .concat()
            }
            Statement::Print(expr) => {
                let code = expr.generate_code(ctx);
                let var = get_last_var_name(&code);
                [code, vec![IR::Command("print".to_string(), var)]].concat()
            }
            Statement::Return => vec![IR::Command(
                "return".to_string(),
                Operand::Literal(String::new()),
            )],
            Statement::Break => vec![IR::Command(
                "break".to_string(),
                Operand::Literal(String::new()),
            )],
            Statement::Assignment(lval, expr) => {
                let l_code = lval.generate_code(ctx);
                let expr_code = expr.generate_code(ctx);
                [
                    l_code.clone(),
                    expr_code.clone(),
                    vec![IR::Assignment(
                        get_last_var_name(&l_code).to_string(),
                        get_last_var_name(&expr_code),
                    )],
                ]
                .concat()
            }
            Statement::For {
                initial_assignment,
                condition,
                post_assignment,
                body,
            } => {
                let l_check = ctx.get_new_label();
                let l_true = ctx.get_new_label();
                let l_false = ctx.get_new_label();
                let cond_code = condition.generate_code(ctx);
                let cond_var = get_last_var_name(&cond_code);
                [
                    initial_assignment.generate_code(ctx),
                    vec![IR::Label(l_check.clone())],
                    cond_code,
                    vec![
                        IR::IfTrue(cond_var.to_string(), l_true.clone()),
                        IR::IfFalse(cond_var.to_string(), l_false.clone()),
                    ],
                    vec![IR::Label(l_true)],
                    body.generate_code(ctx),
                    post_assignment.generate_code(ctx),
                    vec![IR::Goto(l_check)],
                    vec![IR::Label(l_false)],
                ]
                .concat()
            }
        }
    }
}

impl CodeGeneratable for Program<'_> {
    fn generate_code(&self, ctx: &mut Context) -> Vec<IR> {
        match self {
            Program::Statement(stmt) => stmt.generate_code(ctx),
            // TODO: not entirely implemented
            // we won't have function calls and parameters working correctly
            Program::FuncList(f) => f
                .into_iter()
                .flat_map(|f| {
                    [
                        vec![IR::Label(ctx.get_new_label())],
                        f.clone()
                            .body
                            .into_iter()
                            .flat_map(|stmt| stmt.generate_code(ctx))
                            .collect::<Vec<IR>>(),
                    ]
                    .concat()
                })
                .collect(),
        }
    }
}

impl CodeGeneratable for LValue<'_> {
    fn generate_code(&self, ctx: &mut Context) -> Vec<IR> {
        match self {
            LValue::NameReference(name) => vec![IR::Operand(Operand::Variable(name.to_string()))],
            LValue::ArrayAccess(lval, expr) => {
                let l_code = lval.generate_code(ctx);
                let e_code = expr.generate_code(ctx);
                [
                    l_code.clone(),
                    e_code.clone(),
                    vec![IR::AccessArray(
                        ctx.get_new_name(),
                        get_last_var_name(&l_code),
                        get_last_var_name(&e_code),
                    )],
                ]
                .concat()
            }
        }
    }
}
