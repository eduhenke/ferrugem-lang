use std::{
    borrow::{Borrow, BorrowMut},
    cell::{Ref, RefCell},
    collections::HashMap,
    ops::{Deref, Not},
    rc::Rc,
};

use crate::ast::{
    BinOp, Expression, FunctionDefinition, LValue, Program, Statement, Type, UnaryOp,
};

// // Um código de representação intermediaŕio de 3 endereços
// enum IR {
//     BinOp(String, BinOp, String),
//     UnaryOp(UnaryOp, String),
//     Assignment(String, Box<IR>),
// }

#[derive(Debug, Clone)]
pub enum SemanticError {
    AlreadyDeclared,
    MustBePreviouslyDeclared,
    MustBeAFunction,
    TypeOfParametersIncorrect,
    BreakMustBeInsideOfFor,
    ReturnMustBeInsideOfFunction,
    TypesMustBeTheSame,
    OperationOnlyForNumeric,
}

#[derive(Debug, Clone)]
pub struct Link<T>(Rc<RefCell<T>>);

impl<T> Link<T> {
    pub fn new(data: T) -> Self {
        Self(Rc::new(RefCell::new(data)))
    }
}

impl<T> Deref for Link<T> {
    type Target = RefCell<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Function,
    For,
    Block,
    Global,
}

#[derive(Debug, Clone)]
pub struct Scope<'a> {
    pub symbol_table: HashMap<&'a str, Type<'a>>,
    pub child_scopes: Vec<Link<Scope<'a>>>,
    pub parent_scope: Option<Link<Scope<'a>>>,
    pub kind: ScopeKind,
}

impl<'a> Scope<'a> {
    fn new(parent_scope: Link<Scope<'a>>, kind: ScopeKind) -> Scope<'a> {
        Scope {
            symbol_table: HashMap::new(),
            child_scopes: Vec::new(),
            parent_scope: Some(parent_scope),
            kind,
        }
    }
    pub fn new_global() -> Link<Scope<'a>> {
        Link::new(Scope {
            symbol_table: HashMap::new(),
            child_scopes: Vec::new(),
            parent_scope: None,
            kind: ScopeKind::Global,
        })
    }

    fn create_child_scope(&mut self, kind: ScopeKind) -> Link<Scope<'a>> {
        let child = Link::new(Scope::new(Link::new(self.clone()), kind));
        self.child_scopes.push(Link::clone(&child));
        child
    }

    fn add_symbol(&mut self, name: &'a str, ty: Type<'a>) -> Result<(), SemanticError> {
        // let mut me = self.clone();
        let was_empty = self.symbol_table.insert(name, ty).is_none();
        if !was_empty {
            return Err(SemanticError::AlreadyDeclared);
        }
        Ok(())
    }

    fn lookup_symbol(&self, name: &str) -> Result<Type<'a>, SemanticError> {
        if let Some(ty) = self.symbol_table.get(name) {
            Ok(ty.clone())
        } else {
            match &self.parent_scope {
                None => Err(SemanticError::MustBePreviouslyDeclared),
                Some(scope) => (*scope.clone()).borrow().lookup_symbol(name),
            }
        }
    }

    fn is_inside(&self, kind: ScopeKind) -> bool {
        if self.kind == kind {
            true
        } else {
            match &self.parent_scope {
                None => false,
                Some(scope) => (*scope.clone()).borrow().is_inside(kind),
            }
        }
    }
}

pub(crate) trait TypeCheckable {
    fn type_check<'a>(
        &'a self,
        scope: Link<Scope<'a>>,
    ) -> Result<(Link<Scope<'a>>, Type<'a>), SemanticError>;
}

impl<'a> TypeCheckable for Program<'a> {
    fn type_check<'b>(
        &'b self,
        scope: Link<Scope<'b>>,
    ) -> Result<(Link<Scope<'b>>, Type<'b>), SemanticError> {
        match self {
            Program::FuncList(funcs) => funcs
                .into_iter()
                .try_fold((scope, Type::Void), |(scope, _), f| f.type_check(scope)),
            Program::Statement(stmt) => stmt.type_check(scope),
        }
    }
}

impl<'a> TypeCheckable for FunctionDefinition<'a> {
    fn type_check<'b>(
        &'b self,
        scope: Link<Scope<'b>>,
    ) -> Result<(Link<Scope<'b>>, Type<'b>), SemanticError> {
        let f_scope = scope
            .deref()
            .borrow_mut()
            .create_child_scope(ScopeKind::Function);
        // add function parameters into child scope
        self.clone().parameters.into_iter().try_fold(
            f_scope.clone(),
            |scope, (p_ty, p_name)| {
                scope
                    .clone()
                    .deref()
                    .borrow_mut()
                    .add_symbol(p_name, p_ty)
                    .map(|_| scope)
            },
        )?;

        // lower all statements of the function
        self.body
            .iter()
            .try_fold((f_scope.clone(), Type::Void), |(scope, _), stmt| {
                stmt.type_check(scope)
            })?;

        // add function to current scope
        let f_type = Type::Function {
            params_types: self
                .clone()
                .parameters
                .into_iter()
                .map(|(ty, _)| ty)
                .collect(),
            return_type: Box::new(
                self.body
                    .last()
                    .and_then(|stmt| stmt.type_check(f_scope).map(|(sc, ty)| ty).ok())
                    .unwrap_or(Type::Void),
            ),
        };
        scope
            .clone()
            .deref()
            .borrow_mut()
            .add_symbol(self.name, f_type.clone())
            .map(|_| (scope, f_type))
    }
}

impl<'a> TypeCheckable for Expression<'a> {
    fn type_check<'b>(
        &'b self,
        scope: Link<Scope<'b>>,
    ) -> Result<(Link<Scope<'b>>, Type<'b>), SemanticError> {
        match self {
            Expression::Binary(rhs, op, lhs) => {
                rhs.type_check(scope).and_then(|(scope, rhs_ty)| {
                    lhs.type_check(scope.clone()).and_then(|(_, lhs_ty)| {
                        println!(
                            "type checking binary, rhs({:?}): {:?}; lhs({:?}): {:?}",
                            rhs_ty, rhs, lhs_ty, lhs
                        );
                        if !rhs_ty.can_be_casted_to(&lhs_ty) {
                            Err(SemanticError::TypesMustBeTheSame)
                        } else {
                            Ok((scope, rhs_ty))
                        }
                    })
                })
            }
            Expression::Unary(op, expr) => expr.type_check(scope).and_then(|(scope, ty)| {
                if !ty.is_numeric() {
                    Err(SemanticError::OperationOnlyForNumeric)
                } else {
                    Ok((scope, ty))
                }
            }),
            Expression::IntLiteral(_) => Ok((scope, Type::Int)),
            Expression::FloatLiteral(_) => Ok((scope, Type::Float)),
            Expression::StringLiteral(_) => Ok((scope, Type::String)),
            Expression::Null => Ok((scope, Type::Null)),
            Expression::LValue(lval) => lval.type_check(scope),
            Expression::FunctionCall(f, params) => {
                let f_type = scope.deref().borrow().lookup_symbol(f)?;
                match f_type {
                    Type::Function {
                        params_types,
                        return_type,
                    } => {
                        let supplied_params_types = params
                            .into_iter()
                            .map(|p| scope.deref().borrow().lookup_symbol(p))
                            .collect::<Result<Vec<Type>, _>>()?;
                        let params_correct = supplied_params_types
                            .iter()
                            .zip(params_types)
                            .all(|(a, b)| *a == b);
                        if !params_correct {
                            Err(SemanticError::TypeOfParametersIncorrect)
                        } else {
                            Ok((scope, *return_type))
                        }
                    }
                    _ => Err(SemanticError::MustBeAFunction),
                }
            }
            Expression::Alloc(_) => todo!(),
        }
    }
}

impl<'a> TypeCheckable for Statement<'a> {
    fn type_check<'b>(
        &'b self,
        scope: Link<Scope<'b>>,
    ) -> Result<(Link<Scope<'b>>, Type<'b>), SemanticError> {
        match self {
            Statement::VariableDeclaration(ty, name) => scope
                .clone()
                .deref()
                .borrow_mut()
                .add_symbol(name, ty.clone())
                .map(|_| (scope, Type::Void)),
            Statement::StatementList(stmts) => {
                let mut child = scope
                    .deref()
                    .borrow_mut()
                    .create_child_scope(ScopeKind::Block);
                let mut ty = Type::Void;
                for stmt in stmts {
                    let rst = stmt.type_check(child)?;
                    child = rst.0;
                    ty = rst.1;
                }

                Ok((scope, ty))
            }
            Statement::If {
                condition,
                true_path,
                // TODO:
                false_path,
            } => {
                condition
                    .type_check(scope.clone())
                    .and(true_path.type_check(scope.clone()))
                // false_path.map(|stmt| {
                //     stmt.lower(scope);
                // });
            }
            Statement::Read(lval) => lval.type_check(scope),
            Statement::Print(stmt) => stmt.type_check(scope),
            Statement::Return => match scope
                .clone()
                .deref()
                .borrow()
                .is_inside(ScopeKind::Function)
            {
                true => Ok((scope, Type::Void)),
                false => Err(SemanticError::ReturnMustBeInsideOfFunction),
            },
            Statement::Break => match scope.clone().deref().borrow().is_inside(ScopeKind::For) {
                true => Ok((scope, Type::Void)),
                false => Err(SemanticError::BreakMustBeInsideOfFor),
            },
            // TODO:
            Statement::Assignment(lval, expr) => {
                lval.type_check(scope.clone()).and(expr.type_check(scope))
            }
            Statement::For {
                initial_assignment,
                condition,
                post_assignment,
                body,
            } => initial_assignment
                .type_check(scope.clone())
                .and(condition.type_check(scope.clone()))
                .and(post_assignment.type_check(scope.clone()))
                .and_then(|_| {
                    let for_scope = scope
                        .deref()
                        .borrow_mut()
                        .create_child_scope(ScopeKind::For);
                    body.type_check(for_scope).map(|_| (scope, Type::Void))
                }),
        }
    }
}

impl<'a> TypeCheckable for LValue<'a> {
    fn type_check<'b>(
        &'b self,
        scope: Link<Scope<'b>>,
    ) -> Result<(Link<Scope<'b>>, Type<'b>), SemanticError> {
        match self {
            &LValue::NameReference(name) => (*scope.clone())
                .borrow()
                .lookup_symbol(name)
                .map(|ty| (scope, ty)),
            &LValue::ArrayAccess(_, _) => todo!(),
        }
    }
}
