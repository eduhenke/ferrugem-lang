use std::{
    borrow::{Borrow, BorrowMut},
    cell::{Ref, RefCell},
    collections::HashMap,
    ops::Deref,
    rc::Rc,
};

use id_arena::{Arena, Id};

use crate::ast::{Expression, LValue, Program, Statement, Type};

#[derive(Debug, Clone)]
pub enum SemanticError {
    AlreadyDeclared,
    MustBePreviouslyDeclared,
}

pub type ScopeId<'a> = Id<Scope<'a>>;

// #[derive(Debug, Clone)]
// pub struct Rc<RefCell<T>(R>c<RefCell<T>>);
// impl<T>RefCell Rc<<T> >{
//     pub fn new(data: T) -> Self {
//         Self(Rc::new(RefCell::new(data)))
//     }
// }

// impl<T> Deref forRefCell Rc<<T> >{
//     type Target = RefCell<T>;
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }

#[derive(Debug, Clone)]
pub struct Scope<'a> {
    pub symbol_table: HashMap<&'a str, Type<'a>>,
    pub child_scopes: Vec<Rc<RefCell<Scope<'a>>>>,
    pub parent_scope: Option<Rc<RefCell<Scope<'a>>>>,
}

impl<'a> Scope<'a> {
    fn new(parent_scope: Rc<RefCell<Scope<'a>>>) -> Scope<'a> {
        Scope {
            symbol_table: HashMap::new(),
            child_scopes: Vec::new(),
            parent_scope: Some(parent_scope),
        }
    }
    pub fn new_global() -> Rc<RefCell<Scope<'a>>> {
        Rc::new(RefCell::new(Scope {
            symbol_table: HashMap::new(),
            child_scopes: Vec::new(),
            parent_scope: None,
        }))
    }

    fn create_child_scope(&mut self) -> Rc<RefCell<Scope<'a>>> {
        let child = Rc::new(RefCell::new(Scope::new(Rc::new(RefCell::new(
            self.clone(),
        )))));
        self.child_scopes.push(Rc::clone(&child));
        child
    }

    fn add_symbol(&mut self, name: &'a str, ty: &'a Type<'a>) -> Result<(), SemanticError> {
        // let mut me = self.clone();
        let was_empty = self.symbol_table.insert(name, ty.to_owned()).is_none();
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
}

pub(crate) trait Lowerable {
    fn lower<'a>(
        &'a self,
        scope: Rc<RefCell<Scope<'a>>>,
    ) -> Result<Rc<RefCell<Scope<'a>>>, SemanticError>;
}

impl<'a> Lowerable for Program<'a> {
    fn lower<'b>(
        &'b self,
        scope: Rc<RefCell<Scope<'b>>>,
    ) -> Result<Rc<RefCell<Scope<'b>>>, SemanticError> {
        match self {
            Program::FuncList(funcs) => todo!(),
            Program::Statement(stmt) => stmt.lower(scope),
        }
    }
}

impl<'a> Lowerable for Expression<'a> {
    fn lower<'b>(
        &'b self,
        scope: Rc<RefCell<Scope<'b>>>,
    ) -> Result<Rc<RefCell<Scope<'b>>>, SemanticError> {
        match self {
            Expression::Binary(_, _, _) => todo!(),
            Expression::Unary(_, _) => todo!(),
            Expression::IntLiteral(_) => todo!(),
            Expression::FloatLiteral(_) => todo!(),
            Expression::StringLiteral(_) => todo!(),
            Expression::Null => todo!(),
            Expression::LValue(_) => todo!(),
            Expression::FunctionCall(_, _) => todo!(),
            Expression::Alloc(_) => todo!(),
        }
    }
}

impl<'a> Lowerable for Statement<'a> {
    fn lower<'b>(
        &'b self,
        scope: Rc<RefCell<Scope<'b>>>,
    ) -> Result<Rc<RefCell<Scope<'b>>>, SemanticError> {
        match self {
            // Statement::VariableDeclaration(ty, name) => todo!(),
            Statement::VariableDeclaration(ty, name) => (*scope.clone())
                .borrow_mut()
                .add_symbol(name, ty)
                .map(move |_| scope),
            Statement::StatementList(stmts) => {
                let mut child = (*scope).borrow_mut().create_child_scope();

                // let mut asd: Scope = todo!();
                // let child = asd.create_child_scope();

                for stmt in stmts {
                    child = stmt.lower(child)?;
                }

                // scope.child_scopes.insert("001", child.borrow());
                Ok(scope)
            }
            Statement::If {
                condition,
                true_path,
                false_path,
            } => todo!(),
            Statement::Read(lval) => lval.lower(scope),
            Statement::Print(stmt) => stmt.lower(scope),
            Statement::Return => todo!(),
            Statement::Break => todo!(),
            Statement::Assignment(lval, _expr) => lval.lower(scope),
            Statement::For {
                initial_assignment,
                condition,
                post_assignment,
                body,
            } => todo!(),
        }
    }
}

impl<'a> Lowerable for LValue<'a> {
    fn lower<'b>(
        &'b self,
        scope: Rc<RefCell<Scope<'b>>>,
    ) -> Result<Rc<RefCell<Scope<'b>>>, SemanticError> {
        match self {
            &LValue::NameReference(name) => {
                (*scope.clone()).borrow().lookup_symbol(name).map(|_| scope)
            }
            &LValue::ArrayAccess(_, _) => todo!(),
        }
    }
}
