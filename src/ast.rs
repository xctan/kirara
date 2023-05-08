use std::{cell::RefCell, rc::Rc, collections::HashMap};

use id_arena::{Arena, Id};

pub type ObjectId = Id<AstObject>;

use crate::{
    token::{TokenRange, TokenSpan, Token},
    ctype::{BinaryOpType, Type},
};

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub lhs: Rc<RefCell<AstNode>>,
    pub rhs: Rc<RefCell<AstNode>>,
    pub op: BinaryOpType,
}

#[derive(Debug, Clone)]
pub enum AstNodeType {
    Unit,
    I64Number(i64),
    BinaryOp(BinaryOp),
    Block(Vec<Rc<RefCell<AstNode>>>),
    ExprStmt(Rc<RefCell<AstNode>>),
    Return(Rc<RefCell<AstNode>>),
}

#[derive(Debug)]
pub struct AstNode {
    pub node: AstNodeType,
    pub token: TokenRange,
    // ty: Type,
}

#[derive(Debug)]
pub struct AstObject {
    pub name: String,
    pub ty: Type,
    pub token: TokenRange,
    pub is_local: bool,

    pub data: AstObjectType,
}

#[derive(Debug)]
pub struct AstFuncData {
    pub params: Vec<ObjectId>,
    pub locals: Vec<ObjectId>,
    pub body: Rc<RefCell<AstNode>>,
}

#[derive(Debug)]
pub enum AstObjectType {
    Func(AstFuncData),
    Var,
}

#[derive(Debug)]
pub enum ScopeVar {
    Var(ObjectId),
    // EnumVal(i32),
    // TypeDef(Type),
}

#[derive(Debug)]
pub struct Scope {
    pub vars: HashMap<String, ScopeVar>,
    // pub tags: HashMap<String, Type>,
}

#[derive(Debug)]
pub struct AstContext {
    pub objects: RefCell<Arena<AstObject>>,
    pub globals: Vec<ObjectId>,
    pub locals: Vec<ObjectId>,
    pub scopes: Vec<Scope>,
}

impl AstContext {
    pub fn new() -> Self {
        Self {
            objects: RefCell::new(Arena::new()),
            globals: Vec::new(),
            locals: Vec::new(),
            scopes: vec![Scope {
                vars: HashMap::new(),
            }],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope {
            vars: HashMap::new(),
        });
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn find_var(&self, name: &str) -> Option<ObjectId> {
        for scope in self.scopes.iter().rev() {
            if let Some(ScopeVar::Var(id)) = scope.vars.get(name) {
                return Some(*id);
            }
        }
        None
    }

    pub fn new_local_var(&mut self, token: TokenSpan<'_>, ty: Type) -> ObjectId {
        let name = token.as_str().to_string();
        if self.find_var(name.as_str()).is_some() {
            panic!("redefinition of variable: {}", name);
        }

        let obj = AstObject {
            name: name.clone(),
            ty,
            token: token.as_range(),
            is_local: true,
            data: AstObjectType::Var,
        };
        let id = self.objects.borrow_mut().alloc(obj);
        self.locals.push(id);
        self.scopes.last_mut().unwrap().vars.insert(name, ScopeVar::Var(id));
        id
    }
}