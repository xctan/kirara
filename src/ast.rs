use std::{cell::RefCell, rc::{Rc, Weak}, collections::HashMap};

use id_arena::{Arena, Id};

pub type ObjectId = Id<AstObject>;

use crate::{
    token::TokenRange,
    ctype::{BinaryOpType, Type}, ir::value::ValueId,
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
    Variable(ObjectId),
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
    pub ty: Weak<Type>,
    pub token: TokenRange,
    pub is_local: bool,

    pub data: AstObjectType,

    pub ir_value: Option<ValueId>,
}

impl AstObject {
    fn new(name: String, ty: Weak<Type>, is_local: bool, data: AstObjectType) -> Self {
        Self {
            name,
            ty,
            token: 0..0,
            is_local,
            data,
            ir_value: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstFuncData {
    pub params: Vec<ObjectId>,
    pub locals: Vec<ObjectId>,
    pub body: Rc<RefCell<AstNode>>,
}

#[derive(Debug, Clone)]
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
    pub objects: Arena<AstObject>,
    pub globals: Vec<ObjectId>,
    pub locals: Vec<ObjectId>,
    pub scopes: Vec<Scope>,
}

impl AstContext {
    pub fn new() -> Self {
        Self {
            objects: Arena::new(),
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

    pub fn find_var(&self, name: &str) -> Option<ScopeVar> {
        for scope in self.scopes.iter().rev() {
            if let Some(ScopeVar::Var(id)) = scope.vars.get(name) {
                return Some(ScopeVar::Var(*id));
            }
        }
        None
    }

    pub fn new_local_var(&mut self, name: &str, ty: Weak<Type>) -> ObjectId {
        let name = name.to_string();
        let obj = AstObject::new(
            name.clone(),
            ty,
            true,
            AstObjectType::Var,
        );
        let id = self.objects.alloc(obj);
        self.locals.push(id);
        self.scopes.last_mut().unwrap().vars.insert(name, ScopeVar::Var(id));
        id
    }

    pub fn new_global_var(&mut self, name: &str, ty: Weak<Type>) -> ObjectId {
        let name = name.to_string();
        let obj = AstObject::new(
            name.clone(),
            ty,
            false,
            AstObjectType::Var,
        );
        let id = self.objects.alloc(obj);
        self.globals.push(id);
        self.scopes.first_mut().unwrap().vars.insert(name, ScopeVar::Var(id));
        id
    }

    pub fn get_object_mut(&mut self, id: ObjectId) -> Option<&mut AstObject> {
        self.objects.get_mut(id)
    }

    pub fn get_object(&self, id: ObjectId) -> Option<&AstObject> {
        self.objects.get(id)
    }
}