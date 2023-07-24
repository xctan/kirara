use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{alloc::{Arena, Id}, ctype::TypeKind};

pub(in crate::ast) mod context;
pub mod parse;
pub mod transform;

pub type ObjectId = Id<AstObject>;
pub use parse::parse;
pub(in crate::ast) use context::*;

use crate::{
    ctype::{BinaryOpType, Type, UnaryOpType},
    ir::value::ValueId,
    token::TokenRange,
};

use self::transform::const_fold::ast_const_fold;

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub expr: Rc<RefCell<AstNode>>,
    pub op: UnaryOpType,
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub lhs: Rc<RefCell<AstNode>>,
    pub rhs: Rc<RefCell<AstNode>>,
    pub op: BinaryOpType,
}

#[derive(Debug, Clone)]
pub struct Convert {
    pub from: Rc<RefCell<AstNode>>,
    pub to: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct FunCall {
    pub func: Rc<RefCell<AstNode>>,
    pub args: Vec<Rc<RefCell<AstNode>>>,
}

#[derive(Debug, Clone)]
pub enum AstNodeType {
    Unit,
    I1Number(bool),
    I32Number(i32),
    I64Number(i64),
    Variable(ObjectId),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    Convert(Convert),
    FunCall(FunCall),
    Block(Vec<Rc<RefCell<AstNode>>>),
    ExprStmt(Rc<RefCell<AstNode>>),
    Return(Rc<RefCell<AstNode>>),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    LabelStmt(LabelStmt),
    GotoStmt(GotoStmt),
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Rc<RefCell<AstNode>>,
    pub then: Rc<RefCell<AstNode>>,
    pub els: Rc<RefCell<AstNode>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub cond: Rc<RefCell<AstNode>>,
    pub body: Rc<RefCell<AstNode>>,
}

#[derive(Debug, Clone)]
pub struct LabelStmt {
    pub label: String,
    pub body: Rc<RefCell<AstNode>>,
}

#[derive(Debug, Clone)]
pub struct GotoStmt {
    pub label: String,
}

#[derive(Debug, Clone)]
pub struct AstNode {
    pub node: AstNodeType,
    pub token: TokenRange,
    pub ty: Option<Rc<Type>>,
}

impl AstNode {
    pub fn new(node: AstNodeType, token: TokenRange) -> Self {
        Self {
            node,
            token,
            ty: None,
        }
    }

    // pub fn is_immediate(&self) -> bool {
    //     match self.node {
    //         AstNodeType::I1Number(_) | AstNodeType::I32Number(_) | AstNodeType::I64Number(_) => true,
    //         _ => false,
    //     }
    // }

    pub fn i32_number(val: i32, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::I32Number(val), token);
        Rc::new(RefCell::new(node))
    }

    pub fn i64_number(val: i64, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::I64Number(val), token);
        Rc::new(RefCell::new(node))
    }

    pub fn convert(from: Rc<RefCell<Self>>, to: Rc<Type>, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::Convert(Convert { from, to }), token);
        Rc::new(RefCell::new(node))
    }

    pub fn unary(expr: Rc<RefCell<AstNode>>, op: UnaryOpType, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::UnaryOp(UnaryOp { expr, op }), token);
        Rc::new(RefCell::new(node))
    }

    pub fn binary(
        lhs: Rc<RefCell<AstNode>>,
        rhs: Rc<RefCell<AstNode>>,
        op: BinaryOpType,
        token: TokenRange,
    ) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }), token);
        Rc::new(RefCell::new(node))
    }

    pub fn call(func: Rc<RefCell<Self>>, args: Vec<Rc<RefCell<Self>>>, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::FunCall(FunCall { func, args }), token);
        Rc::new(RefCell::new(node))
    }

    pub fn expr_stmt(expr: Rc<RefCell<Self>>, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::ExprStmt(expr), token);
        Rc::new(RefCell::new(node))
    }

    pub fn unit(token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::Unit, token);
        Rc::new(RefCell::new(node))
    }

    pub fn is_unit(&self) -> bool {
        matches!(self.node, AstNodeType::Unit)
    }

    pub fn ret(expr: Rc<RefCell<Self>>, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::Return(expr), token);
        Rc::new(RefCell::new(node))
    }

    pub fn variable(id: ObjectId, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::Variable(id), token);
        Rc::new(RefCell::new(node))
    }

    pub fn block(stmts: Vec<Rc<RefCell<Self>>>, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::Block(stmts), token);
        Rc::new(RefCell::new(node))
    }

    pub fn r#if(
        cond: Rc<RefCell<Self>>,
        then: Rc<RefCell<Self>>,
        els: Rc<RefCell<Self>>,
        token: TokenRange,
    ) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::IfStmt(IfStmt { cond, then, els }), token);
        Rc::new(RefCell::new(node))
    }

    pub fn r#while(cond: Rc<RefCell<Self>>, body: Rc<RefCell<Self>>, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::WhileStmt(WhileStmt { cond, body }), token);
        Rc::new(RefCell::new(node))
    }

    pub fn label(label: String, body: Rc<RefCell<Self>>, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::LabelStmt(LabelStmt { label, body }), token);
        Rc::new(RefCell::new(node))
    }

    pub fn goto(label: String, token: TokenRange) -> Rc<RefCell<Self>> {
        let node = Self::new(AstNodeType::GotoStmt(GotoStmt { label }), token);
        Rc::new(RefCell::new(node))
    }
}

pub trait AstNodeRewrite {
    fn rewrite(&self, new: Rc<RefCell<AstNode>>);
}

impl AstNodeRewrite for Rc<RefCell<AstNode>> {
    fn rewrite(&self, new: Rc<RefCell<AstNode>>) {
        self.borrow_mut().node = new.borrow().node.clone();
        self.borrow_mut().ty = new.borrow().ty.clone();
    }
}

#[derive(Debug)]
pub struct AstObject {
    pub name: String,
    pub ty: Rc<Type>,
    pub token: TokenRange,
    pub is_local: bool,
    /// declaration or definition
    pub is_decl: bool,

    pub data: AstObjectType,

    pub ir_value: Option<ValueId>,
}

impl AstObject {
    fn new(name: String, ty: Rc<Type>, is_local: bool, data: AstObjectType) -> Self {
        Self {
            name,
            ty,
            token: 0..0,
            is_local,
            is_decl: false,
            data,
            ir_value: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstFuncData {
    pub locals: Vec<ObjectId>,
    pub params: Vec<ObjectId>,
    pub body: Rc<RefCell<AstNode>>,
    pub ret_var: Option<ObjectId>,
    pub func_ty: Rc<Type>,
    // is_definition
    // is_static
    // is_inline
}

#[derive(Debug, Clone)]
pub enum AstObjectType {
    Func(AstFuncData),
    Var(Initializer),
}

#[derive(Debug, Clone)]
pub enum InitData {
    Expr(Rc<RefCell<AstNode>>),
    ScalarI32(i32),
    // ScalarF32(f32),
    Aggregate(Vec<Initializer>),
    ZeroInit,
}

impl Default for InitData {
    fn default() -> Self {
        Self::ZeroInit
    }
}

impl InitData {
    pub fn eval(&mut self) {
        let new_self = match self {
            Self::Expr(expr) => {
                ast_const_fold(expr.clone());
                match expr.borrow().node.clone() {
                    AstNodeType::I1Number(num) => {
                        Self::ScalarI32(num as i32)
                    },
                    AstNodeType::I32Number(num) => {
                        Self::ScalarI32(num)
                    },
                    _ => return,
                }
            },
            Self::Aggregate(data) => {
                for init in data.iter_mut() {
                    init.data.eval();
                }
                return;
            },
            _ => return,
        };
        *self = new_self;
    }

    pub fn is_const(&self) -> bool {
        match self {
            Self::Expr(_) => false,
            Self::ScalarI32(_) => true,
            Self::Aggregate(data) => data.iter().all(|init| init.data.is_const()),
            Self::ZeroInit => true,
        }
    }

    fn is_all_zeroinit(&self) -> bool {
        match self {
            Self::Expr(_) => false,
            Self::ScalarI32(_) => false,
            Self::Aggregate(data) => data.iter().all(|init| init.data.is_all_zeroinit()),
            Self::ZeroInit => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Initializer {
    pub ty: Rc<Type>,
    pub data: InitData,
}

impl Initializer {
    pub fn new(ty: Rc<Type>) -> Self {
        let data = match ty.kind.clone() {
            // scalar
            TypeKind::I1 |
            TypeKind::I32 |
            TypeKind::I64 |
            TypeKind::Ptr(_) => InitData::ZeroInit,
            // aggregate
            TypeKind::Array(arr) => {
                let mut data = Vec::new();
                for _ in 0..arr.len {
                    data.push(Initializer::new(arr.base_type.clone()));
                }
                InitData::Aggregate(data)
            }
            _ => unimplemented!("unknown initializer type: {:?}", ty.kind),
        };
        Self { ty, data }
    }

    pub fn eval(&mut self) {
        self.data.eval();
        if self.data.is_all_zeroinit() {
            self.data = InitData::ZeroInit;
        } else {
            self.fill_zero();
        }
    }

    fn fill_zero(&mut self) {
        match self.data {
            InitData::Aggregate(ref mut data) => {
                for init in data.iter_mut() {
                    init.fill_zero();
                }
            },
            InitData::ZeroInit => {
                let zero = match self.ty.kind {
                    TypeKind::I1 => InitData::ScalarI32(0),
                    TypeKind::I32 => InitData::ScalarI32(0),
                    _ => return,
                };
                self.data = zero;
            },
            _ => {}
        }
    }

    // pub fn as_array(&self) -> &[Initializer] {
    //     match &self.data {
    //         InitData::Aggregate(data) => data,
    //         _ => unreachable!(),
    //     }
    // }

    pub fn as_array_mut(&mut self) -> &mut [Initializer] {
        match &mut self.data {
            InitData::Aggregate(data) => data,
            _ => unreachable!(),
        }
    }
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
    pub scopes: Vec<Scope>,

    pub locals: Vec<ObjectId>,
    pub labels: HashSet<String>,
    pub gotos: Vec<String>,
    pub loop_stack: Vec<(String, String)>,
    counter: usize,
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
            labels: HashSet::new(),
            gotos: Vec::new(),
            loop_stack: Vec::new(),
            counter: 0,
        }
    }

    pub fn reset_func_data(&mut self) {
        self.locals.clear();
        self.labels.clear();
        self.gotos.clear();
        self.loop_stack.clear();
        self.counter = 0;
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope {
            vars: HashMap::new(),
        });
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn gen_unique_name(&mut self, prefix: &str) -> String {
        let name = format!("{}{}", prefix, self.counter);
        self.counter += 1;
        name
    }

    pub fn register_label(&mut self, name: String) -> bool {
        self.labels.insert(name)
    }

    pub fn register_goto(&mut self, name: String) {
        self.gotos.push(name);
    }

    pub fn push_loop(&mut self, cont: String, brk: String) {
        self.loop_stack.push((cont, brk));
    }

    pub fn get_loop_labels(&self) -> Option<&(String, String)> {
        self.loop_stack.last()
    }

    pub fn pop_loop(&mut self) -> Option<(String, String)> {
        self.loop_stack.pop()
    }

    pub fn validate_gotos(&self) -> bool {
        self.gotos.iter()
            .all(|name| self.labels.contains(name))
    }

    pub fn find_var(&self, name: &str) -> Option<ScopeVar> {
        for scope in self.scopes.iter().rev() {
            if let Some(ScopeVar::Var(id)) = scope.vars.get(name) {
                return Some(ScopeVar::Var(*id));
            }
        }
        None
    }

    pub fn find_func(&self, name: &str) -> Option<ObjectId> {
        return if let Some(ScopeVar::Var(id)) = self.scopes.first().unwrap().vars.get(name) {
            let obj = self.objects.get(*id).unwrap();
            match obj.data {
                AstObjectType::Func(_) => Some(*id),
                AstObjectType::Var(_) => {
                    // declaration only
                    if obj.ty.is_function() {
                        Some(*id)
                    } else {
                        None
                    }
                }
            }
        } else {
            None
        }
    }

    pub fn new_local_var(&mut self, name: &str, ty: Rc<Type>) -> ObjectId {
        let name = name.to_string();
        let obj = AstObject::new(
            name.clone(),
            ty.clone(),
            true,
            AstObjectType::Var(Initializer { ty: ty.clone(), data: InitData::ZeroInit }),
        );
        let id = self.objects.alloc(obj);
        self.locals.push(id);
        self.scopes
            .last_mut()
            .unwrap()
            .vars
            .insert(name, ScopeVar::Var(id));
        id
    }

    pub fn new_global_var(&mut self, name: &str, ty: Rc<Type>) -> ObjectId {
        let name = name.to_string();
        let obj = AstObject::new(
            name.clone(),
            ty.clone(),
            false,
            AstObjectType::Var(Initializer { ty: ty.clone(), data: InitData::ZeroInit })
        );
        let id = self.objects.alloc(obj);
        self.globals.push(id);
        self.scopes
            .first_mut()
            .unwrap()
            .vars
            .insert(name, ScopeVar::Var(id));
        id
    }

    pub fn get_object_mut(&mut self, id: ObjectId) -> Option<&mut AstObject> {
        self.objects.get_mut(id)
    }

    pub fn get_object(&self, id: ObjectId) -> Option<&AstObject> {
        self.objects.get(id)
    }
}
