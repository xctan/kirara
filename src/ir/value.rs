use std::{rc::Weak, collections::HashSet};

use id_arena::Id;

use crate::ctype::{Type, BinaryOpType as BinaryOp};

pub type ValueId = Id<Value>;

pub trait ValueTrait {
    fn name(&self) -> String { String::new() }
    fn set_name(&mut self, _name: String) {}
    fn ty(&self) -> Weak<Type> { Type::void_type() }
    fn set_ty(&mut self, _ty: Weak<Type>) {}
}

// todo: use derive macro
macro_rules! impl_value_trait {
    ($ty:ident) => {
        impl ValueTrait for $ty {
            fn name(&self) -> String {
                self.name.clone()
            }
            fn set_name(&mut self, name: String) {
                self.name = name;
            }
            fn ty(&self) -> Weak<Type> {
                self.ty.clone()
            }
            fn set_ty(&mut self, ty: Weak<Type>) {
                self.ty = ty;
            }
        }
    };
    ($en:ident { $($var:ident),+ $(,)? }) => {
        impl ValueTrait for $en {
            fn name(&self) -> String {
                match self {
                    $( $en::$var(v) => v.name(), )+
                }
            }
            fn set_name(&mut self, name: String) {
                match self {
                    $( $en::$var(v) => v.set_name(name), )+
                }
            }
            fn ty(&self) -> Weak<Type> {
                match self {
                    $( $en::$var(v) => v.ty(), )+
                }
            }
            fn set_ty(&mut self, ty: Weak<Type>) {
                match self {
                    $( $en::$var(v) => v.set_ty(ty), )+
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub value: ValueType,

    pub prev: Option<ValueId>,
    pub next: Option<ValueId>,

    pub(in crate::ir) used_by: HashSet<ValueId>,
}

impl ValueTrait for Value {
    fn name(&self) -> String { self.value.name() }
    fn set_name(&mut self, name: String) { self.value.set_name(name); }
    fn ty(&self) -> Weak<Type> { self.value.ty() }
    fn set_ty(&mut self, ty: Weak<Type>) { self.value.set_ty(ty); }
}

impl Value {
    pub fn new(v: ValueType) -> Self {
        Self {
            value: v,
            prev: None,
            next: None,
            used_by: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueType {
    Global(GlobalValue),
    Instruction(InstructionValue),
    Constant(ConstantValue),
}

impl_value_trait!{ValueType{
    Global,
    Instruction,
    Constant,
}}

#[derive(Debug, Clone)]
pub struct GlobalValue {
    pub name: String,
    pub ty: Weak<Type>,
}

impl_value_trait!(GlobalValue);

#[derive(Debug, Clone, Copy)]
pub enum ConstantValue {
    I32(i32),
}

impl ValueTrait for ConstantValue {
    fn ty(&self) -> Weak<Type> {
        match self {
            ConstantValue::I32(_) => Type::i32_type(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum InstructionValue {
    BinaryInst(BinaryInst),
    LoadInst(LoadInst),
    StoreInst(StoreInst),
    AllocaInst(AllocaInst),
    ReturnInst(ReturnInst),
}

impl_value_trait!{InstructionValue{
    BinaryInst,
    LoadInst,
    StoreInst,
    AllocaInst,
    ReturnInst,
}}

#[derive(Debug, Clone)]
pub struct BinaryInst {
    pub lhs: ValueId,
    pub rhs: ValueId,
    pub op: BinaryOp,
    pub name: String,
    pub ty: Weak<Type>,
}

impl_value_trait!(BinaryInst);

#[derive(Debug, Clone)]
pub struct LoadInst {
    pub ptr: ValueId,
    pub name: String,
    pub ty: Weak<Type>,
}

impl_value_trait!(LoadInst);

#[derive(Debug, Clone)]
pub struct StoreInst {
    pub ptr: ValueId,
    pub value: ValueId,
}

impl ValueTrait for StoreInst {}

#[derive(Debug, Clone)]
pub struct AllocaInst {
    pub ty: Weak<Type>,
    pub name: String,
}

impl_value_trait!(AllocaInst);

#[derive(Debug, Clone)]
pub struct ReturnInst {
    pub value: Option<ValueId>,
}

impl ValueTrait for ReturnInst {}