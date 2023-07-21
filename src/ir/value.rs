use std::rc::Rc;
use std::{rc::Weak, collections::HashSet};

use crate::alloc::Id;

use crate::ctype::{Type, BinaryOpType as BinaryOp};
use crate::for_each_bb_and_inst;

use super::structure::{BlockId, TransUnit};

pub type ValueId = Id<Value>;

pub trait ValueTrait {
    fn name(&self) -> String { String::new() }
    fn set_name(&mut self, _name: String) {}
    fn ty(&self) -> Rc<Type> { Type::void_type() }
    fn set_ty(&mut self, _ty: Rc<Type>) {}
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
            fn ty(&self) -> Rc<Type> {
                self.ty.clone()
            }
            fn set_ty(&mut self, ty: Rc<Type>) {
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
            fn ty(&self) -> Rc<Type> {
                match self {
                    $( $en::$var(v) => v.ty(), )+
                }
            }
            fn set_ty(&mut self, ty: Rc<Type>) {
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

    pub used_by: HashSet<ValueId>,
}

impl ValueTrait for Value {
    fn name(&self) -> String { self.value.name() }
    fn set_name(&mut self, name: String) { self.value.set_name(name); }
    fn ty(&self) -> Rc<Type> { self.value.ty() }
    fn set_ty(&mut self, ty: Rc<Type>) { self.value.set_ty(ty); }
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
    Parameter(ParameterValue),
}

impl_value_trait!{ValueType{
    Global,
    Instruction,
    Constant,
    Parameter,
}}

impl ValueType {
    pub fn as_inst(&self) -> &InstructionValue {
        match self {
            ValueType::Instruction(v) => v,
            _ => panic!("not an instruction"),
        }
    }

    pub fn as_inst_mut(&mut self) -> &mut InstructionValue {
        match self {
            ValueType::Instruction(v) => v,
            _ => panic!("not an instruction"),
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, ValueType::Constant(_))
    }

    pub fn as_constant(&self) -> &ConstantValue {
        match self {
            ValueType::Constant(v) => v,
            _ => panic!("not a constant"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParameterValue {
    pub name: String,
    pub ty: Rc<Type>,
}

impl_value_trait!(ParameterValue);

#[derive(Debug, Clone)]
pub struct GlobalValue {
    pub name: String,
    pub ty: Rc<Type>,
}

impl_value_trait!(GlobalValue);

#[derive(Debug, Clone, Copy)]
pub enum ConstantValue {
    I1(bool),
    I32(i32),
}

impl ValueTrait for ConstantValue {
    fn ty(&self) -> Rc<Type> {
        match self {
            ConstantValue::I1(_) => Type::i1_type(),
            ConstantValue::I32(_) => Type::i32_type(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum InstructionValue {
    Binary(BinaryInst),
    Load(LoadInst),
    Store(StoreInst),
    Alloca(AllocaInst),
    Return(ReturnInst),
    Branch(BranchInst),
    Jump(JumpInst),
    Zext(ZextInst),
    Phi(PhiInst),
    GetElemPtr(GetElemPtrInst),
}

impl_value_trait!{InstructionValue{
    Binary,
    Load,
    Store,
    Alloca,
    Return,
    Branch,
    Jump,
    Zext,
    Phi,
    GetElemPtr,
}}

#[derive(Debug, Clone)]
pub struct BinaryInst {
    pub lhs: ValueId,
    pub rhs: ValueId,
    pub op: BinaryOp,
    pub name: String,
    pub ty: Rc<Type>,
}

impl_value_trait!(BinaryInst);

#[derive(Debug, Clone)]
pub struct LoadInst {
    pub ptr: ValueId,
    pub name: String,
    pub ty: Rc<Type>,
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
    pub alloc_ty: Rc<Type>,
    pub ty: Rc<Type>,
    pub name: String,
}

impl_value_trait!(AllocaInst);

#[derive(Debug, Clone)]
pub struct ReturnInst {
    pub value: Option<ValueId>,
}

impl ValueTrait for ReturnInst {}

#[derive(Debug, Clone)]
pub struct BranchInst {
    pub cond: ValueId,
    pub succ: BlockId,
    pub fail: BlockId,
}

impl ValueTrait for BranchInst {}

#[derive(Debug, Clone)]
pub struct JumpInst {
    pub succ: BlockId,
}

impl ValueTrait for JumpInst {}

#[derive(Debug, Clone)]
pub struct ZextInst {
    pub value: ValueId,
    pub from: Rc<Type>,
    pub ty: Rc<Type>,
    pub name: String,
}

impl_value_trait!(ZextInst);

#[derive(Debug, Clone)]
pub struct PhiInst {
    pub ty: Rc<Type>,
    pub name: String,
    pub args: Vec<(ValueId, BlockId)>,
}

impl_value_trait!(PhiInst);

#[derive(Debug, Clone)]
pub struct GetElemPtrInst {
    pub ptr: ValueId,
    // pub indices: Vec<ValueId>,
    pub index: ValueId,
    pub name: String,
    pub ty: Rc<Type>,
    pub aggregate_ty: Rc<Type>,
}

impl_value_trait!(GetElemPtrInst);

pub fn calculate_used_by(unit: &mut TransUnit, func: &str) {
    let func = unit.funcs[func].clone();

    for_each_bb_and_inst!{
        unit, func(bb, block, inst, insn), {}, {
            let inst_mut = unit.values.get_mut(inst).unwrap();
            inst_mut.used_by.clear();
        }
    }

    for_each_bb_and_inst!{
        unit, func(bb, block, inst, insn), {}, {
            match insn.value.as_inst() {
                InstructionValue::Binary(b) => {
                    let lhs_mut = unit.values.get_mut(b.lhs).unwrap();
                    lhs_mut.used_by.insert(inst);
                    let rhs_mut = unit.values.get_mut(b.rhs).unwrap();
                    rhs_mut.used_by.insert(inst);
                },
                InstructionValue::Load(l) => {
                    let ptr_mut = unit.values.get_mut(l.ptr).unwrap();
                    ptr_mut.used_by.insert(inst);
                },
                InstructionValue::Store(s) => {
                    let ptr_mut = unit.values.get_mut(s.ptr).unwrap();
                    ptr_mut.used_by.insert(inst);
                    let value_mut = unit.values.get_mut(s.value).unwrap();
                    value_mut.used_by.insert(inst);
                },
                InstructionValue::Alloca(_) => (),
                InstructionValue::Return(r) => {
                    if let Some(value) = r.value {
                        let value_mut = unit.values.get_mut(value).unwrap();
                        value_mut.used_by.insert(inst);
                    }
                },
                InstructionValue::Branch(b) => {
                    let cond_mut = unit.values.get_mut(b.cond).unwrap();
                    cond_mut.used_by.insert(inst);
                },
                InstructionValue::Jump(_) => (),
                InstructionValue::Zext(z) => {
                    let value_mut = unit.values.get_mut(z.value).unwrap();
                    value_mut.used_by.insert(inst);
                },
                InstructionValue::Phi(p) => {
                    for (value, _) in p.args.iter() {
                        let value_mut = unit.values.get_mut(*value).unwrap();
                        value_mut.used_by.insert(inst);
                    }
                },
                InstructionValue::GetElemPtr(g) => {
                    let ptr_mut = unit.values.get_mut(g.ptr).unwrap();
                    ptr_mut.used_by.insert(inst);
                    let index_mut = unit.values.get_mut(g.index).unwrap();
                    index_mut.used_by.insert(inst);
                },
            }
        }
    }
}