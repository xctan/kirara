use std::collections::HashMap;
use std::rc::Rc;

use crate::alloc::{Id, Arena};
use crate::ast::Initializer;
use crate::ctype::{Type, BinaryOpType, TypePtrHelper};
use crate::ir::value::{
    InstructionValue, ReturnInst, StoreInst, LoadInst, BinaryInst, BranchInst, 
    ZextInst, GetElemPtrInst, ValueType, PhiInst
};

use super::builder::IrFuncBuilder;
use super::value::{ValueId, Value, ConstantValue, AllocaInst, ValueTrait, JumpInst, GlobalValue};

#[derive(Debug, Clone)]
pub struct IrFunc {
    pub bbs: Vec<BlockId>,
    pub entry_bb: BlockId,
    pub ty: Rc<Type>,
    pub params: Vec<ValueId>,
}


#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub name: String,
    
    pub is_labeled: bool,

    pub preds: Vec<BlockId>,
    /// reverse post order
    pub rpo: usize,
    /// immediate dominated by
    pub idom: Option<BlockId>,
    /// dominated children
    pub dom: Vec<BlockId>,
    /// dominance frontier
    pub df: Vec<BlockId>,

    pub insts_start: Option<ValueId>,
    pub insts_end: Option<ValueId>,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            is_labeled: false,
            preds: Vec::new(),
            rpo: 0,
            idom: None,
            dom: Vec::new(),
            df: Vec::new(),
            insts_start: None,
            insts_end: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.insts_start.is_none()
    }
}

pub type BlockId = Id<BasicBlock>;

#[derive(Debug)]
pub struct TransUnit {
    /// global value allocator
    pub values: Arena<Value>,
    pub blocks: Arena<BasicBlock>,
    pub inst_bb: HashMap<ValueId, BlockId>,
    // global variable defs
    pub globals: HashMap<String, Initializer>,

    // funcs
    pub funcs: HashMap<String, IrFunc>,

    counter: usize,
}


impl TransUnit {
    pub fn new() -> Self {
        Self {
            values: Arena::new(),
            blocks: Arena::new(),
            inst_bb: HashMap::new(),
            globals: HashMap::new(),
            funcs: HashMap::new(),
            counter: 0,
        }
    }

    pub fn builder<'a>(&'a mut self, ty: Rc<Type>) -> IrFuncBuilder<'a> {
        IrFuncBuilder {
            unit: self,
            cur_bb: None,
            bbs: Vec::new(),
            entry_bb: None,
            labels: HashMap::new(),
            jumps: Vec::new(),
            ty,
            params: Vec::new(),
        }
    }

    pub fn funcs(&self) -> Vec<String> {
        self.funcs.keys().map(|id| id.clone()).collect()
    }

    pub fn count(&mut self) -> usize {
        let ret = self.counter;
        self.counter += 1;
        ret
    }

    pub fn gen_local_name(&mut self) -> String {
        let name = format!("%{}", self.count());
        name
    }

    pub fn add_used_by(&mut self, value: ValueId, user: ValueId) {
        let value = self.values.get_mut(value).unwrap();
        value.used_by.insert(user);
    }

    // methods for modifying insts in bb

    // insert before some inst in bb
    #[allow(unused)]
    pub fn insert_before(&mut self, bb: BlockId, value: ValueId, before: ValueId) {
        self.inst_bb.insert(value, bb);
        let that = self.values.get(before).unwrap();
        let prev = that.prev;
        let this = self.values.get_mut(value).unwrap();
        this.prev = prev;
        this.next = Some(before);
        let that = self.values.get_mut(before).unwrap();
        that.prev = Some(value);
        if let Some(prev) = prev {
            let mut prev = self.values.get_mut(prev).unwrap();
            prev.next = Some(value);
        } else {
            let bb = self.blocks.get_mut(bb).unwrap();
            bb.insts_start = Some(value);
        }
    }

    /// insert a inst at the beginning of a bb
    pub fn insert_at_begin(&mut self, bb: BlockId, value: ValueId) {
        self.inst_bb.insert(value, bb);
        let block = self.blocks.get(bb).unwrap();
        match block.insts_start {
            None => {
                let bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_start = Some(value);
                bb.insts_end = Some(value);
            }
            Some(start) => {
                let mut this = self.values.get_mut(value).unwrap();
                this.next = Some(start);
                let mut start = self.values.get_mut(start).unwrap();
                start.prev = Some(value);
                let bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_start = Some(value);
            }
        }
    }

    /// remove a inst from bb
    pub fn remove(&mut self, bb: BlockId, value: ValueId) {
        let this = self.values.get(value).unwrap().clone();
        match this.prev {
            Some(prev) => {
                let mut prev = self.values.get_mut(prev).unwrap();
                prev.next = this.next;
            }
            None => {
                let mut bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_start = this.next;
            }
        }
        match this.next {
            Some(next) => {
                let mut next = self.values.get_mut(next).unwrap();
                next.prev = this.prev;
            }
            None => {
                let mut bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_end = this.prev;
            }
        }
        self.values.remove(value);
    }

    /// replace all occurrences of old inst value with new
    pub fn replace(&mut self, old: ValueId, new: ValueId) {
        let occurrences = self.values.get(old).unwrap().used_by.clone();
        for oc in occurrences {
            self.add_used_by(new, oc);
            let user = self.values.get(oc).unwrap();
            macro_rules! rep {
                ($this:ident, $kind:ident, $st:ident { $($member:ident),+ }) => {
                    if false { unreachable!() }
                    $(
                        else if $this.$member == old {
                            InstructionValue::$kind($st { $member: new, ..$this })
                        }
                    )+
                    else { unreachable!() }
                }
            }
            let new_value = match user.value.as_inst().clone() {
                InstructionValue::Alloca(_) => unreachable!(),
                InstructionValue::Return(ReturnInst { value }) => {
                    if let Some(value) = value {
                        if value == old {
                            InstructionValue::Return(ReturnInst { value: Some(new) })
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
                InstructionValue::Store(this) => rep!(this, Store, StoreInst { ptr, value }),
                InstructionValue::Load(li) => rep!(li, Load, LoadInst { ptr }),
                InstructionValue::Binary(bin) => rep!(bin, Binary, BinaryInst { lhs, rhs }),
                InstructionValue::Branch(br) => rep!(br, Branch, BranchInst { cond }),
                InstructionValue::Jump(_) => unreachable!(),
                InstructionValue::Zext(ze) => rep!(ze, Zext, ZextInst { value }),
                InstructionValue::GetElemPtr(gep) => {
                    rep!(gep, GetElemPtr, GetElemPtrInst { ptr, index })
                }
                InstructionValue::Phi(phi) => {
                    let mut new_phi = phi.clone();
                    new_phi
                        .args
                        .iter_mut()
                        .filter(|arg| arg.0 == old)
                        .for_each(|arg| arg.0 = new);
                    InstructionValue::Phi(new_phi)
                }
            };
            let user = self.values.get_mut(oc).unwrap();
            user.value = ValueType::Instruction(new_value);
        }
    }

    /// get successors of a bb
    pub fn succ(&self, bb: BlockId) -> Vec<BlockId> {
        let bb = self.blocks.get(bb).unwrap();
        let last = bb.insts_end.unwrap();
        let last = self.values.get(last).unwrap();
        match last.value.as_inst() {
            &InstructionValue::Branch(ref insn) => {
                vec![insn.succ, insn.fail]
            }
            &InstructionValue::Jump(ref insn) => {
                vec![insn.succ]
            }
            &InstructionValue::Return(_) => {
                vec![]
            }
            _ => panic!("Invalid terminator instruction"),
        }
    }

    // global value builders
    pub fn const_i32(&mut self, val: i32) -> ValueId {
        let val = ConstantValue::I32(val);
        let val = ValueType::Constant(val);
        let val = Value::new(val);
        self.values.alloc(val)
    }

    pub fn const_i1(&mut self, val: bool) -> ValueId {
        let val = ConstantValue::I1(val);
        let val = ValueType::Constant(val);
        let val = Value::new(val);
        self.values.alloc(val)
    }

    pub fn global(&mut self, name: &str, ty: Rc<Type>) -> ValueId {
        let ty = ty.get_nocv();
        let val = ValueType::Global(GlobalValue{ name: name.to_string(), ty });
        let val = Value::new(val);
        self.values.alloc(val)
    }

    // local inst builders
    pub fn alloca(&mut self, ty: Rc<Type>) -> ValueId {
        let ty = ty.get_nocv();
        let inst = AllocaInst {
            name: self.gen_local_name(),
            alloc_ty: ty.clone(),
            ty: Type::ptr_to(ty),
        };
        let val = ValueType::Instruction(InstructionValue::Alloca(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        id
    }

    pub fn ret(&mut self, value: Option<ValueId>) -> ValueId {
        let inst = ReturnInst { value };
        let val = ValueType::Instruction(InstructionValue::Return(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        if let Some(value) = value {
            self.add_used_by(value, id);
        }
        id
    }

    pub fn store(&mut self, value: ValueId, ptr: ValueId) -> ValueId {
        let inst = StoreInst { value, ptr };
        let val = ValueType::Instruction(InstructionValue::Store(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(value, id);
        id
    }

    pub fn load(&mut self, ptr: ValueId) -> ValueId {
        let ptr_val = self.values.get(ptr).unwrap();
        let ty = ptr_val.ty().base_type().get_nocv();
        let inst = LoadInst {
            name: self.gen_local_name(),
            ty,
            ptr,
        };
        let val = ValueType::Instruction(InstructionValue::Load(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(ptr, id);
        id
    }

    pub fn binary(&mut self, op: BinaryOpType, lhs: ValueId, rhs: ValueId) -> ValueId {
        let ty = match op {
            BinaryOpType::Add
            | BinaryOpType::Sub
            | BinaryOpType::Mul
            | BinaryOpType::Div
            | BinaryOpType::Mod
            | BinaryOpType::Assign => {
                let lhs = self.values.get(lhs).unwrap();
                lhs.ty().get_nocv()
            }
            BinaryOpType::Ne
            | BinaryOpType::Eq
            | BinaryOpType::Lt
            | BinaryOpType::Le
            | BinaryOpType::Gt
            | BinaryOpType::Ge
            | BinaryOpType::LogAnd
            | BinaryOpType::LogOr => Type::i1_type(),
            _ => unimplemented!(),
        };
        let inst = BinaryInst {
            lhs,
            rhs,
            op,
            name: self.gen_local_name(),
            ty,
        };
        let val = ValueType::Instruction(InstructionValue::Binary(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(lhs, id);
        self.add_used_by(rhs, id);
        id
    }

    pub fn branch(&mut self, cond: ValueId, succ: BlockId, fail: BlockId) -> ValueId {
        let inst = BranchInst { cond, succ, fail };
        let val = ValueType::Instruction(InstructionValue::Branch(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(cond, id);
        id
    }

    pub fn jump(&mut self, succ: BlockId) -> ValueId {
        let inst = JumpInst { succ };
        let val = ValueType::Instruction(InstructionValue::Jump(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        id
    }

    pub fn zext(&mut self, value: ValueId, ty: Rc<Type>) -> ValueId {
        let from = self.values.get(value).unwrap().ty();
        let inst = ZextInst {
            name: self.gen_local_name(),
            value,
            ty,
            from,
        };
        let val = ValueType::Instruction(InstructionValue::Zext(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(value, id);
        id
    }

    pub fn phi(&mut self, args: Vec<(ValueId, BlockId)>) -> ValueId {
        assert!(!args.is_empty()); // used to infer type
        let ty = self.values.get(args[0].0).unwrap().ty();
        let inst = PhiInst {
            name: self.gen_local_name(),
            ty,
            args: args.clone(),
        };
        let val = ValueType::Instruction(InstructionValue::Phi(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        for (value, _) in args {
            self.add_used_by(value, id);
        }
        id
    }

    pub fn gep(&mut self, ptr: ValueId, idx: ValueId) -> ValueId {
        let ptr_val = self.values.get(ptr).unwrap().clone();
        let ty = ptr_val.ty().base_type().get_nocv();
        let inst = GetElemPtrInst {
            name: self.gen_local_name(),
            ty: Type::ptr_to(ty.base_type()),
            aggregate_ty: ty,
            ptr,
            index: idx,
        };
        let val = ValueType::Instruction(InstructionValue::GetElemPtr(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(ptr, id);
        self.add_used_by(idx, id);
        id
    }
}