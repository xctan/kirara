use std::{rc::Weak, collections::HashMap};

use id_arena::{Arena, Id};

use crate::ctype::{Type, BinaryOpType, TypePtrHelper};

use super::value::*;

#[derive(Debug)]
pub struct IrFunc {
    pub bbs: Vec<BlockId>,
    pub entry_bb: BlockId,
    pub ty: Weak<Type>,
    pub params: Vec<ValueId>,
}

#[derive(Debug)]
pub struct IrFuncBuilder<'a> {
    pub bbs: Vec<BlockId>,
    entry_bb: Option<BlockId>,
    cur_bb: Option<BlockId>,
    pub labels: HashMap<String, BlockId>,
    pub jumps: Vec<(BackPatchItem, String)>,
    counter: usize,
    pub unit: &'a mut TransUnit,
    pub ty: Weak<Type>,
    pub params: Vec<ValueId>,
}

impl IrFuncBuilder<'_> {
    pub fn count(&mut self) -> usize {
        let c = self.counter;
        self.counter += 1;
        c
    }

    pub fn gen_local_name(&mut self) -> String {
        let name = format!("%{}", self.count());
        name
    }

    pub fn cur_bb(&self) -> BlockId {
        self.cur_bb.unwrap()
    }

    #[allow(unused)]
    pub fn entry_bb(&self) -> BlockId {
        self.entry_bb.unwrap()
    }

    pub fn start(&mut self) {
        let name = format!("{}", self.count());
        let bb = self.unit.blocks.alloc(BasicBlock::new(name));
        self.bbs.push(bb);
        self.entry_bb = Some(bb);
        self.cur_bb = Some(bb);
    }

    pub fn finish(self) -> IrFunc {
        IrFunc {
            bbs: self.bbs,
            entry_bb: self.entry_bb.unwrap(),
            ty: self.ty,
            params: self.params,
        }
    }

    /// start a new bb and return old and new bb
    pub fn start_new_bb(&mut self) -> (BlockId, BlockId) {
        let old = self.cur_bb();
        let name = format!("{}", self.count());
        let bb = self.unit.blocks.alloc(BasicBlock::new(name));
        self.bbs.push(bb);
        self.cur_bb = Some(bb);
        (old, bb)
    }

    /// insert at end of bb
    pub fn push_at(&mut self, bb: BlockId, value: ValueId, track: bool) {
        // track preds of bb
        if track {
            let val = self.unit.values.get(value).unwrap().clone();
            match val.value {
                ValueType::Instruction(InstructionValue::Branch(ref br)) => {
                    self.add_predecessor(br.succ, bb);
                    self.add_predecessor(br.fail, bb);
                },
                ValueType::Instruction(InstructionValue::Jump(ref jmp)) => {
                    self.add_predecessor(jmp.succ, bb);
                },
                _ => (),
            }
        }

        self.unit.inst_bb.insert(value, bb);
        let bb = self.unit.blocks.get_mut(bb).unwrap();
        match (bb.insts_start, bb.insts_end) {
            (None, None) => {
                bb.insts_start = Some(value);
                bb.insts_end = Some(value);
            }
            (Some(_), Some(end)) => {
                let mut this = self.unit.values.get_mut(value).unwrap();
                this.prev = Some(end);
                let mut end = self.unit.values.get_mut(end).unwrap();
                end.next = Some(value);
                bb.insts_end = Some(value);
            }
            _ => unreachable!(),
        }
    }

    // insert before some inst in bb
    pub fn insert_at(&mut self, bb: BlockId, value: ValueId, before: ValueId) {
        self.unit.inst_bb.insert(value, bb);
        let that = self.unit.values.get(before).unwrap();
        let prev = that.prev;
        let this = self.unit.values.get_mut(value).unwrap();
        this.prev = prev;
        this.next = Some(before);
        let that = self.unit.values.get_mut(before).unwrap();
        that.prev = Some(value);
        if let Some(prev) = prev {
            let mut prev = self.unit.values.get_mut(prev).unwrap();
            prev.next = Some(value);
        } else {
            let bb = self.unit.blocks.get_mut(bb).unwrap();
            bb.insts_start = Some(value);
        }
    }

    pub fn push(&mut self, value: ValueId) {
        self.push_at(self.cur_bb(), value, true);
    }

    #[allow(unused)]
    pub fn insert(&mut self, value: ValueId, before: ValueId) {
        self.insert_at(self.cur_bb(), value, before);
    }

    pub fn add_used_by(&mut self, value: ValueId, user: ValueId) {
        let value = self.unit.values.get_mut(value).unwrap();
        value.used_by.insert(user);
    }

    pub fn add_predecessor(&mut self, bb: BlockId, pred: BlockId) {
        let bb = self.unit.blocks.get_mut(bb).unwrap();
        bb.preds.push(pred);
    }

    // global value builders
    pub fn const_i32(&mut self, val: i32) -> ValueId {
        let val = ConstantValue::I32(val);
        let val = ValueType::Constant(val);
        let val = Value::new(val);
        self.unit.values.alloc(val)
    }

    pub fn const_i1(&mut self, val: bool) -> ValueId {
        let val = ConstantValue::I1(val);
        let val = ValueType::Constant(val);
        let val = Value::new(val);
        self.unit.values.alloc(val)
    }

    pub fn param(&mut self, ty: Weak<Type>) -> ValueId {
        let var = ParameterValue {
            name: self.gen_local_name(),
            ty,
        };
        let val = ValueType::Parameter(var);
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        self.params.push(id);
        id
    }

    // local inst builders
    pub fn alloca(&mut self, ty: Weak<Type>) -> (&mut Self, ValueId) {
        let inst = AllocaInst {
            name: self.gen_local_name(),
            ty,
        };
        let val = ValueType::Instruction(InstructionValue::Alloca(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        (self, id)
    }

    pub fn ret(&mut self, value: Option<ValueId>) -> (&mut Self, ValueId) {
        let inst = ReturnInst {
            value,
        };
        let val = ValueType::Instruction(InstructionValue::Return(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        if let Some(value) = value {
            self.add_used_by(value, id);
        }
        (self, id)
    }

    pub fn store(&mut self, value: ValueId, ptr: ValueId) -> (&mut Self, ValueId) {
        let inst = StoreInst {
            value,
            ptr,
        };
        let val = ValueType::Instruction(InstructionValue::Store(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        self.add_used_by(value, id);
        (self, id)
    }

    pub fn load(&mut self, ptr: ValueId) -> (&mut Self, ValueId) {
        let ptr_val = self.unit.values.get(ptr).unwrap();
        let ty = ptr_val.ty();
        let inst = LoadInst {
            name: self.gen_local_name(),
            ty,
            ptr,
        };
        let val = ValueType::Instruction(InstructionValue::Load(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        self.add_used_by(ptr, id);
        (self, id)
    }

    pub fn binary(&mut self, op: BinaryOpType, lhs: ValueId, rhs: ValueId) -> (&mut Self, ValueId) {
        let ty = match op {
            BinaryOpType::Add |
            BinaryOpType::Sub |
            BinaryOpType::Mul |
            BinaryOpType::Div |
            BinaryOpType::Mod |
            BinaryOpType::Assign => {
                let lhs = self.unit.values.get(lhs).unwrap();
                lhs.ty()
            },
            BinaryOpType::Ne |
            BinaryOpType::Eq |
            BinaryOpType::Lt |
            BinaryOpType::Le |  
            BinaryOpType::Gt |
            BinaryOpType::Ge |
            BinaryOpType::LogAnd |
            BinaryOpType::LogOr => Type::i1_type(),
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
        let id = self.unit.values.alloc(val);
        self.add_used_by(lhs, id);
        self.add_used_by(rhs, id);
        (self, id)
    }

    pub fn branch(&mut self, cond: ValueId, succ: BlockId, fail: BlockId) -> (&mut Self, ValueId) {
        let inst = BranchInst {
            cond,
            succ,
            fail,
        };
        let val = ValueType::Instruction(InstructionValue::Branch(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        self.add_used_by(cond, id);
        (self, id)
    }

    pub fn jump(&mut self, succ: BlockId) -> (&mut Self, ValueId) {
        let inst = JumpInst {
            succ,
        };
        let val = ValueType::Instruction(InstructionValue::Jump(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        (self, id)
    }

    pub fn zext(&mut self, value: ValueId, ty: Weak<Type>) -> (&mut Self, ValueId) {
        let from = self.unit.values.get(value).unwrap().ty();
        let inst = ZextInst {
            name: self.gen_local_name(),
            value,
            ty,
            from,
        };
        let val = ValueType::Instruction(InstructionValue::Zext(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
       self.add_used_by(value, id);
        (self, id)
    }

    pub fn phi(&mut self, args: Vec<(ValueId, BlockId)>) -> (&mut Self, ValueId) {
        assert!(!args.is_empty()); // used to infer type
        let ty = self.unit.values.get(args[0].0).unwrap().ty();
        let inst = PhiInst {
            name: self.gen_local_name(),
            ty,
            args: args.clone(),
        };
        let val = ValueType::Instruction(InstructionValue::Phi(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        for (value, _) in args {
            self.add_used_by(value, id);
        }
        (self, id)
    }

    pub fn gep(&mut self, ptr: ValueId, idx: ValueId) -> (&mut Self, ValueId) {
        let ptr_val = self.unit.values.get(ptr).unwrap().clone();
        let ty = ptr_val.ty().get().base_type();
        let inst = GetElemPtrInst {
            name: self.gen_local_name(),
            ty,
            aggregate_ty: ptr_val.ty(),
            ptr,
            index: idx,
        };
        let val = ValueType::Instruction(InstructionValue::GetElemPtr(inst));
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        self.add_used_by(ptr, id);
        self.add_used_by(idx, id);
        (self, id)
    }
}

#[derive(Debug)]
pub struct TransUnit {
    /// global value allocator
    pub values: Arena<Value>,
    pub blocks: Arena<BasicBlock>,
    pub inst_bb: HashMap<ValueId, BlockId>,
    // global variable defs

    // funcs
    pub funcs: HashMap<String, IrFunc>,
}

impl TransUnit {
    pub fn new() -> Self {
        Self {
            values: Arena::new(),
            blocks: Arena::new(),
            inst_bb: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn builder<'a>(&'a mut self, ty: Weak<Type>) -> IrFuncBuilder<'a> {
        IrFuncBuilder {
            unit: self,
            cur_bb: None,
            bbs: Vec::new(),
            entry_bb: None,
            counter: 0,
            labels: HashMap::new(),
            jumps: Vec::new(),
            ty,
            params: Vec::new(),
        }
    }
}

pub trait LocalInstExt {
    fn push(self) -> ValueId;

    fn push_only(self) -> ValueId;

    fn push_to(self, at: BlockId) -> ValueId;
}

impl LocalInstExt for (&mut IrFuncBuilder<'_>, ValueId) {
    fn push(self) -> ValueId {
        let (unit, value) = self;
        unit.push(value);
        value
    }

    fn push_only(self) -> ValueId {
        let (unit, value) = self;
        unit.push_at(unit.cur_bb(), value, false);
        value
    }

    fn push_to(self, at: BlockId) -> ValueId {
        let (unit, value) = self;
        unit.push_at(at, value, true);
        value
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    
    pub is_labeled: bool,

    pub preds: Vec<BlockId>,
    // idom
    // dom_by
    // doms

    pub insts_start: Option<ValueId>,
    pub insts_end: Option<ValueId>,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            is_labeled: false,
            preds: Vec::new(),
            insts_start: None,
            insts_end: None,
        }
    }

    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.insts_start.is_none()
    }
}

pub type BlockId = Id<BasicBlock>;

#[derive(Debug, Clone)]
pub struct BackPatchItem {
    pub branch: ValueId,
    pub slot: BackPatchType,
}

#[derive(Debug, Clone)]
pub enum BackPatchType {
    BranchSuccess,
    BranchFail,
    Jump,
}

impl BackPatchItem {
    pub fn backpatch(&self, builder: &mut IrFuncBuilder<'_>, bb: BlockId) {
        match self.slot {
            BackPatchType::BranchSuccess => {
                let inst = builder.unit.values.get_mut(self.branch).unwrap();
                match &mut inst.value {
                    ValueType::Instruction(InstructionValue::Branch(ref mut insn)) => {
                        insn.succ = bb;
                    }
                    _ => unreachable!(),
                }
            },
            BackPatchType::BranchFail => {
                let inst = builder.unit.values.get_mut(self.branch).unwrap();
                match &mut inst.value {
                    ValueType::Instruction(InstructionValue::Branch(ref mut insn)) => {
                        insn.fail = bb;
                    }
                    _ => unreachable!(),
                }
            },
            BackPatchType::Jump => {
                let inst = builder.unit.values.get_mut(self.branch).unwrap();
                match &mut inst.value {
                    ValueType::Instruction(InstructionValue::Jump(ref mut insn)) => {
                        insn.succ = bb;
                    }
                    _ => unreachable!(),
                }
            },
        }
        let inst_bb = builder.unit.inst_bb.get(&self.branch).unwrap();
        builder.add_predecessor(bb, *inst_bb);
    }
}