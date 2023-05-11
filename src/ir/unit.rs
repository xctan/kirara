use std::rc::Weak;

use id_arena::{Arena, Id};

use crate::ctype::{Type, BinaryOpType};

use super::value::*;

#[derive(Debug)]
pub struct TransUnit {
    /// global value allocator
    pub values: Arena<Value>,
    pub blocks: Arena<BasicBlock>,
    // global variable defs
    // funcs

    // todo: move to function
    pub bbs: Vec<BlockId>,
    // temporary usage
    pub cur_bb: BlockId,
    pub entry_bb: BlockId,

    counter: usize,
}

impl TransUnit {
    pub fn new() -> Self {
        let mut bb_arena = Arena::new();
        let entry_bb = 
            bb_arena.alloc(BasicBlock::new("0".into()));
        Self {
            values: Arena::new(),
            blocks: bb_arena,
            cur_bb: entry_bb,
            entry_bb,
            bbs: vec![entry_bb],
            counter: 1,
        }
    }

    pub fn count(&mut self) -> usize {
        let c = self.counter;
        self.counter += 1;
        c
    }

    pub fn gen_local_name(&mut self) -> String {
        let name = format!("%{}", self.count());
        name
    }

    pub fn start_new_bb(&mut self) -> BlockId {
        let name = format!("{}", self.count());
        let bb = self.blocks.alloc(BasicBlock::new(name));
        self.bbs.push(bb);
        self.cur_bb = bb;
        bb
    }

    // insert at end of bb
    pub fn push_at(&mut self, bb: BlockId, value: ValueId) {
        let bb = self.blocks.get_mut(bb).unwrap();
        match (bb.insts_start, bb.insts_end) {
            (None, None) => {
                bb.insts_start = Some(value);
                bb.insts_end = Some(value);
            }
            (Some(_), Some(end)) => {
                let mut this = self.values.get_mut(value).unwrap();
                this.prev = Some(end);
                let mut end = self.values.get_mut(end).unwrap();
                end.next = Some(value);
                bb.insts_end = Some(value);
            }
            _ => unreachable!(),
        }
    }

    // insert before some inst in bb
    pub fn insert_at(&mut self, bb: BlockId, value: ValueId, before: ValueId) {
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

    pub fn push(&mut self, value: ValueId) {
        self.push_at(self.cur_bb, value);
    }

    #[allow(unused)]
    pub fn insert(&mut self, value: ValueId, before: ValueId) {
        self.insert_at(self.cur_bb, value, before);
    }

    // global value builders
    pub fn const_i32(&mut self, val: i32) -> ValueId {
        let val = ConstantValue::I32(val);
        let val = ValueType::Constant(val);
        let val = Value::new(val);
        self.values.alloc(val)
    }

    // local inst builders
    pub fn alloca(&mut self, ty: Weak<Type>) -> (&mut Self, ValueId) {
        let inst = AllocaInst {
            name: self.gen_local_name(),
            ty,
        };
        let val = ValueType::Instruction(InstructionValue::Alloca(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        (self, id)
    }

    pub fn ret(&mut self, value: Option<ValueId>) -> (&mut Self, ValueId) {
        let inst = ReturnInst {
            value,
        };
        let val = ValueType::Instruction(InstructionValue::Return(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        if let Some(value) = value {
            let value = self.values.get_mut(value).unwrap();
            value.used_by.insert(id);
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
        let id = self.values.alloc(val);
        let value = self.values.get_mut(value).unwrap();
        value.used_by.insert(id);
        (self, id)
    }

    pub fn load(&mut self, ptr: ValueId) -> (&mut Self, ValueId) {
        let ptr_val = self.values.get(ptr).unwrap();
        let ty = ptr_val.ty();
        let inst = LoadInst {
            name: self.gen_local_name(),
            ty,
            ptr,
        };
        let val = ValueType::Instruction(InstructionValue::Load(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        let ptr_val = self.values.get_mut(ptr).unwrap();
        ptr_val.used_by.insert(id);
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
                let lhs = self.values.get(lhs).unwrap();
                lhs.ty()
            },
            BinaryOpType::Ne => Type::i1_type(),
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
        let lhs = self.values.get_mut(lhs).unwrap();
        lhs.used_by.insert(id);
        let rhs = self.values.get_mut(rhs).unwrap();
        rhs.used_by.insert(id);
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
        let id = self.values.alloc(val);
        let cond = self.values.get_mut(cond).unwrap();
        cond.used_by.insert(id);
        (self, id)
    }

    pub fn jump(&mut self, succ: BlockId) -> (&mut Self, ValueId) {
        let inst = JumpInst {
            succ,
        };
        let val = ValueType::Instruction(InstructionValue::Jump(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        (self, id)
    }
}

pub trait LocalInstExt {
    fn push(self) -> ValueId;

    fn push_to(self, at: BlockId) -> ValueId;
}

impl LocalInstExt for (&mut TransUnit, ValueId) {
    fn push(self) -> ValueId {
        let (unit, value) = self;
        unit.push(value);
        value
    }

    fn push_to(self, at: BlockId) -> ValueId {
        let (unit, value) = self;
        unit.push_at(at, value);
        value
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,

    // pred
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