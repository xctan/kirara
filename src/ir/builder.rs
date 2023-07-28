use std::rc::Rc;
use std::collections::HashMap;

use crate::ctype::{BinaryOpType, Type};

use super::structure::TransUnit;
use super::{
    structure::{BasicBlock, BlockId, IrFunc},
    value::*,
};

/// A builder for constructing IR functions, with various helper methods for control flow manipulation.
#[derive(Debug)]
pub struct IrFuncBuilder<'a> {
    pub bbs: Vec<BlockId>,
    pub(in crate::ir) entry_bb: Option<BlockId>,
    pub(in crate::ir) cur_bb: Option<BlockId>,
    pub labels: HashMap<String, BlockId>,
    pub jumps: Vec<(BackPatchItem, String)>,
    pub unit: &'a mut TransUnit,
    pub ty: Rc<Type>,
    pub params: Vec<ValueId>,
}

macro_rules! inst_proxy {
    ($name:ident ( $($arg:ident : $typ:ty),* )) => (
        pub fn $name(&mut self, $($arg: $typ),*) -> (&mut Self, ValueId) {
            let id = self.unit.$name($($arg),*);
            (self, id)
        }
    )
}

impl IrFuncBuilder<'_> {
    pub fn count(&mut self) -> usize {
        self.unit.count()
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
        let mut entry = BasicBlock::new(name);
        entry.is_labeled = true;
        let bb = self.unit.blocks.alloc(entry);
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
        // println!("new bb: {}", name);
        let bb = self.unit.blocks.alloc(BasicBlock::new(name));
        self.bbs.push(bb);
        self.cur_bb = Some(bb);
        (old, bb)
    }

    pub fn start_new_named_bb(&mut self, name: &str) -> (BlockId, BlockId) {
        let old = self.cur_bb();
        let name = format!("{}.{}", name, self.count());
        // println!("new bb: {}", name);
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
                }
                ValueType::Instruction(InstructionValue::Jump(ref jmp)) => {
                    self.add_predecessor(jmp.succ, bb);
                }
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

    pub fn push(&mut self, value: ValueId) {
        self.push_at(self.cur_bb(), value, true);
    }

    pub fn add_predecessor(&mut self, bb: BlockId, pred: BlockId) {
        let bb = self.unit.blocks.get_mut(bb).unwrap();
        bb.preds.push(pred);
    }

    // function param builder
    pub fn param(&mut self, ty: Rc<Type>) -> ValueId {
        let var = ParameterValue {
            name: self.unit.gen_local_name(),
            ty,
        };
        let val = ValueType::Parameter(var);
        let val = Value::new(val);
        let id = self.unit.values.alloc(val);
        self.params.push(id);
        id
    }

    // proxy to value builders
    pub fn const_i32(&mut self, val: i32) -> ValueId {
        self.unit.const_i32(val)
    }
    pub fn const_i1(&mut self, val: bool) -> ValueId {
        self.unit.const_i1(val)
    }
    pub fn const_f32(&mut self, val: f32) -> ValueId {
        self.unit.const_f32(val)
    }

    // proxy to inst builders
    inst_proxy!(alloca(ty: Rc<Type>));
    inst_proxy!(ret(val: Option<ValueId>));
    inst_proxy!(store(val: ValueId, ptr: ValueId));
    inst_proxy!(load(ptr: ValueId));
    inst_proxy!(binary(op: BinaryOpType, lhs: ValueId, rhs: ValueId));
    inst_proxy!(branch(cond: ValueId, succ: BlockId, fail: BlockId));
    inst_proxy!(jump(succ: BlockId));
    inst_proxy!(zext(val: ValueId, ty: Rc<Type>));
    inst_proxy!(phi(args: Vec<(ValueId, BlockId)>, ty: Rc<Type>));
    inst_proxy!(gep(ptr: ValueId, idx: Vec<ValueId>));
    inst_proxy!(call(func: &str, ret: Rc<Type>, args: Vec<ValueId>));
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
            }
            BackPatchType::BranchFail => {
                let inst = builder.unit.values.get_mut(self.branch).unwrap();
                match &mut inst.value {
                    ValueType::Instruction(InstructionValue::Branch(ref mut insn)) => {
                        insn.fail = bb;
                    }
                    _ => unreachable!(),
                }
            }
            BackPatchType::Jump => {
                let inst = builder.unit.values.get_mut(self.branch).unwrap();
                match &mut inst.value {
                    ValueType::Instruction(InstructionValue::Jump(ref mut insn)) => {
                        insn.succ = bb;
                    }
                    _ => unreachable!(),
                }
            }
        }
        let inst_bb = builder.unit.inst_bb.get(&self.branch).unwrap();
        builder.add_predecessor(bb, *inst_bb);
        let target_bb = builder.unit.blocks.get_mut(bb).unwrap();
        target_bb.is_labeled = true;
    }
}
