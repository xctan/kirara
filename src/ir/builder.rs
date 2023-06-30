use std::{rc::Weak, collections::HashMap};

use id_arena::Arena;

use crate::ctype::{Type, BinaryOpType, TypePtrHelper};

use super::{value::*, structure::{BlockId, BasicBlock, IrFunc}};

/// A builder for constructing IR functions, with various helper methods for control flow manipulation.
#[derive(Debug)]
pub struct IrFuncBuilder<'a> {
    pub bbs: Vec<BlockId>,
    entry_bb: Option<BlockId>,
    cur_bb: Option<BlockId>,
    pub labels: HashMap<String, BlockId>,
    pub jumps: Vec<(BackPatchItem, String)>,
    pub unit: &'a mut TransUnit,
    pub ty: Weak<Type>,
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

    pub fn push(&mut self, value: ValueId) {
        self.push_at(self.cur_bb(), value, true);
    }

    pub fn add_predecessor(&mut self, bb: BlockId, pred: BlockId) {
        let bb = self.unit.blocks.get_mut(bb).unwrap();
        bb.preds.push(pred);
    }

    // function param builder
    pub fn param(&mut self, ty: Weak<Type>) -> ValueId {
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

    // proxy to inst builders
    inst_proxy!(alloca(ty: Weak<Type>));
    inst_proxy!(ret(val: Option<ValueId>));
    inst_proxy!(store(val: ValueId, ptr: ValueId));
    inst_proxy!(load(ptr: ValueId));
    inst_proxy!(binary(op: BinaryOpType, lhs: ValueId, rhs: ValueId));
    inst_proxy!(branch(cond: ValueId, succ: BlockId, fail: BlockId));
    inst_proxy!(jump(succ: BlockId));
    inst_proxy!(zext(val: ValueId, ty: Weak<Type>));
    inst_proxy!(phi(args: Vec<(ValueId, BlockId)>));
    inst_proxy!(gep(ptr: ValueId, idx: ValueId));
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

    counter: usize,
}

impl TransUnit {
    pub fn new() -> Self {
        Self {
            values: Arena::new(),
            blocks: Arena::new(),
            inst_bb: HashMap::new(),
            funcs: HashMap::new(),
            counter: 0,
        }
    }

    pub fn builder<'a>(&'a mut self, ty: Weak<Type>) -> IrFuncBuilder<'a> {
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
                InstructionValue::Store(this) => rep!(this, Store, StoreInst{ptr, value}),
                InstructionValue::Load(li) => rep!(li, Load, LoadInst{ptr}),
                InstructionValue::Binary(bin) => rep!(bin, Binary, BinaryInst{lhs, rhs}),
                InstructionValue::Branch(br) => rep!(br, Branch, BranchInst{cond}),
                InstructionValue::Jump(_) => unreachable!(),
                InstructionValue::Zext(ze) => rep!(ze, Zext, ZextInst{value}),
                InstructionValue::GetElemPtr(gep) => rep!(gep, GetElemPtr, GetElemPtrInst{ptr, index}),
                InstructionValue::Phi(phi) => {
                    let mut new_phi = phi.clone();
                    new_phi.args.iter_mut()
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
            },
            &InstructionValue::Jump(ref insn) => {
                vec![insn.succ]
            },
            &InstructionValue::Return(_) => {
                vec![]
            },
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

    // local inst builders
    pub fn alloca(&mut self, ty: Weak<Type>) -> ValueId {
        let inst = AllocaInst {
            name: self.gen_local_name(),
            ty,
        };
        let val = ValueType::Instruction(InstructionValue::Alloca(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        id
    }

    pub fn ret(&mut self, value: Option<ValueId>) -> ValueId {
        let inst = ReturnInst {
            value,
        };
        let val = ValueType::Instruction(InstructionValue::Return(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        if let Some(value) = value {
            self.add_used_by(value, id);
        }
        id
    }

    pub fn store(&mut self, value: ValueId, ptr: ValueId) -> ValueId {
        let inst = StoreInst {
            value,
            ptr,
        };
        let val = ValueType::Instruction(InstructionValue::Store(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(value, id);
        id
    }

    pub fn load(&mut self, ptr: ValueId) -> ValueId {
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
        self.add_used_by(ptr, id);
        id
    }

    pub fn binary(&mut self, op: BinaryOpType, lhs: ValueId, rhs: ValueId) -> ValueId {
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
        let id = self.values.alloc(val);
        self.add_used_by(lhs, id);
        self.add_used_by(rhs, id);
        id
    }

    pub fn branch(&mut self, cond: ValueId, succ: BlockId, fail: BlockId) -> ValueId {
        let inst = BranchInst {
            cond,
            succ,
            fail,
        };
        let val = ValueType::Instruction(InstructionValue::Branch(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(cond, id);
        id
    }

    pub fn jump(&mut self, succ: BlockId) -> ValueId {
        let inst = JumpInst {
            succ,
        };
        let val = ValueType::Instruction(InstructionValue::Jump(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        id
    }

    pub fn zext(&mut self, value: ValueId, ty: Weak<Type>) -> ValueId {
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
        let id = self.values.alloc(val);
        self.add_used_by(ptr, id);
        self.add_used_by(idx, id);
        id
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