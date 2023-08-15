use std::collections::HashMap;
use std::rc::Rc;

use crate::alloc::{Id, Arena};
use crate::ast::Initializer;
use crate::ctype::{Type, BinaryOpType, Linkage};
use crate::ir::value::{
    InstructionValue, ReturnInst, StoreInst, LoadInst, BinaryInst, BranchInst, 
    UnaryInst, GetElemPtrInst, ValueType, PhiInst
};

use super::builder::IrFuncBuilder;
use super::callgraph::CallGraphInfo;
use super::cfg::LoopInfo;
use super::memdep::ModRef;
use super::value::{ValueId, Value, ConstantValue, AllocaInst, ValueTrait, JumpInst, GlobalValue, CallInst, UnaryOp, MemPhiInst, MemOpInst};

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
    pub is_entry: bool,

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

    pub mphi_lts: Vec<ValueId>,
    pub mphi_stl: Vec<ValueId>,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            is_labeled: false,
            is_entry: false,
            preds: Vec::new(),
            rpo: 0,
            idom: None,
            dom: Vec::new(),
            df: Vec::new(),
            insts_start: None,
            insts_end: None,
            mphi_lts: Vec::new(),
            mphi_stl: Vec::new(),
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
    pub undef: Option<ValueId>,
    // global variable defs
    pub globals: HashMap<String, GlobalObject>,

    // funcs
    pub funcs: HashMap<String, IrFunc>,
    pub external: HashMap<String, Rc<Type>>,

    // analysis results
    pub callgraph: HashMap<String, CallGraphInfo>,
    pub loopinfo: HashMap<String, LoopInfo>,
    pub modref: Option<ModRef>,
    pub dom_level: HashMap<String, HashMap<BlockId, u32>>,

    counter: usize,
}

#[derive(Debug)]
pub struct GlobalObject {
    pub init: Initializer,
    pub linkage: Linkage,
}

impl TransUnit {
    pub fn new() -> Self {
        let mut s = Self {
            values: Arena::new(),
            blocks: Arena::new(),
            inst_bb: HashMap::new(),
            undef: None,
            globals: HashMap::new(),
            external: HashMap::new(),
            funcs: HashMap::new(),
            callgraph: HashMap::new(),
            loopinfo: HashMap::new(),
            modref: None,
            dom_level: HashMap::new(),
            counter: 0,
        };
        let val = ConstantValue::Undef;
        let val = ValueType::Constant(val);
        let val = Value::new(val);
        s.undef = Some(s.values.alloc(val));
        s
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
        self.values
            .get_mut(value)
            .map(|value| value.used_by.insert(user));
    }

    pub fn remove_used_by(&mut self, value: ValueId, user: ValueId) {
        self.values
            .get_mut(value)
            .map(|value| value.used_by.remove(&user));
    }

    // methods for modifying insts in bb

    // insert before some inst in bb
    pub fn insert_before(&mut self, bb: BlockId, value: ValueId, before: ValueId) {
        self.inst_bb.insert(value, bb);
        let that = self.values.get(before).unwrap();
        let prev = that.prev;
        let this = self.values.get_mut(value).unwrap();
        if !this.value.is_inst() {
            // harmless virtual inst
            return
        }
        this.prev = prev;
        this.next = Some(before);
        let that = self.values.get_mut(before).unwrap();
        that.prev = Some(value);
        if let Some(prev) = prev {
            let prev = self.values.get_mut(prev).unwrap();
            prev.next = Some(value);
        } else {
            let bb = self.blocks.get_mut(bb).unwrap();
            bb.insts_start = Some(value);
        }
    }

    pub fn insert_before2(&mut self, value: ValueId, before: ValueId) {
        let bb = self.inst_bb.get(&before).unwrap();
        self.insert_before(*bb, value, before);
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
                let this = self.values.get_mut(value).unwrap();
                this.next = Some(start);
                let start = self.values.get_mut(start).unwrap();
                start.prev = Some(value);
                let bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_start = Some(value);
            }
        }
    }

    pub fn insert_at_end(&mut self, bb: BlockId, value: ValueId) {
        self.inst_bb.insert(value, bb);
        let block = self.blocks.get(bb).unwrap();
        match block.insts_end {
            None => {
                let bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_start = Some(value);
                bb.insts_end = Some(value);
            }
            Some(end) => {
                let this = self.values.get_mut(value).unwrap();
                this.prev = Some(end);
                let end = self.values.get_mut(end).unwrap();
                end.next = Some(value);
                let bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_end = Some(value);
            }
        }
    }

    pub fn insert_before_end(&mut self, bb: BlockId, value: ValueId) {
        let block = self.blocks.get(bb).unwrap();
        match block.insts_end {
            None => {
                self.insert_at_begin(bb, value);
            }
            Some(end) => {
                self.insert_before(bb, value, end);
            }
        }
    }

    pub fn takeout(&mut self, value: ValueId) {
        let bb = *self.inst_bb.get(&value).unwrap();
        let this = self.values.get(value).unwrap().clone();
        match this.prev {
            Some(prev) => {
                let prev = self.values.get_mut(prev).unwrap();
                prev.next = this.next;
            }
            None => {
                let bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_start = this.next;
            }
        }
        match this.next {
            Some(next) => {
                let next = self.values.get_mut(next).unwrap();
                next.prev = this.prev;
            }
            None => {
                let bb = self.blocks.get_mut(bb).unwrap();
                bb.insts_end = this.prev;
            }
        }

        self.inst_bb.remove(&value);
    }

    /// remove a inst from bb and delete it
    pub fn remove(&mut self, _bb: BlockId, value: ValueId) {
        // eprintln!("removing {:?}", value);
        // let val = self.values.get(value).unwrap();
        // eprintln!("value: {:?}", val);
        self.remove2(value)
    }

    pub fn remove2(&mut self, value: ValueId) {
        self.takeout(value);
        let oprs = self.get_operands(value);
        for opr in oprs {
            self.remove_used_by(opr, value);
        }
        self.values.remove(value);
    }

    pub fn get_operands(&self, inst: ValueId) -> Vec<ValueId> {
        let inst = self.values[inst].value.as_inst().clone();
        match inst {
            InstructionValue::Alloca(_) => vec![],
            InstructionValue::Return(ret) => {
                if let Some(ret) = ret.value {
                    vec![ret]
                } else {
                    vec![]
                }
            }
            InstructionValue::Store(st) => vec![st.value, st.ptr],
            InstructionValue::Load(ld) => vec![ld.ptr],
            InstructionValue::Binary(bin) => vec![bin.lhs, bin.rhs],
            InstructionValue::Branch(br) => vec![br.cond],
            InstructionValue::Jump(_) => vec![],
            InstructionValue::Unary(un) => vec![un.value],
            InstructionValue::GetElemPtr(gep) => {
                let mut ret = vec![gep.ptr];
                ret.extend(gep.indices);
                ret
            },
            InstructionValue::Phi(phi) => {
                let mut ret = vec![];
                for (val, _) in phi.args {
                    ret.push(val);
                }
                ret
            },
            InstructionValue::Call(call) => {
                let mut ret = vec![];
                ret.extend(call.args);
                ret
            },
            InstructionValue::MemOp(_) => vec![],
            InstructionValue::MemPhi(mphi) => {
                let mut ret = vec![];
                for (val, _) in mphi.args {
                    ret.push(val);
                }
                ret
            },
        }
    }

    /// replace all occurrences of old inst value with new
    pub fn replace(&mut self, old: ValueId, new: ValueId) {
        let occurrences = self.values.get(old).unwrap().used_by.clone();
        for oc in occurrences {
            let user = match self.values.get(oc) {
                Some(user) => user.clone(),
                None => continue,
            };
            self.add_used_by(new, oc);
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
                InstructionValue::Unary(ze) => rep!(ze, Unary, UnaryInst { value }),
                InstructionValue::GetElemPtr(gep) => {
                    // rep!(gep, GetElemPtr, GetElemPtrInst { ptr, index })
                    if gep.ptr == old {
                        InstructionValue::GetElemPtr(GetElemPtrInst { ptr: new, ..gep })
                    } else {
                        let new_indices = gep.indices.iter().map(|&idx| {
                            if idx == old {
                                new
                            } else {
                                idx
                            }
                        }).collect();
                        InstructionValue::GetElemPtr(GetElemPtrInst { indices: new_indices, ..gep })
                    }
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
                InstructionValue::Call(call) => {
                    let mut new_call = call.clone();
                    new_call
                        .args
                        .iter_mut()
                        .filter(|arg| **arg == old)
                        .for_each(|arg| *arg = new);
                    InstructionValue::Call(new_call)
                }
                raw @ InstructionValue::MemOp(_) => raw,
                raw @ InstructionValue::MemPhi(_) => raw,
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

    pub fn succ_mut(&mut self, bb: BlockId) -> Vec<&mut BlockId> {
        let bb = &mut self.blocks[bb];
        let last = bb.insts_end.unwrap();
        let last = &mut self.values[last];
        match last.value.as_inst_mut() {
            &mut InstructionValue::Branch(ref mut insn) => {
                vec![&mut insn.succ, &mut insn.fail]
            }
            &mut InstructionValue::Jump(ref mut insn) => {
                vec![&mut insn.succ]
            }
            &mut InstructionValue::Return(_) => {
                vec![]
            }
            _ => panic!("Invalid terminator instruction"),
        }
    }

    pub fn has_side_effect(&self, inst: ValueId) -> bool {
        let insn = self.values[inst].value.as_inst();
        match insn {
            InstructionValue::Branch(_) |
            InstructionValue::Jump(_) |
            InstructionValue::Return(_) |
            InstructionValue::Store(_) => true,
            InstructionValue::Call(c) => {
                // TODO: check if function is pure or const
                // pure: a function does not modify any global memory.
                // const: a function does not read/modify any global memory.
                let cg = self.modref.as_ref().unwrap();
                cg.has_side_effect(&c.func)
            }
            _ => false,
        }
    }

    pub fn func_is_const(&self, func: &str) -> bool {
        let cg = self.modref.as_ref().unwrap();
        cg.is_const(func)
    }

    pub fn inst_bb(&self, inst: ValueId) -> Option<BlockId> {
        let val = &self.values[inst];
        match val.value {
            ValueType::Instruction(_) => self.inst_bb.get(&inst).cloned(),
            _ => None,
        }
    }

    pub fn rebuild_bb_cahce(&mut self, func: &str) {
        let mut bbs = self.funcs[func].bbs.clone();
        bbs.retain(|b| self.blocks.get(*b).is_some());
        self.funcs.get_mut(func).unwrap().bbs = bbs;
    }

    // global value builders
    pub fn undef(&self) -> ValueId {
        self.undef.unwrap()
    }

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

    pub fn const_f32(&mut self, val: f32) -> ValueId {
        let val = ConstantValue::F32(val);
        let val = ValueType::Constant(val);
        let val = Value::new(val);
        self.values.alloc(val)
    }

    pub fn global(&mut self, name: &str, ty: Rc<Type>) -> ValueId {
        let ty = ty;
        let val = ValueType::Global(GlobalValue{ name: name.to_string(), ty });
        let val = Value::new(val);
        self.values.alloc(val)
    }

    // local inst builders
    pub fn alloca(&mut self, ty: Rc<Type>) -> ValueId {
        let ty = ty;
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
        self.add_used_by(ptr, id);
        id
    }

    pub fn load(&mut self, ptr: ValueId) -> ValueId {
        let ptr_val = self.values.get(ptr).unwrap();
        let ty = ptr_val.ty().base_type();
        let inst = LoadInst {
            name: self.gen_local_name(),
            ty,
            ptr,
            use_store: None,
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
            | BinaryOpType::Xor
            | BinaryOpType::And
            | BinaryOpType::Assign => {
                let lhs = self.values.get(lhs).unwrap();
                lhs.ty()
            }
            BinaryOpType::Ne
            | BinaryOpType::Eq
            | BinaryOpType::Lt
            | BinaryOpType::Le
            | BinaryOpType::Gt
            | BinaryOpType::Ge
            | BinaryOpType::LogAnd
            | BinaryOpType::LogOr => Type::i1_type(),
            _ => unimplemented!("BinaryOpType {:?} not implemented", op),
        };
        let inst = BinaryInst {
            lhs,
            rhs,
            op,
            name: self.gen_local_name(),
            ty,
            op_ty: {
                let lhs = self.values.get(lhs).unwrap();
                lhs.ty()
            },
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
        let target_bb = &mut self.blocks[succ];
        target_bb.is_labeled = true;
        let inst = JumpInst { succ };
        let val = ValueType::Instruction(InstructionValue::Jump(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        id
    }

    pub fn unary(&mut self, value: ValueId, op: UnaryOp) -> ValueId {
        let inst = UnaryInst {
            name: self.gen_local_name(),
            value,
            ty: op.dst_ty(),
            op: op,
        };
        let val = ValueType::Instruction(InstructionValue::Unary(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(value, id);
        id
    }

    pub fn phi(&mut self, args: Vec<(ValueId, BlockId)>, ty: Rc<Type>) -> ValueId {
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

    pub fn gep(&mut self, ptr: ValueId, indices: Vec<ValueId>) -> ValueId {
        let ptr_val = self.values.get(ptr).unwrap().clone();
        // let ty = ptr_val.ty().base_type();
        let mut ty = ptr_val.ty();
        for _ in 0..indices.len() {
            ty = ty.base_type();
        }
        let inst = GetElemPtrInst {
            name: self.gen_local_name(),
            ty: Type::ptr_to(ty),
            base_ty: ptr_val.ty().base_type(),
            ptr,
            indices: indices.clone(),
        };
        let val = ValueType::Instruction(InstructionValue::GetElemPtr(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        self.add_used_by(ptr, id);
        for idx in indices {
            self.add_used_by(idx, id);
        }
        id
    }

    // direct call
    pub fn call(&mut self, func: &str, ret: Rc<Type>, args: Vec<ValueId>) -> ValueId {
        let func = func.to_string();
        let inst = CallInst {
            name: if ret.is_void() {"".to_string()} else {self.gen_local_name()},
            func,
            ty: ret,
            args: args.clone(),
        };
        let val = ValueType::Instruction(InstructionValue::Call(inst));
        let val = Value::new(val);
        let id = self.values.alloc(val);
        for arg in args {
            self.add_used_by(arg, id);
        }
        id
    }

    pub fn memphi(&mut self, bind_ptr: ValueId, blk: BlockId) -> ValueId {
        let undef = self.undef();
        let block = &self.blocks[blk];
        let preds: Vec<_> = block.preds
            .iter()
            .map(|&id| (undef, id))
            .collect();
        let inst = MemPhiInst {
            args: preds,
            bind_ptr,
        };
        let val = ValueType::Instruction(InstructionValue::MemPhi(inst));
        let val = Value::new(val);
        self.values.alloc(val)
    }

    pub fn memop(&mut self, load: ValueId) -> ValueId {
        let inst = MemOpInst {
            load,
            after_load: None,
        };
        let val = ValueType::Instruction(InstructionValue::MemOp(inst));
        let val = Value::new(val);
        self.values.alloc(val)
    }
}