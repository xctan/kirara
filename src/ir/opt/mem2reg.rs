//! Memory to register promotion.
//! 

use std::{collections::{HashMap, HashSet}, rc::Rc};

use crate::{
    ir::{
        structure::TransUnit, cfg::compute_df,
        value::{InstructionValue, AllocaInst, StoreInst, ValueId, LoadInst, PhiInst, GetElemPtrInst, ReturnInst, CallInst}
    },
    ctype::{TypeKind, Type, TypePtrCompare}
};

use super::IrPass;

pub struct Mem2Reg;

impl IrPass for Mem2Reg {
    fn run(&self, unit: &mut TransUnit) {
        for k in unit.funcs() {
            mem2reg(unit, k.as_str());
        }
    }
}

fn mem2reg(unit: &mut TransUnit, func: &str) {
    // obtain dominance frontier
    compute_df(unit, func);
    let func = unit.funcs.get(func).unwrap().clone();

    let mut addr_used = HashSet::new();
    for bb in &func.bbs {
        let block = unit.blocks.get(*bb).unwrap();
        let mut inst = block.insts_start;
        while let Some(insn_id) = inst {
            let insn = unit.values.get(insn_id).unwrap().clone();
            inst = insn.next;

            match insn.value.as_inst() {
                InstructionValue::GetElemPtr(GetElemPtrInst { ptr, .. }) => {
                    addr_used.insert(*ptr);
                },
                InstructionValue::Store(StoreInst { value, .. }) => {
                    addr_used.insert(*value);
                },
                InstructionValue::Return(ReturnInst { value, .. }) => {
                    // ??? will the local address be returned?
                    if let Some(value) = value {
                        addr_used.insert(*value);
                    }
                },
                InstructionValue::Call(CallInst { args, .. }) => {
                    for arg in args {
                        addr_used.insert(*arg);
                    }
                },
                _ => (),
            }
        }
    }

    let mut alloca_ids = HashMap::new();
    let mut allocas = Vec::new();
    for bb in &func.bbs {
        let block = unit.blocks.get(*bb).unwrap();
        let mut inst = block.insts_start;
        while let Some(insn_id) = inst {
            let insn = unit.values.get(insn_id).unwrap().clone();
            inst = insn.next;
            if addr_used.contains(&insn_id) {
                continue;
            }
            if let InstructionValue::Alloca(AllocaInst{alloc_ty, ..}) = insn.value.as_inst() {
                let ty = alloc_ty.clone();
                if matches!(ty.kind, TypeKind::I32 | TypeKind::Ptr(_) | TypeKind::F32) {
                    let alloca_id = alloca_ids.len();
                    alloca_ids.insert(insn_id, alloca_id);
                    allocas.push(ty);
                }
            }
        }
    }

    let mut alloca_defs = vec![vec![]; allocas.len()];
    for bb in &func.bbs {
        let block = unit.blocks.get(*bb).unwrap();
        let mut inst = block.insts_start;
        while let Some(insn_id) = inst {
            let insn = unit.values.get(insn_id).unwrap();
            if let InstructionValue::Store(StoreInst{ptr, ..}) = insn.value.as_inst() {
                alloca_ids.entry(*ptr).and_modify(|id| {
                    alloca_defs[*id].push(bb);
                });
            }
            inst = insn.next;
        }
    }

    // stage 1: phi insertion
    let mut visited = HashSet::new();
    let mut undef = UndefValue::new();
    let mut worklist = Vec::new();
    let mut phis = HashMap::new();
    for (id, ty) in allocas.iter().enumerate() {
        visited.clear();
        alloca_defs[id].iter().for_each(|bb| worklist.push(**bb));
        while let Some(bb) = worklist.pop() {
            let df = unit.blocks.get(bb).unwrap().df.clone();

            // println!("bb {}", unit.blocks.get(bb).unwrap().name);
            // let idom = unit.blocks.get(bb).unwrap().idom.unwrap();
            // println!("  idom: {}", unit.blocks.get(idom).unwrap().name);
            // print!("  df: {{");
            // let mut i = 0;
            // for d in &df {
            //     if i != 0 {
            //         print!(", ");
            //     }
            //     print!("{}", unit.blocks.get(*d).unwrap().name);
            //     i += 1;
            // }
            // println!("}}");

            for y in df {
                if !visited.contains(&y) {
                    visited.insert(y);
                    worklist.push(y);

                    let ud = undef.obtain(unit, (*ty).clone());
                    let preds = unit.blocks.get(y).unwrap().preds.clone();
                    let args = preds.into_iter()
                        .map(|pred| (ud, pred))
                        .collect();
                    let phi = unit.phi(args, ty.clone());
                    phis.insert(phi, id);
                    unit.insert_at_begin(y, phi);
                }
            }
        }
    }

    // stage 2: variable renaming
    let mut visited = HashSet::new();
    let mut worklist = Vec::new();
    let values: Vec<_> = allocas.iter()
        .map(|ty| undef.obtain(unit, (*ty).clone()))
        .collect();
    visited.insert(func.entry_bb);
    worklist.push((func.entry_bb, values));
    while let Some((bb, mut values)) = worklist.pop() {
        let block = unit.blocks.get(bb).unwrap();
        // println!("bb {}", block.name);
        let mut inst = block.insts_start;
        while let Some(insn_id) = inst {
            let insn = unit.values.get(insn_id).unwrap().clone();

            if alloca_ids.contains_key(&insn_id) {
                // this is an alloca instruction promoted to register
                unit.remove(bb, insn_id);
            } else if let InstructionValue::Load(LoadInst{ptr, ..}) = insn.value.as_inst() {
                alloca_ids.entry(*ptr).and_modify(|id| {
                    let val = values[*id];
                    unit.replace(insn_id, val);
                    unit.remove(bb, insn_id);
                });
            } else if let InstructionValue::Store(StoreInst { ptr, value }) = insn.value.as_inst() {
                alloca_ids.entry(*ptr).and_modify(|id| {
                    values[*id] = *value;
                    unit.remove(bb, insn_id);
                });
            } else if let InstructionValue::Phi(_) = insn.value.as_inst() {
                if let Some(id) = phis.get(&insn_id) {
                    // only phi's placed at previous stage define a variable
                    values[*id] = insn_id;
                }
            }

            // println!("{:?}", values);

            inst = insn.next;
        }

        for succ in unit.succ(bb) {
            if !visited.contains(&succ) {
                visited.insert(succ);
                worklist.push((succ, values.clone()));
            }
            let block = unit.blocks.get(succ).unwrap().clone();
            let mut inst = block.insts_start;
            while let Some(insn_id) = inst {
                let insn = unit.values.get(insn_id).unwrap().clone();

                if phis.contains_key(&insn_id) {
                    let mut index = 0;
                    for (idx, pred) in block.preds.iter().enumerate() {
                        if *pred == bb {
                            index = idx;
                            break;
                        }
                    }
                    let mut phi = unit.values.get_mut(insn_id).unwrap().value.as_inst().clone();
                    if let InstructionValue::Phi(PhiInst{args, ..}) = &mut phi {
                        // FIXME: track use-def correctly
                        args[index].0 = values[phis[&insn_id]];
                        let def = unit.values.get_mut(values[phis[&insn_id]]).unwrap();
                        def.used_by.insert(insn_id);
                    } else {
                        unreachable!();
                    }
                    let phi_mut = unit.values.get_mut(insn_id).unwrap().value.as_inst_mut();
                    *phi_mut = phi;
                } else {
                    // phi's must be placed at the beginning of the block, so we can stop here
                    break;
                }
                
                inst = insn.next;
            }
        }
    }
}

struct UndefValue {
    values: Vec<(Rc<Type>, ValueId)>,
}

impl UndefValue {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
        }
    }

    pub fn obtain(&mut self, unit: &mut TransUnit, ty: Rc<Type>) -> ValueId {
        for (t, v) in &self.values {
            if ty.is_same_as(t) {
                return *v;
            }
        }
        let val = match ty.kind {
            TypeKind::I32 => unit.undef(),
            TypeKind::F32 => unit.undef(),
            TypeKind::Ptr(_) => unit.undef(),
            _ => unimplemented!(),
        };
        self.values.push((ty, val));
        val
    }
}