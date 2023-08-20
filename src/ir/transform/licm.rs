//! Move constant stores to the beginning of the loop.

use std::{rc::Rc, cell::RefCell, collections::{HashSet, HashMap}};

use crate::ir::{
    structure::{TransUnit, BasicBlock},
    value::{InstructionValue, ValueType, ValueId, Value, ValueTrait, ConstantValue, calculate_used_by},
    cfg::Loop
};

use super::{IrFuncPass, IrPass};

pub struct LoopInvariantCodeMotion;

impl IrPass for LoopInvariantCodeMotion {
    fn run(&self, unit: &mut TransUnit) {
        let funcs = unit.funcs.keys().cloned().collect::<Vec<_>>();
        for func in &funcs {
            self.run_on_func(unit, func);
        }
    }
}

impl IrFuncPass for LoopInvariantCodeMotion {
    fn run_on_func(&self, unit: &mut TransUnit, func: &str) {
        store_motion(unit, func);
    }
}

pub fn store_motion(unit: &mut TransUnit, func: &str) {
    let info = unit.loopinfo.get(func).unwrap().clone();

    for l in &info.loops {
        // while_cond
        let header = l.borrow().blocks[0];
        // find while_body
        let mut succ = unit.succ(header);
        let old_succ = succ.clone();
        succ.retain(|&x| l.borrow().blocks.contains(&x));
        let body = succ[0];
        eprintln!("examining body entry bb {}", unit.blocks[body].name);

        let mut candidates = HashSet::new();
        let mut iter = unit.blocks[body].insts_start;
        while let Some(vid) = iter {
            let inst = unit.values[vid].clone();
            iter = inst.next;
            let inst = inst.value.as_inst();
            if let InstructionValue::Store(s) = inst {
                let orig = unit.get_base_object(s.ptr);
                let ptr_is_loop_invariant =
                    unit.is_loop_invariant(s.ptr, l);
                let val_is_loop_invariant =
                    unit.is_loop_invariant(s.value, l);
                if ptr_is_loop_invariant && val_is_loop_invariant {
                    candidates.insert(orig);

                    let orig_value = &unit.values[orig];
                    eprintln!("{}: {} is possibly loop invariant", func, orig_value.name());
                }
            }
        }
        // scan for clobbering stores,
        // i.e. stores outside the entry of loop body
        let mr = unit.modref.as_ref().unwrap();
        for bb in &l.borrow().blocks {
            if bb == &header {
                continue;
            }
            let mut iter = unit.blocks[*bb].insts_start;
            while let Some(vid) = iter {
                let inst = unit.values[vid].clone();
                iter = inst.next;
                let inst = inst.value.as_inst();
                if let InstructionValue::Call(call) = inst {
                    candidates.retain(|c| {
                        !unit.call_may_alias(*c, call.clone(), mr)
                    })
                }
                // tail call at loop body entry???
            }
        }
        for bb in &l.borrow().blocks {
            if bb == &header || bb == &body {
                continue;
            }
            let mut iter = unit.blocks[*bb].insts_start;
            while let Some(vid) = iter {
                let inst = unit.values[vid].clone();
                iter = inst.next;
                let inst = inst.value.as_inst();
                if let InstructionValue::Store(s) = inst {
                    let orig = unit.get_base_object(s.ptr);
                    candidates.remove(&orig);
                }
            }
        }

        for c in &candidates {
            eprintln!("{}: {} is loop invariant", func, unit.values[*c].name());
        }

        // split loop header and move stores
        if candidates.len() > 0 {
            // clone loop header
            let block = BasicBlock::new("999".to_owned());
            let new_header = unit.blocks.alloc(block);
            unit.funcs.get_mut(func).unwrap().bbs.push(new_header);
            let mut valuemap = HashMap::new();
            let bbmap = HashMap::new();
            let undef = ConstantValue::Undef;
            let undef = ValueType::Constant(undef);
            let undef = Value::new(undef);
            let mut iter = unit.blocks[header].insts_start;
            while let Some(vid) = iter {
                let inst = unit.values[vid].clone();
                iter = inst.next;
                let newid = unit.values.alloc(undef.clone());
                valuemap.insert(vid, newid);
                unit.insert_at_end(new_header, newid);
            }
            let mut iter = unit.blocks[header].insts_start;
            while let Some(vid) = iter {
                let inst = unit.values[vid].clone();
                iter = inst.next;
                let inst = inst.value.as_inst();
                let inner = unit.clone_inst(inst, &valuemap, &bbmap);
                let dstvid = valuemap[&vid];
                let dstvalue = unit.values.get_mut(dstvid).unwrap();
                dstvalue.value = ValueType::Instruction(inner);
            }

            // create and redirect to new entry block
            let header_block = unit.blocks[header].clone();
            let new_preds = header_block.preds
                .iter()
                .filter(|&x| !l.borrow().blocks.contains(x))
                .map(|&x| x)
                .collect::<Vec<_>>();
            let new_header_block = unit.blocks.get_mut(new_header).unwrap();
            new_header_block.preds = new_preds.clone();
            let mut iter = new_header_block.insts_start;
            while let Some(vid) = iter {
                let inst = unit.values.get_mut(vid).unwrap();
                iter = inst.next;
                let inst = inst.value.as_inst_mut();
                if let InstructionValue::Phi(phi) = inst {
                    phi.args.retain(|&(_, bb)| new_preds.contains(&bb));
                } else {
                    break;
                }
            }
            for pred in &new_preds {
                let succ_mut = unit.succ_mut(*pred);
                for bb in succ_mut {
                    if bb == &header {
                        *bb = new_header;
                    }
                }
            }

            // update old entry block, remove extra preds
            let header_block = unit.blocks.get_mut(header).unwrap();
            header_block.preds.retain(|&x| l.borrow().blocks.contains(&x));
            let mut iter = header_block.insts_start;
            while let Some(vid) = iter {
                let inst = unit.values.get_mut(vid).unwrap();
                iter = inst.next;
                let inst = inst.value.as_inst_mut();
                if let InstructionValue::Phi(phi) = inst {
                    phi.args.retain(|&(_, bb)| l.borrow().blocks.contains(&bb));
                } else {
                    break;
                }
            }

            // move stores
            let mut iter = unit.blocks[body].insts_start;
            while let Some(vid) = iter {
                let inst = unit.values[vid].clone();
                iter = inst.next;
                let inst = inst.value.as_inst();
                if let InstructionValue::Store(s) = inst {
                    let orig = unit.get_base_object(s.ptr);
                    if candidates.contains(&orig) {
                        unit.takeout(vid);
                        unit.insert_before_end(new_header, vid);
                    }
                }
            }

            // merge definition of two headers
            for s in old_succ.iter() {
                let mut conv = BasicBlock::new("998".to_owned());
                conv.preds = vec![header, new_header];
                let conv = unit.blocks.alloc(conv);
                unit.funcs.get_mut(func).unwrap().bbs.push(conv);
                // valuemap: old -> new
                //   phimap: old -> phi
                let mut phimap = HashMap::new();
                for (old, new) in &valuemap {
                    let oldvalue = &unit.values[*old];
                    if !oldvalue.ty().is_void() {
                        let ty = oldvalue.ty();
                        let args = vec![
                            (*old, header),
                            (*new, new_header),
                        ];
                        let phi = unit.phi(args, ty);
                        phimap.insert(*old, phi);
                        unit.insert_at_begin(conv, phi);
                    }
                }
                let jump = unit.jump(*s);
                unit.insert_at_end(conv, jump);

                for smut in unit.succ_mut(header) {
                    if smut == s {
                        *smut = conv;
                    }
                }
                for smut in unit.succ_mut(new_header) {
                    if smut == s {
                        *smut = conv;
                    }
                }

                let sblock = unit.blocks.get_mut(*s).unwrap();
                let idx = sblock.preds
                    .iter()
                    .position(|&x| x == header)
                    .unwrap();
                sblock.preds[idx] = conv;
                let worklist = sblock.dom.clone();
                let mut iter = sblock.insts_start;
                while let Some(vid) = iter {
                    let inst = unit.values.get_mut(vid).unwrap();
                    iter = inst.next;
                    let inst = inst.value.as_inst_mut();
                    if let InstructionValue::Phi(phi) = inst {
                        phi.args[idx].1 = conv;
                    } else {
                        break;
                    }
                }
                for bb in worklist {
                    let mut iter = unit.blocks[bb].insts_start;
                    while let Some(vid) = iter {
                        let inst = unit.values.get(vid).unwrap();
                        iter = inst.next;
                        for opr in unit.get_operands_mut(vid) {
                            if phimap.contains_key(opr) {
                                *opr = phimap[opr];
                            }
                        }
                    }
                }
            }

            // giveup
            calculate_used_by(unit, func);
        }
    }
}

impl TransUnit {
    fn is_loop_invariant(&self, val: ValueId, l: &Rc<RefCell<Loop>>) -> bool {
        match &self.values[val].value {
            ValueType::Instruction(_) => {
                let bb = self.inst_bb[&val];
                !l.borrow().blocks.contains(&bb)
            }
            _ => true,
        }
    }
}