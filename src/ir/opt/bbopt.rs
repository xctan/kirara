use std::collections::HashSet;

use crate::ir::{structure::TransUnit, value::{InstructionValue, BranchInst, JumpInst}};

use super::IrPass;

pub struct BasicBlockOptimization;

impl IrPass for BasicBlockOptimization {
    fn run(unit: &mut TransUnit) {
        for k in unit.funcs() {
            bbopt(unit, k.as_str());
        }
    }
}

pub fn bbopt(unit: &mut TransUnit, func: &str) /*-> bool*/ {
    let bbs = unit.funcs[func].bbs.clone();

    let mut changed = true;
    while changed {
        changed = false;

        // br true, bb1, bb2
        // br false, bb1, bb2
        // br cond, bb1, bb1
        for bb in &bbs {
            // bb may be deleted
            let last = 
                match unit.blocks.get(*bb) {
                    Some(block) => block,
                    None => continue,
                }
                .insts_end.unwrap();
            let last_inst = unit.values[last].clone();
            if let InstructionValue::Branch(BranchInst { cond, succ, fail }) 
                = last_inst.value.as_inst()
            {
                let mut deleted = None;
                let cond_value = &unit.values[*cond];
                if cond_value.value.is_constant() {
                    let cond_value = cond_value.value.as_constant().as_i1();
                    if cond_value {
                        deleted = Some(*fail);
                        let jump = unit.jump(*succ);
                        unit.insert_at_end(*bb, jump);
                    } else {
                        deleted = Some(*succ);
                        let jump = unit.jump(*fail);
                        unit.insert_at_end(*bb, jump);
                    }
                } else if *succ == *fail {
                    deleted = Some(*fail);
                    let jump = unit.jump(*succ);
                    unit.insert_at_end(*bb, jump);
                    changed = true;
                }
                if let Some(deleted) = deleted {
                    unit.remove(*bb, last);
                    let deleted_bb = &mut unit.blocks[deleted];
                    deleted_bb.preds.retain(|&x| x != *bb);
                    let mut iter = deleted_bb.insts_start;
                    while let Some(inst) = iter {
                        let inst = &mut unit.values[inst];
                        iter = inst.next;
                        let inst = inst.value.as_inst_mut();
                        if let InstructionValue::Phi(phi) = inst {
                            phi.args.retain(|&(_, x)| x != deleted);
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        // bb has only one last jump: br bb
        'bb: for bb in bbs.iter().skip(1) {
            let last = 
                match unit.blocks.get(*bb) {
                    Some(block) => block,
                    None => continue,
                }
                .insts_start.unwrap();
            let last_inst = unit.values[last].clone();
            if let InstructionValue::Jump(JumpInst { succ }) = last_inst.value.as_inst() {
                // sanity check
                assert!(unit.blocks[*bb].insts_start == unit.blocks[*bb].insts_end);
                if succ != bb {
                    let target = &unit.blocks[*succ];
                    let target_first = unit.values[target.insts_start.unwrap()].clone();
                    if let InstructionValue::Phi(_phi) = target_first.value.as_inst() {
                        for p in &target.preds {
                            let pred = &unit.blocks[*p];
                            let pred_tail = unit.values[pred.insts_end.unwrap()].clone();
                            if let InstructionValue::Branch(br) = pred_tail.value.as_inst() {
                                // keep this bb to distinguish from the other bb
                                if br.succ == *bb || br.fail == *bb {
                                    continue 'bb;
                                }
                            }
                        }
                    }
                    changed = true;
                    let this_preds = unit.blocks[*bb].preds.clone();
                    let target = &mut unit.blocks[*succ];
                    target.preds.retain(|&x| x != *bb);
                    for p in &this_preds {
                        target.preds.push(*p);
                    }
                    for p in &this_preds {
                        unit.succ_mut(*p)
                            .into_iter()
                            .filter(|x| **x == *bb)
                            .for_each(|x| *x = *succ);
                    }
                    let mut iter = unit.blocks[*succ].insts_start;
                    while let Some(inst) = iter {
                        let inst = &mut unit.values[inst];
                        iter = inst.next;
                        let inst = inst.value.as_inst_mut();
                        if let InstructionValue::Phi(phi) = inst {
                            let (idx, (v, _)) = phi.args
                                .iter()
                                .enumerate()
                                .find(|&(_, (_, x))| *x == *bb)
                                .unwrap();
                            let v = *v;
                            phi.args.remove(idx);
                            for p in &this_preds {
                                phi.args.push((v, *p));
                            }
                        } else {
                            break;
                        }
                    }
                    unit.blocks.remove(*bb);
                }
            }
        }
    }

    // some basic blocks are deleted, so remove invalid reference here
    unit.rebuild_bb_cahce(func);

    // remove unreachable basic blocks
    // mark
    let mut visited = HashSet::new();
    let mut worklist = vec![unit.funcs[func].entry_bb];
    while let Some(bb) = worklist.pop() {
        if visited.contains(&bb) {
            continue;
        }
        visited.insert(bb);
        for s in unit.succ(bb) {
            worklist.push(s);
        }
    }
    // sweep
    let bbs = unit.funcs[func].bbs.clone();
    for bb in &bbs {
        if !visited.contains(bb) {
            for s in unit.succ(*bb) {
                let mut iter = 
                    match unit.blocks.get(s) {
                        Some(block) => block,
                        None => continue,
                    }
                    .insts_start;
                while let Some(inst) = iter {
                    let inst = &mut unit.values[inst];
                    iter = inst.next;
                    let inst = inst.value.as_inst_mut();
                    if let InstructionValue::Phi(phi) = inst {
                        phi.args.retain(|&(_, x)| x != s);
                    } else {
                        break;
                    }
                }
            }
            unit.blocks.remove(*bb);
        }
    }
}