use std::collections::HashSet;

use crate::ir::{structure::TransUnit, value::{InstructionValue, BranchInst, JumpInst}};

use super::IrPass;

pub struct BasicBlockOptimization;

impl IrPass for BasicBlockOptimization {
    fn run(&self, unit: &mut TransUnit) {
        for k in unit.funcs() {
            bbopt(unit, k.as_str());
        }
    }
}

pub fn bbopt(unit: &mut TransUnit, func: &str) -> bool {
    let bbs = unit.funcs[func].bbs.clone();

    let mut changed = true;
    while changed {
        changed = false;

        // simplify branch instruction
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
                    // deleted_bb.preds.retain(|&x| x != *bb);
                    let idx = deleted_bb.preds
                        .iter()
                        .position(|&x| x == *bb)
                        .unwrap();
                    deleted_bb.preds.remove(idx);
                    let mut iter = deleted_bb.insts_start;
                    while let Some(inst) = iter {
                        let inst = &mut unit.values[inst];
                        iter = inst.next;
                        let inst = inst.value.as_inst_mut();
                        if let InstructionValue::Phi(phi) = inst {
                            phi.args.remove(idx);
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        // remove empty bb
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
                    let index = target.preds
                        .iter()
                        .position(|x| x == bb)
                        .unwrap();
                    target.preds.remove(index);
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
                            let v = phi.args[index].0;
                            phi.args.remove(index);
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
                // let mut iter = 
                //     match unit.blocks.get(s) {
                //         Some(block) => block,
                //         None => continue,
                //     }
                //     .insts_start;
                let block = match unit.blocks.get_mut(s) {
                    Some(block) => block,
                    None => continue,
                };
                let idx = block.preds
                    .iter()
                    .position(|x| *x == *bb)
                    .unwrap();
                block.preds.remove(idx);
                let mut iter = block.insts_start;
                while let Some(inst) = iter {
                    let inst = &mut unit.values[inst];
                    iter = inst.next;
                    let inst = inst.value.as_inst_mut();
                    if let InstructionValue::Phi(phi) = inst {
                        phi.args.remove(idx);
                    } else {
                        break;
                    }
                }
            }
            unit.blocks.remove(*bb);
        }
    }

    // some basic blocks are deleted, so remove invalid reference here too
    unit.rebuild_bb_cahce(func);

    let mut inst_changed = false;

    // remove redundant phi
    let bbs = unit.funcs[func].bbs.clone();
    for bb in &bbs {
        let block = &unit.blocks[*bb];
        if block.preds.len() == 1 {
            let mut iter = block.insts_start;
            while let Some(inst) = iter {
                let insn = unit.values[inst].clone();
                iter = insn.next;
                if let InstructionValue::Phi(phi) = insn.value.as_inst() {
                    let v = phi.args[0].0;
                    unit.replace(inst, v);
                    unit.remove(*bb, inst);
                    inst_changed = true;
                } else {
                    break;
                }
            }
        }
    }

    // merge basic blocks
    for bb in &bbs {
        loop {
            if let Some(block) = unit.blocks.get(*bb) {
                let block_last = block.insts_end.unwrap();
                let last = unit.values[block_last].clone();
                if let InstructionValue::Jump(br) = last.value.as_inst() {
                    let target = &unit.blocks[br.succ];
                    if target.preds.len() == 1 {
                        let mut iter = target.insts_start;
                        while let Some(inst) = iter {
                            let insn = unit.values[inst].clone();
                            iter = insn.next;
                            unit.insert_before(*bb, inst, block_last);
                        }
                        unit.remove(*bb, block_last);
                        for s in unit.succ(br.succ) {
                            let block = &mut unit.blocks[s];
                            let idx = block.preds
                                .iter()
                                .position(|p| *p == br.succ)
                                .unwrap();
                            block.preds[idx] = *bb;
                            let mut iter = block.insts_start;
                            while let Some(inst) = iter {
                                let insn = &mut unit.values[inst];
                                iter = insn.next;
                                let insn = insn.value.as_inst_mut();
                                if let InstructionValue::Phi(phi) = insn {
                                    phi.args[idx].1 = *bb;
                                } else {
                                    break;
                                }
                            }
                        }
                        unit.blocks.remove(br.succ);
                        continue;
                    }
                }
            }
            break;
        }
    }

    // some basic blocks are deleted, so remove invalid reference here too
    unit.rebuild_bb_cahce(func);

    inst_changed
}