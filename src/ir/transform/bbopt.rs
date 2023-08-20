use std::collections::HashSet;

use crate::{
    ir::{structure::TransUnit, value::{InstructionValue, BranchInst, JumpInst, ConstantValue}},
    ctype::BinaryOpType
};

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

    if crate::ARGS.optimize == "999" {
        // recognize switch pattern
        // use crate::ir::export::IrFuncFormatter;
        // eprintln!("before switch merging\n{}", IrFuncFormatter::new(unit, func));
        let mut visited = HashSet::new();
        let mut worklist = vec![unit.funcs[func].entry_bb];
        while let Some(bb) = worklist.pop() {
            if visited.contains(&bb) {
                continue;
            }
            visited.insert(bb);
            
            // scan for switch pattern
            // header:
            //   ...
            //   %cond0 = icmp eq %a, constant0
            //   br %cond0, %bb0, %case1
            // case1:
            //   %cond1 = icmp eq %a, constant1
            //   br %cond1, %bb1, %case2
            // case2:
            //   %cond2 = icmp ne %a, constant2
            //   br %cond2, %case3, %bb2
            // case3:
            //   %cond3 = icmp ne %a, constant3
            //   br %cond3, %default, %bb3 ; <- default is last non-switch bb
            // ...
            let mut targets = vec![];
            let mut next = bb;
            let mut base = None;
            let mut delete = vec![];
            let mut delete_bbs = vec![];

            'switch: loop {
                visited.insert(next);
                if targets.len() == 0 {
                    if unit.bb_inst_count(next) < 2 {
                        break;
                    }
                } else {
                    if unit.bb_inst_count(next) != 2 {
                        break;
                    }
                }
                let last = unit.blocks[next].insts_end.unwrap();
                let last_val = &unit.values[last];
                let (cond, cond_value, br) =
                    if let InstructionValue::Branch(br) = last_val.value.as_inst()
                {
                    let cond = br.cond;
                    let cond_value = &unit.values[br.cond];
                    if cond_value.used_by.len() > 1
                        || !cond_value.value.is_inst()
                        || unit.inst_bb[&last] != unit.inst_bb[&br.cond] {
                        // cond is used by other instruction, we cannot delete it
                        break;
                    }
                    (cond, cond_value, br)
                } else {
                    break;
                };
                let new_next = if let InstructionValue::Binary(bin) = cond_value.value.as_inst() {
                    let lhs = &unit.values[bin.lhs];
                    let rhs = &unit.values[bin.rhs];
                    if lhs.value.is_constant() || !rhs.value.is_constant() {
                        break;
                    }
                    if let Some(b) = base {
                        if bin.lhs != b {
                            break;
                        }
                    } else {
                        base = Some(bin.lhs);
                    }
                    let rhs_literal = match rhs.value.as_constant() {
                        ConstantValue::I32(x) => *x,
                        _ => break,
                    };
                    if bin.op == BinaryOpType::Eq {
                        // check phi inst
                        let target = &unit.blocks[br.succ];
                        let target_first = unit.values[target.insts_start.unwrap()].clone();
                        if let InstructionValue::Phi(_phi) = target_first.value.as_inst() {
                            // keep this bb to distinguish from the other bb if needed
                            for p in &target.preds {
                                if *p == next {
                                    // skip self
                                    continue;
                                }
                                let succ = unit.succ(*p);
                                if succ.into_iter().any(|tpred| tpred == next) {
                                    // abandon
                                    targets.clear();
                                    break 'switch;
                                }
                            }
                        }

                        targets.push((rhs_literal, br.succ));
                        br.fail
                    } else if bin.op == BinaryOpType::Ne {
                        // check phi inst
                        let target = &unit.blocks[br.fail];
                        let target_first = unit.values[target.insts_start.unwrap()].clone();
                        if let InstructionValue::Phi(_phi) = target_first.value.as_inst() {
                            // keep this bb to distinguish from the other bb if needed
                            for p in &target.preds {
                                if *p == next {
                                    // skip self
                                    continue;
                                }
                                let succ = unit.succ(*p);
                                if succ.into_iter().any(|tpred| tpred == next) {
                                    // abandon
                                    targets.clear();
                                    break 'switch;
                                }
                            }
                        }

                        targets.push((rhs_literal, br.fail));
                        br.succ
                    } else {
                        // not eq/ne, cannot be switch pattern
                        break;
                    }
                } else {
                    break;
                };
                // don't delete switch head
                if targets.len() > 1 {
                    delete_bbs.push(next);
                }
                delete.push(cond);
                delete.push(last);
                next = new_next;
            }
            // eprintln!("{targets:?}");

            if targets.len() >= 4 {
                // rewrite as switch
                let head = bb;
                let cond = base.unwrap();
                let default = next;
                for bb in &delete_bbs {
                    for s in unit.succ(*bb) {
                        if let Some(block) = unit.blocks.get_mut(s) {
                            let idx = block.preds
                                .iter()
                                .position(|p| *p == *bb)
                                .unwrap();
                            block.preds.remove(idx);
                            let mut iter = block.insts_start;
                            while let Some(inst) = iter {
                                let insn = &mut unit.values[inst];
                                iter = insn.next;
                                let insn = insn.value.as_inst_mut();
                                if let InstructionValue::Phi(phi) = insn {
                                    phi.args.remove(idx);
                                } else {
                                    break;
                                }
                            }
                        }
                    }
                    unit.blocks.remove(*bb);
                }
                for v in &delete {
                    unit.remove2(*v);
                }
                for (_, target) in targets.iter().skip(1) {
                    let tblk = &mut unit.blocks[*target];
                    if tblk.preds.contains(&head) {
                        continue;
                    }
                    tblk.preds.push(head);
                    let mut iter = tblk.insts_start;
                    let undef = unit.undef();
                    while let Some(inst) = iter {
                        let insn = &mut unit.values[inst];
                        iter = insn.next;
                        let insn = insn.value.as_inst_mut();
                        if let InstructionValue::Phi(phi) = insn {
                            phi.args.push((undef, head));
                        } else {
                            break;
                        }
                    }
                }
                let tblk = &mut unit.blocks[default];
                tblk.preds.push(head);
                let mut iter = tblk.insts_start;
                let undef = unit.undef();
                while let Some(inst) = iter {
                    let insn = &mut unit.values[inst];
                    iter = insn.next;
                    let insn = insn.value.as_inst_mut();
                    if let InstructionValue::Phi(phi) = insn {
                        phi.args.push((undef, head));
                    } else {
                        break;
                    }
                }
                let sw = unit.switch(cond, default, targets);
                unit.insert_at_end(head, sw);
            }

            for s in unit.succ(bb) {
                worklist.push(s);
            }
        }
        
        unit.rebuild_bb_cahce(func);
        // eprintln!("after switch merging\n{}", IrFuncFormatter::new(unit, func));
    }

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
                // eprintln!("optimize const branch: {}", unit.blocks[*bb].name);
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
                    while let Some(vid) = iter {
                        let inst = &mut unit.values[vid];
                        iter = inst.next;
                        let inst = inst.value.as_inst_mut();
                        let orig = if let InstructionValue::Phi(phi) = inst {
                            let orig = phi.args[idx].0;
                            phi.args.remove(idx);
                            orig
                        } else {
                            break;
                        };
                        // maintain usage info, remove phi from its user
                        let value = &mut unit.values[orig];
                        value.used_by.remove(&vid);
                    }
                }
            }
        }

        // remove empty bb
        'bb: for bb in bbs.iter() {
            if matches!(unit.blocks.get(*bb), Some(block) if block.is_entry) {
                continue;
            }
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
                    // eprintln!("optimize empty bb: {}", unit.blocks[*bb].name);
                    let target_first = unit.values[target.insts_start.unwrap()].clone();
                    if let InstructionValue::Phi(_phi) = target_first.value.as_inst() {
                        // keep this bb to distinguish from the other bb if needed
                        for p in &target.preds {
                            if *p == *bb {
                                // skip self
                                continue;
                            }
                            let succ = unit.succ(*p);
                            if succ.into_iter().any(|tpred| tpred == *bb) {
                                continue 'bb;
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
    eprintln!("remove unreachable bb");
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

            let mut iter = unit.blocks[*bb].insts_start;
            while let Some(vid) = iter {
                let inst = &mut unit.values[vid];
                iter = inst.next;
                unit.remove2(vid);
            }

            // eprintln!("remove unreachable bb: {}", unit.blocks[*bb].name);
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
            // eprintln!("remove redundant phi: {}", unit.blocks[*bb].name);
            let mut iter = block.insts_start;
            while let Some(inst) = iter {
                let insn = unit.values[inst].clone();
                iter = insn.next;
                if let InstructionValue::Phi(phi) = insn.value.as_inst() {
                    assert!(phi.args.len() == 1);
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
                        // eprintln!("merge bb: {} with {}", unit.blocks[*bb].name, target.name);
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