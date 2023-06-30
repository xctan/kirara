use std::collections::HashSet;

use super::{builder::TransUnit, structure::BlockId};

fn reverse_post_order(unit: &mut TransUnit, entry_bb: BlockId) -> Vec<BlockId> {
    let mut stack = Vec::new();
    let mut visited = HashSet::new();
    let mut result = Vec::new();
    stack.push((entry_bb, 0));
    visited.insert(entry_bb);
    while !stack.is_empty() {
        let (top_id, index) = stack.pop().unwrap();
        let succ = unit.succ(top_id);
        if index == succ.len() {
            result.push(top_id);
            continue;
        }
        stack.push((top_id, index + 1));
        let target = succ[succ.len() - 1 - index];
        if !visited.contains(&target) {
            visited.insert(target);
            stack.push((target, 0));
        }
    }
    let mut counter = 1usize;
    let mut ordering = Vec::new();
    while !result.is_empty() {
        let bb = result.pop().unwrap();
        ordering.push(bb);
        let bb = unit.blocks.get_mut(bb).unwrap();
        bb.rpo = counter;
        counter += 1;
    }

    ordering
}

fn intersect(unit: &TransUnit, i: BlockId, j: BlockId) -> BlockId {
    let mut finger1 = i;
    let mut finger2 = j;
    while finger1 != finger2 {
        let mut finger1_order = unit.blocks.get(finger1).unwrap().rpo;
        let mut finger2_order = unit.blocks.get(finger2).unwrap().rpo;
        while finger1_order > finger2_order {
            if let Some(finger) = unit.blocks.get(finger1).unwrap().idom {
                finger1 = finger;
                finger1_order = unit.blocks.get(finger1).unwrap().rpo;
            } else {
                return finger2;
            }
        }
        while finger2_order > finger1_order {
            if let Some(finger) = unit.blocks.get(finger2).unwrap().idom {
                finger2 = finger;
                finger2_order = unit.blocks.get(finger2).unwrap().rpo;
            } else {
                return finger1;
            }
        }
    }

    finger1
}

/// Compute dominance frontier
pub fn compute_df(unit: &mut TransUnit, func: &str) {
    let func = unit.funcs.get(func).unwrap().clone();
    let entry_bb = func.entry_bb;
    let entry = unit.blocks.get_mut(entry_bb).unwrap();
    entry.idom = Some(entry_bb);

    let ordering = reverse_post_order(unit, entry_bb);

    // calculate immediate dominator
    let mut changed = true;
    while changed {
        changed = false;
        for node in &ordering {
            let n = unit.blocks.get(*node).unwrap();
            if !n.preds.is_empty() {
                let mut new_idom = unit.blocks.get(*n.preds.first().unwrap()).unwrap().idom.unwrap();
                for pred in n.preds.iter().skip(1) {
                    new_idom = intersect(unit, *pred, new_idom);
                }
                if n.idom != Some(new_idom) {
                    let n = unit.blocks.get_mut(*node).unwrap();
                    n.idom = Some(new_idom);
                    changed = true;
                }
            }
        }
    }

    // calculate dominance frontier (figure 9.10)
    for n in &ordering {
        let node = unit.blocks.get(*n).unwrap().clone();
        if node.preds.len() > 1 {
            for p in &node.preds {
                let mut runner_id = *p;
                while runner_id != node.idom.unwrap() {
                    let runner = unit.blocks.get_mut(runner_id).unwrap();
                    runner.df.push(*n);
                    runner_id = runner.idom.unwrap();
                }
            }
        }
    }
    for n in &ordering {
        let node = unit.blocks.get_mut(*n).unwrap();
        node.df.sort();
        node.df.dedup();
    }
}