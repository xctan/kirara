use std::collections::HashSet;

use crate::ir::structure::TransUnit;

use super::{IrPass, IrFuncPass};

pub struct DeadCodeElimination;

impl IrPass for DeadCodeElimination {
    fn run(&self, unit: &mut TransUnit) {
        for k in unit.funcs() {
            dce(unit, k.as_str());
        }
    }
}

impl IrFuncPass for DeadCodeElimination {
    fn run_on_func(&self, unit: &mut TransUnit, func: &str) {
        dce(unit, func);
    }
}

pub fn dce(unit: &mut TransUnit, func: &str) {
    let bbs = unit.funcs[func].bbs.clone();

    let mut visited = HashSet::new();
    let mut worklist = vec![];
    for bb in &bbs {
        let mut iter = unit.blocks[*bb].insts_start;
        while let Some(vid) = iter {
            if unit.has_side_effect(vid) {
                worklist.push(vid);
            }
            iter = unit.values[vid].next;
        }
    }
    while let Some(vid) = worklist.pop() {
        if visited.contains(&vid) {
            continue;
        }
        visited.insert(vid);
        for op in unit.get_operands(vid) {
            let op_val = &unit.values[op];
            if op_val.value.is_inst() {
                worklist.push(op);
            }
        }
    }

    for bb in &bbs {
        let mut iter = unit.blocks[*bb].insts_start;
        while let Some(vid) = iter {
            iter = unit.values[vid].next;
            if !visited.contains(&vid) {
                unit.remove(*bb, vid);
            }
        }
    }
}
