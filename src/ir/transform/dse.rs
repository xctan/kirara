use std::collections::HashSet;

use crate::ir::{structure::TransUnit, value::InstructionValue};

use super::IrFuncPass;

pub struct DeadStoreElimination;

impl IrFuncPass for DeadStoreElimination {
    fn run_on_func(&self, unit: &mut TransUnit, func: &str) {
        dse(unit, func);
    }
}

pub fn dse(unit: &mut TransUnit, func: &str) {
    // we have memdep info!
    let memdep = unit.memdep.remove(func).unwrap();

    let mut dead_store = HashSet::new();
    for (_, ra) in &memdep {
        // store definition is killed by store/call but not memphi
        ra.killed_by
            .iter()
            .filter(|(s, k)| {
                // this store shouldn't outlive func
                k.len() > 0
                && k
                    .iter()
                    .all(|u| {
                        let sval = &unit.values[**s];
                        let uvalue = &unit.values[*u].value.as_inst();
                        // is it a store inst?
                        sval.value.is_inst()
                        && matches!(
                            (sval.value.as_inst(), uvalue),
                            (InstructionValue::Store(a), InstructionValue::Store(b))
                            if a.ptr == b.ptr
                        )
                    })
            })
            .for_each(|(s, _)| {
                dead_store.insert(*s);
            })
    }

    let bbs = unit.funcs[func].bbs.clone();

    // bail out used store
    for bb in &bbs {
        let mut iter = unit.blocks[*bb].insts_start;
        while let Some(vid) = iter {
            let inst = unit.values[vid].clone();
            iter = inst.next;

            match inst.value.as_inst() {
                InstructionValue::Load(l) => {
                    dead_store.remove(&l.use_store.unwrap());
                },
                _ => {}
            }
        }
    }

    // remove dead store
    for bb in &bbs {
        let mut iter = unit.blocks[*bb].insts_start;
        while let Some(vid) = iter {
            let inst = unit.values[vid].clone();
            iter = inst.next;

            if dead_store.contains(&vid) {
                unit.remove2(vid);
            }
        }
    }

    unit.memdep.insert(func.to_owned(), memdep);
}