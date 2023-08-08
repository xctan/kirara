use std::collections::{HashMap, HashSet};

use super::{value::{ValueId, InstructionValue}, structure::{BlockId, TransUnit}};

pub struct MemorySSA {
    // which version is used
    memuse: HashMap<ValueId, u32>,
    // which version is killed
    memdef: HashMap<ValueId, u32>,
    // the version is defined by which MemoryDef/MemoryPhi
    def_by: HashMap<u32, MemoryDef>,
    // defs: HashMap<MemoryDef, u32>,
    // merge memory defs
    memphi: HashMap<BlockId, Vec<(BlockId, u32)>>,
}

// a mix of MemoryDef and MemoryPhi
#[derive(Hash, PartialEq, Eq)]
pub enum MemoryDef {
    Instruction(ValueId),
    MemoryPhi(BlockId),
}

const LIVE_ON_ENTRY: u32 = 0;

impl MemorySSA {
    pub fn build(unit: &TransUnit, func: &str) -> MemorySSA {
        // need to obtain dominance frontier before building memory ssa

        let f = unit.funcs[func].clone();

        // stage 1
        let mut memphi = HashMap::new();
        let mut visited = HashSet::new();
        let mut worklist = vec![];
        for bb in &f.bbs {
            let mut iter = unit.blocks[*bb].insts_start;
            while let Some(vid) = iter {
                let value = &unit.values[vid];
                iter = value.next;
                match value.value.as_inst() {
                    InstructionValue::Store(_) => {
                        worklist.push(*bb);
                    },
                    InstructionValue::Call(c) => {
                        let _ = c;
                        // todo: check if the function has side effect (purity)
                        worklist.push(*bb);
                    }
                    _ => {}
                }
            }
        }
        while let Some(bb) = worklist.pop() {
            // somewhere inside this bb there is a store-like instruction
            let df = &unit.blocks[bb].df;
            for y in df {
                if !visited.contains(y) {
                    visited.insert(*y);
                    worklist.push(*y);

                    let preds = &unit.blocks[*y].preds;
                    let phi: Vec<_> = preds
                        .iter()
                        .map(|x| (*x, LIVE_ON_ENTRY))
                        .collect();
                    memphi.insert(*y, phi);
                }
            }
        }

        // stage 2
        let mut counter = 1u32;
        let mut visited = HashSet::new();
        let mut worklist = Vec::new();
        // let mut defs = HashMap::new();
        let mut def_by = HashMap::new();
        let mut memuse = HashMap::new();
        let mut memdef = HashMap::new();
        worklist.push((f.entry_bb, LIVE_ON_ENTRY));
        while let Some((w, mut v)) = worklist.pop() {
            if visited.contains(&w) {
                continue;
            }
            visited.insert(w);

            if memphi.contains_key(&w) {
                let id = counter;
                counter += 1;
                v = id;
                // defs.insert(MemoryDef::MemoryPhi(w), id);
                def_by.insert(id, MemoryDef::MemoryPhi(w));
            }

            let mut iter = unit.blocks[w].insts_start;
            while let Some(vid) = iter {
                let value = &unit.values[vid];
                iter = value.next;

                match value.value.as_inst() {
                    InstructionValue::Load(_) => {
                        memuse.insert(vid, v);
                    },
                    InstructionValue::Store(_) | InstructionValue::Call(_) => {
                        let id = counter;
                        counter += 1;
                        // kill old version
                        memdef.insert(vid, v);
                        v = id;
                        // defs.insert(MemoryDef::Instruction(vid), id);
                        def_by.insert(id, MemoryDef::Instruction(vid));
                    },
                    _ => {}
                }
            }

            for succ in unit.succ(w) {
                worklist.push((succ, v));

                let phi = memphi.get_mut(&succ).unwrap();
                phi.iter_mut().for_each(|(x, y)| {
                    if *x == w {
                        *y = v;
                    }
                });
            }
        }

        MemorySSA {
            memuse,
            memdef,
            def_by,
            memphi,
        }
    }
}