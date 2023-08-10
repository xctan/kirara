use std::collections::{HashMap, HashSet};

use super::{value::{ValueId, InstructionValue, ValueType, CallInst}, structure::{BlockId, TransUnit}};

pub struct MemoryDependency {
    // which version is used
    memuse: HashMap<ValueId, u32>,
    // which version is killed
    memdef: HashMap<ValueId, u32>,
    // the version is defined by which MemoryDef/MemoryPhi
    def_by: HashMap<u32, MemoryDef>,
    pub store_defs: HashMap<ValueId, u32>,
    // merge memory defs
    memphi: HashMap<BlockId, Vec<(BlockId, u32)>>,

    results: HashMap<ValueId, u32>,
    pub mr: ModRef,
}

// a mix of MemoryDef and MemoryPhi
#[derive(Hash, PartialEq, Eq)]
pub enum MemoryDef {
    Instruction(ValueId),
    MemoryPhi(BlockId),
}

const LIVE_ON_ENTRY: u32 = 0;

impl MemoryDependency {
    pub fn build(unit: &TransUnit, func: &str, mr: &ModRef) -> MemoryDependency {
        // need to obtain dominance frontier before building memory ssa

        let f = unit.funcs[func].clone();

        // stage 1
        let mut memphi = HashMap::new();
        let mut worklist = vec![];
        for bb in &f.bbs {
            let mut iter = unit.blocks[*bb].insts_start;
            while let Some(vid) = iter {
                let value = &unit.values[vid];
                iter = value.next;
                match value.value.as_inst() {
                    // FIXME: check user ptr
                    // FIXME: check user ptr
                    // FIXME: check user ptr
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
        let mut visited = HashSet::new();
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
        let mut store_defs = HashMap::new();
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
                // defs.insert(w, id);
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
                    InstructionValue::Store(_) => {
                        let id = counter;
                        counter += 1;
                        // kill old version
                        memdef.insert(vid, v);
                        v = id;
                        store_defs.insert(vid, id);
                        def_by.insert(id, MemoryDef::Instruction(vid));
                    },
                    InstructionValue::Call(_) => {
                        // todo: check if the function is pure
                        let id = counter;
                        counter += 1;
                        // kill old version
                        memdef.insert(vid, v);
                        v = id;
                        store_defs.insert(vid, id);
                        def_by.insert(id, MemoryDef::Instruction(vid));
                    },
                    _ => {}
                }
            }

            for succ in unit.succ(w) {
                worklist.push((succ, v));

                if memphi.contains_key(&succ) {
                    let phi = memphi.get_mut(&succ).unwrap();
                    phi.iter_mut().for_each(|(x, y)| {
                        if *x == w {
                            *y = v;
                        }
                    });
                }
            }
        }

        MemoryDependency {
            memuse,
            memdef,
            store_defs,
            def_by,
            memphi,
            results: HashMap::new(),
            mr: mr.clone(),
        }
    }

    fn locate_impl(&self, unit: &TransUnit, val: ValueId, ver: u32, depth: u32) -> u32 {
        if ver == 0 {
            return 0;
        }
        if depth > 5 {
            return ver;
        }
        let def = self.def_by.get(&ver).unwrap();
        match def {
            MemoryDef::Instruction(inst) => {
                let value = &unit.values[*inst];
                match value.value.as_inst() {
                    InstructionValue::Store(s) => {
                        if unit.may_alias(val, s.ptr, &self.mr) {
                            return ver;
                        } else {
                            let last_def = self.memdef.get(inst).unwrap();
                            return self.locate_impl(unit, val, *last_def, depth + 1);
                        }
                    }
                    InstructionValue::Call(call) => {
                        if unit.call_may_alias(val, call.clone(), &self.mr) {
                            return ver;
                        } else {
                            let last_def = self.memdef.get(inst).unwrap();
                            return self.locate_impl(unit, val, *last_def, depth + 1);
                        }
                    }
                    _ => return ver,
                }
            }
            MemoryDef::MemoryPhi(phi) => {
                let phi = self.memphi.get(phi).unwrap();
                let mut results = vec![];
                for (_, ver) in phi {
                    results.push(self.locate_impl(unit, val, *ver, depth + 1));
                }
                results.sort();
                results.dedup();
                if results.len() == 1 {
                    return results[0];
                } else {
                    return ver;
                }
            }
        }
    }

    pub fn locate(&mut self, unit: &TransUnit, val: ValueId) -> u32 {
        if self.results.contains_key(&val) {
            return self.results[&val];
        }
        let ver = *self.memuse.get(&val).unwrap();
        let value = &unit.values[val];
        let ptrval = match value.value.as_inst() {
            InstructionValue::Load(l) => l.ptr,
            // InstructionValue::Store(s) => s.ptr,
            _ => panic!("not a memory value"),
        };
        let ver = self.locate_impl(unit, ptrval, ver, 0);
        self.results.insert(val, ver);
        ver
    }

    // pub fn locate_bb(&mut self, unit: &TransUnit, val: ValueId) -> BlockId {
    //     let ver = self.locate(unit, val);
    //     let def = self.def_by.get(&ver).unwrap();
    //     match def {
    //         MemoryDef::Instruction(inst) => {
    //             unit.inst_bb[inst]
    //         }
    //         MemoryDef::MemoryPhi(phi) => {
    //             *phi
    //         }
    //     }
    // }
}

#[derive(Clone)]
pub struct ModRef {
    pref: HashMap<ValueId, HashSet<ValueId>>,
}

impl TransUnit {
    fn may_alias(
        &self,
        val1: ValueId,
        val2: ValueId,
        mr: &ModRef,
    ) -> bool {
        if val1 == val2 {
            return true;
        }

        let o1 = self.get_base_object(val1);
        let o2 = self.get_base_object(val2);
        if o1 == o2 {
            return true;
        }

        let value1 = &self.values[val1];
        let value2 = &self.values[val2];

        match value1.value {
            ValueType::Parameter(_) => {
                if let ValueType::Parameter(_) = value2.value {
                    // assuming __restrict__
                    return false;
                }
                if let Some(set) = mr.pref.get(&val1) {
                    return set.contains(&val2);
                }
                return false;
            }
            _ => {
                match value2.value {
                    ValueType::Parameter(_) => {
                        if let Some(set) = mr.pref.get(&val2) {
                            return set.contains(&val1);
                        }
                        return false;
                    }
                    ValueType::Instruction(InstructionValue::Call(ref call)) => {
                        return self.call_may_alias(val1, call.clone(), mr);
                    }
                    _ => {
                        return false;
                    }
                }
            }
        }
    }

    fn call_may_alias(&self, val: ValueId, call: CallInst, mr: &ModRef) -> bool {
        let value = self.values[val].clone();
        match value.value {
            ValueType::Parameter(p) => {
                if p.ty.is_ptr() {
                    // maybe we can track foreign pointers further
                    true
                } else {
                    false
                }
            }
            ValueType::Global(_) => {
                // todo: check whether the global is mutated by the function
                true
            }
            _ => {
                // todo: check whether the function writes to the pointer
                call.args.iter().any(|x| self.may_alias(val, *x, mr))
            }
        }
    }

    fn collect_param_ref(&self) -> HashMap<ValueId, HashSet<ValueId>> {
        let mut res = HashMap::new();
        let mut changed = true;

        while changed {
            changed = false;
            for (_, f) in &self.funcs {
                for bb in &f.bbs {
                    let block = &self.blocks[*bb];
                    let mut iter = block.insts_start;
                    while let Some(vid) = iter {
                        let value = &self.values[vid];
                        iter = value.next;
    
                        if let InstructionValue::Call(call) = value.value.as_inst() {
                            let func = match self.funcs.get(&call.name) {
                                Some(f) => f,
                                None => continue,
                            };
                            let fn_ty = func.ty.as_function();
                            let ir_func = if let Some(ir_func) = self.funcs.get(&call.name) {
                                ir_func
                            } else {
                                continue;
                            };
                            for (idx, (pval, (_, ty))) in call.args.iter().zip(fn_ty.params).enumerate() {
                                if ty.is_ptr() {
                                    let o = self.get_base_object(*pval);
                                    let ovalue = &self.values[o];
                                    match ovalue.value {
                                        ValueType::Global(_) => {
                                            if res
                                                .entry(ir_func.params[idx])
                                                .or_insert(HashSet::new())
                                                .insert(o)
                                            {
                                                changed = true;
                                            }
                                            
                                        },
                                        ValueType::Parameter(_) => {
                                            if let Some(set) = res.get(&o).cloned() {
                                                if res
                                                    .entry(ir_func.params[idx])
                                                    .or_insert(HashSet::new())
                                                    .union(&set)
                                                    .count()
                                                    != set.len()
                                                {
                                                    changed = true;
                                                    res.get_mut(&o).unwrap().extend(set.clone());
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        res
    }

    fn get_base_object(&self, val: ValueId) -> ValueId {
        let value = self.values[val].clone();
        match value.value {
            ValueType::Instruction(InstructionValue::GetElemPtr(gep)) => {
                self.get_base_object(gep.ptr)
            },
            // bitcast???
            _ => val,
        }
    }

    pub fn compute_meta(&self) -> ModRef {
        let pref = self.collect_param_ref();
        ModRef {
            pref,
        }
    }
}