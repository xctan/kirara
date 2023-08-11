use std::collections::{HashMap, HashSet};

use super::{value::{ValueId, InstructionValue, ValueType, CallInst}, structure::TransUnit};

impl TransUnit {
    pub fn clear_memdep(&mut self, func: &str) {
        let f = self.funcs[func].clone();
        let mut deleted = HashSet::new();

        for bb in &f.bbs {
            for mphi in self.blocks[*bb].mphi_lts.clone() {
                self.values.remove(mphi);
                deleted.insert(mphi);
            }
            let block = &mut self.blocks[*bb];
            block.mphi_lts.clear();
            
            let mut removed = vec![];
            let mut iter = block.insts_start;
            while let Some(vid) = iter {
                let value = &mut self.values[vid];
                iter = value.next;
                match value.value.as_inst_mut() {
                    InstructionValue::Load(l) => {
                        l.use_store = None;
                    },
                    InstructionValue::MemOp(_) => {
                        removed.push(vid);
                        deleted.insert(vid);
                    },
                    _ => {}
                }
            }
            for vid in removed {
                self.remove2(vid);
            }
        }

        for bb in &f.bbs {
            let block = &self.blocks[*bb];
            let mut iter = block.insts_start;
            while let Some(vid) = iter {
                let inst = &mut self.values[vid];
                iter = inst.next;
                inst.used_by.retain(|u| !deleted.contains(u));
            }
        }

        // for bb in &f.bbs {
        //     let block = &self.blocks[*bb];
        //     let mut iter = block.insts_start;
        //     while let Some(vid) = iter {
        //         let inst = &self.values[vid];
        //         iter = inst.next;
        //         assert!(inst.used_by.iter().all(|u| self.values.get(*u).is_some()))
        //     }
        // }
    }

    pub fn compute_memdep(&mut self, func: &str, mr: &ModRef) {
        struct RelatedAccess {
            id: usize,
            loads: Vec<ValueId>,
            stores: HashSet<ValueId>,
            killed_by: HashMap<ValueId, Vec<ValueId>>,
            store_ops: HashMap<ValueId, ValueId>,
        }
        impl RelatedAccess {
            fn new(id: usize) -> Self {
                Self {
                    id,
                    loads: vec![],
                    stores: HashSet::new(),
                    killed_by: HashMap::new(),
                    store_ops: HashMap::new(),
                }
            }
        }

        super::cfg::compute_dom(self, func);
        let f = self.funcs[func].clone();
        
        let mut loads = HashMap::<_, RelatedAccess>::new();
        for bb in &f.bbs {
            let mut iter = self.blocks[*bb].insts_start;
            while let Some(vid) = iter {
                let value = &self.values[vid];
                iter = value.next;
                match value.value.as_inst() {
                    InstructionValue::Load(l) => {
                        let orig = self.get_base_object(l.ptr);
                        if loads.contains_key(&orig) {
                            loads.get_mut(&orig).unwrap().loads.push(vid);
                            continue;
                        }
                        let mut ra = RelatedAccess::new(loads.len());
                        ra.loads.push(vid);
                        for bb in &f.bbs {
                            let mut iter = self.blocks[*bb].insts_start;
                            while let Some(vid) = iter {
                                let value = &self.values[vid];
                                iter = value.next;
                                match value.value.as_inst() {
                                    InstructionValue::Store(s) => {
                                        if self.may_alias(s.ptr, orig, mr) {
                                            ra.stores.insert(vid);
                                        }
                                    }
                                    InstructionValue::Call(c) => {
                                        // todo: check side effect!
                                        if self.call_may_alias(orig, c.clone(), mr) {
                                            ra.stores.insert(vid);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        loads.insert(orig, ra);
                    }
                    _ => {}
                }
            }
        }

        let mut worklist = vec![];
        let mut visited = HashSet::new();
        for (&orig, ra) in &loads {
            visited.clear();
            for i in &ra.stores {
                let inst_bb = self.inst_bb[i];
                worklist.push(inst_bb);
            }
            while let Some(bb) = worklist.pop() {
                for &df in &self.blocks[bb].df.clone() {
                    if !visited.contains(&df) {
                        visited.insert(df);
                        let phi = self.memphi(orig, df);
                        self.blocks.get_mut(df).unwrap().mphi_lts.push(phi);
                        self.inst_bb.insert(phi, df);
                        worklist.push(df);
                    }
                }
            }
        }

        let mut worklist = vec![(f.entry_bb, vec![self.undef(); loads.len()])];
        let mut vis = HashSet::new();
        while let Some((bb, mut values)) = worklist.pop() {
            if vis.contains(&bb) {
                continue;
            }
            vis.insert(bb);
            let memphis = self.blocks[bb].mphi_lts.clone();
            for &phi in &memphis {
                let mphi = self.values.get_mut(phi).unwrap().as_mphi();
                let id = loads.get(&mphi.bind_ptr).unwrap().id;
                values[id] = phi;
            }
            let mut iter = self.blocks[bb].insts_start;
            while let Some(vid) = iter {
                let value = &self.values[vid];
                iter = value.next;

                let memdef = match value.value.as_inst() {
                    InstructionValue::Load(l) => {
                        let base = self.get_base_object(l.ptr);
                        values[loads.get(&base).unwrap().id]
                    }
                    InstructionValue::Store(_) | InstructionValue::Call(_) => {
                        for (_, info) in &mut loads {
                            if info.stores.contains(&vid) {
                                // kill old definition
                                info.killed_by.entry(values[info.id]).or_default().push(vid);
                                values[info.id] = vid;
                            }
                        }
                        continue;
                    }
                    _ => continue
                };

                let value = &mut self.values[vid];
                match value.value.as_inst_mut() {
                    InstructionValue::Load(l) => {
                        l.use_store = Some(memdef);
                        let memdef_val = &self.values[memdef];
                        // track usage of memphi only
                        // FIXME: add used_by not only for memphi?
                        if matches!(
                            memdef_val.value,
                            ValueType::Instruction(InstructionValue::MemPhi(_))
                        ) {
                            self.add_used_by(memdef, vid);
                        }
                    }
                    _ => unreachable!()
                }
            }
            for succ in &self.succ(bb) {
                worklist.push((*succ, values.clone()));
                let memphis = self.blocks[*succ].mphi_lts.clone();
                for &phi in &memphis {
                    let mphi = self.values.get_mut(phi).unwrap().as_mphi();
                    let id = loads.get(&mphi.bind_ptr).unwrap().id;
                    mphi.args.iter_mut().find(|x| x.1 == bb).unwrap().0 = values[id];
                    // self.add_used_by(values[id], phi);
                }
            }
        }

        // store killed by store
        for (&orig, ra) in &mut loads {
            for &store in &ra.stores {
                // store doesn't actually use load,
                // so we use pseudo memop inst here for simple cleanup
                let op = self.memop(orig);
                self.insert_before2(op, store);
                ra.store_ops.insert(store, op);
            }

            for &load in &ra.loads {
                let load_value = &self.values[load];
                let self_bb = self.inst_bb[&load];
                let self_dom = self.blocks[self_bb].dom.clone();
                match load_value.value.as_inst() {
                    InstructionValue::Load(l) => {
                        let st = l.use_store.unwrap();
                        if let Some(k) = ra.killed_by.get(&st) {
                            for &ks in k {
                                // no actual inference
                                let store_bb = self.inst_bb[&ks];
                                if !self_dom.contains(&store_bb) {
                                    continue;
                                }
                                // store depends on this load, so the load cannot be
                                // scheduled after this store
                                let agent = ra.store_ops[&ks];
                                self.add_used_by(load, agent);
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
        }

        // store killed by memory phi
        for bb in &f.bbs {
            let phis = self.blocks[*bb].mphi_lts.clone();
            for phi in phis {
                let mphi = self.values[phi].clone();
                match mphi.value.as_inst() {
                    InstructionValue::MemPhi(mphi) => {
                        let ra = &loads[&mphi.bind_ptr];
                        let killed = mphi.args.clone();
                        for &l in &ra.loads {
                            let load = self.values[l].clone();
                            let load_dom = 
                                self.blocks[self.inst_bb[&l]].dom.clone();
                            match load.value.as_inst() {
                                InstructionValue::Load(load) => {
                                    let st = load.use_store.unwrap();
                                    if killed
                                        .iter()
                                        .any(|&(stphi, bb)| {
                                            stphi == st && load_dom.contains(&bb)
                                        })
                                    {
                                        self.add_used_by(l, phi);
                                    }
                                },
                                _ => unreachable!()
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
        }

    }
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