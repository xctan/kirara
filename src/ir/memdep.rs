use std::collections::{HashMap, HashSet};

use super::{
    value::{ValueId, InstructionValue, ValueType, CallInst},
    structure::TransUnit, IrPass
};

impl TransUnit {
    pub fn clear_memdep(&mut self, func: &str) {
        let f = self.funcs[func].clone();
        let mut deleted = HashSet::new();

        for bb in &f.bbs {
            for mphi in self.blocks[*bb].mphi.clone() {
                self.values.remove(mphi);
                deleted.insert(mphi);
            }
            let block = &mut self.blocks[*bb];
            block.mphi.clear();
            
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

        // super::cfg::compute_dom(self, func);
        // ensure cfg info is calculated before this!
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
                        self.blocks.get_mut(df).unwrap().mphi.push(phi);
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
            let memphis = self.blocks[bb].mphi.clone();
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
                let memphis = self.blocks[*succ].mphi.clone();
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
            let phis = self.blocks[*bb].mphi.clone();
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


#[derive(Clone, Debug)]
pub struct ModRef {
    pref: HashMap<ValueId, HashSet<ValueId>>,
    gwrite: HashMap<String, HashSet<ValueId>>,
    gread: HashMap<String, HashSet<ValueId>>,
    pwrite: HashMap<String, Vec<bool>>,
    pread: HashMap<String, Vec<bool>>,
}

impl ModRef {
    pub fn has_side_effect(&self, func: &str) -> bool {
        self.gwrite.get(func).map(|x| !x.is_empty()).unwrap_or(true) ||
        self.pwrite.get(func).map(|x| x.iter().any(|x| *x)).unwrap_or(true)
    }

    pub fn is_const(&self, func: &str) -> bool {
        self.gread.get(func).map(|x| x.is_empty()).unwrap_or(false) &&
        self.gwrite.get(func).map(|x| x.is_empty()).unwrap_or(false) &&
        self.pread.get(func).map(|x| x.iter().all(|x| !*x)).unwrap_or(false) &&
        self.pwrite.get(func).map(|x| x.iter().all(|x| !*x)).unwrap_or(false)
    }
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
                // use crate::ir::value::ValueTrait;
                // let value = &self.values[val];
                // print!("[ check whether {:?} clobbers {:?}: ", call.func, value.name());


                let res = mr.gwrite
                    .get(&call.func)
                    .map(|set| set.contains(&val))
                    .unwrap_or(false)
                || call.args.iter().any(|x| self.may_alias(val, *x, mr));

                // println!("{} ]", res);

                res
                // true
            }
            _ => {
                // todo: check whether the function writes to the pointer
                call.args.iter().any(|x| self.may_alias(val, *x, mr))
            }
        }
    }

    fn collect_param_ref(&self) -> ModRef {
        let mut res = HashMap::new();
        let mut global_write = HashMap::new();
        let mut global_read = HashMap::new();
        let mut param_write = HashMap::new();
        let mut param_read = HashMap::new();

        // external functions
        for (name, ty) in &self.external {
            global_read
                .entry(name.clone())
                .or_insert(HashSet::new())
                .insert(self.undef());
            global_write
                .entry(name.clone())
                .or_insert(HashSet::new())
                .insert(self.undef());
            // assume external functions read and write its ptr args
            let pwrite = ty.as_function()
                .params
                .iter()
                .map(|p| p.1.is_ptr())
                .collect::<Vec<_>>();
            let pread = pwrite.clone();
            param_write.insert(name.clone(), pwrite);
            param_read.insert(name.clone(), pread);
        }
        for (name, f) in &self.funcs {
            let func_ty = &f.ty;
            let pwrite = vec![false; func_ty.as_function().params.len()];
            let pread = pwrite.clone();
            param_write.insert(name.clone(), pwrite);
            param_read.insert(name.clone(), pread);
        }

        // trial version of Steensgaard’s algorithm :)
        // should run mem2reg first because we don't actually analyze pointers
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

        for (name, f) in &self.funcs {
            for bb in &f.bbs {
                let block = &self.blocks[*bb];
                let mut iter = block.insts_start;
                while let Some(vid) = iter {
                    let value = &self.values[vid];
                    iter = value.next;

                    if let InstructionValue::Store(s) = value.value.as_inst() {
                        let o = self.get_base_object(s.ptr);
                        let ovalue = &self.values[o];
                        match ovalue.value {
                            ValueType::Global(_) => {
                                global_write
                                    .entry(name.clone())
                                    .or_insert(HashSet::new())
                                    .insert(o);
                            }
                            ValueType::Parameter(_) => {
                                let idx = f.params.iter().position(|x| *x == o).unwrap();
                                param_write
                                    .get_mut(name)
                                    .unwrap()[idx] = true;
                            }
                            _ => {}
                        }
                    } else if let InstructionValue::Load(l) = value.value.as_inst() {
                        let o = self.get_base_object(l.ptr);
                        let ovalue = &self.values[o];
                        match ovalue.value {
                            ValueType::Global(_) => {
                                global_read
                                    .entry(name.clone())
                                    .or_insert(HashSet::new())
                                    .insert(o);
                            }
                            ValueType::Parameter(_) => {
                                let idx = f.params.iter().position(|x| *x == o).unwrap();
                                param_read
                                    .get_mut(name)
                                    .unwrap()[idx] = true;
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        // propagate global/param read/write
        let mut changed = true;
        while changed {
            changed = false;
            for (name, f) in &self.funcs {
                // caller <= callee
                for callee in &self.callgraph[name].callee {
                    global_write.entry(name.clone()).or_default();
                    global_write.entry(callee.clone()).or_default();
                    global_read.entry(name.clone()).or_default();
                    global_read.entry(callee.clone()).or_default();

                    let write_union = global_write
                        .get(name)
                        .unwrap()
                        .union(global_write.get(callee).unwrap())
                        .cloned()
                        .collect::<HashSet<_>>();
                    if write_union.len() != global_write.get(name).unwrap().len() {
                        changed = true;
                        global_write
                            .insert(name.clone(), write_union);
                    }

                    let read_union = global_read
                        .get(name)
                        .unwrap()
                        .union(global_read.get(callee).unwrap())
                        .cloned()
                        .collect::<HashSet<_>>();
                    if read_union.len() != global_read.get(name).unwrap().len() {
                        changed = true;
                        global_read
                            .insert(name.clone(), read_union);
                    }
                }

                // caller <= callee
                for bb in &f.bbs {
                    let block = &self.blocks[*bb];
                    let mut iter = block.insts_start;
                    while let Some(vid) = iter {
                        let value = &self.values[vid];
                        iter = value.next;

                        match value.value.as_inst() {
                            InstructionValue::Call(call) => {
                                for (_, arg) in param_write[&call.func]
                                    .clone()
                                    .iter()
                                    .zip(call.args.iter())
                                    .filter(|(b, _)| **b)
                                {
                                    let orig = self.get_base_object(*arg);
                                    let argvalue = &self.values[orig];
                                    match argvalue.value {
                                        ValueType::Global(_) => {
                                            if global_write
                                                .entry(name.clone())
                                                .or_insert(HashSet::new())
                                                .insert(orig)
                                            {
                                                changed = true;
                                            }
                                        }
                                        ValueType::Parameter(_) => {
                                            let idx: usize = f.params.iter().position(|x| *x == orig).unwrap();
                                            if param_write
                                                .get_mut(name)
                                                .unwrap()[idx]
                                            {
                                                continue;
                                            } else {
                                                changed = true;
                                                param_write
                                                    .get_mut(name)
                                                    .unwrap()[idx] = true;
                                            }
                                        }
                                        _ => {}
                                    }
                                }

                                for (_, arg) in param_read[&call.func]
                                    .clone()
                                    .iter()
                                    .zip(call.args.iter())
                                    .filter(|(b, _)| **b)
                                {
                                    let orig = self.get_base_object(*arg);
                                    let argvalue = &self.values[orig];
                                    match argvalue.value {
                                        ValueType::Global(_) => {
                                            if global_read
                                                .entry(name.clone())
                                                .or_insert(HashSet::new())
                                                .insert(orig)
                                            {
                                                changed = true;
                                            }
                                        }
                                        ValueType::Parameter(_) => {
                                            let idx: usize = f.params.iter().position(|x| *x == orig).unwrap();
                                            if param_read
                                                .get_mut(name)
                                                .unwrap()[idx]
                                            {
                                                continue;
                                            } else {
                                                changed = true;
                                                param_read
                                                    .get_mut(name)
                                                    .unwrap()[idx] = true;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        

        ModRef {
            pref: res,
            gwrite: global_write,
            gread: global_read,
            pwrite: param_write,
            pread: param_read,
        }
    }

    pub fn get_base_object(&self, val: ValueId) -> ValueId {
        let value = self.values[val].clone();
        match value.value {
            ValueType::Instruction(InstructionValue::GetElemPtr(gep)) => {
                self.get_base_object(gep.ptr)
            },
            // bitcast???
            _ => val,
        }
    }

}

pub struct ComputeGlobalModRef;

impl IrPass for ComputeGlobalModRef {
    fn run(&self, unit: &mut TransUnit) {
        // should run callgraph pass before this?
        let mr = unit.collect_param_ref();
        // println!("{:#?}\n", mr);
        unit.modref = Some(mr);
    }
}