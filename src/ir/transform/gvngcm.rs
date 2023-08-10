use std::collections::{HashMap, HashSet};

use crate::{ir::{
    structure::{TransUnit, BlockId},
    cfg::{reverse_post_order, LoopInfo, compute_dom_level, self},
    memdep::{MemoryDependency, ModRef},
    value::{
        ValueId, BinaryInst, ConstantValue, ValueType, InstructionValue,
        GetElemPtrInst, LoadInst, CallInst
    }
}, ctype::BinaryOpType};

use super::IrPass;

pub struct GVNGCM;

impl IrPass for GVNGCM {
    fn run(&self, unit: &mut TransUnit) {
        let mr = unit.compute_meta();
        for k in unit.funcs() {
            run_gvn(unit, k.as_str(), &mr);
            // eprintln!("{}", unit);
            super::dce::dce(unit, k.as_str());
            // eprintln!("{}", unit);
            run_gcm(unit, k.as_str(), &mr);
            // eprintln!("{}", unit);
        }
    }
}

fn run_gvn(unit: &mut TransUnit, func: &str, mr: &ModRef) {
    let f = unit.funcs[func].clone();
    let dep = MemoryDependency::build(unit, func, mr);
    let mut table = ValueRegistry::new(dep);
    let rpo = reverse_post_order(unit, f.entry_bb);

    for bb in &rpo {
        run_gvn_on_bb(unit, *bb, &mut table);
    }
}

fn run_gvn_on_bb(unit: &mut TransUnit, bb: BlockId, table: &mut ValueRegistry) {
    let block = unit.blocks[bb].clone();
    let mut iter = block.insts_start;
    while let Some(vid) = iter {
        let inst = unit.values[vid].clone();
        iter = inst.next;

        match inst.value.as_inst() {
            InstructionValue::Phi(phi) => {
                let mut all_same = true;
                let first = table.lookup_or_add(unit, phi.args[0].0);
                for arg in phi.args.iter().skip(1) {
                    if first != table.lookup_or_add(unit, arg.0) {
                        all_same = false;
                        break;
                    }
                }
                if all_same {
                    // table.resolved.insert(vid, first);
                    unit.replace(vid, first);
                }
            },
            InstructionValue::Store(s) => {
                let ver = table.dep.store_defs[&vid];
                let key = (s.ptr, ver);
                table.loads.insert(key, s.value);
            },
            _ => {
                let new = table.lookup_or_add(unit, vid);
                if new != vid {
                    unit.replace(vid, new);
                }
            }
        }
    }
}

struct ValueRegistry {
    resolved: HashMap<ValueId, ValueId>,
    binop: HashMap<(BinaryOpType, ValueId, ValueId), ValueId>,
    constant: HashMap<ConstantValue, ValueId>,
    gep: HashMap<(ValueId, Vec<ValueId>), ValueId>,
    dep: MemoryDependency,
    loads: HashMap<(ValueId, u32), ValueId>,
    calls: HashMap<(String, Vec<ValueId>), ValueId>,
}

impl ValueRegistry {
    fn new(dep: MemoryDependency) -> Self {
        Self {
            resolved: HashMap::new(),
            binop: HashMap::new(),
            constant: HashMap::new(),
            gep: HashMap::new(),
            dep,
            loads: HashMap::new(),
            calls: HashMap::new(),
        }
    }

    fn lookup_or_add(&mut self, unit: &mut TransUnit, val: ValueId) -> ValueId {
        let value = unit.values[val].clone();
        // eprintln!("lookupOrAdd {:#?}", value);
        if self.resolved.contains_key(&val) {
            return self.resolved[&val];
        }
        let result = match value.value {
            ValueType::Constant(c) => {
                if self.constant.contains_key(&c) {
                    self.constant[&c]
                } else {
                    self.constant.insert(c, val);
                    val
                }
            }
            ValueType::Instruction(inst) => {
                match inst {
                    InstructionValue::Binary(bin) => {
                        self.find_binop(unit, val, bin)
                    }
                    InstructionValue::GetElemPtr(gep) => {
                        self.find_gep(unit, val, gep)
                    }
                    InstructionValue::Load(l) => {
                        self.find_load(unit, val, l)
                    }
                    InstructionValue::Call(call) => {
                        self.find_call(unit, val, call)
                    }
                    _ => val,
                }
            }
            _ => val,
        };

        self.resolved.insert(val, result);
        result
    }

    fn find_binop(&mut self, unit: &mut TransUnit, val: ValueId, bin: BinaryInst) -> ValueId {
        let lhs = self.lookup_or_add(unit, bin.lhs);
        let rhs = self.lookup_or_add(unit, bin.rhs);
        let key = (bin.op, lhs, rhs);
        if self.binop.contains_key(&key) {
            self.binop[&key]
        } else {
            self.binop.insert(key, val);
            // commutative ops: add, mul, eq, ne
            if let Some(revop) = bin.op.reverse() {
                let key = (revop, rhs, lhs);
                self.binop.insert(key, val);
            }
            val
        }
    }

    fn find_gep(&mut self, unit: &mut TransUnit, val: ValueId, gep: GetElemPtrInst) -> ValueId {
        let ptr = self.lookup_or_add(unit, gep.ptr);
        let mut indices = Vec::new();
        for i in gep.indices {
            indices.push(self.lookup_or_add(unit, i));
        }
        let key = (ptr, indices);
        if self.gep.contains_key(&key) {
            self.gep[&key]
        } else {
            self.gep.insert(key, val);
            val
        }
    }

    fn find_load(&mut self, unit: &mut TransUnit, val: ValueId, l: LoadInst) -> ValueId {
        let ptr = self.lookup_or_add(unit, l.ptr);
        let ver = self.dep.locate(unit, val);
        let key = (ptr, ver);
        // store can be cached in main loop
        if self.loads.contains_key(&key) {
            self.loads[&key]
        } else {
            self.loads.insert(key, val);
            val
        }
    }

    fn find_call(&mut self, unit: &mut TransUnit, val: ValueId, call: CallInst) -> ValueId {
        // todo: check if the function is pure
        let _ = (unit, call);
        let _ = &self.calls;
        val
    }

}

fn run_gcm(unit: &mut TransUnit, func: &str, mr: &ModRef) {
    let f = unit.funcs[func].clone();
    let mut dep = MemoryDependency::build(unit, func, mr);
    let info = LoopInfo::compute(unit, func);
    let dom_level = compute_dom_level(unit, func);
    let mut insts = vec![];
    let mut vis = HashSet::new();
    let entry = f.entry_bb;

    for bb in &f.bbs {
        let block = &unit.blocks[*bb];
        let mut iter = block.insts_start;
        while let Some(vid) = iter {
            let inst = unit.values[vid].clone();
            iter = inst.next;
            insts.push(vid);
        }
    }

    for inst in &insts {
        schedule_early(unit, &mut vis, *inst, &dom_level, entry, &mut dep);
    }
    vis.clear();
    // eprintln!("{}", unit);

    for inst in insts.iter().rev() {
        schedule_late(unit, &mut vis, *inst, &info, entry);
    }
}

fn schedule_early(
    unit: &mut TransUnit, vis: &mut HashSet<ValueId>,
    inst: ValueId, dom_level: &HashMap<BlockId, u32>,
    entry: BlockId, dep: &mut MemoryDependency,
) {
    if vis.contains(&inst) {
        return;
    }
    vis.insert(inst);

    macro_rules! transfer {
        ($block:expr, $inst:expr) => {
            unit.takeout(inst);
            unit.insert_before_end($block, inst);
        };
    }
    macro_rules! schedule {
        ($op:expr) => {
            let value1 = unit.values[$op].clone();
            if value1.value.is_inst() {
                schedule_early(unit, vis, $op, dom_level, entry, dep);
                let op_bb = unit.inst_bb[&$op];
                schedule!(@op_bb);
            }
        };
        (@ $bb:expr) => {
            let op_bb = $bb;
            let x_bb = unit.inst_bb[&inst];
            if dom_level[&x_bb] < dom_level[&op_bb] {
                transfer!(op_bb, inst);
            }
        }
    }

    let value = unit.values[inst].clone();
    if !value.value.is_inst() {
        return;
    }
    match value.value.as_inst(){
        InstructionValue::Binary(bin) => {
            transfer!(entry, inst);
            schedule!(bin.lhs);
            schedule!(bin.rhs);
        }
        InstructionValue::GetElemPtr(gep) => {
            transfer!(entry, inst);
            schedule!(gep.ptr);
            for i in &gep.indices {
                schedule!(*i);
            }
        }
        // InstructionValue::Load(l) => {
        //     transfer!(entry, inst);
        //     schedule!(l.ptr);
        //     schedule!(@dep.locate_bb(unit, inst));
        // }
        // TODO: pure function
        _ => {}
    }
}

fn schedule_late(
    unit: &mut TransUnit, vis: &mut HashSet<ValueId>,
    inst: ValueId, info: &LoopInfo, entry: BlockId,
) {
    if vis.contains(&inst) {
        return;
    }
    vis.insert(inst);

    let value = unit.values[inst].clone();
    if !value.value.is_inst() {
        return;
    }
    if !matches!(
        value.value.as_inst(),
        InstructionValue::Binary(_) | InstructionValue::GetElemPtr(_) /*| InstructionValue::Load(_) */
        /* TODO: pure function */
    ) {
        return;
    }
    
    let mut user_bbs = vec![];
    // eprintln!("{:#?}", value);
    for u in &value.used_by {
        schedule_late(unit, vis, *u, info, entry);
        let user = unit.inst_bb[u];
        let user_value = unit.values[*u].clone();
        if let InstructionValue::Phi(phi) = user_value.value.as_inst() {
            for (arg, bb) in &phi.args {
                if *arg == inst {
                    user_bbs.push(*bb);
                }
            }
        } else {
            user_bbs.push(user);
        }
    }

    let mut lca = user_bbs[0];
    for i in user_bbs.iter().skip(1) {
        lca = cfg::intersect(unit, entry, lca, *i);
    }

    let mut best_bb = lca;
    let mut best_loop_depth = info.depth(best_bb);
    let inst_bb = unit.inst_bb[&inst]; // upper bound
    while lca != inst_bb {
        lca = unit.blocks[lca].idom.unwrap();
        let loop_depth = info.depth(lca);
        if loop_depth < best_loop_depth {
            best_bb = lca;
            best_loop_depth = loop_depth;
        }
    }

    unit.takeout(inst);
    unit.insert_before_end(best_bb, inst);
    let mut iter = unit.blocks[best_bb].insts_start;
    while let Some(vid) = iter {
        let inst1 = unit.values[vid].clone();
        iter = inst1.next;
        if !matches!(inst1.value.as_inst(), InstructionValue::Phi(_)) {
            if value.used_by.contains(&vid) {
                unit.takeout(inst);
                unit.insert_before(best_bb, inst, vid);
                break;
            }
        }
    }
}