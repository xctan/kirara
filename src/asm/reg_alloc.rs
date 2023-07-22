use super::import::*;

/// number of assignable general registers
/// including: a0-a7, s1-s11, t0-t6
const KGPR: usize = 26;

impl MachineProgram {
    pub fn allocate_registers(&mut self, ir: &mut TransUnit) {
        let funcs = self.funcs.keys().cloned().collect::<Vec<_>>();
        for func in funcs {
            let loopinfo = LoopInfo::compute(ir, func.as_str());
            let mut done = false;

            while !done {
                liveness_analysis(self, func.as_str());
                let mut allocator = RegisterAllocator::new(self, func.as_str(), &loopinfo);
                allocator.run();
                if allocator.spilled_nodes.is_empty() {
                    done = true;
                    let used_regs_allocated = allocator.used_regs;
                    let func = self.funcs.get_mut(func.as_str()).unwrap();
                    func.used_regs.extend(used_regs_allocated);
                } else {
                    allocator.rewrite_program();
                }
            }
        }
    }
}

struct RegisterAllocator<'a> {
    unit: &'a mut MachineProgram,
    func: &'a str,
    loopinfo: &'a LoopInfo,

    adj_list: HashMap<MachineOperand, HashSet<MachineOperand>>,
    adj_set: HashSet<(MachineOperand, MachineOperand)>,
    degree: HashMap<MachineOperand, usize>,
    alias: HashMap<MachineOperand, MachineOperand>,
    move_list: HashMap<MachineOperand, HashSet<Move>>,
    simplify_worklist: HashSet<MachineOperand>,
    freeze_worklist: HashSet<MachineOperand>,
    spill_worklist: HashSet<MachineOperand>,
    spilled_nodes: HashSet<MachineOperand>,
    coalesced_nodes: HashSet<MachineOperand>,
    select_stack: Vec<MachineOperand>,
    coalesced_moves: HashSet<Move>,
    constrained_moves: HashSet<Move>,
    frozen_moves: HashSet<Move>,
    worklist_moves: HashSet<Move>,
    active_moves: HashSet<Move>,
    
    loop_count: HashMap<MachineOperand, usize>,

    used_regs: HashSet<RVGPR>,
}

impl<'a> RegisterAllocator<'a> {
    pub fn new(unit: &'a mut MachineProgram, func: &'a str, loopinfo: &'a LoopInfo) -> Self {
        let mut init = Self {
            unit,
            func,
            loopinfo,
            adj_list: HashMap::new(),
            adj_set: HashSet::new(),
            degree: HashMap::new(),
            alias: HashMap::new(),
            move_list: HashMap::new(),
            simplify_worklist: HashSet::new(),
            freeze_worklist: HashSet::new(),
            spill_worklist: HashSet::new(),
            spilled_nodes: HashSet::new(),
            coalesced_nodes: HashSet::new(),
            select_stack: Vec::new(),
            coalesced_moves: HashSet::new(),
            constrained_moves: HashSet::new(),
            frozen_moves: HashSet::new(),
            worklist_moves: HashSet::new(),
            active_moves: HashSet::new(),
            loop_count: HashMap::new(),
            used_regs: HashSet::new(),
        };

        for i in 0..KGPR {
            init.degree.insert(MachineOperand::PreColored(RVGPR::x(i)), 0x40000000);
        }
        init.used_regs.extend(
            init.unit.funcs[func].used_regs
                .iter()
                .cloned()
        );

        init
    }

    pub fn run(&mut self) {
        self.build();
        self.make_worklist();
        while !self.is_empty() {
            if !self.simplify_worklist.is_empty() {
                self.simplify();
            } else if !self.worklist_moves.is_empty() {
                self.coalesce();
            } else if !self.freeze_worklist.is_empty() {
                self.freeze();
            } else if !self.spill_worklist.is_empty() {
                self.select_spill();
            }
        }
        self.assign_colors();
    }

    pub fn rewrite_program(&mut self) {
        struct Checkpoint {
            first_use: Option<Id<MachineInst>>,
            last_def: Option<Id<MachineInst>>,
            vreg: u32,
            offset: u32,
            load: fn(MachineOperand, MachineOperand, i32) -> RV64Instruction,
            save: fn(MachineOperand, MachineOperand, i32) -> RV64Instruction,
        }
        impl Checkpoint {
            fn new(
                offset: u32,
                load: fn(MachineOperand, MachineOperand, i32) -> RV64Instruction,
                save: fn(MachineOperand, MachineOperand, i32) -> RV64Instruction
            ) -> Self {
                Self {
                    first_use: None,
                    last_def: None,
                    vreg: 0,
                    offset,
                    load,
                    save
                }
            }

            fn run(&mut self, allocator: &mut RegisterAllocator) {
                if let Some(first_use) = self.first_use {
                    assert!(self.vreg != 0);
                    if self.offset < 2048 {
                        allocator.unit.insert_before(
                            first_use,
                            (self.load)(
                                MachineOperand::Virtual(self.vreg),
                                MachineOperand::PreColored(RVGPR::sp()),
                                self.offset as i32
                            )
                        )
                    } else {
                        allocator.unit.insert_before(
                            first_use,
                            RV64InstBuilder::LIMM(
                                MachineOperand::Virtual(self.vreg),
                                self.offset as i32
                            )
                        );
                        allocator.unit.insert_before(
                            first_use,
                            RV64InstBuilder::ADD(
                                MachineOperand::Virtual(self.vreg),
                                MachineOperand::Virtual(self.vreg),
                                MachineOperand::PreColored(RVGPR::sp())
                            )
                        );
                        allocator.unit.insert_before(
                            first_use,
                            (self.load)(
                                MachineOperand::Virtual(self.vreg),
                                MachineOperand::Virtual(self.vreg),
                                0
                            )
                        );
                    }

                    self.first_use = None;
                }
                
                if let Some(last_def) = self.last_def {
                    assert!(self.vreg != 0);
                    if self.offset < 2048 {
                        allocator.unit.insert_after(
                            last_def,
                            (self.save)(
                                MachineOperand::Virtual(self.vreg),
                                MachineOperand::PreColored(RVGPR::sp()),
                                self.offset as i32
                            )
                        )
                    } else {
                        allocator.unit.insert_after(
                            last_def,
                            (self.save)(
                                MachineOperand::Virtual(self.vreg),
                                MachineOperand::Virtual(self.vreg),
                                0
                            )
                        );
                        allocator.unit.insert_after(
                            last_def,
                            RV64InstBuilder::ADD(
                                MachineOperand::Virtual(self.vreg),
                                MachineOperand::Virtual(self.vreg),
                                MachineOperand::PreColored(RVGPR::sp())
                            )
                        );
                        allocator.unit.insert_after(
                            last_def,
                            RV64InstBuilder::LIMM(
                                MachineOperand::Virtual(self.vreg),
                                self.offset as i32
                            )
                        );
                    }

                    self.last_def = None;
                }

                self.vreg = 0;
            }
        }

        let bbs = self.unit.funcs[self.func].bbs.clone();
        for n in &self.spilled_nodes.clone() {
            let vreg_number = if let MachineOperand::Virtual(vreg) = n {
                *vreg
            } else {
                unreachable!()
            };

            let load = match self.unit.funcs[self.func].vreg_types[&vreg_number] {
                VRegType::Int32 => RV64InstBuilder::LW,
                VRegType::Int64 => RV64InstBuilder::LD,
                _ => unreachable!(),
            };
            let store = match self.unit.funcs[self.func].vreg_types[&vreg_number] {
                VRegType::Int32 => RV64InstBuilder::SW,
                VRegType::Int64 => RV64InstBuilder::SD,
                _ => unreachable!(),
            };
            let (alignment, size) = match self.unit.funcs[self.func].vreg_types[&vreg_number] {
                VRegType::Int32 => (4, 4),
                VRegType::Int64 => (8, 8),
                _ => unreachable!(),
            };

            // fixup stack offset
            if self.unit.funcs[self.func].stack_size % alignment != 0 {
                let new_stack_size = 
                    self.unit.funcs[self.func].stack_size
                    + alignment
                    - self.unit.funcs[self.func].stack_size % alignment;
                let func = self.unit.funcs.get_mut(self.func).unwrap();
                func.stack_size = new_stack_size;
            }
            let offset = self.unit.funcs[self.func].stack_size;
            self.unit.funcs.get_mut(self.func).unwrap().stack_size += size;
            let mut checkpoint = Checkpoint::new(offset, load, store);

            for bb in &bbs {
                let mut span = 0;
                let mut iter = self.unit.blocks[*bb].insts_head;
                while let Some(inst) = iter {
                    let insn = &mut self.unit.insts[inst];
                    iter = insn.next;

                    let (defs, uses) = self.unit.get_def_use(inst);
                    if defs.contains(n) {
                        // store
                        if checkpoint.vreg == 0 {
                            checkpoint.vreg = self.unit.funcs[self.func].virtual_max;
                            let func = self.unit.funcs.get_mut(self.func).unwrap();
                            func.virtual_max += 1;
                            func.vreg_types.insert(checkpoint.vreg, func.vreg_types[&vreg_number]);
                        }
                        checkpoint.last_def = Some(inst);

                        let operands = self.unit.get_operands_mut(inst);
                        assert!(defs.len() == 1);
                        *operands.into_iter().next().unwrap() = MachineOperand::Virtual(checkpoint.vreg);
                    }

                    for u in &uses {
                        if u == n {
                            // load
                            if checkpoint.vreg == 0 {
                                checkpoint.vreg = self.unit.funcs[self.func].virtual_max;
                                let func = self.unit.funcs.get_mut(self.func).unwrap();
                                func.virtual_max += 1;
                                func.vreg_types.insert(checkpoint.vreg, func.vreg_types[&vreg_number]);
                            }
                            if checkpoint.first_use.is_none() && checkpoint.last_def.is_none() {
                                checkpoint.first_use = Some(inst);
                            }
                        }
                    }
                    let operands = self.unit.get_operands_mut(inst);
                    for op in operands.into_iter().skip(defs.len()) {
                        if *op == *n {
                            *op = MachineOperand::Virtual(checkpoint.vreg);
                        }
                    }

                    span += 1;
                    if span > 30 {
                        checkpoint.run(self);
                    }
                }
                checkpoint.run(self);
            }
        }
    }

    fn add_edge(&mut self, u: MachineOperand, v: MachineOperand) {
        if u == v {
            return;
        }
        if !self.adj_set.contains(&(u, v)) {
            self.adj_set.insert((u, v));
            self.adj_set.insert((v, u));
            if !u.is_precolored() {
                self.adj_list.entry(u).or_insert(HashSet::new()).insert(v);
                *self.degree.entry(u).or_insert(0) += 1;
            }
            if !v.is_precolored() {
                self.adj_list.entry(v).or_insert(HashSet::new()).insert(u);
                *self.degree.entry(v).or_insert(0) += 1;
            }
        }
    }

    fn build(&mut self) {
        let bbs = self.unit.funcs[self.func].bbs.clone();
        for bb in bbs.iter().rev() {
            let block = &self.unit.blocks[*bb];
            let mut live = block.liveout.clone();

            let mut iter = block.insts_tail;
            while let Some(inst) = iter {
                let insn = self.unit.insts[inst].clone();
                iter = insn.prev;

                let (defs, uses) = self.unit.get_def_use(inst);

                if let RV64Instruction::MV{ rd, rs } = insn.inst {
                    if rd.needs_coloring() && rs.needs_coloring() {
                        live.remove(&rs);
                        let inst = Move { id: inst, src: rs, dst: rd };
                        self.move_list.entry(rs).or_insert(HashSet::new()).insert(inst);
                        self.move_list.entry(rd).or_insert(HashSet::new()).insert(inst);
                        self.worklist_moves.insert(inst);
                    }
                }

                for d in &defs {
                    if d.needs_coloring() {
                        live.insert(*d);
                    }
                }

                for d in &defs {
                    if d.needs_coloring() {
                        for l in &live {
                            self.add_edge(*d, *l);
                        }
                    }
                }

                let block = &self.unit.blocks[*bb];
                for d in &defs {
                    if d.needs_coloring() {
                        live.remove(d);
                        self.loop_count
                            .entry(*d)
                            .or_insert(0)
                            .add_assign(self.loopinfo.depth(block.bb));
                    }
                }

                for u in &uses {
                    if u.needs_coloring() {
                        live.insert(*u);
                        self.loop_count
                            .entry(*u)
                            .or_insert(0)
                            .add_assign(self.loopinfo.depth(block.bb));
                    }
                }
            }
        }
    }

    fn adjacent(&self, n: MachineOperand) -> HashSet<MachineOperand> {
        self.adj_list
            .get(&n)
            .unwrap_or(&HashSet::new())
            .iter()
            .cloned()
            .filter(|x| {
                !self.select_stack.contains(x) &&
                !self.coalesced_nodes.contains(x)
            })
            .collect()
    }

    fn node_moves(&self, n: MachineOperand) -> HashSet<Move> {
        self.move_list
            .get(&n)
            .unwrap_or(&HashSet::new())
            .iter()
            .cloned()
            .filter(|x| {
                !self.active_moves.contains(x) &&
                !self.worklist_moves.contains(x)
            })
            .collect()
    }

    fn move_related(&self, n: MachineOperand) -> bool {
        !self.node_moves(n).is_empty()
    }

    fn make_worklist(&mut self) {
        let vmax = self.unit.funcs[self.func].virtual_max;
        for i in 1..vmax {
            if !matches!(
                self.unit.funcs[self.func].vreg_types[&i],
                VRegType::Int32 | VRegType::Int64
            ) {
                continue;
            }

            let v = MachineOperand::Virtual(i);
            self.degree.entry(v).or_insert(0);
            self.adj_list.entry(v).or_insert(HashSet::new());
            if self.degree[&v] >= KGPR {
                self.spill_worklist.insert(v);
            } else if self.move_related(v) {
                self.freeze_worklist.insert(v);
            } else {
                self.simplify_worklist.insert(v);
            }
        }
    }

    fn enable_moves(&mut self, n: MachineOperand) {
        for m in self.node_moves(n) {
            if self.active_moves.contains(&m) {
                self.active_moves.remove(&m);
                self.worklist_moves.insert(m);
            }
        }

        for a in self.adjacent(n) {
            for m in self.node_moves(a) {
                if self.active_moves.contains(&m) {
                    self.active_moves.remove(&m);
                    self.worklist_moves.insert(m);
                }
            }
        }
    }

    fn decrement_degree(&mut self, m: MachineOperand) {
        let d = self.degree[&m];
        self.degree.insert(m, d - 1);
        if d == KGPR {
            self.enable_moves(m);
            self.spill_worklist.insert(m);
            if self.move_related(m) {
                self.freeze_worklist.insert(m);
            } else {
                self.simplify_worklist.insert(m);
            }
        }
    }

    fn simplify(&mut self) {
        let n = *self.simplify_worklist.iter().next().unwrap();
        self.simplify_worklist.remove(&n);
        self.select_stack.push(n);
        for m in self.adjacent(n) {
            self.decrement_degree(m);
        }
    }

    fn get_alias(&self, mut n: MachineOperand) -> MachineOperand {
        while self.coalesced_nodes.contains(&n) {
            n = self.alias[&n];
        }
        n
    }

    fn add_work_list(&mut self, u: MachineOperand) {
        if !u.is_precolored() && !self.move_related(u) && self.degree[&u] < KGPR {
            self.freeze_worklist.remove(&u);
            self.simplify_worklist.insert(u);
        }
    }

    fn ok(&self, t: MachineOperand, r: MachineOperand) -> bool {
        self.degree[&t] < KGPR ||
        t.is_precolored() ||
        self.adj_set.contains(&(t, r))
    }

    fn adj_ok(&self, u: MachineOperand, v: MachineOperand) -> bool {
        self
            .adjacent(v)
            .iter()
            .all(|t| self.ok(*t, u))
    }

    fn combine(&mut self, u: MachineOperand, v: MachineOperand) {
        if self.freeze_worklist.contains(&v) {
            self.freeze_worklist.remove(&v);
        } else {
            self.spill_worklist.remove(&v);
        }
        self.coalesced_nodes.insert(v);
        self.alias.insert(v, u);
        let move_list_v = self.move_list[&v].clone();
        self.move_list
            .entry(u)
            .or_insert(HashSet::new())
            .extend(move_list_v);
        for t in self.adjacent(v) {
            self.add_edge(t, u);
            self.decrement_degree(t);
        }
        if self.degree[&u] >= KGPR && self.freeze_worklist.contains(&u) {
            self.freeze_worklist.remove(&u);
            self.spill_worklist.insert(u);
        }
    }

    fn conservative(&self, mut adj_u: HashSet<MachineOperand>, adj_v: HashSet<MachineOperand>) -> bool {
        adj_u.extend(adj_v);
        adj_u
            .iter()
            .filter(|x| self.degree[x] >= KGPR)
            .count()
            .lt(&KGPR)
    }

    fn coalesce(&mut self) {
        let m = *self.worklist_moves.iter().next().unwrap();
        let u = self.get_alias(m.dst);
        let v = self.get_alias(m.src);

        let (u, v) = if v.is_precolored() {
            (v, u)
        } else {
            (u, v)
        };
        self.worklist_moves.remove(&m);

        if u == v {
            self.coalesced_moves.insert(m);
            self.add_work_list(u);
        } else if v.is_precolored() || self.adj_set.contains(&(u, v)) {
            self.constrained_moves.insert(m);
            self.add_work_list(u);
            self.add_work_list(v);
        } else if (u.is_precolored() && self.adj_ok(v, u)) ||
                  (!u.is_precolored() && self.conservative(self.adjacent(u), self.adjacent(v))) {
            self.coalesced_moves.insert(m);
            self.combine(u, v);
            self.add_work_list(u);
        } else {
            self.active_moves.insert(m);
        }
    }

    fn freeze_moves(&mut self, u: MachineOperand) {
        for m in self.node_moves(u) {
            if self.active_moves.contains(&m) {
                self.active_moves.remove(&m);
            } else {
                self.worklist_moves.remove(&m);
            }
            self.frozen_moves.insert(m);

            let v = if m.dst == u {
                m.src
            } else {
                m.dst
            };
            if !self.move_related(v) && self.degree[&v] < KGPR {
                self.freeze_worklist.remove(&v);
                self.simplify_worklist.insert(v);
            }
        }
    }

    fn freeze(&mut self) {
        let u = *self.freeze_worklist.iter().next().unwrap();
        self.freeze_worklist.remove(&u);
        self.simplify_worklist.insert(u);
        self.freeze_moves(u);
    }

    fn select_spill(&mut self) -> MachineOperand {
        struct OrderedF32(f32);
        impl PartialEq for OrderedF32 {
            fn eq(&self, other: &Self) -> bool {
                self.0.eq(&other.0)
            }
        }
        impl PartialOrd for OrderedF32 {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                self.0.partial_cmp(&other.0)
            }
        }
        impl Eq for OrderedF32 {}
        impl Ord for OrderedF32 {
            fn cmp(&self, other: &Self) -> Ordering {
                self.0.partial_cmp(&other.0).unwrap_or(Ordering::Equal)
            }
        }

        let m = self.spill_worklist
            .iter()
            .max_by_key(|x| {
                if self.loop_count[x] == 0 {
                    OrderedF32(self.degree[x] as f32 * 2.0)
                } else {
                    OrderedF32(self.degree[x] as f32 / self.loop_count[x] as f32)
                }
            })
            .unwrap()
            .clone();
        self.simplify_worklist.insert(m);
        self.freeze_moves(m);
        self.spill_worklist.remove(&m);

        m
    }

    fn assign_colors(&mut self) {
        let mut colored: HashMap<MachineOperand, MachineOperand> = HashMap::new();
        while let Some(n) = self.select_stack.pop() {
            // GPRs
            let mut ok_colors: BTreeSet<_> = (0..=7).map(|x| RVGPR::a(x))
                .chain((1..=11).map(|x| RVGPR::s(x)))
                .chain((0..=6).map(|x| RVGPR::t(x)))
                .collect();

            for w in &self.adj_list[&n] {
                let alias = self.get_alias(*w);
                if let Some(a) = alias.color() {
                    ok_colors.remove(&a);
                } else if let MachineOperand::Virtual(_v) = alias {
                    if colored.contains_key(&alias) {
                        ok_colors.remove(&colored[&alias].color().unwrap());
                    }
                }
            }

            if ok_colors.is_empty() {
                self.spilled_nodes.insert(n);
            } else {
                let c = *ok_colors.iter().next().unwrap();
                colored.insert(n, MachineOperand::Allocated(c));
                self.used_regs.insert(c);
            }
        }

        // failed to satisfy constraints, try to spill
        if !self.spilled_nodes.is_empty() {
            return;
        }

        for n in &self.coalesced_nodes {
            let alias = self.get_alias(*n);
            if alias.is_precolored() {
                colored.insert(*n, alias);
            } else {
                colored.insert(*n, colored[&alias]);
            }
        }

        let bbs = self.unit.funcs[self.func].bbs.clone();
        for bb in bbs {
            let mut iter = self.unit.blocks[bb].insts_head;
            while let Some(inst) = iter {
                let insn = self.unit.insts[inst].clone();
                iter = insn.next;
                
                let operands = self.unit.get_operands_mut(inst);
                for op in operands {
                    if let MachineOperand::Virtual(v) = op {
                        *op = colored[&MachineOperand::Virtual(*v)].clone();
                    }
                }
            }
        }
    }

    fn is_empty(&self) -> bool {
        self.simplify_worklist.is_empty() &&
        self.worklist_moves.is_empty() &&
        self.freeze_worklist.is_empty() &&
        self.spill_worklist.is_empty()
    }
}


fn liveness_analysis(unit: &mut MachineProgram, func: &str) {
    let bbs = unit.funcs[func].bbs.clone();
    
    for bb in &bbs {
        let block = &mut unit.blocks[*bb];
        block.liveuse.clear();
        block.livedef.clear();

        let mut iter = block.insts_head;
        while let Some(inst) = iter {
            let insn = unit.insts[inst].clone();
            iter = insn.next;
            let (defs, uses) = unit.get_def_use(inst);

            let block = &mut unit.blocks[*bb];
            for u in uses {
                if u.needs_coloring() && !block.livedef.contains(&u) {
                    block.liveuse.insert(u);
                }
            }
            for d in defs {
                if d.needs_coloring() && !block.liveuse.contains(&d) {
                    block.livedef.insert(d);
                }
            }
        }

        let block = &mut unit.blocks[*bb];
        block.livein = block.liveuse.clone();
        block.liveout.clear();
    }

    let mut changed = true;
    while changed {
        changed = false;
        for bb in &bbs {
            let block = &unit.blocks[*bb];
            let succs = block.succs.clone();
            let mut liveout = HashSet::new();
            for succ in &succs {
                liveout.extend(unit.blocks[*succ].livein.clone());
            }

            let block = &mut unit.blocks[*bb];
            if liveout != block.liveout {
                changed = true;
                block.liveout = liveout;
                let mut livein = block.liveuse.clone();
                for out in &block.liveout {
                    if !block.livedef.contains(out) {
                        livein.insert(*out);
                    }
                }
                block.livein = livein;
            }
        }
    }
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct Move {
    pub id: Id<MachineInst>,
    pub src: MachineOperand,
    pub dst: MachineOperand,
}