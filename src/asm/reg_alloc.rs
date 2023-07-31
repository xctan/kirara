use super::include::*;

#[derive(PartialEq, PartialOrd)]
struct OrderedF32(f32);
impl Eq for OrderedF32 {}
impl Ord for OrderedF32 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}

impl MachineProgram {
    pub fn allocate_registers(&mut self, ir: &mut TransUnit) {
        let funcs = ir.funcs.keys().cloned().collect::<Vec<_>>();
        for func in funcs {
            // let vmax = self.funcs[&func].virtual_max;
            // assert!(vmax <= 2048, "todo: use another allocation algorithm for large functions");

            let loopinfo = LoopInfo::compute(ir, func.as_str());
            
            self.gpr_pass(&loopinfo, &func, ir);
            self.fpr_pass(&loopinfo, &func, ir);
        }
    }

    fn gpr_pass(&mut self, loopinfo: &LoopInfo, func: &str, ir: &mut TransUnit) {
        let mut done = false;

        while !done {
            let liveness = 
                liveness_analysis::<GPOperand, RVGPR, VirtGPR>(self, func, ir);
            let mut allocator = 
                RegisterAllocator::<GPOperand, RVGPR, VirtGPR>::new(
                    &loopinfo,
                    liveness,
                );
            allocator.prepare(self, func);
            allocator.run();
            allocator.assign_colors(self, func);
            if allocator.spilled_nodes.is_empty() {
                done = true;
                let used_regs_allocated = allocator.used_regs;
                let func = self.funcs.get_mut(func).unwrap();
                func.used_regs.extend(used_regs_allocated);
            } else {
                RegisterSpilling::<GPOperand, RVGPR, VirtGPR>::apply(self, allocator.spilled_nodes, func);
            }
        }
    }

    fn fpr_pass(&mut self, loopinfo: &LoopInfo, func: &str, ir: &mut TransUnit) {
        let mut done = false;

        while !done {
            let liveness = 
                liveness_analysis::<FPOperand, RVFPR, VirtFPR>(self, func, ir);
            let mut allocator = 
                RegisterAllocator::<FPOperand, RVFPR, VirtFPR>::new(
                    &loopinfo,
                    liveness,
                );
            allocator.prepare(self, func);
            allocator.run();
            allocator.assign_colors(self, func);
            if allocator.spilled_nodes.is_empty() {
                done = true;
                let used_regs_allocated = allocator.used_regs;
                let func = self.funcs.get_mut(func).unwrap();
                func.used_regsf.extend(used_regs_allocated);
            } else {
                RegisterSpilling::<FPOperand, RVFPR, VirtFPR>::apply(self, allocator.spilled_nodes, func);
            }
        }
    }
}

struct RegisterAllocator<'a, O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    loopinfo: &'a LoopInfo,

    adj_list: HashMap<O, HashSet<O>>,
    adj_set: HashSet<(O, O)>,
    degree: HashMap<O, isize>,
    alias: HashMap<O, O>,
    move_list: HashMap<O, HashSet<Move<O, R, V>>>,
    simplify_worklist: BTreeSet<O>,
    freeze_worklist: BTreeSet<O>,
    spill_worklist: BTreeSet<(OrderedF32, O)>,
    spilled_nodes: HashSet<O>,
    coalesced_nodes: HashSet<O>,
    select_stack: Vec<O>,
    select_stack_set: HashSet<O>,
    coalesced_moves: HashSet<Move<O, R, V>>,
    constrained_moves: HashSet<Move<O, R, V>>,
    frozen_moves: HashSet<Move<O, R, V>>,
    worklist_moves: HashSet<Move<O, R, V>>,
    active_moves: HashSet<Move<O, R, V>>,
    
    loop_count: HashMap<O, usize>,

    used_regs: HashSet<R>,
    liveness: HashMap<Id<MachineBB>, Liveness<O, R, V>>,

    _marker: PhantomData<V>,
}

impl<'a, O, R, V> RegisterAllocator<'a, O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister + std::fmt::Display,
    V: VirtualRegister + std::fmt::Display,
    RV64Instruction: OperandInfo<O, R, V>,
{
    pub fn new(
        loopinfo: &'a LoopInfo,
        liveness: HashMap<Id<MachineBB>, Liveness<O, R, V>>,
    ) -> Self {
        let mut init = Self {
            loopinfo,
            adj_list: HashMap::new(),
            adj_set: HashSet::new(),
            degree: HashMap::new(),
            alias: HashMap::new(),
            move_list: HashMap::new(),
            simplify_worklist: BTreeSet::new(),
            freeze_worklist: BTreeSet::new(),
            spill_worklist: BTreeSet::new(),
            spilled_nodes: HashSet::new(),
            coalesced_nodes: HashSet::new(),
            select_stack: Vec::new(),
            select_stack_set: HashSet::new(),
            coalesced_moves: HashSet::new(),
            constrained_moves: HashSet::new(),
            frozen_moves: HashSet::new(),
            worklist_moves: HashSet::new(),
            active_moves: HashSet::new(),
            loop_count: HashMap::new(),
            used_regs: HashSet::new(),
            liveness,
            _marker: PhantomData,
        };

        // for i in 0..32 {
        //     init.degree.insert(O::pre(RVGPR::x(i)), 0x40000000);
        // }
        for r in O::physical_regs() {
            init.degree.insert(r, 0x40000000);
        }

        init
    }

    pub fn prepare(&mut self, unit: &MachineProgram, func: &str)
    where
        MachineFunc: FuncVirtReg<V>,
    {
        self.build(unit, func);
        let f = unit.funcs.get(func).unwrap();
        self.make_worklist(FuncVirtReg::<V>::get_vreg(f));
    }

    pub fn run(&mut self) {
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
    }

    fn add_edge(&mut self, u: O, v: O) {
        if u == v {
            return;
        }
        if !self.adj_set.contains(&(u, v)) {
            self.adj_set.insert((u, v));
            self.adj_set.insert((v, u));
            if !u.is_precolored() {
                self.adj_list.entry(u).or_default().insert(v);
                *self.degree.entry(u).or_insert(0) += 1;
            }
            if !v.is_precolored() {
                self.adj_list.entry(v).or_default().insert(u);
                *self.degree.entry(v).or_insert(0) += 1;
            }
        }
    }

    fn estimate_cost(&self, x: &O) -> OrderedF32 {
        if self.loop_count[x] == 0 {
            OrderedF32(self.degree[x] as f32 * 2.0)
        } else {
            OrderedF32(self.degree[x] as f32 / self.loop_count[x] as f32)
        }
    }

    fn build(&mut self, unit: &MachineProgram, func: &str) {
        let bbs = unit.funcs[func].bbs.clone();
        for bb in bbs.iter().rev() {
            let block = &unit.blocks[*bb];
            let bblive = &self.liveness[bb];
            let mut live = bblive.liveout.clone();

            let mut iter = block.insts_tail;
            while let Some(inst) = iter {
                let insn = unit.insts[inst].clone();
                iter = insn.prev;

                // println!("{}", insn.inst);
                let (defs, uses) = OperandInfo::<O, R, V>::get_def_use(&insn.inst);
                // print!("defs: ");
                // for d in &defs {
                //     print!("{} ", d);
                // }
                // println!();
                // print!("uses: ");
                // for u in &uses {
                //     print!("{} ", u);
                // }
                // println!();

                if let Some((rd, rs)) = OperandInfo::<O, R, V>::get_move(&insn.inst) {
                    if rd.needs_coloring() && rs.needs_coloring() {
                        live.remove(&rs);
                        let inst = Move::new(rs, rd);
                        self.move_list.entry(rs).or_default().insert(inst);
                        self.move_list.entry(rd).or_default().insert(inst);
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
                            // if !d.is_precolored() || !l.is_precolored() {
                            //     println!("add edge: {} {}", d, l);
                            // }
                            self.add_edge(*d, *l);
                        }
                    }
                }

                let block = &unit.blocks[*bb];
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

    fn adjacent(&self, n: O) -> HashSet<O> {
        self.adj_list
            .get(&n)
            .map(|s| {
                s
                    .iter()
                    .cloned()
                    .filter(|x| {
                        !self.select_stack_set.contains(x) &&
                        !self.coalesced_nodes.contains(x)
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    fn node_moves(&self, n: O) -> BTreeSet<Move<O, R, V>> {
        self.move_list
            .get(&n)
            .map(|s| {
                s
                    .iter()
                    .cloned()
                    .filter(|x| {
                        !self.active_moves.contains(x) &&
                        !self.worklist_moves.contains(x)
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    fn move_related(&self, n: O) -> bool {
        !self.node_moves(n).is_empty()
    }

    fn make_worklist(&mut self, vregs: &BTreeSet<V>) {
        for v in vregs {
            let v = O::virt(*v);
            self.degree.entry(v).or_insert(0);
            self.adj_list.entry(v).or_default();
            if self.degree[&v] >= R::NUM {
                self.spill_worklist.insert((self.estimate_cost(&v), v));
            } else if self.move_related(v) {
                self.freeze_worklist.insert(v);
            } else {
                self.simplify_worklist.insert(v);
            }
        }
    }

    fn enable_moves(&mut self, n: O) {
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

    fn decrement_degree(&mut self, m: O) {
        let d = *self.degree.get(&m).expect(format!("{} not in degree", m).as_str());
        self.degree.insert(m, d - 1);
        if d == R::NUM {
            self.enable_moves(m);
            self.spill_worklist.insert((self.estimate_cost(&m), m));
            if self.move_related(m) {
                self.freeze_worklist.insert(m);
            } else {
                self.simplify_worklist.insert(m);
            }
        }
    }

    fn simplify(&mut self) {
        let n = self.simplify_worklist.pop_first().unwrap();
        self.select_stack.push(n);
        self.select_stack_set.insert(n);
        for m in self.adjacent(n) {
            self.decrement_degree(m);
        }
    }

    fn get_alias(&self, mut n: O) -> O {
        while self.coalesced_nodes.contains(&n) {
            n = self.alias[&n];
        }
        n
    }

    fn add_work_list(&mut self, u: O) {
        if !u.is_precolored() && !self.move_related(u) && self.degree[&u] < R::NUM {
            self.freeze_worklist.remove(&u);
            self.simplify_worklist.insert(u);
        }
    }

    fn ok(&self, t: O, r: O) -> bool {
        self.degree[&t] < R::NUM ||
        t.is_precolored() ||
        self.adj_set.contains(&(t, r))
    }

    fn adj_ok(&self, u: O, v: O) -> bool {
        self
            .adjacent(v)
            .iter()
            .all(|t| self.ok(*t, u))
    }

    fn combine(&mut self, u: O, v: O) {
        if self.freeze_worklist.contains(&v) {
            self.freeze_worklist.remove(&v);
        } else {
            self.spill_worklist.retain(|&(_, x)| x != v);
        }
        self.coalesced_nodes.insert(v);
        self.alias.insert(v, u);
        let move_list_v = self.move_list[&v].clone();
        self.move_list
            .entry(u)
            .or_default()
            .extend(move_list_v);
        for t in self.adjacent(v) {
            self.add_edge(t, u);
            self.decrement_degree(t);
        }
        if self.degree[&u] >= R::NUM && self.freeze_worklist.contains(&u) {
            self.freeze_worklist.remove(&u);
            self.spill_worklist.insert((self.estimate_cost(&u), u));
        }
    }

    fn conservative(&self, mut adj_u: HashSet<O>, adj_v: HashSet<O>) -> bool {
        adj_u.extend(adj_v);
        adj_u
            .iter()
            .filter(|x| self.degree[x] >= R::NUM)
            .count()
            .lt(&(R::NUM as usize))
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

    fn freeze_moves(&mut self, u: O) {
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
            if !self.move_related(v) && self.degree[&v] < R::NUM {
                self.freeze_worklist.remove(&v);
                self.simplify_worklist.insert(v);
            }
        }
    }

    fn freeze(&mut self) {
        let u = self.freeze_worklist.pop_first().unwrap();
        self.simplify_worklist.insert(u);
        self.freeze_moves(u);
    }

    fn select_spill(&mut self) -> O {
        let m = self.spill_worklist
            .pop_last()
            .map(|(_, m)| m)
            .unwrap();
        self.simplify_worklist.insert(m);
        self.freeze_moves(m);

        m
    }

    pub fn assign_colors(&mut self, unit: &mut MachineProgram, func: &str) {
        let mut colored: BTreeMap<O, O> = BTreeMap::new();
        while let Some(n) = self.select_stack.pop() {
            self.select_stack_set.remove(&n);
            // GPRs
            let mut ok_colors: BTreeSet<_> = R::assignable_registers().into_iter().collect();
            // println!("node {}", n);

            for w in &self.adj_list[&n] {
                // print!("candidates: ");
                // for c in &ok_colors {
                //     print!("{} ", c);
                // }
                // println!();

                let alias = self.get_alias(*w);
                if let Some(a) = alias.color() {
                    // println!("remove p {}", a);
                    ok_colors.remove(&a);
                } else if alias.is_virtual() {
                    // if colored.contains_key(&alias) {
                    //     // println!("remove v {}", colored[&alias].color().unwrap());
                    //     ok_colors.remove(&colored[&alias].color().unwrap());
                    // }
                    colored
                        .get(&alias)
                        .and_then(|a| a.color())
                        .map(|c| ok_colors.remove(&c));
                }
            }
            // print!("candidates: ");
            // for c in &ok_colors {
            //     print!("{} ", c);
            // }
            // println!();

            if ok_colors.is_empty() {
                self.spilled_nodes.insert(n);
            } else {
                let c = *ok_colors.iter().next().unwrap();
                colored.insert(n, O::alloc(c));
                // println!("color {}", c);
                self.used_regs.insert(c);
            }
        }
        // println!("colors: {:?}", colored);

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

        let bbs = unit.funcs[func].bbs.clone();
        for bb in bbs {
            let mut iter = unit.blocks[bb].insts_head;
            while let Some(inst) = iter {
                let insn = &mut unit.insts[inst];
                iter = insn.next;
                
                let operands = OperandInfo::<O, R, V>::get_def_mut(&mut insn.inst);
                for op in operands {
                    if op.is_virtual() {
                        // print!("{} -> ", op);
                        *op = colored[op].clone();
                        // println!("{}", op);
                    }
                }
                let operands = OperandInfo::<O, R, V>::get_use_mut(&mut insn.inst);
                for op in operands {
                    if op.is_virtual() {
                        *op = colored[op].clone();
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

struct Checkpoint<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    first_use: Option<Id<MachineInst>>,
    last_def: Option<Id<MachineInst>>,
    vreg: Option<V>,
    offset: u32,
    load: fn(O, GPOperand, i32) -> RV64Instruction,
    save: fn(O, GPOperand, i32) -> RV64Instruction,
    _marker: PhantomData<(O, R, V)>,
}

impl<O, R, V> Checkpoint<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    fn new(
        offset: u32,
        load: fn(O, GPOperand, i32) -> RV64Instruction,
        save: fn(O, GPOperand, i32) -> RV64Instruction
    ) -> Self {
        Self {
            first_use: None,
            last_def: None,
            vreg: None,
            offset,
            load,
            save,
            _marker: PhantomData,
        }
    }

    fn run(&mut self, allocator: &mut RegisterSpilling<O, R, V>, unit: &mut MachineProgram)
    where
        RV64Instruction: OperandInfo<O, R, V>,
    {
        if let Some(first_use) = self.first_use {
            assert!(self.vreg.is_some(), "malformed checkpoint");
            let vreg = self.vreg.unwrap();
            unit.insert_before(
                first_use,
                RV64InstBuilder::COMMENT(format!("reload {}", vreg))
            );
            if self.offset < 2048 {
                unit.insert_before(
                    first_use,
                    (self.load)(
                        O::virt(vreg),
                        GPOperand::pre(RVGPR::sp()),
                        self.offset as i32
                    )
                );
            } else {
                let tmp = VirtGPR::new(
                    allocator.next_vreg(),
                    VirtGPRType::Int64,
                );
                allocator.vgpr.insert(tmp);
                let tmp = GPOperand::virt(tmp);
                let (upper, lower) = split_imm32(self.offset as i32);

                unit.insert_before(
                    first_use,
                    RV64InstBuilder::LUI(
                        tmp,
                        upper
                    )
                );
                unit.insert_before(
                    first_use,
                    RV64InstBuilder::ADD(
                        tmp,
                        tmp,
                        GPOperand::pre(RVGPR::sp())
                    )
                );
                unit.insert_before(
                    first_use,
                    (self.load)(
                        O::virt(vreg),
                        tmp,
                        lower
                    )
                );
            }

            self.first_use = None;
        }
        
        if let Some(last_def) = self.last_def {
            assert!(self.vreg.is_some(), "malformed checkpoint");
            let vreg = self.vreg.unwrap();
            if self.offset < 2048 {
                unit.insert_after(
                    last_def,
                    (self.save)(
                        O::virt(vreg),
                        GPOperand::pre(RVGPR::sp()),
                        self.offset as i32
                    )
                )
            } else {
                let tmp = VirtGPR::new(
                    allocator.next_vreg(),
                    VirtGPRType::Int64,
                );
                allocator.vgpr.insert(tmp);
                let tmp = GPOperand::virt(tmp);
                let (upper, lower) = split_imm32(self.offset as i32);

                unit.insert_after(
                    last_def,
                    (self.save)(
                        O::virt(vreg),
                        tmp,
                        lower
                    )
                );
                unit.insert_after(
                    last_def,
                    RV64InstBuilder::ADD(
                        tmp,
                        tmp,
                        GPOperand::pre(RVGPR::sp())
                    )
                );
                unit.insert_after(
                    last_def,
                    RV64InstBuilder::LUI(
                        tmp,
                        upper
                    )
                );
            }
            unit.insert_after(
                last_def,
                RV64InstBuilder::COMMENT(format!("spill {}", vreg))
            );

            self.last_def = None;
        }

        self.vreg = None;
    }
}

pub struct Liveness<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    pub liveuse: HashSet<O>,
    pub livedef: HashSet<O>,
    pub livein: HashSet<O>,
    pub liveout: HashSet<O>,
    _marker: PhantomData<(O, R, V)>,
}

impl<O, R, V> Liveness<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    pub fn new() -> Self {
        Self {
            liveuse: HashSet::new(),
            livedef: HashSet::new(),
            livein: HashSet::new(),
            liveout: HashSet::new(),
            _marker: PhantomData,
        }
    }
}

fn liveness_analysis<O, R, V>(
    unit: &mut MachineProgram, 
    func: &str,
    ir: &mut TransUnit,
) -> HashMap<Id<MachineBB>, Liveness<O, R, V>>
where
    O: Operand<R, V> ,
    R: PhysicalRegister,
    V: VirtualRegister,
    RV64Instruction: OperandInfo<O, R, V>,
{
    let mut liveness = HashMap::new();

    let entry_irbb = ir.funcs[func].entry_bb;
    let bbmap = &unit.funcs[func].bb_map;
    let ordering = crate::ir::cfg::dfs_order(ir, entry_irbb);
    let ordering = ordering.iter().rev().map(|bb| bbmap[bb]).collect::<Vec<_>>();

    for bb in &ordering {
        let mut bblive = Liveness::new();

        let block = &unit.blocks[*bb];
        let mut iter = block.insts_head;
        while let Some(inst) = iter {
            let insn = unit.insts[inst].clone();
            iter = insn.next;
            let (defs, uses) = OperandInfo::<O, R, V>::get_def_use(&insn.inst);

            for u in uses {
                if u.needs_coloring() && !bblive.livedef.contains(&u) {
                    bblive.liveuse.insert(u);
                }
            }
            for d in defs {
                if d.needs_coloring() && !bblive.liveuse.contains(&d) {
                    bblive.livedef.insert(d);
                }
            }
        }

        bblive.livein = bblive.liveuse.clone();
        bblive.liveout.clear();

        liveness.insert(*bb, bblive);
    }

    let mut changed = true;
    while changed {
        changed = false;
        for bb in &ordering {
            let block = &unit.blocks[*bb];
            let succs = block.succs.clone();
            let mut liveout = HashSet::new();
            for succ in &succs {
                let succ_livein = &liveness[succ].livein;
                liveout.extend(succ_livein.clone());
            }

            let bblive = liveness.get_mut(bb).unwrap();
            if liveout != bblive.liveout {
                changed = true;
                bblive.liveout = liveout;
                let mut livein = bblive.liveuse.clone();
                for out in &bblive.liveout {
                    if !bblive.livedef.contains(out) {
                        livein.insert(*out);
                    }
                }
                bblive.livein = livein;
            }
        }
    }

    liveness
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, PartialOrd, Ord)]
struct Move<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    pub src: O,
    pub dst: O,
    _marker: PhantomData<(O, R, V)>,
}

impl<O, R, V> Move<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    pub fn new(src: O, dst: O) -> Self {
        Self {
            src,
            dst,
            _marker: PhantomData,
        }
    }
}

struct RegisterSpilling<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    spilled_nodes: HashSet<O>,
    vregs: BTreeSet<V>,
    vgpr: BTreeSet<VirtGPR>,
    stack_size: u32,
    vmax: u32,
    _marker: PhantomData<(O, R, V)>,
}

impl<'a> RegisterSpilling<GPOperand, RVGPR, VirtGPR> {
    pub fn apply(unit: &mut MachineProgram, spilled_nodes: HashSet<GPOperand>, func: &str) {
        let mut bbs = vec![];
        std::mem::swap(&mut bbs, &mut unit.funcs.get_mut(func).unwrap().bbs);
        let mut vregs = BTreeSet::new();
        std::mem::swap(&mut vregs, &mut unit.funcs.get_mut(func).unwrap().virtual_gprs);
        let stack_size = unit.funcs[func].stack_size;
        let vmax = unit.funcs[func].virtual_max;

        let mut spilling = Self {
            spilled_nodes,
            vregs,
            vgpr: BTreeSet::new(),
            stack_size,
            vmax,
            _marker: PhantomData,
        };
        spilling.rewrite_program(unit, &bbs);

        unit.funcs.get_mut(func).unwrap().stack_size = spilling.stack_size;
        unit.funcs.get_mut(func).unwrap().virtual_max = spilling.vmax;
        std::mem::swap(&mut bbs, &mut unit.funcs.get_mut(func).unwrap().bbs);
        std::mem::swap(&mut spilling.vregs, &mut unit.funcs.get_mut(func).unwrap().virtual_gprs);

        unit.funcs.get_mut(func).unwrap().virtual_gprs.extend(spilling.vgpr);
    }
}

impl<'a> RegisterSpilling<FPOperand, RVFPR, VirtFPR> {
    pub fn apply(unit: &mut MachineProgram, spilled_nodes: HashSet<FPOperand>, func: &str) {
        let mut bbs = vec![];
        std::mem::swap(&mut bbs, &mut unit.funcs.get_mut(func).unwrap().bbs);
        let mut vregs = BTreeSet::new();
        std::mem::swap(&mut vregs, &mut unit.funcs.get_mut(func).unwrap().virtual_fprs);
        let stack_size = unit.funcs[func].stack_size;
        let vmax = unit.funcs[func].virtual_max;

        let mut spilling = Self {
            spilled_nodes,
            vregs,
            vgpr: BTreeSet::new(),
            stack_size,
            vmax,
            _marker: PhantomData,
        };
        spilling.rewrite_program(unit, &bbs);

        unit.funcs.get_mut(func).unwrap().stack_size = spilling.stack_size;
        unit.funcs.get_mut(func).unwrap().virtual_max = spilling.vmax;
        std::mem::swap(&mut bbs, &mut unit.funcs.get_mut(func).unwrap().bbs);
        std::mem::swap(&mut spilling.vregs, &mut unit.funcs.get_mut(func).unwrap().virtual_fprs);

        unit.funcs.get_mut(func).unwrap().virtual_gprs.extend(spilling.vgpr);
    }
}

impl<O, R, V> RegisterSpilling<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
    RV64Instruction: OperandInfo<O, R, V>,
{
    fn next_vreg(&mut self) -> u32 {
        let old = self.vmax;
        self.vmax += 1;
        old
    }

    fn rewrite_program(&mut self, unit: &mut MachineProgram, bbs: &Vec<Id<MachineBB>>) {
        for n in &self.spilled_nodes.clone() {
            let virt = n.as_virtual();
            let load = n.load_fn();
            let store = n.store_fn();
            let alignment = n.align();
            let size = n.size();

            // fixup stack offset
            self.stack_size = round_up(self.stack_size, alignment);
            let offset = self.stack_size;
            self.stack_size += size;
            let mut checkpoint = Checkpoint::new(offset, load, store);

            for bb in bbs {
                let mut span = 0;
                let mut iter = unit.blocks[*bb].insts_head;
                while let Some(inst) = iter {
                    let insn = &mut unit.insts[inst];
                    iter = insn.next;

                    let (defs, uses) = OperandInfo::<O, R, V>::get_def_use(&insn.inst);
                    if defs.contains(n) {
                        // store
                        if checkpoint.vreg.is_none() {
                            let v = V::from_other(self.next_vreg(), &virt);
                            self.vregs.insert(v);
                            checkpoint.vreg = Some(v);
                        }
                        checkpoint.last_def = Some(inst);

                        let mut defs = OperandInfo::<O, R, V>::get_def_mut(&mut insn.inst);
                        assert!(defs.len() == 1);
                        *defs[0] = O::virt(checkpoint.vreg.unwrap());
                    }

                    for u in &uses {
                        if u == n {
                            // load
                            if checkpoint.vreg.is_none() {
                                let v = V::from_other(self.next_vreg(), &virt);
                                self.vregs.insert(v);
                                checkpoint.vreg = Some(v);
                            }
                            if checkpoint.first_use.is_none() && checkpoint.last_def.is_none() {
                                checkpoint.first_use = Some(inst);
                            }
                        }
                    }
                    let uses = OperandInfo::<O, R, V>::get_use_mut(&mut insn.inst);
                    for op in uses {
                        if *op == *n {
                            *op = O::virt(checkpoint.vreg.unwrap());
                        }
                    }

                    span += 1;
                    if span > 30 {
                        checkpoint.run(self, unit);
                    }
                }
                checkpoint.run(self, unit);
            }
        }
    }

}
