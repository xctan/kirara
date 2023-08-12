use std::{collections::{HashSet, HashMap}, rc::{Weak, Rc}, cell::RefCell};

use super::{structure::TransUnit, structure::BlockId};

pub fn dfs_order(unit: &TransUnit, entry_bb: BlockId) -> Vec<BlockId> {
    let mut stack = Vec::new();
    let mut visited = HashSet::new();
    let mut result = Vec::new();
    stack.push(entry_bb);
    while let Some(top_id) = stack.pop() {
        if visited.contains(&top_id) {
            continue;
        }
        visited.insert(top_id);
        result.push(top_id);
        let succ = unit.succ(top_id);
        for target in succ {
            stack.push(target);
        }
    }

    result
}

pub fn reverse_post_order(unit: &mut TransUnit, entry_bb: BlockId) -> Vec<BlockId> {
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

pub fn intersect(unit: &TransUnit, entry_bb: BlockId, i: BlockId, j: BlockId) -> BlockId {
    if i == entry_bb {
        return i;
    } else if j == entry_bb {
        return j;
    }
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
                unreachable!()
            }
        }
        while finger2_order > finger1_order {
            if let Some(finger) = unit.blocks.get(finger2).unwrap().idom {
                finger2 = finger;
                finger2_order = unit.blocks.get(finger2).unwrap().rpo;
            } else {
                unreachable!()
            }
        }
    }

    finger1
}

/// Compute immediate dominator
fn compute_idom(unit: &mut TransUnit, func: &str) {
    let func = unit.funcs.get(func).unwrap().clone();
    let entry_bb = func.entry_bb;
    let entry = unit.blocks.get_mut(entry_bb).unwrap();
    entry.idom = None;
    // Entry block to function must not have predecessors!
    assert!(entry.preds.is_empty());
    for bb in &func.bbs {
        unit.blocks.get_mut(*bb).unwrap().idom = None;
    }
    let successors = unit.succ(entry_bb);
    for succ in successors {
        let succ = unit.blocks.get_mut(succ).unwrap();
        succ.idom = Some(entry_bb);
    }

    let ordering = reverse_post_order(unit, entry_bb);

    // println!("calculation order:");
    // let mut i = 0;
    // for bb in &ordering {
    //     if i != 0 {
    //         print!(" -> ");
    //     }
    //     print!("{}", unit.blocks[*bb].name);
    //     i += 1;
    // }
    // println!();
    // for node in &ordering {
    //     let n = unit.blocks.get(*node).unwrap();
    //     let idom = n.idom;
    //     let idom = if let Some(idom) = idom {
    //         unit.blocks.get(idom).unwrap().name.clone()
    //     } else {
    //         "None".to_owned()
    //     };
    //     println!("{} -> {idom}", n.name);
    // }
    // println!();

    // calculate immediate dominator
    let mut changed = true;
    while changed {
        changed = false;
        for node in &ordering {
            let n = unit.blocks.get(*node).unwrap();
            if n.preds.len() == 0 || *node == entry_bb {
                continue;
            } else if n.preds.len() == 1 {
                let n = unit.blocks.get_mut(*node).unwrap();
                if n.idom != Some(*n.preds.first().unwrap()) {
                    n.idom = Some(*n.preds.first().unwrap());
                    changed = true;
                }
            } else {
                // multiple predecessors
                let new_idom = n.preds
                    .iter()
                    .cloned()
                    .filter(|p| unit.blocks[*p].idom.is_some() || *p == entry_bb)
                    .reduce(|a, b| intersect(unit, entry_bb, a, b));

                if new_idom.is_some() && n.idom != new_idom {
                    let n = unit.blocks.get_mut(*node).unwrap();
                    n.idom = new_idom;
                    changed = true;
                }
            }
        }
    }

    // for node in &ordering {
    //     let n = unit.blocks.get(*node).unwrap();
    //     let idom = n.idom;
    //     let idom = if let Some(idom) = idom {
    //         unit.blocks.get(idom).unwrap().name.clone()
    //     } else {
    //         "None".to_owned()
    //     };
    //     println!("{} -> {idom}", n.name);
    // }
}

pub fn compute_dom(unit: &mut TransUnit, func: &str) {
    compute_idom(unit, func);

    let func = unit.funcs.get(func).unwrap().clone();

    for n in &func.bbs {
        let node = unit.blocks.get_mut(*n).unwrap();
        node.dom.clear();
    }

    for bb in func.bbs {
        let block = &mut unit.blocks[bb];
        // a block dominates itself
        block.dom.push(bb);

        let mut runner = block.idom;
        while let Some(r) = runner {
            let upperbound = unit.blocks.capacity();
            let block = &mut unit.blocks[r];
            block.dom.push(bb);
            // sanity check
            assert!(block.dom.len() <= upperbound);
            runner = block.idom;
        }
    }
}

/// Compute dominance frontier
pub fn compute_df(unit: &mut TransUnit, func: &str) {
    compute_idom(unit, func);
    let bbs = unit.funcs.get(func).unwrap().bbs.clone();

    for n in &bbs {
        let node = unit.blocks.get_mut(*n).unwrap();
        node.df.clear();
    }

    // calculate dominance frontier (figure 9.10)
    for n in &bbs {
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
    for n in &bbs {
        let node = unit.blocks.get_mut(*n).unwrap();
        node.df.sort();
        node.df.dedup();
    }
}

pub fn compute_dom_level(unit: &TransUnit, func: &str) -> HashMap<BlockId, u32> {
    let mut dom_level = HashMap::new();
    let func = unit.funcs.get(func).unwrap().clone();
    let entry_bb = func.entry_bb;
    dom_level.insert(entry_bb, 0);
    for bb in &func.bbs {
        let block = unit.blocks.get(*bb).unwrap();
        if block.preds.len() == 0 || *bb == entry_bb {
            continue;
        }
        let mut level = 0;
        let mut runner = block.idom;
        while let Some(r) = runner {
            level += 1;
            runner = unit.blocks.get(r).unwrap().idom;
        }
        dom_level.insert(*bb, level);
    }
    dom_level
}

#[derive(Debug)]
pub struct Loop {
    pub parent: Weak<RefCell<Loop>>,
    pub sub_loops: Vec<Rc<RefCell<Loop>>>,
    pub blocks: Vec<BlockId>,
}

impl Loop {
    pub fn new(header: BlockId) -> Self {
        Loop {
            parent: Weak::new(),
            sub_loops: Vec::new(),
            blocks: vec![header],
        }
    }

    pub fn header(&self) -> BlockId {
        self.blocks[0]
    }

    pub fn depth(&self) -> usize {
        let mut depth = 1;
        let mut runner = self.parent.upgrade();
        while let Some(parent) = runner {
            depth += 1;
            runner = parent.borrow().parent.upgrade();
        }
        depth
    }
}

#[derive(Debug)]
pub struct LoopInfo {
    // top level loops
    pub loops: Vec<Rc<RefCell<Loop>>>,
    // deepest loop containing the block
    pub loop_of_bb: HashMap<BlockId, Rc<RefCell<Loop>>>,
}

impl LoopInfo {
    pub fn compute(unit: &mut TransUnit, func: &str) -> Self {
        compute_dom(unit, func);
        let mut worklist = Vec::new();
        let mut vis = HashSet::new();
        let mut loopinfo = LoopInfo {
            loops: Vec::new(),
            loop_of_bb: HashMap::new(),
        };
        let entry = unit.funcs.get(func).unwrap().entry_bb;
        loopinfo.dfs(&mut worklist, unit, entry);
        loopinfo.populate(&mut vis, unit, entry);
        
        // for bb in &unit.funcs[func].bbs {
        //     let block = &unit.blocks[*bb];
        //     eprintln!("bb {} loop depth = {}", block.name, loopinfo.depth(*bb));
        // }

        loopinfo
    }

    fn dfs(&mut self, worklist: &mut Vec<BlockId>, unit: &mut TransUnit, this: BlockId) {
        let block = &unit.blocks[this];
        let children = block.dom.clone();
        for child in children {
            if child == this {
                continue;
            }
            let child_node = &unit.blocks[child];
            // sdom only
            if child_node.idom.is_some() && child_node.idom != Some(this) {
                continue;
            }
            self.dfs(worklist, unit, child);
        }
        assert!(worklist.is_empty());

        // look for predecessors dominated by this node (i.e. back edge)
        for pred in &unit.blocks[this].preds {
            if unit.blocks[this].dom.contains(pred) {
                worklist.push(*pred);
            }
        }
        if !worklist.is_empty() {
            let l = Rc::new(RefCell::new(Loop::new(this)));
            while let Some(bb) = worklist.pop() {
                if self.loop_of_bb.contains_key(&bb) {
                    let mut sub = self.loop_of_bb[&bb].clone();
                    // find outermost loop
                    while let Some(parent) = sub.clone().borrow().parent.upgrade() {
                        sub = parent;
                    }
                    if !Rc::ptr_eq(&sub, &l) {
                        // merge with outermost loop
                        sub.borrow_mut().parent = Rc::downgrade(&l);
                        let header = sub.borrow().header();
                        let header = &unit.blocks[header].preds;
                        worklist.extend(
                            header.iter()
                                .filter(|bb| {
                                    !self.loop_of_bb.contains_key(bb) ||
                                    !Rc::ptr_eq(&self.loop_of_bb[bb], &sub)
                                })
                        );
                    }
                } else {
                    if this != bb {
                        worklist.extend(unit.blocks[bb].preds.iter());
                    }
                    self.loop_of_bb.insert(bb, l.clone());
                }
            }
        }
    }

    fn populate(&mut self, vis: &mut HashSet<BlockId>, unit: &mut TransUnit, this: BlockId) {
        if vis.contains(&this) {
            return;
        }
        vis.insert(this);
        for bb in unit.succ(this) {
            self.populate(vis, unit, bb);
        }
        if let Some(sub) = self.loop_of_bb.get(&this) {
            let mut sub = sub.clone();
            if sub.borrow().header() == this {
                if sub.borrow().parent.upgrade().is_none() {
                    self.loops.push(sub.clone());
                    sub.borrow_mut().blocks[1..].reverse();
                    sub.borrow_mut().sub_loops.reverse();
                } else {
                    sub.borrow().parent.upgrade().unwrap().borrow_mut().sub_loops.push(sub.clone());
                    sub.borrow_mut().blocks[1..].reverse();
                    sub.borrow_mut().sub_loops.reverse();
                    sub = sub.clone().borrow().parent.upgrade().unwrap();
                }
            }
            loop {
                sub.borrow_mut().blocks.push(this);
                if sub.borrow().parent.upgrade().is_none() {
                    break;
                }
                sub = sub.clone().borrow().parent.upgrade().unwrap();
            }
        }
    }

    pub fn depth(&self, bb: BlockId) -> usize {
        self.loop_of_bb
            .get(&bb)
            .map(|l| l.borrow().depth())
            .unwrap_or(0)
    }
}