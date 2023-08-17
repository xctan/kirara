use std::collections::{HashSet, HashMap};

use super::{structure::TransUnit, value::InstructionValue, IrPass};

pub struct ComputeCallGraph;

impl IrPass for ComputeCallGraph {
    fn run(&self, unit: &mut TransUnit) {
        unit.compute_callgraph();
    }
}

#[derive(Debug, Clone)]
pub struct CallGraphInfo {
    pub caller: HashSet<String>,
    pub callee: HashSet<String>,
}

impl CallGraphInfo {
    pub fn new() -> Self {
        Self {
            caller: HashSet::new(),
            callee: HashSet::new(),
        }
    }
}

impl Default for CallGraphInfo {
    fn default() -> Self {
        Self::new()
    }
}

impl TransUnit {
    pub fn compute_callgraph(&mut self) {
        let cg = self.do_compute_callgraph();
        self.callgraph = cg;
    }

    fn do_compute_callgraph(&self) -> HashMap<String, CallGraphInfo> {
        let mut res = HashMap::new();
        
        for (decl, _ty) in &self.external {
            // external functions
            res.insert(decl.clone(), CallGraphInfo::new());
        }

        for (name, func) in &self.funcs {
            res.entry(name.clone()).or_default();
            for &bb in &func.bbs {
                let block = &self.blocks[bb];
                let mut iter = block.insts_start;
                while let Some(vid) = iter {
                    let inst = &self.values[vid];
                    iter = inst.next;

                    let inst = inst.value.as_inst();
                    match inst {
                        InstructionValue::Call(c) => {
                            res.entry(name.clone()).or_default().callee.insert(c.func.clone());
                            res.entry(c.func.clone()).or_default().caller.insert(name.clone());
                        },
                        InstructionValue::TailCall(c) => {
                            res.entry(name.clone()).or_default().callee.insert(c.func.clone());
                            res.entry(c.func.clone()).or_default().caller.insert(name.clone());
                        },
                        _ => {},
                    }
                }
            }
        }

        // println!("callgraph: {:#?}", res);

        res
    }

}