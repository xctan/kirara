use super::{MachineProgram, RV64Instruction};

impl MachineProgram {
    pub fn simplify(&mut self) {
        let funcs = self.funcs.keys().cloned().collect::<Vec<_>>();
        for func in funcs {
            simplify_func(self, &func);
        }
    }
}

fn simplify_func(unit: &mut MachineProgram, func: &str) {
    let bbs = unit.funcs.get(func).unwrap().bbs.clone();
    for bb in &bbs {
        let mut iter = unit.blocks[*bb].insts_head;
        while let Some(inst) = iter {
            let insn = unit.insts.get(inst).unwrap().clone();
            iter = insn.next;

            if let RV64Instruction::MV { rd, rs } = insn.inst {
                if rd == rs {
                    unit.remove(inst);
                }
            }
        }
    }
}