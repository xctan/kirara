use super::import::*;

impl MachineProgram {
    pub fn setup_stack(&mut self) {
        let funcs = self.funcs.keys().cloned().collect::<Vec<_>>();
        for func in funcs {
            let mfunc = self.funcs.get_mut(&func).unwrap();
            let local_size = mfunc.stack_size;
            
            let callee_saved = mfunc.used_regs
                .iter()
                .filter(|reg| reg.is_callee_saved())
                .collect::<Vec<_>>();
            let thin = mfunc.used_regs
                .iter()
                .all(|reg| reg != &RVGPR::ra() && reg != &RVGPR::fp());
            let callee_saved = callee_saved
                .into_iter()
                .filter(|reg| reg != &&RVGPR::ra() && reg != &&RVGPR::fp())
                .collect::<Vec<_>>();
            let saved_size = if thin {
                callee_saved.len() as u32 * 8
            } else {
                callee_saved.len() as u32 * 8 + 16
            };
            let stack_size = local_size + saved_size;

            // alignment requirement
            let stack_size = if stack_size % 16 == 0 {
                stack_size
            } else {
                stack_size + 16 - stack_size % 16
            };

            macro_rules! r {
                (a0) => { GPOperand::PreColored(RVGPR::a(0)) };
                (t0) => { GPOperand::PreColored(RVGPR::t(0)) };
                (fp) => { GPOperand::PreColored(RVGPR::s(0)) };
                ($ident:ident) => { GPOperand::PreColored(RVGPR::$ident()) };
            }

            let mut prologue = vec![];
            macro_rules! i {
                ($mnemonic:ident $($operand:expr),*) => {
                    prologue.push(RV64InstBuilder::$mnemonic($($operand),*))
                };
            }
            let small_stack_size = u32::min(stack_size, 2032);
            let saved_offset = |idx| {
                small_stack_size as i32 - 8 - idx as i32 * 8
            };
            if small_stack_size != 0 {
                i!(ADDI r!(sp), r!(sp), -(small_stack_size as i32));
            }
            let save_adj = if thin {
                0
            } else {
                i!(SD r!(ra), r!(sp), saved_offset(0));
                i!(SD r!(fp), r!(sp), saved_offset(1));
                i!(ADDI r!(fp), r!(sp), small_stack_size as i32);
                2
            };
            for (idx, reg) in callee_saved.iter().enumerate() {
                i!(SD GPOperand::PreColored(**reg), r!(sp), saved_offset(idx as u32 + save_adj));
            }
            let rem_stack_size = stack_size - small_stack_size;
            if rem_stack_size != 0 {
                if super::codegen::is_imm12(-(rem_stack_size as i32)) {
                    i!(ADDI r!(sp), r!(sp), -(rem_stack_size as i32));
                } else {
                    i!(LIMM r!(t0), -(rem_stack_size as i32));
                    i!(ADD r!(sp), r!(sp), r!(t0));
                }
            }

            let mut epilogue = vec![];
            macro_rules! i {
                ($mnemonic:ident $($operand:expr),*) => {
                    epilogue.push(RV64InstBuilder::$mnemonic($($operand),*))
                };
            }
            if rem_stack_size != 0 {
                if super::codegen::is_imm12(rem_stack_size as i32) {
                    i!(ADDI r!(sp), r!(sp), rem_stack_size as i32);
                } else {
                    if thin {
                        i!(LIMM r!(t0), rem_stack_size as i32);
                        i!(ADD r!(sp), r!(t0), r!(sp));
                    } else {
                        i!(ADDI r!(sp), r!(fp), -(small_stack_size as i32));
                    }
                }
            }
            for (idx, reg) in callee_saved.iter().enumerate() {
                i!(LD GPOperand::PreColored(**reg), r!(sp), saved_offset(idx as u32 + save_adj));
            }
            if !thin {
                i!(LD r!(fp), r!(sp), saved_offset(1));
                i!(LD r!(ra), r!(sp), saved_offset(0));
            }
            if small_stack_size != 0 {
                i!(ADDI r!(sp), r!(sp), small_stack_size as i32);
            }

            // let caller_saved = mfunc.used_regs
            //     .iter()
            //     .filter(|reg| reg.is_caller_saved())
            //     .collect::<Vec<_>>();
            // let mut call_prologue = vec![];
            // let mut call_epilogue = vec![];

            let bbs = mfunc.bbs.clone();
            for bb in bbs {
                let mut iter = self.blocks[bb].insts_head;
                while let Some(inst) = iter {
                    let insn = self.insts[inst].clone();
                    iter = insn.next;

                    match insn.inst {
                        RV64Instruction::ENTER => {
                            for i in prologue.iter() {
                                self.insert_before(inst, i.clone());
                            }
                            self.remove(inst);
                        }
                        RV64Instruction::LEAVE => {
                            for i in epilogue.iter() {
                                self.insert_before(inst, i.clone());
                            }
                            self.remove(inst);
                        }
                        _ => continue,
                    }
                }
            }
        }
    }
}
