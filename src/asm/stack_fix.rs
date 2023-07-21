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
            let saved_size = callee_saved.len() as u32 * 8;
            let stack_size = local_size + saved_size;

            // alignment requirement
            let stack_size = if stack_size % 16 == 0 {
                stack_size
            } else {
                stack_size + 16 - stack_size % 16
            };

            macro_rules! r {
                (a0) => { MachineOperand::PreColored(RVGPR::a(0)) };
                (fp) => { MachineOperand::PreColored(RVGPR::s(0)) };
                ($ident:ident) => { MachineOperand::PreColored(RVGPR::$ident()) };
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
            i!(ADDI r!(sp), r!(sp), -(small_stack_size as i32));
            i!(SD r!(ra), r!(sp), saved_offset(0));
            i!(SD r!(fp), r!(sp), saved_offset(1));
            i!(ADDI r!(fp), r!(sp), small_stack_size as i32);
            for (idx, reg) in callee_saved.iter().enumerate() {
                i!(SD r!(sp), MachineOperand::PreColored(**reg), saved_offset(idx as u32 + 2));
            }
            let rem_stack_size = stack_size - small_stack_size;
            if rem_stack_size != 0 {
                if super::codegen::is_imm12(-(rem_stack_size as i32)) {
                    i!(ADDI r!(sp), r!(sp), -(rem_stack_size as i32));
                } else {
                    let (hi, lo) = super::codegen::split_imm32(-(rem_stack_size as i32));
                    i!(LUI r!(fp), hi);
                    i!(ADDI r!(sp), r!(fp), lo);
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
                    let (hi, lo) = super::codegen::split_imm32(rem_stack_size as i32);
                    i!(LUI r!(fp), hi);
                    i!(ADDI r!(sp), r!(fp), lo);
                }
            }
            for (idx, reg) in callee_saved.iter().enumerate() {
                i!(LD MachineOperand::PreColored(**reg), r!(sp), saved_offset(idx as u32 + 2));
            }
            i!(LD r!(fp), r!(sp), saved_offset(1));
            i!(LD r!(ra), r!(sp), saved_offset(0));
            i!(ADDI r!(sp), r!(sp), small_stack_size as i32);

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
