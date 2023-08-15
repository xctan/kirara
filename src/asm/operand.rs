use crate::{implement_typed_instruction, implement_typed_float_instruction};

use super::include::*;

pub trait OperandInfo<O, R, V>
where
    O: Operand<R, V>,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    /// Returns Some(dst, src) if the instruction is a move
    fn get_move(&self) -> Option<(O, O)>;
    /// Returns register (definitions, usages) of the instruction
    fn get_def_use(&self) -> (Vec<O>, Vec<O>);
    fn get_def_mut(&mut self) -> Vec<&mut O>;
    fn get_use_mut(&mut self) -> Vec<&mut O>;
}

// TODO: generate this file by procedural macro

impl OperandInfo<GPOperand, RVGPR, VirtGPR> for RV64Instruction {
    fn get_move(&self) -> Option<(GPOperand, GPOperand)> {
        if let RV64Instruction::MV{ rd, rs } = self {
            Some((*rd, *rs))
        } else {
            None
        }
    }

    fn get_def_use(&self) -> (Vec<GPOperand>, Vec<GPOperand>) {
        macro_rules! extract {
            (r ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return (vec![*rd], vec![*rs1, *rs2])
                }
            };
            (i ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rd, rs1, .. } = self {
                    return (vec![*rd], vec![*rs1])
                }
            };
            (s; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rs1, rs2, .. } = self {
                    return (vec![], vec![*rs1, *rs2])
                }
            };
            (cj; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rs1, rs2, .. } = self {
                    return (vec![], vec![*rs1, *rs2])
                }
            };
            (fi; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, imm } = self {
                    return (vec![], vec![*rs1])
                }
            };
            (fs; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rs2, rs1, imm } = self {
                    return (vec![], vec![*rs1])
                }
            };
            (fma; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2, rs3 } = self {
                    return (vec![], vec![])
                }
            };
            (fr; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return (vec![], vec![])
                }
            };
            (fcmp; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return (vec![*rd], vec![])
                }
            };
        }

        implement_typed_instruction!(extract);
        implement_typed_float_instruction!(extract);

        if let RV64Instruction::FMVWX { rs1, .. } = self {
            return (vec![], vec![*rs1])
        }
        if let RV64Instruction::FCVTWS { rd, .. } = self {
            return (vec![*rd], vec![])
        }
        if let RV64Instruction::FNEGS { .. } = self {
            return (vec![], vec![])
        }
        if let RV64Instruction::FCVTWS { rd, .. } = self {
            return (vec![*rd], vec![])
        }
        if let RV64Instruction::FCVTSW { rs1, .. } = self {
            return (vec![], vec![*rs1])
        }
        if let RV64Instruction::FMVSS { .. } = self {
            return (vec![], vec![])
        }
        if let RV64Instruction::FMVDD { .. } = self {
            return (vec![], vec![])
        }
        
        if let RV64Instruction::LUI { rd, .. } = self {
            return (vec![*rd], vec![])
        }
        if let RV64Instruction::MV { rd, rs } = self {
            return (vec![*rd], vec![*rs])
        }
        if let RV64Instruction::LIMM { rd, .. } = self {
            return (vec![*rd], vec![])
        }
        if let RV64Instruction::LADDR { rd, .. } = self {
            return (vec![*rd], vec![])
        }
        if let RV64Instruction::RET = self {
            return (vec![], vec![GPOperand::PreColored(RVGPR::a(0))]);
        }
        if let RV64Instruction::JUMP { .. } = self {
            return (vec![], vec![]);
        }
        if matches!(self,
            RV64Instruction::ENTER | RV64Instruction::LEAVE | RV64Instruction::COMMENT { .. }
            | RV64Instruction::LABEL { .. }
        ) {
            return (vec![], vec![]);
        }
        if let RV64Instruction::NOP = self {
            return (vec![], vec![]);
        }
        if let RV64Instruction::CALL { params, .. } = self {
            let uses: Vec<_> = params
                .iter()
                .take(8)
                .enumerate()
                .filter(|(_, t)| !**t)
                .map(|(i, _)| GPOperand::PreColored(RVGPR::a(i)))
                .collect();
            let defs: Vec<_> = (0..=7).map(|i| GPOperand::PreColored(RVGPR::a(i)))
                .chain((0..=6).map(|i| GPOperand::PreColored(RVGPR::t(i))))
                .collect();
            return (defs, uses);
        }
        if let RV64Instruction::TAIL { params, .. } = self {
            let uses: Vec<_> = params
                .iter()
                .take(8)
                .enumerate()
                .filter(|(_, t)| !**t)
                .map(|(i, _)| GPOperand::PreColored(RVGPR::a(i)))
                .collect();
            let defs: Vec<_> = (0..=7).map(|i| GPOperand::PreColored(RVGPR::a(i)))
                .chain((0..=6).map(|i| GPOperand::PreColored(RVGPR::t(i))))
                .collect();
            return (defs, uses);
        }

        panic!("unimplemented: {:?}", self)
    }

    fn get_def_mut(&mut self) -> Vec<&mut GPOperand> {
        macro_rules! extract {
            (r ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rd, .. } = self {
                    return vec![rd]
                }
            };
            (i ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rd, .. } = self {
                    return vec![rd]
                }
            };
            (s; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (cj; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (fi; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, imm } = self {
                    return vec![]
                }
            };
            (fs; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rs2, rs1, imm } = self {
                    return vec![]
                }
            };
            (fma; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2, rs3 } = self {
                    return vec![]
                }
            };
            (fr; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![]
                }
            };
            (fcmp; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![rd]
                }
            };
        }

        implement_typed_instruction!(extract);
        implement_typed_float_instruction!(extract);

        if let RV64Instruction::FMVWX { .. } = self {
            return vec![]
        }
        if let RV64Instruction::FCVTWS { rd, .. } = self {
            return vec![rd]
        }
        if let RV64Instruction::FNEGS { .. } = self {
            return vec![]
        }
        if let RV64Instruction::FCVTWS { rd, .. } = self {
            return vec![rd]
        }
        if let RV64Instruction::FCVTSW { .. } = self {
            return vec![]
        }
        if let RV64Instruction::FMVSS { .. } = self {
            return vec![]
        }
        if let RV64Instruction::FMVDD { .. } = self {
            return vec![]
        }

        if let RV64Instruction::LUI { rd, .. } = self {
            return vec![rd];
        }
        if let RV64Instruction::MV { rd, .. } = self {
            return vec![rd];
        }
        if let RV64Instruction::LIMM { rd, .. } = self {
            return vec![rd];
        }
        if let RV64Instruction::LADDR { rd, .. } = self {
            return vec![rd]
        }

        macro_rules! nop {
            ($mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![];
                }
            };
        }
        nop!(RET);
        nop!(JUMP);
        nop!(NOP);
        nop!(ENTER);
        nop!(LEAVE);
        nop!(COMMENT);
        nop!(LABEL);
        nop!(CALL);
        nop!(TAIL);

        panic!("unimplemented: {:?}", self)
    }

    fn get_use_mut(&mut self) -> Vec<&mut GPOperand> {
        macro_rules! extract {
            (r ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rs1, rs2, .. } = self {
                    return vec![rs1, rs2]
                }
            };
            (i ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rs1, .. } = self {
                    return vec![rs1]
                }
            };
            (s; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rs1, rs2, .. } = self {
                    return vec![rs1, rs2]
                }
            };
            (cj; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rs1, rs2, .. } = self {
                    return vec![rs1, rs2]
                }
            };
            (fi; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, imm } = self {
                    return vec![rs1]
                }
            };
            (fs; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rs2, rs1, imm } = self {
                    return vec![rs1]
                }
            };
            (fma; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2, rs3 } = self {
                    return vec![]
                }
            };
            (fr; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![]
                }
            };
            (fcmp; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![]
                }
            };
        }
        implement_typed_instruction!(extract);
        implement_typed_float_instruction!(extract);

        if let RV64Instruction::MV { rs, .. } = self {
            return vec![rs];
        }

        macro_rules! nop {
            ($mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![];
                }
            };
        }
        nop!(LUI);
        nop!(LIMM);
        nop!(LADDR);
        nop!(RET);
        nop!(JUMP);
        nop!(NOP);
        nop!(ENTER);
        nop!(LEAVE);
        nop!(COMMENT);
        nop!(LABEL);
        nop!(CALL);
        nop!(TAIL);

        if let RV64Instruction::FMVWX { rs1, .. } = self {
            return vec![rs1]
        }
        nop!(FCVTWS);
        nop!(FNEGS);
        nop!(FCVTWS);
        if let RV64Instruction::FCVTSW { rs1, .. } = self {
            return vec![rs1]
        }
        nop!(FMVSS);
        nop!(FMVDD);

        panic!("unimplemented: {:?}", self)
    }
}

impl OperandInfo<FPOperand, RVFPR, VirtFPR> for RV64Instruction {
    fn get_move(&self) -> Option<(FPOperand, FPOperand)> {
        if let RV64Instruction::FMVSS{ rd, rs } = self {
            Some((*rd, *rs))
        } else if let RV64Instruction::FMVDD{ rd, rs } = self {
            Some((*rd, *rs))
        } else {
            None
        }
    }

    fn get_def_use(&self) -> (Vec<FPOperand>, Vec<FPOperand>) {
        macro_rules! extract {
            (r ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return (vec![], vec![])
                }
            };
            (i ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return (vec![], vec![])
                }
            };
            (s; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return (vec![], vec![])
                }
            };
            (cj; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return (vec![], vec![])
                }
            };
            (fi; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, imm } = self {
                    return (vec![*rd], vec![])
                }
            };
            (fs; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rs2, rs1, imm } = self {
                    return (vec![], vec![*rs2])
                }
            };
            (fma; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2, rs3 } = self {
                    return (vec![*rd], vec![*rs1, *rs2, *rs3])
                }
            };
            (fr; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return (vec![*rd], vec![*rs1, *rs2])
                }
            };
            (fcmp; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return (vec![], vec![*rs1, *rs2])
                }
            };
        }

        implement_typed_instruction!(extract);
        implement_typed_float_instruction!(extract);

        if let RV64Instruction::FMVWX { rd, .. } = self {
            return (vec![*rd], vec![])
        }
        if let RV64Instruction::FCVTWS { rs1, .. } = self {
            return (vec![], vec![*rs1])
        }
        if let RV64Instruction::FNEGS { .. } = self {
            return (vec![], vec![])
        }
        if let RV64Instruction::FCVTWS { rs1, .. } = self {
            return (vec![], vec![*rs1])
        }
        if let RV64Instruction::FCVTSW { rd, .. } = self {
            return (vec![*rd], vec![])
        }
        if let RV64Instruction::FMVSS { rd, rs } = self {
            return (vec![*rd], vec![*rs])
        }
        if let RV64Instruction::FMVDD { rd, rs } = self {
            return (vec![*rd], vec![*rs])
        }
        
        if let RV64Instruction::LUI { .. } = self {
            return (vec![], vec![])
        }
        if let RV64Instruction::MV { .. } = self {
            return (vec![], vec![])
        }
        if let RV64Instruction::LIMM { .. } = self {
            return (vec![], vec![])
        }
        if let RV64Instruction::LADDR { .. } = self {
            return (vec![], vec![])
        }
        if let RV64Instruction::RET = self {
            // estimate the return value
            return (vec![], vec![FPOperand::PreColored(RVFPR::fa(0))]);
        }
        if let RV64Instruction::JUMP { .. } = self {
            return (vec![], vec![]);
        }
        if matches!(self, 
            RV64Instruction::ENTER | RV64Instruction::LEAVE | RV64Instruction::COMMENT { .. }
            | RV64Instruction::LABEL { .. }
        ) {
            return (vec![], vec![]);
        }
        if let RV64Instruction::NOP = self {
            return (vec![], vec![]);
        }
        if let RV64Instruction::CALL { params, .. } = self {
            let uses: Vec<_> = params
                .iter()
                .take(8)
                .enumerate()
                .filter(|(_, t)| **t)
                .map(|(i, _)| FPOperand::PreColored(RVFPR::fa(i)))
                .collect();
            let defs: Vec<_> = (0..=7).map(|i| FPOperand::PreColored(RVFPR::fa(i)))
                .chain((0..=11).map(|i| FPOperand::PreColored(RVFPR::ft(i))))
                .collect();
            return (defs, uses);
        }
        if let RV64Instruction::TAIL { params, .. } = self {
            let uses: Vec<_> = params
                .iter()
                .take(8)
                .enumerate()
                .filter(|(_, t)| **t)
                .map(|(i, _)| FPOperand::PreColored(RVFPR::fa(i)))
                .collect();
            let defs: Vec<_> = (0..=7).map(|i| FPOperand::PreColored(RVFPR::fa(i)))
                .chain((0..=11).map(|i| FPOperand::PreColored(RVFPR::ft(i))))
                .collect();
            return (defs, uses);
        }

        panic!("unimplemented: {:?}", self)
    }

    fn get_def_mut(&mut self) -> Vec<&mut FPOperand> {
        macro_rules! extract {
            (r ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (i ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (s; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (cj; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (fi; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, imm } = self {
                    return vec![rd]
                }
            };
            (fs; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rs2, rs1, imm } = self {
                    return vec![]
                }
            };
            (fma; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2, rs3 } = self {
                    return vec![rd]
                }
            };
            (fr; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![rd]
                }
            };
            (fcmp; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![]
                }
            };
        }

        implement_typed_instruction!(extract);
        implement_typed_float_instruction!(extract);

        if let RV64Instruction::FMVWX { rd, .. } = self {
            return vec![rd]
        }
        if let RV64Instruction::FCVTWS { .. } = self {
            return vec![]
        }
        if let RV64Instruction::FNEGS { rd, .. } = self {
            return vec![rd]
        }
        if let RV64Instruction::FCVTWS { .. } = self {
            return vec![]
        }
        if let RV64Instruction::FCVTSW { rd, .. } = self {
            return vec![rd]
        }
        if let RV64Instruction::FMVSS { rd, .. } = self {
            return vec![rd]
        }
        if let RV64Instruction::FMVDD { rd, .. } = self {
            return vec![rd]
        }

        if let RV64Instruction::LUI { .. } = self {
            return vec![];
        }
        if let RV64Instruction::MV { .. } = self {
            return vec![];
        }
        if let RV64Instruction::LIMM { .. } = self {
            return vec![];
        }
        if let RV64Instruction::LADDR { .. } = self {
            return vec![]
        }

        macro_rules! nop {
            ($mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![];
                }
            };
        }
        nop!(RET);
        nop!(JUMP);
        nop!(NOP);
        nop!(ENTER);
        nop!(LEAVE);
        nop!(COMMENT);
        nop!(LABEL);
        nop!(CALL);
        nop!(TAIL);

        panic!("unimplemented: {:?}", self)
    }

    fn get_use_mut(&mut self) -> Vec<&mut FPOperand> {
        macro_rules! extract {
            (r ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (i ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (s; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (cj; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![]
                }
            };
            (fi; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, imm } = self {
                    return vec![]
                }
            };
            (fs; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rs2, rs1, imm } = self {
                    return vec![rs2]
                }
            };
            (fma; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2, rs3 } = self {
                    return vec![rs1, rs2, rs3]
                }
            };
            (fr; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![rs1, rs2]
                }
            };
            (fcmp; $mnemonic:ident) => {
                #[allow(non_snake_case, unused)]
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![rs1, rs2]
                }
            };
        }
        implement_typed_instruction!(extract);
        implement_typed_float_instruction!(extract);

        if let RV64Instruction::MV { .. } = self {
            return vec![];
        }

        macro_rules! nop {
            ($mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { .. } = self {
                    return vec![];
                }
            };
        }
        nop!(LUI);
        nop!(LIMM);
        nop!(LADDR);
        nop!(RET);
        nop!(JUMP);
        nop!(NOP);
        nop!(ENTER);
        nop!(LEAVE);
        nop!(COMMENT);
        nop!(LABEL);
        nop!(CALL);
        nop!(TAIL);

        nop!(FMVWX);
        if let RV64Instruction::FMVXW { rs1, .. } = self {
            return vec![rs1]
        }
        if let RV64Instruction::FCVTWS { rs1, .. } = self {
            return vec![rs1]
        }
        if let RV64Instruction::FNEGS { rs1, .. } = self {
            return vec![rs1]
        }
        if let RV64Instruction::FCVTWS { rs1, .. } = self {
            return vec![rs1]
        }
        if let RV64Instruction::FCVTSW { .. } = self {
            return vec![]
        }
        if let RV64Instruction::FMVSS { rs, .. } = self {
            return vec![rs]
        }
        if let RV64Instruction::FMVDD { rs, .. } = self {
            return vec![rs]
        }

        panic!("unimplemented: {:?}", self)
    }
}