use std::{fmt::{Display, Debug}, collections::HashMap};

use crate::ctype::Linkage;

use super::{GPOperand, RVGPR, MachineProgram, RV64Instruction, DataLiteral, AsmGlobalObject, FPOperand, RVFPR, OperandBase};

impl Display for MachineProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
    }
}

impl MachineProgram {
    pub fn print(&self, writer: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(writer, "# Built-in library")?;
        let builtin = include_str!("../rt/builtin.S");
        writeln!(writer, "{}", builtin)?;

        let mut counter = 0;

        let mut bb_names = HashMap::new();
        let mut funcs = self.funcs.iter().collect::<Vec<_>>();
        funcs.sort_by_key(|(name, _)| *name);
        for (_, f) in funcs {
            counter += 1;
            let mut counter2 = 0;
            let mut count2 = || {
                counter2 += 1;
                counter2
            };

            writeln!(writer, "\t.section\t.text")?;
            writeln!(writer, "\t.align\t4")?;
            writeln!(writer, "\t.globl\t{}", f.func)?;
            writeln!(writer, "\t.type\t{}, @function", f.func)?;
            writeln!(writer, "{}:", f.func)?;

            for bb in f.bbs.iter() {
                let mbb = &self.blocks[*bb];
                bb_names.insert(*bb, format!(".LBB{}_{}.{}", counter, count2(), mbb.name));
            }

            let bbs = f.bbs.clone();

            for i in 0..bbs.len() {
                let bb = bbs[i];
                let next_bb = bbs.get(i + 1);
                if i != 0 {
                    writeln!(writer, "{}:", bb_names[&bb])?;
                }

                let mut iter = self.blocks[bb].insts_head;
                while let Some(inst_id) = iter {
                    let inst = self.insts[inst_id].clone();
                    iter = inst.next;

                    if matches!(inst.inst, RV64Instruction::NOP) {
                        continue;
                    }

                    let cond = match inst.inst {
                        RV64Instruction::JEQ { rs1, rs2, succ, fail } =>
                            Some(("eq", rs1, rs2, succ, fail)),
                        RV64Instruction::JNE { rs1, rs2, succ, fail } =>
                            Some(("ne", rs1, rs2, succ, fail)),
                        RV64Instruction::JLT { rs1, rs2, succ, fail } =>
                            Some(("lt", rs1, rs2, succ, fail)),
                        RV64Instruction::JGE { rs1, rs2, succ, fail } =>
                            Some(("ge", rs1, rs2, succ, fail)),
                        RV64Instruction::JLTU { rs1, rs2, succ, fail } =>
                            Some(("ltu", rs1, rs2, succ, fail)),
                        RV64Instruction::JGEU { rs1, rs2, succ, fail } =>
                            Some(("geu", rs1, rs2, succ, fail)),
                        _ => None,
                    };
                    if let Some((op, rs1, rs2, succ, fail)) = cond {
                        writeln!(writer, "\tb{}\t{}, {}, {}", op, rs1, rs2, bb_names[&succ])?;
                        if next_bb.is_none() || next_bb.is_some() && *next_bb.unwrap() != fail {
                            writeln!(writer, "\tj\t{}", bb_names[&fail])?;
                        }
                        continue;
                    }

                    if let RV64Instruction::JUMP { target } = inst.inst {
                        if next_bb.is_none() || next_bb.is_some() && *next_bb.unwrap() != target {
                            writeln!(writer, "\tj\t{}", bb_names[&target])?;
                        }
                        continue;
                    }

                    if let RV64Instruction::OFFSET { dest, base } = inst.inst {
                        writeln!(writer, "\t.word\t{}-{}", bb_names[&dest], base)?;
                        continue;
                    }
                    if let RV64Instruction::BLTUl { rs1, rs2, succ } = inst.inst {
                        writeln!(writer, "\tbltu\t{}, {}, {}", rs1, rs2, bb_names[&succ])?;
                        continue;
                    }

                    match inst.inst {
                        RV64Instruction::SEQ { rd, rs1, rs2 } => {
                            writeln!(writer, "\txor\t{}, {}, {}", rd, rs1, rs2)?;
                            writeln!(writer, "\tseqz\t{}, {}", rd, rd)?; // alias for sltiu rd, rd, 1
                        }
                        RV64Instruction::SNE { rd, rs1, rs2 } => {
                            writeln!(writer, "\txor\t{}, {}, {}", rd, rs1, rs2)?;
                            writeln!(writer, "\tsnez\t{}, {}", rd, rd)?; // alias for sltu rd, zero, rd
                        }
                        RV64Instruction::SGE { rd, rs1, rs2 } => {
                            writeln!(writer, "\tslt\t{}, {}, {}", rd, rs1, rs2)?;
                            writeln!(writer, "\txori\t{}, {}, 1", rd, rd)?;
                        }
                        RV64Instruction::SGEU { rd, rs1, rs2 } => {
                            writeln!(writer, "\tsltu\t{}, {}, {}", rd, rs1, rs2)?;
                            writeln!(writer, "\txori\t{}, {}, 1", rd, rd)?;
                        }
                        RV64Instruction::LABEL { comment } => {
                            writeln!(writer, "{}:", comment)?;
                        }
                        _ => {
                            writeln!(writer, "\t{}", inst.inst)?;
                        }
                    }
                }
            }

            writeln!(writer)?;
        }

        let mut symbols = self.symbols.iter().collect::<Vec<_>>();
        symbols.sort_by_key(|(name, _)| *name);
        for (s, AsmGlobalObject{ data, linkage }) in symbols {
            match linkage {
                Linkage::Global => {
                    writeln!(writer, "\t.globl\t{}", s)?;
                }
                Linkage::Static => {},
                Linkage::Extern => {
                    continue;
                }
            }
            if data.iter().all(|d| matches!(d, DataLiteral::Zero(_))) {
                writeln!(writer, "\t.section\t.bss")?;
            } else {
                writeln!(writer, "\t.section\t.data")?;
            }
            writeln!(writer, "\t.type\t{}, @object", s)?;
            writeln!(writer, "{}:", s)?;
            for dd in data {
                writeln!(writer, "\t{}", dd)?;
            }
            writeln!(writer)?;
        }

        let mut tables = self.tables.iter().collect::<Vec<_>>();
        tables.sort_by_key(|(name, _)| *name);
        for (s, data) in tables {
            writeln!(writer, "\t.section\t.rodata")?;
            writeln!(writer, "\t.type\t{}, @object", s)?;
            writeln!(writer, "{}:", s)?;
            for dd in &data.1 {
                writeln!(writer, "\t.word\t{}-{}", bb_names[dd], s)?;
            }
            writeln!(writer)?;
        }

        Ok(())
    }
}

impl Display for RV64Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RV64Instruction::LUI { rd, imm } =>
                write!(f, "lui\t{}, {}", rd, imm),
            RV64Instruction::LB { rd, rs1, imm } =>
                write!(f, "lb\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::LH { rd, rs1, imm } =>
                write!(f, "lh\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::LW { rd, rs1, imm } =>
                write!(f, "lw\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::LBU { rd, rs1, imm } =>
                write!(f, "lbu\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::LHU { rd, rs1, imm } =>
                write!(f, "lhu\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::SB { rs1, rs2, imm } =>
                write!(f, "sb\t{}, {}({})", rs2, imm, rs1),
            RV64Instruction::SH { rs1, rs2, imm } =>
                write!(f, "sh\t{}, {}({})", rs2, imm, rs1),
            RV64Instruction::SW { rs1, rs2, imm } =>
                write!(f, "sw\t{}, {}({})", rs2, imm, rs1),
            RV64Instruction::ADDI { rd, rs1, imm } =>
                write!(f, "addi\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::SLTI { rd, rs1, imm } =>
                write!(f, "slti\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::SLTIU { rd, rs1, imm } =>
                write!(f, "sltiu\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::XORI { rd, rs1, imm } =>
                write!(f, "xori\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::ORI { rd, rs1, imm } =>
                write!(f, "ori\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::ANDI { rd, rs1, imm } =>
                write!(f, "andi\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::SLLI { rd, rs1, imm } =>
                write!(f, "slli\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::SRLI { rd, rs1, imm } =>
                write!(f, "srli\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::SRAI { rd, rs1, imm } =>
                write!(f, "srai\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::ADD { rd, rs1, rs2 } =>
                write!(f, "add\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SUB { rd, rs1, rs2 } =>
                write!(f, "sub\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLL { rd, rs1, rs2 } =>
                write!(f, "sll\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLT { rd, rs1, rs2 } =>
                write!(f, "slt\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLTU { rd, rs1, rs2 } =>
                write!(f, "sltu\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::XOR { rd, rs1, rs2 } =>
                write!(f, "xor\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SRL { rd, rs1, rs2 } =>
                write!(f, "srl\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SRA { rd, rs1, rs2 } =>
                write!(f, "sra\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::OR { rd, rs1, rs2 } =>
                write!(f, "or\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::AND { rd, rs1, rs2 } =>
                write!(f, "and\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::LWU { rd, rs1, imm } =>
                write!(f, "lwu\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::LD { rd, rs1, imm } =>
                write!(f, "ld\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::SD { rs1, rs2, imm } =>
                write!(f, "sd\t{}, {}({})", rs2, imm, rs1),
            RV64Instruction::ADDIW { rd, rs1, imm } =>
                write!(f, "addiw\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::SLLIW { rd, rs1, imm } =>
                write!(f, "slliw\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::SRLIW { rd, rs1, imm } =>
                write!(f, "srliw\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::SRAIW { rd, rs1, imm } =>
                write!(f, "sraiw\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::ADDW { rd, rs1, rs2 } =>
                write!(f, "addw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SUBW { rd, rs1, rs2 } =>
                write!(f, "subw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLLW { rd, rs1, rs2 } =>
                write!(f, "sllw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SRLW { rd, rs1, rs2 } =>
                write!(f, "srlw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SRAW { rd, rs1, rs2 } =>
                write!(f, "sraw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MUL { rd, rs1, rs2 } =>
                write!(f, "mul\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MULH { rd, rs1, rs2 } =>
                write!(f, "mulh\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MULHSU { rd, rs1, rs2 } =>
                write!(f, "mulhsu\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MULHU { rd, rs1, rs2 } =>
                write!(f, "mulhu\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::DIV { rd, rs1, rs2 } =>
                write!(f, "div\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::DIVU { rd, rs1, rs2 } =>
                write!(f, "divu\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::REM { rd, rs1, rs2 } =>
                write!(f, "rem\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::REMU { rd, rs1, rs2 } =>
                write!(f, "remu\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MULW { rd, rs1, rs2 } =>
                write!(f, "mulw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::DIVW { rd, rs1, rs2 } =>
                write!(f, "divw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::DIVUW { rd, rs1, rs2 } =>
                write!(f, "divuw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::REMW { rd, rs1, rs2 } =>
                write!(f, "remw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::REMUW { rd, rs1, rs2 } =>
                write!(f, "remuw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FLW { rd, rs1, imm } =>
                write!(f, "flw\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::FSW { rs1, rs2, imm } =>
                write!(f, "fsw\t{}, {}({})", rs2, imm, rs1),
            RV64Instruction::FMADDS { rd, rs1, rs2, rs3 } =>
                write!(f, "fmadd.s\t{}, {}, {}, {}", rd, rs1, rs2, rs3),
            RV64Instruction::FMSUBS { rd, rs1, rs2, rs3 } =>
                write!(f, "fmsub.s\t{}, {}, {}, {}", rd, rs1, rs2, rs3),
            RV64Instruction::FNMSUBS { rd, rs1, rs2, rs3 } =>
                write!(f, "fnmsub.s\t{}, {}, {}, {}", rd, rs1, rs2, rs3),
            RV64Instruction::FNMADDS { rd, rs1, rs2, rs3 } =>
                write!(f, "fnmadd.s\t{}, {}, {}, {}", rd, rs1, rs2, rs3),
            RV64Instruction::FADDS { rd, rs1, rs2 } =>
                write!(f, "fadd.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FSUBS { rd, rs1, rs2 } =>
                write!(f, "fsub.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FMULS { rd, rs1, rs2 } =>
                write!(f, "fmul.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FDIVS { rd, rs1, rs2 } =>
                write!(f, "fdiv.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FSQRTS { rd, rs1 } =>
                write!(f, "fsqrt.s\t{}, {}", rd, rs1),
            RV64Instruction::FSGNJS { rd, rs1, rs2 } =>
                write!(f, "fsgnj.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FSGNJNS { rd, rs1, rs2 } =>
                write!(f, "fsgnjn.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FSGNJXS { rd, rs1, rs2 } =>
                write!(f, "fsgnjx.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FMINS { rd, rs1, rs2 } =>
                write!(f, "fmin.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FMAXS { rd, rs1, rs2 } =>
                write!(f, "fmax.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FCVTWS { rd, rs1 } =>
                write!(f, "fcvt.w.s\t{}, {}, rtz", rd, rs1),
            RV64Instruction::FCVTWUS { rd, rs1 } =>
                write!(f, "fcvt.wu.s\t{}, {}", rd, rs1),
            RV64Instruction::FMVXW { rd, rs1 } =>
                write!(f, "fmv.x.w\t{}, {}", rd, rs1),
            RV64Instruction::FEQS { rd, rs1, rs2 } =>
                write!(f, "feq.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FLTS { rd, rs1, rs2 } =>
                write!(f, "flt.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FLES { rd, rs1, rs2 } =>
                write!(f, "fle.s\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::FCLASSS { rd, rs1 } =>
                write!(f, "fclass.s\t{}, {}", rd, rs1),
            RV64Instruction::FCVTSW { rd, rs1 } =>
                write!(f, "fcvt.s.w\t{}, {}", rd, rs1),
            RV64Instruction::FCVTSWU { rd, rs1 } =>
                write!(f, "fcvt.s.wu\t{}, {}", rd, rs1),
            RV64Instruction::FMVWX { rd, rs1 } =>
                write!(f, "fmv.w.x\t{}, {}", rd, rs1),
            RV64Instruction::FSD { rs1, rs2, imm } =>
                write!(f, "fsd\t{}, {}({})", rs2, imm, rs1),
            RV64Instruction::FLD { rd, rs1, imm } =>
                write!(f, "fld\t{}, {}({})", rd, imm, rs1),
            RV64Instruction::ADDUW { rd, rs1, rs2 } =>
                write!(f, "add.uw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SH1ADD { rd, rs1, rs2 } =>
                write!(f, "sh1add\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SH1ADDUW { rd, rs1, rs2 } =>
                write!(f, "sh1add.uw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SH2ADD { rd, rs1, rs2 } => {
                match (rd.color(), rs1.color(), rs2.color()) {
                    (Some(rd), Some(rs1), Some(rs2)) => {
                        // registers are allocated, but we need to encode it ourself
                        let instruction: u32 =
                            0b0010000 << 25 |
                            (rs2 as u16 as u32) << 20 |
                            (rs1 as u16 as u32) << 15 |
                            0b100 << 12 |
                            (rd as u16 as u32) << 7 |
                            0b0110011;
                        write!(f, ".word\t0x{instruction:08x}\t# sh2add\t{rd}, {rs1}, {rs2}")
                    }
                    // not allocated, so it won't be used by the assembler, and we call it directly
                    _ => write!(f, "sh2add\t{}, {}, {}", rd, rs1, rs2)
                }
            },
            RV64Instruction::SH2ADDUW { rd, rs1, rs2 } =>
                write!(f, "sh2add.uw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SH3ADD { rd, rs1, rs2 } => {
                match (rd.color(), rs1.color(), rs2.color()) {
                    (Some(rd), Some(rs1), Some(rs2)) => {
                        // registers are allocated, but we need to encode it ourself
                        let instruction: u32 =
                            0b0010000 << 25 |
                            (rs2 as u16 as u32) << 20 |
                            (rs1 as u16 as u32) << 15 |
                            0b110 << 12 |
                            (rd as u16 as u32) << 7 |
                            0b0110011;
                        write!(f, ".word\t0x{:08x}", instruction)
                    }
                    // not allocated, so it won't be used by the assembler, and we call it directly
                    _ => write!(f, "sh3add\t{}, {}, {}", rd, rs1, rs2)
                }
            },
            RV64Instruction::SH3ADDUW { rd, rs1, rs2 } =>
                write!(f, "sh3add.uw\t{}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLLIUW { rd, rs1, imm } =>
                write!(f, "slli.uw\t{}, {}, {}", rd, rs1, imm),
            RV64Instruction::COMMENT { comment } =>
                write!(f, "# {}", comment),
            RV64Instruction::CALL { callee, .. } =>
                write!(f, "call\t{}", callee),
            RV64Instruction::TAIL { callee, .. } =>
                write!(f, "tail\t{}", callee),
            RV64Instruction::RET =>
                write!(f, "ret"),
            RV64Instruction::MV { rd, rs } =>
                write!(f, "mv\t{}, {}", rd, rs),
            RV64Instruction::FMVSS { rd, rs } => 
                write!(f, "fmv.s\t{}, {}", rd, rs),
            RV64Instruction::FMVDD { rd, rs } => 
                write!(f, "fmv.d\t{}, {}", rd, rs),
            RV64Instruction::FNEGS { rd, rs1 } =>
                write!(f, "fneg.s\t{}, {}", rd, rs1),
            RV64Instruction::LIMM { rd, imm } =>
                write!(f, "li\t{}, {}", rd, imm),
            RV64Instruction::LADDR { rd, label } =>
                write!(f, "la\t{}, {}", rd, label),
            RV64Instruction::BEQ { rs1, rs2, imm } => {
                if 4000 <= *imm && *imm <= 4009 {
                    // forward local label
                    write!(f, "beq\t{}, {}, {}f", rs1, rs2, imm - 4000)
                } else if 6000 <= *imm && *imm <= 6009 {
                    // backward local label
                    write!(f, "beq\t{}, {}, {}b", rs1, rs2, imm - 6000)
                } else {
                    panic!("unimplemented branch: {}", imm);
                }
            }
            RV64Instruction::BLTU { rs1, rs2, imm } => {
                if 4000 <= *imm && *imm <= 4009 {
                    // forward local label
                    write!(f, "bltu\t{}, {}, {}f", rs1, rs2, imm - 4000)
                } else if 6000 <= *imm && *imm <= 6009 {
                    // backward local label
                    write!(f, "bltu\t{}, {}, {}b", rs1, rs2, imm - 6000)
                } else {
                    panic!("unimplemented branch: {}", imm);
                }
            }
            RV64Instruction::JALR { rd, rs1, imm } => {
                if *imm == 0 && *rd == GPOperand::PreColored(RVGPR::X0) {
                    write!(f, "jr\t{}", rs1)
                } else {
                    write!(f, "jalr\t{}, {}({})", rd, imm, rs1)
                }
            }
            RV64Instruction::ENTER =>
                write!(f, "enter"),
            RV64Instruction::LEAVE =>
                write!(f, "leave"),
            RV64Instruction::JUMP { target } =>
                write!(f, "j\tid:{:?}", target),
            RV64Instruction::NOP =>
                write!(f, "nop"),
            _ => panic!("unimplemented instruction: {:?}", self),
        }
    }
}

impl Display for GPOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GPOperand::Virtual(reg) => write!(f, "{}", reg),
            GPOperand::PreColored(reg) => write!(f, "{}", reg),
            GPOperand::Allocated(reg) => write!(f, "{}", reg),
        }
    }
}

impl Display for FPOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FPOperand::Virtual(v) => write!(f, "%{}f", v.id()),
            FPOperand::Allocated(r) => write!(f, "{}", r),
            FPOperand::PreColored(r) => write!(f, "{}", r),
        }
    }
}

impl Display for RVGPR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Display for RVFPR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Display for DataLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataLiteral::Byte(b) => write!(f, ".byte\t{}", b),
            DataLiteral::Word(w) => write!(f, ".word\t{}", w),
            DataLiteral::WordHex(w) => write!(
                f,
                ".word\t0x{:08x}                        # float {}",
                w,
                unsafe {
                    let f: f32 = std::mem::transmute(*w);
                    f
                }
            ),
            DataLiteral::Zero(size) => write!(f, ".zero\t{}", size),
        }
    }
}