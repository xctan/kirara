use std::{fmt::{Display, Debug}, collections::{HashMap, HashSet}};

use super::{MachineOperand, RVGPR, MachineProgram, RV64Instruction};

impl MachineProgram {
    pub fn print(&self) {
        let mut counter = 0;

        for (_, f) in &self.funcs {
            counter += 1;
            let mut counter2 = 0;
            let mut count2 = || {
                counter2 += 1;
                counter2
            };

            println!("{}:", f.func);

            let mut bb_names = HashMap::new();
            for bb in f.bbs.iter() {
                bb_names.insert(*bb, format!(".LBB{}_{}", counter, count2()));
            }

            // reschedule the order of basic blocks
            let mut bbs = Vec::with_capacity(f.bbs.len());
            let mut vis = HashSet::new();
            let mut worklist = vec![f.bbs[0]];
            while let Some(current) = worklist.pop() {
                if vis.contains(&current) {
                    continue;
                }
                vis.insert(current);
                bbs.push(current);
                let mbb = &self.blocks[current];
                let final_inst = mbb.insts_tail.unwrap();
                let inst = self.insts[final_inst].clone();
                match inst.inst {
                    RV64Instruction::JUMP { target } => {
                        worklist.push(target);
                    },
                    RV64Instruction::JEQ { succ, fail, .. } => {
                        worklist.push(succ);
                        worklist.push(fail);
                    },
                    RV64Instruction::JNE { succ, fail, .. } => {
                        worklist.push(succ);
                        worklist.push(fail);
                    },
                    RV64Instruction::JLT { succ, fail, .. } => {
                        worklist.push(succ);
                        worklist.push(fail);
                    },
                    RV64Instruction::JGE { succ, fail, .. } => {
                        worklist.push(succ);
                        worklist.push(fail);
                    },
                    RV64Instruction::JLTU { succ, fail, .. } => {
                        worklist.push(succ);
                        worklist.push(fail);
                    },
                    RV64Instruction::JGEU { succ, fail, .. } => {
                        worklist.push(succ);
                        worklist.push(fail);
                    },
                    _ => ()
                };
            }

            for i in 0..bbs.len() {
                let bb = bbs[i];
                let next_bb = bbs.get(i + 1);
                if i != 0 {
                    println!("{}:", bb_names[&bb]);
                }

                let mut iter = self.blocks[bb].insts_head;
                while let Some(inst_id) = iter {
                    let inst = self.insts[inst_id].clone();
                    iter = inst.next;

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
                        println!("  b{} {}, {}, {}", op, rs1, rs2, bb_names[&succ]);
                        if next_bb.is_none() || next_bb.is_some() && *next_bb.unwrap() != fail {
                            println!("  j {}", bb_names[&fail]);
                        }
                        continue;
                    }

                    if let RV64Instruction::JUMP { target } = inst.inst {
                        if next_bb.is_none() || next_bb.is_some() && *next_bb.unwrap() != target {
                            println!("  j {}", bb_names[&target]);
                        }
                        continue;
                    }

                    // if inst.inlined {
                    //     continue;
                    // }
                    match inst.inst {
                        RV64Instruction::SEQ { rd, rs1, rs2 } => {
                            println!("  xor {}, {}, {}", rd, rs1, rs2);
                            println!("  seqz {}", rd); // alias for sltiu rd, rd, 1
                        }
                        RV64Instruction::SNE { rd, rs1, rs2 } => {
                            println!("  xor {}, {}, {}", rd, rs1, rs2);
                            println!("  snez {}", rd); // alias for sltu rd, zero, rd
                        }
                        RV64Instruction::SGE { rd, rs1, rs2 } => {
                            println!("  slt {}, {}, {}", rd, rs1, rs2);
                            println!("  xori {}, {}, 1", rd, rd);
                        }
                        RV64Instruction::SGEU { rd, rs1, rs2 } => {
                            println!("  sltu {}, {}, {}", rd, rs1, rs2);
                            println!("  xori {}, {}, 1", rd, rd);
                        }
                        _ => println!("  {}", inst.inst),
                    }
                }
            }
        }
    }
}

impl Display for RV64Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RV64Instruction::LUI { rd, imm } =>
                write!(f, "lui {}, {}", rd, imm),
            RV64Instruction::LB { rd, rs1, imm } =>
                write!(f, "lb {}, {}({})", rd, imm, rs1),
            RV64Instruction::LH { rd, rs1, imm } =>
                write!(f, "lh {}, {}({})", rd, imm, rs1),
            RV64Instruction::LW { rd, rs1, imm } =>
                write!(f, "lw {}, {}({})", rd, imm, rs1),
            RV64Instruction::LBU { rd, rs1, imm } =>
                write!(f, "lbu {}, {}({})", rd, imm, rs1),
            RV64Instruction::LHU { rd, rs1, imm } =>
                write!(f, "lhu {}, {}({})", rd, imm, rs1),
            RV64Instruction::SB { rs1, rs2, imm } =>
                write!(f, "sb {}, {}({})", rs2, imm, rs1),
            RV64Instruction::SH { rs1, rs2, imm } =>
                write!(f, "sh {}, {}({})", rs2, imm, rs1),
            RV64Instruction::SW { rs1, rs2, imm } =>
                write!(f, "sw {}, {}({})", rs2, imm, rs1),
            RV64Instruction::ADDI { rd, rs1, imm } =>
                write!(f, "addi {}, {}, {}", rd, rs1, imm),
            RV64Instruction::SLTI { rd, rs1, imm } =>
                write!(f, "slti {}, {}, {}", rd, rs1, imm),
            RV64Instruction::SLTIU { rd, rs1, imm } =>
                write!(f, "sltiu {}, {}, {}", rd, rs1, imm),
            RV64Instruction::XORI { rd, rs1, imm } =>
                write!(f, "xori {}, {}, {}", rd, rs1, imm),
            RV64Instruction::ORI { rd, rs1, imm } =>
                write!(f, "ori {}, {}, {}", rd, rs1, imm),
            RV64Instruction::ANDI { rd, rs1, imm } =>
                write!(f, "andi {}, {}, {}", rd, rs1, imm),
            RV64Instruction::SLLI { rd, rs1, imm } =>
                write!(f, "slli {}, {}, {}", rd, rs1, imm),
            RV64Instruction::SRLI { rd, rs1, imm } =>
                write!(f, "srli {}, {}, {}", rd, rs1, imm),
            RV64Instruction::SRAI { rd, rs1, imm } =>
                write!(f, "srai {}, {}, {}", rd, rs1, imm),
            RV64Instruction::ADD { rd, rs1, rs2 } =>
                write!(f, "add {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SUB { rd, rs1, rs2 } =>
                write!(f, "sub {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLL { rd, rs1, rs2 } =>
                write!(f, "sll {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLT { rd, rs1, rs2 } =>
                write!(f, "slt {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLTU { rd, rs1, rs2 } =>
                write!(f, "sltu {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::XOR { rd, rs1, rs2 } =>
                write!(f, "xor {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SRL { rd, rs1, rs2 } =>
                write!(f, "srl {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SRA { rd, rs1, rs2 } =>
                write!(f, "sra {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::OR { rd, rs1, rs2 } =>
                write!(f, "or {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::AND { rd, rs1, rs2 } =>
                write!(f, "and {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::LWU { rd, rs1, imm } =>
                write!(f, "lwu {}, {}({})", rd, imm, rs1),
            RV64Instruction::LD { rd, rs1, imm } =>
                write!(f, "ld {}, {}({})", rd, imm, rs1),
            RV64Instruction::SD { rs1, rs2, imm } =>
                write!(f, "sd {}, {}({})", rs2, imm, rs1),
            RV64Instruction::ADDIW { rd, rs1, imm } =>
                write!(f, "addiw {}, {}, {}", rd, rs1, imm),
            RV64Instruction::SLLIW { rd, rs1, imm } =>
                write!(f, "slliw {}, {}, {}", rd, rs1, imm),
            RV64Instruction::SRLIW { rd, rs1, imm } =>
                write!(f, "srliw {}, {}, {}", rd, rs1, imm),
            RV64Instruction::SRAIW { rd, rs1, imm } =>
                write!(f, "sraiw {}, {}, {}", rd, rs1, imm),
            RV64Instruction::ADDW { rd, rs1, rs2 } =>
                write!(f, "addw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SUBW { rd, rs1, rs2 } =>
                write!(f, "subw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SLLW { rd, rs1, rs2 } =>
                write!(f, "sllw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SRLW { rd, rs1, rs2 } =>
                write!(f, "srlw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::SRAW { rd, rs1, rs2 } =>
                write!(f, "sraw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MUL { rd, rs1, rs2 } =>
                write!(f, "mul {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MULH { rd, rs1, rs2 } =>
                write!(f, "mulh {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MULHSU { rd, rs1, rs2 } =>
                write!(f, "mulhsu {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MULHU { rd, rs1, rs2 } =>
                write!(f, "mulhu {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::DIV { rd, rs1, rs2 } =>
                write!(f, "div {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::DIVU { rd, rs1, rs2 } =>
                write!(f, "divu {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::REM { rd, rs1, rs2 } =>
                write!(f, "rem {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::REMU { rd, rs1, rs2 } =>
                write!(f, "remu {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::MULW { rd, rs1, rs2 } =>
                write!(f, "mulw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::DIVW { rd, rs1, rs2 } =>
                write!(f, "divw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::DIVUW { rd, rs1, rs2 } =>
                write!(f, "divuw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::REMW { rd, rs1, rs2 } =>
                write!(f, "remw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::REMUW { rd, rs1, rs2 } =>
                write!(f, "remuw {}, {}, {}", rd, rs1, rs2),
            RV64Instruction::COMMENT { comment } =>
                write!(f, "# {}", comment),
            RV64Instruction::CALL { callee } =>
                write!(f, "call {}", callee),
            RV64Instruction::RET =>
                write!(f, "ret"),
            RV64Instruction::MV { rd, rs } =>
                write!(f, "mv {}, {}", rd, rs),
            RV64Instruction::LIMM { rd, imm } =>
                write!(f, "li {}, {}", rd, imm),
            RV64Instruction::LADDR { rd, label } =>
                write!(f, "la {}, {}", rd, label),
            _ => panic!("unimplemented instruction: {:?}", self),
        }
    }
}

impl Display for MachineOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MachineOperand::Virtual(reg) => write!(f, "%{}", reg),
            MachineOperand::PreColored(reg) => write!(f, "{}", reg),
            MachineOperand::Allocated(reg) => write!(f, "{}", reg),
        }
    }
}

impl Display for RVGPR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}