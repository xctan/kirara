use std::{collections::{HashSet, HashMap}, fmt::Debug};

use crate::{ir::structure::BlockId, alloc::{Id, Arena}};

pub mod codegen;
pub mod export;
pub mod reg_alloc;
pub mod simplify;

/// Listing of physical registers
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
#[repr(u16)]
pub enum RVGPR {
    X0, // always zero
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    X29,
    X30,
    X31,
}

impl PartialOrd for RVGPR {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        fn classify(reg_num: u16) -> i32 {
            // a0-a7 are allocated first
            if reg_num >= 10 && reg_num <= 17 {
                return 1;
            }
            // s0-s11 are allocated next
            if reg_num >= 8 && reg_num <= 9 || reg_num >= 18 && reg_num <= 27 {
                return 2;
            }
            // t0-t6 are allocated last
            if reg_num >= 5 && reg_num <= 7 || reg_num >= 28 && reg_num <= 31 {
                return 3;
            }
            return 10000;
        }
        let self_reg = unsafe { std::mem::transmute::<_, u16>(*self) };
        let self_class = classify(self_reg);
        let other_reg = unsafe { std::mem::transmute::<_, u16>(*other) };
        let other_class = classify(other_reg);
        if self_class == other_class {
            Some(self_reg.cmp(&other_reg))
        } else {
            Some(self_class.cmp(&other_class))
        }
    }
}

impl Ord for RVGPR {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl RVGPR {
    pub fn a(i: usize) -> Self {
        match i {
            0 => RVGPR::X10,
            1 => RVGPR::X11,
            2 => RVGPR::X12,
            3 => RVGPR::X13,
            4 => RVGPR::X14,
            5 => RVGPR::X15,
            6 => RVGPR::X16,
            7 => RVGPR::X17,
            _ => panic!("invalid A register index"),
        }
    }

    pub fn sp() -> Self {
        RVGPR::X2
    }

    pub fn fp() -> Self {
        RVGPR::X8
    }

    pub fn zero() -> Self {
        RVGPR::X0
    }

    pub fn s(i: usize) -> Self {
        match i {
            0 => RVGPR::X8,
            1 => RVGPR::X9,
            2 => RVGPR::X18,
            3 => RVGPR::X19,
            4 => RVGPR::X20,
            5 => RVGPR::X21,
            6 => RVGPR::X22,
            7 => RVGPR::X23,
            8 => RVGPR::X24,
            9 => RVGPR::X25,
            10 => RVGPR::X26,
            11 => RVGPR::X27,
            _ => panic!("invalid S register index"),
        }
    }

    pub fn t(i: usize) -> Self {
        match i {
            0 => RVGPR::X5,
            1 => RVGPR::X6,
            2 => RVGPR::X7,
            3 => RVGPR::X28,
            4 => RVGPR::X29,
            5 => RVGPR::X30,
            6 => RVGPR::X31,
            _ => panic!("invalid T register index"),
        }
    }

    pub fn x(i: usize) -> Self {
        match i {
            0 => RVGPR::X1,
            1 => RVGPR::X2,
            2 => RVGPR::X3,
            3 => RVGPR::X4,
            4 => RVGPR::X5,
            5 => RVGPR::X6,
            6 => RVGPR::X7,
            7 => RVGPR::X8,
            8 => RVGPR::X9,
            9 => RVGPR::X10,
            10 => RVGPR::X11,
            11 => RVGPR::X12,
            12 => RVGPR::X13,
            13 => RVGPR::X14,
            14 => RVGPR::X15,
            15 => RVGPR::X16,
            16 => RVGPR::X17,
            17 => RVGPR::X18,
            18 => RVGPR::X19,
            19 => RVGPR::X20,
            20 => RVGPR::X21,
            21 => RVGPR::X22,
            22 => RVGPR::X23,
            23 => RVGPR::X24,
            24 => RVGPR::X25,
            25 => RVGPR::X26,
            26 => RVGPR::X27,
            27 => RVGPR::X28,
            28 => RVGPR::X29,
            29 => RVGPR::X30,
            30 => RVGPR::X31,
            _ => panic!("invalid X register index"),
        }
    }
}

impl Debug for RVGPR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // use abi names
            RVGPR::X0 => write!(f, "zero"),
            RVGPR::X1 => write!(f, "ra"),
            RVGPR::X2 => write!(f, "sp"),
            RVGPR::X3 => write!(f, "gp"),
            RVGPR::X4 => write!(f, "tp"),
            RVGPR::X5 => write!(f, "t0"),
            RVGPR::X6 => write!(f, "t1"),
            RVGPR::X7 => write!(f, "t2"),
            RVGPR::X8 => write!(f, "s0"),
            RVGPR::X9 => write!(f, "s1"),
            RVGPR::X10 => write!(f, "a0"),
            RVGPR::X11 => write!(f, "a1"),
            RVGPR::X12 => write!(f, "a2"),
            RVGPR::X13 => write!(f, "a3"),
            RVGPR::X14 => write!(f, "a4"),
            RVGPR::X15 => write!(f, "a5"),
            RVGPR::X16 => write!(f, "a6"),
            RVGPR::X17 => write!(f, "a7"),
            RVGPR::X18 => write!(f, "s2"),
            RVGPR::X19 => write!(f, "s3"),
            RVGPR::X20 => write!(f, "s4"),
            RVGPR::X21 => write!(f, "s5"),
            RVGPR::X22 => write!(f, "s6"),
            RVGPR::X23 => write!(f, "s7"),
            RVGPR::X24 => write!(f, "s8"),
            RVGPR::X25 => write!(f, "s9"),
            RVGPR::X26 => write!(f, "s10"),
            RVGPR::X27 => write!(f, "s11"),
            RVGPR::X28 => write!(f, "t3"),
            RVGPR::X29 => write!(f, "t4"),
            RVGPR::X30 => write!(f, "t5"),
            RVGPR::X31 => write!(f, "t6"),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum MachineOperand {
    Virtual(u32),
    Allocated(RVGPR),
    PreColored(RVGPR),
}

impl MachineOperand {
    pub fn needs_coloring(&self) -> bool {
        matches!(self, MachineOperand::Virtual(_) | MachineOperand::PreColored(_))
    }

    pub fn is_precolored(&self) -> bool {
        matches!(self, MachineOperand::PreColored(_))
    }

    pub fn color(&self) -> Option<RVGPR> {
        match self {
            MachineOperand::Virtual(_) => None,
            MachineOperand::Allocated(r) => Some(*r),
            MachineOperand::PreColored(r) => Some(*r),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum RV64Instruction {
    // RV32I

    // U-type: rd, imm
    LUI { rd: MachineOperand, imm: i32 },
    AUIPC { rd: MachineOperand, imm: i32 },
    // J-type: rd, imm
    JAL { rd: MachineOperand, imm: i32 },
    // I-type: rd, rs1, imm
    JALR { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    // B-type: rs1, rs2, imm
    BEQ { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    BNE { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    BLT { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    BGE { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    BLTU { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    BGEU { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    // I-type: rd, rs1, imm
    LB { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    LH { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    LW { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    LBU { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    LHU { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    // S-type: rs1, rs2, imm
    SB { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    SH { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    SW { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    // I-type: rd, rs1, imm
    ADDI { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    SLTI { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    SLTIU { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    XORI { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    ORI { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    ANDI { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    SLLI { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    SRLI { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    SRAI { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    // R-type: rd, rs1, rs2
    ADD { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SUB { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SLL { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SLT { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SLTU { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    XOR { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SRL { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SRA { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    OR { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    AND { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },

    // RV64I additions

    // I-type: rd, rs1, imm
    LWU { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    LD { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    // S-type: rs1, rs2, imm
    SD { rs1: MachineOperand, rs2: MachineOperand, imm: i32 },
    // I-type: rd, rs1, imm
    ADDIW { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    SLLIW { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    SRLIW { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    SRAIW { rd: MachineOperand, rs1: MachineOperand, imm: i32 },
    // R-type: rd, rs1, rs2
    ADDW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SUBW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SLLW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SRLW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SRAW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },

    // RV32M
    // R-type: rd, rs1, rs2
    MUL { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    MULH { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    MULHSU { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    MULHU { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    DIV { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    DIVU { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    REM { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    REMU { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },

    // RV64M
    // R-type: rd, rs1, rs2
    MULW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    DIVW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    DIVUW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    REMW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    REMUW { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },

    // todo: RV32F

    // pseudo instructions for convenience
    COMMENT { comment: String },
    CALL { callee: String },
    RET,
    // alias for addi
    MV { rd: MachineOperand, rs: MachineOperand },
    // fused branch instructions with logical targets
    JEQ { rs1: MachineOperand, rs2: MachineOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JNE { rs1: MachineOperand, rs2: MachineOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JLT { rs1: MachineOperand, rs2: MachineOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JGE { rs1: MachineOperand, rs2: MachineOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JLTU { rs1: MachineOperand, rs2: MachineOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JGEU { rs1: MachineOperand, rs2: MachineOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JUMP { target: Id<MachineBB> },
    LIMM { rd: MachineOperand, imm: i32 },
    LADDR { rd: MachineOperand, label: String },
    // fused compare instructions for inlining
    SEQ { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SNE { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SGE { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
    SGEU { rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand },
}

macro_rules! implement_typed_instruction {
    ($implementer:ident) => {
        $implementer!(i; LB);
        $implementer!(i; LH);
        $implementer!(i; LW);
        $implementer!(i; LBU);
        $implementer!(i; LHU);
        $implementer!(s; SB);
        $implementer!(s; SH);
        $implementer!(s; SW);

        $implementer!(i; ADDI);
        $implementer!(i; SLTI);
        $implementer!(i; SLTIU);
        $implementer!(i; XORI);
        $implementer!(i; ORI);
        $implementer!(i; ANDI);
        $implementer!(i; SLLI);
        $implementer!(i; SRLI);
        $implementer!(i; SRAI);

        $implementer!(r; ADD);
        $implementer!(r; SUB);
        $implementer!(r; SLL);
        $implementer!(r; SLT);
        $implementer!(r; SLTU);
        $implementer!(r; XOR);
        $implementer!(r; SRL);
        $implementer!(r; SRA);
        $implementer!(r; OR);
        $implementer!(r; AND);

        $implementer!(i; LWU);
        $implementer!(i; LD);
        $implementer!(s; SD);

        $implementer!(i; ADDIW);
        $implementer!(i; SLLIW);
        $implementer!(i; SRLIW);
        $implementer!(i; SRAIW);
        $implementer!(r; ADDW);
        $implementer!(r; SUBW);
        $implementer!(r; SLLW);
        $implementer!(r; SRLW);
        $implementer!(r; SRAW);

        $implementer!(r; MUL);
        $implementer!(r; MULH);
        $implementer!(r; MULHSU);
        $implementer!(r; MULHU);
        $implementer!(r; DIV);
        $implementer!(r; DIVU);
        $implementer!(r; REM);
        $implementer!(r; REMU);
        $implementer!(r; MULW);
        $implementer!(r; DIVW);
        $implementer!(r; DIVUW);
        $implementer!(r; REMW);
        $implementer!(r; REMUW);

        $implementer!(cj; JEQ);
        $implementer!(cj; JNE);
        $implementer!(cj; JLT);
        $implementer!(cj; JGE);
        $implementer!(cj; JLTU);
        $implementer!(cj; JGEU);

        $implementer!(r; SEQ);
        $implementer!(r; SNE);
        $implementer!(r; SGE);
        $implementer!(r; SGEU);
    };
}

impl RV64Instruction {
    pub fn get_rd(&self) -> Option<MachineOperand> {
        match self.clone() {
            RV64Instruction::LUI { rd, .. } => Some(rd),
            RV64Instruction::AUIPC { rd, .. } => Some(rd),
            RV64Instruction::JAL { rd, .. } => Some(rd),
            RV64Instruction::JALR { rd, .. } => Some(rd),
            RV64Instruction::LB { rd, .. } => Some(rd),
            RV64Instruction::LH { rd, .. } => Some(rd),
            RV64Instruction::LW { rd, .. } => Some(rd),
            RV64Instruction::LBU { rd, .. } => Some(rd),
            RV64Instruction::LHU { rd, .. } => Some(rd),
            RV64Instruction::ADDI { rd, .. } => Some(rd),
            RV64Instruction::SLTI { rd, .. } => Some(rd),
            RV64Instruction::SLTIU { rd, .. } => Some(rd),
            RV64Instruction::XORI { rd, .. } => Some(rd),
            RV64Instruction::ORI { rd, .. } => Some(rd),
            RV64Instruction::ANDI { rd, .. } => Some(rd),
            RV64Instruction::SLLI { rd, .. } => Some(rd),
            RV64Instruction::SRLI { rd, .. } => Some(rd),
            RV64Instruction::SRAI { rd, .. } => Some(rd),
            RV64Instruction::ADD { rd, .. } => Some(rd),
            RV64Instruction::SUB { rd, .. } => Some(rd),
            RV64Instruction::SLL { rd, .. } => Some(rd),
            RV64Instruction::SLT { rd, .. } => Some(rd),
            RV64Instruction::SLTU { rd, .. } => Some(rd),
            RV64Instruction::XOR { rd, .. } => Some(rd),
            RV64Instruction::SRL { rd, .. } => Some(rd),
            RV64Instruction::SRA { rd, .. } => Some(rd),
            RV64Instruction::OR { rd, .. } => Some(rd),
            RV64Instruction::AND { rd, .. } => Some(rd),
            RV64Instruction::LWU { rd, .. } => Some(rd),
            RV64Instruction::LD { rd, .. } => Some(rd),
            RV64Instruction::ADDIW { rd, .. } => Some(rd),
            RV64Instruction::SLLIW { rd, .. } => Some(rd),
            RV64Instruction::SRLIW { rd, .. } => Some(rd),
            RV64Instruction::SRAIW { rd, .. } => Some(rd),
            RV64Instruction::ADDW { rd, .. } => Some(rd),
            RV64Instruction::SUBW { rd, .. } => Some(rd),
            RV64Instruction::SLLW { rd, .. } => Some(rd),
            RV64Instruction::SRLW { rd, .. } => Some(rd),
            RV64Instruction::SRAW { rd, .. } => Some(rd),
            RV64Instruction::MUL { rd, .. } => Some(rd),
            RV64Instruction::MULH { rd, .. } => Some(rd),
            RV64Instruction::MULHSU { rd, .. } => Some(rd),
            RV64Instruction::MULHU { rd, .. } => Some(rd),
            RV64Instruction::DIV { rd, .. } => Some(rd),
            RV64Instruction::DIVU { rd, .. } => Some(rd),
            RV64Instruction::REM { rd, .. } => Some(rd),
            RV64Instruction::REMU { rd, .. } => Some(rd),
            RV64Instruction::MULW { rd, .. } => Some(rd),
            RV64Instruction::DIVW { rd, .. } => Some(rd),
            RV64Instruction::DIVUW { rd, .. } => Some(rd),
            RV64Instruction::REMW { rd, .. } => Some(rd),
            RV64Instruction::REMUW { rd, .. } => Some(rd),
            RV64Instruction::MV { rd, .. } => Some(rd),
            RV64Instruction::LIMM { rd, .. } => Some(rd),
            RV64Instruction::LADDR { rd, .. } => Some(rd),
            RV64Instruction::SEQ { rd, .. } => Some(rd),
            RV64Instruction::SNE { rd, .. } => Some(rd),
            RV64Instruction::SGE { rd, .. } => Some(rd),
            RV64Instruction::SGEU { rd, .. } => Some(rd),
            _ => None,
        }
    }

    pub fn get_def_use(&self) -> (Vec<MachineOperand>, Vec<MachineOperand>) {
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
        }

        implement_typed_instruction!(extract);
        
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
            return (vec![], vec![MachineOperand::PreColored(RVGPR::a(0))]);
        }
        if let RV64Instruction::JUMP { .. } = self {
            return (vec![], vec![]);
        }

        panic!("unimplemented: {:?}", self)
    }

    pub fn get_operands_mut(&mut self) -> Vec<&mut MachineOperand> {
        macro_rules! extract {
            (r ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rd, rs1, rs2 } = self {
                    return vec![rd, rs1, rs2]
                }
            };
            (i ; $mnemonic:ident) => {
                if let RV64Instruction::$mnemonic { rd, rs1, .. } = self {
                    return vec![rd, rs1]
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
        }

        if let RV64Instruction::LUI { rd, .. } = self {
            return vec![rd];
        }
        if let RV64Instruction::MV { rd, rs } = self {
            return vec![rd, rs];
        }
        if let RV64Instruction::LIMM { rd, .. } = self {
            return vec![rd];
        }
        if let RV64Instruction::LADDR { rd, .. } = self {
            return vec![rd]
        }
        if let RV64Instruction::RET = self {
            return vec![];
        }
        if let RV64Instruction::JUMP { .. } = self {
            return vec![];
        }

        implement_typed_instruction!(extract);

        panic!("unimplemented: {:?}", self)
    }
}

macro_rules! builder_impl_rv64 {
    (r ; $mnemonic:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused)]
        pub fn $mnemonic(rd: MachineOperand, rs1: MachineOperand, rs2: MachineOperand) -> RV64Instruction {
            RV64Instruction::$mnemonic { rd, rs1, rs2 }
        }
    };
    (i ; $mnemonic:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused)]
        pub fn $mnemonic(rd: MachineOperand, rs1: MachineOperand, imm: i32) -> RV64Instruction {
            // check if imm is a 12-bit signed integer
            if imm < 4096 && imm >= -4096 {
                RV64Instruction::$mnemonic { rd, rs1, imm }
            } else {
                panic!("immediate value out of range");
            }
        }
    };
    (s; $mnemonic:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused)]
        pub fn $mnemonic(rs2: MachineOperand, rs1: MachineOperand, imm: i32) -> RV64Instruction {
            // check if imm is a 12-bit signed integer
            if imm < 4096 && imm >= -4096 {
                RV64Instruction::$mnemonic { rs1, rs2, imm }
            } else {
                panic!("immediate value out of range")
            }
        }
    };
    (cj; $mnemonic:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused)]
        pub fn $mnemonic(rs1: MachineOperand, rs2: MachineOperand, succ: Id<MachineBB>, fail: Id<MachineBB>) -> RV64Instruction {
            RV64Instruction::$mnemonic { rs1, rs2, succ, fail }
        }
    };
}

pub struct RV64InstBuilder;

impl RV64InstBuilder {
    implement_typed_instruction!(builder_impl_rv64);

    #[allow(non_snake_case)]
    pub fn LUI(rd: MachineOperand, imm: i32) -> RV64Instruction {
        // check if imm is a 20-bit signed integer
        if imm < 1048576 && imm >= -1048576 {
            RV64Instruction::LUI { rd, imm }
        } else {
            panic!("immediate value out of range")
        }
    }

    #[allow(non_snake_case, unused)]
    pub fn COMMENT(comment: String) -> RV64Instruction {
        RV64Instruction::COMMENT { comment }
    }
    #[allow(non_snake_case)]
    pub fn JUMP(target: Id<MachineBB>) -> RV64Instruction {
        RV64Instruction::JUMP { target }
    }
    #[allow(non_snake_case, unused)]
    pub fn CALL(callee: String) -> RV64Instruction {
        RV64Instruction::CALL { callee }
    }
    #[allow(non_snake_case)]
    pub fn RET() -> RV64Instruction {
        RV64Instruction::RET
    }
    #[allow(non_snake_case)]
    pub fn MV(rd: MachineOperand, rs: MachineOperand) -> RV64Instruction {
        RV64Instruction::MV { rd, rs }
    }
    #[allow(non_snake_case)]
    pub fn LIMM(rd: MachineOperand, imm: i32) -> RV64Instruction {
        RV64Instruction::LIMM { rd, imm }
    }
    #[allow(non_snake_case)]
    pub fn LADDR(rd: MachineOperand, label: String) -> RV64Instruction {
        RV64Instruction::LADDR { rd, label }
    }
}

#[derive(Clone, Debug)]
pub struct MachineInst {
    pub inst: RV64Instruction,
    pub bb: Id<MachineBB>,
    /// identify whether this instruction is always inlined, e.g. addi
    pub inlined: bool,
    pub prev: Option<Id<MachineInst>>,
    pub next: Option<Id<MachineInst>>,
}

pub struct MachineBB {
    pub bb: BlockId,
    pub insts_head: Option<Id<MachineInst>>,
    pub insts_tail: Option<Id<MachineInst>>,
    pub preds: Vec<Id<MachineBB>>,
    pub succs: Vec<Id<MachineBB>>,

    pub liveuse: HashSet<MachineOperand>,
    pub livedef: HashSet<MachineOperand>,
    pub livein: HashSet<MachineOperand>,
    pub liveout: HashSet<MachineOperand>,
}

impl MachineBB {
    pub fn new(bb: BlockId) -> Self {
        Self {
            bb,
            insts_head: None,
            insts_tail: None,
            preds: Vec::new(),
            succs: Vec::new(),
            liveuse: HashSet::new(),
            livedef: HashSet::new(),
            livein: HashSet::new(),
            liveout: HashSet::new(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum VRegType {
    Int32,
    Int64,
    #[allow(unused)]
    Fp32,
}

pub struct MachineFunc {
    pub func: String,
    pub entry: Option<Id<MachineBB>>,
    pub bbs: Vec<Id<MachineBB>>,
    pub virtual_max: u32,
    pub vreg_types: HashMap<u32, VRegType>,
    pub stack_size: u32,
    pub saved_regs: HashSet<RVGPR>,

    // use_lr?

    // todo: omit_fp_fixup, for sp-relative addressing
}

#[derive(Debug, Clone, Copy)]
pub enum DataLiteral {
    // .byte
    // .half
    /// .word
    Word(u32),
    // .quad
    /// .zero
    Zero(u32),
}

pub struct MachineProgram {
    pub funcs: HashMap<String, MachineFunc>,

    /// data of global variables (functions are not included)
    pub symbols: HashMap<String, Vec<DataLiteral>>,

    // global_decl

    pub blocks: Arena<MachineBB>,
    pub insts: Arena<MachineInst>,
    // mbb to mfunc map
    pub block_map: HashMap<Id<MachineBB>, usize>,

    /// record the final definition of each virtual register
    pub vreg_def: HashMap<MachineOperand, Id<MachineInst>>,
}

impl MachineProgram {
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
            symbols: HashMap::new(),
            blocks: Arena::new(),
            insts: Arena::new(),
            block_map: HashMap::new(),

            vreg_def: HashMap::new(),
        }
    }

    pub fn new_func(&mut self, func: String) -> MachineFunc {
        self.vreg_def.clear();
        MachineFunc {
            func,
            entry: None,
            bbs: Vec::new(),
            virtual_max: 0,
            vreg_types: HashMap::new(),
            stack_size: 0,
            saved_regs: HashSet::new(),
        }
    }

    pub fn push_to_end(&mut self, mbb: Id<MachineBB>, inst: RV64Instruction) {
        let mblock_tail = self.blocks[mbb].insts_tail;
        let minst = MachineInst {
            inst: inst.clone(),
            bb: mbb,
            inlined: false,
            prev: mblock_tail,
            next: None,
        };
        let minst_id = self.insts.alloc(minst);
        if let Some(rd) = inst.get_rd() {
            self.define_vreg(rd, minst_id);
        }
        self.mark_inline_inst(minst_id);
        let mblock = &mut self.blocks[mbb];
        mblock.insts_tail = Some(minst_id);
        if mblock.insts_head.is_none() {
            mblock.insts_head = Some(minst_id);
        } else {
            self.insts[mblock_tail.unwrap()].next = Some(minst_id);
        }
    }

    pub fn push_to_begin(&mut self, mbb: Id<MachineBB>, inst: RV64Instruction) {
        let mblock_head = self.blocks[mbb].insts_head;
        let minst = MachineInst {
            inst: inst.clone(),
            bb: mbb,
            inlined: false,
            prev: None,
            next: mblock_head,
        };
        let minst_id = self.insts.alloc(minst);
        if let Some(rd) = inst.get_rd() {
            self.define_vreg(rd, minst_id);
        }
        self.mark_inline_inst(minst_id);
        let mblock = &mut self.blocks[mbb];
        if let Some(head) = mblock_head {
            self.insts[head].prev = Some(minst_id);
        } else {
            mblock.insts_tail = Some(minst_id);
        }
        self.blocks[mbb].insts_head = Some(minst_id);
    }

    pub fn insert_before(&mut self, before: Id<MachineInst>, inst: RV64Instruction) {
        let minst = MachineInst {
            inst: inst.clone(),
            bb: self.insts[before].bb,
            inlined: false,
            prev: self.insts[before].prev,
            next: Some(before),
        };
        let minst_id = self.insts.alloc(minst);
        if let Some(rd) = inst.get_rd() {
            self.define_vreg(rd, minst_id);
        }
        self.mark_inline_inst(minst_id);
        if let Some(prev) = self.insts[before].prev {
            self.insts[prev].next = Some(minst_id);
        } else {
            self.blocks[self.insts[before].bb].insts_head = Some(minst_id);
        }
        self.insts[before].prev = Some(minst_id);
    }

    pub fn insert_after(&mut self, after: Id<MachineInst>, inst: RV64Instruction) {
        let minst = MachineInst {
            inst: inst.clone(),
            bb: self.insts[after].bb,
            inlined: false,
            prev: Some(after),
            next: self.insts[after].next,
        };
        let minst_id = self.insts.alloc(minst);
        if let Some(rd) = inst.get_rd() {
            self.define_vreg(rd, minst_id);
        }
        self.mark_inline_inst(minst_id);
        if let Some(next) = self.insts[after].next {
            self.insts[next].prev = Some(minst_id);
        } else {
            self.blocks[self.insts[after].bb].insts_tail = Some(minst_id);
        }
        self.insts[after].next = Some(minst_id);
    }

    pub fn remove(&mut self, inst: Id<MachineInst>) {
        let minst = self.insts[inst].clone();
        if let Some(prev) = minst.prev {
            self.insts[prev].next = minst.next;
        } else {
            self.blocks[minst.bb].insts_head = minst.next;
        }
        if let Some(next) = minst.next {
            self.insts[next].prev = minst.prev;
        } else {
            self.blocks[minst.bb].insts_tail = minst.prev;
        }
        if let Some(rd) = minst.inst.get_rd() {
            self.vreg_def.remove(&rd);
        }
        self.insts.remove(inst);
    }

    pub fn mark_inline(&mut self, inst: Id<MachineInst>, status: bool) {
        self.insts[inst].inlined = status;
    }

    fn define_vreg(&mut self, vreg: MachineOperand, minst: Id<MachineInst>) {
        if matches!(vreg, MachineOperand::Virtual(_)) {
            self.vreg_def.insert(vreg, minst);
        }
    }

    fn mark_inline_inst(&mut self, minst: Id<MachineInst>) {
        match self.insts[minst].inst {
            RV64Instruction::ADDI { .. } |
            RV64Instruction::SEQ { .. } |
            RV64Instruction::SNE { .. } |
            RV64Instruction::SLT { .. } |
            RV64Instruction::SGE { .. } |
            RV64Instruction::SLTU { .. } |
            RV64Instruction::SGEU { .. } => {
                self.mark_inline(minst, true);
            }
            _ => {}
        }
    }

    pub fn get_def_use(&self, minst: Id<MachineInst>) -> (Vec<MachineOperand>, Vec<MachineOperand>) {
        let inst = &self.insts[minst].inst;
        if let RV64Instruction::CALL { callee: _ } = inst {
            unimplemented!()
        }

        inst.get_def_use()
    }

    pub fn get_operands_mut(&mut self, minst: Id<MachineInst>) -> Vec<&mut MachineOperand> {
        let inst = &mut self.insts[minst].inst;
        if let RV64Instruction::CALL { callee: _ } = inst {
            unimplemented!()
        }

        inst.get_operands_mut()
    }
}