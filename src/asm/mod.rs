use std::{collections::{HashSet, HashMap}, fmt::Debug};

use crate::{ir::structure::BlockId, alloc::{Id, Arena}};

pub mod codegen;
pub mod export;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum RVReg {
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

// /// ABI names for registers
// macro_rules! reg {
//     (zero) => { RVReg::X0 };
//     (ra) => { RVReg::X1 };
//     (sp) => { RVReg::X2 };
//     (gp) => { RVReg::X3 };
//     (tp) => { RVReg::X4 };
//     (t0) => { RVReg::X5 };
//     (t1) => { RVReg::X6 };
//     (t2) => { RVReg::X7 };
//     (s0) => { RVReg::X8 };
//     (fp) => { RVReg::X8 };
//     (s1) => { RVReg::X9 };
//     (a0) => { RVReg::X10 };
//     (a1) => { RVReg::X11 };
//     (a2) => { RVReg::X12 };
//     (a3) => { RVReg::X13 };
//     (a4) => { RVReg::X14 };
//     (a5) => { RVReg::X15 };
//     (a6) => { RVReg::X16 };
//     (a7) => { RVReg::X17 };
//     (s2) => { RVReg::X18 };
//     (s3) => { RVReg::X19 };
//     (s4) => { RVReg::X20 };
//     (s5) => { RVReg::X21 };
//     (s6) => { RVReg::X22 };
//     (s7) => { RVReg::X23 };
//     (s8) => { RVReg::X24 };
//     (s9) => { RVReg::X25 };
//     (s10) => { RVReg::X26 };
//     (s11) => { RVReg::X27 };
//     (t3) => { RVReg::X28 };
//     (t4) => { RVReg::X29 };
//     (t5) => { RVReg::X30 };
//     (t6) => { RVReg::X31 };
// }

impl RVReg {
    pub fn a(i: usize) -> Self {
        match i {
            0 => RVReg::X10,
            1 => RVReg::X11,
            2 => RVReg::X12,
            3 => RVReg::X13,
            4 => RVReg::X14,
            5 => RVReg::X15,
            6 => RVReg::X16,
            7 => RVReg::X17,
            _ => panic!("invalid A register index"),
        }
    }

    pub fn sp() -> Self {
        RVReg::X2
    }

    pub fn fp() -> Self {
        RVReg::X8
    }

    pub fn zero() -> Self {
        RVReg::X0
    }

    pub fn s(i: usize) -> Self {
        match i {
            0 => RVReg::X8,
            1 => RVReg::X9,
            2 => RVReg::X18,
            3 => RVReg::X19,
            4 => RVReg::X20,
            5 => RVReg::X21,
            6 => RVReg::X22,
            7 => RVReg::X23,
            8 => RVReg::X24,
            9 => RVReg::X25,
            10 => RVReg::X26,
            11 => RVReg::X27,
            _ => panic!("invalid S register index"),
        }
    }

    pub fn t(i: usize) -> Self {
        match i {
            0 => RVReg::X5,
            1 => RVReg::X6,
            2 => RVReg::X7,
            3 => RVReg::X28,
            4 => RVReg::X29,
            5 => RVReg::X30,
            6 => RVReg::X31,
            _ => panic!("invalid T register index"),
        }
    }
}

impl Debug for RVReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // use abi names
            RVReg::X0 => write!(f, "zero"),
            RVReg::X1 => write!(f, "ra"),
            RVReg::X2 => write!(f, "sp"),
            RVReg::X3 => write!(f, "gp"),
            RVReg::X4 => write!(f, "tp"),
            RVReg::X5 => write!(f, "t0"),
            RVReg::X6 => write!(f, "t1"),
            RVReg::X7 => write!(f, "t2"),
            RVReg::X8 => write!(f, "s0"),
            RVReg::X9 => write!(f, "s1"),
            RVReg::X10 => write!(f, "a0"),
            RVReg::X11 => write!(f, "a1"),
            RVReg::X12 => write!(f, "a2"),
            RVReg::X13 => write!(f, "a3"),
            RVReg::X14 => write!(f, "a4"),
            RVReg::X15 => write!(f, "a5"),
            RVReg::X16 => write!(f, "a6"),
            RVReg::X17 => write!(f, "a7"),
            RVReg::X18 => write!(f, "s2"),
            RVReg::X19 => write!(f, "s3"),
            RVReg::X20 => write!(f, "s4"),
            RVReg::X21 => write!(f, "s5"),
            RVReg::X22 => write!(f, "s6"),
            RVReg::X23 => write!(f, "s7"),
            RVReg::X24 => write!(f, "s8"),
            RVReg::X25 => write!(f, "s9"),
            RVReg::X26 => write!(f, "s10"),
            RVReg::X27 => write!(f, "s11"),
            RVReg::X28 => write!(f, "t3"),
            RVReg::X29 => write!(f, "t4"),
            RVReg::X30 => write!(f, "t5"),
            RVReg::X31 => write!(f, "t6"),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum MachineOperand {
    Virtual(u32),
    Allocated(RVReg),
    PreColored(RVReg),
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
    #[allow(non_snake_case)]
    pub fn LUI(rd: MachineOperand, imm: i32) -> RV64Instruction {
        // check if imm is a 20-bit signed integer
        if imm < 1048576 && imm >= -1048576 {
            RV64Instruction::LUI { rd, imm }
        } else {
            panic!("immediate value out of range")
        }
    }

    builder_impl_rv64!(i; LB);
    builder_impl_rv64!(i; LH);
    builder_impl_rv64!(i; LW);
    builder_impl_rv64!(i; LBU);
    builder_impl_rv64!(i; LHU);
    builder_impl_rv64!(s; SB);
    builder_impl_rv64!(s; SH);
    builder_impl_rv64!(s; SW);

    builder_impl_rv64!(i; ADDI);
    builder_impl_rv64!(i; SLTI);
    builder_impl_rv64!(i; SLTIU);
    builder_impl_rv64!(i; XORI);
    builder_impl_rv64!(i; ORI);
    builder_impl_rv64!(i; ANDI);
    builder_impl_rv64!(i; SLLI);
    builder_impl_rv64!(i; SRLI);
    builder_impl_rv64!(i; SRAI);

    builder_impl_rv64!(r; ADD);
    builder_impl_rv64!(r; SUB);
    builder_impl_rv64!(r; SLL);
    builder_impl_rv64!(r; SLT);
    builder_impl_rv64!(r; SLTU);
    builder_impl_rv64!(r; XOR);
    builder_impl_rv64!(r; SRL);
    builder_impl_rv64!(r; SRA);
    builder_impl_rv64!(r; OR);
    builder_impl_rv64!(r; AND);

    builder_impl_rv64!(i; LWU);
    builder_impl_rv64!(i; LD);
    builder_impl_rv64!(s; SD);

    builder_impl_rv64!(i; ADDIW);
    builder_impl_rv64!(i; SLLIW);
    builder_impl_rv64!(i; SRLIW);
    builder_impl_rv64!(i; SRAIW);
    builder_impl_rv64!(r; ADDW);
    builder_impl_rv64!(r; SUBW);
    builder_impl_rv64!(r; SLLW);
    builder_impl_rv64!(r; SRLW);
    builder_impl_rv64!(r; SRAW);

    builder_impl_rv64!(r; MUL);
    builder_impl_rv64!(r; MULH);
    builder_impl_rv64!(r; MULHSU);
    builder_impl_rv64!(r; MULHU);
    builder_impl_rv64!(r; DIV);
    builder_impl_rv64!(r; DIVU);
    builder_impl_rv64!(r; REM);
    builder_impl_rv64!(r; REMU);
    builder_impl_rv64!(r; MULW);
    builder_impl_rv64!(r; DIVW);
    builder_impl_rv64!(r; DIVUW);
    builder_impl_rv64!(r; REMW);
    builder_impl_rv64!(r; REMUW);

    #[allow(non_snake_case)]
    pub fn COMMENT(comment: String) -> RV64Instruction {
        RV64Instruction::COMMENT { comment }
    }
    #[allow(non_snake_case)]
    pub fn JUMP(target: Id<MachineBB>) -> RV64Instruction {
        RV64Instruction::JUMP { target }
    }
    #[allow(non_snake_case)]
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
    builder_impl_rv64!(cj; JEQ);
    builder_impl_rv64!(cj; JNE);
    builder_impl_rv64!(cj; JLT);
    builder_impl_rv64!(cj; JGE);
    builder_impl_rv64!(cj; JLTU);
    builder_impl_rv64!(cj; JGEU);
    #[allow(non_snake_case)]
    pub fn LIMM(rd: MachineOperand, imm: i32) -> RV64Instruction {
        RV64Instruction::LIMM { rd, imm }
    }
    builder_impl_rv64!(r; SEQ);
    builder_impl_rv64!(r; SNE);
    builder_impl_rv64!(r; SGE);
    builder_impl_rv64!(r; SGEU);
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

pub struct MachineFunc {
    pub func: String,
    pub entry: Option<Id<MachineBB>>,
    pub bbs: Vec<Id<MachineBB>>,
    pub virtual_max: u32,
    pub stack_size: u32,
    pub saved_regs: HashSet<RVReg>,

    // use_lr?

    // todo: omit_fp_fixup, for sp-relative addressing
}

pub struct MachineProgram {
    pub funcs: Vec<MachineFunc>,

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
            funcs: Vec::new(),
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
            stack_size: 0,
            saved_regs: HashSet::new(),
        }
    }

    pub fn alloc_block(&mut self, idx: usize, mbb: MachineBB) -> Id<MachineBB> {
        let mbb_id = self.blocks.alloc(mbb);
        self.block_map.insert(mbb_id, idx);
        mbb_id
    }

    pub fn push_to_end(&mut self, mbb: Id<MachineBB>, inst: RV64Instruction) {
        let mblock_tail = self.blocks[mbb].insts_tail;
        let minst = MachineInst {
            inst,
            bb: mbb,
            inlined: false,
            prev: mblock_tail,
            next: None,
        };
        let minst_id = self.insts.alloc(minst);
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
            inst,
            bb: mbb,
            inlined: false,
            prev: None,
            next: mblock_head,
        };
        let minst_id = self.insts.alloc(minst);
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
            inst,
            bb: self.insts[before].bb,
            inlined: false,
            prev: self.insts[before].prev,
            next: Some(before),
        };
        let minst_id = self.insts.alloc(minst);
        self.mark_inline_inst(minst_id);
        if let Some(prev) = self.insts[before].prev {
            self.insts[prev].next = Some(minst_id);
        } else {
            self.blocks[self.insts[before].bb].insts_head = Some(minst_id);
        }
        self.insts[before].prev = Some(minst_id);
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
}