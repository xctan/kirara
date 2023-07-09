use std::{rc::Rc, collections::HashSet};

use crate::ir::structure::BlockId;

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

/// ABI names for registers
macro_rules! reg {
    (zero) => { RVReg::X0 };
    (ra) => { RVReg::X1 };
    (sp) => { RVReg::X2 };
    (gp) => { RVReg::X3 };
    (tp) => { RVReg::X4 };
    (t0) => { RVReg::X5 };
    (t1) => { RVReg::X6 };
    (t2) => { RVReg::X7 };
    (s0) => { RVReg::X8 };
    (fp) => { RVReg::X8 };
    (s1) => { RVReg::X9 };
    (a0) => { RVReg::X10 };
    (a1) => { RVReg::X11 };
    (a2) => { RVReg::X12 };
    (a3) => { RVReg::X13 };
    (a4) => { RVReg::X14 };
    (a5) => { RVReg::X15 };
    (a6) => { RVReg::X16 };
    (a7) => { RVReg::X17 };
    (s2) => { RVReg::X18 };
    (s3) => { RVReg::X19 };
    (s4) => { RVReg::X20 };
    (s5) => { RVReg::X21 };
    (s6) => { RVReg::X22 };
    (s7) => { RVReg::X23 };
    (s8) => { RVReg::X24 };
    (s9) => { RVReg::X25 };
    (s10) => { RVReg::X26 };
    (s11) => { RVReg::X27 };
    (t3) => { RVReg::X28 };
    (t4) => { RVReg::X29 };
    (t5) => { RVReg::X30 };
    (t6) => { RVReg::X31 };
}

pub enum MachineOperand {
    Allocated(RVReg),
    Virtual(u32),
}

pub enum RV64Instruction {
    // RV32I

    // U-type: rd, imm
    LUI(MachineOperand, i32),
    AUIPC(MachineOperand, i32),
    // J-type: rd, imm
    JAL(MachineOperand, i32),
    // I-type: rd, rs1, imm
    JALR(MachineOperand, MachineOperand, i32),
    // B-type: rs1, rs2, imm
    BEQ(MachineOperand, MachineOperand, i32),
    BNE(MachineOperand, MachineOperand, i32),
    BLT(MachineOperand, MachineOperand, i32),
    BGE(MachineOperand, MachineOperand, i32),
    BLTU(MachineOperand, MachineOperand, i32),
    BGEU(MachineOperand, MachineOperand, i32),
    // I-type: rd, rs1, imm
    LB(MachineOperand, MachineOperand, i32),
    LH(MachineOperand, MachineOperand, i32),
    LW(MachineOperand, MachineOperand, i32),
    LBU(MachineOperand, MachineOperand, i32),
    LHU(MachineOperand, MachineOperand, i32),
    // S-type: rs1, rs2, imm
    SB(MachineOperand, MachineOperand, i32),
    SH(MachineOperand, MachineOperand, i32),
    SW(MachineOperand, MachineOperand, i32),
    // I-type: rd, rs1, imm
    ADDI(MachineOperand, MachineOperand, i32),
    SLTI(MachineOperand, MachineOperand, i32),
    SLTIU(MachineOperand, MachineOperand, i32),
    XORI(MachineOperand, MachineOperand, i32),
    ORI(MachineOperand, MachineOperand, i32),
    ANDI(MachineOperand, MachineOperand, i32),
    SLLI(MachineOperand, MachineOperand, i32),
    SRLI(MachineOperand, MachineOperand, i32),
    SRAI(MachineOperand, MachineOperand, i32),
    // R-type: rd, rs1, rs2
    ADD(MachineOperand, MachineOperand, MachineOperand),
    SUB(MachineOperand, MachineOperand, MachineOperand),
    SLL(MachineOperand, MachineOperand, MachineOperand),
    SLT(MachineOperand, MachineOperand, MachineOperand),
    SLTU(MachineOperand, MachineOperand, MachineOperand),
    XOR(MachineOperand, MachineOperand, MachineOperand),
    SRL(MachineOperand, MachineOperand, MachineOperand),
    SRA(MachineOperand, MachineOperand, MachineOperand),
    OR(MachineOperand, MachineOperand, MachineOperand),
    AND(MachineOperand, MachineOperand, MachineOperand),

    // RV64I additions

    // I-type: rd, rs1, imm
    LWU(MachineOperand, MachineOperand, i32),
    LD(MachineOperand, MachineOperand, i32),
    // S-type: rs1, rs2, imm
    SD(MachineOperand, MachineOperand, i32),
    // I-type: rd, rs1, imm
    ADDIW(MachineOperand, MachineOperand, i32),
    SLLIW(MachineOperand, MachineOperand, i32),
    SRLIW(MachineOperand, MachineOperand, i32),
    SRAIW(MachineOperand, MachineOperand, i32),
    // R-type: rd, rs1, rs2
    ADDW(MachineOperand, MachineOperand, MachineOperand),
    SUBW(MachineOperand, MachineOperand, MachineOperand),
    SLLW(MachineOperand, MachineOperand, MachineOperand),
    SRLW(MachineOperand, MachineOperand, MachineOperand),
    SRAW(MachineOperand, MachineOperand, MachineOperand),

    // RV32M
    // R-type: rd, rs1, rs2
    MUL(MachineOperand, MachineOperand, MachineOperand),
    MULH(MachineOperand, MachineOperand, MachineOperand),
    MULHSU(MachineOperand, MachineOperand, MachineOperand),
    MULHU(MachineOperand, MachineOperand, MachineOperand),
    DIV(MachineOperand, MachineOperand, MachineOperand),
    DIVU(MachineOperand, MachineOperand, MachineOperand),
    REM(MachineOperand, MachineOperand, MachineOperand),
    REMU(MachineOperand, MachineOperand, MachineOperand),

    // RV64M
    // R-type: rd, rs1, rs2
    MULW(MachineOperand, MachineOperand, MachineOperand),
    DIVW(MachineOperand, MachineOperand, MachineOperand),
    DIVUW(MachineOperand, MachineOperand, MachineOperand),
    REMW(MachineOperand, MachineOperand, MachineOperand),
    REMUW(MachineOperand, MachineOperand, MachineOperand),

    // todo: RV32F
}

pub struct MachineInst {
    pub inst: RV64Instruction,
    pub bb: Rc<MachineBB>,
    pub prev: Option<Rc<MachineInst>>,
    pub next: Option<Rc<MachineInst>>,
}

pub struct MachineBB {
    pub bb: BlockId,
    pub insts_head: Option<Rc<MachineInst>>,
    pub insts_tail: Option<Rc<MachineInst>>,
    pub preds: Vec<Rc<MachineBB>>,
    pub succs: Vec<Rc<MachineBB>>,

    pub liveuse: HashSet<MachineOperand>,
    pub livedef: HashSet<MachineOperand>,
    pub livein: HashSet<MachineOperand>,
    pub liveout: HashSet<MachineOperand>,
}

pub struct MachineFunc {
    pub func: String,

    pub virtual_max: u32,

    pub stack_size: u32,

    pub saved_regs: HashSet<RVReg>,
}