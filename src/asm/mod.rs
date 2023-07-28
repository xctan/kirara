use std::{collections::{HashMap, BTreeSet}, fmt::{Debug, Display}, hash::Hash};

use crate::{ir::structure::BlockId, alloc::{Id, Arena}, ctype::Linkage};

pub mod codegen;
pub mod export;
pub mod reg_alloc;
pub mod simplify;
pub mod context;
mod import;

pub trait PhysicalRegister: Clone + Copy + Ord
where
    Self: Sized,
{
    fn is_callee_saved(&self) -> bool;
    /// all registers
    fn registers() -> Vec<Self>;
    /// assignable registers
    fn assignable_registers() -> Vec<Self>;
    /// number of assignable registers
    const NUM: isize;
}

pub trait VirtualRegister: Display + Clone + Copy + Ord {
    fn from_other(id: u32, other: &Self) -> Self;
}

/// Listing of physical registers
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
#[allow(unused)]
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

impl PhysicalRegister for RVGPR {
    fn is_callee_saved(&self) -> bool {
        let id = unsafe { std::mem::transmute::<_, u16>(*self) };
        // s0-s11
        id >= 8 && id <= 9 || id >= 18 && id <= 27
    }

    fn registers() -> Vec<Self> {
        (0..=31)
            .map(|i| unsafe { std::mem::transmute::<u16, RVGPR>(i) })
            .collect()
    }

    fn assignable_registers() -> Vec<Self> {
        (0..=7).map(|x| RVGPR::a(x))
                .chain((1..=11).map(|x| RVGPR::s(x)))
                .chain((0..=6).map(|x| RVGPR::t(x)))
                .collect()
    }

    const NUM: isize = 26;
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
        if i > 7 {
            panic!("invalid A register index");
        }
        unsafe { std::mem::transmute::<u16, RVGPR>(i as u16 + 10) }
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

    pub fn ra() -> Self {
        RVGPR::X1
    }

    pub fn s(i: usize) -> Self {
        if i > 11 {
            panic!("invalid S register index");
        }
        if i < 2 {
            unsafe { std::mem::transmute::<u16, RVGPR>(i as u16 + 8) }
        } else {
            unsafe { std::mem::transmute::<u16, RVGPR>(i as u16 + 16) }
        }
    }

    pub fn t(i: usize) -> Self {
        if i > 6 {
            panic!("invalid T register index");
        }
        if i < 3 {
            unsafe { std::mem::transmute::<u16, RVGPR>(i as u16 + 5) }
        } else {
            unsafe { std::mem::transmute::<u16, RVGPR>(i as u16 + 25) }
        }
    }

    // pub fn x(i: usize) -> Self {
    //     if i > 31 {
    //         panic!("invalid X register index");
    //     }
    //     unsafe { std::mem::transmute::<u16, RVGPR>(i as u16) }
    // }

    // pub fn idx(&self) -> u16 {
    //     unsafe { std::mem::transmute::<_, u16>(*self) }
    // }
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

#[repr(u8)]
pub enum VirtGPRType {
    Int32,
    Int64,
}

impl Display for VirtGPR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty() {
            VirtGPRType::Int32 => write!(f, "i32"),
            VirtGPRType::Int64 => write!(f, "i64"),
        }?;
        write!(f, " %{}", self.0 & 0xffffff)
    }
}

impl Debug for VirtGPR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty() {
            VirtGPRType::Int32 => write!(f, "i32"),
            VirtGPRType::Int64 => write!(f, "i64"),
        }?;
        write!(f, " %{}", self.0 & 0xffffff)
    }
}

impl VirtualRegister for VirtGPR {
    fn from_other(id: u32, other: &Self) -> Self {
        VirtGPR::new(id, other.ty())
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VirtGPR(u32);

impl VirtGPR {
    pub fn new(id: u32, ty: VirtGPRType) -> Self {
        assert!(id < (1 << 24));
        VirtGPR((ty as u32) << 24 | id)
    }

    pub fn ty(&self) -> VirtGPRType {
        match self.0 >> 24 {
            0 => VirtGPRType::Int32,
            1 => VirtGPRType::Int64,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
#[allow(unused)]
#[repr(u16)]
pub enum RVFPR {
    F0,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    F25,
    F26,
    F27,
    F28,
    F29,
    F30,
    F31,
}

impl PhysicalRegister for RVFPR {
    fn is_callee_saved(&self) -> bool {
        let id = unsafe { std::mem::transmute::<_, u16>(*self) };
        // s0-s11
        id >= 8 && id <= 9 || id >= 18 && id <= 27
    }

    fn registers() -> Vec<Self> {
        (0..=31)
            .map(|i| unsafe { std::mem::transmute::<u16, RVFPR>(i) })
            .collect()
    }

    fn assignable_registers() -> Vec<Self> {
        (0..=31)
            .map(|i| unsafe { std::mem::transmute::<u16, RVFPR>(i) })
            .collect()
    }

    const NUM: isize = 32;
}

impl PartialOrd for RVFPR {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        fn classify(reg_num: u16) -> i32 {
            // fa0-fa7 are allocated first
            if reg_num >= 10 && reg_num <= 17 {
                return 1;
            }
            // fs0-fs11 are allocated next
            if reg_num >= 8 && reg_num <= 9 || reg_num >= 18 && reg_num <= 27 {
                return 2;
            }
            // ft0-ft11 are allocated last
            return 3;
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

impl Ord for RVFPR {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl RVFPR {
    pub fn fa(i: usize) -> Self {
        if i > 7 {
            panic!("invalid FA register index");
        }
        unsafe { std::mem::transmute::<u16, RVFPR>(i as u16 + 10) }
    }

    pub fn fx(i: usize) -> Self {
        if i > 31 {
            panic!("invalid FP register index");
        }
        unsafe { std::mem::transmute::<u16, RVFPR>(i as u16) }
    }
}

impl Debug for RVFPR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RVFPR::F0 => write!(f, "ft0"),
            RVFPR::F1 => write!(f, "ft1"),
            RVFPR::F2 => write!(f, "ft2"),
            RVFPR::F3 => write!(f, "ft3"),
            RVFPR::F4 => write!(f, "ft4"),
            RVFPR::F5 => write!(f, "ft5"),
            RVFPR::F6 => write!(f, "ft6"),
            RVFPR::F7 => write!(f, "ft7"),
            RVFPR::F8 => write!(f, "fs0"),
            RVFPR::F9 => write!(f, "fs1"),
            RVFPR::F10 => write!(f, "fa0"),
            RVFPR::F11 => write!(f, "fa1"),
            RVFPR::F12 => write!(f, "fa2"),
            RVFPR::F13 => write!(f, "fa3"),
            RVFPR::F14 => write!(f, "fa4"),
            RVFPR::F15 => write!(f, "fa5"),
            RVFPR::F16 => write!(f, "fa6"),
            RVFPR::F17 => write!(f, "fa7"),
            RVFPR::F18 => write!(f, "fs2"),
            RVFPR::F19 => write!(f, "fs3"),
            RVFPR::F20 => write!(f, "fs4"),
            RVFPR::F21 => write!(f, "fs5"),
            RVFPR::F22 => write!(f, "fs6"),
            RVFPR::F23 => write!(f, "fs7"),
            RVFPR::F24 => write!(f, "fs8"),
            RVFPR::F25 => write!(f, "fs9"),
            RVFPR::F26 => write!(f, "fs10"),
            RVFPR::F27 => write!(f, "fs11"),
            RVFPR::F28 => write!(f, "ft8"),
            RVFPR::F29 => write!(f, "ft9"),
            RVFPR::F30 => write!(f, "ft10"),
            RVFPR::F31 => write!(f, "ft11"),
        }
    }
}

#[repr(u8)]
pub enum VirtFPRType {
    Fp32,
    Fp64,
}

impl VirtualRegister for VirtFPR {
    fn from_other(id: u32, other: &Self) -> Self {
        VirtFPR::new(id, other.ty())
    }
}

impl Display for VirtFPR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty() {
            VirtFPRType::Fp32 => write!(f, "f32")?,
            VirtFPRType::Fp64 => write!(f, "f64")?,
        };
        write!(f, " %{}", self.id())
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VirtFPR(u32);

impl VirtFPR {
    pub fn new(id: u32, ty: VirtFPRType) -> Self {
        assert!(id < (1 << 24));
        VirtFPR((ty as u32) << 24 | id)
    }

    pub fn ty(&self) -> VirtFPRType {
        match self.0 >> 24 {
            0 => VirtFPRType::Fp32,
            1 => VirtFPRType::Fp64,
            _ => unreachable!(),
        }
    }

    pub fn id(&self) -> u32 {
        self.0 & 0xffffff
    }
}

pub trait OperandBase<R, V>
where
    Self: Sized,
    R: PhysicalRegister,
    V: VirtualRegister,
{
    type Register;
    fn virt(i: V) -> Self;
    fn alloc(r: R) -> Self;
    fn pre(r: R) -> Self;
    fn needs_coloring(&self) -> bool;
    fn is_precolored(&self) -> bool;
    fn is_virtual(&self) -> bool;
    fn color(&self) -> Option<R>;
    fn as_virtual(&self) -> V;
    fn physical_regs() -> Vec<Self>;
}

pub trait Operand<R, V>: OperandBase<R, V> + Eq + Hash + Clone + Copy + Ord + Debug + Display
where
    R: PhysicalRegister,
    V: VirtualRegister,
{
    fn load_fn(&self) -> fn(Self, GPOperand, i32) -> RV64Instruction;
    fn store_fn(&self) -> fn(Self, GPOperand, i32) -> RV64Instruction;
    fn size(&self) -> u32;
    fn align(&self) -> u32;
}

macro_rules! define_operand {
    ($operand:ident, $reg:ty, $vreg:ty) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub enum $operand {
            Virtual($vreg),
            Allocated($reg),
            PreColored($reg),
        }

        impl OperandBase<$reg, $vreg> for $operand {
            type Register = $reg;

            #[inline(always)]
            fn virt(i: $vreg) -> Self {
                Self::Virtual(i)
            }

            #[inline(always)]
            fn alloc(r: $reg) -> Self {
                Self::Allocated(r)
            }

            #[inline(always)]
            fn pre(r: $reg) -> Self {
                Self::PreColored(r)
            }
        
            #[inline(always)]
            fn needs_coloring(&self) -> bool {
                matches!(self, Self::Virtual(_) | Self::PreColored(_))
            }
        
            #[inline(always)]
            fn is_precolored(&self) -> bool {
                matches!(self, Self::PreColored(_))
            }

            #[inline(always)]
            fn is_virtual(&self) -> bool {
                matches!(self, Self::Virtual(_))
            }
        
            #[inline(always)]
            fn color(&self) -> Option<$reg> {
                match self {
                    Self::Virtual(_) => None,
                    Self::Allocated(r) => Some(*r),
                    Self::PreColored(r) => Some(*r),
                }
            }

            #[inline(always)]
            fn as_virtual(&self) -> $vreg {
                match self {
                    Self::Virtual(v) => *v,
                    _ => unreachable!(),
                }
            }

            fn physical_regs() -> Vec<Self> {
                <$reg>::registers()
                    .into_iter()
                    .map(Self::pre)
                    .collect()
            }
        }
    }
}

define_operand!(GPOperand, RVGPR, VirtGPR);

impl Operand<RVGPR, VirtGPR> for GPOperand
where
    Self: OperandBase<RVGPR, VirtGPR>,
{
    fn load_fn(&self) -> fn(Self, GPOperand, i32) -> RV64Instruction {
        match self.as_virtual().ty() {
            VirtGPRType::Int32 => RV64InstBuilder::LW,
            VirtGPRType::Int64 => RV64InstBuilder::LD,
        }
    }

    fn store_fn(&self) -> fn(Self, GPOperand, i32) -> RV64Instruction {
        match self.as_virtual().ty() {
            VirtGPRType::Int32 => RV64InstBuilder::SW,
            VirtGPRType::Int64 => RV64InstBuilder::SD,
        }
    }

    fn size(&self) -> u32 {
        match self.as_virtual().ty() {
            VirtGPRType::Int32 => 4,
            VirtGPRType::Int64 => 8,
        }
    }

    fn align(&self) -> u32 {
        match self.as_virtual().ty() {
            VirtGPRType::Int32 => 4,
            VirtGPRType::Int64 => 8,
        }
    }
}

define_operand!(FPOperand, RVFPR, VirtFPR);

impl Operand<RVFPR, VirtFPR> for FPOperand {
    fn load_fn(&self) -> fn(Self, GPOperand, i32) -> RV64Instruction {
        todo!()
    }

    fn store_fn(&self) -> fn(Self, GPOperand, i32) -> RV64Instruction {
        todo!()
    }

    fn size(&self) -> u32 {
        todo!()
    }

    fn align(&self) -> u32 {
        todo!()
    }
}

impl Display for FPOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum RV64Instruction {
    // RV32I

    // U-type: rd, imm
    LUI { rd: GPOperand, imm: i32 },
    AUIPC { rd: GPOperand, imm: i32 },
    // J-type: rd, imm
    JAL { rd: GPOperand, imm: i32 },
    // I-type: rd, rs1, imm
    JALR { rd: GPOperand, rs1: GPOperand, imm: i32 },
    // B-type: rs1, rs2, imm
    BEQ { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    BNE { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    BLT { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    BGE { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    BLTU { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    BGEU { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    // I-type: rd, rs1, imm
    LB { rd: GPOperand, rs1: GPOperand, imm: i32 },
    LH { rd: GPOperand, rs1: GPOperand, imm: i32 },
    LW { rd: GPOperand, rs1: GPOperand, imm: i32 },
    LBU { rd: GPOperand, rs1: GPOperand, imm: i32 },
    LHU { rd: GPOperand, rs1: GPOperand, imm: i32 },
    // S-type: rs1, rs2, imm
    SB { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    SH { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    SW { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    // I-type: rd, rs1, imm
    ADDI { rd: GPOperand, rs1: GPOperand, imm: i32 },
    SLTI { rd: GPOperand, rs1: GPOperand, imm: i32 },
    SLTIU { rd: GPOperand, rs1: GPOperand, imm: i32 },
    XORI { rd: GPOperand, rs1: GPOperand, imm: i32 },
    ORI { rd: GPOperand, rs1: GPOperand, imm: i32 },
    ANDI { rd: GPOperand, rs1: GPOperand, imm: i32 },
    SLLI { rd: GPOperand, rs1: GPOperand, imm: i32 },
    SRLI { rd: GPOperand, rs1: GPOperand, imm: i32 },
    SRAI { rd: GPOperand, rs1: GPOperand, imm: i32 },
    // R-type: rd, rs1, rs2
    ADD { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SUB { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SLL { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SLT { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SLTU { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    XOR { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SRL { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SRA { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    OR { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    AND { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },

    // RV64I additions

    // I-type: rd, rs1, imm
    LWU { rd: GPOperand, rs1: GPOperand, imm: i32 },
    LD { rd: GPOperand, rs1: GPOperand, imm: i32 },
    // S-type: rs1, rs2, imm
    SD { rs1: GPOperand, rs2: GPOperand, imm: i32 },
    // I-type: rd, rs1, imm
    ADDIW { rd: GPOperand, rs1: GPOperand, imm: i32 },
    SLLIW { rd: GPOperand, rs1: GPOperand, imm: i32 },
    SRLIW { rd: GPOperand, rs1: GPOperand, imm: i32 },
    SRAIW { rd: GPOperand, rs1: GPOperand, imm: i32 },
    // R-type: rd, rs1, rs2
    ADDW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SUBW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SLLW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SRLW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SRAW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },

    // RV32M
    // R-type: rd, rs1, rs2
    MUL { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    MULH { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    MULHSU { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    MULHU { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    DIV { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    DIVU { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    REM { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    REMU { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },

    // RV64M
    // R-type: rd, rs1, rs2
    MULW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    DIVW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    DIVUW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    REMW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    REMUW { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },

    // todo: RV32F

    // pseudo instructions for convenience
    COMMENT { comment: String },
    CALL { callee: String, params: usize },
    RET,
    // alias for addi
    MV { rd: GPOperand, rs: GPOperand },
    // fused branch instructions with logical targets
    JEQ { rs1: GPOperand, rs2: GPOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JNE { rs1: GPOperand, rs2: GPOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JLT { rs1: GPOperand, rs2: GPOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JGE { rs1: GPOperand, rs2: GPOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JLTU { rs1: GPOperand, rs2: GPOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JGEU { rs1: GPOperand, rs2: GPOperand, succ: Id<MachineBB>, fail: Id<MachineBB> },
    JUMP { target: Id<MachineBB> },
    LIMM { rd: GPOperand, imm: i32 },
    LADDR { rd: GPOperand, label: String },
    // fused compare instructions for inlining
    SEQ { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SNE { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SGE { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    SGEU { rd: GPOperand, rs1: GPOperand, rs2: GPOperand },
    // prologue and epilogue of a function
    ENTER,
    LEAVE,
    NOP,
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
    /// Get destination vreg for peephole optimization on the fly
    pub fn get_rd(&self) -> Option<GPOperand> {
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
}

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
            return (vec![], vec![GPOperand::PreColored(RVGPR::a(0))]);
        }
        if let RV64Instruction::JUMP { .. } = self {
            return (vec![], vec![]);
        }
        if matches!(self, RV64Instruction::ENTER | RV64Instruction::LEAVE | RV64Instruction::COMMENT { .. }) {
            return (vec![], vec![]);
        }
        if let RV64Instruction::NOP = self {
            return (vec![], vec![]);
        }
        if let RV64Instruction::CALL { params, .. } = self {
            let uses: Vec<_> = (0..usize::min(8, *params))
                .map(|i| GPOperand::PreColored(RVGPR::a(i)))
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
        }
        implement_typed_instruction!(extract);

        if let RV64Instruction::LUI { rd, .. } = self {
            return vec![rd];
        }
        if let RV64Instruction::MV { rd, rs } = self {
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
        nop!(CALL);

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
        }
        implement_typed_instruction!(extract);

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
        nop!(CALL);

        panic!("unimplemented: {:?}", self)
    }
}

macro_rules! builder_impl_rv64 {
    (r ; $mnemonic:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused)]
        pub fn $mnemonic(rd: GPOperand, rs1: GPOperand, rs2: GPOperand) -> RV64Instruction {
            RV64Instruction::$mnemonic { rd, rs1, rs2 }
        }
    };
    (i ; $mnemonic:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused)]
        pub fn $mnemonic(rd: GPOperand, rs1: GPOperand, imm: i32) -> RV64Instruction {
            // skiping range check to encode %pcrel_hi %perel_lo
            RV64Instruction::$mnemonic { rd, rs1, imm }
        }
    };
    (s; $mnemonic:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused)]
        #[inline(never)]
        pub fn $mnemonic(rs2: GPOperand, rs1: GPOperand, imm: i32) -> RV64Instruction {
            // same as above
            RV64Instruction::$mnemonic { rs1, rs2, imm }
        }
    };
    (cj; $mnemonic:ident) => {
        #[allow(non_snake_case)]
        #[allow(unused)]
        pub fn $mnemonic(rs1: GPOperand, rs2: GPOperand, succ: Id<MachineBB>, fail: Id<MachineBB>) -> RV64Instruction {
            RV64Instruction::$mnemonic { rs1, rs2, succ, fail }
        }
    };
}

pub struct RV64InstBuilder;

impl RV64InstBuilder {
    implement_typed_instruction!(builder_impl_rv64);

    #[allow(non_snake_case)]
    pub fn LUI(rd: GPOperand, imm: i32) -> RV64Instruction {
        RV64Instruction::LUI { rd, imm }
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
    pub fn CALL(callee: String, params: usize) -> RV64Instruction {
        RV64Instruction::CALL { callee, params }
    }
    #[allow(non_snake_case)]
    pub fn RET() -> RV64Instruction {
        RV64Instruction::RET
    }
    #[allow(non_snake_case)]
    pub fn MV(rd: GPOperand, rs: GPOperand) -> RV64Instruction {
        RV64Instruction::MV { rd, rs }
    }
    #[allow(non_snake_case)]
    pub fn LIMM(rd: GPOperand, imm: i32) -> RV64Instruction {
        RV64Instruction::LIMM { rd, imm }
    }
    #[allow(non_snake_case)]
    pub fn LADDR(rd: GPOperand, label: String) -> RV64Instruction {
        RV64Instruction::LADDR { rd, label }
    }
    #[allow(non_snake_case)]
    pub fn LEAVE() -> RV64Instruction {
        RV64Instruction::LEAVE
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
    pub name: String,
    pub insts_head: Option<Id<MachineInst>>,
    pub insts_tail: Option<Id<MachineInst>>,
    pub preds: Vec<Id<MachineBB>>,
    pub succs: Vec<Id<MachineBB>>,

}

impl MachineBB {
    pub fn new(bb: BlockId, name: &str) -> Self {
        Self {
            bb,
            name: String::from(name),
            insts_head: None,
            insts_tail: None,
            preds: Vec::new(),
            succs: Vec::new(),
        }
    }
}

pub struct MachineFunc {
    pub func: String,
    pub entry: Option<Id<MachineBB>>,
    pub bbs: Vec<Id<MachineBB>>,
    pub virtual_max: u32,
    pub stack_size: u32,
    pub used_regs: BTreeSet<RVGPR>,
    pub virtual_gprs: BTreeSet<VirtGPR>,

    // use_lr?

    // todo: omit_fp_fixup, for sp-relative addressing
}

pub trait FuncVirtReg<V>
where
    V: VirtualRegister
{
    fn get_vreg(&self) -> &BTreeSet<V>;
}

impl FuncVirtReg<VirtGPR> for MachineFunc {
    fn get_vreg(&self) -> &BTreeSet<VirtGPR> {
        &self.virtual_gprs
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DataLiteral {
    // .byte
    // .half
    /// .word
    Word(u32),
    /// .word for float
    WordHex(u32),
    // .quad
    /// .zero
    Zero(u32),
}

#[derive(Debug)]
pub struct AsmGlobalObject {
    pub data: Vec<DataLiteral>,
    pub linkage: Linkage,
}

pub struct MachineProgram {
    pub funcs: HashMap<String, MachineFunc>,

    /// data of global variables (functions are not included)
    pub symbols: HashMap<String, AsmGlobalObject>,

    // global_decl

    pub blocks: Arena<MachineBB>,
    pub insts: Arena<MachineInst>,
    // mbb to mfunc map
    pub block_map: HashMap<Id<MachineBB>, usize>,

    /// record the final definition of each virtual register
    pub vreg_def: HashMap<GPOperand, Id<MachineInst>>,
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
            stack_size: 0,
            used_regs: BTreeSet::new(),
            virtual_gprs: BTreeSet::new(),
        }
    }

    pub fn push_to_end(&mut self, mbb: Id<MachineBB>, inst: RV64Instruction) -> Id<MachineInst> {
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
        minst_id
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

    fn define_vreg(&mut self, vreg: GPOperand, minst: Id<MachineInst>) {
        if matches!(vreg, GPOperand::Virtual(_)) {
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

#[inline(always)]
pub fn round_up(value: u32, align: u32) -> u32 {
    (value + align - 1) & !(align - 1)
}