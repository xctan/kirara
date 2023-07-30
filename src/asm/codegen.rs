use std::{collections::{HashMap, HashSet}, rc::Rc};

use crate::{
    ir::{structure::{BasicBlock, TransUnit},
    value::{Value, ValueType, InstructionValue, ValueTrait, ConstantValue, calculate_used_by, UnaryOp}},
    alloc::Id, asm::{RV64InstBuilder, RVGPR}, ctype::{TypeKind, BinaryOpType, Type}, ast::{Initializer, InitData},
};

use super::{
    MachineProgram, MachineFunc, MachineBB, GPOperand, RV64Instruction,
    DataLiteral, MachineInst, AsmGlobalObject, VirtGPR, VirtGPRType, FPOperand, VirtFPRType, VirtFPR, RVFPR,
};

impl TransUnit {
    pub fn emit_asm(&mut self) -> MachineProgram {
        let mut program = MachineProgram::new();

        for (global, init) in &self.globals {
            program.symbols.insert(global.clone(), AsmGlobalObject {
                data: compress(flatten_initializer(&init.init)),
                linkage: init.linkage,
            });
        }

        for func in self.funcs() {
            let mfunc = AsmFuncBuilder::new(&func, &mut program, self).build();
            program.funcs.insert(func, mfunc);
        }

        program
    }
}

fn flatten_initializer(init: &Initializer) -> Vec<DataLiteral> {
    match init.data {
        InitData::ScalarI32(i) => vec![DataLiteral::Word(i as u32)],
        InitData::ScalarF32(f) => vec![DataLiteral::WordHex(f.to_bits())],
        InitData::Aggregate(ref aggr) => {
            let mut res = Vec::new();
            for init in aggr.iter() {
                res.extend(flatten_initializer(init));
            }
            res
        }
        InitData::ZeroInit => vec![DataLiteral::Zero(init.ty.size as u32)],
        _ => unimplemented!("flatten_initializer"),
    }
}

fn compress(data: Vec<DataLiteral>) -> Vec<DataLiteral> {
    let mut res = Vec::new();
    let mut zero_counter = 0;
    for d in data {
        match d {
            DataLiteral::Word(w) => {
                if w != 0 {
                    if zero_counter > 0 {
                        res.push(DataLiteral::Zero(zero_counter));
                        zero_counter = 0;
                    }
                    res.push(DataLiteral::Word(w));
                } else {
                    zero_counter += 4;
                }
            }
            DataLiteral::WordHex(w) => {
                if w != 0 {
                    if zero_counter > 0 {
                        res.push(DataLiteral::Zero(zero_counter));
                        zero_counter = 0;
                    }
                    res.push(DataLiteral::WordHex(w));
                } else {
                    zero_counter += 4;
                }
            }
            DataLiteral::Zero(z) => {
                zero_counter += z;
            }
        }
    }
    if zero_counter > 0 {
        res.push(DataLiteral::Zero(zero_counter));
    }
    res
}

struct AsmFuncBuilder<'a> {
    pub name: String,
    pub prog: &'a mut MachineProgram,
    pub unit: &'a mut TransUnit,

    pub bb_map: HashMap<Id<BasicBlock>, Id<MachineBB>>,

    // mappings from ir entity to asm entity
    pub val_map: HashMap<Id<Value>, GPOperand>,
    pub val_mapf: HashMap<Id<Value>, FPOperand>,
    // todo: global decl

    pub virtual_max: u32,
    pub used_regs: HashSet<RVGPR>,
    pub virtual_gprs: HashSet<VirtGPR>,
    pub virtual_fprs: HashSet<VirtFPR>,

    entry_marker: Option<Id<MachineInst>>,
}

macro_rules! pre {
    (zero) => { GPOperand::PreColored(RVGPR::zero()) };
    (a0) => { GPOperand::PreColored(RVGPR::a(0)) };
    (fp) => { GPOperand::PreColored(RVGPR::fp()) };
    (sp) => { GPOperand::PreColored(RVGPR::sp()) };
    (a $i:expr) => { GPOperand::PreColored(RVGPR::a($i)) };
    (fa0) => { FPOperand::PreColored(RVFPR::fa(0)) };
    (fa $i:expr) => { FPOperand::PreColored(RVFPR::fa($i)) };
}

impl<'a> AsmFuncBuilder<'a> {
    pub fn new(name: &str, prog: &'a mut MachineProgram, unit: &'a mut TransUnit) -> Self {
        Self {
            name: name.to_string(),
            prog,
            unit,
            bb_map: HashMap::new(),
            val_map: HashMap::new(),
            val_mapf: HashMap::new(),
            virtual_max: 1,
            used_regs: HashSet::new(),
            virtual_gprs: HashSet::new(),
            virtual_fprs: HashSet::new(),
            entry_marker: None,
        }
    }

    pub fn build(mut self) -> MachineFunc {
        let mut mfunc = self.build_inner();
        mfunc.virtual_max = self.virtual_max;
        mfunc.used_regs.extend(self.used_regs);
        mfunc.virtual_gprs.extend(self.virtual_gprs);
        mfunc.virtual_fprs.extend(self.virtual_fprs);

        mfunc
    }

    fn build_inner(&mut self) -> MachineFunc {
        let mut mfunc = self.prog.new_func(self.name.clone());
        calculate_used_by(self.unit, self.name.as_str());

        // 1. create machine bb as per ir bb
        let irfunc = self.unit.funcs[self.name.as_str()].clone();
        for bb in &irfunc.bbs {
            let block = &self.unit.blocks[*bb];
            let mbb = self.prog.blocks.alloc(MachineBB::new(*bb, &block.name));
            self.bb_map.insert(*bb, mbb);
            mfunc.bbs.push(mbb);
        }
        // maintain preds and succs
        for bb in &mfunc.bbs {
            let block = &self.prog.blocks[*bb];
            let irbb = self.unit.blocks.get(block.bb).unwrap();
            let block_mut = &mut self.prog.blocks[*bb];
            for pred in irbb.preds.iter() {
                block_mut.preds.push(self.bb_map[pred]);
            }
            for succ in self.unit.succ(block_mut.bb) {
                block_mut.succs.push(self.bb_map[&succ]);
            }
        }
        mfunc.entry = Some(self.bb_map[&irfunc.entry_bb]);
        let entry_marker = self.prog.push_to_end(
            mfunc.entry.unwrap(),
            RV64Instruction::NOP,
        );
        self.entry_marker = Some(entry_marker);

        // 2. translate instructions except phi
        for bb in &irfunc.bbs {
            let mbb = self.bb_map[bb];
            let block = &self.unit.blocks[*bb];
            macro_rules! emit {
                // as for the first operand:
                // - for SW/SD, the first operand is also rs
                ($mnemonic:ident $($operand0:expr)? $(, $($operand:expr),*)?) => {{
                    $(if
                        RV64InstBuilder::$mnemonic as usize == RV64InstBuilder::SD as usize ||
                        RV64InstBuilder::$mnemonic as usize == RV64InstBuilder::SW as usize
                    {
                        self.mark_usage($operand0);
                    })?
                    $($(self.mark_usage($operand);)*)?
                    self.prog.push_to_end(mbb, RV64InstBuilder::$mnemonic($($operand0, )? $($($operand),*)?))
                }};
                ($mnemonic:expr ; $($operand0:expr)? $(, $($operand:expr),*)?) => {{
                    $(if
                        $mnemonic as usize == RV64InstBuilder::SD as usize ||
                        $mnemonic as usize == RV64InstBuilder::SW as usize
                    {
                        self.mark_usage($operand0);
                    })?
                    $($(self.mark_usage($operand);)*)?
                    self.prog.push_to_end(mbb, $mnemonic($($operand0, )? $($($operand),*)?))
                }};
            }

            let mut iter = block.insts_start;
            while let Some(inst) = iter {
                let insn = &self.unit.values[inst].clone();
                iter = insn.next;

                let iv = insn.value.as_inst().clone();
                match iv {
                    InstructionValue::Binary(b) if self.is_integral(b.lhs) => {
                        let dst = self.resolve(inst, mbb);

                        let mo_lhs = self.resolve_ensure_reg(b.lhs, mbb);

                        let val_rhs = self.unit.values[b.rhs].clone();
                        if val_rhs.value.is_constant() && is_i_type(b.op) {
                            match *val_rhs.value.as_constant() {
                                ConstantValue::I32(mut i) => {
                                    if matches!(b.op, BinaryOpType::Sub) {
                                        // use addi to sub
                                        i = -i;
                                    }
                                    if is_imm12(i) {
                                        let eliminate_cmp = insn.used_by
                                            .iter()
                                            .all(|val_id| {
                                                let value = &self.unit.values[*val_id];
                                                matches!(
                                                    value.value, 
                                                    ValueType::Instruction(InstructionValue::Branch(_))
                                                )
                                            });
                                        match b.op {
                                            BinaryOpType::Add | BinaryOpType::Sub => {
                                                emit!(ADDIW dst, mo_lhs, i);
                                            }
                                            BinaryOpType::Ne => {
                                                if eliminate_cmp {
                                                    let tmp = self.resolve_ensure_reg(b.rhs, mbb);
                                                    emit!(SNE dst, mo_lhs, tmp);
                                                } else {
                                                    emit!(XORI dst, mo_lhs, i);
                                                    emit!(SLTU dst, pre!(zero), dst);
                                                }
                                            }
                                            BinaryOpType::Eq => {
                                                if eliminate_cmp {
                                                    let tmp = self.resolve_ensure_reg(b.rhs, mbb);
                                                    emit!(SEQ dst, mo_lhs, tmp);
                                                } else {
                                                    emit!(XORI dst, mo_lhs, i);
                                                    emit!(SLTIU dst, dst, 1);
                                                }
                                            }
                                            BinaryOpType::Lt => {
                                                if eliminate_cmp {
                                                    let tmp = self.resolve_ensure_reg(b.rhs, mbb);
                                                    emit!(SLT dst, mo_lhs, tmp);
                                                } else {
                                                    emit!(SLTI dst, mo_lhs, i);
                                                }
                                            }
                                            BinaryOpType::Le => {
                                                let tmp = self.resolve_ensure_reg(b.rhs, mbb);
                                                emit!(SGE dst, tmp, mo_lhs);
                                            }
                                            BinaryOpType::Gt => {
                                                let tmp = self.resolve_ensure_reg(b.rhs, mbb);
                                                emit!(SLT dst, tmp, mo_lhs);
                                            }
                                            BinaryOpType::Ge => {
                                                if eliminate_cmp {
                                                    let tmp = self.resolve_ensure_reg(b.rhs, mbb);
                                                    emit!(SGE dst, mo_lhs, tmp);
                                                } else {
                                                    emit!(SLTI dst, mo_lhs, i);
                                                    emit!(XORI dst, dst, 1);
                                                }
                                            }
                                            _ => unimplemented!(),
                                        }
                                    } else {
                                        // fallback to register
                                        match b.op {
                                            BinaryOpType::Add | BinaryOpType::Sub => {
                                                let tmp = self.new_vreg();
                                                let (hi, lo) = split_imm32(i);
                                                emit!(LUI tmp, hi);
                                                emit!(ADDIW tmp, tmp, lo);
                                                continue
                                            }
                                            _ => {}
                                        }
                                        let tmp = self.resolve_ensure_reg(b.rhs, mbb);
                                        match b.op {
                                            BinaryOpType::Ne => {
                                                // emit!(XOR dst, mo_lhs, tmp);
                                                // emit!(SLTU dst, pre!(zero), dst);
                                                emit!(SNE dst, mo_lhs, tmp);
                                            }
                                            BinaryOpType::Eq => {
                                                // emit!(XOR dst, mo_lhs, tmp);
                                                // emit!(SLTIU dst, dst, 1);
                                                emit!(SEQ dst, mo_lhs, tmp);
                                            }
                                            BinaryOpType::Lt => {
                                                emit!(SLT dst, mo_lhs, tmp);
                                            }
                                            BinaryOpType::Le => {
                                                emit!(SGE dst, tmp, mo_lhs);
                                            }
                                            BinaryOpType::Gt => {
                                                emit!(SLT dst, tmp, mo_lhs);
                                            }
                                            BinaryOpType::Ge => {
                                                emit!(SGE dst, mo_lhs, tmp);
                                            }
                                            _ => unimplemented!(),
                                        }
                                    }
                                }
                                ConstantValue::I1(i) => {
                                    match b.op {
                                        BinaryOpType::Xor => {
                                            emit!(XORI dst, mo_lhs, i as i32);
                                        }
                                        _ => unimplemented!(),
                                    }
                                }
                                _ => unimplemented!(),
                            }
                        } else {
                            let mo_rhs = self.resolve_ensure_reg(b.rhs, mbb);
                            match b.ty().kind {
                                TypeKind::I32 => {
                                    match b.op {
                                        BinaryOpType::Add => {
                                            emit!(ADDW dst, mo_lhs, mo_rhs);
                                        }
                                        BinaryOpType::Sub => {
                                            emit!(SUBW dst, mo_lhs, mo_rhs);
                                        }
                                        BinaryOpType::Mul => {
                                            emit!(MULW dst, mo_lhs, mo_rhs);
                                        }
                                        BinaryOpType::Div => {
                                            emit!(DIVW dst, mo_lhs, mo_rhs);
                                        }
                                        BinaryOpType::Mod => {
                                            emit!(REMW dst, mo_lhs, mo_rhs);
                                        }
                                        _ => unimplemented!(),
                                    }
                                }
                                TypeKind::I1 => {
                                    match b.op {
                                        BinaryOpType::Ne => {
                                            emit!(SNE dst, mo_lhs, mo_rhs);
                                        }
                                        BinaryOpType::Eq => {
                                            emit!(SEQ dst, mo_lhs, mo_rhs);
                                        }
                                        BinaryOpType::Lt => {
                                            emit!(SLT dst, mo_lhs, mo_rhs);
                                        }
                                        BinaryOpType::Le => {
                                            // !(rhs < lhs)
                                            emit!(SGE dst, mo_rhs, mo_lhs);
                                        }
                                        BinaryOpType::Gt => {
                                            emit!(SLT dst, mo_rhs, mo_lhs);
                                        }
                                        BinaryOpType::Ge => {
                                            // !(lhs < rhs)
                                            emit!(SGE dst, mo_lhs, mo_rhs);
                                        }
                                        BinaryOpType::Xor => {
                                            emit!(XOR dst, mo_lhs, mo_rhs);
                                        }
                                        _ => unimplemented!(),
                                    }
                                }
                                _ => unimplemented!("binop, type: {:?}", b.ty()),
                            }
                        }
                    },
                    InstructionValue::Binary(b) if self.is_floating(b.lhs) => {
                        let mo_lhs = self.resolve_fp(b.lhs, mbb);
                        let mo_rhs = self.resolve_fp(b.rhs, mbb);
                        match b.op {
                            BinaryOpType::Add |
                            BinaryOpType::Sub |
                            BinaryOpType::Mul |
                            BinaryOpType::Div => {
                                let dst = self.resolve_fp(inst, mbb);
                                match b.op {
                                    BinaryOpType::Add => {
                                        emit!(FADDS dst, mo_lhs, mo_rhs);
                                    },
                                    BinaryOpType::Sub => {
                                        emit!(FSUBS dst, mo_lhs, mo_rhs);
                                    },
                                    BinaryOpType::Mul => {
                                        emit!(FMULS dst, mo_lhs, mo_rhs);
                                    },
                                    BinaryOpType::Div => {
                                        emit!(FDIVS dst, mo_lhs, mo_rhs);
                                    },
                                    _ => unreachable!(),
                                }
                            },
                            BinaryOpType::Ne |
                            BinaryOpType::Eq |
                            BinaryOpType::Lt |
                            BinaryOpType::Le |
                            BinaryOpType::Gt |
                            BinaryOpType::Ge => {
                                let dst = self.resolve(inst, mbb);
                                match b.op {
                                    BinaryOpType::Ne => {
                                        emit!(FEQS dst, mo_lhs, mo_rhs);
                                        emit!(XORI dst, dst, 1);
                                    },
                                    BinaryOpType::Eq => {
                                        emit!(FEQS dst, mo_lhs, mo_rhs);
                                    },
                                    BinaryOpType::Lt => {
                                        emit!(FLTS dst, mo_lhs, mo_rhs);
                                    },
                                    BinaryOpType::Le => {
                                        emit!(FLES dst, mo_lhs, mo_rhs);
                                    },
                                    BinaryOpType::Gt => {
                                        emit!(FLTS dst, mo_rhs, mo_lhs);
                                    },
                                    BinaryOpType::Ge => {
                                        emit!(FLES dst, mo_rhs, mo_lhs);
                                    },
                                    _ => unreachable!(),
                                }
                            },
                            _ => unreachable!(),
                        }
                    },
                    InstructionValue::Binary(b) => {
                        unreachable!("binop, type: {:?}", b.ty());
                    }
                    InstructionValue::Load(l) => {
                        match l.ty().kind {
                            TypeKind::I32 | TypeKind::I64 | TypeKind::Ptr(_) => {
                                let instruction = match l.ty.kind {
                                    TypeKind::I32 => RV64InstBuilder::LW,
                                    TypeKind::I64 => RV64InstBuilder::LD,
                                    TypeKind::Ptr(_) => RV64InstBuilder::LD,
                                    _ => unimplemented!("load type: {:?}", l.ty),
                                };
        
                                let ptr = self.resolve(l.ptr, mbb);
                                let dst = self.resolve(inst, mbb);
                                if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                                    let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                                    if let RV64Instruction::ADDI { rs1, imm, .. } = ptr_inst {
                                        emit!(instruction; dst, rs1, imm);
                                    } else {
                                        emit!(instruction; dst, ptr, 0);
                                    }
                                } else {
                                    emit!(instruction; dst, ptr, 0);
                                }
                            }
                            TypeKind::F32 => {
                                let ptr = self.resolve(l.ptr, mbb);
                                let dst = self.resolve_fp(inst, mbb);
                                if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                                    let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                                    if let RV64Instruction::ADDI { rs1, imm, .. } = ptr_inst {
                                        emit!(FLW dst, rs1, imm);
                                    } else {
                                        emit!(FLW dst, ptr, 0);
                                    }
                                } else {
                                    emit!(FLW dst, ptr, 0);
                                }
                            }
                            _ => unimplemented!("store type: {:?}", l.ty()),
                        }
                    },
                    InstructionValue::Store(s) => {
                        let val = self.unit.values[s.value].clone();
                        match val.ty().kind {
                            TypeKind::I32 | TypeKind::I64 | TypeKind::Ptr(_) => {
                                let instruction = match val.ty().kind {
                                    TypeKind::I32 => RV64InstBuilder::SW,
                                    TypeKind::I64 => RV64InstBuilder::SD,
                                    TypeKind::Ptr(_) => RV64InstBuilder::SD,
                                    _ => unreachable!("store type: {:?}", val.ty()),
                                };
        
                                let ptr = self.resolve(s.ptr, mbb);
                                let src = self.resolve_ensure_reg(s.value, mbb);
                                if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                                    let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                                    if let RV64Instruction::ADDI { rs1, imm, .. } = ptr_inst {
                                        emit!(instruction; src, rs1, imm);
                                    } else {
                                        emit!(instruction; src, ptr, 0);
                                    }
                                } else {
                                    emit!(instruction; src, ptr, 0);
                                }
                            }
                            TypeKind::F32 => {
                                let ptr = self.resolve(s.ptr, mbb);
                                let src = self.resolve_fp(s.value, mbb);
                                if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                                    let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                                    if let RV64Instruction::ADDI { rs1, imm, .. } = ptr_inst {
                                        emit!(FSW src, rs1, imm);
                                    } else {
                                        emit!(FSW src, ptr, 0);
                                    }
                                } else {
                                    emit!(FSW src, ptr, 0);
                                }
                            }
                            _ => unimplemented!("store type: {:?}", val.ty()),
                        }
                    },
                    InstructionValue::Alloca(a) => {
                        let dst = self.resolve(inst, mbb);
                        let object_size = a.alloc_ty.size() as u32;
                        let object_align = a.alloc_ty.align() as u32;
                        // fixup
                        if mfunc.stack_size % object_align != 0 {
                            mfunc.stack_size += object_align - mfunc.stack_size % object_align;
                        }
                        let object_begin = mfunc.stack_size;
                        mfunc.stack_size += object_size;
                        // FIXME: sp changed if any register is spilled, so offset should be fixed later
                        if is_imm12(object_begin as i32) {
                            emit!(ADDI dst, pre!(sp), object_begin as i32);
                        } else {
                            emit!(LIMM dst, object_begin as i32);
                            emit!(ADD dst, pre!(sp), dst);
                            // let mut amount = object_begin as i32;
                            // let mut first = true;
                            // while amount > 0 {
                            //     let chip = i32::min(2032, amount);
                            //     amount -= chip;
                            //     if first {
                            //         emit!(ADDI dst, pre!(sp), chip);
                            //         first = false;
                            //     } else {
                            //         emit!(ADDI dst, dst, chip);
                            //     }
                            // }
                        }
                        // // alternative stack layout
                        // if is_imm12(-(mfunc.stack_size as i32) - 16) {
                        //     emit!(ADDI dst, pre!(fp), -(mfunc.stack_size as i32) - 16);
                        // } else {
                        //     emit!(LIMM dst, -(mfunc.stack_size as i32) - 16);
                        //     emit!(ADD dst, pre!(fp), dst);
                        // }
                        // self.used_regs.insert(RVGPR::fp());
                    },
                    InstructionValue::Return(r) => {
                        if let Some(val) = r.value {
                            self.used_regs.insert(RVGPR::a(0));
                            let value = self.unit.values[val].clone();
                            match value.ty().kind {
                                TypeKind::I32 => {
                                    if let Some(imm) = self.resolve_constant(val) {
                                        emit!(LIMM pre!(a0), imm);
                                    } else {
                                        let src = self.resolve(val, mbb);
                                        // avoid using addi directly here, because it shouldn't be inlined
                                        emit!(MV pre!(a0), src);
                                    }
                                },
                                TypeKind::F32 => {
                                    let src = self.resolve_fp(val, mbb);
                                    emit!(FMVSS pre!(fa0), src);
                                },
                                _ => unimplemented!("return type: {:?}", value.ty()),
                            }
                        }
                        emit!(LEAVE);
                        emit!(RET);
                    },
                    InstructionValue::Branch(b) => {
                        let cond_reg = self.resolve_ensure_reg(b.cond, mbb);
                        if let Some(cond_id) = self.prog.vreg_def.get(&cond_reg) {
                            let cond_inst = self.prog.insts[*cond_id].inst.clone();
                            match cond_inst {
                                RV64Instruction::SEQ { rs1, rs2, .. } => {
                                    emit!(JEQ rs1, rs2, self.bb_map[&b.succ], self.bb_map[&b.fail]);
                                },
                                RV64Instruction::SNE { rs1, rs2, .. } => {
                                    emit!(JNE rs1, rs2, self.bb_map[&b.succ], self.bb_map[&b.fail]);
                                },
                                RV64Instruction::SLT { rs1, rs2, .. } => {
                                    emit!(JLT rs1, rs2, self.bb_map[&b.succ], self.bb_map[&b.fail]);
                                },
                                RV64Instruction::SGE { rs1, rs2, .. } => {
                                    emit!(JGE rs1, rs2, self.bb_map[&b.succ], self.bb_map[&b.fail]);
                                },
                                RV64Instruction::SLTU { rs1, rs2, .. } => {
                                    emit!(JLTU rs1, rs2, self.bb_map[&b.succ], self.bb_map[&b.fail]);
                                },
                                RV64Instruction::SGEU { rs1, rs2, .. } => {
                                    emit!(JGEU rs1, rs2, self.bb_map[&b.succ], self.bb_map[&b.fail]);
                                },
                                _ => {
                                    emit!(JNE cond_reg, pre!(zero), self.bb_map[&b.succ], self.bb_map[&b.fail]);
                                }
                            }
                        } else {
                            emit!(JNE cond_reg, pre!(zero), self.bb_map[&b.succ], self.bb_map[&b.fail]);
                        }
                    },
                    InstructionValue::Jump(j) => {
                        emit!(JUMP self.bb_map[&j.succ]);
                    },
                    InstructionValue::Unary(c) => {
                        // todo: this should be a general conversion instruction!
                        match c.op {
                            UnaryOp::ZextI32I1 => {
                                let dst = self.resolve(inst, mbb);
                                let val = self.resolve_ensure_reg(c.value, mbb);
                                emit!(MV dst, val);
                            }
                            UnaryOp::CvtF32I32 => {
                                let dst = self.resolve_fp(inst, mbb);
                                let val = self.resolve_ensure_reg(c.value, mbb);
                                emit!(FCVTSW dst, val);
                            }
                            UnaryOp::CvtI32F32 => {
                                let dst = self.resolve(inst, mbb);
                                let val = self.resolve_fp(c.value, mbb);
                                emit!(FCVTWS dst, val);
                            }
                            UnaryOp::NegF32 => {
                                let dst = self.resolve_fp(inst, mbb);
                                let val = self.resolve_fp(c.value, mbb);
                                emit!(FNEGS dst, val);
                            }
                        }
                    },
                    InstructionValue::Phi(_) => (),
                    InstructionValue::GetElemPtr(g) => {
                        let dst = self.resolve(inst, mbb);
                        let mut ptr = self.resolve(g.ptr, mbb);
                        let mut ty = Type::ptr_to(g.base_ty);
                        for index in g.indices {
                            let elem_size = ty.base_type().size() as i32;
                            ty = ty.base_type();
                            let constant_index = self.resolve_constant(index);
                            if let Some(idx) = constant_index {
                                let offset = idx * elem_size;
                                if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                                    let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                                    if let RV64Instruction::ADDI { rd, rs1, imm } = ptr_inst {
                                        let merged = offset + imm;
                                        if is_imm12(merged) {
                                            emit!(ADDI dst, rs1, merged);
                                        } else if is_imm12(offset) {
                                            emit!(ADDI dst, rd, offset);
                                        } else {
                                            let tmp = self.new_vreg64();
                                            emit!(LIMM tmp, offset);
                                            emit!(ADD dst, rd, tmp);
                                        }
                                    } else {
                                        if is_imm12(offset) {
                                            emit!(ADDI dst, ptr, offset);
                                        } else {
                                            let tmp = self.new_vreg64();
                                            emit!(LIMM tmp, offset);
                                            emit!(ADD dst, ptr, tmp);
                                        }
                                    }
                                } else {
                                    if is_imm12(offset) {
                                        emit!(ADDI dst, ptr, offset);
                                    } else {
                                        let tmp = self.new_vreg64();
                                        emit!(LIMM tmp, offset);
                                        emit!(ADD dst, ptr, tmp);
                                    }
                                }
                            } else {
                                let idx = self.resolve(index, mbb);
                                let tmp = self.new_vreg64();
                                if elem_size.count_ones() == 1 {
                                    if  elem_size.trailing_zeros() > 0 {
                                        emit!(SLLI tmp, idx, elem_size.trailing_zeros() as i32);
                                    }
                                    emit!(ADD dst, ptr, tmp);
                                } else {
                                    emit!(LIMM tmp, elem_size);
                                    emit!(MUL tmp, idx, tmp);
                                    emit!(ADD dst, ptr, tmp);
                                }
                            }
                            ptr = dst;
                        }
                    },
                    InstructionValue::Call(c) => {
                        enum ReturnValue {
                            GPR(GPOperand),
                            F32(FPOperand),
                            Void,
                        }
                        let dst = if c.ty.is_void() {
                            ReturnValue::Void
                        } else if c.ty.is_int() {
                            ReturnValue::GPR(self.resolve(inst, mbb))
                        } else {
                            assert!(c.ty.is_float());
                            ReturnValue::F32(self.resolve_fp(inst, mbb))
                        };
                        self.used_regs.insert(RVGPR::ra());
                        // record parameter type
                        let mut args = Vec::new();

                        for (idx, arg) in c.args.iter().take(8).enumerate() {
                            let value = self.unit.values[*arg].clone();
                            match value.ty().kind {
                                TypeKind::I32 | TypeKind::Ptr(_) => {
                                    let target = pre!(a idx);
                                    if let Some(imm) = self.resolve_constant(*arg) {
                                        emit!(LIMM target, imm);
                                        continue;
                                    }
                                    let reg = self.resolve(*arg, mbb);
                                    emit!(MV target, reg);
                                    args.push(false);
                                },
                                TypeKind::F32 => {
                                    let target = pre!(fa idx);
                                    let reg = self.resolve_fp(*arg, mbb);
                                    emit!(FMVSS target, reg);
                                    args.push(true);
                                }
                                _ => unimplemented!("call arg type: {:?}", value.ty()),
                            };
                            
                        }
                        if c.args.len() > 8 {
                            let extra = c.args.len() - 8;
                            let stack_size = extra * 8;
                            let stack_size = if stack_size % 16 == 0 {
                                stack_size
                            } else {
                                stack_size + 8
                            };
                            // already so many arguments!
                            assert!(stack_size <= 2032);
                            
                            for i in 0..extra {
                                let arg = c.args[8 + i];
                                let value = self.unit.values[arg].clone();
                                match value.ty().kind {
                                    TypeKind::I32 | TypeKind::Ptr(_) => {
                                        let reg = self.resolve_ensure_reg(arg, mbb);
                                        emit!(SD reg, pre!(sp), (i * 8) as i32 - stack_size as i32);
                                        args.push(false);
                                    }
                                    TypeKind::F32 => {
                                        let reg = self.resolve_fp(arg, mbb);
                                        emit!(FSW reg, pre!(sp), (i * 8) as i32 - stack_size as i32);
                                        args.push(true);
                                    }
                                    _ => unimplemented!("call arg type: {:?}", value.ty()),
                                }
                            }

                            let inst = emit!(ADDI pre!(sp), pre!(sp), -(stack_size as i32));
                            self.prog.mark_inline(inst, false);
                        }

                        emit!(CALL c.func.clone(), args.clone());

                        if c.args.len() > 8 {
                            let extra = c.args.len() - 8;
                            let stack_size = extra * 8;
                            let stack_size = if stack_size % 16 == 0 {
                                stack_size
                            } else {
                                stack_size + 8
                            };
                            assert!(stack_size <= 2032);
                            let inst = emit!(ADDI pre!(sp), pre!(sp), stack_size as i32);
                            self.prog.mark_inline(inst, false);
                        }
                        match dst {
                            ReturnValue::GPR(gp) => {
                                emit!(MV gp, pre!(a0));
                            },
                            ReturnValue::F32(fp) => {
                                emit!(FMVSS fp, pre!(fa0));
                            },
                            ReturnValue::Void => {},
                        }
                    },
                }
            }
        }

        // 3. handle phi nodes
        for bb in &irfunc.bbs {
            let mbb = self.bb_map[bb];
            let block = &self.unit.blocks[*bb];

            // let mut incoming = Vec::new();
            let mut outgoing = HashMap::new();
            let mut outgoing_imm = HashMap::new();

            let mut iter = block.insts_start;
            while let Some(inst) = iter {
                let insn = &self.unit.values[inst].clone();
                iter = insn.next;

                if let InstructionValue::Phi(phi) = &insn.value.as_inst() {
                    // for example: %dst = phi [%a, %b], [%c, %d]
                    // incoming value are added to the beginning of this bb:
                    // this:
                    //     mv dst, vreg
                    //     ...
                    // outgoing value are added to the end of each predecessor (before final branches):
                    // b:  ...
                    //     mv vreg, a
                    //     j ...
                    // d:  ...
                    //     mv vreg, c
                    //     blt ...

                    // let vreg = self.new_vreg();
                    // incoming.push((self.resolve(inst, mbb), vreg));
                    let vreg = self.resolve(inst, mbb);
                    for (val, bb) in &phi.args {
                        let value = self.unit.values[*val].clone();
                        if value.value.is_constant() {
                            outgoing_imm
                                .entry(*bb)
                                .or_insert(Vec::new())
                                .push((vreg, value.value.as_constant().clone()));
                        } else {
                            outgoing
                                .entry(*bb)
                                .or_insert(Vec::new())
                                .push((vreg, self.resolve(*val, mbb)));
                        }
                    }
                } else {
                    // end of phi nodes
                    break;
                }
            }

            // for (lhs, rhs) in incoming {
            //     self.prog.push_to_begin(mbb, RV64InstBuilder::MV(lhs, rhs));
            // }
            for (pred, insts) in outgoing {
                let mbb = self.bb_map[&pred];
                if let Some(last) = self.prog.blocks[mbb].insts_tail {
                    for (lhs, rhs) in insts {
                        self.prog.insert_before(last, RV64InstBuilder::MV(lhs, rhs));
                    }
                } else {
                    for (lhs, rhs) in insts {
                        self.prog.push_to_end(mbb, RV64InstBuilder::MV(lhs, rhs));
                    }
                }
            }
            for (pred, insts) in outgoing_imm {
                let mbb = self.bb_map[&pred];
                let mut loads = vec![];
                for (lhs, rhs) in insts {
                    match rhs {
                        ConstantValue::I32(imm) => loads.push(RV64InstBuilder::LIMM(lhs, imm)),
                        ConstantValue::I1(imm) => loads.push(RV64InstBuilder::LIMM(lhs, imm as i32)),
                        ConstantValue::F32(imm) => {
                            let tmp = self.new_vreg64();
                            loads.push(RV64InstBuilder::LIMM(tmp, imm.to_bits() as i32));
                            // FMV.S.X lhs, tmp
                        }
                        ConstantValue::Undef => continue,
                    }
                }
                if let Some(last) = self.prog.blocks[mbb].insts_tail {
                    todo!("phi node with imm");
                } else {
                    todo!("phi node with imm");
                }
            }
        }

        // remove useless instructions (already inlined)
        for bb in &irfunc.bbs {
            let mbb = self.bb_map[bb];
            let mut iter = self.prog.blocks[mbb].insts_head;
            while let Some(inst) = iter {
                let insn = self.prog.insts[inst].clone();
                iter = insn.next;

                if insn.inlined {
                    self.prog.remove(inst);
                }
            }
        }

        // 4. add real prolouge
        self.prog.push_to_begin(
            mfunc.entry.unwrap(),
            // placeholder for prolouge
            RV64Instruction::ENTER,
        );

        mfunc
    }

    fn new_vreg(&mut self) -> GPOperand {
        let old = self.virtual_max;
        self.virtual_max += 1;
        let v = VirtGPR::new(old, VirtGPRType::Int32);
        self.virtual_gprs.insert(v);
        GPOperand::Virtual(v)
    }

    fn new_vreg64(&mut self) -> GPOperand {
        let old = self.virtual_max;
        self.virtual_max += 1;
        let v = VirtGPR::new(old, VirtGPRType::Int64);
        self.virtual_gprs.insert(v);
        GPOperand::Virtual(v)
    }

    fn new_vregf32(&mut self) -> FPOperand {
        let old = self.virtual_max;
        self.virtual_max += 1;
        let v = VirtFPR::new(old, VirtFPRType::Fp32);
        self.virtual_fprs.insert(v);
        FPOperand::Virtual(v)
    }

    fn type_to_reg(&mut self, ty: Rc<Type>) -> GPOperand {
        match ty.kind {
            TypeKind::I1 => self.new_vreg(),
            TypeKind::I32 => self.new_vreg(),
            TypeKind::I64 => self.new_vreg64(),
            TypeKind::Ptr(_) => self.new_vreg64(),
            _ => unimplemented!("unknown type width: {:?}", ty.kind)
        }
    }

    fn type_to_regf(&mut self, ty: Rc<Type>) -> FPOperand {
        match ty.kind {
            TypeKind::F32 => self.new_vregf32(),
            _ => unimplemented!("unknown type width: {:?}", ty.kind)
        }
    }

    fn resolve_fp(&mut self, val: Id<Value>, _mbb: Id<MachineBB>) -> FPOperand {
        let value = self.unit.values[val].clone();
        match value.value {
            ValueType::Constant(_c) => {
                match _c {
                    ConstantValue::F32(num) => {
                        let dst = self.new_vregf32();
                        if num != 0.0 {
                            let reg = self.new_vreg();
                            // just 32 bits, so this *should* make sense?
                            self.prog.push_to_end(_mbb, RV64InstBuilder::LIMM(reg, num.to_bits() as i32));
                            self.prog.push_to_end(_mbb, RV64InstBuilder::FMVWX(dst, reg));
                        } else {
                            self.prog.push_to_end(_mbb, RV64InstBuilder::FMVWX(dst, pre!(zero)));
                        }
                        dst
                    }
                    _ => unimplemented!("unknown constant type: {:?}", _c),
                }
            }
            ValueType::Instruction(ref _inst) => {
                if self.val_mapf.contains_key(&val) {
                    self.val_mapf[&val]
                } else {
                    let res = self.type_to_regf(value.ty());
                    self.val_mapf.insert(val, res);
                    res
                }
            },
            ValueType::Parameter(ref _param) => {
                if self.val_mapf.contains_key(&val) {
                    self.val_mapf[&val]
                } else {
                    let res = self.type_to_regf(value.ty());

                    let params = self.unit.funcs[self.name.as_str()].params.clone();
                    let idx = params.iter().position(|&x| x == val).unwrap();
                    let entry = self.bb_map[&self.unit.funcs[self.name.as_str()].entry_bb];
                    if idx < 8 {
                        // first 8 params are passed in registers
                        self.used_regs.insert(RVGPR::a(idx));
                        // self.prog.push_to_begin(entry, RV64InstBuilder::MV(res, MachineOperand::PreColored(RVGPR::a(idx))));
                        // NO EXTRA MOVE! A registers cannot be written before used,
                        // and the extra move causes erroneous register allocation
                        return FPOperand::PreColored(RVFPR::fa(idx));
                    } else {
                        // extra args are passed on stack, 8 bytes aligned
                        // for i-th arg, it is at [fp + 8 * (i - 8)]
                        self.used_regs.insert(RVGPR::fp());
                        let offset = 8 * (idx - 8) as i32;
                        if is_imm12(offset) {
                            self.prog.push_to_begin(
                                entry, 
                                RV64InstBuilder::FLD(res, GPOperand::PreColored(RVGPR::fp()),
                                offset
                            ));
                        } else {
                            panic!("offset too large: {}", offset)
                        }

                        // todo: the "omit frame pointer" version
                        // save a worklist of loads needed to be fixed with actual offset (with stack size added)
                    }

                    self.val_mapf.insert(val, res);
                    res
                }
            },
            ValueType::Global(ref _g) => {
                panic!("?")
            },
        }
    }

    fn resolve_ensure_reg(&mut self, val: Id<Value>, _mbb: Id<MachineBB>) -> GPOperand {
        let value = self.unit.values[val].clone();
        match value.value {
            ValueType::Constant(_c) => {
                match _c {
                    ConstantValue::I1(i) => {
                        if i {
                            let reg = self.new_vreg();
                            self.prog.push_to_end(_mbb, RV64InstBuilder::LIMM(reg, i as i32));
                            reg
                        } else {
                            pre!(zero)
                        }
                    }
                    ConstantValue::I32(i) => {
                        if i != 0 {
                            let reg = self.new_vreg();
                            self.prog.push_to_end(_mbb, RV64InstBuilder::LIMM(reg, i));
                            reg
                        } else {
                            pre!(zero)
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            _ => self.resolve(val, _mbb),
        }
    }

    fn resolve(&mut self, val: Id<Value>, _mbb: Id<MachineBB>) -> GPOperand {
        let value = self.unit.values[val].clone();
        match value.value {
            ValueType::Constant(_c) => {
                // should check if we can directly encode the immediate to the instruction!
                panic!("")
            },
            ValueType::Instruction(ref _inst) => {
                if self.val_map.contains_key(&val) {
                    self.val_map[&val]
                } else {
                    let res = self.type_to_reg(value.ty());
                    self.val_map.insert(val, res);
                    res
                }
            },
            ValueType::Parameter(ref _param) => {
                if self.val_map.contains_key(&val) {
                    self.val_map[&val]
                } else {
                    let res = self.type_to_reg(value.ty());

                    let params = self.unit.funcs[self.name.as_str()].params.clone();
                    let idx = params.iter().position(|&x| x == val).unwrap();
                    let entry = self.bb_map[&self.unit.funcs[self.name.as_str()].entry_bb];
                    if idx < 8 {
                        // first 8 params are passed in registers
                        self.used_regs.insert(RVGPR::a(idx));
                        // self.prog.push_to_begin(entry, RV64InstBuilder::MV(res, MachineOperand::PreColored(RVGPR::a(idx))));
                        // NO EXTRA MOVE! A registers cannot be written before used,
                        // and the extra move causes erroneous register allocation
                        return GPOperand::PreColored(RVGPR::a(idx));
                    } else {
                        // extra args are passed on stack, 8 bytes aligned
                        // for i-th arg, it is at [fp + 8 * (i - 8)]
                        self.used_regs.insert(RVGPR::fp());
                        let offset = 8 * (idx - 8) as i32;
                        if is_imm12(offset) {
                            self.prog.push_to_begin(entry, RV64InstBuilder::LD(res, GPOperand::PreColored(RVGPR::fp()), offset));
                        } else {
                            panic!("offset too large: {}", offset)
                            // // try to use itself as scratch register; should it cause any problem?
                            // let (load_high, lo12) = AsmFuncBuilder::load_imm32_hi20(res, offset);
                            
                            // if let Some(minst) = load_high {
                            //     // pushed in reverse order
                            //     self.prog.push_to_begin(entry, RV64InstBuilder::LD(res, res, lo12));
                            //     self.prog.push_to_begin(entry, RV64InstBuilder::ADD(res, res, GPOperand::PreColored(RVGPR::fp())));
                            //     self.prog.push_to_begin(entry, minst);
                            // } else {
                            //     // hi20 cannot be zero, or we can use LD directly
                            //     unreachable!()
                            // }
                        }

                        // todo: the "omit frame pointer" version
                        // save a worklist of loads needed to be fixed with actual offset (with stack size added)
                    }

                    self.val_map.insert(val, res);
                    res
                }
            },
            ValueType::Global(ref g) => {
                if self.val_map.contains_key(&val) {
                    self.val_map[&val]
                } else {
                    let marker = self.entry_marker.unwrap();
                    let res = self.type_to_reg(value.ty());
                    self.prog.insert_after(marker, RV64InstBuilder::LADDR(res, g.name.clone()));

                    self.val_map.insert(val, res);
                    res
                }
            },
        }
    }

    /// test if it is a integer constant
    fn resolve_constant(&mut self, val: Id<Value>) -> Option<i32> {
        let value = self.unit.values[val].clone();
        match value.value {
            ValueType::Constant(c) => {
                match c {
                    ConstantValue::I32(i) => Some(i as i32),
                    _ => None,
                }
            },
            _ => None,
        }
    }

    // fn resolve_constantf(&mut self, val: Id<Value>) -> Option<f32> {
    //     let value = self.unit.values[val].clone();
    //     match value.value {
    //         ValueType::Constant(c) => {
    //             match c {
    //                 ConstantValue::F32(num) => Some(num),
    //                 _ => None,
    //             }
    //         },
    //         _ => None,
    //     }
    // }

    fn is_integral(&self, val: Id<Value>) -> bool {
        let value = self.unit.values[val].clone();
        value.ty().is_int()
    }

    fn is_floating(&self, val: Id<Value>) -> bool {
        let value = self.unit.values[val].clone();
        value.ty().is_float()
    }

    // fn load_imm32(reg: MachineOperand, imm: i32) -> Vec<RV64Instruction> {
    //     let mut ret = Vec::with_capacity(2);
    //     let hi20 = (imm + 0x800) >> 12 & 0xfffff;
    //     let lo12 = imm & 0xfff;

    //     let mut src = MachineOperand::PreColored(RVReg::zero());
    //     if hi20 != 0 {
    //         ret.push(RV64InstBuilder::LUI(reg, hi20));
    //         src = reg;
    //     }
    //     if lo12 != 0 || hi20 == 0 {
    //         ret.push(RV64InstBuilder::ADDIW(reg, src, lo12));
    //     }

    //     ret
    // }

    fn load_imm32_hi20(reg: GPOperand, imm: i32) -> (Option<RV64Instruction>, i32) {
        let hi20 = (imm + 0x800) >> 12 & 0xfffff;
        let lo12 = imm & 0xfff;

        if hi20 != 0 {
            (Some(RV64InstBuilder::LUI(reg, hi20)), lo12)
        } else {
            (None, lo12)
        }
    }

    fn mark_usage<R>(&mut self, reg: R)
    where
        R: PseudoMachineOperand
    {
        if let Some(reg) = reg.as_machopr() {
            if let Some(reg_id) = self.prog.vreg_def.get(&reg) {
                self.prog.mark_inline(*reg_id, false);
            }
        }
    }
}

trait PseudoMachineOperand {
    fn as_machopr(&self) -> Option<GPOperand>;
}

impl PseudoMachineOperand for GPOperand {
    fn as_machopr(&self) -> Option<GPOperand> {
        Some(self.clone())
    }
}

macro_rules! impl_blank_mo {
    ($($t:ty),+ $(,)?) => {
        $(
            impl PseudoMachineOperand for $t {
                fn as_machopr(&self) -> Option<GPOperand> {
                    None
                }
            }
        )*
    };
}

impl_blank_mo! {
    i32,
    Id<MachineBB>,
    String,
    usize,
    FPOperand,
    Vec<bool>,
}

#[inline(always)]
pub fn is_imm12(imm: i32) -> bool {
    imm >= -2048 && imm < 2048
}

#[inline(always)]
pub fn split_imm32(imm: i32) -> (i32, i32) {
    let hi20 = (imm + 0x800) >> 12 & 0xfffff;
    let hi20 = hi20 << 12 >> 12;
    let lo12 = imm & 0x7ff;
    let lo12 = lo12 << 20 >> 20;
    (hi20, lo12)
}

#[inline(always)]
fn is_i_type(op: BinaryOpType) -> bool {
    match op {
        BinaryOpType::Add | 
        BinaryOpType::Sub |
        BinaryOpType::Xor |
        BinaryOpType::Ne |
        BinaryOpType::Eq |
        BinaryOpType::Lt |
        BinaryOpType::Le |
        BinaryOpType::Gt |
        BinaryOpType::Ge => true,
        _ => false,
    }
}
