use std::collections::HashMap;

use crate::{
    ir::{structure::{BasicBlock, TransUnit},
    value::{Value, ValueType, InstructionValue, ValueTrait, ConstantValue, calculate_used_by}},
    alloc::Id, asm::{RV64InstBuilder, RVReg}, ctype::{TypePtrHelper, TypeKind, BinaryOpType},
};

use super::{MachineProgram, MachineFunc, MachineBB, MachineOperand, RV64Instruction};

impl TransUnit {
    pub fn emit_asm(&mut self) -> MachineProgram {
        let mut program = MachineProgram::new();

        for func in self.funcs() {
            let mfunc = AsmFuncBuilder::new(&func, &mut program, self).build();
            program.funcs.push(mfunc);
        }

        program
    }
}

struct AsmFuncBuilder<'a> {
    pub name: String,
    pub prog: &'a mut MachineProgram,
    pub unit: &'a mut TransUnit,

    pub bb_map: HashMap<Id<BasicBlock>, Id<MachineBB>>,

    // mappings from ir entity to asm entity
    pub val_map: HashMap<Id<Value>, MachineOperand>,
    // todo: global decl

    pub virtual_max: u32,
}

macro_rules! pre {
    (zero) => { MachineOperand::PreColored(RVReg::zero()) };
    (a0) => { MachineOperand::PreColored(RVReg::a(0)) };
    (fp) => { MachineOperand::PreColored(RVReg::fp()) };
    (sp) => { MachineOperand::PreColored(RVReg::sp()) };
}

impl<'a> AsmFuncBuilder<'a> {
    pub fn new(name: &str, prog: &'a mut MachineProgram, unit: &'a mut TransUnit) -> Self {
        Self {
            name: name.to_string(),
            prog,
            unit,
            bb_map: HashMap::new(),
            val_map: HashMap::new(),
            virtual_max: 0,
        }
    }

    pub fn build(&mut self) -> MachineFunc {
        let mut mfunc = self.prog.new_func(self.name.clone());
        calculate_used_by(self.unit, self.name.as_str());

        // 1. create machine bb as per ir bb
        let irfunc = self.unit.funcs[self.name.as_str()].clone();
        for bb in &irfunc.bbs {
            let mbb = self.prog.blocks.alloc(MachineBB::new(*bb));
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

        // 2. translate instructions except phi
        for bb in &irfunc.bbs {
            let mbb = self.bb_map[bb];
            let block = &self.unit.blocks[*bb];
            macro_rules! emit {
                ($mnemonic:ident $($operand:expr),*) => {
                    self.prog.push_to_end(mbb, RV64InstBuilder::$mnemonic($($operand),*))
                };
                ($mnemonic:expr ; $($operand:expr),*) => {
                    self.prog.push_to_end(mbb, $mnemonic($($operand),*))
                };
            }

            let mut iter = block.insts_start;
            while let Some(inst) = iter {
                let insn = &self.unit.values[inst].clone();

                let iv = insn.value.as_inst().clone();
                match iv {
                    InstructionValue::Binary(b) => {
                        let dst = self.resolve(inst, mbb);

                        let val_lhs = self.unit.values[b.lhs].value.clone();
                        let mo_lhs = if val_lhs.is_constant() {
                            let val = val_lhs.as_constant().clone();
                            let reg = self.new_vreg();
                            match val {
                                ConstantValue::I1(i) => {
                                    emit!(LIMM reg, i as i32);
                                }
                                ConstantValue::I32(i) => {
                                    emit!(LIMM reg, i);
                                }
                                // _ => unimplemented!(),
                            }
                            reg
                        } else {
                            self.resolve(b.lhs, mbb)
                        };
                        if let Some(minst_id) = self.prog.vreg_def.get(&mo_lhs) {
                            self.prog.mark_inline(*minst_id, false);
                        }

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
                                                    let tmp = self.new_vreg();
                                                    emit!(LIMM tmp, i);
                                                    emit!(SNE dst, mo_lhs, tmp);
                                                } else {
                                                    emit!(XORI dst, mo_lhs, i);
                                                    emit!(SLTU dst, pre!(zero), dst);
                                                }
                                            }
                                            BinaryOpType::Eq => {
                                                if eliminate_cmp {
                                                    let tmp = self.new_vreg();
                                                    emit!(LIMM tmp, i);
                                                    emit!(SEQ dst, mo_lhs, tmp);
                                                } else {
                                                    emit!(XORI dst, mo_lhs, i);
                                                    emit!(SLTIU dst, dst, 1);
                                                }
                                            }
                                            BinaryOpType::Lt => {
                                                if eliminate_cmp {
                                                    let tmp = self.new_vreg();
                                                    emit!(LIMM tmp, i);
                                                    emit!(SLT dst, mo_lhs, tmp);
                                                } else {
                                                    emit!(SLTI dst, mo_lhs, i);
                                                }
                                            }
                                            BinaryOpType::Le => {
                                                let tmp = self.new_vreg();
                                                emit!(LIMM tmp, i);
                                                emit!(SGE dst, tmp, mo_lhs);
                                            }
                                            BinaryOpType::Gt => {
                                                let tmp = self.new_vreg();
                                                emit!(LIMM tmp, i);
                                                emit!(SLT dst, tmp, mo_lhs);
                                            }
                                            BinaryOpType::Ge => {
                                                if eliminate_cmp {
                                                    let tmp = self.new_vreg();
                                                    emit!(LIMM tmp, i);
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
                                        let tmp = self.new_vreg();
                                        emit!(LIMM tmp, i);
                                        match b.op {
                                            BinaryOpType::Add | BinaryOpType::Sub => {
                                                emit!(ADDW dst, mo_lhs, tmp);
                                            }
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
                                _ => unimplemented!(),
                            }
                        } else {
                            let mo_rhs = if val_rhs.value.is_constant() {
                                match *val_rhs.value.as_constant() {
                                    ConstantValue::I32(i) => {
                                        let reg = self.new_vreg();
                                        emit!(LIMM reg, i);
                                        reg
                                    }
                                    _ => unimplemented!(),
                                }
                            } else {
                                self.resolve(b.rhs, mbb)
                            };
                            if let Some(minst_id) = self.prog.vreg_def.get(&mo_rhs) {
                                self.prog.mark_inline(*minst_id, false);
                            }
                            match b.ty().get().kind {
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
                                        _ => unimplemented!(),
                                    }
                                }
                                _ => unimplemented!(),
                            }
                        }
                    },
                    InstructionValue::Load(l) => {
                        let instruction = match l.ty.get().kind {
                            TypeKind::I32 => RV64InstBuilder::LW,
                            TypeKind::I64 => RV64InstBuilder::LD,
                            _ => unimplemented!(),
                        };

                        let ptr = self.resolve(l.ptr, mbb);
                        let dst = self.resolve(inst, mbb);
                        if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                            let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                            if let RV64Instruction::ADDI { rs1, imm, .. } = ptr_inst {
                                emit!(instruction; dst, rs1, imm);
                            } else {
                                self.prog.mark_inline(*ptr_id, false);
                                emit!(instruction; dst, ptr, 0);
                            }
                        } else {
                            emit!(instruction; dst, ptr, 0);
                        }
                    },
                    InstructionValue::Store(s) => {
                        let val = self.unit.values[s.value].clone();
                        let instruction = match val.ty().get().kind {
                            TypeKind::I32 => RV64InstBuilder::SW,
                            TypeKind::I64 => RV64InstBuilder::SD,
                            _ => unimplemented!(),
                        };

                        let ptr = self.resolve(s.ptr, mbb);
                        let value = self.unit.values[s.value].clone();
                        let src = if value.value.is_constant() {
                            let tmp = self.new_vreg();
                            match *value.value.as_constant() {
                                ConstantValue::I32(i) => emit!(LIMM tmp, i),
                                _ => unimplemented!(),
                            }
                            tmp
                        } else {
                            self.resolve(s.value, mbb)
                        };
                        if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                            let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                            if let RV64Instruction::ADDI { rs1, imm, .. } = ptr_inst {
                                emit!(instruction; src, rs1, imm);
                            } else {
                                self.prog.mark_inline(*ptr_id, false);
                                emit!(instruction; src, ptr, 0);
                            }
                        } else {
                            emit!(instruction; src, ptr, 0);
                        }
                    },
                    InstructionValue::Alloca(a) => {
                        let dst = self.resolve(inst, mbb);
                        let object_size = a.ty.get().size() as u32;
                        let object_begin = mfunc.stack_size;
                        mfunc.stack_size += object_size;
                        if is_imm12(object_begin as i32) {
                            emit!(ADDI dst, pre!(sp), object_begin as i32);
                        } else {
                            emit!(LIMM dst, object_begin as i32);
                            emit!(ADD dst, pre!(sp), dst);
                        }
                    },
                    InstructionValue::Return(r) => {
                        if let Some(val) = r.value {
                            let value = self.unit.values[val].clone();
                            match value.ty().get().kind {
                                TypeKind::I32 => {
                                    if let Some(imm) = self.resolve_constant(val) {
                                        emit!(LIMM pre!(a0), imm);
                                    } else {
                                        let src = self.resolve(val, mbb);
                                        // avoid using addi directly here, because it shouldn't be inlined
                                        emit!(MV pre!(a0), src);
                                    }
                                },
                                _ => unimplemented!(),
                            }
                        }
                        emit!(RET);
                    },
                    InstructionValue::Branch(b) => {
                        let cond_reg = self.resolve(b.cond, mbb);
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
                                    self.prog.mark_inline(*cond_id, false);
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
                    InstructionValue::Zext(_) => (),
                    InstructionValue::Phi(_) => (),
                    InstructionValue::GetElemPtr(g) => {
                        let dst = self.resolve(inst, mbb);
                        let ptr = self.resolve(g.ptr, mbb);
                        let elem_size = g.ty.get().size() as i32;
                        if let Some(idx) = self.resolve_constant(g.index) {
                            let offset = idx * elem_size;
                            if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                                let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                                if let RV64Instruction::ADDI { rd, rs1, imm } = ptr_inst {
                                    let merged = offset + imm;
                                    if is_imm12(merged) {
                                        emit!(ADDI dst, rs1, merged);
                                    } else {
                                        self.prog.mark_inline(*ptr_id, false);
                                        emit!(ADDI dst, rd, offset);
                                    }
                                } else {
                                    self.prog.mark_inline(*ptr_id, false);
                                    if is_imm12(offset) {
                                        emit!(ADDI dst, ptr, offset);
                                    } else {
                                        emit!(LIMM dst, offset);
                                        emit!(ADD dst, ptr, dst);
                                    }
                                }
                            } else {
                                if is_imm12(offset) {
                                    emit!(ADDI dst, ptr, offset);
                                } else {
                                    emit!(LIMM dst, offset);
                                    emit!(ADD dst, ptr, dst);
                                }
                            }
                        } else {
                            if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                                self.prog.mark_inline(*ptr_id, false);
                            }
                            let idx = self.resolve(g.index, mbb);
                            if elem_size.count_ones() == 1 {
                                if  elem_size.trailing_zeros() > 0 {
                                    emit!(SLLI dst, idx, elem_size.trailing_zeros() as i32);
                                }
                                emit!(ADD dst, ptr, dst);
                            } else {
                                emit!(LIMM dst, elem_size);
                                emit!(MUL dst, idx, dst);
                                emit!(ADD dst, ptr, dst);
                            }
                        }
                    },
                }

                iter = insn.next;
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
                if let Some(last) = self.prog.blocks[mbb].insts_tail {
                    for (lhs, rhs) in insts {
                        match rhs {
                            ConstantValue::I32(imm) =>
                                self.prog.insert_before(last, RV64InstBuilder::LIMM(lhs, imm)),
                            ConstantValue::I1(imm) =>
                                self.prog.insert_before(last, RV64InstBuilder::LIMM(lhs, imm as i32)),
                        }
                    }
                } else {
                    for (lhs, rhs) in insts {
                        match rhs {
                            ConstantValue::I32(imm) =>
                                self.prog.push_to_end(mbb, RV64InstBuilder::LIMM(lhs, imm)),
                            ConstantValue::I1(imm) =>
                                self.prog.push_to_end(mbb, RV64InstBuilder::LIMM(lhs, imm as i32)),
                        }
                    }
                }
            }
        }

        mfunc.virtual_max = self.virtual_max;
        mfunc
    }

    fn new_vreg(&mut self) -> MachineOperand {
        let old = self.virtual_max;
        self.virtual_max += 1;
        MachineOperand::Virtual(old)
    }

    fn resolve(&mut self, val: Id<Value>, _mbb: Id<MachineBB>) -> MachineOperand {
        let value = self.unit.values[val].clone();
        match value.value {
            ValueType::Constant(_c) => {
                // should check if we can directly encode the immediate to the instruction!
                panic!("")
            },
            ValueType::Instruction(_inst) => {
                if self.val_map.contains_key(&val) {
                    self.val_map[&val]
                } else {
                    let res = self.new_vreg();
                    self.val_map.insert(val, res);
                    res
                }
            },
            ValueType::Parameter(_param) => {
                if self.val_map.contains_key(&val) {
                    self.val_map[&val]
                } else {
                    let res = self.new_vreg();

                    let params = self.unit.funcs[self.name.as_str()].params.clone();
                    let idx = params.iter().position(|&x| x == val).unwrap();
                    let entry = self.bb_map[&self.unit.funcs[self.name.as_str()].entry_bb];
                    if idx < 8 {
                        // first 8 params are passed in registers
                        self.prog.push_to_begin(entry, RV64InstBuilder::MV(res, MachineOperand::PreColored(RVReg::a(idx))));
                    } else {
                        // extra args are passed on stack, 8 bytes aligned
                        // for i-th arg, it is at [fp + 8 * (i - 8)]
                        let offset = 8 * (idx - 8) as i32;
                        if is_imm12(offset) {
                            self.prog.push_to_begin(entry, RV64InstBuilder::LD(res, MachineOperand::PreColored(RVReg::fp()), offset));
                        } else {
                            // try to use itself as scratch register; should it cause any problem?
                            let (load_high, lo12) = AsmFuncBuilder::load_imm32_hi20(res, offset);
                            
                            if let Some(minst) = load_high {
                                // pushed in reverse order
                                self.prog.push_to_begin(entry, RV64InstBuilder::LD(res, res, lo12));
                                self.prog.push_to_begin(entry, RV64InstBuilder::ADD(res, res, MachineOperand::PreColored(RVReg::fp())));
                                self.prog.push_to_begin(entry, minst);
                            } else {
                                // hi20 cannot be zero, or we can use LD directly
                                unreachable!()
                            }
                        }

                        // todo: the "omit frame pointer" version
                    }

                    self.val_map.insert(val, res);
                    res
                }
            },
            ValueType::Global(_g) => todo!(),
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

    fn load_imm32_hi20(reg: MachineOperand, imm: i32) -> (Option<RV64Instruction>, i32) {
        let hi20 = (imm + 0x800) >> 12 & 0xfffff;
        let lo12 = imm & 0xfff;

        if hi20 != 0 {
            (Some(RV64InstBuilder::LUI(reg, hi20)), lo12)
        } else {
            (None, lo12)
        }
    }
}

#[inline(always)]
fn is_imm12(imm: i32) -> bool {
    imm >= -2048 && imm < 2048
}

#[inline(always)]
fn is_i_type(op: BinaryOpType) -> bool {
    match op {
        BinaryOpType::Add | 
        BinaryOpType::Sub |
        BinaryOpType::Ne |
        BinaryOpType::Eq |
        BinaryOpType::Lt |
        BinaryOpType::Le |
        BinaryOpType::Gt |
        BinaryOpType::Ge => true,
        _ => false,
    }
}