use std::collections::HashMap;

use crate::{
    ir::{builder::TransUnit, structure::BasicBlock, value::{Value, ValueType, InstructionValue, ValueTrait, ConstantValue}},
    alloc::Id, asm::{RV64InstBuilder, RVReg}, ctype::{TypePtrHelper, Type, TypeKind}
};

use super::{MachineProgram, MachineFunc, MachineBB, MachineOperand, RV64Instruction};

impl TransUnit {
    pub fn emit_asm(&self) -> MachineProgram {
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
    pub unit: &'a TransUnit,

    pub bb_map: HashMap<Id<BasicBlock>, Id<MachineBB>>,

    // mappings from ir entity to asm entity
    pub val_map: HashMap<Id<Value>, MachineOperand>,
    // todo: global decl

    pub virtual_max: u32,
}

impl<'a> AsmFuncBuilder<'a> {
    pub fn new(name: &str, prog: &'a mut MachineProgram, unit: &'a TransUnit) -> Self {
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

        // 1. create machine bb as per ir bb
        let irfunc = self.unit.funcs.get(self.name.as_str()).unwrap();
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
            let mut iter = block.insts_start;
            while let Some(inst) = iter {
                let insn = &self.unit.values[inst];

                let iv = insn.value.as_inst().clone();
                match iv {
                    InstructionValue::Binary(_) => todo!(),
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
                                self.prog.push_to_end(mbb, instruction(dst, rs1, imm));
                            } else {
                                self.prog.mark_inline(*ptr_id, false);
                                self.prog.push_to_end(mbb, instruction(dst, ptr, 0));
                            }
                        } else {
                            self.prog.push_to_end(mbb, instruction(dst, ptr, 0));
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
                        let src = self.resolve(s.value, mbb);
                        if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                            let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                            if let RV64Instruction::ADDI { rs1, imm, .. } = ptr_inst {
                                self.prog.push_to_end(mbb, instruction(src, rs1, imm));
                            } else {
                                self.prog.mark_inline(*ptr_id, false);
                                self.prog.push_to_end(mbb, instruction(src, ptr, 0));
                            }
                        } else {
                            self.prog.push_to_end(mbb, instruction(src, ptr, 0));
                        }
                    },
                    InstructionValue::Alloca(_) => todo!(),
                    InstructionValue::Return(r) => {
                        if let Some(val) = r.value {
                            let value = self.unit.values[val].clone();
                            match value.ty().get().kind {
                                TypeKind::I32 => {
                                    let src = self.resolve(val, mbb);
                                    self.prog.push_to_end(mbb, RV64InstBuilder::ADDI(
                                        MachineOperand::PreColored(RVReg::a(0)),
                                        src,
                                        0
                                    ));
                                },
                                _ => unimplemented!(),
                            }
                        }
                        self.prog.push_to_end(mbb, RV64InstBuilder::RET());
                    },
                    InstructionValue::Branch(_) => todo!(),
                    InstructionValue::Jump(j) => {
                        self.prog.push_to_end(mbb, RV64InstBuilder::JUMP(self.bb_map[&j.succ]));
                    },
                    InstructionValue::Zext(_) => (),
                    InstructionValue::Phi(_) => todo!(),
                    InstructionValue::GetElemPtr(g) => {
                        let dst = self.resolve(inst, mbb);
                        let ptr = self.resolve(g.ptr, mbb);
                        let elem_size = g.ty.get().base_type().get().size() as i32;
                        if let Some(idx) = self.resolve_constant(g.index) {
                            let offset = idx * elem_size;
                            if let Some(ptr_id) = self.prog.vreg_def.get(&ptr) {
                                let ptr_inst = self.prog.insts[*ptr_id].inst.clone();
                                if let RV64Instruction::ADDI { rd, rs1, imm } = ptr_inst {
                                    let merged = offset + imm;
                                    if is_imm12(merged) {
                                        self.prog.push_to_end(mbb, RV64InstBuilder::ADDI(dst, rs1, merged));
                                    } else {
                                        self.prog.mark_inline(*ptr_id, false);
                                        self.prog.push_to_end(mbb, RV64InstBuilder::ADDI(dst, rd, offset));
                                    }
                                } else {
                                    self.prog.mark_inline(*ptr_id, false);
                                    if is_imm12(offset) {
                                        self.prog.push_to_end(mbb, RV64InstBuilder::ADDI(dst, ptr, offset));
                                    } else {
                                        self.prog.push_to_end(mbb, RV64InstBuilder::LIMM(dst, offset));
                                        self.prog.push_to_end(mbb, RV64InstBuilder::ADD(dst, ptr, dst));
                                    }
                                }
                            } else {
                                if is_imm12(offset) {
                                    self.prog.push_to_end(mbb, RV64InstBuilder::ADDI(dst, ptr, offset));
                                } else {
                                    self.prog.push_to_end(mbb, RV64InstBuilder::LIMM(dst, offset));
                                    self.prog.push_to_end(mbb, RV64InstBuilder::ADD(dst, ptr, dst));
                                }
                            }
                        } else {
                            let idx = self.resolve(g.index, mbb);
                            self.prog.push_to_end(mbb, RV64InstBuilder::LIMM(dst, elem_size));
                            self.prog.push_to_end(mbb, RV64InstBuilder::MUL(dst, idx, dst));
                            self.prog.push_to_end(mbb, RV64InstBuilder::ADD(dst, ptr, dst));
                        }
                    },
                }

                iter = insn.next;
            }
        }

        mfunc
    }

    fn new_vreg(&mut self) -> MachineOperand {
        let old = self.virtual_max;
        self.virtual_max += 1;
        MachineOperand::Virtual(old)
    }

    fn resolve(&mut self, val: Id<Value>, mbb: Id<MachineBB>) -> MachineOperand {
        let value = self.unit.values[val].clone();
        match value.value {
            ValueType::Constant(_c) => {
                // should check if we can directly encode the immediate to the instruction!
                unreachable!("")
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
                    let entry = self.prog.funcs[self.prog.block_map[&mbb]].entry.unwrap();
                    if idx < 8 {
                        // first 8 params are passed in registers
                        self.prog.push_to_begin(entry, RV64InstBuilder::ADDI(
                            res,
                            MachineOperand::PreColored(RVReg::a(idx)),
                            0
                        ));
                    } else {
                        // extra args are passed on stack, 8 bytes aligned
                        // for i-th arg, it is at [fp + 8 * (i - 8)]
                        let offset = 8 * (idx - 8) as i32;
                        if is_imm12(offset) {
                            self.prog.push_to_begin(entry, RV64InstBuilder::LD(
                                res,
                                MachineOperand::PreColored(RVReg::fp()),
                                offset
                            ));
                        } else {
                            // try to use itself as scratch register; should it cause any problem?
                            let (load_high, lo12) = AsmFuncBuilder::load_imm32_hi20(res, 8 * (idx - 8) as i32);
                            
                            if let Some(minst) = load_high {
                                // pushed in reverse order
                                self.prog.push_to_begin(entry, RV64InstBuilder::LD(
                                    res,
                                    res,
                                    lo12
                                ));
                                self.prog.push_to_begin(entry, RV64InstBuilder::ADD(
                                    res,
                                    res,
                                    MachineOperand::PreColored(RVReg::fp())
                                ));
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
            ValueType::Global(g) => todo!(),
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

    fn load_imm32(reg: MachineOperand, imm: i32) -> Vec<RV64Instruction> {
        let mut ret = Vec::with_capacity(2);
        let hi20 = (imm + 0x800) >> 12 & 0xfffff;
        let lo12 = imm & 0xfff;

        let mut src = MachineOperand::PreColored(RVReg::zero());
        if hi20 != 0 {
            ret.push(RV64InstBuilder::LUI(reg, hi20));
            src = reg;
        }
        if lo12 != 0 || hi20 == 0 {
            ret.push(RV64InstBuilder::ADDIW(reg, src, lo12));
        }

        ret
    }

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