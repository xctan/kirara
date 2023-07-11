use std::collections::HashMap;

use crate::{
    ir::{builder::TransUnit, structure::BasicBlock, value::{Value, ValueType, InstructionValue}},
    alloc::Id, asm::{RV64InstBuilder, RVReg}
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
        let mut mfunc = MachineFunc::new(self.name.clone());

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
                    InstructionValue::Load(_) => todo!(),
                    InstructionValue::Store(_) => todo!(),
                    InstructionValue::Alloca(_) => todo!(),
                    InstructionValue::Return(_) => todo!(),
                    InstructionValue::Branch(_) => todo!(),
                    InstructionValue::Jump(_) => todo!(),
                    InstructionValue::Zext(_) => todo!(),
                    InstructionValue::Phi(_) => todo!(),
                    InstructionValue::GetElemPtr(_) => todo!(),
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
        let mo = match value.value {
            ValueType::Constant(c) => {
                // should check if we can directly encode the immediate to the instruction!
                todo!()
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
                        ).unwrap());
                    } else {
                        // extra args are passed on stack, 8 bytes aligned
                        // for i-th arg, it is at [fp + 8 * (i - 8)]
                        if let Ok(minst) = RV64InstBuilder::LD(
                            res,
                            MachineOperand::PreColored(RVReg::fp()),
                            8 * (idx - 8) as i32
                        ) {
                            self.prog.push_to_begin(entry, minst);
                        } else {
                            // try to use itself as scratch register; should it cause any problem?
                            let (load_high, lo12) = AsmFuncBuilder::load_imm32_hi20(res, 8 * (idx - 8) as i32);
                            
                            if let Some(minst) = load_high {
                                // pushed in reverse order
                                self.prog.push_to_begin(entry, RV64InstBuilder::LD(
                                    res,
                                    res,
                                    lo12
                                ).unwrap());
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
        };

        todo!()
    }

    fn load_imm32(reg: MachineOperand, imm: i32) -> Vec<RV64Instruction> {
        let mut ret = Vec::with_capacity(2);
        let hi20 = (imm + 0x800) >> 12 & 0xfffff;
        let lo12 = imm & 0xfff;

        let mut src = MachineOperand::PreColored(RVReg::zero());
        if hi20 != 0 {
            ret.push(RV64InstBuilder::LUI(reg, hi20).unwrap());
            src = reg;
        }
        if lo12 != 0 || hi20 == 0 {
            ret.push(RV64InstBuilder::ADDIW(reg, src, lo12).unwrap());
        }

        ret
    }

    fn load_imm32_hi20(reg: MachineOperand, imm: i32) -> (Option<RV64Instruction>, i32) {
        let hi20 = (imm + 0x800) >> 12 & 0xfffff;
        let lo12 = imm & 0xfff;

        if hi20 != 0 {
            (Some(RV64InstBuilder::LUI(reg, hi20).unwrap()), lo12)
        } else {
            (None, lo12)
        }
    }
}