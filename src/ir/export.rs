use crate::ctype::TypePtrHelper;

use super::{
    value::{ValueType, ValueId, ConstantValue, InstructionValue, ValueTrait},
    unit::TransUnit
};

impl TransUnit {
    pub fn print(&self) {
        let arena = &self.values;
        let bb0 = self.blocks.get(self.entry_bb).unwrap();
        let mut insts = bb0.insts_start;
        while let Some(inst) = insts {
            let inst = arena.get(inst).unwrap();
            insts = inst.next;
            match inst.value {
                ValueType::Global(_) => unimplemented!(),
                ValueType::Instruction(ref insn) => {
                    match *insn {
                        InstructionValue::BinaryInst(ref insn) => {
                            print!("{} = {} {} ", insn.name, insn.op, insn.ty.get());
                            self.print_value(insn.lhs);
                            print!(", ");
                            self.print_value(insn.rhs);
                            println!();
                        }
                        InstructionValue::ReturnInst(ref insn) => {
                            print!("ret");
                            if let Some(val) = insn.value {
                                print!(" {} ", arena.get(val).unwrap().ty().get());
                                self.print_value(val);
                            } else {
                                print!(" void");
                            }
                            println!();
                        }
                        InstructionValue::LoadInst(ref insn) => {
                            print!("{} = load {}, ptr ", insn.name, insn.ty.get());
                            self.print_value(insn.ptr);
                            println!();
                        }
                        InstructionValue::StoreInst(ref insn) => {
                            let v = arena.get(insn.value).unwrap();
                            print!("store {} ", v.ty().get());
                            self.print_value(insn.value);
                            print!(", ptr ");
                            self.print_value(insn.ptr);
                            println!();
                        }
                        InstructionValue::AllocaInst(ref insn) => {
                            print!("{} = alloca {}, align {}", insn.name, insn.ty.get(), insn.ty.get().align());
                            println!();
                        }
                    }
                }
                ValueType::Constant(_) => unimplemented!(),
            }
        }
    }

    pub fn print_value(&self, id: ValueId) {
        let bb = &self.values;
        let val = bb.get(id).unwrap();
        match val.value {
            ValueType::Global(_) => unimplemented!(),
            ValueType::Constant(c) => {
                match c {
                    ConstantValue::I32(i) => print!("{}", i),
                }
            }
            ValueType::Instruction(ref insn) => {
                print!("{}", insn.name());
            }
        }
    }
}