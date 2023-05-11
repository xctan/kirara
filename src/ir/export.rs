use crate::ctype::TypePtrHelper;

use super::{
    value::{ValueType, ValueId, ConstantValue, InstructionValue, ValueTrait},
    unit::TransUnit
};

impl TransUnit {
    pub fn print(&self) {
        let arena = &self.values;
        for bb in &self.bbs {
            let bb = *bb;
            let bb0 = self.blocks.get(bb).unwrap();
            println!("{}:", bb0.name);
            let mut insts = bb0.insts_start;
            while let Some(inst) = insts {
                print!("  ");
                let inst = arena.get(inst).unwrap();
                insts = inst.next;
                match inst.value {
                    ValueType::Global(_) => unimplemented!(),
                    ValueType::Instruction(ref insn) => {
                        match *insn {
                            InstructionValue::Binary(ref insn) => {
                                let lhs = arena.get(insn.lhs).unwrap();
                                print!("{} = {} {} ", insn.name, insn.op, lhs.ty().get());
                                self.print_value(insn.lhs);
                                print!(", ");
                                self.print_value(insn.rhs);
                                println!();
                            }
                            InstructionValue::Return(ref insn) => {
                                print!("ret");
                                if let Some(val) = insn.value {
                                    print!(" {} ", arena.get(val).unwrap().ty().get());
                                    self.print_value(val);
                                } else {
                                    print!(" void");
                                }
                                println!();
                            }
                            InstructionValue::Load(ref insn) => {
                                print!("{} = load {}, ptr ", insn.name, insn.ty.get());
                                self.print_value(insn.ptr);
                                println!();
                            }
                            InstructionValue::Store(ref insn) => {
                                let v = arena.get(insn.value).unwrap();
                                print!("store {} ", v.ty().get());
                                self.print_value(insn.value);
                                print!(", ptr ");
                                self.print_value(insn.ptr);
                                println!();
                            }
                            InstructionValue::Alloca(ref insn) => {
                                print!("{} = alloca {}, align {}", insn.name, insn.ty.get(), insn.ty.get().align());
                                println!();
                            }
                            InstructionValue::Branch(ref insn) => {
                                print!("br ");
                                print!("{} ", arena.get(insn.cond).unwrap().ty().get());
                                self.print_value(insn.cond);
                                print!(", ");
                                let succ = self.blocks.get(insn.succ).unwrap();
                                let fail = self.blocks.get(insn.fail).unwrap();
                                println!("label %{}, label %{}", succ.name, fail.name);
                            }
                            InstructionValue::Jump(ref insn) => {
                                let succ = self.blocks.get(insn.succ).unwrap();
                                println!("br label %{}", succ.name);
                            }
                            InstructionValue::Zext(ref insn) => {
                                print!("{} = zext {} ", insn.name, arena.get(insn.value).unwrap().ty().get());
                                self.print_value(insn.value);
                                print!(" to {}", insn.ty.get());
                                println!();
                            }
                        }
                    }
                    ValueType::Constant(_) => unimplemented!(),
                }
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