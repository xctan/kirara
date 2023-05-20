use crate::ctype::TypePtrHelper;

use super::{
    value::{ValueType, ValueId, ConstantValue, InstructionValue, ValueTrait},
    builder::{TransUnit, IrFunc}
};

impl TransUnit {
    pub fn print(&self) {
        for (name, func) in &self.funcs {
            self.print_func(name, func);
        }
    }

    pub fn print_func(&self, name: &str, func: &IrFunc) {
        let ty = func.ty.get().as_function();
        print!("define {} @{}(", ty.ret_type.get(), name);
        for (idx, (_, argty)) in ty.params.iter().enumerate() {
            if idx != 0 {
                print!(", ");
            }
            print!("{} %{}", argty.get(), idx);
        }
        println!(") {{");

        let arena = &self.values;
        for bb in &func.bbs {
            let bb = *bb;
            let bb0 = self.blocks.get(bb).unwrap();
            print!("{}:", bb0.name);
            bb0.preds.iter().enumerate().for_each(|(idx, bb)| {
                if idx == 0 {
                    print!("                                  ; preds: ");
                } else {
                    print!(", ");
                }
                print!("%{}", self.blocks.get(*bb).unwrap().name);
            });
            println!();
            let mut insts = bb0.insts_start;
            while let Some(inst) = insts {
                print!("  ");
                let inst = arena.get(inst).unwrap();
                insts = inst.next;
                match inst.value {
                    ValueType::Global(_) | ValueType::Parameter(_) => unreachable!(),
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
                            InstructionValue::Phi(ref insn) => {
                                print!("{} = phi {} ", insn.name, insn.ty.get());
                                for (idx, &(val, bb)) in insn.args.iter().enumerate() {
                                    if idx != 0 {
                                        print!(", ");
                                    }
                                    print!("[");
                                    self.print_value(val);
                                    print!(", %{}]", self.blocks.get(bb).unwrap().name);
                                }
                                println!();
                            }
                            InstructionValue::GetElemPtr(ref gep) => {
                                print!("{} = getelementptr inbounds {}, ptr ", gep.name, gep.aggregate_ty.get());
                                self.print_value(gep.ptr);
                                let idx_val = arena.get(gep.index).unwrap();
                                print!(", i64 0, {} ", idx_val.ty().get());
                                self.print_value(gep.index);
                                println!();
                            }
                        }
                    }
                    ValueType::Constant(_) => unimplemented!(),
                }
            }
        }

        println!("}}");
    }

    pub fn print_value(&self, id: ValueId) {
        let bb = &self.values;
        let val = bb.get(id).unwrap();
        match val.value {
            ValueType::Global(_) => unimplemented!(),
            ValueType::Constant(c) => {
                match c {
                    ConstantValue::I1(b) => print!("{}", b),
                    ConstantValue::I32(i) => print!("{}", i),
                }
            }
            ValueType::Instruction(ref insn) => {
                print!("{}", insn.name());
            }
            ValueType::Parameter(ref p) => {
                print!("{}", p.name());
            }
        }
    }
}