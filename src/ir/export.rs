use super::{
    value::{Value, ValueId, ConstantValue, InstructionValue, ValueTrait},
    unit::TransUnit
};

impl TransUnit {
    pub fn print(&self) {
        let bb = self.values.borrow();
        for inst in &self.instns {
            let inst = bb.get(*inst).unwrap();
            match *inst {
                Value::Global(_) => unimplemented!(),
                Value::Instruction(ref insn) => {
                    match *insn {
                        InstructionValue::BinaryOperator(ref insn) => {
                            print!("{} = {} {} ", insn.name, insn.op, insn.ty);
                            self.print_value(insn.lhs);
                            print!(", ");
                            self.print_value(insn.rhs);
                            println!();
                        }
                        InstructionValue::ReturnInst(ref insn) => {
                            print!("ret");
                            if let Some(val) = insn.value {
                                print!(" {} ", bb.get(val).unwrap().ty());
                                self.print_value(val);
                            } else {
                                print!(" void");
                            }
                            println!();
                        }
                        InstructionValue::LoadInst(ref insn) => {
                            print!("{} = load {}, ptr ", insn.name, insn.ty);
                            self.print_value(insn.ptr);
                            println!();
                        }
                        InstructionValue::StoreInst(ref insn) => {
                            let v = bb.get(insn.value).unwrap();
                            print!("store {} ", v.ty());
                            self.print_value(insn.value);
                            print!(", ptr ");
                            self.print_value(insn.ptr);
                            println!();
                        }
                        InstructionValue::AllocaInst(ref insn) => {
                            print!("{} = alloca {}, align {}", insn.name, insn.ty, insn.ty.align());
                            println!();
                        }
                    }
                }
                Value::Constant(_) => unimplemented!(),
            }
        }
    }

    pub fn print_value(&self, id: ValueId) {
        let bb = self.values.borrow();
        let val = bb.get(id).unwrap();
        match *val {
            Value::Global(_) => unimplemented!(),
            Value::Constant(c) => {
                match c {
                    ConstantValue::I32(i) => print!("{}", i),
                }
            }
            Value::Instruction(ref insn) => {
                print!("{}", insn.name());
            }
        }
    }
}