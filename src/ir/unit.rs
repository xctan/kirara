use std::cell::RefCell;

use id_arena::Arena;

use crate::ir::value::ValueTrait;

use super::value::{Value, ValueId, ConstantValue, InstructionValue};

#[derive(Debug)]
pub struct TransUnit {
    /// global value allocator
    pub values: RefCell<Arena<Value>>,
    // global variable defs
    // funcs

    // todo: move to bb
    pub instns: Vec<ValueId>,
    counter: usize,
}

impl TransUnit {
    pub fn new() -> Self {
        Self {
            values: RefCell::new(Arena::new()),
            instns: Vec::new(),
            counter: 0,
        }
    }

    pub fn gen_local_name(&mut self) -> String {
        let name = format!("%{}", self.counter);
        self.counter += 1;
        name
    }

    pub fn push_inst(&mut self, inst: ValueId) {
        self.instns.push(inst);
    }

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