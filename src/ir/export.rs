use crate::{ast::{Initializer, InitData}, ctype::{Linkage, TypeKind}, ir::value::UnaryInst};

use super::{
    value::{ValueType, ValueId, ConstantValue, InstructionValue, ValueTrait, BinaryOp},
    structure::{TransUnit, GlobalObject}, structure::IrFunc
};

impl TransUnit {
    pub fn print(&self) {
        for (name, init) in &self.globals {
            self.print_global(name, init);
        }

        for (name, func) in &self.funcs {
            self.print_func(name, func);
        }
    }

    fn print_global(&self, name: &str, init: &GlobalObject) {
        print!(
            "@{name} = {} ", 
            match init.linkage {
                Linkage::Global => "global",
                Linkage::Static => "internal global",
                Linkage::Extern => "external global"
            }
        );
        self.print_initializer(&init.init);
        println!(", align {}", init.init.ty.align());
    }

    fn print_initializer(&self, init: &Initializer) {
        print!("{} ", init.ty);
        match init.data {
            InitData::ScalarI32(i) => print!("{}", i),
            InitData::ScalarF32(f) => print!("0x{:x}", f.to_bits()),
            InitData::Aggregate(ref data) => {
                print!("[");
                for (idx, val) in data.iter().enumerate() {
                    if idx != 0 {
                        print!(", ");
                    }
                    self.print_initializer(val);
                }
                print!("]");
            },
            InitData::ZeroInit => print!("zeroinitializer"),
            _ => panic!("unexpected non-constant initializer: {:?}", init.data),
        }
    }

    fn print_typed_value(&self, val: ValueId) {
        let value = self.values.get(val).unwrap();
        print!("{} ", value.ty());
        self.print_value(val);
    }

    fn print_func(&self, name: &str, func: &IrFunc) {
        let ty = func.ty.as_function();
        print!("define {} @{}(", ty.ret_type, name);
        for (idx, (_, argty)) in ty.params.iter().enumerate() {
            if idx != 0 {
                print!(", ");
            }
            print!("{} %{}", argty, idx);
        }
        println!(") {{");

        let arena = &self.values;
        for bb in &func.bbs {
            let bb = *bb;
            let bb0 = self.blocks.get(bb).unwrap();
            print!("{:<40}", format!("{}:", bb0.name));
            bb0.preds.iter().enumerate().for_each(|(idx, bb)| {
                if idx == 0 {
                    print!("; preds: ");
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
                                let op_name = match lhs.ty().kind {
                                    TypeKind::I32 => {
                                        match insn.op {
                                            BinaryOp::Add => "add",
                                            BinaryOp::Sub => "sub",
                                            BinaryOp::Mul => "mul",
                                            BinaryOp::Div => "sdiv",
                                            BinaryOp::Mod => "srem",
                                            BinaryOp::Ne => "icmp ne",
                                            BinaryOp::Lt => "icmp slt",
                                            BinaryOp::Eq => "icmp eq",
                                            BinaryOp::Le => "icmp sle",
                                            BinaryOp::Gt => "icmp sgt",
                                            BinaryOp::Ge => "icmp sge",
                                            BinaryOp::Xor => "xor",
                                            _ => unreachable!("i32 op {:?}", insn.op),
                                        }
                                    }
                                    TypeKind::F32 => {
                                        match insn.op {
                                            BinaryOp::Add => "fadd",
                                            BinaryOp::Sub => "fsub",
                                            BinaryOp::Mul => "fmul",
                                            BinaryOp::Div => "fdiv",
                                            BinaryOp::Ne => "fcmp one",
                                            BinaryOp::Lt => "fcmp olt",
                                            BinaryOp::Eq => "fcmp oeq",
                                            BinaryOp::Le => "fcmp ole",
                                            BinaryOp::Gt => "fcmp ogt",
                                            BinaryOp::Ge => "fcmp oge",
                                            _ => unreachable!("f32 op {:?}", insn.op),
                                        }
                                    }
                                    TypeKind::I1 => {
                                        match insn.op {
                                            BinaryOp::Xor => "xor",
                                            _ => unreachable!("i1 op {:?}", insn.op),
                                        }
                                    }
                                    _ => unimplemented!("binary op for type {:?}", lhs.ty().kind),
                                };
                                print!("{} = {} {} ", insn.name, op_name, lhs.ty());
                                self.print_value(insn.lhs);
                                print!(", ");
                                self.print_value(insn.rhs);
                                println!();
                            }
                            InstructionValue::Return(ref insn) => {
                                print!("ret");
                                if let Some(val) = insn.value {
                                    print!(" {} ", arena.get(val).unwrap().ty());
                                    self.print_value(val);
                                } else {
                                    print!(" void");
                                }
                                println!();
                            }
                            InstructionValue::Load(ref insn) => {
                                print!("{} = load {}, ptr ", insn.name, insn.ty);
                                self.print_value(insn.ptr);
                                println!();
                            }
                            InstructionValue::Store(ref insn) => {
                                let v = arena.get(insn.value).unwrap();
                                print!("store {} ", v.ty());
                                self.print_value(insn.value);
                                print!(", ptr ");
                                self.print_value(insn.ptr);
                                println!();
                            }
                            InstructionValue::Alloca(ref insn) => {
                                print!("{} = alloca {}, align {}", insn.name, insn.alloc_ty, insn.alloc_ty.align());
                                println!();
                            }
                            InstructionValue::Branch(ref insn) => {
                                print!("br ");
                                print!("{} ", arena.get(insn.cond).unwrap().ty());
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
                            InstructionValue::Unary(ref insn @ UnaryInst { op: super::value::UnaryOp::NegF32, .. }) => {
                                print!("{} = fneg {} ", insn.name, arena.get(insn.value).unwrap().ty());
                                self.print_value(insn.value);
                                println!();
                            }
                            InstructionValue::Unary(ref insn) => {
                                let op_name = match insn.op {
                                    super::value::UnaryOp::CvtF32I32 => "sitofp",
                                    super::value::UnaryOp::CvtI32F32 => "fptosi",
                                    super::value::UnaryOp::ZextI32I1 => "zext",
                                    _ => unimplemented!("ir export unary op {:?}", insn.op)
                                };
                                print!("{} = {} {} ", insn.name, op_name, arena.get(insn.value).unwrap().ty());
                                self.print_value(insn.value);
                                print!(" to {}", insn.ty);
                                println!();
                            }
                            InstructionValue::Phi(ref insn) => {
                                print!("{} = phi {} ", insn.name, insn.ty);
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
                                print!("{} = getelementptr inbounds {}, ", gep.name, gep.base_ty);
                                self.print_typed_value(gep.ptr);
                                for idx in &gep.indices {
                                    print!(", ");
                                    self.print_typed_value(*idx);
                                }
                                println!();
                            }
                            InstructionValue::Call(ref call) => {
                                if call.ty.is_void() {
                                    print!("call {} @{}", call.ty, call.func);
                                } else {
                                    print!("{} = call {} @{}", call.name, call.ty, call.func);
                                }
                                print!("(");
                                for (idx, arg) in call.args.iter().enumerate() {
                                    if idx != 0 {
                                        print!(", ");
                                    }
                                    print!("{} ", arena.get(*arg).unwrap().ty());
                                    self.print_value(*arg);
                                }
                                println!(")");
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
            ValueType::Global(ref g) => {
                print!("@{}", g.name);
            },
            ValueType::Constant(c) => {
                match c {
                    ConstantValue::Undef => print!("undef"),
                    ConstantValue::I1(b) => print!("{}", b),
                    ConstantValue::I32(i) => print!("{}", i),
                    ConstantValue::F32(f) => print!("{}", f),
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