use std::fmt::Display;

use crate::{ast::{Initializer, InitData}, ctype::{Linkage, TypeKind}, ir::value::UnaryInst};

use super::{
    value::{ValueType, ValueId, ConstantValue, InstructionValue, ValueTrait, BinaryOp},
    structure::{TransUnit, GlobalObject}, structure::IrFunc
};

impl Display for TransUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
    }
}

#[allow(unused)]
pub struct IrFuncFormatter<'a> {
    unit: &'a TransUnit,
    func: &'a str,
}

#[allow(unused)]
impl<'a> IrFuncFormatter<'a> {
    pub fn new(unit: &'a TransUnit, func: &'a str) -> Self {
        Self {
            unit,
            func,
        }
    }
}

impl<'a> Display for IrFuncFormatter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let irfunc = &self.unit.funcs[self.func];
        self.unit.print_func(self.func, irfunc, f)
    }
}

impl TransUnit {
    pub fn print(&self, writer: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut keys: Vec<_> = self.globals.keys().collect();
        keys.sort();
        for name in keys {
            let init = self.globals.get(name).unwrap();
            self.print_global(name, init, writer)?;
        }
        writeln!(writer)?;

        let mut keys: Vec<_> = self.funcs.keys().collect();
        keys.sort();
        for name in keys {
            let func = self.funcs.get(name).unwrap();
            self.print_func(name, func, writer)?;
            writeln!(writer)?;
        }

        let mut keys: Vec<_> = self.external.keys().collect();
        keys.sort();
        for name in keys {
            let ty = self.external.get(name).unwrap().as_function();
            write!(writer, "declare {} @{}(", ty.ret_type, name)?;
            for (idx, (_, argty)) in ty.params.iter().enumerate() {
                if idx != 0 {
                    write!(writer, ", ")?;
                }
                write!(writer, "{} %{}", argty, idx)?;
            }
            writeln!(writer, ")")?;
            writeln!(writer)?;
        }

        Ok(())
    }

    fn print_global(&self, name: &str, init: &GlobalObject, writer: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(writer, 
            "@{name} = {} ", 
            match init.linkage {
                Linkage::Global => "global",
                Linkage::Static => "internal global",
                Linkage::Extern => "external global"
            }
        )?;
        self.print_initializer(&init.init, writer)?;
        writeln!(writer, ", align {}", init.init.ty.align())
    }

    fn print_initializer(&self, init: &Initializer, writer: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(writer, "{} ", init.ty)?;
        match init.data {
            InitData::ScalarI32(i) => write!(writer, "{}", i),
            InitData::ScalarF32(f) => write!(writer, "0x{:x}", (f as f64).to_bits()),
            InitData::Aggregate(ref data) => {
                write!(writer, "[")?;
                for (idx, val) in data.iter().enumerate() {
                    if idx != 0 {
                        write!(writer, ", ")?;
                    }
                    self.print_initializer(val, writer)?;
                }
                write!(writer, "]")
            },
            InitData::ZeroInit => write!(writer, "zeroinitializer"),
            // FIXME: dump str correctly
            InitData::StringLiteral(ref s) => write!(writer, "c\"{}\"", s),
            _ => panic!("unexpected non-constant initializer: {:?}", init.data),
        }
    }

    fn print_typed_value(&self, val: ValueId, writer: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = self.values.get(val).unwrap();
        write!(writer, "{} ", value.ty())?;
        self.print_value(val, writer)
    }

    fn print_func(&self, name: &str, func: &IrFunc, writer: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ty = func.ty.as_function();
        write!(writer, "define {} @{}(", ty.ret_type, name)?;
        for (idx, (_, argty)) in ty.params.iter().enumerate() {
            if idx != 0 {
                write!(writer, ", ")?;
            }
            write!(writer, "{} %{}", argty, idx)?;
        }
        writeln!(writer, ") {{")?;

        let arena = &self.values;
        for bb in &func.bbs {
            let bb = *bb;
            let bb0 = self.blocks.get(bb).unwrap();
            write!(writer, "{:<40}", format!("{}:", bb0.name))?;
            bb0.preds.iter().enumerate().for_each(|(idx, bb)| {
                if idx == 0 {
                    write!(writer, "; preds: ").unwrap();
                } else {
                    write!(writer, ", ").unwrap();
                }
                write!(writer, "%{}", self.blocks.get(*bb).map(|bb| bb.name.as_str()).unwrap_or("???")).unwrap();
            });
            writeln!(writer, )?;
            let mut insts = bb0.insts_start;
            while let Some(vid) = insts {
                let inst = arena.get(vid).unwrap();
                insts = inst.next;
                // writeln!(writer, "; id = {:?}", vid)?;
                // write!(writer, "; used by")?;
                // for (idx, user) in inst.used_by.iter().enumerate() {
                //     if idx != 0 {
                //         write!(writer, ",")?;
                //     }
                //     write!(writer, " {:?} [{}]", user, self.values.get(*user).is_some())?;
                // }
                // writeln!(writer)?;
                write!(writer, "  ")?;
                match inst.value {
                    ValueType::Global(_) | ValueType::Parameter(_) => unreachable!(),
                    ValueType::Instruction(ref insn) => {
                        match *insn {
                            InstructionValue::Binary(ref insn) => {
                                let lhs = match arena.get(insn.lhs) {
                                    Some(value) => value,
                                    None => {
                                        writeln!(writer, "UNDEFINED")?;
                                        continue
                                    }
                                };
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
                                            BinaryOp::And => "and",
                                            BinaryOp::Shl => "shl",
                                            BinaryOp::Shr => "ashr",
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
                                write!(writer, "{} = {} {} ", insn.name, op_name, lhs.ty())?;
                                self.print_value(insn.lhs, writer)?;
                                write!(writer, ", ")?;
                                self.print_value(insn.rhs, writer)?;
                                writeln!(writer, )?;
                            }
                            InstructionValue::Return(ref insn) => {
                                write!(writer, "ret")?;
                                if let Some(val) = insn.value {
                                    write!(writer, " {} ", arena.get(val).unwrap().ty())?;
                                    self.print_value(val, writer)?;
                                } else {
                                    write!(writer, " void")?;
                                }
                                writeln!(writer, )?;
                            }
                            InstructionValue::Load(ref insn) => {
                                write!(writer, "{} = load {}, ptr ", insn.name, insn.ty)?;
                                self.print_value(insn.ptr, writer)?;
                                writeln!(writer, )?;
                            }
                            InstructionValue::Store(ref insn) => {
                                let v = arena.get(insn.value).unwrap();
                                write!(writer, "store {} ", v.ty())?;
                                self.print_value(insn.value, writer)?;
                                write!(writer, ", ptr ")?;
                                self.print_value(insn.ptr, writer)?;
                                writeln!(writer, )?;
                            }
                            InstructionValue::Alloca(ref insn) => {
                                write!(writer, "{} = alloca {}, align {}", insn.name, insn.alloc_ty, insn.alloc_ty.align())?;
                                writeln!(writer, )?;
                            }
                            InstructionValue::Branch(ref insn) => {
                                write!(writer, "br ")?;
                                write!(writer, "{} ", arena.get(insn.cond).unwrap().ty())?;
                                self.print_value(insn.cond, writer)?;
                                write!(writer, ", ")?;
                                let succ = self.blocks.get(insn.succ).unwrap();
                                let fail = self.blocks.get(insn.fail).unwrap();
                                writeln!(writer, "label %{}, label %{}", succ.name, fail.name)?;
                            }
                            InstructionValue::Jump(ref insn) => {
                                let succ = self.blocks.get(insn.succ).unwrap();
                                writeln!(writer, "br label %{}", succ.name)?;
                            }
                            InstructionValue::Unary(ref insn @ UnaryInst { op: super::value::UnaryOp::NegF32, .. }) => {
                                write!(writer, "{} = fneg {} ", insn.name, arena.get(insn.value).unwrap().ty())?;
                                self.print_value(insn.value, writer)?;
                                writeln!(writer, )?;
                            }
                            InstructionValue::Unary(ref insn) => {
                                let op_name = match insn.op {
                                    super::value::UnaryOp::CvtF32I32 => "sitofp",
                                    super::value::UnaryOp::CvtI32F32 => "fptosi",
                                    super::value::UnaryOp::ZextI32I1 => "zext",
                                    _ => unimplemented!("ir export unary op {:?}", insn.op)
                                };
                                write!(writer, "{} = {} {} ", insn.name, op_name, arena.get(insn.value).unwrap().ty())?;
                                self.print_value(insn.value, writer)?;
                                write!(writer, " to {}", insn.ty)?;
                                writeln!(writer, )?;
                            }
                            InstructionValue::Phi(ref insn) => {
                                write!(writer, "{} = phi {} ", insn.name, insn.ty)?;
                                for (idx, &(val, bb)) in insn.args.iter().enumerate() {
                                    if idx != 0 {
                                        write!(writer, ", ")?;
                                    }
                                    write!(writer, "[")?;
                                    self.print_value(val, writer)?;
                                    write!(writer, ", %{}]", self.blocks.get(bb).map(|bb| bb.name.as_str()).unwrap_or("???"))?;
                                }
                                writeln!(writer, )?;
                            }
                            InstructionValue::GetElemPtr(ref gep) => {
                                write!(writer, "{} = getelementptr inbounds {}, ", gep.name, gep.base_ty)?;
                                self.print_typed_value(gep.ptr, writer)?;
                                for idx in &gep.indices {
                                    write!(writer, ", ")?;
                                    self.print_typed_value(*idx, writer)?;
                                }
                                writeln!(writer, )?;
                            }
                            InstructionValue::Call(ref call) => {
                                if call.ty.is_void() {
                                    write!(writer, "call {} @{}", call.ty, call.func)?;
                                } else {
                                    write!(writer, "{} = call {} @{}", call.name, call.ty, call.func)?;
                                }
                                write!(writer, "(")?;
                                for (idx, arg) in call.args.iter().enumerate() {
                                    if idx != 0 {
                                        write!(writer, ", ")?;
                                    }
                                    write!(writer, "{} ", arena.get(*arg).unwrap().ty())?;
                                    self.print_value(*arg, writer)?;
                                }
                                writeln!(writer, ")")?;
                            }
                            InstructionValue::TailCall(ref call) => {
                                if call.ty.is_void() {
                                    write!(writer, "tail call {} @{}", call.ty, call.func)?;
                                } else {
                                    write!(writer, "{} = tail call {} @{}", call.name, call.ty, call.func)?;
                                }
                                write!(writer, "(")?;
                                for (idx, arg) in call.args.iter().enumerate() {
                                    if idx != 0 {
                                        write!(writer, ", ")?;
                                    }
                                    write!(writer, "{} ", arena.get(*arg).unwrap().ty())?;
                                    self.print_value(*arg, writer)?;
                                }
                                writeln!(writer, ")")?;
                            }
                            InstructionValue::MemOp(ref mop) => {
                                write!(writer, "\r; memop")?;
                                if let Some(l) = mop.after_load {
                                    write!(writer, " after ")?;
                                    self.print_value(l, writer)?;
                                }
                                writeln!(writer)?;
                            }
                            InstructionValue::MemPhi(_) => {}
                            InstructionValue::Switch(ref sw) => {
                                let cond_v = arena.get(sw.cond).unwrap();
                                write!(writer, "switch {} ", cond_v.ty())?;
                                self.print_value(sw.cond, writer)?;
                                writeln!(writer, ", label %{} [", self.blocks.get(sw.default).unwrap().name)?;
                                for (_idx, &(val, bb)) in sw.cases.iter().enumerate() {
                                    // if idx != 0 {
                                    //     write!(writer, ", ")?;
                                    // }
                                    // only supported type
                                    writeln!(writer, "    i32 {}, label %{}", val, self.blocks.get(bb).unwrap().name)?;
                                }
                                writeln!(writer, "  ]")?;
                            }
                        }
                    }
                    ValueType::Constant(_) => unimplemented!(),
                }
            }
        }

        writeln!(writer, "}}")
    }

    pub fn print_value(&self, id: ValueId, writer: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bb = &self.values;
        let val = match bb.get(id) {
            Some(v) => v,
            None => {
                return write!(writer, "@UNDEFINED@")
            }
        };
        match val.value {
            ValueType::Global(ref g) => {
                write!(writer, "@{}", g.name)
            },
            ValueType::Constant(c) => {
                match c {
                    ConstantValue::Undef => write!(writer, "undef"),
                    ConstantValue::I1(b) => write!(writer, "{}", b),
                    ConstantValue::I32(i) => write!(writer, "{}", i),
                    ConstantValue::F32(f) => write!(writer, "0x{:x}", (f as f64).to_bits()),
                }
            }
            ValueType::Instruction(ref insn) => {
                write!(writer, "{}", insn.name())
            }
            ValueType::Parameter(ref p) => {
                write!(writer, "{}", p.name())
            }
        }
    }
}