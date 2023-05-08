use super::{
    unit::TransUnit,
    value::{BinaryOperator, ConstantValue, InstructionValue, Value, ValueId, ReturnInst, GlobalValue, AllocaInst, LoadInst, StoreInst},
};
use crate::{ast::*, ctype::{Type, BinaryOpType}};

pub trait EmitIr {
    fn emit_ir(&self, unit: &mut TransUnit, ctx: &mut AstContext);
}

impl AstContext {
    pub fn emit_ir(&mut self, unit: &mut TransUnit) {
        for var in self.globals.clone() {
            let var = self.get_object(var).unwrap();
            match var.data.clone() {
                AstObjectType::Func(func) => {
                    for v in &func.locals {
                        let obj = self.get_object_mut(*v).unwrap();
                        let inst = AllocaInst {
                            name: format!("%{}", obj.name),
                            ty: obj.ty.clone(),
                        };
                        let val = Value::Instruction(InstructionValue::AllocaInst(inst));
                        let id = unit.values.borrow_mut().alloc(val);
                        unit.push_inst(id);
                        obj.ir_value = Some(id);
                    }

                    func.body.borrow().emit_ir(unit, self);
                }
                AstObjectType::Var => {
                    let val = Value::Global(
                        GlobalValue {
                            name: var.name.clone(),
                            ty: var.ty.clone(),
                            // init: None,
                        }
                    );
                    let id = unit.values.borrow_mut().alloc(val);
                    unit.push_inst(id);
                }
            }
        }
    }
}

impl EmitIr for AstNode {
    fn emit_ir(&self, unit: &mut TransUnit, ctx: &mut AstContext) {
        match &self.node {
            AstNodeType::ExprStmt(expr) => {
                expr.borrow().node.emit_ir_expr(unit, ctx);
            },
            AstNodeType::Return(expr) => {
                let expr = expr.borrow().node.emit_ir_expr(unit, ctx);
                let insn = ReturnInst { value: expr };
                let insn = InstructionValue::ReturnInst(insn);
                let val = Value::Instruction(insn);
                let id = unit.values.borrow_mut().alloc(val);
                unit.push_inst(id);
            },
            AstNodeType::Block(stmts) => {
                for stmt in stmts {
                    stmt.borrow().emit_ir(unit, ctx);
                }
            },
            _ => unimplemented!(),
        }
    }
}

trait EmitIrExpr {
    fn emit_ir_expr(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> Option<ValueId>;
}

impl EmitIrExpr for AstNodeType {
    fn emit_ir_expr(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> Option<ValueId> {
        match self {
            AstNodeType::ExprStmt(expr) => expr.borrow().node.emit_ir_expr(unit, ctx),
            AstNodeType::I64Number(num) => {
                let val = Value::Constant(ConstantValue::I32(
                    i32::try_from(*num % i32::MAX as i64).unwrap(),
                ));
                let id = unit.values.borrow_mut().alloc(val);
                Some(id)
            }
            AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }) => {
                if matches!(op, &BinaryOpType::Assign) {
                    let lhs = lhs.borrow().node.emit_ir_lvalue(unit, ctx).unwrap();
                    let rhs = rhs.borrow().node.emit_ir_expr(unit, ctx).unwrap();
                    let inst = StoreInst {
                        value: rhs,
                        ptr: lhs,
                    };
                    let val = Value::Instruction(InstructionValue::StoreInst(inst));
                    let id = unit.values.borrow_mut().alloc(val);
                    unit.push_inst(id);
                    Some(rhs)
                } else {
                    let lhs = lhs.borrow().node.emit_ir_expr(unit, ctx).unwrap();
                    let rhs = rhs.borrow().node.emit_ir_expr(unit, ctx).unwrap();
                    let insn = BinaryOperator {
                        lhs,
                        rhs,
                        op: op.clone(),
                        name: unit.gen_local_name(),
                        // todo: type annotation and type checking in ast
                        ty: Type::I32,
                    };
                    let val = Value::Instruction(InstructionValue::BinaryOperator(insn));
                    let id = unit.values.borrow_mut().alloc(val);
                    unit.push_inst(id);
                    Some(id)
                }
            }
            AstNodeType::Variable(var) => {
                let var = ctx.get_object(*var).unwrap();
                let val = var.ir_value.unwrap();
                let inst = LoadInst {
                    name: unit.gen_local_name(),
                    ty: var.ty.clone(),
                    ptr: val,
                };
                let val = Value::Instruction(InstructionValue::LoadInst(inst));
                let id = unit.values.borrow_mut().alloc(val);
                unit.push_inst(id);
                Some(id)
            }
            AstNodeType::Unit => None,
            _ => unimplemented!(),
        }
    }
}

trait EmitIrLValue {
    fn emit_ir_lvalue(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> Option<ValueId>;
}

impl EmitIrLValue for AstNodeType {
    fn emit_ir_lvalue(&self, _unit: &mut TransUnit, ctx: &mut AstContext) -> Option<ValueId> {
        match self {
            AstNodeType::Variable(var) => {
                let var = ctx.get_object(*var).unwrap();
                let val = var.ir_value.unwrap();
                Some(val)
            }
            _ => unimplemented!(),
        }
    }
}