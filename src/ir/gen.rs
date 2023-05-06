use super::{
    unit::TransUnit,
    value::{BinaryOperator, ConstantValue, InstructionValue, Value, ValueId, ReturnInst},
};
use crate::{ast::*, ctype::Type};

pub trait EmitIr {
    fn emit_ir(&self, unit: &mut TransUnit);
}

impl EmitIr for AstNode<'_> {
    fn emit_ir(&self, unit: &mut TransUnit) {
        match &self.node {
            AstNodeType::ExprStmt(expr) => {
                expr.borrow().node.emit_ir_expr(unit);
            },
            AstNodeType::Return(expr) => {
                let expr = expr.borrow().node.emit_ir_expr(unit);
                let insn = ReturnInst { value: expr };
                let insn = InstructionValue::ReturnInst(insn);
                let val = Value::Instruction(insn);
                let id = unit.values.borrow_mut().alloc(val);
                unit.push_inst(id);
            },
            _ => unimplemented!(),
        }
    }
}

trait EmitIrExpr {
    fn emit_ir_expr(&self, unit: &mut TransUnit) -> Option<ValueId>;
}

impl EmitIrExpr for AstNodeType<'_> {
    fn emit_ir_expr(&self, unit: &mut TransUnit) -> Option<ValueId> {
        match self {
            AstNodeType::ExprStmt(expr) => expr.borrow().node.emit_ir_expr(unit),
            AstNodeType::I64Number(num) => {
                let val = Value::Constant(ConstantValue::I32(
                    i32::try_from(*num % i32::MAX as i64).unwrap(),
                ));
                let id = unit.values.borrow_mut().alloc(val);
                Some(id)
            }
            AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }) => {
                let lhs = lhs.borrow().node.emit_ir_expr(unit).unwrap();
                let rhs = rhs.borrow().node.emit_ir_expr(unit).unwrap();
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
            AstNodeType::Unit => None,
            _ => unimplemented!(),
        }
    }
}
