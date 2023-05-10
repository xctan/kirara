use std::{rc::Rc, cell::RefCell};

use super::{unit::{TransUnit, LocalInstExt}, value::ValueId};
use crate::{ast::*, ctype::BinaryOpType};

pub trait EmitIr {
    fn emit_ir(&self, unit: &mut TransUnit, ctx: &mut AstContext);
}

impl AstContext {
    pub fn emit_ir(&mut self, unit: &mut TransUnit) {
        for var in self.globals.clone() {
            let var = self.get_object(var).unwrap();
            match var.data.clone() {
                AstObjectType::Func(ref func) => {
                    for v in &func.locals {
                        let obj = self.get_object_mut(*v).unwrap();
                        let id = unit.alloca(obj.ty.clone()).push();
                        obj.ir_value = Some(id);
                    }

                    func.body.emit_ir(unit, self);
                }
                AstObjectType::Var => {
                    todo!()
                }
            }
        }
    }
}

impl EmitIr for AstNode {
    fn emit_ir(&self, unit: &mut TransUnit, ctx: &mut AstContext) {
        match &self.node {
            AstNodeType::ExprStmt(expr) => {
                expr.emit_ir_expr(unit, ctx);
            },
            AstNodeType::Return(expr) => {
                // current implementation is not correct for multiple returns
                let expr = expr.emit_ir_expr(unit, ctx);
                unit.ret(Some(expr)).push();
            },
            AstNodeType::Block(stmts) => {
                for stmt in stmts {
                    stmt.emit_ir(unit, ctx);
                }
            },
            AstNodeType::IfStmt(ifs) => {
                // let cond = ifs.cond.emit_ir_expr(unit, ctx);
                todo!()
            },
            _ => unimplemented!(),
        }
    }
}

impl EmitIr for Rc<RefCell<AstNode>> {
    fn emit_ir(&self, unit: &mut TransUnit, ctx: &mut AstContext) {
        self.borrow().emit_ir(unit, ctx);
    }
}

trait EmitIrExpr {
    fn emit_ir_expr(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId;
}

impl EmitIrExpr for AstNodeType {
    fn emit_ir_expr(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId {
        match self {
            AstNodeType::ExprStmt(expr) => expr.emit_ir_expr(unit, ctx),
            AstNodeType::I64Number(num) => {
                unit.const_i32(*num as i32)
            }
            AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }) => {
                if matches!(op, &BinaryOpType::Assign) {
                    let lhs = lhs.emit_ir_lvalue(unit, ctx);
                    let rhs = rhs.emit_ir_expr(unit, ctx);
                    unit.store(rhs, lhs).push();
                    rhs
                } else {
                    let lhs = lhs.emit_ir_expr(unit, ctx);
                    let rhs = rhs.emit_ir_expr(unit, ctx);
                    unit.binary(*op, lhs, rhs).push()
                }
            }
            AstNodeType::Variable(var) => {
                let var = ctx.get_object(*var).unwrap();
                let val = var.ir_value.unwrap();
                unit.load(val).push()
            }
            _ => unimplemented!(),
        }
    }
}

impl EmitIrExpr for Rc<RefCell<AstNode>> {
    fn emit_ir_expr(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId {
        self.borrow().node.emit_ir_expr(unit, ctx)
    }
}

trait EmitIrLValue {
    fn emit_ir_lvalue(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId;
}

impl EmitIrLValue for AstNodeType {
    fn emit_ir_lvalue(&self, _unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId {
        match self {
            AstNodeType::Variable(var) => {
                let var = ctx.get_object(*var).unwrap();
                var.ir_value.unwrap()
            }
            _ => unimplemented!(),
        }
    }
}

impl EmitIrLValue for Rc<RefCell<AstNode>> {
    fn emit_ir_lvalue(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId {
        self.borrow().node.emit_ir_lvalue(unit, ctx)
    }
}