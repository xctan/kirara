use std::{rc::Rc, cell::RefCell};

use super::{unit::{TransUnit, LocalInstExt}, value::ValueId};
use crate::{ast::*, ctype::{BinaryOpType, TypePtrHelper, Type}};

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
            AstNodeType::Unit => (),
            AstNodeType::IfStmt(ifs) => {
                // if let AstNodeType::BinaryOp(op) = &ifs.cond.borrow().node {
                //     let BinaryOp { lhs, rhs, op } = op;
                //     if matches!(op, BinaryOpType::LogAnd | BinaryOpType::LogOr) {

                //         return;
                //     }
                // }
                let cond = ifs.cond.emit_ir_expr(unit, ctx);
                let (root, _) = unit.start_new_bb();
                ifs.then.emit_ir(unit, ctx);
                // succ: get last bb of then ext bb
                let (succ, fail) = unit.start_new_bb();
                unit.branch(cond, succ, fail).push_to(root);
                if !&ifs.els.borrow().is_unit() {
                    ifs.els.emit_ir(unit, ctx);
                    let (fail_last, finally) = unit.start_new_bb();
                    unit.jump(finally).push_to(succ);
                    unit.jump(finally).push_to(fail_last);
                } else {
                    unit.jump(fail).push_to(succ);
                }
            },
            AstNodeType::WhileStmt(whiles) => {
                let (root, test) = unit.start_new_bb();
                unit.jump(test).push_to(root);
                let cond = whiles.cond.emit_ir_expr(unit, ctx);
                let (test_last, body) = unit.start_new_bb();
                whiles.body.emit_ir(unit, ctx);
                let (body_last, fail) = unit.start_new_bb();
                unit.jump(test).push_to(body_last);
                unit.branch(cond, body, fail).push_to(test_last);
            }
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
            AstNodeType::I32Number(num) => {
                unit.const_i32(*num)
            }
            AstNodeType::Convert(Convert { from, to }) => {
                let from_id = from.emit_ir_expr(unit, ctx);
                let to = to.upgrade().unwrap();
                match ((*from.borrow().ty.get()).clone(), (*to).clone()) {
                    (Type::I32, Type::I1) => {
                        let zero = unit.const_i32(0);
                        unit.binary(BinaryOpType::Ne, from_id, zero).push()
                    },
                    (Type::I1, Type::I32) => {
                        unit.zext(from_id, Type::i32_type()).push()
                    },
                    _ => unimplemented!(),
                }
            }
            AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }) => {
                if matches!(op, &BinaryOpType::Assign) {
                    let lhs = lhs.emit_ir_lvalue(unit, ctx);
                    let rhs = rhs.emit_ir_expr(unit, ctx);
                    unit.store(rhs, lhs).push();
                    rhs
                } else if matches!(op, &BinaryOpType::LogAnd) {
                    let lhs = lhs.emit_ir_expr(unit, ctx);
                    let (left, _) = unit.start_new_bb();
                    let rhs = rhs.emit_ir_expr(unit, ctx);
                    let (right, fini) = unit.start_new_bb();
                    let f = unit.const_i1(false);
                    unit.branch(lhs, right, fini).push_to(left);
                    unit.jump(fini).push_to(right);
                    unit.phi(vec![(f, left), (rhs, right)]).push()
                } else if matches!(op, &BinaryOpType::LogOr) {
                    let lhs = lhs.emit_ir_expr(unit, ctx);
                    let (left, _) = unit.start_new_bb();
                    let rhs = rhs.emit_ir_expr(unit, ctx);
                    let (right, fini) = unit.start_new_bb();
                    let t = unit.const_i1(true);
                    unit.branch(lhs, fini, right).push_to(left);
                    unit.jump(fini).push_to(right);
                    unit.phi(vec![(t, left), (rhs, right)]).push()
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
