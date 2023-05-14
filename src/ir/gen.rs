use std::{rc::Rc, cell::RefCell};

use super::{unit::{TransUnit, LocalInstExt, BlockId}, value::ValueId};
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
                let (tl, fl) = ifs.cond.emit_ir_logical(unit, ctx);
                tl.iter().for_each(|item| item.backpatch(unit, unit.cur_bb));
                ifs.then.emit_ir(unit, ctx);
                let (succ_last, fail) = unit.start_new_bb();
                fl.iter().for_each(|item| item.backpatch(unit, unit.cur_bb));
                if !&ifs.els.borrow().is_unit() {
                    ifs.els.emit_ir(unit, ctx);
                    let (fail_last, finally) = unit.start_new_bb();
                    unit.jump(finally).push_to(succ_last);
                    unit.jump(finally).push_to(fail_last);
                } else {
                    unit.jump(fail).push_to(succ_last);
                }
            },
            AstNodeType::WhileStmt(whiles) => {
                let (root, test) = unit.start_new_bb();
                unit.jump(test).push_to(root);
                let (tl, fl) = whiles.cond.emit_ir_logical(unit, ctx);
                tl.iter().for_each(|item| item.backpatch(unit, unit.cur_bb));
                whiles.body.emit_ir(unit, ctx);
                let (body_last, fail) = unit.start_new_bb();
                fl.iter().for_each(|item| item.backpatch(unit, fail));
                unit.jump(test).push_to(body_last);
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
                } else if matches!(op, &BinaryOpType::LogAnd | &BinaryOpType::LogOr) {
                    self.emit_ir_logical_as_value(unit, ctx)
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

struct BackPatchItem {
    pub branch: ValueId,
    pub slot: BackPatchType,
}

enum BackPatchType {
    BranchSuccess,
    BranchFail,
}

impl BackPatchItem {
    fn backpatch(&self, unit: &mut TransUnit, bb: BlockId) {
        use super::value::{ValueType, InstructionValue};
        match self.slot {
            BackPatchType::BranchSuccess => {
                let inst = unit.values.get_mut(self.branch).unwrap();
                match &mut inst.value {
                    ValueType::Instruction(InstructionValue::Branch(ref mut insn)) => {
                        insn.succ = bb;
                    }
                    _ => unreachable!(),
                }
            },
            BackPatchType::BranchFail => {
                let inst = unit.values.get_mut(self.branch).unwrap();
                match &mut inst.value {
                    ValueType::Instruction(InstructionValue::Branch(ref mut insn)) => {
                        insn.fail = bb;
                    }
                    _ => unreachable!(),
                }
            }
        }
        let inst_bb = unit.inst_bb.get(&self.branch).unwrap();
        unit.add_predecessor(bb, *inst_bb);
    }
}

/// Emit IR for logical expression in control flow representation
trait EmitIrLogical {
    /// Get (truelist, falselist) of logical expression, and backpatch later
    fn emit_ir_logical(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> (Vec<BackPatchItem>, Vec<BackPatchItem>);

    /// Get value of **logical and/or** expression, and use it as value
    fn emit_ir_logical_as_value(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId;
}

trait EmitIrLogicalInner {
    fn emit_ir_logical_inner(&self, unit: &mut TransUnit, ctx: &mut AstContext, last: bool) -> (Vec<BackPatchItem>, Vec<BackPatchItem>, Option<ValueId>);
}

impl EmitIrLogicalInner for AstNodeType {
    fn emit_ir_logical_inner(&self, unit: &mut TransUnit, ctx: &mut AstContext, last: bool) -> (Vec<BackPatchItem>, Vec<BackPatchItem>, Option<ValueId>) {
        match self {
            AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }) => {
                if matches!(op, &BinaryOpType::LogAnd) {
                    let (t1, f1, _) = lhs.emit_ir_logical_inner(unit, ctx, false);
                    // current bb is rhs entry; continue if lhs is true
                    t1.iter().for_each(|item| item.backpatch(unit, unit.cur_bb));
                    let (t2, f2, l) = rhs.emit_ir_logical_inner(unit, ctx, last);
                    return (
                        t2,
                        f1.into_iter().chain(f2.into_iter()).collect(),
                        l,
                    );
                } else if matches!(op, &BinaryOpType::LogOr) {
                    let (t1, f1, _) = lhs.emit_ir_logical_inner(unit, ctx, false);
                    // current bb is rhs entry; continue if lhs is false
                    f1.iter().for_each(|item| item.backpatch(unit, unit.cur_bb));
                    let (t2, f2, l) = rhs.emit_ir_logical_inner(unit, ctx, last);
                    return (
                        t1.into_iter().chain(t2.into_iter()).collect(),
                        f2,
                        l,
                    );
                }
            },
            // LogNot
            // make clippy happy
            AstNodeType::Unit => (),
            _ => ()
        };
        // and everything else
        let val = self.emit_ir_expr(unit, ctx);
        if !last {
            // use placeholder block id
            let br = unit.branch(val, unit.cur_bb, unit.cur_bb).push_only();
            unit.start_new_bb();
            (
                vec![BackPatchItem { branch: br, slot: BackPatchType::BranchSuccess }],
                vec![BackPatchItem { branch: br, slot: BackPatchType::BranchFail }],
                None,
            )
        } else {
            let (last, next) = unit.start_new_bb();
            unit.jump(next).push_to(last);
            (vec![], vec![], Some(val))
        }
    }
}

impl EmitIrLogicalInner for Rc<RefCell<AstNode>> {
    fn emit_ir_logical_inner(&self, unit: &mut TransUnit, ctx: &mut AstContext, last: bool) -> (Vec<BackPatchItem>, Vec<BackPatchItem>, Option<ValueId>) {
        self.borrow().node.emit_ir_logical_inner(unit, ctx, last)
    }
}

impl EmitIrLogical for AstNodeType {
    fn emit_ir_logical(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> (Vec<BackPatchItem>, Vec<BackPatchItem>) {
        let (t, f, _) = self.emit_ir_logical_inner(unit, ctx, false);
        (t, f)
    }

    fn emit_ir_logical_as_value(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId {
        match self {
            AstNodeType::BinaryOp(BinaryOp { op, .. }) => {
                if matches!(op, &BinaryOpType::LogAnd | &BinaryOpType::LogOr) {
                    let (t1, f1, last) = self.emit_ir_logical_inner(unit, ctx, true);
                    let t = unit.const_i1(true);
                    let f = unit.const_i1(false);
                    let mut args = vec![];
                    // link all true and false branch to this bb
                    t1.iter().for_each(|item| {
                        item.backpatch(unit, unit.cur_bb);
                        let bb = unit.inst_bb.get(&item.branch).unwrap();
                        args.push((t, *bb));
                    });
                    f1.iter().for_each(|item| {
                        item.backpatch(unit, unit.cur_bb);
                        let bb = unit.inst_bb.get(&item.branch).unwrap();
                        args.push((f, *bb));
                    });
                    let bb_last = unit.inst_bb.get(&last.unwrap()).unwrap();
                    args.push((last.unwrap(), *bb_last));
                    unit.phi(args).push()
                } else {
                    panic!("not a logical and/or expression")
                }
            },
            _ => panic!("not a logical and/or expression")
        }
    }
}

impl EmitIrLogical for Rc<RefCell<AstNode>> {
    fn emit_ir_logical(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> (Vec<BackPatchItem>, Vec<BackPatchItem>) {
        self.borrow().node.emit_ir_logical(unit, ctx)
    }

    fn emit_ir_logical_as_value(&self, unit: &mut TransUnit, ctx: &mut AstContext) -> ValueId {
        self.borrow().node.emit_ir_logical_as_value(unit, ctx)
    }
}