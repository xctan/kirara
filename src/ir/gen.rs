use std::{cell::RefCell, rc::Rc};

use super::{
    builder::{BackPatchItem, BackPatchType, LocalInstExt, TransUnit, IrFuncBuilder},
    value::ValueId,
};
use crate::{
    ast::*,
    ctype::{BinaryOpType, Type, TypePtrHelper, TypeKind},
};

pub trait EmitIr {
    fn emit_ir(&self, unit: &mut IrFuncBuilder, ctx: &mut AstContext);
}

impl AstContext {
    pub fn emit_ir(&mut self, unit: &mut TransUnit) {
        for var in self.globals.clone() {
            let var = self.get_object(var).unwrap();
            match var.data.clone() {
                AstObjectType::Func(ref func) => {
                    let name = var.name.clone();
                    let mut builder = unit.builder(var.ty.clone());

                    let ty = var.ty.get();
                    let func_ty = ty.as_function();
                    let params: Vec<_> = func_ty.params
                        .iter()
                        .map(|(_, ty)| builder.param(ty.clone()))
                        .collect();

                    builder.start();
                    
                    for v in &func.locals {
                        let obj = self.get_object_mut(*v).unwrap();
                        let id = builder.alloca(obj.ty.clone()).push();
                        obj.ir_value = Some(id);
                    }
                    func.params.iter()
                        .zip(params)
                        .for_each(|(v, id)| {
                            let obj = self.get_object_mut(*v).unwrap();
                            builder.store(id, obj.ir_value.unwrap()).push();
                        });

                    func.body.emit_ir(&mut builder, self);

                    let jumps = builder.jumps.clone();
                    jumps.iter().for_each(|(j, target)| {
                        let label = builder.labels.get(target).unwrap().clone();
                        j.backpatch(&mut builder, label);
                    });

                    // return value
                    match func.ret_var {
                        Some(ret) => {
                            let obj = self.get_object(ret).unwrap();
                            let id = builder.load(obj.ir_value.unwrap()).push();
                            builder.ret(Some(id)).push();
                        }
                        None => {
                            builder.ret(None).push();
                        }
                    }

                    let func = builder.finish();
                    unit.funcs.insert(name, func);
                }
                AstObjectType::Var => {
                    todo!()
                }
            }
        }
    }
}

impl EmitIr for AstNode {
    fn emit_ir(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) {
        match &self.node {
            AstNodeType::ExprStmt(expr) => {
                expr.emit_ir_expr(builder, ctx);
            }
            AstNodeType::Return(expr) => {
                // current implementation is not correct for multiple returns
                let expr = expr.emit_ir_expr(builder, ctx);
                builder.ret(Some(expr)).push();
            }
            AstNodeType::Block(stmts) => {
                for stmt in stmts {
                    stmt.emit_ir(builder, ctx);
                }
            }
            AstNodeType::Unit => (),
            AstNodeType::IfStmt(ifs) => {
                let (tl, fl) = ifs.cond.emit_ir_logical(builder, ctx);
                tl.iter().for_each(|item| item.backpatch(builder, builder.cur_bb()));
                ifs.then.emit_ir(builder, ctx);
                let (succ_last, fail) = builder.start_new_bb();
                fl.iter().for_each(|item| item.backpatch(builder, fail));
                ifs.els.emit_ir(builder, ctx);
                let (fail_last, finally) = builder.start_new_bb();
                builder.jump(finally).push_to(succ_last);
                builder.jump(finally).push_to(fail_last);
            }
            AstNodeType::WhileStmt(whiles) => {
                let (root, test) = builder.start_new_bb();
                builder.jump(test).push_to(root);
                if !matches!(whiles.cond.borrow().node, AstNodeType::I1Number(true)) {
                    let (tl, fl) = whiles.cond.emit_ir_logical(builder, ctx);
                    tl.iter().for_each(|item| item.backpatch(builder, builder.cur_bb()));
                    whiles.body.emit_ir(builder, ctx);
                    let (body_last, fail) = builder.start_new_bb();
                    builder.jump(test).push_to(body_last);
                    fl.iter().for_each(|item| item.backpatch(builder, fail));
                } else {
                    whiles.body.emit_ir(builder, ctx);
                    let (body_last, _) = builder.start_new_bb();
                    builder.jump(test).push_to(body_last);
                }
            }
            AstNodeType::LabelStmt(labeled) => {
                let (old, new) = builder.start_new_bb();
                builder.jump(new).push_to(old);
                labeled.body.emit_ir(builder, ctx);
                builder.labels.insert(labeled.label.clone(), new);
            }
            AstNodeType::GotoStmt(goto) => {
                let j = builder.jump(builder.cur_bb()).push_only();
                builder.jumps.push((
                    BackPatchItem {
                        branch: j,
                        slot: BackPatchType::Jump,
                    },
                    goto.label.clone(),
                ));
                builder.start_new_bb();
            }
            _ => unimplemented!(),
        }
    }
}

impl EmitIr for Rc<RefCell<AstNode>> {
    fn emit_ir(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) {
        self.borrow().emit_ir(builder, ctx);
    }
}

trait EmitIrExpr {
    fn emit_ir_expr(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId;
}

impl EmitIrExpr for AstNodeType {
    fn emit_ir_expr(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId {
        match self {
            AstNodeType::I1Number(num) => builder.const_i1(*num),
            AstNodeType::I32Number(num) => builder.const_i32(*num),
            AstNodeType::Convert(Convert { from, to }) => {
                let from_id = from.emit_ir_expr(builder, ctx);
                let to = to.upgrade().unwrap();
                match (&from.borrow().ty.get().kind, &to.kind) {
                    (TypeKind::I32, TypeKind::I1) => {
                        let zero = builder.const_i32(0);
                        builder.binary(BinaryOpType::Ne, from_id, zero).push()
                    }
                    (TypeKind::I1, TypeKind::I32) => builder.zext(from_id, Type::i32_type()).push(),
                    _ => unimplemented!(),
                }
            }
            AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }) => {
                if matches!(op, &BinaryOpType::Assign) {
                    let lhs = lhs.emit_ir_lvalue(builder, ctx);
                    let rhs = rhs.emit_ir_expr(builder, ctx);
                    builder.store(rhs, lhs).push();
                    rhs
                } else if matches!(op, &BinaryOpType::LogAnd | &BinaryOpType::LogOr) {
                    self.emit_ir_logical_as_value(builder, ctx)
                } else {
                    let lhs = lhs.emit_ir_expr(builder, ctx);
                    let rhs = rhs.emit_ir_expr(builder, ctx);
                    builder.binary(*op, lhs, rhs).push()
                }
            }
            AstNodeType::Variable(var) => {
                let var = ctx.get_object(*var).unwrap();
                let val = var.ir_value.unwrap();
                builder.load(val).push()
            }
            _ => unimplemented!(),
        }
    }
}

impl EmitIrExpr for Rc<RefCell<AstNode>> {
    fn emit_ir_expr(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId {
        self.borrow().node.emit_ir_expr(builder, ctx)
    }
}

trait EmitIrLValue {
    fn emit_ir_lvalue(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId;
}

impl EmitIrLValue for AstNodeType {
    fn emit_ir_lvalue(&self, _builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId {
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
    fn emit_ir_lvalue(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId {
        self.borrow().node.emit_ir_lvalue(builder, ctx)
    }
}

/// Emit IR for logical expression in control flow representation
trait EmitIrLogical {
    /// Get (truelist, falselist) of logical expression, and backpatch later
    fn emit_ir_logical(
        &self,
        builder: &mut IrFuncBuilder,
        ctx: &mut AstContext,
    ) -> (Vec<BackPatchItem>, Vec<BackPatchItem>);

    /// Get value of **logical and/or** expression, and use it as value
    fn emit_ir_logical_as_value(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId;
}

trait EmitIrLogicalInner {
    fn emit_ir_logical_inner(
        &self,
        builder: &mut IrFuncBuilder,
        ctx: &mut AstContext,
        last: bool,
    ) -> (Vec<BackPatchItem>, Vec<BackPatchItem>, Option<ValueId>);
}

impl EmitIrLogicalInner for AstNodeType {
    fn emit_ir_logical_inner(
        &self,
        builder: &mut IrFuncBuilder,
        ctx: &mut AstContext,
        last: bool,
    ) -> (Vec<BackPatchItem>, Vec<BackPatchItem>, Option<ValueId>) {
        match self {
            AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }) => {
                if matches!(op, &BinaryOpType::LogAnd) {
                    let (t1, f1, _) = lhs.emit_ir_logical_inner(builder, ctx, false);
                    // current bb is rhs entry; continue if lhs is true
                    t1.iter().for_each(|item| item.backpatch(builder, builder.cur_bb()));
                    let (t2, f2, l) = rhs.emit_ir_logical_inner(builder, ctx, last);
                    return (t2, f1.into_iter().chain(f2.into_iter()).collect(), l);
                } else if matches!(op, &BinaryOpType::LogOr) {
                    let (t1, f1, _) = lhs.emit_ir_logical_inner(builder, ctx, false);
                    // current bb is rhs entry; continue if lhs is false
                    f1.iter().for_each(|item| item.backpatch(builder, builder.cur_bb()));
                    let (t2, f2, l) = rhs.emit_ir_logical_inner(builder, ctx, last);
                    return (t1.into_iter().chain(t2.into_iter()).collect(), f2, l);
                }
            }
            // LogNot
            // make clippy happy
            AstNodeType::Unit => (),
            _ => (),
        };
        // and everything else
        let val = self.emit_ir_expr(builder, ctx);
        if !last {
            // use placeholder block id
            let br = builder.branch(val, builder.cur_bb(), builder.cur_bb()).push_only();
            builder.start_new_bb();
            (
                vec![BackPatchItem {
                    branch: br,
                    slot: BackPatchType::BranchSuccess,
                }],
                vec![BackPatchItem {
                    branch: br,
                    slot: BackPatchType::BranchFail,
                }],
                None,
            )
        } else {
            let (last, next) = builder.start_new_bb();
            builder.jump(next).push_to(last);
            (vec![], vec![], Some(val))
        }
    }
}

impl EmitIrLogicalInner for Rc<RefCell<AstNode>> {
    fn emit_ir_logical_inner(
        &self,
        builder: &mut IrFuncBuilder,
        ctx: &mut AstContext,
        last: bool,
    ) -> (Vec<BackPatchItem>, Vec<BackPatchItem>, Option<ValueId>) {
        self.borrow().node.emit_ir_logical_inner(builder, ctx, last)
    }
}

impl EmitIrLogical for AstNodeType {
    fn emit_ir_logical(
        &self,
        builder: &mut IrFuncBuilder,
        ctx: &mut AstContext,
    ) -> (Vec<BackPatchItem>, Vec<BackPatchItem>) {
        let (t, f, _) = self.emit_ir_logical_inner(builder, ctx, false);
        (t, f)
    }

    fn emit_ir_logical_as_value(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId {
        match self {
            AstNodeType::BinaryOp(BinaryOp { op, .. }) => {
                if matches!(op, &BinaryOpType::LogAnd | &BinaryOpType::LogOr) {
                    let (t1, f1, last) = self.emit_ir_logical_inner(builder, ctx, true);
                    let t = builder.const_i1(true);
                    let f = builder.const_i1(false);
                    let mut args = vec![];
                    // link all true and false branch to this bb
                    t1.iter().for_each(|item| {
                        item.backpatch(builder, builder.cur_bb());
                        let bb = builder.unit.inst_bb.get(&item.branch).unwrap();
                        args.push((t, *bb));
                    });
                    f1.iter().for_each(|item| {
                        item.backpatch(builder, builder.cur_bb());
                        let bb = builder.unit.inst_bb.get(&item.branch).unwrap();
                        args.push((f, *bb));
                    });
                    let bb_last = builder.unit.inst_bb.get(&last.unwrap()).unwrap();
                    args.push((last.unwrap(), *bb_last));
                    builder.phi(args).push()
                } else {
                    panic!("not a logical and/or expression")
                }
            }
            _ => panic!("not a logical and/or expression"),
        }
    }
}

impl EmitIrLogical for Rc<RefCell<AstNode>> {
    fn emit_ir_logical(
        &self,
        builder: &mut IrFuncBuilder,
        ctx: &mut AstContext,
    ) -> (Vec<BackPatchItem>, Vec<BackPatchItem>) {
        self.borrow().node.emit_ir_logical(builder, ctx)
    }

    fn emit_ir_logical_as_value(&self, builder: &mut IrFuncBuilder, ctx: &mut AstContext) -> ValueId {
        self.borrow().node.emit_ir_logical_as_value(builder, ctx)
    }
}
