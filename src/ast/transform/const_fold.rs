use std::{rc::Rc, cell::RefCell};

use crate::{ast::*, ctype::TypeKind};

use super::AstTransformPass;

pub struct ConstFoldPass;

impl AstTransformPass for ConstFoldPass {
    fn apply(self, tree: &mut AstFuncData) {
        ast_const_fold(tree.body.clone());
    }
}

/// do constant folding on AST
pub fn ast_const_fold(tree: Rc<RefCell<AstNode>>) {
    let tree0 = tree.borrow();
    let new_node: Option<AstNodeType> = match tree0.node.clone() {
        AstNodeType::I1Number(_) => None,
        AstNodeType::I32Number(_) => None,
        AstNodeType::I64Number(_) => None,
        AstNodeType::Variable(var) => {
            let variable = get_object(var).unwrap();
            let var_ty = variable.ty.clone();
            if let TypeKind::Const(_ty) = var_ty.kind.clone() {
                // sysy extension: const variable can be evaluated at compile time
                let mut init = match variable.data.clone() {
                    AstObjectType::Var(init) => init,
                    _ => unreachable!(),
                };
                init.eval();
                match init.data {
                    InitData::ScalarI32(i) => Some(AstNodeType::I32Number(i)),
                    _ => None,
                }
            } else {
                None
            }
        },
        AstNodeType::Convert(Convert{from, ref to}) => {
            assert!(tree.as_ptr() != from.as_ptr());
            ast_const_fold(from.clone());
            match (from.borrow().node.clone(), &to.kind) {
                (AstNodeType::I1Number(num), &TypeKind::I32) => Some(AstNodeType::I32Number(num as i32)),
                (AstNodeType::I32Number(num), &TypeKind::I32) => Some(AstNodeType::I32Number(num)),
                (AstNodeType::I32Number(num), &TypeKind::I1) => Some(AstNodeType::I1Number(num != 0)),
                (AstNodeType::I1Number(num), &TypeKind::I1) => Some(AstNodeType::I1Number(num)),
                _ => None,
            }
        }
        AstNodeType::BinaryOp(BinaryOp{lhs, rhs, op}) => {
            use {
                AstNodeType::I32Number as Int,
                AstNodeType::I1Number as Bool,
            };
            ast_const_fold(lhs.clone());
            ast_const_fold(rhs.clone());
            match (lhs.borrow().node.clone(), rhs.borrow().node.clone()) {
                (AstNodeType::I32Number(lhs), AstNodeType::I32Number(rhs)) => {
                    let num = match op {
                        BinaryOpType::Add => Int(lhs + rhs),
                        BinaryOpType::Sub => Int(lhs - rhs),
                        BinaryOpType::Mul => Int(lhs * rhs),
                        BinaryOpType::Div => Int(lhs / rhs),
                        BinaryOpType::Mod => Int(lhs % rhs),
                        BinaryOpType::Ne => Bool(lhs != rhs),
                        BinaryOpType::Eq => Bool(lhs == rhs),
                        BinaryOpType::Lt => Bool(lhs < rhs),
                        BinaryOpType::Le => Bool(lhs <= rhs),
                        BinaryOpType::Gt => Bool(lhs > rhs),
                        BinaryOpType::Ge => Bool(lhs >= rhs),
                        _ => unreachable!("op {:?}, lhs {:?}, rhs {:?}", op, lhs, rhs),
                    };
                    Some(num)
                },
                (AstNodeType::I1Number(lhs), AstNodeType::I1Number(rhs)) => {
                    let num = match op {
                        BinaryOpType::LogAnd => Bool(lhs && rhs),
                        BinaryOpType::LogOr => Bool(lhs || rhs),
                        _ => unreachable!(),
                    };
                    Some(num)
                }
                _ => None,
            }
        },
        AstNodeType::FunCall(funcall) => {
            for arg in funcall.args {
                ast_const_fold(arg);
            }
            None
        },
        AstNodeType::Return(expr) => {
            ast_const_fold(expr);
            None
        },
        AstNodeType::ExprStmt(expr) => {
            ast_const_fold(expr);
            None
        },
        AstNodeType::Unit => None,
        AstNodeType::Block(v) => {
            for expr in v {
                ast_const_fold(expr.clone());
            }
            None
        },
        AstNodeType::IfStmt(ifs) => {
            ast_const_fold(ifs.cond.clone());
            ast_const_fold(ifs.then.clone());
            if !ifs.els.borrow().is_unit() {
                ast_const_fold(ifs.els);
            }
            None
        },
        AstNodeType::WhileStmt(ref whiles) => {
            ast_const_fold(whiles.cond.clone());
            ast_const_fold(whiles.body.clone());
            None
        },
        AstNodeType::LabelStmt(ref label) => {
            ast_const_fold(label.body.clone());
            None
        },
        AstNodeType::GotoStmt(_) => None,
    };
    drop(tree0);

    if let Some(new_node) = new_node {
        (*tree).borrow_mut().node = new_node;
    }
}

pub fn eval(tree: Rc<RefCell<AstNode>>) -> Option<usize> {
    let tree0 = tree.borrow();
    match tree0.node.clone() {
        AstNodeType::I1Number(num) => Some(num as usize),
        AstNodeType::I32Number(num) => Some(num as usize),
        AstNodeType::I64Number(num) => Some(num as usize),
        AstNodeType::Variable(var) => {
            let variable = get_object(var).unwrap();
            let var_ty = variable.ty.clone();
            if let TypeKind::Const(_ty) = var_ty.kind.clone() {
                // sysy extension: const variable can be evaluated at compile time
                let mut init = match variable.data.clone() {
                    AstObjectType::Var(init) => init,
                    _ => unreachable!(),
                };
                init.eval();
                match init.data {
                    InitData::ScalarI32(i) => Some(i as usize),
                    _ => None,
                }
            } else {
                None
            }
        },
        AstNodeType::Convert(Convert{from, ..}) => {
            assert!(tree.as_ptr() != from.as_ptr());
            eval(from.clone())
        },
        AstNodeType::BinaryOp(BinaryOp{lhs, rhs, op}) => {
            let lhs = eval(lhs.clone())?;
            let rhs = eval(rhs.clone())?;
            let num = match op {
                BinaryOpType::Add => lhs + rhs,
                BinaryOpType::Sub => lhs - rhs,
                BinaryOpType::Mul => lhs * rhs,
                BinaryOpType::Div => lhs / rhs,
                BinaryOpType::Mod => lhs % rhs,
                BinaryOpType::Ne => (lhs != rhs) as usize,
                BinaryOpType::Eq => (lhs == rhs) as usize,
                BinaryOpType::Lt => (lhs < rhs) as usize,
                BinaryOpType::Le => (lhs <= rhs) as usize,
                BinaryOpType::Gt => (lhs > rhs) as usize,
                BinaryOpType::Ge => (lhs >= rhs) as usize,
                BinaryOpType::LogAnd => (lhs != 0 && rhs != 0) as usize,
                BinaryOpType::LogOr => (lhs != 0 || rhs != 0) as usize,
                _ => unreachable!(),
            };
            Some(num)
        },
        _ => None
    }
}