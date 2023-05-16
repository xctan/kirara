use std::{rc::Rc, cell::RefCell};

use crate::{ast::*, ctype::{Type, TypePtrHelper}};

use super::AstTransformPass;

pub struct ConstFoldPass;

impl AstTransformPass for ConstFoldPass {
    fn apply(self, tree: Rc<RefCell<AstNode>>) {
        ast_const_fold(tree);
    }
}

/// do constant folding on AST
fn ast_const_fold(tree: Rc<RefCell<AstNode>>) {
    let tree0 = tree.borrow();
    let new_node: Option<AstNodeType> = match tree0.node.clone() {
        AstNodeType::I1Number(_) => None,
        AstNodeType::I32Number(_) => None,
        AstNodeType::I64Number(_) => None,
        AstNodeType::Variable(_) => None,
        AstNodeType::Convert(Convert{from, ref to}) => {
            assert!(tree.as_ptr() != from.as_ptr());
            ast_const_fold(from.clone());
            match (from.borrow().node.clone(), (*to.get()).clone()) {
                (AstNodeType::I1Number(num), Type::I32) => Some(AstNodeType::I32Number(num as i32)),
                (AstNodeType::I32Number(num), Type::I32) => Some(AstNodeType::I32Number(num)),
                (AstNodeType::I32Number(num), Type::I1) => Some(AstNodeType::I1Number(num != 0)),
                (AstNodeType::I1Number(num), Type::I1) => Some(AstNodeType::I1Number(num)),
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
                        _ => unreachable!(),
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
