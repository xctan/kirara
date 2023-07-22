use std::{rc::Rc, cell::RefCell};

use crate::{ast::*, ctype::{Type, TypePtrCompare, TypePtrHelper}};

use super::AstTransformPass;

pub struct TypeCheckPass;

impl AstTransformPass for TypeCheckPass {
    fn apply(self, tree: &mut AstFuncData) {
        ast_type_check(tree.body.clone());
    }
}

// type check and fix
pub fn ast_type_check(tree: Rc<RefCell<AstNode>>) {
    let tree0 = tree.borrow();
    let new_type: Rc<Type> = match tree0.node.clone() {
        AstNodeType::I1Number(_) => Type::const_of(Type::i1_type()),
        AstNodeType::I32Number(_) => Type::const_of(Type::i32_type()),
        AstNodeType::I64Number(_) => Type::const_of(Type::i64_type()),
        AstNodeType::Variable(id) => get_object(id).unwrap().ty.clone(),
        AstNodeType::Convert(Convert { from, to }) => {
            ast_type_check(from);
            to
        },
        AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op: BinaryOpType::Index }) => {
            ast_type_check(lhs.clone());
            ast_type_check(rhs.clone());
            lhs.borrow().ty.clone().unwrap().base_type()
        }
        AstNodeType::BinaryOp(BinaryOp { lhs, rhs, op }) => {
            ast_type_check(lhs.clone());
            ast_type_check(rhs.clone());
            let common_ty = match op {
                BinaryOpType::LogAnd | BinaryOpType::LogOr => Type::i1_type(),
                _ => Type::get_common_type(
                    lhs.borrow().ty.clone().unwrap().clone(),
                    rhs.borrow().ty.clone().unwrap().clone(),
                )
            };
            let lhs_new = ast_gen_convert(lhs.clone(), common_ty.clone());
            let mut lhs_mut = lhs.borrow_mut();
            lhs_mut.node = lhs_new;
            lhs_mut.ty = Some(common_ty.clone());
            drop(lhs_mut);
            let rhs_new = ast_gen_convert(rhs.clone(), common_ty.clone());
            let mut rhs_mut = rhs.borrow_mut();
            rhs_mut.node = rhs_new;
            rhs_mut.ty = Some(common_ty.clone());
            drop(rhs_mut);
            match op {
                BinaryOpType::Add => common_ty,
                BinaryOpType::Sub => common_ty,
                BinaryOpType::Mul => common_ty,
                BinaryOpType::Div => common_ty,
                BinaryOpType::Mod => common_ty,
                BinaryOpType::Ne => Type::i1_type(),
                BinaryOpType::Eq => Type::i1_type(),
                BinaryOpType::Lt => Type::i1_type(),
                BinaryOpType::Le => Type::i1_type(),
                BinaryOpType::Gt => Type::i1_type(),
                BinaryOpType::Ge => Type::i1_type(),
                BinaryOpType::Assign => {
                    if lhs.borrow().ty.clone().unwrap().is_same_as(&rhs.borrow().ty.clone().unwrap().get_nocv()) ||
                        lhs.borrow().ty.clone().unwrap().is_same_as(&rhs.borrow().ty.clone().unwrap())
                    {
                        lhs.borrow().ty.clone().unwrap().clone()
                    } else {
                        panic!("type mismatch: left {}, right {}", lhs.borrow().ty.clone().unwrap(), rhs.borrow().ty.clone().unwrap());
                    }
                },
                BinaryOpType::LogAnd => Type::i1_type(),
                BinaryOpType::LogOr => Type::i1_type(),
                _ => unreachable!(),
            }
        },
        AstNodeType::FunCall(funcall) => {
            ast_type_check(funcall.func.clone());
            let obj = funcall.func.clone();
            let func = obj.borrow().clone().ty.unwrap().as_function();
            if func.params.len() == 0 {
                // variadic function in C, skip type check
            } else if func.params.len() == 1 && func.params[0].1.is_void() {
                // no argument
                assert!(funcall.args.len() == 0);
            } else {
                assert!(funcall.args.len() == func.params.len());
                for (i, arg) in funcall.args.iter().enumerate() {
                    ast_type_check(arg.clone());
                    let arg_new = ast_gen_convert(arg.clone(), func.params[i].1.clone());
                    let mut arg_mut = arg.borrow_mut();
                    arg_mut.node = arg_new;
                    arg_mut.ty = Some(func.params[i].1.clone());
                }
            }

            func.ret_type.clone()
        }
        AstNodeType::Return(expr) => {
            ast_type_check(expr);
            Type::void_type()
        },
        AstNodeType::ExprStmt(expr) => {
            ast_type_check(expr);
            Type::void_type()
        },
        AstNodeType::Unit => Type::void_type(),
        AstNodeType::Block(v) => {
            for expr in v {
                ast_type_check(expr.clone());
            }
            Type::void_type()
        },
        AstNodeType::IfStmt(ifs) => {
            ast_type_check(ifs.cond.clone());
            let cond_new = ast_gen_convert(ifs.cond.clone(), Type::i1_type());
            let mut cond_mut = ifs.cond.borrow_mut();
            cond_mut.node = cond_new;
            cond_mut.ty = Some(Type::i1_type());
            ast_type_check(ifs.then.clone());
            if !ifs.els.borrow().is_unit() {
                ast_type_check(ifs.els);
            }
            Type::void_type()
        },
        AstNodeType::WhileStmt(whiles) => {
            ast_type_check(whiles.cond.clone());
            let cond_new = ast_gen_convert(whiles.cond.clone(), Type::i1_type());
            let mut cond_mut = whiles.cond.borrow_mut();
            cond_mut.node = cond_new;
            cond_mut.ty = Some(Type::i1_type());
            ast_type_check(whiles.body.clone());
            Type::void_type()
        },
        AstNodeType::LabelStmt(label) => {
            ast_type_check(label.body);
            Type::void_type()
        },
        AstNodeType::GotoStmt(_) => Type::void_type(),
    };
    drop(tree0);

    (*tree).borrow_mut().ty = Some(new_type);
}

fn ast_gen_convert(from: Rc<RefCell<AstNode>>, to: Rc<Type>) -> AstNodeType {
    if from.borrow().ty.clone().unwrap().is_const() {
        return from.borrow().node.clone();
    }

    if from.borrow().ty.clone().unwrap().is_same_as(&to) {
        return from.borrow().node.clone();
    }

    let from = Rc::new(RefCell::new((*from.borrow()).clone()));
    AstNode::convert(from, to, 0..0).borrow().node.clone()
}