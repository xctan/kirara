use std::{cell::RefCell, rc::Rc};

use crate::ctype::{Type, BinaryOpType};

use super::{AstNode, ObjectId, AstNodeRewrite, AstFuncData};

pub trait AstTransformPass {
    fn apply(self, tree: &mut AstFuncData);
}

pub(in crate::ast) mod type_check;
pub(in crate::ast) mod const_fold;
mod dead_code;
mod multi_return;

pub use const_fold::eval;

pub struct AstPassManager;

impl AstPassManager {
    pub fn apply_passes(self, tree: &mut AstFuncData) {
        type_check::TypeCheckPass.apply(tree);
        const_fold::ConstFoldPass.apply(tree);
        dead_code::DeadCodeRemovalPass.apply(tree);
        multi_return::MergeMultiReturnPass.apply(tree);
        type_check::TypeCheckPass.apply(tree);
    }
}

pub trait AstRewriteVisitor {
    // basic elements
    fn visit_i1_number(&mut self, _num: bool) -> Option<Rc<RefCell<AstNode>>> { None }
    fn visit_i32_number(&mut self, _num: i32) -> Option<Rc<RefCell<AstNode>>> { None }
    fn visit_i64_number(&mut self, _num: i64) -> Option<Rc<RefCell<AstNode>>> { None }
    fn visit_variable(&mut self, _id: ObjectId) -> Option<Rc<RefCell<AstNode>>> { None }
    // expression constructs
    fn visit_convert(&mut self, _from: Rc<RefCell<AstNode>>, _to: Rc<Type>) -> Option<Rc<RefCell<AstNode>>> { 
        self.rewrite(_from);
        None
     }
    fn visit_binary_op(&mut self, _lhs: Rc<RefCell<AstNode>>, _rhs: Rc<RefCell<AstNode>>, _op: BinaryOpType) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_lhs);
        self.rewrite(_rhs);
        None
    }
    // fn visit_unary_op(&mut self, _rhs: Rc<RefCell<AstNode>>, _op: BinaryOpType) -> Option<Rc<RefCell<AstNode>>> { None }
    // control flow constructs
    fn visit_return(&mut self, _expr: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_expr);
        None
    }
    fn visit_expr_stmt(&mut self, _expr: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_expr);
        None
    }
    fn visit_unit(&mut self) -> Option<Rc<RefCell<AstNode>>> { None }
    fn visit_block(&mut self, _stmts: Vec<Rc<RefCell<AstNode>>>) -> Option<Rc<RefCell<AstNode>>> {
        for stmt in _stmts {
            self.rewrite(stmt);
        }
        None
    }
    fn visit_if(&mut self, _cond: Rc<RefCell<AstNode>>, _then: Rc<RefCell<AstNode>>, _els: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_cond);
        self.rewrite(_then);
        self.rewrite(_els);
        None
    }
    fn visit_while(&mut self, _cond: Rc<RefCell<AstNode>>, _body: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_cond);
        self.rewrite(_body);
        None
    }
    fn visit_label(&mut self, _label: String, _stmt: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_stmt);
        None
    }
    fn visit_goto(&mut self, _label: String) -> Option<Rc<RefCell<AstNode>>> { None }
    fn visit_funcall(&mut self, _func: Rc<RefCell<AstNode>>, _args: Vec<Rc<RefCell<AstNode>>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_func);
        for arg in _args {
            self.rewrite(arg);
        }
        None
    }

    fn before_statement(&mut self, _stmt: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> { None }

    fn rewrite(&mut self, tree: Rc<RefCell<AstNode>>) {
        let new_node = match tree.borrow().node.clone() {
            crate::ast::AstNodeType::ExprStmt(_) |
            crate::ast::AstNodeType::Block(_) |
            crate::ast::AstNodeType::IfStmt(_) |
            crate::ast::AstNodeType::WhileStmt(_) |
            crate::ast::AstNodeType::LabelStmt(_) |
            crate::ast::AstNodeType::GotoStmt(_) |
            crate::ast::AstNodeType::Return(_) => self.before_statement(tree.clone()),
            _ => None
        };
        if let Some(new_node) = new_node {
            tree.rewrite(new_node);
            // tree is updated, so before_statement hook should be applied again
            self.rewrite(tree);
            return;
        }
        let new_node = match tree.borrow().node.clone() {
            crate::ast::AstNodeType::I1Number(num) => self.visit_i1_number(num),
            crate::ast::AstNodeType::I32Number(num) => self.visit_i32_number(num),
            crate::ast::AstNodeType::I64Number(num) => self.visit_i64_number(num),
            crate::ast::AstNodeType::Variable(id) => self.visit_variable(id),
            crate::ast::AstNodeType::Convert(convert) => self.visit_convert(convert.from, convert.to),
            crate::ast::AstNodeType::FunCall(funcall) => self.visit_funcall(funcall.func, funcall.args),
            crate::ast::AstNodeType::BinaryOp(binary_op) => self.visit_binary_op(binary_op.lhs, binary_op.rhs, binary_op.op),
            // crate::ast::AstNodeType::UnaryOp(unary_op) => self.visit_unary_op(unary_op.rhs, unary_op.op),
            crate::ast::AstNodeType::Return(expr) => self.visit_return(expr),
            crate::ast::AstNodeType::ExprStmt(expr) => self.visit_expr_stmt(expr),
            crate::ast::AstNodeType::Unit => self.visit_unit(),
            crate::ast::AstNodeType::Block(stmts) => self.visit_block(stmts),
            crate::ast::AstNodeType::IfStmt(r#if) => self.visit_if(r#if.cond, r#if.then, r#if.els),
            crate::ast::AstNodeType::WhileStmt(r#while) => self.visit_while(r#while.cond, r#while.body),
            crate::ast::AstNodeType::LabelStmt(label) => self.visit_label(label.label, label.body),
            crate::ast::AstNodeType::GotoStmt(goto) => self.visit_goto(goto.label),
        };
        if let Some(new_node) = new_node {
            tree.rewrite(new_node);
        }
    }
}