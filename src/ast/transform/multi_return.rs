use std::{rc::Rc, cell::RefCell};

use crate::{
    ast::{ObjectId, AstNode, AstNodeType, AstFuncData, context::new_local_var},
    ctype::{BinaryOpType, TypeKind},
};

use super::{AstRewriteVisitor, AstTransformPass};

/// scan the ast and check if any existing variable can be used to store the return value
struct ReturnScanner {
    pub ret_var: Option<ObjectId>,
    counter: usize,
}

impl ReturnScanner {
    fn new() -> Self {
        Self {
            ret_var: None,
            counter: 0,
        }
    }
}

impl AstRewriteVisitor for ReturnScanner {
    fn visit_return(&mut self, _expr: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_expr.clone());
        self.counter += 1;
        match _expr.borrow().node.clone() {
            AstNodeType::Variable(id) => {
                if self.counter == 1 {
                    self.ret_var = Some(id);
                } else {
                    if let Some(ret_var) = self.ret_var {
                        if ret_var != id {
                            self.ret_var = None;
                        }
                    }
                }
            }
            _ => self.ret_var = None,
        }
        None
    }
}

struct ReturnVariableRewriter {
    ret_var: ObjectId,
}

impl ReturnVariableRewriter {
    fn new(ret_var: ObjectId) -> Self {
        Self {
            ret_var,
        }
    }
}

impl AstRewriteVisitor for ReturnVariableRewriter {
    fn visit_return(&mut self, _expr: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_expr.clone());
        // println!("{:?}", _expr.borrow().node);
        match &_expr.borrow().node {
            AstNodeType::FunCall(call) if call.args.len() <= 8 => {
                // enable sibling call opt
                None
            }
            _ => {
                let var = AstNode::variable(self.ret_var, 0..0);
                let assign = AstNode::binary(var, _expr.clone(), BinaryOpType::Assign, 0..0);
                let stmt = AstNode::expr_stmt(assign, 0..0);
                let goto = AstNode::goto(".exit".to_string(), 0..0);
                let block = AstNode::block(vec![stmt, goto], 0..0);
                Some(block)
            }
        }
    }
}

struct ReturnGotoRewriter;

impl AstRewriteVisitor for ReturnGotoRewriter {
    fn visit_return(&mut self, _expr: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_expr.clone());
        let goto = AstNode::goto(".exit".to_string(), 0..0);
        Some(goto)
    }
}

pub struct MergeMultiReturnPass;

impl AstTransformPass for MergeMultiReturnPass {
    fn apply(self, tree: &mut AstFuncData) {
        let mut scanner = ReturnScanner::new();
        scanner.rewrite(tree.body.clone());
        match scanner.ret_var {
            Some(id) => {
                tree.ret_var = Some(id);
                // just replace all return to goto .exit
                ReturnGotoRewriter.rewrite(tree.body.clone());
            },
            None => {
                let ret_type = tree.func_ty.as_function().ret_type;
                match &ret_type.kind {
                    &TypeKind::Void => {
                        ReturnGotoRewriter.rewrite(tree.body.clone());
                    },
                    _ => {
                        let id = new_local_var(".return", ret_type);
                        tree.ret_var = Some(id);
                        let mut rewriter = ReturnVariableRewriter::new(id);
                        rewriter.rewrite(tree.body.clone());
                    }
                };
            },
        };
        let exit_label = AstNode::label(".exit".to_string(), AstNode::unit(0..0), 0..0);
        let block = AstNode::block(vec![tree.body.clone(), exit_label], 0..0);
        tree.body = block;
    }
}