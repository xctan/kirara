use std::{cell::RefCell, rc::Rc};

use crate::{
    token::TokenSpan,
    ctype::BinaryOpType,
};

#[derive(Debug, Clone)]
pub struct BinaryOp<'a> {
    pub lhs: Rc<RefCell<AstNode<'a>>>,
    pub rhs: Rc<RefCell<AstNode<'a>>>,
    pub op: BinaryOpType,
}

#[derive(Debug, Clone)]
pub enum AstNodeType<'a> {
    Unit,
    I64Number(i64),
    BinaryOp(BinaryOp<'a>),
    ExprStmt(Rc<RefCell<AstNode<'a>>>),
    Return(Rc<RefCell<AstNode<'a>>>),
}

#[derive(Debug)]
pub struct AstNode<'a> {
    pub node: AstNodeType<'a>,
    pub token: TokenSpan<'a>,
    // ty: Type,
}