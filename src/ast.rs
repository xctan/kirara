use std::{cell::RefCell, rc::Rc};

use crate::token::TokenSpan;

#[derive(Debug)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    pub lhs: Rc<RefCell<AstNode<'a>>>,
    pub rhs: Rc<RefCell<AstNode<'a>>>,
    pub op: BinaryOpType,
}

#[derive(Debug)]
pub enum AstNodeType<'a> {
    Number,
    Return(Rc<RefCell<AstNode<'a>>>),
    BinaryOp(BinaryOp<'a>),
}

#[derive(Debug)]
pub struct AstNode<'a> {
    pub node: AstNodeType<'a>,
    pub token: TokenSpan<'a>,
    // ty: Type,
}