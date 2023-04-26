use std::rc::Rc;

use crate::token::TokenSpan;

#[derive(Debug)]
pub enum AstNodeType<'a> {
    Number,
    Return(Rc<AstNode<'a>>),
}

#[derive(Debug)]
pub struct AstNode<'a> {
    pub node: AstNodeType<'a>,
    pub token: TokenSpan<'a>,
    // ty: Type,
}