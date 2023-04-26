use nom::{
    IResult, 
    combinator::map, 
    sequence::tuple, 
    bytes::complete::tag, 
    InputLength
};

use std::rc::Rc;

use crate::{
    token::{TokenSpan, TokenType, Token}, 
    ast::{AstNode, AstNodeType}
};

fn expression(cursor: TokenSpan) -> IResult<TokenSpan, Rc<AstNode>> {
    // as for now, just a most simplified version: number
    map(
        tag(TokenType::IntegerConst),
        |token| Rc::new(AstNode {
            node: AstNodeType::Number,
            token,
        })
    )(cursor)
}

fn return_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<AstNode>> {
    map(
        tuple((
            tag(Token("return", TokenType::Keyword)),
            expression,
            tag(Token(";", TokenType::Punctuation)),
        )),
        |(_, expr, _)| Rc::new(AstNode {
            node: AstNodeType::Return(expr.clone()),
            token: TokenSpan(cursor.0.split_at(1 + expr.token.input_len() + 1).0),
        })
    )(cursor)
}

pub fn parse<'a, T>(curosr: T) -> IResult<TokenSpan<'a>, Rc<AstNode<'a>>>
where T: Into<TokenSpan<'a>>
{
    return_statement(curosr.into())
}