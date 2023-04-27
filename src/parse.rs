use nom::{
    IResult,
    combinator::map,
    sequence::tuple,
    InputLength,
};

use std::rc::Rc;

use crate::{
    token::TokenSpan,
    ast::{AstNode, AstNodeType}
};

macro_rules! ttag {
    (IntCn) => {
        nom::bytes::complete::tag(
            $crate::token::TokenType::IntegerConst
        )
    };
    (K ( $t:literal )) => {
        nom::bytes::complete::tag(
            $crate::token::Token($t, $crate::token::TokenType::Keyword)
        )
    };
    (P ( $t:literal )) => {
        nom::bytes::complete::tag(
            $crate::token::Token($t, $crate::token::TokenType::Punctuation)
        )
    };
}

fn number_constant(cursor: TokenSpan) -> IResult<TokenSpan, Rc<AstNode>> {
    map(
        ttag!(IntCn),
        |token| Rc::new(AstNode {
            node: AstNodeType::Number,
            token,
        })
    )(cursor)
}

fn expression(cursor: TokenSpan) -> IResult<TokenSpan, Rc<AstNode>> {
    number_constant(cursor)
}

fn return_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<AstNode>> {
    map(
        tuple((
            ttag!(K("return")),
            expression,
            ttag!(P(";")))),
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