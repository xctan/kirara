use nom::{
    IResult,
    combinator::map,
    sequence::tuple,
    InputLength,
    multi::many0,
};

use std::{rc::Rc, cell::RefCell};

use crate::{
    token::TokenSpan,
    ast::{AstNode, AstNodeType, BinaryOp, BinaryOpType},
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

fn number_constant(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        ttag!(IntCn),
        |token| Rc::new(RefCell::new(AstNode {
            node: AstNodeType::Number,
            token,
        }))
    )(cursor)
}

fn primary(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    number_constant(cursor)
}

fn expression(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // primary ( '+' primary )*
    map(
        tuple((
            primary,
            many0(
                tuple((
                    ttag!(P("+")),
                    primary))))),
        |(first, others)| {
            let mut node = first;
            for (sign, other) in others {
                let length =
                    node.borrow().token.input_len() + sign.input_len() + other.borrow().token.input_len();
                match sign.0[0].0 {
                    "+" => {
                        node = Rc::new(RefCell::new(AstNode {
                            node: AstNodeType::BinaryOp(BinaryOp {
                                lhs: node,
                                rhs: other,
                                op: BinaryOpType::Add,
                            }),
                            token: TokenSpan(cursor.0.split_at(length).0),
                        }));
                    },
                    _ => unreachable!(),
                }
            }
            node
        }
    )(cursor)
}

fn return_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        tuple((
            ttag!(K("return")),
            expression,
            ttag!(P(";")))),
        |(_, expr, _)| Rc::new(RefCell::new(AstNode {
            node: AstNodeType::Return(expr.clone()),
            token: TokenSpan(cursor.0.split_at(1 + expr.borrow().token.input_len() + 1).0),
        }))
    )(cursor)
}

pub fn parse<'a, T>(curosr: T) -> IResult<TokenSpan<'a>, Rc<RefCell<AstNode<'a>>>>
where T: Into<TokenSpan<'a>>
{
    return_statement(curosr.into())
}