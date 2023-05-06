use nom::{
    IResult,
    combinator::{map, opt},
    sequence::tuple,
    InputLength,
    multi::many0,
    branch::alt,
};

use std::{rc::Rc, cell::RefCell, convert::TryInto};

use crate::{
    token::TokenSpan,
    ast::{AstNode, AstNodeType, BinaryOp},
    ctype::BinaryOpType,
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
            node: AstNodeType::I64Number(0),
            token,
        }))
    )(cursor)
}

fn primary(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    number_constant(cursor)
}

fn multiplication(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // primary ( ('*' | '/' | '%') primary )*
    map(
        tuple((
            primary,
            many0(
                tuple((
                    alt((ttag!(P("*")), ttag!(P("/")), ttag!(P("%")))),
                    primary))))),
        |(first, others)| {
            let mut node = first;
            for (sign, other) in others {
                let length =
                    node.borrow().token.input_len() + sign.input_len() + other.borrow().token.input_len();
                let op = match sign.0[0].0 {
                    "*" => BinaryOpType::Mul,
                    "/" => BinaryOpType::Div,
                    "%" => BinaryOpType::Mod,
                    _ => unreachable!(),
                };
                node = Rc::new(RefCell::new(AstNode {
                    node: AstNodeType::BinaryOp(BinaryOp {
                        lhs: node,
                        rhs: other,
                        op,
                    }),
                    token: TokenSpan(cursor.0.split_at(length).0),
                }));
            }
            node
        }
    )(cursor)
}

fn expression(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // multiplication ( ('+' | '-') multiplication )*
    map(
        tuple((
            multiplication,
            many0(
                tuple((
                    alt((ttag!(P("+")), ttag!(P("-")))),
                    multiplication))))),
        |(first, others)| {
            let mut node = first;
            for (sign, other) in others {
                let length =
                    node.borrow().token.input_len() + sign.input_len() + other.borrow().token.input_len();
                let op = match sign.0[0].0 {
                    "+" => BinaryOpType::Add,
                    "-" => BinaryOpType::Sub,
                    _ => unreachable!(),
                };
                node = Rc::new(RefCell::new(AstNode {
                    node: AstNodeType::BinaryOp(BinaryOp {
                        lhs: node,
                        rhs: other,
                        op,
                    }),
                    token: TokenSpan(cursor.0.split_at(length).0),
                }));
            }
            node
        }
    )(cursor)
}

fn expression_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        tuple((
            opt(expression),
            ttag!(P(";")))),
        |(expr, _)| {
            if let Some(expr) = expr {
                Rc::new(RefCell::new(AstNode {
                    node: AstNodeType::ExprStmt(expr.clone()),
                    token: TokenSpan(cursor.0.split_at(expr.borrow().token.input_len() + 1).0),
                }))
            } else {
                Rc::new(RefCell::new(AstNode { 
                    node: AstNodeType::Unit,
                    token: TokenSpan(cursor.0.split_at(1).0),
                }))
            }
        }
    )(cursor)
}

fn return_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        tuple((
            ttag!(K("return")),
            expression_statement)),
        |(_, expr)| Rc::new(RefCell::new(AstNode {
            node: AstNodeType::Return(expr.clone()),
            token: TokenSpan(cursor.0.split_at(1 + expr.borrow().token.input_len()).0),
        }))
    )(cursor)
}

pub fn parse<'a, T>(curosr: T) -> IResult<TokenSpan<'a>, Rc<RefCell<AstNode<'a>>>>
where T: Into<TokenSpan<'a>>
{
    return_statement(curosr.into())
        .map(|(rest, node)| {
            ast_const_fold(node.clone());
            (rest, node)
        })
}

/// do constant folding on AST
fn ast_const_fold(tree: Rc<RefCell<AstNode<'_>>>) {
    let tree0 = tree.borrow();
    let new_node: Option<AstNodeType> = match tree0.node.clone() {
        AstNodeType::I64Number(_) => {
            // fixme: handle overflow
            let num = TryInto::<i64>::try_into(tree0.token).unwrap();
            Some(AstNodeType::I64Number(num))
        },
        AstNodeType::BinaryOp(BinaryOp{lhs, rhs, op}) => {
            ast_const_fold(lhs.clone());
            ast_const_fold(rhs.clone());
            match (lhs.borrow().node.clone(), rhs.borrow().node.clone()) {
                (AstNodeType::I64Number(lhs), AstNodeType::I64Number(rhs)) => {
                    let num = match op {
                        BinaryOpType::Add => lhs + rhs,
                        BinaryOpType::Sub => lhs - rhs,
                        BinaryOpType::Mul => lhs * rhs,
                        BinaryOpType::Div => lhs / rhs,
                        BinaryOpType::Mod => lhs % rhs,
                    };
                    Some(AstNodeType::I64Number(num))
                },
                _ => None,
            }
        },
        AstNodeType::Return(expr) => {
            ast_const_fold(expr.clone());
            None
        },
        AstNodeType::ExprStmt(expr) => {
            ast_const_fold(expr.clone());
            None
        },
        AstNodeType::Unit => None,
    };
    drop(tree0);

    if let Some(new_node) = new_node {
        tree.borrow_mut().node = new_node;
    }
}