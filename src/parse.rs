use nom::{
    IResult,
    combinator::{map, opt, success},
    sequence::tuple,
    InputLength,
    multi::{many0, many1, many0_count},
    branch::alt,
};

use std::{rc::{Rc, Weak}, cell::RefCell, convert::TryInto, mem::{ManuallyDrop, swap}, borrow::BorrowMut, os::unix::thread};

use crate::{
    token::{TokenSpan, Token, range_between},
    ast::{AstNode, AstNodeType, BinaryOp, AstContext, ObjectId, ScopeVar, AstFuncData, AstObject, AstObjectType},
    ctype::{BinaryOpType, Type},
};

macro_rules! ttag {
    (IntCn) => {
        nom::bytes::complete::tag(
            $crate::token::TokenType::IntegerConst
        )
    };
    (I) => {
        nom::bytes::complete::tag(
            $crate::token::TokenType::Identifier
        )
    };
    (K ( $t:literal )) => {
        nom::bytes::complete::tag(
            $crate::token::Token($t, $crate::token::TokenType::Keyword)
        )
    };
    (P ( $t:literal )) => {
        nom::bytes::complete::tag::<
            $crate::token::Token<'_>,
            $crate::token::TokenSpan<'_>,
            nom::error::Error<$crate::token::TokenSpan<'_>>
        >(
            $crate::token::Token($t, $crate::token::TokenType::Punctuation)
        )
    };
}

fn number_constant(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        ttag!(IntCn),
        |token: TokenSpan<'_>| {
            let num = match TryInto::<i64>::try_into(token) {
                Ok(num) => num,
                Err(_) => panic!("integer constant overflow"),
            };
            Rc::new(RefCell::new(AstNode {
                node: AstNodeType::I64Number(num),
                token: token.as_range(),
            }))
        }
    )(cursor)
}

fn identifier(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        ttag!(I),
        |token: TokenSpan<'_>| {
            let var = find_var(token.as_str());
            match var {
                Some(ScopeVar::Var(var)) => Rc::new(RefCell::new(AstNode {
                    node: AstNodeType::Variable(var),
                    token: token.as_range(),
                })),
                None => panic!("undefined variable: {}", token.as_str()),
            }
        }
    )(cursor)
}

fn primary(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // identifier | number_constant
    alt((
        map(
            tuple((ttag!(P("(")), expression, ttag!(P(")")))),
            |(l, expr, r)| {
                let token = range_between(&l.as_range(), &r.as_range());
                (*expr).borrow_mut().token = token;
                expr
            }),
        identifier,
        number_constant,
    ))(cursor)
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
                    node.borrow().token.len() + sign.input_len() + other.borrow().token.len();
                let op = match sign.0[0].0 {
                    "*" => BinaryOpType::Mul,
                    "/" => BinaryOpType::Div,
                    "%" => BinaryOpType::Mod,
                    _ => unreachable!(),
                };
                let token = range_between(&node.borrow().token, &other.borrow().token);
                node = Rc::new(RefCell::new(AstNode {
                    token,
                    node: AstNodeType::BinaryOp(BinaryOp {
                        lhs: node,
                        rhs: other,
                        op,
                    }),
                }));
            }
            node
        }
    )(cursor)
}

fn addition(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
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
                    node.borrow().token.len() + sign.input_len() + other.borrow().token.len();
                let op = match sign.0[0].0 {
                    "+" => BinaryOpType::Add,
                    "-" => BinaryOpType::Sub,
                    _ => unreachable!(),
                };
                let token = range_between(&node.borrow().token, &other.borrow().token);
                node = Rc::new(RefCell::new(AstNode {
                    node: AstNodeType::BinaryOp(BinaryOp {
                        lhs: node,
                        rhs: other,
                        op,
                    }),
                    token,
                }));
            }
            node
        }
    )(cursor)
}

fn assignment(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // conditional ( '=' assignment )?
    // use addition temporarily
    map(
        tuple((
            addition,
            opt(tuple((
                ttag!(P("=")),
                assignment))))),
        |(first, others)| {
            if let Some((_, other)) = others {
                let token = range_between(&first.borrow().token, &other.borrow().token);
                Rc::new(RefCell::new(AstNode {
                    node: AstNodeType::BinaryOp(BinaryOp {
                        lhs: first,
                        rhs: other,
                        op: BinaryOpType::Assign,
                    }),
                    token,
                }))
            } else {
                first
            }
        }
    )(cursor)
}

fn expression(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // assignment, as for now
    assignment(cursor)
}

fn expression_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        tuple((
            opt(expression),
            ttag!(P(";")))),
        |(expr, semi)| {
            if let Some(expr) = expr {
                let token = range_between(&expr.borrow().token, &semi.as_range());
                Rc::new(RefCell::new(AstNode {
                    node: AstNodeType::ExprStmt(expr),
                    token,
                }))
            } else {
                Rc::new(RefCell::new(AstNode { 
                    node: AstNodeType::Unit,
                    token: semi.as_range(),
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
        |(r, expr)| {
            let token = range_between(&r.as_range(), &expr.borrow().token);
            Rc::new(RefCell::new(AstNode {
                node: AstNodeType::Return(expr),
                token,
            }))
        }
    )(cursor)
}

// declspec contains base type and variable attributes; todo: var_attr
fn declspec(cursor: TokenSpan) -> IResult<TokenSpan, Weak<Type>> {
    map(
        many1(alt((
            ttag!(K("int")),
        ))),
        |decl: Vec<TokenSpan>| {
            const VOID: i32 = 1 << 0;
            const INT: i32 = 1 << 8;
            let mut ty = Type::i32_type();
            let mut counter = 0;

            for d in decl {
                match d.0[0].0 {
                    "void" => counter += VOID,
                    "int" => counter += INT,
                    _ => unreachable!(),
                }

                match counter {
                    VOID => ty = Type::void_type(),
                    INT => ty = Type::i32_type(),
                    _ => panic!("invalid type"),
                }
            }
            
            ty
        }
    )(cursor)
}

fn declarator((cursor, ty): (TokenSpan, Weak<Type>)) -> IResult<TokenSpan, (Weak<Type>, TokenSpan)> {
    // "*"* ("(" declarator ")" | identifier) type_suffix
    let mut ty = ty.clone();

    let (left, _consumed) = many0_count(ttag!(P("*")))(cursor)?;
    // todo: parse pointer

    if let Ok((_cursor, _)) = ttag!(P("("))(left) {
        todo!()
    } else {
        let (cursor, id) = ttag!(I)(left)?;
        // let (cursor, _) = type_suffix(cursor)?;
        Ok((cursor, (ty, id)))
    }
}

fn declaration(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // declspec declarator ("=" initializer)? ("," declarator ("=" initializer)?)* ";"
    let (mut cursor0, ty) = declspec(cursor)?;
    let mut init = vec![];
    let mut count = 0;

    loop {
        if let Ok((cursor1, _)) = ttag!(P(";"))(cursor0) {
            cursor0 = cursor1;
            break;
        }

        if count > 0 {
            let (cursor1, _) = ttag!(P(","))(cursor0)?;
            cursor0 = cursor1;
        }
        count += 1;

        let (cursor1, (ty, id)) = declarator((cursor0, ty.clone()))?;
        cursor0 = cursor1;
        let object_id = new_local_var_with_token(id, ty);

        if let Ok((cursor2, _)) = ttag!(P("="))(cursor0) {
            cursor0 = cursor2;
            
            let (cursor2, expr) = assignment(cursor0)?;
            cursor0 = cursor2;
            let obj = get_object_mut(object_id).unwrap();
            let token = range_between(&obj.token, &expr.borrow().token);
            let node = BinaryOp{
                lhs: Rc::new(RefCell::new(AstNode{
                    node: AstNodeType::Variable(object_id),
                    token: obj.token.clone(),
                })),
                rhs: expr,
                op: BinaryOpType::Assign,
            };
            let node = Rc::new(RefCell::new(AstNode{
                node: AstNodeType::BinaryOp(node),
                token: token.clone(),
            }));
            let node = Rc::new(RefCell::new(AstNode{
                node: AstNodeType::ExprStmt(node),
                token,
            }));
            init.push(node);
        }
    }
    
    Ok((
        cursor0,
        Rc::new(RefCell::new(AstNode{
            node: AstNodeType::Block(init),
            token: range_between(&cursor.as_range(), &cursor0.as_range()),
        }))
    ))
}

static mut CTX: Option<AstContext> = None;

fn enter_scope() {
    unsafe {
        CTX.as_mut().unwrap().enter_scope();
    }
}

fn leave_scope() {
    unsafe {
        CTX.as_mut().unwrap().leave_scope();
    }
}

fn new_local_var_with_token(id: TokenSpan, ty: Weak<Type>) -> ObjectId {
    let instance = unsafe {
        CTX.as_mut().unwrap()
    };
    let obj_id = instance.new_local_var(id.as_str(), ty);
    let obj = instance.get_object_mut(obj_id).unwrap();
    obj.token = id.as_range();
    obj_id
}

fn new_global_var(id: &str, ty: Weak<Type>) -> ObjectId {
    let instance = unsafe {
        CTX.as_mut().unwrap()
    };
    instance.new_global_var(id, ty)
}

fn get_object_mut(id: ObjectId) -> Option<&'static mut AstObject> {
    unsafe {
        CTX.as_mut().unwrap().get_object_mut(id)
    }
}

fn find_var(id: &str) -> Option<ScopeVar> {
    unsafe {
        CTX.as_mut().unwrap().find_var(id)
    }
}

fn init_context() {
    unsafe {
        CTX.replace(AstContext::new());
    }
}

fn take_context() -> AstContext {
    unsafe {
        CTX.take().unwrap()
    }
}

fn get_context_locals_mut() -> &'static mut Vec<ObjectId> {
    unsafe {
        &mut CTX.as_mut().unwrap().locals
    }
}

pub fn statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // return_statement | expression_statement | ... // todo
    // declaration is not a statement!
    alt((
        return_statement,
        compound_statement,
        expression_statement,
    ))(cursor)
}

pub fn compound_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // "{" (declaration | statement)* "}"
    map(
        tuple((
            ttag!(P("{")),
            map(success(()), |_| enter_scope()),
            many0(alt((
                declaration,
                statement))),
            map(success(()), |_| leave_scope()),
            ttag!(P("}")))),
        |(l, _, v, _, r)| {
            let token = range_between(&l.as_range(), &r.as_range());
            Rc::new(RefCell::new(AstNode {
                node: AstNodeType::Block(v),
                token,
            }))
        }
    )(cursor)
}

pub fn parse<'a>(curosr: &'a Vec<Token>) -> Result<AstContext, nom::Err<nom::error::Error<TokenSpan<'a>>>>
{
    init_context();

    let (_, body) = compound_statement(curosr.into())
        .map(|(rest, node)| {
            ast_const_fold(node.clone());
            (rest, node)
        })?;
    let mut l = vec![];
    swap(&mut l, get_context_locals_mut());
    let function = AstFuncData {
        params: vec![],
        locals: l,
        body,
    };
    let id = new_global_var("main", Type::void_type());
    let obj = get_object_mut(id).unwrap();
    obj.data = AstObjectType::Func(function);
    
    Ok(take_context())
}

/// do constant folding on AST
fn ast_const_fold(tree: Rc<RefCell<AstNode>>) {
    let tree0 = tree.borrow();
    let new_node: Option<AstNodeType> = match tree0.node.clone() {
        AstNodeType::I64Number(_) => None,
        AstNodeType::Variable(_) => None,
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
                        BinaryOpType::Assign => return,
                    };
                    Some(AstNodeType::I64Number(num))
                },
                _ => None,
            }
        },
        AstNodeType::Return(expr) => {
            ast_const_fold(expr);
            None
        },
        AstNodeType::ExprStmt(expr) => {
            ast_const_fold(expr);
            None
        },
        AstNodeType::Unit => None,
        AstNodeType::Block(v) => {
            for expr in v {
                ast_const_fold(expr.clone());
            }
            None
        },
    };
    drop(tree0);

    if let Some(new_node) = new_node {
        (*tree).borrow_mut().node = new_node;
    }
}