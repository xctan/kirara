use nom::{
    IResult,
    combinator::{map, opt, success},
    sequence::tuple,
    multi::{many0, many1, many0_count},
    branch::alt,
};

use std::{rc::{Rc, Weak}, cell::RefCell, convert::TryInto, mem::swap};

use crate::{
    token::{TokenSpan, Token, range_between},
    ast::*,
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
            if let Ok(num) = TryInto::<i32>::try_into(token) {
                AstNode::i32_number(num, token.as_range())
            } else if let Ok(num) = TryInto::<i64>::try_into(token) {
                AstNode::i64_number(num, token.as_range())
            } else {
                panic!("integer constant overflow");
            }
        }
    )(cursor)
}

fn identifier(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        ttag!(I),
        |token: TokenSpan<'_>| {
            let var = find_var(token.as_str());
            match var {
                Some(ScopeVar::Var(var)) => Rc::new(RefCell::new(AstNode::new(AstNodeType::Variable(var), token.as_range()))),
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
                let op = match sign.as_str() {
                    "*" => BinaryOpType::Mul,
                    "/" => BinaryOpType::Div,
                    "%" => BinaryOpType::Mod,
                    _ => unreachable!(),
                };
                let token = range_between(&node.borrow().token, &other.borrow().token);
                node = AstNode::binary(node, other, op, token);
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
                let op = match sign.as_str() {
                    "+" => BinaryOpType::Add,
                    "-" => BinaryOpType::Sub,
                    _ => unreachable!(),
                };
                let token = range_between(&node.borrow().token, &other.borrow().token);
                node = AstNode::binary(node, other, op, token);
            }
            node
        }
    )(cursor)
}

fn relational(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // shift ( ('<' | '>' | "<=" | ">=") shift )*
    // use addition as shift temporarily
    map(
        tuple((
            addition,
            many0(
                tuple((
                    alt((
                        ttag!(P("<")),
                        ttag!(P(">")),
                        ttag!(P("<=")),
                        ttag!(P(">=")))),
                    addition))))),
        |(first, others)| {
            let mut node = first;
            for (sign, other) in others {
                let op = match sign.as_str() {
                    "<" => BinaryOpType::Lt,
                    ">" => BinaryOpType::Gt,
                    "<=" => BinaryOpType::Le,
                    ">=" => BinaryOpType::Ge,
                    _ => unreachable!(),
                };
                let token = range_between(&node.borrow().token, &other.borrow().token);
                node = AstNode::binary(node, other, op, token);
            }
            node
        }
    )(cursor)
}

fn equality(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // relational ( ("==" | "!=") relational )*
    map(
        tuple((
            relational,
            many0(
                tuple((
                    alt((ttag!(P("==")), ttag!(P("!=")))),
                    relational))))),
        |(first, others)| {
            let mut node = first;
            for (sign, other) in others {
                let op = match sign.as_str() {
                    "==" => BinaryOpType::Eq,
                    "!=" => BinaryOpType::Ne,
                    _ => unreachable!(),
                };
                let token = range_between(&node.borrow().token, &other.borrow().token);
                node = AstNode::binary(node, other, op, token);
            }
            node
        }
    )(cursor)
}

fn logical_and(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // bitwise_or ( "&&" bitwise_or )*
    // use equality as bitwise_or temporarily
    map(
        tuple((
            equality,
            many0(
                tuple((
                    ttag!(P("&&")),
                    equality))))),
        |(first, others)| {
            let mut node = first;
            for (_, other) in others {
                let token = range_between(&node.borrow().token, &other.borrow().token);
                node = AstNode::binary(node, other, BinaryOpType::LogAnd, token);
            }
            node
        }
    )(cursor)
}

fn logical_or(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // logical_and ( "||" logical_and )*
    map(
        tuple((
            logical_and,
            many0(
                tuple((
                    ttag!(P("||")),
                    logical_and))))),
        |(first, others)| {
            let mut node = first;
            for (_, other) in others {
                let token = range_between(&node.borrow().token, &other.borrow().token);
                node = AstNode::binary(node, other, BinaryOpType::LogOr, token);
            }
            node
        }
    )(cursor)
}

fn assignment(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // conditional ( '=' assignment )?
    // use logical_or as conditional temporarily
    map(
        tuple((
            logical_or,
            opt(tuple((
                ttag!(P("=")),
                assignment))))),
        |(first, others)| {
            if let Some((_, other)) = others {
                let token = range_between(&first.borrow().token, &other.borrow().token);
                AstNode::binary(first, other, BinaryOpType::Assign, token)
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
                AstNode::expr_stmt(expr, token)
            } else {
                AstNode::unit(semi.as_range())
            }
        }
    )(cursor)
}

fn return_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        tuple((
            ttag!(K("return")),
            opt(expression),
            ttag!(P(";")))),
        |(r, expr, p)| {
            if let Some(expr) = expr {
                let token = range_between(&r.as_range(), &p.as_range());
                AstNode::ret(expr, token)
            } else {
                let token = range_between(&r.as_range(), &p.as_range());
                AstNode::ret(AstNode::unit(r.as_range()), token)
            }
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
                match d.as_str() {
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
    // let mut ty = ty.clone();
    let ty = ty.clone();

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
            let var = AstNode::variable(object_id, obj.token.clone());
            let assign_expr = AstNode::binary(var, expr, BinaryOpType::Assign, token.clone());
            let node = AstNode::expr_stmt(assign_expr, token);
            init.push(node);
        }
    }
    
    Ok((
        cursor0,
        AstNode::block(
            init, 
            range_between(&cursor.as_range(), &cursor0.as_range())
        )
    ))
}

fn statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // return_statement | expression_statement | ... // todo
    // declaration is not a statement!
    alt((
        return_statement,
        if_statement,
        while_statement,
        goto_statement,
        label_statement,
        compound_statement,
        expression_statement,
    ))(cursor)
}

fn compound_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
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
            AstNode::block(v, token)
        }
    )(cursor)
}

fn if_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // "if" "(" expression ")" statement ("else" statement)?
    map(
        tuple((
            ttag!(K("if")),
            ttag!(P("(")),
            expression,
            ttag!(P(")")),
            statement,
            opt(tuple((
                ttag!(K("else")),
                statement))))),
        |(k_if, _, exp, _, stmt, el)| {
            if let Some((_, els)) = el {
                let token = range_between(&k_if.as_range(), &els.borrow().token);
                AstNode::r#if(exp, stmt, els, token)
            } else {
                let token = range_between(&k_if.as_range(), &stmt.borrow().token);
                AstNode::r#if(exp, stmt, AstNode::unit(token.clone()), token)
            }
        }
    )(cursor)
}

fn while_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // "while" "(" expression ")" statement
    map(
        tuple((
            ttag!(K("while")),
            ttag!(P("(")),
            expression,
            ttag!(P(")")),
            statement)),
        |(k_while, _, exp, _, stmt)| {
            let token = range_between(&k_while.as_range(), &stmt.borrow().token);
            AstNode::r#while(exp, stmt, token)
        }
    )(cursor)
}

fn label_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // identifier ":" statement
    map(
        tuple((
            ttag!(I),
            ttag!(P(":")),
            statement)),
        |(id, _, stmt)| {
            let token = range_between(&id.as_range(), &stmt.borrow().token);
            if !register_label(id.as_str().into()) {
                panic!("label {} is already defined", id.as_str());
            }
            AstNode::label(id.as_str().into(), stmt, token)
        }
    )(cursor)
}

fn goto_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // todo: computed goto(indirected jump)
    // "goto" identifier ";"
    map(
        tuple((
            ttag!(K("goto")),
            ttag!(I),
            ttag!(P(";")))),
        |(k_goto, id, _)| {
            let token = range_between(&k_goto.as_range(), &id.as_range());
            register_goto(id.as_str().into());
            AstNode::r#goto(id.as_str().into(), token)
        }
    )(cursor)
}

pub fn parse<'a>(curosr: &'a Vec<Token>) -> Result<AstContext, nom::Err<nom::error::Error<TokenSpan<'a>>>>
{
    init_context();

    let (_, body) = compound_statement(curosr.into())
        .map(|(rest, node)| {
            transform::AstPassManager.apply_passes(node.clone());
            (rest, node)
        })?;
    if !validate_gotos() {
        panic!("goto undefined label");
    }
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

