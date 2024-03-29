use nom::{
    IResult,
    combinator::{map, opt, success, peek},
    sequence::{tuple, delimited},
    multi::{many0, many1, many0_count, separated_list1, separated_list0},
    branch::alt,
};

use std::{rc::Rc, cell::RefCell, convert::TryInto, mem::swap};

use crate::{
    token::{TokenSpan, Token, range_between},
    ast::*,
    ctype::{BinaryOpType, Type, TypePtrCompare},
};

use super::transform::{AstPassManager, type_check::ast_type_check};

macro_rules! ttag {
    (IntCn) => {
        nom::bytes::complete::tag(
            $crate::token::TokenType::IntegerConst
        )
    };
    (FpCn) => {
        nom::bytes::complete::tag(
            $crate::token::TokenType::FloatConst
        )
    };
    (I) => {
        nom::bytes::complete::tag::<
            $crate::token::TokenType,
            $crate::token::TokenSpan<'_>,
            nom::error::Error<$crate::token::TokenSpan<'_>>
        >(
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
    (S) => {
        nom::bytes::complete::tag(
            $crate::token::TokenType::StringLiteral
        )
    };
}

fn integer_constant(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        ttag!(IntCn),
        |token: TokenSpan<'_>| {
            if let Ok(num) = TryInto::<i32>::try_into(token) {
                AstNode::i32_number(num, token.as_range())
            } else if let Ok(num) = TryInto::<i64>::try_into(token) {
                AstNode::i64_number(num, token.as_range())
            } else {
                panic!("malformed integer constant: {}", token.as_str());
            }
        }
    )(cursor)
}

fn float_constant(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        ttag!(FpCn),
        |token: TokenSpan<'_>| {
            if let Ok(num) = TryInto::<f32>::try_into(token) {
                AstNode::f32_number(num, token.as_range())
            } else {
                panic!("malformed float constant: {}", token.as_str());
            }
        }
    )(cursor)
}

fn string_literal(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    map(
        ttag!(S),
        |token: TokenSpan<'_>| {
            AstNode::string_literal(token.as_str().to_owned(), token.as_range())
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
    // identifier | number_constant | '(' expression ')'
    alt((
        map(
            tuple((ttag!(P("(")), expression, ttag!(P(")")))),
            |(l, expr, r)| {
                let token = range_between(&l.as_range(), &r.as_range());
                (*expr).borrow_mut().token = token;
                expr
            }),
        identifier,
        integer_constant,
        float_constant,
        string_literal,
    ))(cursor)
}

fn postfix(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // C: compound_literal | primary ( '[' expression ']' | '(' argument_expression_list? ')' | '.' identifier | '->' identifier | '++' | '--' )*

    let (mut cursor, mut node) = primary(cursor)?;

    loop {
        if let Ok((cursor1, (_, expr, r))) = tuple((
            ttag!(P("[")),
            expression,
            ttag!(P("]")),
        ))(cursor) {
            let token = range_between(&node.borrow().token, &r.as_range());
            node = AstNode::binary(node, expr, BinaryOpType::Index, token);
            cursor = cursor1;
            continue;
        }

        if let Ok((cursor1, (_, args, r))) = tuple((
            ttag!(P("(")),
            separated_list0(ttag!(P(",")), assignment),
            ttag!(P(")")),
        ))(cursor) {
            let token = range_between(&node.borrow().token, &r.as_range());
            node = AstNode::call(node, args, token);
            cursor = cursor1;
            continue;
        }

        return Ok((cursor, node));
    }
}

fn unary(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // postfix | ('+' | '-' | '!' | '~' | '&' | '*') cast | ('++' | '--') unary | '&&' identifier
    // partial impl
    alt((
        postfix,
        map(tuple((
            alt((
                ttag!(P("+")),
                ttag!(P("-")),
                ttag!(P("!")),
                ttag!(P("~")),
                ttag!(P("&")),
                ttag!(P("*")))),
            cast)),
            |(sign, expr)| {
                let op = match sign.as_str() {
                    "+" => return expr,
                    "-" => UnaryOpType::Neg,
                    "!" => UnaryOpType::LogNot,
                    // "~" => UnaryOpType::BitNot,
                    "&" => UnaryOpType::Addr,
                    "*" => UnaryOpType::Deref,
                    _ => unreachable!(),
                };
                let token = range_between(&sign.as_range(), &expr.borrow().token);
                AstNode::unary(expr, op, token)
            }
        )
    ))(cursor)
}

fn cast(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // unary | '(' type_name ')' cast
    // stub impl
    unary(cursor)
}

fn multiplication(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // cast ( ('*' | '/' | '%') cast )*
    // use postfix as cast temporarily
    map(
        tuple((
            cast,
            many0(
                tuple((
                    alt((ttag!(P("*")), ttag!(P("/")), ttag!(P("%")))),
                    cast))))),
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

fn conditional(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // stub impl
    logical_or(cursor)
}

fn assignment(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // conditional ( '=' assignment )?
    map(
        tuple((
            conditional,
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
fn declspec(cursor: TokenSpan) -> IResult<TokenSpan, Rc<Type>> {
    map(
        many1(alt((
            ttag!(K("int")),
            ttag!(K("void")),
            ttag!(K("const")),
            ttag!(K("float")),
        ))),
        |decl: Vec<TokenSpan>| {
            const VOID: i32 = 1 << 0;
            const INT: i32 = 1 << 8;
            const FLOAT: i32 = 1 << 12;
            let mut ty = Type::i32_type();
            let mut counter = 0;
            let mut is_const = false;

            for d in decl {
                match d.as_str() {
                    "void" => counter += VOID,
                    "int" => counter += INT,
                    "const" => is_const = true,
                    "float" => counter += FLOAT,
                    _ => unreachable!(),
                }

                match counter {
                    0 => continue,
                    VOID => ty = Type::void_type(),
                    INT => ty = Type::i32_type(),
                    FLOAT => ty = Type::f32_type(),
                    _ => panic!("invalid type"),
                }
            }
            
            if !is_const {
                ty
            } else {
                Type::const_of(ty)
            }
        }
    )(cursor)
}

fn declarator((cursor, ty): (TokenSpan, Rc<Type>)) -> IResult<TokenSpan, (Rc<Type>, Option<TokenSpan>)> {
    // "*"* ("(" declarator ")" | identifier) type_suffix
    // let mut ty = ty.clone();
    let mut ty = ty.clone();

    let (left, _consumed) = many0_count(ttag!(P("*")))(cursor)?;
    // todo: parse pointer attr
    for _ in 0.._consumed {
        ty = Type::ptr_to(ty);
    }

    if let Ok((l_cursor, _)) = ttag!(P("("))(left) {
        let void = Type::void_type();
        let (r_cursor, (_ty, _)) = declarator((l_cursor, void))?;
        let (rem_cursor, _) = ttag!(P(")"))(r_cursor)?;
        let (extra, suffix_ty) = type_suffix((rem_cursor, ty))?;
        let (_, real_ty) = declarator((l_cursor, suffix_ty))?;
        Ok((extra, real_ty))
    } else {
        let (cursor, id) = ttag!(I)(left)
            .map(|(cursor, token)| (cursor, Some(token)))
            .unwrap_or((left, None));
        let (cursor, ty) = type_suffix((cursor, ty))?;
        Ok((cursor, (ty, id)))
    }
}

fn type_suffix((cursor, ty): (TokenSpan, Rc<Type>)) -> IResult<TokenSpan, Rc<Type>> {
    // func_params | array_dimensions | empty
    if let Ok((cursor, ty)) = func_params((cursor.clone(), ty.clone())) {
        return Ok((cursor, ty));
    }

    if let Ok((cursor, ty)) = array_dimensions((cursor.clone(), ty.clone())) {
        return Ok((cursor, ty));
    }

    Ok((cursor, ty))
}

fn array_dimensions((cursor, ty): (TokenSpan, Rc<Type>)) -> IResult<TokenSpan, Rc<Type>> {
    // C: "[" ("static" | "restrict")* const_expr? "]" type_suffix
    // "[" const_expr? "]" type_suffix
    let (cursor, dim) = delimited(
        ttag!(P("[")),
        opt(conditional),
        ttag!(P("]")),
    )(cursor)?;

    if let Some(dim) = dim {
        if let Some(dim) = transform::eval(dim) {
            let (cursor, ty) = type_suffix((cursor, ty))?;
            Ok((cursor, Type::array_of(ty, dim.try_into().unwrap())))
        } else {
            unimplemented!("variable length array");
        }
    } else {
        let (cursor, ty) = type_suffix((cursor, ty))?;
        Ok((cursor, Type::array_of(ty, -1)))
    }
}

fn func_params((cursor, ty): (TokenSpan, Rc<Type>)) -> IResult<TokenSpan, Rc<Type>> {
    // C: "(" ("void" | param ("," param)* ("," "...")?)? ")"
    // "(" ("void" | param ("," param)*)? ")"
    // param = declspec declarator
    delimited(
        ttag!(P("(")),
        alt((
            map(
                tuple((
                    ttag!(K("void")),
                    peek(ttag!(P(")"))),
                )),
                |_| Type::func_type(ty.clone(), vec![(gen_unique_name(".anon"), Type::void_type())]),
            ),
            map(
                separated_list1(ttag!(P(",")), param),
                |mut params| {
                    assert!(params.iter().all(|(_, ty)| !ty.is_void()));
                    params.iter_mut().for_each(|(_, ty)| {
                        if ty.is_array() {
                            *ty = ty.decay();
                        }
                    });
                    Type::func_type(ty.clone(), params)
                }
            ),
            map(
                success(()),
                // todo: func w/o params is variadic
                |_| Type::func_type(ty.clone(), vec![]),
            )
        )),
        ttag!(P(")")),
    )(cursor)
}

fn param(cursor: TokenSpan) -> IResult<TokenSpan, (String, Rc<Type>)> {
    // declspec declarator
    let base_ty = declspec(cursor)?;
    let (cursor, (ty, id)) = declarator(base_ty)?;
    Ok((
        cursor,
        (id.map(|t| t.as_str().to_string()).unwrap_or(gen_unique_name(".anon")), ty)
    ))
}

fn local_initializer(init: &mut Vec<Rc<RefCell<AstNode>>>, mut this: Rc<RefCell<AstNode>>, initializer: &Initializer, mut expr_only: bool) {
    match &initializer.data {
        InitData::ScalarI32(int) => {
            if expr_only {
                return;
            }
            let rhs = AstNode::i32_number(*int, 0..0);
            let assign = 
                AstNode::binary(this.clone(), rhs, BinaryOpType::Assign, this.borrow().token.clone());
            let node = AstNode::expr_stmt(assign, this.borrow().token.clone());
            init.push(node);
        },
        InitData::ScalarF32(float) => {
            if expr_only {
                return;
            }
            let rhs = AstNode::f32_number(*float, 0..0);
            let assign = 
                AstNode::binary(this.clone(), rhs, BinaryOpType::Assign, this.borrow().token.clone());
            let node = AstNode::expr_stmt(assign, this.borrow().token.clone());
            init.push(node);
        }
        InitData::Expr(expr) => {
            let assign = 
                AstNode::binary(this.clone(), expr.clone(), BinaryOpType::Assign, this.borrow().token.clone());
            let node = AstNode::expr_stmt(assign, this.borrow().token.clone());
            init.push(node);
        },
        InitData::ZeroInit => {
            if expr_only {
                return;
            }
            ast_type_check(this.clone());
            if !this.borrow().ty().is_array() {
                // todo: float 0.0f?
                let rhs = AstNode::i32_number(0, 0..0);
                let assign = 
                    AstNode::binary(this.clone(), rhs, BinaryOpType::Assign, this.borrow().token.clone());
                let node = AstNode::expr_stmt(assign, this.borrow().token.clone());
                init.push(node);
            } else {
                let memset = find_func("memset").unwrap();
                let memset = AstNode::variable(memset, 0..0);
                let zero = AstNode::i32_number(0, 0..0);
                let len = AstNode::i32_number(this.borrow().ty().size().try_into().unwrap(), 0..0);
                let args = vec![this.clone(), zero, len];
                let call = AstNode::call(memset, args, this.borrow().token.clone());
                let node = AstNode::expr_stmt(call, this.borrow().token.clone());
                init.push(node);
            }
        },
        InitData::Aggregate(data) => {
            // think twice before expanding all stuff to assignment
            let is_const = initializer.data.is_const();
            let const_ratio = if is_const {
                1.0
            } else {
                initializer.data.const_ratio()
            };
            if !expr_only && (is_const || const_ratio > 0.5) {
                ast_type_check(this.clone());
                let var_name = match &this.borrow().node {
                    AstNodeType::Variable(v) => {
                        let var_obj = get_object(*v).unwrap();
                        var_obj.name.clone()
                    }
                    _ => "".to_string()
                };
                let const_init = initializer.retain_const();
                let object_id = new_global_var(
                    &gen_unique_name(&format!("__const_init.{var_name}.")),
                    this.borrow().ty(),
                    Linkage::Static
                );
                let init_obj = get_object_mut(object_id).unwrap();
                init_obj.data = AstObjectType::Var(const_init);
                let memcpy = find_func("memcpy").unwrap();
                let memcpy = AstNode::variable(memcpy, 0..0);
                let args = vec![
                    this.clone(),
                    AstNode::variable(object_id, 0..0),
                    AstNode::i32_number(this.borrow().ty().size().try_into().unwrap(), 0..0),
                ];
                let call = AstNode::call(memcpy, args, this.borrow().token.clone());
                let node = AstNode::expr_stmt(call, this.borrow().token.clone());
                init.push(node);
                expr_only = true;

                let newnode = this.borrow().deep_clone();
                this = newnode;
            }
            if !is_const {
                for (index, d) in data.iter().enumerate() {
                    let idx = AstNode::i32_number(index.try_into().unwrap(), 0..0);
                    let member = 
                        AstNode::binary(this.clone(), idx, BinaryOpType::Index, this.borrow().token.clone());
                    local_initializer(init, member, &d, expr_only);
                }
            }
        }
        InitData::StringLiteral(_s) => {
            unimplemented!()
        }
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
        let id = id
            .ok_or(nom::Err::Error(nom::error::Error {
                input: cursor0,
                code: nom::error::ErrorKind::Tag,
            }))?;
        let object_id = new_local_var_with_token(id, ty.clone());

        if let Ok((cursor2, _)) = ttag!(P("="))(cursor0) {
            cursor0 = cursor2;

            let mut initializer = Initializer::new(ty.clone());
            (cursor0, _) = initializer2(cursor0, &mut initializer)?;
            initializer.eval(ty.clone());
            let var = AstNode::variable(object_id, range_between(&cursor2.as_range(), &cursor0.as_range()));
            local_initializer(&mut init, var, &initializer, false);
            let obj = get_object_mut(object_id).unwrap();
            obj.data = AstObjectType::Var(initializer);
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
        break_statement,
        continue_statement,
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

/// specialized version of compound_statement
/// we need to enter scope before parameters are declared
fn function_body(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // "{" (declaration | statement)* "}"
    map(
        tuple((
            ttag!(P("{")),
            many0(alt((
                declaration,
                statement))),
            ttag!(P("}")))),
        |(l, v, r)| {
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
            map(success(()), |_| {
                let brk = gen_unique_name("brk.");
                let cont = gen_unique_name("cont.");
                push_loop(cont, brk);
            }),
            ttag!(P("(")),
            expression,
            ttag!(P(")")),
            statement)),
        |(k_while, _, _, exp, _, stmt)| {
            let token = range_between(&k_while.as_range(), &stmt.borrow().token);
            let (cont, brk) = pop_loop().unwrap();
            let stmt_token = stmt.borrow().token.clone();
            let cont = AstNode::label(cont, AstNode::unit(0..0), stmt_token.clone());
            let stmt = AstNode::block(vec![stmt, cont], stmt_token);
            let wh = AstNode::r#while(exp, stmt, token.clone());
            let brk = AstNode::label(brk, AstNode::unit(0..0), token.clone());
            AstNode::block(vec![wh, brk], token)
        }
    )(cursor)
}

fn break_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // "break" ";"
    map(
        tuple((
            ttag!(K("break")),
            ttag!(P(";")))),
        |(k_break, p)| {
            let token = range_between(&k_break.as_range(), &p.as_range());
            let brk = get_loop_labels().expect("stray break").1.clone();
            AstNode::goto(brk, token)
        }
    )(cursor)
}

fn continue_statement(cursor: TokenSpan) -> IResult<TokenSpan, Rc<RefCell<AstNode>>> {
    // "continue" ";"
    map(
        tuple((
            ttag!(K("continue")),
            ttag!(P(";")))),
        |(k_continue, p)| {
            let token = range_between(&k_continue.as_range(), &p.as_range());
            let cont = get_loop_labels().expect("stray continue").0.clone();
            AstNode::goto(cont, token)
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

fn function((cursor, ty): (TokenSpan, Rc<Type>)) -> IResult<TokenSpan, ()> {
    // function_def = declspec declarator (compound_statement | ";")
    let (cursor, (ty, name)) = declarator((cursor.clone(), ty))?;
    let name = name
        .ok_or(nom::Err::Error(nom::error::Error {
            input: cursor,
            code: nom::error::ErrorKind::Tag,
        }))?
        .as_str()
        .to_string();
    if name.is_empty() {
        panic!("function name omitted");
    }

    let obj_id = if let Some(old_id) = find_func(&name) {
        let old_obj = get_object(old_id).unwrap();
        if !old_obj.ty.is_same_as(&ty) {
            panic!("redefinition of {} as different kind of symbol", name);
        }
        old_id
    } else {
        new_global_var(&name, ty.clone(), Linkage::Global)
    };

    if let Ok((cursor, _)) = ttag!(P(";"))(cursor) {
        let obj = get_object_mut(obj_id).unwrap();
        obj.is_decl = true;
        return Ok((cursor, ()));
    }

    let obj = get_object(obj_id).unwrap();
    let params = obj.ty.as_function().params;

    enter_scope();
    let params: Vec<_> = params.into_iter()
        .map(|(name, ty)| new_local_var(&name, ty))
        .collect();

    let (cursor, body) = function_body(cursor)?;
    if !validate_gotos() {
        panic!("goto undefined label");
    }
    
    let mut func = AstFuncData {
        body,
        ret_var: None,
        locals: vec![],
        params,
        func_ty: ty,
    };
    AstPassManager.apply_passes(&mut func);
    swap(&mut func.locals, get_context_locals_mut());
    leave_scope();

    let obj = get_object_mut(obj_id).unwrap();
    obj.data = AstObjectType::Func(func);

    reset_func_data();

    return Ok((cursor, ()));
}

fn is_function(cursor: TokenSpan) -> bool {
    if let Ok(_) = ttag!(P(";"))(cursor) {
        return false;
    }
    if let Ok(_) = ttag!(P(","))(cursor) {
        return false;
    }

    if let Ok((_, (ty, _))) = declarator((cursor.clone(), Type::void_type())) {
        return ty.is_function();
    } else {
        // ????
        panic!("cannot determine if function or not");
    }
}

fn global_declaration((cursor, ty): (TokenSpan, Rc<Type>)) -> IResult<TokenSpan, ()> {
    // global_declaration = declspec (declarator ("," declarator)* | ";")
    let mut cursor0 = cursor;
    let mut count = 0;

    loop {
        if let Ok((cursor1, _)) = ttag!(P(";"))(cursor0) {
            cursor0 = cursor1;
            break Ok((cursor0, ()));
        }

        if count > 0 {
            let (cursor1, _) = ttag!(P(","))(cursor0)?;
            cursor0 = cursor1;
        }
        count += 1;

        let (cursor1, (ty, id)) = declarator((cursor0, ty.clone()))?;
        cursor0 = cursor1;
        let id = id.ok_or(nom::Err::Error(nom::error::Error {
            input: cursor0,
            code: nom::error::ErrorKind::Tag,
        }))?;
        let object_id = new_global_var(id.as_str(), ty.clone(), Linkage::Global);

        if let Ok((cursor2, _)) = ttag!(P("="))(cursor0) {
            cursor0 = cursor2;
            
            let mut initializer = Initializer::new(ty.clone());
            (cursor0, _) = initializer2(cursor0, &mut initializer)?;
            initializer.eval(ty.clone());
            if !initializer.data.is_const() {
                panic!("initializer element is not constant: {}, {:#?}", id.as_str(), initializer.data);
            }
            let obj = get_object_mut(object_id).unwrap();
            obj.data = AstObjectType::Var(initializer);
        }
    }
}

fn initializer2<'a>(cursor: TokenSpan<'a>, init: &mut Initializer) -> IResult<TokenSpan<'a>, ()> {
    // expression | "{" initializer ("," initializer)* ","? "}" 

    // todo: string-literal if ty is array and token is string-literal

    // array
    if init.ty.is_array() {
        if ttag!(P("{"))(cursor).is_ok() {
            return array_initializer1(cursor, init);
        } else {
            return array_initializer2(cursor, init, 0);
        }
    }

    // todo: struct & union

    // scalar surrounded by braces
    if let Ok((cursor, _)) = ttag!(P("{"))(cursor) {
        initializer2(cursor, init)?;
        // should we discard any excess elements?
        return ttag!(P("}"))(cursor).map(|(cursor, _)| (cursor, ()));
    }

    // expression
    let (cursor, expr) = assignment(cursor)?;
    init.data = InitData::Expr(expr);

    Ok((cursor, ()))
}

fn skip_initialier(cursor: TokenSpan) -> IResult<TokenSpan, ()> {
    alt((
        map(tuple((ttag!(P("{")), skip_initialier, ttag!(P("}")))), |_| ()),
        map(assignment, |_| ()),
    ))(cursor)
}

fn array_initializer1<'a>(cursor: TokenSpan<'a>, init: &mut Initializer) -> IResult<TokenSpan<'a>, ()> {
    let (mut cursor, _) = ttag!(P("{"))(cursor)?;
    if let Ok((cursor, _)) = ttag!(P("}"))(cursor) {
        return Ok((cursor, ()));
    }
    
    let mut index = 0;
    loop {
        if let Ok(_) = ttag!(P("["))(cursor) {
            unimplemented!("designated initializer");
        }

        if index < init.ty.as_array().len as usize {
            (cursor, _) = initializer2(cursor, &mut init.as_array_mut()[index])?;
            index += 1;
        } else {
            (cursor, _) = skip_initialier(cursor)?;
        }

        if let Ok((cursor, _)) = ttag!(P("}"))(cursor) {
            break Ok((cursor, ()));
        }
        (cursor, _) = ttag!(P(","))(cursor)?;
        if let Ok((cursor, _)) = ttag!(P("}"))(cursor) {
            break Ok((cursor, ()));
        }
    }
}

fn array_initializer2<'a>(mut cursor: TokenSpan<'a>, init: &mut Initializer, mut index: usize) -> IResult<TokenSpan<'a>, ()> {
    loop {
        if index >= init.ty.as_array().len as usize || ttag!(P("}"))(cursor).is_ok() {
            break Ok((cursor, ()));
        }
        if index > 0 {
            (cursor, _) = ttag!(P(","))(cursor)?;
        }

        if let Ok(_) = ttag!(P("["))(cursor) {
            unimplemented!("designated initializer");
        }
        if let Ok(_) = ttag!(P("."))(cursor) {
            unimplemented!("designated initializer");
        }

        (cursor, _) = initializer2(cursor, &mut init.as_array_mut()[index])?;

        index += 1;
    }
}

pub fn parse<'a>(curosr: &'a Vec<Token>) -> Result<AstContext, nom::Err<nom::error::Error<TokenSpan<'a>>>>
{
    init_context();

    let mut cur = TokenSpan::from(curosr.as_slice());
    while cur.0.len() > 0 {
        let (cursor, base_ty) = declspec(cur)?;

        if is_function(cursor) {
            cur = function((cursor, base_ty))?.0;
            continue;
        }

        cur = global_declaration((cursor, base_ty))?.0;
    }
    
    Ok(take_context())
}

