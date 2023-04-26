use nom::{
    branch::alt,
    bytes::complete::{tag, is_not, take_until},
    character::complete::{digit0, one_of, satisfy, multispace1, alpha1, alphanumeric1},
    combinator::{recognize, value, opt, map},
    multi::{many1, many0_count},
    sequence::{pair, tuple},
    IResult,
    Compare, InputTake, InputLength,
};

use std::{io::stdin, rc::Rc};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum TokenType {
    IntegerConst,
    Keyword,
    Identifier,
    Punctuation,
}

#[derive(Debug, Copy, Clone, Eq)]
struct Token<'a>(&'a str, TokenType);

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1 && *self.0 == *other.0
    }
}

#[derive(Debug, Copy, Clone)]
struct TokenSpan<'a>(&'a [Token<'a>]);

impl Compare<TokenType> for TokenSpan<'_> {
    fn compare(&self, t: TokenType) -> nom::CompareResult {
        if self.0.is_empty() {
            nom::CompareResult::Incomplete
        } else if self.0[0].1 == t {
            nom::CompareResult::Ok
        } else {
            nom::CompareResult::Error
        }
    }

    fn compare_no_case(&self, t: TokenType) -> nom::CompareResult {
        self.compare(t)
    }
}

impl Compare<Token<'_>> for TokenSpan<'_> {
    fn compare(&self, t: Token) -> nom::CompareResult {
        if self.0.is_empty() {
            nom::CompareResult::Incomplete
        } else if self.0[0] == t {
            nom::CompareResult::Ok
        } else {
            nom::CompareResult::Error
        }
    }

    fn compare_no_case(&self, t: Token) -> nom::CompareResult {
        self.compare(t)
    }
}

impl<'a> InputTake for TokenSpan<'a> {
    fn take(&self, count: usize) -> Self {
        TokenSpan(&self.0[..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (TokenSpan(suffix), TokenSpan(prefix))
    }
}

impl InputLength for TokenType {
    fn input_len(&self) -> usize { 1 }
}

impl InputLength for Token<'_> {
    fn input_len(&self) -> usize { 1 }
}

impl InputLength for TokenSpan<'_> {
    fn input_len(&self) -> usize { self.0.len() }
}

impl<'a> From<&'a [Token<'a>]> for TokenSpan<'a> {
    fn from(tokens: &'a [Token<'a>]) -> Self {
        TokenSpan(tokens)
    }
}

impl<'a> From<&'a std::vec::Vec<Token<'_>>> for TokenSpan<'a> {
    fn from(tokens: &'a std::vec::Vec<Token<'_>>) -> Self {
        TokenSpan(tokens.as_slice())
    }
}

#[derive(Debug)]
enum AstNodeType<'a> {
    Number,
    Return(Rc<AstNode<'a>>),
}

#[derive(Debug)]
struct AstNode<'a> {
    node: AstNodeType<'a>,
    token: TokenSpan<'a>,
    // ty: Type,
}

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    let mut tokens = Vec::new();
    let mut input = input.as_str();
    while !input.is_empty() {
        if let Ok((i, _)) = junk(input) {
            input = i;
            continue;
        }
        if input.is_empty() {
            break;
        }

        let (i, o) = alt((integer_const, word, punctuation))(input).unwrap();
        input = i;
        tokens.push(o);
    }

    for token in &tokens {
        println!("{:?}", token);
    }

    let (i, o) = return_statement(TokenSpan(&tokens)).unwrap();
    println!("{:#?}", o);
}

fn ltrim(input: &str) -> IResult<&str, ()> {
    value(
        (),
        multispace1
    )(input)
}

fn peol_comment(input: &str) -> IResult<&str, ()> {
    value(
        (),
        pair(
            tag("//"),
            opt(is_not("\r\n"))
        )
    )(input)
}

fn pinline_comment(input: &str) -> IResult<&str, ()> {
    value(
        (),
        tuple((
            tag("/*"),
            take_until("*/"),
            tag("*/")
        ))
    )(input)
}

fn junk(input: &str) -> IResult<&str, ()> {
    alt((
        ltrim,
        peol_comment,
        pinline_comment
    ))(input)
}

fn decimal_const(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        one_of("123456789"), // non-zero digit
        digit0,              // zero or more digits
    ))(input)
}

fn octal_const(input: &str) -> IResult<&str, &str> {
    recognize(many1(one_of("01234567")))(input)
}

fn hexadecimal_const(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((tag("0x"), tag("0X"))),
        recognize(
                many1(satisfy(|c: char| c.is_ascii_hexdigit())))
    ))(input)
}

fn integer_const(input: &str) -> IResult<&str, Token> {
    map(
        alt((
            hexadecimal_const,
            octal_const,
            decimal_const,)),
        |s| Token(s, TokenType::IntegerConst)
    )(input)
}

fn keyword(input: &str) -> IResult<&str, Token> {
    map(
        alt((
            tag("return"),
        )),
        |s| Token(s, TokenType::Keyword)
    )(input)
}

fn identifier(input: &str) -> IResult<&str, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))))),
        |s| Token(s, TokenType::Identifier)
    )(input)
}

fn word(input: &str) -> IResult<&str, Token> {
    alt((
        keyword, 
        identifier
    ))(input)
}

fn punctuation(input: &str) -> IResult<&str, Token> {
    map(
        alt((
            tag(";"),
        )),
        |s| Token(s, TokenType::Punctuation)
    )(input)
}

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