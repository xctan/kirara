use nom::{
    branch::alt,
    bytes::complete::{tag},
    character::complete::{digit0, one_of, satisfy, multispace0},
    combinator::{recognize},
    multi::many1,
    sequence::{pair},
    IResult,
};

use std::io::stdin;

#[derive(Debug)]
enum Token<'a> {
    IntegerConst(&'a str),
    Keyword(&'a str),
    Identifier(&'a str),
}

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    let mut tokens = Vec::new();
    let mut input = input.as_str();
    while !input.is_empty() {
        let (i, _) = ltrim(input).unwrap();
        if i.is_empty() {
            break;
        }
        let (i, o) = alt((integer_const, word))(i).unwrap();
        input = i;
        tokens.push(o);
    }

    for token in tokens {
        println!("{:?}", token);
    }
}

fn ltrim(input: &str) -> IResult<&str, ()> {
    multispace0(input).map(|(i, _)| (i, ()))
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
    alt((
        hexadecimal_const,
        octal_const,
        decimal_const,
    ))(input).map(|(i, o)| (i, Token::IntegerConst(o)))
}

fn keyword(input: &str) -> IResult<&str, Token> {
    alt((
        tag("return"),
    ))(input).map(|(i, o)| (i, Token::Keyword(o)))
}

fn identifier(input: &str) -> IResult<&str, Token> {
    recognize(pair(
        satisfy(|c: char| c.is_alphabetic() || c == '_'),
        many1(satisfy(|c: char| c.is_alphanumeric() || c == '_'))
    ))(input).map(|(i, o)| (i, Token::Identifier(o)))
}

fn word(input: &str) -> IResult<&str, Token> {
    alt((
        keyword, 
        identifier
    ))(input)
}