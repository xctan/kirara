use nom::{
    branch::alt,
    bytes::complete::{tag, is_not, take_until},
    character::complete::{digit0, one_of, satisfy, multispace1, alpha1, alphanumeric1},
    combinator::{recognize, value, opt},
    multi::{many1, many0_count},
    sequence::{pair, tuple},
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
        if let Ok((i, _)) = junk(input) {
            input = i;
            continue;
        }
        if input.is_empty() {
            break;
        }

        let (i, o) = alt((integer_const, word))(input).unwrap();
        input = i;
        tokens.push(o);
    }

    for token in tokens {
        println!("{:?}", token);
    }
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
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_"))))
    ))(input).map(|(i, o)| (i, Token::Identifier(o)))
}

fn word(input: &str) -> IResult<&str, Token> {
    alt((
        keyword, 
        identifier
    ))(input)
}