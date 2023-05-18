use nom::{
    branch::alt,
    bytes::complete::{tag, is_not, take_until},
    character::complete::{digit0, one_of, satisfy, multispace1, alpha1, alphanumeric1},
    combinator::{recognize, value, opt, map, not},
    multi::{many1, many0_count},
    sequence::{pair, tuple},
    IResult,
};

use crate::token::{Token, TokenType};

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
            decimal_const,
            hexadecimal_const,
            octal_const,)),
        |s| Token(s, TokenType::IntegerConst)
    )(input)
}

fn keyword(input: &str) -> IResult<&str, Token> {
    map(
        pair(
            alt((
                tag("return"),
                tag("int"),
                tag("void"),
                tag("if"),
                tag("else"),
                tag("while"),
                tag("goto"),
                tag("break"),
                tag("continue"),
            )),
            not(
                alt((
                    alphanumeric1,
                    tag("_"))))),
        |(s, _)| Token(s, TokenType::Keyword)
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
            tag("<="),
            tag(">="),
            tag("=="),
            tag("!="),
            tag("||"),
            tag("&&"),
            tag("<"),
            tag(">"),
            tag(";"),
            tag(","),
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("%"),
            tag("="),
            tag("{"),
            tag("}"),
            tag("("),
            tag(")"),
            tag(":"),
        )),
        |s| Token(s, TokenType::Punctuation)
    )(input)
}

fn combined(input: &str) -> IResult<&str, Token> {
    alt((
        integer_const,
        word,
        punctuation
    ))(input)
}

pub fn tokenize(mut input: &str) -> IResult<&str, Vec<Token>> {
    let mut tokens = Vec::new();
    while !input.is_empty() {
        if let Ok((i, _)) = junk(input) {
            input = i;
            continue;
        }
        if input.is_empty() {
            break;
        }

        let (i, o) = combined(input)?;
        input = i;
        tokens.push(o);
    }
    Ok((input, tokens))
}