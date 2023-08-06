use nom::{
    branch::alt,
    bytes::complete::{tag, is_not, take_until},
    character::complete::{one_of, satisfy, multispace1, alpha1, alphanumeric1, digit1},
    combinator::{recognize, value, opt, map, not, peek},
    multi::{many0_count, separated_list1, many1},
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
        peek(one_of("123456789")), // non-zero digit
        separated_list1(tag("'"), digit1)
    ))(input)
}

fn octal_const(input: &str) -> IResult<&str, &str> {
    recognize(separated_list1(tag("'"), many1(one_of("01234567"))))(input)
}

fn hexadecimal_const(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((tag("0x"), tag("0X"))),
        recognize(
            separated_list1(tag("'"), many1(satisfy(|c: char| c.is_ascii_hexdigit()))))
    ))(input)
}

fn integer_const(input: &str) -> IResult<&str, Token> {
    map(
        alt((
            hexadecimal_const,
            decimal_const,
            octal_const)),
        |s| Token(s, TokenType::IntegerConst)
    )(input)
}

fn whole_number(input: &str) -> IResult<&str, &str> {
    recognize(separated_list1(tag("'"), digit1))(input)
}

fn exponent(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        alt((tag("e"), tag("E"))),
        opt(alt((tag("+"), tag("-")))),
        whole_number
    )))(input)
}

fn floating_decimal(input: &str) -> IResult<&str, &str> {
    alt((
        recognize(tuple((
            whole_number,
            opt(tag(".")),
            exponent))),
        recognize(tuple((
            opt(whole_number),
            tag("."),
            whole_number,
            opt(exponent)))),
        recognize(tuple((
            whole_number,
            tag("."),
            opt(whole_number)))))
    )(input)
}

fn whole_number_h(input: &str) -> IResult<&str, &str> {
    recognize(separated_list1(
        tag("'"),
        many1(satisfy(|c: char| c.is_ascii_hexdigit()))
    ))(input)
}

fn exponent_h(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        alt((tag("p"), tag("P"))),
        opt(alt((tag("+"), tag("-")))),
        whole_number
    )))(input)
}

fn floating_hexadecimal(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((tag("0x"), tag("0X"))),
        alt((
            recognize(tuple((
                whole_number_h,
                opt(tag(".")),
                exponent_h))),
            recognize(tuple((
                opt(whole_number_h),
                tag("."),
                whole_number_h,
                exponent_h)))
    ))))(input)
}

fn floating_const(input: &str) -> IResult<&str, Token> {
    map(
        alt((
            floating_hexadecimal,
            floating_decimal)),
        |s| Token(s, TokenType::FloatConst)
    )(input)
}

fn keyword(input: &str) -> IResult<&str, Token> {
    map(
        pair(
            alt((
                tag("return"),
                tag("int"),
                tag("float"),
                tag("void"),
                tag("if"),
                tag("else"),
                tag("while"),
                tag("goto"),
                tag("break"),
                tag("continue"),
                tag("const"),
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
                tag("="))),
            alt((
                tag("{"),
                tag("}"),
                tag("("),
                tag(")"),
                tag("["),
                tag("]"),
                tag(":"),
                tag("!"),
                tag("#"),
                tag("&"),
            ))
        )),
        |s| Token(s, TokenType::Punctuation)
    )(input)
}

fn combined(input: &str) -> IResult<&str, Token> {
    alt((
        floating_const,
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