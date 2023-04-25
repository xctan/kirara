use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit0, one_of},
    combinator::recognize,
    multi::many1,
    sequence::{pair, preceded},
    IResult,
};

use std::io::stdin;

enum Token<'a> {
    IntegerConst(&'a str),
}

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    let num = integer_const(input.as_str());
    let (_, num) = num.unwrap();

    match num {
        Token::IntegerConst(num) => println!("  mov a1, {}", num),
    }
    println!("  ret");
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
        recognize(many1(one_of("0123456789abcdefABCDEF"))),
    ))(input)
}

fn integer_const(input: &str) -> IResult<&str, Token> {
    alt((
        hexadecimal_const,
        octal_const,
        decimal_const,
    ))(input).map(|(i, o)| (i, Token::IntegerConst(o)))
}