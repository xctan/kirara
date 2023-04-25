use nom::{
    IResult, combinator::recognize, character::complete::{one_of, digit0},
    sequence::pair,
};

use std::io::stdin;

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    let num = decimal_const(input.as_str());

    println!("  mv a1, {}", num.unwrap().1);
    println!("  ret");
}

fn decimal_const(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            one_of("123456789"), // non-zero digit
            digit0,              // zero or more digits
        )
    )(input)
}