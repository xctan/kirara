use std::io::stdin;

mod token;
mod lex;
mod ast;
mod parse;
use crate::lex::tokenize;
use crate::parse::parse;

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    let tokens = tokenize(input.as_str()).unwrap().1;

    for token in &tokens {
        println!("{:?}", token);
    }

    let ast = parse(&tokens).unwrap().1;
    println!("{:#?}", ast);
}
