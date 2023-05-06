use std::io::stdin;

mod token;
mod lex;
mod ast;
mod parse;
mod ir;
mod ctype;
use crate::{
    lex::tokenize,
    parse::parse,
    ir::gen::EmitIr,
};

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    let tokens = tokenize(input.as_str()).unwrap().1;

    for token in &tokens {
        println!("{:?}", token);
    }

    let ast = parse(&tokens).unwrap().1;
    println!("{:#?}", ast);

    let mut unit = ir::unit::TransUnit::new();
    ast.borrow().emit_ir(&mut unit);
    println!("{:#?}", unit);
}
