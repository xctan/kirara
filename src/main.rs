use std::io::stdin;

mod token;
mod lex;
mod ast;
mod ir;
mod ctype;
use crate::{
    lex::tokenize,
    ast::parse,
};

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    let tokens = tokenize(input.as_str()).unwrap().1;

    // for token in &tokens {
    //     println!("{:?}", token);
    // }

    let mut ast = parse(&tokens).unwrap();
    // println!("{:#?}", ast);

    let mut unit = ir::builder::TransUnit::new();
    ast.emit_ir(&mut unit);
    unit.print();
}
