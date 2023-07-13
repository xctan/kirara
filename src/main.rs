use std::io::{stdin, Read};

mod alloc;
mod token;
mod lex;
mod ast;
mod ir;
mod asm;
mod ctype;
use crate::{
    lex::tokenize,
    ast::parse,
    ir::IrPass,
};

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let tokens = tokenize(input.as_str()).unwrap().1;

    // for token in &tokens {
    //     println!("{:?}", token);
    // }

    let mut ast = parse(&tokens).unwrap();
    // println!("{:#?}", ast);

    let mut unit = ast.emit_ir();
    // unit.print();

    ir::opt::mem2reg::Mem2Reg::run(&mut unit);
    ir::opt::canonicalize::Canonicalize::run(&mut unit);
    unit.print();

    let mut asm = unit.emit_asm();
    asm.print();
}
