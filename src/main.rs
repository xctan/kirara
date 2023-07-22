use std::io::{stdin, Read};
use clap::Parser;

#[derive(Parser, Default, Debug)]
struct Arguments {
    #[clap(short, long)]
    /// Dump every intermediate representation
    dump: bool,
}

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
    let args = Arguments::parse();
    macro_rules! debug {
        ($act:expr) => {
            if args.dump {
                $act
            }
        };
    }

    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let tokens = tokenize(input.as_str()).unwrap().1;

    // for token in &tokens {
    //     println!("{:?}", token);
    // }

    let mut ast = parse(&tokens).unwrap();
    // debug!(println!("{:#?}", ast));

    let mut unit = ast.emit_ir();
    debug!(unit.print());

    ir::opt::canonicalize::Canonicalize::run(&mut unit);
    ir::opt::mem2reg::Mem2Reg::run(&mut unit);
    ir::opt::canonicalize::Canonicalize::run(&mut unit);
    debug!(unit.print());

    let mut asm = unit.emit_asm();
    debug!(asm.print());
    asm.allocate_registers(&mut unit);
    asm.simplify();
    asm.setup_stack();
    asm.print();
}
