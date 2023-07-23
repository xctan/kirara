use std::io::Read;
use clap::Parser;

#[derive(Parser, Default, Debug)]
struct Arguments {
    #[clap(short, long)]
    /// Dump every intermediate representation
    dump: bool,
    #[clap(short = 'O', default_value = "0")]
    /// Optimization level
    optimize: String,
    /// Input file
    input: String,
    /// Output file
    #[clap(short, long, default_value = "-")]
    output: String,
    /// Frontend language
    #[clap(short = 'x', long, default_value = "sysy")]
    language: String,
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

    let mut input = if args.language == "sysy" {
        include_str!("defs.h").to_owned()
    } else {
        String::new()
    };
    if args.input == "-" {
        std::io::stdin().read_to_string(&mut input).unwrap();
    } else {
        input.push_str(&std::fs::read_to_string(&args.input).unwrap());
    }

    let tokens = tokenize(input.as_str()).unwrap().1;

    // for token in &tokens {
    //     println!("{:?}", token);
    // }

    let mut ast = parse(&tokens).unwrap();
    // debug!(println!("{:#?}", ast));

    let mut unit = ast.emit_ir();
    debug!(unit.print());

    ir::opt::canonicalize::Canonicalize::run(&mut unit);
    if args.optimize != "0" {
        ir::opt::mem2reg::Mem2Reg::run(&mut unit);
        ir::opt::canonicalize::Canonicalize::run(&mut unit);
    }
    debug!(unit.print());

    let mut asm = unit.emit_asm();
    debug!(println!("{}", asm));
    asm.allocate_registers(&mut unit);
    asm.simplify();
    asm.setup_stack();
    
    if args.output == "-" {
        println!("{}", asm);
    } else {
        std::fs::write(&args.output, asm.to_string()).unwrap();
    }
}
