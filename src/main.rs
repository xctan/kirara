use std::io::Read;
use clap::Parser;
use lazy_static::lazy_static;

#[derive(Parser, Default, Debug)]
pub struct Arguments {
    #[clap(short, long)]
    /// Dump every intermediate representation
    pub dump: bool,
    #[clap(short = 'O', default_value = "1")]
    /// Optimization level
    pub optimize: String,
    /// Input file
    pub input: String,
    /// Output file
    #[clap(short, long, default_value = "-")]
    pub output: String,
    /// Frontend language
    #[clap(short = 'x', long, default_value = "sysy")]
    pub language: String,
    #[clap(short = 'S', long)]
    pub assembly: bool,
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

lazy_static!{
    pub static ref ARGS: Arguments = Arguments::parse();
}

fn main() {
    macro_rules! debug {
        ($act:expr) => {
            if ARGS.dump {
                $act
            }
        };
    }

    let mut input = if ARGS.language == "sysy" {
        include_str!("defs.h").to_owned()
    } else {
        String::new()
    };
    if ARGS.input == "-" {
        std::io::stdin().read_to_string(&mut input).unwrap();
    } else {
        input.push_str(&std::fs::read_to_string(&ARGS.input).unwrap());
    }

    let tokens = tokenize(input.as_str()).unwrap().1;

    // for token in &tokens {
    //     println!("{:?}", token);
    // }

    let mut ast = parse(&tokens).unwrap();
    // debug!(println!("{:#?}", ast));

    let mut unit = ast.emit_ir();
    
    ir::opt::canonicalize::Canonicalize::run(&mut unit);
    if ARGS.optimize != "0" {
        debug!(unit.print());
        ir::opt::mem2reg::Mem2Reg::run(&mut unit);
        // ir::opt::instcomb::InstructionCombination::run(&mut unit);
        // ir::opt::dce::DeadCodeElimination::run(&mut unit);
        ir::opt::canonicalize::Canonicalize::run(&mut unit);
    }
    debug!(unit.print());

    let mut asm = unit.emit_asm();
    debug!(println!("{}", asm));
    asm.allocate_registers(&mut unit);
    asm.simplify();
    asm.setup_stack();
    
    if ARGS.output == "-" {
        print!("{}", asm);
    } else {
        std::fs::write(&ARGS.output, asm.to_string()).unwrap();
    }
}
