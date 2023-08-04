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
    let pwd = std::env::current_dir().unwrap();
    let input_basename = 
        std::path::Path::new(&ARGS.input).file_stem().unwrap().to_str().unwrap();

    let mut input = if ARGS.language == "sysy" {
        include_str!("rt/defs.h").to_owned()
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
    
    if ARGS.optimize != "0" {
        ir::opt::rename::Canonicalize::run(&mut unit);
        if ARGS.dump {
            std::fs::write(
                pwd.join(format!("{}.00.ll", input_basename)),
                unit.to_string()
            ).unwrap();
        }
        ir::opt::mem2reg::Mem2Reg::run(&mut unit);
        ir::opt::instcomb::InstructionCombination::run(&mut unit);
        ir::opt::dce::DeadCodeElimination::run(&mut unit);
    }
    ir::opt::rename::Canonicalize::run(&mut unit);
    if ARGS.dump {
        std::fs::write(
            pwd.join(format!("{}.01.ll", input_basename)),
            unit.to_string()
        ).unwrap();
    }

    let mut asm = unit.emit_asm();
    if ARGS.dump {
        std::fs::write(
            pwd.join(format!("{}.00.s", input_basename)),
            asm.to_string()
        ).unwrap();
    }
    asm.allocate_registers(&mut unit);
    asm.simplify();
    asm.setup_stack();
    
    if ARGS.output == "-" {
        print!("{}", asm);
    } else {
        std::fs::write(&ARGS.output, asm.to_string()).unwrap();
    }
}
