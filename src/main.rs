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
    let input_basename = {
        let raw = std::path::Path::new(&ARGS.input).file_stem().unwrap().to_str().unwrap();
        if raw == "-" {
            // avoid special characters in filename with confusing meaning
            "STDIN"
        } else {
            raw
        }
    };

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
    
    let passes: Vec<Box<dyn IrPass>> = match ARGS.optimize.as_str() {
        "0" => vec![
            Box::new(ir::transform::rename::Canonicalize),
            Box::new(ir::cfg::ComputeControlFlow),
            Box::new(ir::transform::mem2reg::Mem2Reg),
            Box::new(ir::transform::bbopt::BasicBlockOptimization),
            Box::new(ir::cfg::ComputeControlFlow),
            Box::new(ir::transform::mem2reg::Mem2Reg),
            Box::new(ir::transform::instcomb::InstructionCombination),
            Box::new(ir::transform::dce::DeadCodeElimination),
            Box::new(ir::transform::rename::Canonicalize),
        ],
        "1" => vec![
            Box::new(ir::transform::rename::Canonicalize),
            Box::new(ir::transform::bbopt::BasicBlockOptimization),
            Box::new(ir::callgraph::ComputeCallGraph),
            Box::new(ir::cfg::ComputeControlFlow),
            Box::new(ir::transform::mem2reg::Mem2Reg),
            Box::new(ir::memdep::ComputeGlobalModRef),
            Box::new(ir::transform::rename::Canonicalize),
            Box::new(ir::transform::gvngcm::GVNGCM),
            Box::new(ir::transform::inline::FunctionInlining),
            Box::new(ir::transform::bbopt::BasicBlockOptimization),
            Box::new(ir::transform::rename::Canonicalize),
            Box::new(ir::transform::gvngcm::GVNGCM),
            Box::new(ir::transform::rename::Canonicalize),
        ],
        _ => vec![
            Box::new(ir::transform::rename::Canonicalize),
            Box::new(ir::cfg::ComputeControlFlow),
            Box::new(ir::transform::mem2reg::Mem2Reg),
            Box::new(ir::transform::bbopt::BasicBlockOptimization),
            Box::new(ir::cfg::ComputeControlFlow),
            Box::new(ir::transform::mem2reg::Mem2Reg),
            Box::new(ir::transform::instcomb::InstructionCombination),
            Box::new(ir::transform::dce::DeadCodeElimination),
            Box::new(ir::transform::rename::Canonicalize),
        ],
    };
    let mut cnt = 0;
    for pass in &passes {
        if ARGS.dump {
            std::fs::write(
                pwd.join(format!("{}.{cnt:02}.ll", input_basename)),
                unit.to_string()
            ).unwrap();
            cnt += 1;
        }
        pass.run(&mut unit);
    }
    if ARGS.dump {
        std::fs::write(
            pwd.join(format!("{}.99.ll", input_basename)),
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
