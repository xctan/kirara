//! Rename registers to their precedence of definition to make LLVM happy.
//! 

use std::collections::HashMap;

use crate::{
    ir::{value::ValueTrait, structure::TransUnit}, for_each_bb_and_inst,
};

use super::IrPass;

pub struct Canonicalize;

impl IrPass for Canonicalize {
    fn run(unit: &mut TransUnit) {
        for k in unit.funcs() {
            canonicalize(unit, k.as_str());
        }
    }
}

fn canonicalize(unit: &mut TransUnit, func: &str) {
    let func = unit.funcs[func].clone();
    let mut block_map = HashMap::new();
    let mut value_map = HashMap::new();
    let mut counter = 0;
    let mut count = || {
        let current = counter;
        counter += 1;
        current
    };

    for p in &func.params {
        let obj = unit.values.get_mut(*p).unwrap();
        obj.set_name(format!("%{}", count()));
    }

    for_each_bb_and_inst!{
        unit, func(bb, block, inst, insn), {
            match block.name.as_str().parse::<usize>() {
                Ok(_) => { block_map.insert(bb, count().to_string()); },
                Err(_) => ()
            };
        }, {
            let name = insn.name().clone();
            if name.len() > 1 {
                match name[1..].parse::<usize>() {
                    Ok(_) => { value_map.insert(inst, format!("%{}", count())); },
                    Err(_) => ()
                };
            }
        }
    }

    for_each_bb_and_inst!{
        unit, func(bb, block, inst, insn), {
            if let Some(name) = block_map.get(&bb) {
                let block = &mut unit.blocks[bb];
                block.name = name.clone();
            }
        }, {
            if let Some(name) = value_map.get(&inst) {
                let value = &mut unit.values[inst];
                value.set_name(name.clone());
            }
        }
    }
}

