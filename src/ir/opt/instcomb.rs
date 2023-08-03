use crate::{
    ir::{structure::TransUnit, value::{InstructionValue, ConstantValue}},
    ctype::BinaryOpType
};

use super::{IrPass, bbopt::bbopt};

pub struct InstructionCombination;

impl IrPass for InstructionCombination {
    fn run(unit: &mut TransUnit) {
        for k in unit.funcs() {
            let mut done = false;
            while !done {
                let comb = combine(unit, k.as_str());
                let bbopt = bbopt(unit, k.as_str());
                done = !comb && !bbopt;
            }
        }
    }
}

fn combine(unit: &mut TransUnit, func: &str) -> bool {
    let mut changed = false;

    // calculate_used_by(unit, func);

    let bbs = unit.funcs[func].bbs.clone();

    macro_rules! extract {
        ($cnt:ident, $opr:expr) => {{
            let val = &unit.values[$opr];
            if val.value.is_constant() {
                if let ConstantValue::$cnt(num) = val.value.as_constant() {
                    Some(*num)
                } else {
                    None
                }
            } else {
                None
            }
        }};
    }

    macro_rules! combine {
        ($($operand:ident),* ; $action:tt) => {
            combine!(@expand_if $($operand),* ; $action)
        };
        (@expand_if $operand:ident ; $action:tt) => {
            if let Some($operand) = $operand {
                $action
            } else {
                None
            }
        };
        (@expand_if $operand:ident, $($operands:ident),+ ; $action:tt) => {
            if let Some($operand) = $operand {
                combine!(@expand_if $($operands),* ; $action)
            } else {
                None
            }
        };
        (@expand_if $operand:ident, $($operands:ident),+ , ; $action:tt) => {
            if let Some($operand) = $operand {
                combine!(@expand_if $($operands),* ; $action)
            } else {
                None
            }
        };
    }

    for bb in &bbs {
        let mut iter = unit.blocks[*bb].insts_start;
        while let Some(inst) = iter {
            let insn = unit.values[inst].clone();
            iter = insn.next;

            let instruction = insn.value.as_inst();

            // op c.i32 c.i32 => c.i32
            // op: add sub mul div mod
            // op c.i32 c.i32 => c.i1
            // op: eq ne lt le gt ge
            // [1] match binary op
            if let InstructionValue::Binary(bin) = instruction {
                // [2.1] lhs is c.i32
                let lhs = extract!(I32, bin.lhs);
                // [2.2] rhs is c.i32
                let rhs = extract!(I32, bin.rhs);

                let new_val = combine!(lhs, rhs; {
                    match bin.op {
                        BinaryOpType::Add => Some(unit.const_i32(lhs.wrapping_add(rhs))),
                        BinaryOpType::Sub => Some(unit.const_i32(lhs.wrapping_sub(rhs))),
                        BinaryOpType::Mul => Some(unit.const_i32(lhs.wrapping_mul(rhs))),
                        BinaryOpType::Div => Some(unit.const_i32(lhs.wrapping_div(rhs))),
                        BinaryOpType::Mod => Some(unit.const_i32(lhs.wrapping_rem(rhs))),
                        BinaryOpType::Eq => Some(unit.const_i1(lhs == rhs)),
                        BinaryOpType::Ne => Some(unit.const_i1(lhs != rhs)),
                        BinaryOpType::Lt => Some(unit.const_i1(lhs < rhs)),
                        BinaryOpType::Le => Some(unit.const_i1(lhs <= rhs)),
                        BinaryOpType::Gt => Some(unit.const_i1(lhs > rhs)),
                        BinaryOpType::Ge => Some(unit.const_i1(lhs >= rhs)),
                        BinaryOpType::And => Some(unit.const_i32(lhs & rhs)),
                        _ => None,
                    }
                });

                if let Some(val) = new_val {
                    unit.replace(inst, val);
                    unit.remove(*bb, inst);
                    changed = true;
                    continue;
                }
            }

            // op c.i1 c.i1 => c.i1
            // op: xor
            if let InstructionValue::Binary(bin) = instruction {
                // [2.1] lhs is c.i1
                let lhs = extract!(I1, bin.lhs);
                // [2.2] rhs is c.i1
                let rhs = extract!(I1, bin.rhs);

                let new_val = combine!(lhs, rhs; {
                    match bin.op {
                        BinaryOpType::Xor => Some(unit.const_i1(lhs ^ rhs)),
                        _ => None,
                    }
                });

                if let Some(val) = new_val {
                    unit.replace(inst, val);
                    unit.remove(*bb, inst);
                    changed = true;
                    continue;
                }
            }

        }
    }

    changed
}