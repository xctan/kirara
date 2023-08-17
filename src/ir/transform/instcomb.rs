use std::collections::HashMap;

use crate::{
    ir::{structure::TransUnit, value::{InstructionValue, ConstantValue, ValueId, ValueTrait}},
    ctype::BinaryOpType
};

use super::{IrPass, IrFuncPass};

pub struct InstructionCombination;

impl IrPass for InstructionCombination {
    fn run(&self, unit: &mut TransUnit) {
        for k in unit.funcs() {
            // let mut done = false;
            // while !done {
            //     let comb = combine(unit, k.as_str());
            //     let bbopt = bbopt(unit, k.as_str());
            //     done = !comb && !bbopt;
            // }
            combine(unit, k.as_str());
        }
    }
}

pub struct ScalarLinearInduction;

impl IrPass for ScalarLinearInduction {
    fn run(&self, unit: &mut TransUnit) {
        let funcs: Vec<_> = unit.funcs.keys().cloned().collect();
        for func in funcs {
            self.run_on_func(unit, &func);
        }
    }
}

impl IrFuncPass for ScalarLinearInduction {
    fn run_on_func(&self, unit: &mut TransUnit, func: &str) {
        let bbs = unit.funcs[func].bbs.clone();
        for bb in &bbs {
            // optimize many additions chained together
            // is this a kind of linear variable induction?
            let mut iter = unit.blocks[*bb].insts_start;
            let mut linvars: HashMap<ValueId, HashMap<ValueId, isize>> = HashMap::new();
            while let Some(inst) = iter {
                let insn = unit.values[inst].clone();
                iter = insn.next;
                let instruction = insn.value.as_inst();

                if !insn.value.ty().is_int() {
                    continue;
                }

                if let InstructionValue::Binary(bin) = instruction {
                    match bin.op {
                        BinaryOpType::Add => {
                            let lhs = linvars
                                .get(&bin.lhs)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| vec![(bin.lhs, 1)].into_iter().collect());
                            let rhs = linvars
                                .get(&bin.rhs)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| vec![(bin.rhs, 1)].into_iter().collect());
                            let mut new_lhs = lhs.clone();
                            for (k, v) in rhs {
                                *new_lhs.entry(k).or_insert(0) += v;
                            }
                            new_lhs.retain(|_, v| *v != 0);
                            linvars.insert(inst, new_lhs);
                        }
                        BinaryOpType::Sub => {
                            let lhs = linvars
                                .get(&bin.lhs)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| vec![(bin.lhs, 1)].into_iter().collect());
                            let rhs = linvars
                                .get(&bin.rhs)
                                .map(|x| x.clone())
                                .unwrap_or_else(|| vec![(bin.rhs, 1)].into_iter().collect());
                            let mut new_lhs = lhs.clone();
                            for (k, v) in rhs {
                                *new_lhs.entry(k).or_insert(0) -= v;
                            }
                            new_lhs.retain(|_, v| *v != 0);
                            linvars.insert(inst, new_lhs);
                        }
                        _ => {}
                    }
                }

                if let Some(linvar) = linvars.get(&inst) {
                    if linvar.len() == 2 && linvar.iter().all(|(_, v)| v.abs() == 1) {
                        // just added, no need to optimize
                    } else if linvar.len() == 0 {
                        // reduced to constant, replace it
                        let new_val = unit.const_i32(0);
                        unit.replace(inst, new_val);
                    } else {
                        let mut acc = None;
                        for (&k, &v) in linvar.iter() {
                            let kval = unit.values[k].value.clone();
                            if kval.is_constant() {
                                let kval = kval.as_constant();
                                if let ConstantValue::I32(kval) = kval {
                                    if *kval == 0 {
                                        continue;
                                    }
                                }
                            }
                            if v == 1 {
                                if let Some(a) = acc {
                                    acc = Some(unit.binary(BinaryOpType::Add, a, k));
                                    unit.insert_before2(acc.unwrap(), inst);
                                } else {
                                    acc = Some(k);
                                }
                            } else if v == -1 {
                                if let Some(a) = acc {
                                    acc = Some(unit.binary(BinaryOpType::Sub, a, k));
                                    unit.insert_before2(acc.unwrap(), inst);
                                } else {
                                    let zero = unit.const_i32(0);
                                    acc = Some(unit.binary(BinaryOpType::Sub, zero, k));
                                    unit.insert_before2(acc.unwrap(), inst);
                                }
                            } else {
                                let multiplier = unit.const_i32(v as i32);
                                let new_val = unit.binary(BinaryOpType::Mul, k, multiplier);
                                unit.insert_before2(new_val, inst);
                                if let Some(a) = acc {
                                    acc = Some(unit.binary(BinaryOpType::Add, a, new_val));
                                    unit.insert_before2(acc.unwrap(), inst);
                                } else {
                                    acc = Some(new_val);
                                }
                            }
                        }
                        if acc.is_none() {
                            acc = Some(unit.const_i32(0));
                        }
                        let acc = acc.unwrap();
                        unit.replace(inst, acc);
                        unit.remove2(inst);
                        let var = linvar.clone();
                        linvars.remove(&inst);
                        linvars.insert(acc, var);
                    }
                }
            }
        }
    }
}

pub fn combine(unit: &mut TransUnit, func: &str) -> bool {
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

            // op c.i32 v.i32 => op v.i32 c.i32
            // op: add mul lt le gt ge eq ne (and or xor)
            if let InstructionValue::Binary(bin) = instruction {
                // [2.1] lhs is c.i32
                let lhs = extract!(I32, bin.lhs);

                let new_val = combine!(lhs; {
                    let _ = lhs;
                    match bin.op {
                        BinaryOpType::Add => Some(unit.binary(bin.op, bin.rhs, bin.lhs)),
                        BinaryOpType::Mul => Some(unit.binary(bin.op, bin.rhs, bin.lhs)),
                        BinaryOpType::Lt => Some(unit.binary(BinaryOpType::Gt, bin.rhs, bin.lhs)),
                        BinaryOpType::Le => Some(unit.binary(BinaryOpType::Ge, bin.rhs, bin.lhs)),
                        BinaryOpType::Gt => Some(unit.binary(BinaryOpType::Lt, bin.rhs, bin.lhs)),
                        BinaryOpType::Ge => Some(unit.binary(BinaryOpType::Le, bin.rhs, bin.lhs)),
                        BinaryOpType::Eq => Some(unit.binary(bin.op, bin.rhs, bin.lhs)),
                        BinaryOpType::Ne => Some(unit.binary(bin.op, bin.rhs, bin.lhs)),
                        _ => None,
                    }
                });

                if let Some(val) = new_val {
                    unit.replace(inst, val);
                    unit.insert_before(*bb, val, inst);
                    unit.remove(*bb, inst);
                    changed = true;
                    continue;
                }
            }

            // v2.i32 = op1 v1.i32 c1.i32   <== remove this via DCE
            //      _ = op2 v2.i32 c2.i32   <== match this
            // op1, op2: add sub
            if let InstructionValue::Binary(bin) = instruction {
                // [2.1] lhs is c.i32
                let rhs = extract!(I32, bin.rhs);
                let lhs = loop {
                    let lhs = unit.values[bin.lhs].value.clone();
                    if !lhs.is_inst() {
                        break None;
                    }
                    let lhs_inst = lhs.as_inst();
                    break if let InstructionValue::Binary(lhs) = lhs_inst {
                        let rhs = extract!(I32, lhs.rhs);

                        combine!(rhs; {
                            match lhs.op {
                                BinaryOpType::Add => Some((lhs.lhs, rhs)),
                                BinaryOpType::Sub => Some((lhs.lhs, -rhs)),
                                _ => None,
                            }
                        })
                    } else {
                        None
                    }
                };

                let new_val = combine!(lhs, rhs; {
                    match bin.op {
                        BinaryOpType::Add => {
                            let rhs = unit.const_i32(lhs.1 + rhs);
                            Some(unit.binary(bin.op, lhs.0, rhs))
                        },
                        BinaryOpType::Sub => {
                            let rhs = unit.const_i32(lhs.1 - rhs);
                            Some(unit.binary(BinaryOpType::Add, lhs.0, rhs))
                        },
                        _ => None,
                    }
                });

                if let Some(val) = new_val {
                    unit.replace(inst, val);
                    unit.insert_before(*bb, val, inst);
                    unit.remove(*bb, inst);
                    changed = true;
                    continue;
                }
            }

            // todo: x / 30 >= 10000 ==> x >= 300000
        }
    }

    changed
}