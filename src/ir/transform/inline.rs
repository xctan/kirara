use std::collections::HashMap;

use crate::ir::{structure::{TransUnit, BasicBlock}, value::{InstructionValue, CallInst, ValueType, JumpInst}};

use super::IrPass;

pub static INLINE_CALLER_LIMIT: usize = 1024;
pub static INLINE_CALLEE_LIMIT: usize = 128;

pub struct FunctionInlining;

impl IrPass for FunctionInlining {
    fn run(&self, unit: &mut TransUnit) {
        do_inline(unit);
    }
}

pub fn do_inline(unit: &mut TransUnit) {
    let funcs: Vec<_> = unit.funcs.keys().cloned().collect();
    let mut stat_start: HashMap<_, _> = funcs
        .iter()
        .map(|f| (f.clone(), unit.func_inst_count(&f)))
        .collect();
    let mut stat = stat_start.clone();
    stat_start.iter_mut().for_each(|(_, v)| *v *= 2);
    let mut worklist = funcs;
    while let Some(f) = worklist.pop() {
        // eprintln!("current {}, stat {:?}", f, stat);
        if inline_func(unit, &f, &mut stat, &stat_start) {
            stat.remove(&f);
            worklist.push(f);
        }
    }
}

/// Try to embed callee into caller function
fn inline_func(
    unit: &mut TransUnit,
    func: &str,
    stat: &mut HashMap<String, usize>,
    limit: &HashMap<String, usize>,
) -> bool {
    // avoid infinite recursion
    if unit.func_inst_count(func) >= limit[func] {
        return false;
    }
    let mut changed = false;

    // collection of all (including new) basic blocks in the function
    let mut bbs = unit.funcs[func].bbs.clone();
    // use worklist here because we don't want recursive expansion
    let mut worklist = unit.funcs[func].bbs.clone();
    'outer: while let Some(callsite_bb) = worklist.pop() {
        let mut iter = unit.blocks[callsite_bb].insts_start;
        while let Some(vid) = iter {
            let inst = unit.values[vid].clone();
            iter = inst.next;

            match inst.value.as_inst() {
                // ignore tail call as for now
                // in case of tail recursion, transform it to loop first (in another pass)
                InstructionValue::Call(call) => {
                    // eprintln!("trying to inline {}", call.func);
                    let callee_inst_count = stat
                        .entry(call.func.clone())
                        .or_insert_with(|| unit.func_inst_count(&call.func))
                        .clone();
                    if callee_inst_count >= INLINE_CALLEE_LIMIT {
                        continue;
                    }
                    changed = true;

                    let tmp_func = format!("{}$INLINE${}", call.func, unit.count());
                    unit.clone_func(&call.func, &tmp_func);
                    // eprintln!("{}", unit);

                    // create landing pad
                    let name = format!("{}", unit.count());
                    let block = BasicBlock::new(name);
                    let landing = unit.blocks.alloc(block);
                    bbs.push(landing);
                    worklist.push(landing); // expand rest section of this basic block
                    // move predecessors
                    for succ in unit.succ(callsite_bb) {
                        let succ_block = unit.blocks.get_mut(succ).unwrap();
                        succ_block.preds
                            .iter_mut()
                            .for_each(|p| { if *p == callsite_bb { *p = landing; } });
                        let mut succ_iter = succ_block.insts_start;
                        while let Some(svid) = succ_iter {
                            let sinst = unit.values.get_mut(svid).unwrap();
                            succ_iter = sinst.next;
                            match sinst.value.as_inst_mut() {
                                InstructionValue::Phi(phi) => {
                                    phi.args
                                        .iter_mut()
                                        .for_each(|(_, bb)| { if *bb == callsite_bb { *bb = landing; } });
                                }
                                _ => break
                            }
                        }
                    }
                    // remove `call' instruction usage info
                    for arg in &call.args {
                        unit.values.get_mut(*arg).unwrap().used_by.remove(&vid);
                    }
                    // move existing instructions to the landing pad
                    let mut rest_iter = iter;
                    while let Some(rvid) = rest_iter {
                        let rinst = unit.values[rvid].clone();
                        rest_iter = rinst.next;
                        unit.takeout(rvid);
                        unit.insert_at_end(landing, rvid);
                    }
                    // hoist call instruction
                    unit.takeout(vid);
                    // destructure the copied function
                    let inlined_func = unit.funcs.remove(&tmp_func).unwrap();
                    bbs.extend(inlined_func.bbs.clone());
                    // jump to inlined entry basic block
                    let jump = unit.jump(inlined_func.entry_bb);
                    unit.insert_at_end(callsite_bb, jump);
                    let inline_entry_block = unit.blocks.get_mut(inlined_func.entry_bb).unwrap();
                    inline_entry_block.preds.push(callsite_bb);
                    inline_entry_block.is_entry = false;
                    // replace parameter with actual value
                    call.args
                        .iter()
                        .zip(inlined_func.params.iter())
                        .for_each(|(argument, parameter)| unit.replace(*parameter, *argument));
                    inlined_func.params
                        .iter()
                        .for_each(|p| { unit.values.remove(*p); });
                    // converge inlined return
                    let mut phi_args = vec![]; 
                    for ibb in &inlined_func.bbs {
                        // the frontend generates `ret' immediately after tail call
                        let in_final = unit.blocks[*ibb].insts_end.unwrap();
                        let in_final_val = unit.values[in_final].clone();
                        if let Some(prev) = in_final_val.prev {
                            let preval = unit.values[prev].clone();
                            match preval.value.as_inst() {
                                InstructionValue::TailCall(tcall) => {
                                    // convert it to normal call
                                    let call = CallInst {
                                        name: tcall.name.clone(),
                                        ty: tcall.ty.clone(),
                                        func: tcall.func.clone(),
                                        args: tcall.args.clone(),
                                    };
                                    let prevalmut = unit.values.get_mut(prev).unwrap();
                                    let inst = InstructionValue::Call(call);
                                    prevalmut.value = ValueType::Instruction(inst);
                                }
                                _ => {}
                            }
                        }
                        match in_final_val.value.as_inst() {
                            InstructionValue::Return(r) => {
                                // we *should* have consistent return value, so not checking type here
                                if let Some(retv) = r.value {
                                    phi_args.push((retv, *ibb));
                                    // maintain usage info
                                    let retvalue = unit.values.get_mut(retv).unwrap();
                                    retvalue.used_by.remove(&in_final);
                                }
                                // rewrite return to jump
                                let jump = JumpInst {
                                    succ: landing,
                                };
                                let inst = InstructionValue::Jump(jump);
                                let ret_mut = unit.values.get_mut(in_final).unwrap();
                                ret_mut.value = ValueType::Instruction(inst);
                                let landing_block = unit.blocks.get_mut(landing).unwrap();
                                landing_block.preds.push(*ibb);
                            },
                            _ => {}
                        }
                    }
                    if phi_args.len() > 0 {
                        // non-void return value, merge its definition
                        let phi = 
                            unit.phi(phi_args.clone(), inlined_func.ty.as_function().ret_type.clone());
                        unit.insert_at_begin(landing, phi);
                        unit.replace(vid, phi);
                        // maintain usage info
                        for (arg, _) in phi_args {
                            let arg_value = unit.values.get_mut(arg).unwrap();
                            arg_value.used_by.insert(phi);
                        }
                    }
                    unit.values.remove(vid);
                    // sync collection of basic blocks
                    assert!(bbs.len() >= unit.funcs.get(func).unwrap().bbs.len());
                    // sync immediately for recursive expansion
                    unit.funcs.get_mut(func).unwrap().bbs = bbs.clone();
                    // each basic block should run only once
                    continue 'outer;
                }
                _ => {}
            }
        }
    }

    changed
}