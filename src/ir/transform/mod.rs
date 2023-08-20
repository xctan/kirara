use super::structure::TransUnit;

pub mod mem2reg;
pub mod rename;
pub mod bbopt;
pub mod instcomb;
pub mod dce;
pub mod gvngcm;
pub mod inline;
// pub mod loopunroll;
pub mod licm;

pub trait IrPass {
    fn run(&self, unit: &mut TransUnit);
}

pub trait IrFuncPass {
    fn run_on_func(&self, unit: &mut TransUnit, func: &str);
}

#[macro_export]
macro_rules! for_each_bb_and_inst {
    ($unit:ident , $func:ident ( $bid:ident ,  $block:ident , $vid:ident , $value:ident ) , $baction:tt , $vaction:tt ) => {
        for bb in &$func.bbs {
            let $bid = *bb;
            let $block = $unit.blocks[$bid].clone();
            $baction
            let mut iter = $block.insts_start;
            while let Some($vid) = iter {
                let $value = $unit.values[$vid].clone();
                iter = $value.next;
                $vaction
            }
        }
    };
}

pub mod misc {
    use super::IrPass;

    pub struct Reorder;
    impl IrPass for Reorder {
        fn run(&self, unit: &mut super::TransUnit) {
            let mut funcs = std::mem::take(&mut unit.funcs);
            for (_, func) in &mut funcs {
                let mut new_bbs = Vec::new();
                let mut visited = std::collections::HashSet::new();
                let mut queue = std::collections::VecDeque::new();
                queue.push_back(func.bbs[0]);
                while let Some(bb) = queue.pop_front() {
                    if visited.contains(&bb) {
                        continue;
                    }
                    visited.insert(bb);
                    new_bbs.push(bb);
                    for inst in unit.succ(bb) {
                        queue.push_back(inst);
                    }
                }
                func.bbs = new_bbs;
            }
            unit.funcs = funcs;
        }
    }
}