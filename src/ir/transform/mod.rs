use super::structure::TransUnit;

pub mod mem2reg;
pub mod rename;
pub mod bbopt;
pub mod instcomb;
pub mod dce;
pub mod gvngcm;
pub mod dse;

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