use super::builder::TransUnit;

pub mod mem2reg;

pub trait IrPass {
    fn run(unit: &mut TransUnit);
}