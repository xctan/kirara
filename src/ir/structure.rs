use std::rc::Weak;

use crate::alloc::Id;

use crate::ctype::Type;

use super::value::ValueId;

#[derive(Debug, Clone)]
pub struct IrFunc {
    pub bbs: Vec<BlockId>,
    pub entry_bb: BlockId,
    pub ty: Weak<Type>,
    pub params: Vec<ValueId>,
}


#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub name: String,
    
    pub is_labeled: bool,

    pub preds: Vec<BlockId>,
    /// reverse post order
    pub rpo: usize,
    /// immediate dominator
    pub idom: Option<BlockId>,
    /// dominance frontier
    pub df: Vec<BlockId>,

    pub insts_start: Option<ValueId>,
    pub insts_end: Option<ValueId>,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            is_labeled: false,
            preds: Vec::new(),
            rpo: 0,
            idom: None,
            df: Vec::new(),
            insts_start: None,
            insts_end: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.insts_start.is_none()
    }
}

pub type BlockId = Id<BasicBlock>;