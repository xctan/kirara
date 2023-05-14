use std::{cell::RefCell, rc::Rc};

use super::AstNode;

pub trait AstTransformPass {
    fn apply(self, tree: Rc<RefCell<AstNode>>);
}

mod type_check;
mod const_fold;

pub struct AstPassManager;

impl AstPassManager {
    pub fn apply_passes(self, tree: Rc<RefCell<AstNode>>) {
        type_check::TypeCheckPass.apply(tree.clone());
        const_fold::ConstFoldPass.apply(tree.clone());
        type_check::TypeCheckPass.apply(tree);
    }
}