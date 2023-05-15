use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

use crate::ast::{AstNode, AstNodeType};

use super::{AstRewriteVisitor, AstTransformPass};

/// Replace false `if`s and `while`s with empty node
struct DeadCodeRemovalHeurisic;

impl AstRewriteVisitor for DeadCodeRemovalHeurisic {
    fn visit_if(
        &mut self,
        cond: Rc<RefCell<AstNode>>,
        then: Rc<RefCell<AstNode>>,
        els: Rc<RefCell<AstNode>>,
    ) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(then.clone());
        self.rewrite(els.clone());
        match cond.borrow().node {
            AstNodeType::I1Number(false) => Some(els),
            AstNodeType::I1Number(true) => Some(then),
            _ => None,
        }
    }

    fn visit_while(
        &mut self,
        cond: Rc<RefCell<AstNode>>,
        body: Rc<RefCell<AstNode>>,
    ) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(body.clone());
        match cond.borrow().node {
            AstNodeType::I1Number(false) => Some(AstNode::unit(0..0)),
            _ => None,
        }
    }
}

/// Builds control flow graph for dead code removal
#[derive(Debug)]
struct CFGBuilder {
    pub entry: Rc<RefCell<AstNode>>,
    pub preds: HashMap<usize, Vec<Weak<RefCell<AstNode>>>>,
    pub previous: Vec<Weak<RefCell<AstNode>>>,
    // labels
    // jumps
}

impl CFGBuilder {
    fn new() -> Self {
        let mut instance = Self {
            entry: AstNode::unit(0..0),
            preds: HashMap::new(),
            previous: vec![],
        };
        instance.previous.push(Rc::downgrade(&instance.entry));
        instance
    }

    fn mark(&mut self, stmt: Rc<RefCell<AstNode>>) {
        self.preds
            .insert(stmt.as_ptr() as usize, self.previous.clone());
        self.previous.clear();
        match stmt.borrow().node {
            AstNodeType::Return(_) => {}
            _ => self.previous.push(Rc::downgrade(&stmt)),
        }
    }

    fn finish(self) -> HashMap<usize, Vec<Weak<RefCell<AstNode>>>> {
        // todo: link jumps
        self.preds
    }
}

impl AstRewriteVisitor for CFGBuilder {
    fn visit_if(
        &mut self,
        _cond: Rc<RefCell<AstNode>>,
        _then: Rc<RefCell<AstNode>>,
        _els: Rc<RefCell<AstNode>>,
    ) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_cond.clone());
        let common_preds = self.previous.clone();
        self.mark(_then.clone());
        self.rewrite(_then.clone());
        let branch1_preds = self.previous.clone();
        self.previous = common_preds;
        self.mark(_els.clone());
        self.rewrite(_els.clone());
        let branch2_preds = self.previous.clone();
        self.previous = branch1_preds
            .into_iter()
            .chain(branch2_preds.into_iter())
            .collect();
        None
    }

    fn visit_while(
        &mut self,
        _cond: Rc<RefCell<AstNode>>,
        _body: Rc<RefCell<AstNode>>,
    ) -> Option<Rc<RefCell<AstNode>>> {
        let common_preds = self.previous.clone();
        self.rewrite(_cond.clone());
        self.mark(_body.clone());
        self.rewrite(_body.clone());
        let branch1_preds = self.previous.clone();
        self.previous = common_preds
            .into_iter()
            .chain(branch1_preds.into_iter())
            .collect();
        None
    }

    fn visit_block(&mut self, _stmts: Vec<Rc<RefCell<AstNode>>>) -> Option<Rc<RefCell<AstNode>>> {
        for stmt in _stmts {
            self.mark(stmt.clone());
            self.rewrite(stmt.clone())
        }
        None
    }
}

struct DeadCodeRemovalCFG {
    pub cfg: HashMap<usize, Vec<Weak<RefCell<AstNode>>>>,
    pub unreachable: Vec<Rc<RefCell<AstNode>>>,
    pub dead: HashSet<usize>,
}

impl DeadCodeRemovalCFG {
    pub fn new(cfg: HashMap<usize, Vec<Weak<RefCell<AstNode>>>>) -> Self {
        Self {
            cfg,
            unreachable: vec![],
            dead: HashSet::new(),
        }
    }

    pub fn is_unreachable(&self, node: Rc<RefCell<AstNode>>) -> bool {
        // does node have predecessors?
        if let Some(preds) = self.cfg.get(&(node.as_ptr() as usize)) {
            // are all predecessors unreachable?
            preds
                .iter()
                .all(|p| self.dead.contains(&(p.as_ptr() as usize)))
        } else {
            // no predecessors, so it's unreachable
            true
        }
    }

    pub fn mark_unreachable(&mut self, node: Rc<RefCell<AstNode>>) {
        if self.dead.insert(node.as_ptr() as usize) {
            self.unreachable.push(node);
        }
    }

    pub fn parse(&mut self, node: Rc<RefCell<AstNode>>) {
        self.rewrite(node.clone());
        let mut count = self.dead.len();
        loop {
            self.rewrite(node.clone());
            if count == self.dead.len() {
                break;
            }
            count = self.dead.len();
        }
        for node in self.unreachable.clone() {
            node.borrow_mut().node = AstNodeType::Unit;
        }
    }
}

impl AstRewriteVisitor for DeadCodeRemovalCFG {
    fn visit_if(&mut self, _cond: Rc<RefCell<AstNode>>, _then: Rc<RefCell<AstNode>>, _els: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_cond.clone());
        self.rewrite(_then.clone());
        if self.is_unreachable(_then.clone()) {
            self.mark_unreachable(_then.clone());
        }
        self.rewrite(_els.clone());
        if self.is_unreachable(_els.clone()) {
            self.mark_unreachable(_els.clone());
        }
        None
    }

    fn visit_while(&mut self, _cond: Rc<RefCell<AstNode>>, _body: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.rewrite(_cond.clone());
        self.rewrite(_body.clone());
        if self.is_unreachable(_body.clone()) {
            self.mark_unreachable(_body.clone());
        }
        None
    }

    fn visit_block(&mut self, _stmts: Vec<Rc<RefCell<AstNode>>>) -> Option<Rc<RefCell<AstNode>>> {
        for stmt in _stmts {
            self.rewrite(stmt.clone());
            if self.is_unreachable(stmt.clone()) {
                self.mark_unreachable(stmt.clone());
            }
        }
        None
    }
}

/// Removes dead code marked as empty node
pub struct DeadCodeRemoval;

impl AstRewriteVisitor for DeadCodeRemoval {
    fn visit_block(&mut self, _stmts: Vec<Rc<RefCell<AstNode>>>) -> Option<Rc<RefCell<AstNode>>> {
        let mut new_stmts = Vec::new();
        for stmt in _stmts {
            self.rewrite(stmt.clone());
            if !matches!(stmt.clone().borrow().node, AstNodeType::Unit) {
                new_stmts.push(stmt);
            }
        }
        Some(AstNode::block(new_stmts, 0..0))
    }
}

pub struct DeadCodeRemovalPass;

impl AstTransformPass for DeadCodeRemovalPass {
    fn apply(self, tree: Rc<RefCell<AstNode>>) {
        DeadCodeRemovalHeurisic.rewrite(tree.clone());
        // println!("{:#?}", tree);
        let mut cfg = CFGBuilder::new();
        cfg.rewrite(tree.clone());
        let cfg = cfg.finish();
        println!("{:#?}", cfg);
        let mut dead_code_removal = DeadCodeRemovalCFG::new(cfg);
        dead_code_removal.parse(tree.clone());
        DeadCodeRemoval.rewrite(tree);
    }
}
