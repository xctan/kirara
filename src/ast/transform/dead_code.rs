use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
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
    pub preds: HashMap<usize, Vec<usize>>,
    pub succs: HashMap<usize, Vec<usize>>,
    pub previous: Vec<usize>,
    // pub labels: HashMap<String, Weak<RefCell<AstNode>>>,
    // pub jumps: Vec<(String, Rc<RefCell<AstNode>>)>,
}

impl CFGBuilder {
    fn new() -> Self {
        let mut instance = Self {
            entry: AstNode::unit(0..0),
            preds: HashMap::new(),
            succs: HashMap::new(),
            previous: vec![],
        };
        instance.previous.push(instance.entry.as_ptr() as usize);
        instance
    }

    fn mark(&mut self, stmt: Rc<RefCell<AstNode>>) {
        self.preds
            .insert(stmt.as_ptr() as usize, self.previous.clone());
        self.previous.clear();
        match stmt.borrow().node {
            AstNodeType::Return(_) => {}
            _ => self.previous.push(stmt.as_ptr() as usize),
        }
    }

    fn finish(mut self) -> Self {
        // todo: link jumps
        self.preds.iter().for_each(|(k, v)| {
            v.iter().for_each(|p| {
                self.succs.entry(*p).or_default().push(*k);
            })
        });
        self
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
        self.rewrite(_then.clone());
        let branch1_preds = self.previous.clone();
        self.previous = common_preds;
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
        self.rewrite(_body.clone());
        let branch1_preds = self.previous.clone();
        // while (true)
        if !matches!(_cond.borrow().node, AstNodeType::I1Number(true)) {
            self.previous = common_preds
                .into_iter()
                .chain(branch1_preds.into_iter())
                .collect();
        } else {
            self.previous.clear();
        }
        None
    }

    fn before_statement(&mut self, _stmt: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        self.mark(_stmt.clone());
        None
    }
}

struct DeadCodeRemovalCFG {
    pub cfg: CFGBuilder,
    pub liveset: HashSet<usize>,
}

impl DeadCodeRemovalCFG {
    pub fn new(cfg: CFGBuilder) -> Self {
        Self {
            cfg,
            liveset: HashSet::new(),
        }
    }

    pub fn parse(mut self) -> Self {
        let mut queue = VecDeque::new();
        queue.push_back(self.cfg.entry.as_ptr() as usize);
        while let Some(stmt) = queue.pop_front() {
            if self.liveset.contains(&stmt) {
                continue;
            }
            self.liveset.insert(stmt);
            if let Some(succs) = self.cfg.succs.get(&stmt) {
                for succ in succs {
                    queue.push_back(*succ);
                }
            }
        }
        self
    }
}

impl AstRewriteVisitor for DeadCodeRemovalCFG {
    fn before_statement(&mut self, _stmt: Rc<RefCell<AstNode>>) -> Option<Rc<RefCell<AstNode>>> {
        if !self.liveset.contains(&(_stmt.as_ptr() as usize)) {
            AstNode::unit(0..0).into()
        } else {
            None
        }
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
        // println!("{:#?}", cfg);
        let mut dce_cfg = DeadCodeRemovalCFG::new(cfg).parse();
        dce_cfg.rewrite(tree.clone());
        DeadCodeRemoval.rewrite(tree);
    }
}
