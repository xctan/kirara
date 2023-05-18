use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

use crate::ast::{AstNode, AstNodeType, AstFuncData};

use super::{AstRewriteVisitor, AstTransformPass};

/// Replace false `if`s and `while`s with empty node
struct DeadCodeRemovalHeuristic;

impl AstRewriteVisitor for DeadCodeRemovalHeuristic {
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
    pub labels: HashMap<String, usize>,
    pub gotos: Vec<(usize, String)>,
    pub useless_labels: HashSet<String>,
}

impl CFGBuilder {
    fn new() -> Self {
        let mut instance = Self {
            entry: AstNode::unit(0..0),
            preds: HashMap::new(),
            succs: HashMap::new(),
            previous: vec![],
            labels: HashMap::new(),
            gotos: vec![],
            useless_labels: HashSet::new(),
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
            AstNodeType::GotoStmt(ref goto) => self
                .gotos
                .push((stmt.as_ptr() as usize, goto.label.clone())),
            AstNodeType::LabelStmt(ref label) => {
                self.labels.insert(label.label.clone(), stmt.as_ptr() as usize);
                self.previous.push(stmt.as_ptr() as usize);
            }
            _ => self.previous.push(stmt.as_ptr() as usize),
        }
    }

    fn finish(mut self) -> Self {
        let mut useful_labels = HashSet::new();
        // link jumps
        self.gotos.iter()
            .for_each(|(k, v)| {
                let target = self.labels.get(v).unwrap();
                // self.succs.entry(*k).or_default().push(*target);
                self.preds.entry(*target).or_default().push(*k);
                useful_labels.insert(v);
            });
        // calculate useless labels
        self.labels.iter()
            .filter(|(k, _)| !useful_labels.contains(k))
            .for_each(|(k, _)| {
                self.useless_labels.insert(k.clone());
            });
        // calculate successors from predecessors
        self.preds.iter()
            .for_each(|(k, v)| {
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
            match _stmt.borrow().node.clone() {
                // remove redundant labels, possibly generated by while statements
                AstNodeType::LabelStmt(labeled) => {
                    if self.cfg.useless_labels.contains(&labeled.label) {
                        Some(labeled.body)
                    } else {
                        None
                    }
                }
                _ => None,
            }
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
    fn apply(self, tree: &mut AstFuncData) {
        DeadCodeRemovalHeuristic.rewrite(tree.body.clone());
        // println!("{:#?}", tree);
        let mut cfg = CFGBuilder::new();
        cfg.rewrite(tree.body.clone());
        let cfg = cfg.finish();
        // println!("{:#?}", cfg);
        let mut dce_cfg = DeadCodeRemovalCFG::new(cfg).parse();
        dce_cfg.rewrite(tree.body.clone());
        DeadCodeRemoval.rewrite(tree.body.clone());
    }
}
