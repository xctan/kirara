use std::cell::RefCell;

use id_arena::{Arena, Id};

use super::value::{Value, ValueId};

#[derive(Debug)]
pub struct TransUnit {
    /// global value allocator
    pub values: RefCell<Arena<Value>>,
    // global variable defs
    // funcs

    // todo: move to bb
    pub instns: Vec<ValueId>,
    counter: usize,
}

impl TransUnit {
    pub fn new() -> Self {
        Self {
            values: RefCell::new(Arena::new()),
            instns: Vec::new(),
            counter: 0,
        }
    }

    pub fn gen_local_name(&mut self) -> String {
        let name = format!("%{}", self.counter);
        self.counter += 1;
        name
    }

    pub fn push_inst(&mut self, inst: ValueId) {
        self.instns.push(inst);
    }
}