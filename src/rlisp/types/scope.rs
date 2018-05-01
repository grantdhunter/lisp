use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::{Expression, Func};

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub variable: HashMap<String, Expression>,
    pub funcs: HashMap<String, Func>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            parent: None,
            variable: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn set_parent(mut self, parent: &Rc<RefCell<Scope>>) -> Scope {
        self.parent = Some(parent.clone());
        self
    }

    pub fn boxup(self) -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(self))
    }

    pub fn get_var(&self, key: &String) -> Option<Expression> {
        if let Some(t) = self.variable.get(key) {
            return Some(t.clone());
        }

        if let Some(ref s) = self.parent {
            return s.borrow().get_var(key);
        }
        None
    }

    pub fn get_func<'a>(&'a self, key: &'a String) -> Option<Func> {
        if let Some(t) = self.funcs.get(key) {
            return Some(t.clone());
        }

        if let Some(ref p) = self.parent {
            return p.borrow().get_func(key);
        }
        None
    }
}
