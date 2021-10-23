use crate::lower::*;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

#[derive(Clone)]
pub struct Stack<'a: 'p, 'p> {
    pub parent: Option<&'a Stack<'p, 'p>>,
    pub variables: RefCell<HashMap<LocalIntern<String>, Variable>>,
    pub captures: Option<Rc<RefCell<HashSet<VariableId>>>>,
}

impl<'a, 'p> Stack<'a, 'p> {
    pub fn root() -> Self {
        Stack {
            parent: None,
            variables: RefCell::new(builtins()),
            captures: None,
        }
    }

    pub fn child_block(&'a self) -> Self {
        Stack {
            parent: Some(self),
            variables: Default::default(),
            captures: None,
        }
    }

    pub fn child_function(&'a self) -> Self {
        Stack {
            parent: Some(self),
            variables: Default::default(),
            captures: Some(Default::default()),
        }
    }
}
