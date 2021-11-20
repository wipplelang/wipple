use crate::compile::*;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Clone)]
pub struct Stack<'a: 'p, 'p> {
    pub parent: Option<&'a Stack<'p, 'p>>,
    pub file_path: Option<InternedString>,
    pub variables: RefCell<HashMap<InternedString, Variable>>,
    pub captures: Option<Arc<RefCell<HashSet<VariableId>>>>,
}

impl<'a, 'p> Stack<'a, 'p> {
    pub fn file(path: InternedString) -> Self {
        Stack {
            parent: None,
            file_path: Some(path),
            variables: RefCell::new(builtins()),
            captures: None,
        }
    }

    pub fn child_block(&'a self) -> Self {
        Stack {
            parent: Some(self),
            file_path: None,
            variables: Default::default(),
            captures: None,
        }
    }

    pub fn child_function(&'a self) -> Self {
        Stack {
            parent: Some(self),
            file_path: None,
            variables: Default::default(),
            captures: Some(Default::default()),
        }
    }
}
