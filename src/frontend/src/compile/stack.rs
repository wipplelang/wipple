use crate::{compile::*, *};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

#[derive(Clone)]
pub struct Stack<'a: 'p, 'p> {
    pub parent: Option<&'a Stack<'p, 'p>>,
    pub file_info: Option<RefCell<FileInfo>>,
    pub variables: RefCell<HashMap<InternedString, Variable>>,
    pub captures: Option<Arc<RefCell<HashSet<VariableId>>>>,
}

impl<'a, 'p> Stack<'a, 'p> {
    pub fn file(info: FileInfo) -> Self {
        Stack {
            parent: None,
            file_info: Some(RefCell::new(info)),
            variables: RefCell::new(builtin_variables()),
            captures: None,
        }
    }

    pub fn child_block(&'a self) -> Self {
        Stack {
            parent: Some(self),
            file_info: None,
            variables: Default::default(),
            captures: None,
        }
    }

    pub fn child_function(&'a self) -> Self {
        Stack {
            parent: Some(self),
            file_info: None,
            variables: Default::default(),
            captures: Some(Default::default()),
        }
    }
}

#[derive(Clone)]
pub struct FileInfo {
    pub path: InternedString,
    pub include_prelude: bool,
}

impl FileInfo {
    pub fn new(path: InternedString) -> Self {
        FileInfo {
            path,
            include_prelude: true,
        }
    }
}
