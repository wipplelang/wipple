use crate::{compile::*, *};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

pub type Variables = HashMap<InternedString, Variable>;

#[derive(Clone)]
pub enum Stack<'a: 'p, 'p> {
    File(RefCell<Variables>, RefCell<FileInfo>),
    Block(RefCell<Variables>, &'a Stack<'p, 'p>),
    Function(
        RefCell<Variables>,
        RefCell<HashSet<VariableId>>,
        &'a Stack<'p, 'p>,
    ),
    Introduction(InternedString, Variable, &'a Stack<'p, 'p>),
}

impl<'a, 'p> Stack<'a, 'p> {
    pub fn file(variables: Variables, info: FileInfo) -> Self {
        Stack::File(RefCell::new(variables), RefCell::new(info))
    }

    pub fn child_block(&'a self) -> Self {
        Stack::Block(Default::default(), self)
    }

    pub fn child_function(&'a self) -> Self {
        Stack::Function(Default::default(), Default::default(), self)
    }

    pub fn child_introduction(&'a self, name: InternedString, variable: Variable) -> Self {
        Stack::Introduction(name, variable, self)
    }

    pub fn parent(&'a self) -> Option<&'a Stack<'p, 'p>> {
        match self {
            Stack::File(..) => None,
            Stack::Block(.., parent)
            | Stack::Function(.., parent)
            | Stack::Introduction(.., parent) => Some(parent),
        }
    }

    pub fn with_variables<T>(&'a self, f: impl FnOnce(&mut Variables) -> T) -> T {
        let mut stack = Some(self);
        while let Some(s) = stack {
            match s {
                Stack::File(variables, ..)
                | Stack::Block(variables, _)
                | Stack::Function(variables, _, _) => {
                    return f(&mut *variables.borrow_mut());
                }
                Stack::Introduction(..) => {
                    stack = s.parent();
                }
            }
        }

        panic!("Stack does not contain a block scope");
    }

    pub fn resolve(&'a self, name: InternedString, info: &mut Info) -> Option<Variable> {
        let mut stack = Some(self);
        let mut variable = None;
        let mut used = vec![info.used_variables.as_ref()];
        while let Some(s) = stack {
            match s {
                Stack::File(variables, _) | Stack::Block(variables, _) => {
                    if let Some(v) = variables.borrow().get(&name) {
                        variable = Some(v.clone());
                        break;
                    } else {
                        stack = s.parent();
                    }
                }
                Stack::Function(variables, captures, _) => {
                    if let Some(v) = variables.borrow().get(&name) {
                        variable = Some(v.clone());
                        break;
                    } else {
                        used.push(captures);
                        stack = s.parent();
                    }
                }
                Stack::Introduction(n, v, _) => {
                    if name == *n {
                        variable = Some(v.clone());
                        break;
                    } else {
                        stack = s.parent();
                    }
                }
            }
        }

        if let Some(variable) = &variable {
            for list in used {
                list.borrow_mut().insert(variable.id);
            }
        }

        variable
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
