pub mod binding;
pub mod expr;
pub mod form;
pub mod info;
pub mod item;
pub mod stack;

use std::{collections::HashMap, rc::Rc};

pub use binding::*;
pub use expr::*;
pub use form::*;
pub use info::*;
pub use item::*;
pub use stack::*;

use crate::id::*;
use serde::Serialize;
use wipple_diagnostics::*;
use wipple_parser::LocalIntern;

#[derive(Debug, Clone, Serialize)]
pub struct File {
    pub id: FileId,
    pub path: LocalIntern<String>,
    pub statements: Vec<Item>,
    pub variables: HashMap<LocalIntern<String>, Variable>,
}

pub fn lower(file: wipple_parser::File, info: &mut Info) {
    let stack = Stack::file(file.path);

    let statements = file
        .statements
        .into_iter()
        .filter_map(parse_statement)
        .map(|statement| statement.lower_to_item(&stack, info))
        .collect();

    info.files.push(Rc::new(File {
        id: FileId::new(),
        path: file.path,
        statements,
        variables: stack.variables.into_inner(),
    }));
}
