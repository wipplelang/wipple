mod binding;
mod expr;
mod forms;
mod info;
mod item;
mod stack;

use std::{collections::HashMap, sync::Arc};

pub use binding::*;
pub use expr::*;
pub use forms::*;
pub use info::*;
pub use item::*;
pub use stack::*;

use crate::id::*;
use serde::Serialize;
use wipple_diagnostics::*;
use interned_string::InternedString;

#[derive(Debug, Clone, Serialize)]
pub struct File {
    pub id: FileId,
    pub path: InternedString,
    pub statements: Vec<Item>,
    pub variables: HashMap<InternedString, Variable>,
}

pub fn lower(file: wipple_parser::File, info: &mut Info) -> Option<Arc<File>> {
    let stack = Stack::file(file.path);

    let statements = file
        .statements
        .into_iter()
        .filter_map(parse_statement)
        .map(|statement| statement.lower_to_item(&stack, info))
        .collect::<Option<_>>()?;

    let file = Arc::new(File {
        id: FileId::new(),
        path: file.path,
        statements,
        variables: stack.variables.into_inner(),
    });

    info.files.push(file.clone());

    Some(file)
}
