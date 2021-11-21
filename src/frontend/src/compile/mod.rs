mod binding;
mod expr;
mod file_attribute;
mod info;
mod item;
mod stack;

pub use binding::*;
pub use expr::*;
pub use file_attribute::*;
pub use info::*;
pub use item::*;
pub use stack::*;

use crate::{id::*, prelude};
use interned_string::InternedString;
use serde::Serialize;
use std::{collections::HashMap, sync::Arc};
use wipple_diagnostics::*;

#[derive(Debug, Clone, Serialize)]
pub struct File {
    pub id: FileId,
    pub path: InternedString,
    pub statements: Vec<Item>,
    pub variables: HashMap<InternedString, Variable>,
}

pub fn lower(file: wipple_parser::File, info: &mut Info) -> Option<Arc<File>> {
    let stack = Stack::file(FileInfo::new(file.path));

    evaluate_attributes(file.attributes, &stack, info)?;

    if stack.file_info.as_ref().unwrap().borrow().include_prelude {
        let mut variables = stack.variables.borrow_mut();
        for (name, variable) in &prelude().variables {
            variables.entry(*name).or_insert_with(|| variable.clone());
        }
    }

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
