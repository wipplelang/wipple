mod binding;
mod expr;
mod file_attribute;
mod form;
mod info;
mod item;
mod stack;

pub use binding::*;
pub use expr::*;
pub use file_attribute::*;
pub use form::*;
pub use info::*;
pub use item::*;
pub use stack::*;

use crate::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct File {
    pub id: FileId,
    pub name: InternedString,
    pub span: Span,
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
        name: file.path,
        span: file.span,
        statements,
        variables: stack.variables.into_inner(),
    });

    info.files.push(file.clone());

    Some(file)
}
