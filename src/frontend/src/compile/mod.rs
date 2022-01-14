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
    let stack = Stack::file(builtin_variables(), FileInfo::new(file.path));

    evaluate_attributes(file.attributes, &stack, info)?;

    if let Stack::File(variables, file_info) = &stack {
        if file_info.borrow().include_prelude {
            let mut variables = variables.borrow_mut();

            // The prelude is always the first file loaded
            let prelude = info.files.first().unwrap();

            for (name, variable) in &prelude.variables {
                variables.entry(*name).or_insert_with(|| variable.clone());
            }
        }
    } else {
        unreachable!();
    }

    let statements = file
        .statements
        .into_iter()
        .filter_map(parse_statement)
        .map(|statement| statement.lower_to_item(&stack, info))
        .collect();

    let variables = match stack {
        Stack::File(variables, _) => variables,
        _ => unreachable!(),
    };

    let file = Arc::new(File {
        id: FileId::new(),
        name: file.path,
        span: file.span,
        statements,
        variables: variables.into_inner(),
    });

    info.files.push(file.clone());

    Some(file)
}
