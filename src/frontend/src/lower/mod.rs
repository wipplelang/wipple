mod binding;
mod expr;
mod form;
mod info;
mod item;
mod stack;

use binding::*;
use expr::*;
use form::*;
use info::*;
use item::*;
use stack::*;

use serde::Serialize;
use std::{cell::RefCell, collections::HashMap};
use wipple_diagnostics::*;
use wipple_parser::LocalIntern;

#[derive(Serialize)]
pub struct File {
    pub program: SpannedItem,
    pub functions: HashMap<FunctionId, Function>,
}

pub use info::{DebugInfo, Function, FunctionId};
pub use item::*;

pub fn lower(file: wipple_parser::File, diagnostics: &mut Diagnostics) -> File {
    let mut info = Info::new(diagnostics);

    let program =
        SpannedExpr::from(wipple_parser::Expr::from(file)).lower_to_item(&Stack::root(), &mut info);

    File {
        program,
        functions: info.functions,
    }
}
