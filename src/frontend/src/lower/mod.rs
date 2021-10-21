mod binding;
mod diagnostics;
mod expr;
mod form;
mod item;
mod stack;

use binding::*;
use diagnostics::*;
use expr::*;
use form::*;
use item::*;
use stack::*;

use std::cell::RefCell;

pub fn lower(
    file: wipple_parser::File,
    diagnostics: &mut wipple_diagnostics::Diagnostics,
) -> SpannedItem {
    let variables = RefCell::new(builtins());
    let stack = Stack::root(Scope::Block {
        variables: &variables,
    });

    SpannedExpr::from(wipple_parser::Expr::from(file))
        .lower_to_item(stack, &mut Diagnostics::new(diagnostics))
}
