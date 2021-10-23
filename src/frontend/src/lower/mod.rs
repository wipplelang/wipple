pub mod binding;
pub mod expr;
pub mod form;
pub mod info;
pub mod item;
pub mod stack;

pub use binding::*;
pub use expr::*;
pub use form::*;
pub use info::*;
pub use item::*;
pub use stack::*;

use crate::id::*;
use wipple_diagnostics::*;
use wipple_parser::LocalIntern;

pub fn lower(file: wipple_parser::File, diagnostics: &mut Diagnostics) -> Item {
    let mut info = Info::new(diagnostics);

    let expr = Expr::from(wipple_parser::Expr::from(file));
    expr.lower_to_item(&Stack::root(), &mut info)
}
