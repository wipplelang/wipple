mod binding;
mod builtin;
mod expr;
mod lowered;
mod scope;
mod variable;

pub use binding::*;
pub use builtin::*;
pub use expr::*;
pub use lowered::*;
pub use scope::*;
pub use variable::*;

use wipple_diagnostics::*;
use wipple_parser as parser;

pub fn lower(file: parser::File, diagnostics: &mut Diagnostics) -> LoweredExpr {
    AnyExpr::from(parser::Expr::from(file)).lower(&mut Scope::root(), diagnostics)
}
