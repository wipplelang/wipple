pub mod expression;
pub mod program;
pub mod typecheck;

use codemap_diagnostic::Diagnostic;
use expression::Expression;
use program::Context;
use wipple_bytecode::module;

pub fn compile<'a>(expr: Expression) -> (Option<module::File<'a>>, Vec<Diagnostic>) {
    let mut context = Context::default();

    todo!();

    let file = if context.diagnostics.is_empty() {
        Some(context.file)
    } else {
        None
    };

    (file, context.diagnostics)
}
