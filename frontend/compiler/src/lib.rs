pub mod expression;
pub mod program;
pub mod typecheck;

use codemap::CodeMap;
use codemap_diagnostic::Diagnostic;
use expression::Expression;
use program::Context;
use std::path::Path;
use wipple_bytecode::module;

pub fn compile_file<'a>(
    expr: Expression,
    // TODO: Better handling of parse errors
    dependencies: impl Fn(&Path) -> Option<Expression>,
) -> (Option<module::File<'a>>, CodeMap, Vec<Diagnostic>) {
    let mut context = Context::default();

    todo!();

    let file = if context.success {
        Some(context.file)
    } else {
        None
    };

    (file, context.codemap, context.diagnostics)
}
