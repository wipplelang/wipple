use crate::QueryCtx;
use wipple_core::{db::Node, facts::Syntax};
use wipple_syntax::file::{File, ParseError};

pub fn syntax_error<'a>(db: &QueryCtx<'a>, node: Node) -> Option<&'a ParseError> {
    db.get::<Syntax>(node)
        .and_then(|Syntax(syntax)| db.ast(syntax).downcast_ref::<File>())
        .and_then(|file| file.error.as_ref())
}
