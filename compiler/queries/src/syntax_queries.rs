use wipple_core::{
    db::{Db, Node},
    facts::Syntax,
};
use wipple_syntax::file::{File, ParseError};

pub fn syntax_error(db: &Db, node: Node) -> Option<&ParseError> {
    db.get::<Syntax>(node)
        .and_then(|Syntax(syntax)| syntax.downcast_ref::<File>())
        .and_then(|file| file.error.as_ref())
}
