pub mod attributes;
pub mod checks;
pub mod constraints;
pub mod expressions;
pub mod file;
pub mod patterns;
pub mod statements;
pub mod types;

use crate::file::parse_file;
use wipple_core::{ast::AstKey, db::Db, span::Str};
use wipple_parse::parser::Parser;

pub fn parse(db: &mut Db, path: impl Into<Str>, source: impl Into<Str>) -> AstKey {
    let file = Parser::new(db, path, source)
        .and_then(|mut parser| {
            let file = parse_file(&mut parser)?;
            parser.finish()?;
            Ok(file)
        })
        .unwrap_or_else(|error| error.into());

    let key = db.in_ast(Box::new(file));

    db.gc();

    key
}
