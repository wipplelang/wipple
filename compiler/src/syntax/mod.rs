mod error;
mod format;
mod lexer;
mod names;
mod parser;

pub use error::*;
pub use format::*;
pub use lexer::*;
pub use names::*;
pub use parser::*;

use crate::{
    database::{Db, NodeRef},
    nodes::parse_file,
};

pub fn parse(db: &mut Db, path: impl AsRef<str>, source: impl AsRef<str>) -> NodeRef {
    let result = (|| -> Result<NodeRef, ParseError> {
        let mut parser = Parser::new(db, path.as_ref(), source.as_ref())?;
        let node = parser.node(parse_file)?;
        parser.finish()?;
        Ok(node)
    })();

    match result {
        Ok(node) => node,
        Err(error) => {
            let node = db.node(error.span.clone(), ParseErrorNode);
            db.insert(&node, error);
            node
        }
    }
}

#[cfg(test)]
pub fn test_parse(name: &str, f: fn(&mut Parser<'_>) -> Result<NodeRef, ParseError>, source: &str) {
    let mut db = Db::new();

    let result = (|| -> Result<NodeRef, ParseError> {
        let mut parser = Parser::new(&mut db, "test", source)?;
        parser.consume_line_breaks();
        let node = f(&mut parser)?;
        parser.consume_line_breaks();
        parser.finish()?;
        Ok(node)
    })()
    .expect("failed to parse");

    insta::with_settings!({
        snapshot_path => format!("{}/snapshots", env!("CARGO_MANIFEST_DIR")),
        prepend_module_to_snapshot => false,
        description => source,
    }, {
        insta::assert_debug_snapshot!(name, result);
    });
}
