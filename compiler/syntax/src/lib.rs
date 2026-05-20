pub mod attributes;
pub mod checks;
pub mod constraints;
pub mod expressions;
pub mod file;
pub mod patterns;
pub mod statements;
pub mod types;

use crate::{expressions::IsExpression, file::parse_file, patterns::IsPattern, types::IsType};
use wipple_core::{
    arcstr::ArcStr,
    db::{Db, Node},
    visit::Visit,
};
use wipple_parse::parser::Parser;

pub fn parse(path: impl Into<ArcStr>, source: impl Into<ArcStr>) -> Box<dyn Visit> {
    let file = Parser::new(path, source)
        .and_then(|mut parser| {
            let file = parse_file(&mut parser)?;
            parser.finish()?;
            Ok(file)
        })
        .unwrap_or_else(|error| error.into());

    Box::new(file)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum GroupOrder {
    Pattern,
    Expression,
    Type,
    Other,
}

impl GroupOrder {
    pub fn new(db: &Db, node: Node) -> Self {
        if db.contains::<IsPattern>(node) {
            GroupOrder::Pattern
        } else if db.contains::<IsExpression>(node) {
            GroupOrder::Expression
        } else if db.contains::<IsType>(node) {
            GroupOrder::Type
        } else {
            GroupOrder::Other
        }
    }
}
