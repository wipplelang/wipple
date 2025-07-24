#![allow(missing_docs)]

mod attribute;
mod expression;
mod pattern;
mod statement;
mod top_level;
mod r#type;
mod type_function;
mod type_representation;
mod utils;

pub use attribute::*;
pub use expression::*;
pub use pattern::*;
pub use statement::*;
pub use top_level::*;
pub use r#type::*;
pub use type_function::*;
pub use type_representation::*;
pub use utils::*;

use crate::{
    syntax::{
        Location,
        parse::{Result, SyntaxKind, base},
        tokenize::TokenTree,
    },
    util::{DefaultFromInfo, WithInfo},
};

pub fn parse_rule<T>(tree: WithInfo<&TokenTree<'_>>, rule: base::Rule<T>) -> Result<T>
where
    Location: From<Location>,
    T: DefaultFromInfo + 'static,
{
    let mut parser = base::Parser::new();

    let stack = base::ParseStack::new(WithInfo {
        info: tree.info.clone(),
        item: SyntaxKind::TopLevel,
    });

    let parsed = rule.parse(&mut parser, tree, &stack, None);

    Result {
        parsed,
        diagnostics: parser.into_diagnostics(),
    }
}
