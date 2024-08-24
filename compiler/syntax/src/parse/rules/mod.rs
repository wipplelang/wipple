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
pub use r#type::*;
pub use statement::*;
pub use top_level::*;
pub use type_function::*;
pub use type_representation::*;
pub use utils::*;

use crate::{
    parse::{base, Result, SyntaxKind},
    tokenize::TokenTree,
    Driver, Location,
};
use wipple_util::{DefaultFromInfo, WithInfo};

pub fn parse_rule<D: Driver, T>(
    driver: &D,
    tree: WithInfo<D::Info, &TokenTree<'_, D>>,
    rule: base::Rule<D, T>,
) -> Result<D, T>
where
    D::Info: From<Location>,
    T: DefaultFromInfo<D::Info> + 'static,
{
    let mut parser = base::Parser::new(driver);

    let stack = base::ParseStack::<D>::new(WithInfo {
        info: tree.info.clone(),
        item: SyntaxKind::TopLevel,
    });

    let parsed = rule.parse(&mut parser, tree, &stack, None);

    Result {
        parsed,
        diagnostics: parser.into_diagnostics(),
    }
}
