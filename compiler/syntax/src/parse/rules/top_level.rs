use crate::{
    parse::{base::Rule, render::RuleToRender, statement, Statement, SyntaxKind},
    tokenize::TokenTree,
    Driver,
};
use derivative::Derivative;
use serde::Deserialize;
use std::fmt::Debug;
use wipple_util::{DefaultFromInfo, WithInfo};

#[allow(missing_docs)]
#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct TopLevel<D: Driver> {
    pub statements: Vec<WithInfo<D::Info, Statement<D>>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for TopLevel<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: TopLevel {
                statements: Vec::new(),
            },
        }
    }
}

pub fn top_level<D: Driver>() -> Rule<D, TopLevel<D>> {
    Rule::switch(
        SyntaxKind::TopLevel,
        [
            || {
                Rule::match_terminal(
                    SyntaxKind::TopLevel,
                    RuleToRender::Block(Vec::new()),
                    |_, tree, _| match tree.item {
                        TokenTree::EmptyFile => Some(TopLevel::default_from_info(tree.info)),
                        _ => None,
                    },
                )
            },
            || {
                Rule::block(SyntaxKind::TopLevel, statement, |_, info, statements, _| {
                    WithInfo {
                        info,
                        item: TopLevel { statements },
                    }
                })
            },
        ],
    )
    .named("A file or code box.")
}
