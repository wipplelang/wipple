use crate::{
    syntax::{
        Location,
        parse::{Statement, SyntaxKind, base::Rule, render::RuleToRender, statement},
        tokenize::TokenTree,
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub struct TopLevel {
    pub statements: Vec<WithInfo<Statement>>,
}

impl DefaultFromInfo for TopLevel {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: TopLevel {
                statements: Vec::new(),
            },
        }
    }
}

pub fn top_level() -> Rule<TopLevel> {
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
