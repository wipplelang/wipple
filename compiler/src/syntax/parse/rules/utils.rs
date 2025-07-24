use crate::syntax::{
    parse::{SyntaxKind, base::Rule, render::RuleToRender},
    tokenize::TokenTree,
};

pub fn name() -> Rule<String> {
    Rule::match_terminal(SyntaxKind::Name, RuleToRender::NAME, |_, tree, _| {
        tree.filter_map(|tree| match tree {
            TokenTree::Name(name) => Some(name.clone().into_owned()),
            _ => None,
        })
    })
}

pub fn text() -> Rule<String> {
    Rule::match_terminal(SyntaxKind::Text, RuleToRender::TEXT, |_, tree, _| {
        tree.filter_map(|tree| match tree {
            TokenTree::Text(text) => Some(text.clone().into_owned()),
            _ => None,
        })
    })
}

pub fn number() -> Rule<String> {
    Rule::match_terminal(SyntaxKind::Number, RuleToRender::NUMBER, |_, tree, _| {
        tree.filter_map(|tree| match tree {
            TokenTree::Number(number) => Some(number.clone().into_owned()),
            _ => None,
        })
    })
}
