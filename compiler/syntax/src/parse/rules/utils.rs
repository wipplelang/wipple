use crate::{
    parse::{base::Rule, render::RuleToRender, SyntaxKind},
    tokenize::TokenTree,
    Driver,
};

pub fn name<D: Driver>() -> Rule<D, String> {
    Rule::match_terminal(SyntaxKind::Name, RuleToRender::NAME, |_, tree, _| {
        tree.filter_map(|tree| match tree {
            TokenTree::Name(name) => Some(name.clone().into_owned()),
            _ => None,
        })
    })
}

pub fn text<D: Driver>() -> Rule<D, String> {
    Rule::match_terminal(SyntaxKind::Text, RuleToRender::TEXT, |_, tree, _| {
        tree.filter_map(|tree| match tree {
            TokenTree::Text(text) => Some(text.clone().into_owned()),
            _ => None,
        })
    })
}

pub fn number<D: Driver>() -> Rule<D, String> {
    Rule::match_terminal(SyntaxKind::Number, RuleToRender::NUMBER, |_, tree, _| {
        tree.filter_map(|tree| match tree {
            TokenTree::Number(number) => Some(number.clone().into_owned()),
            _ => None,
        })
    })
}
