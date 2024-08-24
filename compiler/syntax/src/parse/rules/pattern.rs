use crate::{
    parse::{
        base::Rule, name, number, r#type, render::RuleToRender, text, Expression, SyntaxKind, Type,
    },
    tokenize::{Keyword, NonAssociativeOperator, Operator, TokenTree, VariadicOperator},
    Driver,
};
use derivative::Derivative;
use serde::Deserialize;
use std::fmt::Debug;
use wipple_util::{DefaultFromInfo, WithInfo};

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Pattern<D: Driver> {
    Error,
    Wildcard,
    Unit,
    Number(String),
    Text(String),
    Name(String),
    VariantOrName(WithInfo<D::Info, Option<String>>),
    Destructure(Vec<WithInfo<D::Info, FieldPattern<D>>>),
    #[serde(rename_all = "camelCase")]
    Variant {
        variant: WithInfo<D::Info, Option<String>>,
        value_patterns: Vec<WithInfo<D::Info, Pattern<D>>>,
    },
    Tuple(Vec<WithInfo<D::Info, Pattern<D>>>),
    Or {
        left: WithInfo<D::Info, Box<Pattern<D>>>,
        right: WithInfo<D::Info, Box<Pattern<D>>>,
    },
    Mutate(WithInfo<D::Info, Option<String>>),
    Annotate {
        pattern: WithInfo<D::Info, Box<Pattern<D>>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
}

impl<D: Driver> DefaultFromInfo<D::Info> for Pattern<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Pattern::Error,
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct FieldPattern<D: Driver> {
    pub name: WithInfo<D::Info, Option<String>>,
    pub pattern: WithInfo<D::Info, Pattern<D>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for FieldPattern<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: FieldPattern {
                name: Option::default_from_info(info.clone()),
                pattern: Pattern::default_from_info(info),
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct FieldValue<D: Driver> {
    pub name: WithInfo<D::Info, Option<String>>,
    pub value: WithInfo<D::Info, Expression<D>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for FieldValue<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: FieldValue {
                name: Option::default_from_info(info.clone()),
                value: Expression::default_from_info(info),
            },
        }
    }
}

pub fn pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    Rule::switch(
        SyntaxKind::Pattern,
        [
            wildcard_pattern,
            number_pattern,
            text_pattern,
            destructure_pattern,
            tuple_pattern,
            or_pattern,
            mutate_pattern,
            variant_pattern,
            annotate_pattern,
        ],
    )
    .unwrap_parentheses()
    .no_backtrack()
    .named("A pattern.")
}

pub fn wildcard_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    Rule::match_terminal(
        SyntaxKind::WildcardPattern,
        RuleToRender::UNDERSCORE,
        |_, tree, _| match tree.item {
            TokenTree::Keyword(Keyword::Underscore) => Some(tree.replace(Pattern::Wildcard)),
            _ => None,
        },
    )
    .named("A pattern that matches any value.")
}

pub fn number_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    number()
        .wrapped()
        .map(SyntaxKind::NumberPattern, |number| {
            Pattern::Number(number.item.unwrap())
        })
        .named("A pattern that matches a number.")
}

pub fn text_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    text()
        .wrapped()
        .map(SyntaxKind::TextPattern, |text| {
            Pattern::Text(text.item.unwrap())
        })
        .named("A pattern that matches a piece of text.")
}

pub fn variant_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    Rule::switch(
        SyntaxKind::VariantPattern,
        [
            || {
                Rule::empty_list(SyntaxKind::VariantPattern, |info| WithInfo {
                    info,
                    item: Pattern::Unit,
                })
            },
            || {
                name()
                    .wrapped()
                    .map(SyntaxKind::VariantPattern, Pattern::VariantOrName)
            },
            || {
                Rule::list_prefix(
                    SyntaxKind::VariantPattern,
                    || name().wrapped(),
                    pattern,
                    |_, info, variant, value_patterns, _| WithInfo {
                        info,
                        item: if value_patterns.is_empty() {
                            Pattern::VariantOrName(variant)
                        } else {
                            Pattern::Variant {
                                variant,
                                value_patterns,
                            }
                        },
                    },
                )
            },
        ],
    )
    .named("A pattern that matches a variant or binds to a variable.")
}

pub fn destructure_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    Rule::block(
        SyntaxKind::DestructurePattern,
        || {
            Rule::non_associative_operator(
                SyntaxKind::StructureField,
                NonAssociativeOperator::Assign,
                || name().wrapped().in_list().no_backtrack(),
                pattern,
                |_, info, name, pattern, _| WithInfo {
                    info,
                    item: FieldPattern { name, pattern },
                },
            )
        },
        |_, info, patterns, _| WithInfo {
            info,
            item: Pattern::Destructure(patterns),
        },
    )
    .named("A pattern that matches the fields of a structure.")
}

pub fn tuple_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    Rule::variadic_operator(
        SyntaxKind::TuplePattern,
        VariadicOperator::Tuple,
        pattern,
        |_, info, patterns, _| WithInfo {
            info,
            item: Pattern::Tuple(patterns),
        },
    )
    .named("A pattern that matches a tuple.")
}

pub fn or_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    Rule::operator(
        SyntaxKind::OrPattern,
        Operator::Or,
        pattern,
        pattern,
        |_, info, left, right, _| WithInfo {
            info,
            item: Pattern::Or {
                left: left.boxed(),
                right: right.boxed(),
            },
        },
    )
    .named("A pattern that matches either one of its subpatterns.")
}

pub fn mutate_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    Rule::mutate(
        SyntaxKind::MutatePattern,
        || name().wrapped(),
        |_, info, name, _| WithInfo {
            info,
            item: Pattern::Mutate(name),
        },
    )
    .named("A pattern that changes the value of an existing variable.")
}

pub fn annotate_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
    Rule::non_associative_operator(
        SyntaxKind::AnnotatePattern,
        NonAssociativeOperator::Annotate,
        pattern,
        r#type,
        |_, info, pattern, r#type, _| WithInfo {
            info,
            item: Pattern::Annotate {
                pattern: pattern.boxed(),
                r#type,
            },
        },
    )
    .named("Annotate a pattern with a type.")
}
