use crate::{
    syntax::{
        Location,
        parse::{
            Expression, SyntaxKind, Type, base::Rule, name, number, render::RuleToRender, text,
            r#type,
        },
        tokenize::{Keyword, NonAssociativeOperator, Operator, TokenTree, VariadicOperator},
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub enum Pattern {
    Error,
    Wildcard,
    Number(String),
    Text(String),
    Name(String),
    VariantOrName(WithInfo<Option<String>>),
    Destructure(Vec<WithInfo<FieldPattern>>),
    Variant {
        variant: WithInfo<Option<String>>,
        value_patterns: Vec<WithInfo<Pattern>>,
    },
    Tuple(Vec<WithInfo<Pattern>>),
    Or {
        left: WithInfo<Box<Pattern>>,
        right: WithInfo<Box<Pattern>>,
    },
    Mutate(WithInfo<Option<String>>),
    Annotate {
        pattern: WithInfo<Box<Pattern>>,
        r#type: WithInfo<Type>,
    },
}

impl DefaultFromInfo for Pattern {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: Pattern::Error,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FieldPattern {
    pub name: WithInfo<Option<String>>,
    pub pattern: WithInfo<Pattern>,
}

impl DefaultFromInfo for FieldPattern {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info: info.clone(),
            item: FieldPattern {
                name: Option::default_from_info(info.clone()),
                pattern: Pattern::default_from_info(info),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FieldValue {
    pub name: WithInfo<Option<String>>,
    pub value: WithInfo<Expression>,
}

impl DefaultFromInfo for FieldValue {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info: info.clone(),
            item: FieldValue {
                name: Option::default_from_info(info.clone()),
                value: Expression::default_from_info(info),
            },
        }
    }
}

pub fn pattern() -> Rule<Pattern> {
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

pub fn wildcard_pattern() -> Rule<Pattern> {
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

pub fn number_pattern() -> Rule<Pattern> {
    number()
        .wrapped()
        .map(SyntaxKind::NumberPattern, |number| {
            Pattern::Number(number.item.unwrap())
        })
        .named("A pattern that matches a number.")
}

pub fn text_pattern() -> Rule<Pattern> {
    text()
        .wrapped()
        .map(SyntaxKind::TextPattern, |text| {
            Pattern::Text(text.item.unwrap())
        })
        .named("A pattern that matches a piece of text.")
}

pub fn variant_pattern() -> Rule<Pattern> {
    Rule::switch(
        SyntaxKind::VariantPattern,
        [
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

pub fn destructure_pattern() -> Rule<Pattern> {
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

pub fn tuple_pattern() -> Rule<Pattern> {
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

pub fn or_pattern() -> Rule<Pattern> {
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

pub fn mutate_pattern() -> Rule<Pattern> {
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

pub fn annotate_pattern() -> Rule<Pattern> {
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
