use crate::{
    syntax::{
        Location,
        parse::{SyntaxKind, base::Rule, name, number, text},
        tokenize::NonAssociativeOperator,
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub struct Attributed<T> {
    pub attributes: Vec<WithInfo<Attribute>>,
    pub value: WithInfo<T>,
}

impl<T> DefaultFromInfo for Attributed<T>
where
    T: DefaultFromInfo,
{
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info: info.clone(),
            item: Attributed {
                attributes: Vec::new(),
                value: T::default_from_info(info),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Attribute {
    Error,
    Name(WithInfo<String>),
    Valued {
        name: WithInfo<String>,
        value: WithInfo<AttributeValue>,
    },
}

impl DefaultFromInfo for Attribute {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: Attribute::Error,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AttributeValue {
    Error,
    Name(WithInfo<String>),
    Number(WithInfo<String>),
    Text(WithInfo<String>),
}

impl DefaultFromInfo for AttributeValue {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: AttributeValue::Error,
        }
    }
}

pub fn attribute() -> Rule<Attribute> {
    Rule::switch(
        SyntaxKind::Attribute,
        [
            || {
                name()
                    .wrapped()
                    .map(SyntaxKind::Attribute, |name| {
                        Attribute::Name(name.map(Option::unwrap))
                    })
                    .named("A name.")
            },
            || {
                Rule::non_associative_operator(
                    SyntaxKind::Attribute,
                    NonAssociativeOperator::Assign,
                    || name().wrapped().in_list(),
                    || attribute_value().in_list(),
                    |_, info, name, value, _| WithInfo {
                        info,
                        item: Attribute::Valued {
                            name: name.map(Option::unwrap),
                            value,
                        },
                    },
                )
            },
        ],
    )
    .named("An attribute.")
}

pub fn attribute_value() -> Rule<AttributeValue> {
    Rule::switch(
        SyntaxKind::AttributeValue,
        [
            || {
                name().wrapped().map(SyntaxKind::AttributeValue, |name| {
                    AttributeValue::Name(name.map(Option::unwrap))
                })
            },
            || {
                number()
                    .wrapped()
                    .map(SyntaxKind::AttributeValue, |number| {
                        AttributeValue::Number(number.map(Option::unwrap))
                    })
            },
            || {
                text().wrapped().map(SyntaxKind::AttributeValue, |text| {
                    AttributeValue::Text(text.map(Option::unwrap))
                })
            },
        ],
    )
    .named("An attribute value.")
}
