use crate::{
    parse::{base::Rule, name, number, text, SyntaxKind},
    tokenize::NonAssociativeOperator,
    Driver,
};
use derivative::Derivative;
use serde::Deserialize;
use std::fmt::Debug;
use wipple_util::{DefaultFromInfo, WithInfo};

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug, T: Debug"),
    Clone(bound = "T: Clone"),
    PartialEq(bound = "D::Info: PartialEq, T: PartialEq"),
    Eq(bound = "D::Info: Eq, T: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize<'de>"))]
pub struct Attributed<D: Driver, T> {
    pub attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
    pub value: WithInfo<D::Info, T>,
}

impl<D: Driver, T> DefaultFromInfo<D::Info> for Attributed<D, T>
where
    T: DefaultFromInfo<D::Info>,
{
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: Attributed {
                attributes: Vec::new(),
                value: T::default_from_info(info),
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
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Attribute<D: Driver> {
    Error,
    Name(WithInfo<D::Info, String>),
    Valued {
        name: WithInfo<D::Info, String>,
        value: WithInfo<D::Info, AttributeValue<D>>,
    },
}

impl<D: Driver> DefaultFromInfo<D::Info> for Attribute<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Attribute::Error,
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
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum AttributeValue<D: Driver> {
    Error,
    Name(WithInfo<D::Info, String>),
    Number(WithInfo<D::Info, String>),
    Text(WithInfo<D::Info, String>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for AttributeValue<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: AttributeValue::Error,
        }
    }
}

pub fn attribute<D: Driver>() -> Rule<D, Attribute<D>> {
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

pub fn attribute_value<D: Driver>() -> Rule<D, AttributeValue<D>> {
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
