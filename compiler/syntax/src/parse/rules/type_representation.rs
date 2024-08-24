use crate::{
    parse::{attribute, base::Rule, name, r#type, Attribute, SyntaxKind, Type},
    tokenize::NonAssociativeOperator,
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
pub enum TypeRepresentation<D: Driver> {
    Marker,
    Compound(Vec<WithInfo<D::Info, TypeMember<D>>>),
    Wrapper(WithInfo<D::Info, Type<D>>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeRepresentation<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: TypeRepresentation::Marker,
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
pub struct TypeMember<D: Driver> {
    pub attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
    pub name: WithInfo<D::Info, Option<String>>,
    pub kind: TypeMemberKind<D>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeMember<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: TypeMember {
                attributes: Vec::new(),
                name: Option::default_from_info(info),
                kind: TypeMemberKind::Error,
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
pub enum TypeMemberKind<D: Driver> {
    Error,
    Field(WithInfo<D::Info, Type<D>>),
    Variant(Vec<WithInfo<D::Info, Type<D>>>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeMemberKind<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: TypeMemberKind::Error,
        }
    }
}

pub fn type_representation<D: Driver>() -> Rule<D, TypeRepresentation<D>> {
    Rule::switch(
        SyntaxKind::TypeRepresentation,
        [
            || {
                Rule::block(
                    SyntaxKind::TypeRepresentation,
                    || {
                        Rule::switch(
                            SyntaxKind::TypeMember,
                            [
                                || {
                                    Rule::non_associative_operator(
                                        SyntaxKind::FieldDeclaration,
                                        NonAssociativeOperator::Annotate,
                                        || {
                                            name()
                                                .wrapped()
                                                .attributed_with(attribute())
                                                .in_list()
                                                .no_backtrack()
                                        },
                                        r#type,
                                        |_, info, name, r#type, _| WithInfo {
                                            info,
                                            item: TypeMember {
                                                attributes: name.item.attributes,
                                                name: name.item.value,
                                                kind: TypeMemberKind::Field(r#type),
                                            },
                                        },
                                    )
                                },
                                || {
                                    Rule::list_prefix(
                                        SyntaxKind::VariantDeclaration,
                                        || {
                                            name()
                                                .wrapped()
                                                .attributed_with(attribute())
                                                .no_backtrack()
                                        },
                                        r#type,
                                        |_, info, name, types, _| WithInfo {
                                            info,
                                            item: TypeMember {
                                                attributes: name.item.attributes,
                                                name: name.item.value,
                                                kind: TypeMemberKind::Variant(types),
                                            },
                                        },
                                    )
                                },
                            ],
                        )
                    },
                    |_, info, members, _| WithInfo {
                        info,
                        item: TypeRepresentation::Compound(members),
                    },
                )
            },
            || {
                r#type().map(SyntaxKind::Type, |r#type| {
                    TypeRepresentation::Wrapper(r#type)
                })
            },
        ],
    )
    .no_backtrack()
    .named("A set of fields or variants in a type.")
}
