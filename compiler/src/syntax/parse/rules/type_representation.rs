use crate::{
    syntax::{
        Location,
        parse::{Attribute, SyntaxKind, Type, attribute, base::Rule, name, r#type},
        tokenize::NonAssociativeOperator,
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeRepresentation {
    Marker,
    Compound(Vec<WithInfo<TypeMember>>),
    Wrapper(WithInfo<Type>),
}

impl DefaultFromInfo for TypeRepresentation {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: TypeRepresentation::Marker,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeMember {
    pub attributes: Vec<WithInfo<Attribute>>,
    pub name: WithInfo<Option<String>>,
    pub kind: TypeMemberKind,
}

impl DefaultFromInfo for TypeMember {
    fn default_from_info(info: Location) -> WithInfo<Self> {
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

#[derive(Debug, PartialEq, Eq)]
pub enum TypeMemberKind {
    Error,
    Field(WithInfo<Type>),
    Variant(Vec<WithInfo<Type>>),
}

impl DefaultFromInfo for TypeMemberKind {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: TypeMemberKind::Error,
        }
    }
}

pub fn type_representation() -> Rule<TypeRepresentation> {
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
