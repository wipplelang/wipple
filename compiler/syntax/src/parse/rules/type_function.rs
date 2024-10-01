use crate::{
    parse::{base::Rule, name, r#type, render::RuleToRender, SyntaxKind, Type},
    tokenize::{Keyword, NonAssociativeOperator, TokenTree},
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
pub struct TypeFunction<D: Driver> {
    pub parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,
    pub bounds: Vec<WithInfo<D::Info, Instance<D>>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeFunction<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: TypeFunction {
                parameters: Vec::new(),
                bounds: Vec::new(),
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
pub struct TypeParameter<D: Driver> {
    pub name: WithInfo<D::Info, Option<String>>,
    pub infer: Option<WithInfo<D::Info, ()>>,
    pub default: Option<WithInfo<D::Info, Type<D>>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeParameter<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: TypeParameter {
                name: Option::default_from_info(info),
                infer: None,
                default: None,
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
pub struct Instance<D: Driver> {
    pub r#trait: WithInfo<D::Info, Option<String>>,
    pub parameters: Vec<WithInfo<D::Info, Type<D>>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for Instance<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: Instance {
                r#trait: Option::default_from_info(info),
                parameters: Vec::new(),
            },
        }
    }
}

pub fn type_function<D: Driver>() -> Rule<D, TypeFunction<D>> {
    Rule::switch(
        SyntaxKind::TypeFunction,
        [
            || {
                Rule::list(
                    SyntaxKind::TypeParameter,
                    || type_parameter().no_backtrack(),
                    |_, info, parameters, _| WithInfo {
                        info,
                        item: TypeFunction {
                            parameters,
                            bounds: Vec::new(),
                        },
                    },
                )
            },
            || {
                Rule::non_associative_operator(
                    SyntaxKind::TypeFunction,
                    NonAssociativeOperator::Where,
                    || {
                        Rule::switch(
                            SyntaxKind::TypeParameter,
                            [
                                || {
                                    Rule::match_terminal(
                                        SyntaxKind::TypeParameter,
                                        RuleToRender::UNDERSCORE,
                                        |_, tree, _| match tree.item {
                                            TokenTree::Keyword(Keyword::Underscore) => {
                                                Some(tree.replace(Vec::new()))
                                            }
                                            _ => None,
                                        },
                                    )
                                    .in_list()
                                },
                                || {
                                    Rule::list(
                                        SyntaxKind::TypeParameter,
                                        || type_parameter().no_backtrack(),
                                        |_, info, parameters, _| WithInfo {
                                            info,
                                            item: parameters,
                                        },
                                    )
                                },
                            ],
                        )
                    },
                    || {
                        Rule::list(
                            SyntaxKind::Instance,
                            || instance().no_backtrack(),
                            |_, info, bounds, _| WithInfo { info, item: bounds },
                        )
                    },
                    |_, info, parameters, bounds, _| WithInfo {
                        info,
                        item: TypeFunction {
                            parameters: parameters.item,
                            bounds: bounds.item,
                        },
                    },
                )
            },
        ],
    )
    .named("Provides generic type parameters and bounds to a declaration.")
}

pub fn type_parameter<D: Driver>() -> Rule<D, TypeParameter<D>> {
    Rule::switch(
        SyntaxKind::TypeParameter,
        [
            || {
                name()
                    .wrapped()
                    .map(SyntaxKind::TypeParameter, |name| TypeParameter {
                        name,
                        default: None,
                        infer: None,
                    })
            },
            || {
                Rule::contextual_keyword1(
                    SyntaxKind::TypeParameter,
                    String::from("infer"),
                    || name().wrapped(),
                    |_, info, name, _| WithInfo {
                        info: D::Info::clone(&info),
                        item: TypeParameter {
                            name,
                            default: None,
                            infer: Some(WithInfo { info, item: () }),
                        },
                    },
                )
            },
            || {
                Rule::non_associative_operator(
                    SyntaxKind::TypeParameter,
                    NonAssociativeOperator::Assign,
                    || {
                        Rule::switch(
                            SyntaxKind::TypeParameter,
                            [
                                || {
                                    name().wrapped().in_list().map(
                                        SyntaxKind::TypeParameter,
                                        |name| {
                                            (
                                                name.clone(),
                                                WithInfo {
                                                    info: name.info,
                                                    item: None,
                                                },
                                            )
                                        },
                                    )
                                },
                                || {
                                    Rule::contextual_keyword1(
                                        SyntaxKind::TypeParameter,
                                        String::from("infer"),
                                        || name().wrapped(),
                                        |_, info, name, _| WithInfo {
                                            info: D::Info::clone(&info),
                                            item: (
                                                name,
                                                WithInfo {
                                                    info,
                                                    item: Some(()),
                                                },
                                            ),
                                        },
                                    )
                                },
                            ],
                        )
                    },
                    r#type,
                    |_, info, name, r#type, _| {
                        let (name, default) = name.item;

                        WithInfo {
                            info,
                            item: TypeParameter {
                                name,
                                default: Some(r#type),
                                infer: default.try_unwrap(),
                            },
                        }
                    },
                )
            },
        ],
    )
    .no_backtrack()
    .named("A type parameter.")
}

pub fn instance<D: Driver>() -> Rule<D, Instance<D>> {
    Rule::list_prefix(
        SyntaxKind::Instance,
        || name().wrapped(),
        r#type,
        |_, info, r#trait, parameters, _| WithInfo {
            info,
            item: Instance {
                r#trait,
                parameters,
            },
        },
    )
    .no_backtrack()
    .named("An instance.")
}
