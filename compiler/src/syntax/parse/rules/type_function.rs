use crate::{
    syntax::{
        Location,
        parse::{SyntaxKind, Type, base::Rule, name, render::RuleToRender, r#type},
        tokenize::{Keyword, NonAssociativeOperator, TokenTree},
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub struct TypeFunction {
    pub parameters: Vec<WithInfo<TypeParameter>>,
    pub bounds: Vec<WithInfo<Instance>>,
}

impl DefaultFromInfo for TypeFunction {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: TypeFunction {
                parameters: Vec::new(),
                bounds: Vec::new(),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeParameter {
    pub name: WithInfo<Option<String>>,
    pub infer: Option<WithInfo<()>>,
    pub default: Option<WithInfo<Type>>,
}

impl DefaultFromInfo for TypeParameter {
    fn default_from_info(info: Location) -> WithInfo<Self> {
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

#[derive(Debug, PartialEq, Eq)]
pub struct Instance {
    pub r#trait: WithInfo<Option<String>>,
    pub parameters: Vec<WithInfo<Type>>,
}

impl DefaultFromInfo for Instance {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info: info.clone(),
            item: Instance {
                r#trait: Option::default_from_info(info),
                parameters: Vec::new(),
            },
        }
    }
}

pub fn type_function() -> Rule<TypeFunction> {
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

pub fn type_parameter() -> Rule<TypeParameter> {
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
                        info: Location::clone(&info),
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
                                            info: Location::clone(&info),
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

pub fn instance() -> Rule<Instance> {
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
