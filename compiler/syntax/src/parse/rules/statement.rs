use crate::{
    parse::{
        attribute, base::Rule, expression, instance, name, pattern, r#type, render::RuleToRender,
        text, type_function, type_representation, Attribute, Attributed, Expression, Instance,
        Pattern, SyntaxKind, Type, TypeFunction, TypeRepresentation,
    },
    tokenize::{Keyword, NonAssociativeOperator},
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
pub enum Statement<D: Driver> {
    Error,
    #[serde(rename_all = "camelCase")]
    SyntaxDeclaration {
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
        name: WithInfo<D::Info, String>,
    },
    #[serde(rename_all = "camelCase")]
    TypeDeclaration {
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
        name: WithInfo<D::Info, Option<String>>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        representation: WithInfo<D::Info, TypeRepresentation<D>>,
    },
    #[serde(rename_all = "camelCase")]
    TraitDeclaration {
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
        name: WithInfo<D::Info, Option<String>>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        r#type: Option<WithInfo<D::Info, Type<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    DefaultInstanceDeclaration {
        pattern: WithInfo<D::Info, ()>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        instance: WithInfo<D::Info, Option<Instance<D>>>,
        body: Option<WithInfo<D::Info, Expression<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    InstanceDeclaration {
        pattern: WithInfo<D::Info, ()>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        instance: WithInfo<D::Info, Option<Instance<D>>>,
        body: Option<WithInfo<D::Info, Expression<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    ConstantDeclaration {
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
        name: WithInfo<D::Info, Option<String>>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
    #[serde(rename_all = "camelCase")]
    Assignment {
        pattern: WithInfo<D::Info, Pattern<D>>,
        value: WithInfo<D::Info, Expression<D>>,
    },
    Expression(WithInfo<D::Info, Expression<D>>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for Statement<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Statement::Error,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageDeclarationKind {
    Type,
    Trait,
    Constant,
}

pub fn statement<D: Driver>() -> Rule<D, Statement<D>> {
    Rule::switch(
        SyntaxKind::Statement,
        [
            syntax_declaration,
            type_declaration,
            trait_declaration,
            default_instance_declaration,
            instance_declaration,
            constant_declaration,
            assignment,
            || expression().map(SyntaxKind::Statement, Statement::Expression),
        ],
    )
    .no_backtrack()
    .named("A statement.")
}

pub fn syntax_declaration<D: Driver>() -> Rule<D, Statement<D>> {
    Rule::attributed_keyword1(
        SyntaxKind::SyntaxDeclaration,
        Keyword::Intrinsic,
        attribute,
        || text().wrapped(),
        |_, info, attributes, syntax, _| WithInfo {
            info,
            item: Attributed {
                attributes,
                value: syntax,
            },
        },
    )
    .map(SyntaxKind::SyntaxDeclaration, |declaration| {
        Statement::SyntaxDeclaration {
            attributes: declaration.item.attributes,
            name: declaration.item.value.map(Option::unwrap),
        }
    })
    .named("A syntax declaration.")
}

pub fn type_declaration<D: Driver>() -> Rule<D, Statement<D>> {
    Rule::non_associative_operator(
        SyntaxKind::TypeDeclaration,
        NonAssociativeOperator::Assign,
        || name().wrapped().attributed_with(attribute()).in_list(),
        || {
            Rule::switch(
                SyntaxKind::TypeDeclaration,
                [
                    || {
                        Rule::keyword0(
                            SyntaxKind::TypeRepresentation,
                            Keyword::Type,
                            |_, info, _| <(_, _)>::default_from_info(info),
                        )
                    },
                    || {
                        Rule::keyword1(
                            SyntaxKind::TypeRepresentation,
                            Keyword::Type,
                            type_representation,
                            |_, info: D::Info, representation, _| WithInfo {
                                info: info.clone(),
                                item: (TypeFunction::default_from_info(info), representation),
                            },
                        )
                    },
                    || {
                        Rule::non_associative_operator(
                            SyntaxKind::TypeRepresentation,
                            NonAssociativeOperator::TypeFunction,
                            type_function,
                            || {
                                Rule::keyword0(
                                    SyntaxKind::TypeRepresentation,
                                    Keyword::Type,
                                    |_, info, _| TypeRepresentation::default_from_info(info),
                                )
                            },
                            |_, info, type_function, representation, _| WithInfo {
                                info,
                                item: (type_function, representation),
                            },
                        )
                    },
                    || {
                        Rule::non_associative_operator(
                            SyntaxKind::TypeRepresentation,
                            NonAssociativeOperator::TypeFunction,
                            type_function,
                            || {
                                Rule::keyword1(
                                    SyntaxKind::TypeRepresentation,
                                    Keyword::Type,
                                    type_representation,
                                    |_, _, representation, _| representation,
                                )
                            },
                            |_, info, type_function, representation, _| WithInfo {
                                info,
                                item: (type_function, representation),
                            },
                        )
                    },
                ],
            )
        },
        |_, info, name, declaration, _| {
            let (parameters, representation) = declaration.item;

            WithInfo {
                info,
                item: Statement::TypeDeclaration {
                    attributes: name.item.attributes,
                    name: name.item.value,
                    parameters,
                    representation,
                },
            }
        },
    )
    .named("A type declaration.")
}

pub fn trait_declaration<D: Driver>() -> Rule<D, Statement<D>> {
    Rule::non_associative_operator(
        SyntaxKind::TraitDeclaration,
        NonAssociativeOperator::Assign,
        || name().wrapped().attributed_with(attribute()).in_list(),
        || {
            Rule::switch(
                SyntaxKind::TraitDeclaration,
                [
                    || {
                        Rule::keyword0(
                            SyntaxKind::TraitDeclaration,
                            Keyword::Trait,
                            |_, info: D::Info, _| WithInfo {
                                info: info.clone(),
                                item: (
                                    TypeFunction::default_from_info(info.clone()),
                                    Option::default_from_info(info),
                                ),
                            },
                        )
                    },
                    || {
                        Rule::keyword1(
                            SyntaxKind::TraitDeclaration,
                            Keyword::Trait,
                            || r#type().no_backtrack(),
                            |_, info: D::Info, r#type, _| WithInfo {
                                info: info.clone(),
                                item: (TypeFunction::default_from_info(info), r#type.map(Some)),
                            },
                        )
                    },
                    || {
                        Rule::non_associative_operator(
                            SyntaxKind::TraitDeclaration,
                            NonAssociativeOperator::TypeFunction,
                            type_function,
                            || {
                                Rule::switch(
                                    SyntaxKind::TraitDeclaration,
                                    [
                                        || {
                                            Rule::keyword0(
                                                SyntaxKind::TraitDeclaration,
                                                Keyword::Trait,
                                                |_, info: D::Info, _| {
                                                    Option::default_from_info(info)
                                                },
                                            )
                                        },
                                        || {
                                            Rule::keyword1(
                                                SyntaxKind::TraitDeclaration,
                                                Keyword::Trait,
                                                || r#type().no_backtrack(),
                                                |_, _, r#type, _| r#type.map(Some),
                                            )
                                        },
                                    ],
                                )
                            },
                            |_, info, type_function, r#type, _| WithInfo {
                                info,
                                item: (type_function, r#type),
                            },
                        )
                    },
                ],
            )
        },
        |_, info, name, declaration, _| {
            let (parameters, r#type) = declaration.item;

            WithInfo {
                info,
                item: Statement::TraitDeclaration {
                    attributes: name.item.attributes,
                    name: name.item.value,
                    parameters,
                    r#type: r#type.try_unwrap(),
                },
            }
        },
    )
    .named("A trait declaration.")
}

pub fn default_instance_declaration<D: Driver>() -> Rule<D, Statement<D>> {
    Rule::switch(
        SyntaxKind::InstanceDeclaration,
        [
            || {
                Rule::non_associative_operator(
                    SyntaxKind::InstanceDeclaration,
                    NonAssociativeOperator::Assign,
                    || {
                        Rule::switch(
                            SyntaxKind::InstanceDeclaration,
                            [
                                || {
                                    Rule::non_associative_operator(
                                        SyntaxKind::InstanceDeclaration,
                                        NonAssociativeOperator::TypeFunction,
                                        type_function,
                                        || {
                                            Rule::contextual_keyword2(
                                                SyntaxKind::InstanceDeclaration,
                                                String::from("default"),
                                                || {
                                                    Rule::match_terminal(
                                                        SyntaxKind::InstanceDeclaration,
                                                        RuleToRender::Keyword(
                                                            Keyword::Instance.to_string(),
                                                        ),
                                                        |_, info, _| Some(info.replace(())),
                                                    )
                                                },
                                                || instance().wrapped(),
                                                |_, _, _, instance, _| instance,
                                            )
                                        },
                                        |_, info, type_function, instance, _| WithInfo {
                                            info,
                                            item: (type_function, instance),
                                        },
                                    )
                                },
                                || {
                                    Rule::contextual_keyword2(
                                        SyntaxKind::InstanceDeclaration,
                                        String::from("default"),
                                        || {
                                            Rule::match_terminal(
                                                SyntaxKind::InstanceDeclaration,
                                                RuleToRender::Keyword(
                                                    Keyword::Instance.to_string(),
                                                ),
                                                |_, info, _| Some(info.replace(())),
                                            )
                                        },
                                        || instance().wrapped(),
                                        |_, info: D::Info, _, instance, _| WithInfo {
                                            info: info.clone(),
                                            item: (TypeFunction::default_from_info(info), instance),
                                        },
                                    )
                                },
                            ],
                        )
                    },
                    expression,
                    |_, info, declaration, body, _| {
                        let (parameters, instance) = declaration.item;

                        WithInfo {
                            info,
                            item: Statement::DefaultInstanceDeclaration {
                                pattern: WithInfo {
                                    info: declaration.info,
                                    item: (),
                                },
                                parameters,
                                instance,
                                body: Some(body),
                            },
                        }
                    },
                )
            },
            || {
                Rule::switch(
                    SyntaxKind::InstanceDeclaration,
                    [
                        || {
                            Rule::non_associative_operator(
                                SyntaxKind::InstanceDeclaration,
                                NonAssociativeOperator::TypeFunction,
                                type_function,
                                || {
                                    Rule::contextual_keyword2(
                                        SyntaxKind::InstanceDeclaration,
                                        String::from("default"),
                                        || {
                                            Rule::match_terminal(
                                                SyntaxKind::InstanceDeclaration,
                                                RuleToRender::Keyword(
                                                    Keyword::Instance.to_string(),
                                                ),
                                                |_, info, _| Some(info.replace(())),
                                            )
                                        },
                                        || instance().wrapped(),
                                        |_, _, _, instance, _| instance,
                                    )
                                },
                                |_, info, type_function, instance, _| WithInfo {
                                    info,
                                    item: Statement::DefaultInstanceDeclaration {
                                        pattern: WithInfo {
                                            info: D::Info::clone(&type_function.info),
                                            item: (),
                                        },
                                        parameters: type_function,
                                        instance,
                                        body: None,
                                    },
                                },
                            )
                        },
                        || {
                            Rule::contextual_keyword2(
                                SyntaxKind::InstanceDeclaration,
                                String::from("default"),
                                || {
                                    Rule::match_terminal(
                                        SyntaxKind::InstanceDeclaration,
                                        RuleToRender::Keyword(Keyword::Instance.to_string()),
                                        |_, info, _| Some(info.replace(())),
                                    )
                                },
                                || instance().wrapped(),
                                |_, info: D::Info, _, instance, _| WithInfo {
                                    info: info.clone(),
                                    item: Statement::DefaultInstanceDeclaration {
                                        pattern: WithInfo {
                                            info: info.clone(),
                                            item: (),
                                        },
                                        parameters: TypeFunction::default_from_info(info),
                                        instance,
                                        body: None,
                                    },
                                },
                            )
                        },
                    ],
                )
            },
        ],
    )
    .named("A default instance declaration.")
}

pub fn instance_declaration<D: Driver>() -> Rule<D, Statement<D>> {
    Rule::switch(
        SyntaxKind::InstanceDeclaration,
        [
            || {
                Rule::non_associative_operator(
                    SyntaxKind::InstanceDeclaration,
                    NonAssociativeOperator::Assign,
                    || {
                        Rule::switch(
                            SyntaxKind::InstanceDeclaration,
                            [
                                || {
                                    Rule::non_associative_operator(
                                        SyntaxKind::InstanceDeclaration,
                                        NonAssociativeOperator::TypeFunction,
                                        type_function,
                                        || {
                                            Rule::keyword1(
                                                SyntaxKind::InstanceDeclaration,
                                                Keyword::Instance,
                                                || instance().wrapped(),
                                                |_, _, instance, _| instance,
                                            )
                                        },
                                        |_, info, type_function, instance, _| WithInfo {
                                            info,
                                            item: (type_function, instance),
                                        },
                                    )
                                },
                                || {
                                    Rule::keyword1(
                                        SyntaxKind::InstanceDeclaration,
                                        Keyword::Instance,
                                        || instance().wrapped(),
                                        |_, info: D::Info, instance, _| WithInfo {
                                            info: info.clone(),
                                            item: (TypeFunction::default_from_info(info), instance),
                                        },
                                    )
                                },
                            ],
                        )
                    },
                    expression,
                    |_, info, declaration, body, _| {
                        let (parameters, instance) = declaration.item;

                        WithInfo {
                            info,
                            item: Statement::InstanceDeclaration {
                                pattern: WithInfo {
                                    info: declaration.info,
                                    item: (),
                                },
                                parameters,
                                instance,
                                body: Some(body),
                            },
                        }
                    },
                )
            },
            || {
                Rule::switch(
                    SyntaxKind::InstanceDeclaration,
                    [
                        || {
                            Rule::non_associative_operator(
                                SyntaxKind::InstanceDeclaration,
                                NonAssociativeOperator::TypeFunction,
                                type_function,
                                || {
                                    Rule::keyword1(
                                        SyntaxKind::InstanceDeclaration,
                                        Keyword::Instance,
                                        || instance().wrapped(),
                                        |_, _, instance, _| instance,
                                    )
                                },
                                |_, info, type_function, instance, _| WithInfo {
                                    info,
                                    item: Statement::InstanceDeclaration {
                                        pattern: WithInfo {
                                            info: D::Info::clone(&type_function.info),
                                            item: (),
                                        },
                                        parameters: type_function,
                                        instance,
                                        body: None,
                                    },
                                },
                            )
                        },
                        || {
                            Rule::keyword1(
                                SyntaxKind::InstanceDeclaration,
                                Keyword::Instance,
                                || instance().wrapped(),
                                |_, info: D::Info, instance, _| WithInfo {
                                    info: info.clone(),
                                    item: Statement::InstanceDeclaration {
                                        pattern: WithInfo {
                                            info: info.clone(),
                                            item: (),
                                        },
                                        parameters: TypeFunction::default_from_info(info),
                                        instance,
                                        body: None,
                                    },
                                },
                            )
                        },
                    ],
                )
            },
        ],
    )
    .named("An instance declaration.")
}

pub fn constant_declaration<D: Driver>() -> Rule<D, Statement<D>> {
    Rule::non_associative_operator(
        SyntaxKind::ConstantDeclaration,
        NonAssociativeOperator::Annotate,
        || {
            name()
                .wrapped()
                .attributed_with(attribute())
                .in_list()
                .no_backtrack()
        },
        || {
            Rule::switch(
                SyntaxKind::ConstantDeclaration,
                [
                    || {
                        Rule::non_associative_operator(
                            SyntaxKind::TypeFunction,
                            NonAssociativeOperator::TypeFunction,
                            type_function,
                            r#type,
                            |_, info, type_function, r#type, _| WithInfo {
                                info,
                                item: (type_function, r#type),
                            },
                        )
                    },
                    || {
                        r#type().map(SyntaxKind::Type, |r#type| {
                            (
                                TypeFunction::default_from_info(D::Info::clone(&r#type.info)),
                                r#type,
                            )
                        })
                    },
                ],
            )
        },
        |_, info, name, declaration, _| {
            let (parameters, r#type) = declaration.item;

            WithInfo {
                info,
                item: Statement::ConstantDeclaration {
                    attributes: name.item.attributes,
                    name: name.item.value,
                    parameters,
                    r#type,
                },
            }
        },
    )
    .named("A constant declaration.")
}

pub fn assignment<D: Driver>() -> Rule<D, Statement<D>> {
    Rule::non_associative_operator(
        SyntaxKind::Assignment,
        NonAssociativeOperator::Assign,
        pattern,
        expression,
        |_, info, pattern, value, _| WithInfo {
            info,
            item: Statement::Assignment { pattern, value },
        },
    )
    .named("Assign a value to a pattern.")
}
