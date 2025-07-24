use crate::{
    syntax::{
        Location,
        parse::{
            Attribute, Attributed, Expression, Instance, Pattern, SyntaxKind, Type, TypeFunction,
            TypeRepresentation, attribute, base::Rule, expression, instance, name, pattern,
            render::RuleToRender, text, r#type, type_function, type_representation,
        },
        tokenize::{Keyword, NonAssociativeOperator},
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Error,
    SyntaxDeclaration {
        attributes: Vec<WithInfo<Attribute>>,
        name: WithInfo<String>,
    },
    TypeDeclaration {
        attributes: Vec<WithInfo<Attribute>>,
        name: WithInfo<Option<String>>,
        parameters: WithInfo<TypeFunction>,
        representation: WithInfo<TypeRepresentation>,
    },
    TraitDeclaration {
        attributes: Vec<WithInfo<Attribute>>,
        name: WithInfo<Option<String>>,
        parameters: WithInfo<TypeFunction>,
        r#type: Option<WithInfo<Type>>,
    },
    DefaultInstanceDeclaration {
        pattern: WithInfo<()>,
        parameters: WithInfo<TypeFunction>,
        instance: WithInfo<Option<Instance>>,
        body: Option<WithInfo<Expression>>,
    },
    InstanceDeclaration {
        pattern: WithInfo<()>,
        parameters: WithInfo<TypeFunction>,
        instance: WithInfo<Option<Instance>>,
        body: Option<WithInfo<Expression>>,
    },
    ConstantDeclaration {
        attributes: Vec<WithInfo<Attribute>>,
        name: WithInfo<Option<String>>,
        parameters: WithInfo<TypeFunction>,
        r#type: WithInfo<Type>,
    },
    Assignment {
        pattern: WithInfo<Pattern>,
        value: WithInfo<Expression>,
    },
    Expression(WithInfo<Expression>),
}

impl DefaultFromInfo for Statement {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: Statement::Error,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageDeclarationKind {
    Type,
    Trait,
    Constant,
}

pub fn statement() -> Rule<Statement> {
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

pub fn syntax_declaration() -> Rule<Statement> {
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

pub fn type_declaration() -> Rule<Statement> {
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
                            |_, info: Location, representation, _| WithInfo {
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

pub fn trait_declaration() -> Rule<Statement> {
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
                            |_, info: Location, _| WithInfo {
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
                            |_, info: Location, r#type, _| WithInfo {
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
                                                |_, info: Location, _| {
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

pub fn default_instance_declaration() -> Rule<Statement> {
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
                                        |_, info: Location, _, instance, _| WithInfo {
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
                                            info: Location::clone(&type_function.info),
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
                                |_, info: Location, _, instance, _| WithInfo {
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

pub fn instance_declaration() -> Rule<Statement> {
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
                                        |_, info: Location, instance, _| WithInfo {
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
                                            info: Location::clone(&type_function.info),
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
                                |_, info: Location, instance, _| WithInfo {
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

pub fn constant_declaration() -> Rule<Statement> {
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
                                TypeFunction::default_from_info(Location::clone(&r#type.info)),
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

pub fn assignment() -> Rule<Statement> {
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
