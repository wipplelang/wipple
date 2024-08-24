use crate::{
    parse::{base::Rule, name, render::RuleToRender, text, SyntaxKind},
    tokenize::{Keyword, Operator, TokenTree, VariadicOperator},
    Driver,
};
use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use wipple_util::{DefaultFromInfo, WithInfo};

#[allow(missing_docs)]
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Type<D: Driver> {
    Error,
    Placeholder,
    Unit,
    #[serde(rename_all = "camelCase")]
    Declared {
        name: WithInfo<D::Info, Option<String>>,
        parameters: Vec<WithInfo<D::Info, Type<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    Function {
        inputs: Vec<WithInfo<D::Info, Type<D>>>,
        output: WithInfo<D::Info, Box<Type<D>>>,
    },
    Tuple(Vec<WithInfo<D::Info, Type<D>>>),
    Block(WithInfo<D::Info, Box<Type<D>>>),
    Intrinsic,
    Message {
        message: WithInfo<D::Info, String>,
        inputs: Vec<WithInfo<D::Info, Type<D>>>,
    },
    Equal {
        left: WithInfo<D::Info, Box<Type<D>>>,
        right: WithInfo<D::Info, Box<Type<D>>>,
    },
}

impl<D: Driver> DefaultFromInfo<D::Info> for Type<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Type::Error,
        }
    }
}

pub fn r#type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::switch(
        SyntaxKind::Type,
        [
            placeholder_type,
            function_type,
            tuple_type,
            block_type,
            intrinsic_type,
            message_type,
            equal_type,
            declared_type,
        ],
    )
    .unwrap_parentheses()
    .no_backtrack()
    .named("A type.")
}

pub fn placeholder_type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::match_terminal(
        SyntaxKind::PlaceholderType,
        RuleToRender::UNDERSCORE,
        |_, tree, _| match tree.item {
            TokenTree::Keyword(Keyword::Underscore) => Some(tree.replace(Type::Placeholder)),
            _ => None,
        },
    )
    .named("An inferred type.")
}

pub fn declared_type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::switch(
        SyntaxKind::DeclaredType,
        [
            || {
                Rule::empty_list(SyntaxKind::DeclaredType, |info| WithInfo {
                    info,
                    item: Type::Unit,
                })
            },
            || {
                name().wrapped().map(SyntaxKind::DeclaredType, |name| {
                    // Special case: `Unit` is equivalent to `()`
                    if name.item.as_deref() == Some("Unit") {
                        Type::Unit
                    } else {
                        Type::Declared {
                            name,
                            parameters: Vec::new(),
                        }
                    }
                })
            },
            || {
                Rule::list_prefix(
                    SyntaxKind::DeclaredType,
                    || name().wrapped(),
                    r#type,
                    |parser, info, name, types, stack| WithInfo {
                        info,
                        // Special case: `Unit` is equivalent to `()` and
                        // does not accept parameters
                        item: if name.item.as_deref() == Some("Unit") {
                            for r#type in types {
                                parser.add_diagnostic(
                                    stack.error_expected(r#type.replace(SyntaxKind::Nothing), None),
                                );
                            }

                            Type::Unit
                        } else {
                            Type::Declared {
                                name,
                                parameters: types,
                            }
                        },
                    },
                )
            },
        ],
    )
    .named("A declared type, optionally with parameters.")
}

pub fn function_type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::operator(
        SyntaxKind::FunctionType,
        Operator::Function,
        || {
            Rule::switch(
                SyntaxKind::FunctionInputs,
                [
                    || {
                        Rule::list(SyntaxKind::FunctionInputs, r#type, |_, info, types, _| {
                            WithInfo { info, item: types }
                        })
                    },
                    || r#type().map(SyntaxKind::FunctionInputs, |r#type| vec![r#type]),
                ],
            )
        },
        r#type,
        |_, info, inputs, output, _| WithInfo {
            info,
            item: Type::Function {
                inputs: inputs.item,
                output: output.boxed(),
            },
        },
    )
    .named("A function type.")
}

pub fn tuple_type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::variadic_operator(
        SyntaxKind::TupleType,
        VariadicOperator::Tuple,
        r#type,
        |_, info, types, _| WithInfo {
            info,
            item: Type::Tuple(types),
        },
    )
    .named("A tuple type.")
}

pub fn block_type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::switch(
        SyntaxKind::BlockType,
        [
            || {
                Rule::empty_block(SyntaxKind::BlockType, |info| WithInfo {
                    info: D::Info::clone(&info),
                    item: Type::Block(
                        WithInfo {
                            info,
                            item: Type::Unit,
                        }
                        .boxed(),
                    ),
                })
            },
            || {
                Rule::block(
                    SyntaxKind::BlockType,
                    r#type,
                    |parser, info, types, stack| {
                        let mut types = types.into_iter();

                        let r#type = match types.next() {
                            Some(r#type) => r#type,
                            None => {
                                parser.add_diagnostic(stack.error_expected(
                                    WithInfo {
                                        info: D::Info::clone(&info),
                                        item: SyntaxKind::Type,
                                    },
                                    None,
                                ));

                                WithInfo {
                                    info: info.clone(),
                                    item: Type::Error,
                                }
                            }
                        };

                        for r#type in types {
                            parser.add_diagnostic(
                                stack.error_expected(r#type.replace(SyntaxKind::Nothing), None),
                            );
                        }

                        WithInfo {
                            info,
                            item: Type::Block(r#type.boxed()),
                        }
                    },
                )
            },
        ],
    )
    .named("A type whose value is computed from a block expression.")
}

pub fn intrinsic_type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::match_terminal(
        SyntaxKind::IntrinsicType,
        RuleToRender::Keyword(Keyword::Intrinsic.to_string()),
        |_, tree, _| match tree.item {
            TokenTree::Keyword(Keyword::Intrinsic) => Some(tree.replace(Type::Intrinsic)),
            _ => None,
        },
    )
    .named("An intrinsic type provided by the runtime.")
}

pub fn message_type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::switch(
        SyntaxKind::MessageType,
        [
            || {
                text()
                    .wrapped()
                    .map(SyntaxKind::MessageType, |text| Type::Message {
                        message: text.try_unwrap().unwrap(),
                        inputs: Vec::new(),
                    })
            },
            || {
                Rule::list_prefix(
                    SyntaxKind::MessageType,
                    || text().wrapped(),
                    r#type,
                    |_, info, message, inputs, _| WithInfo {
                        info,
                        item: Type::Message {
                            message: message.try_unwrap().unwrap(),
                            inputs,
                        },
                    },
                )
            },
        ],
    )
    .named("A type-level piece of text used to generate compiler errors.")
}

pub fn equal_type<D: Driver>() -> Rule<D, Type<D>> {
    Rule::operator(
        SyntaxKind::EqualType,
        Operator::Equal,
        r#type,
        r#type,
        |_, info, left, right, _| WithInfo {
            info,
            item: Type::Equal {
                left: left.boxed(),
                right: right.boxed(),
            },
        },
    )
    .named("Use two types in the place of one.")
}
