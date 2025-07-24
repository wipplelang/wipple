use crate::{
    syntax::{
        Location,
        parse::{SyntaxKind, base::Rule, name, render::RuleToRender, text},
        tokenize::{Keyword, Operator, TokenTree, VariadicOperator},
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Error,
    Placeholder,
    Declared {
        name: WithInfo<Option<String>>,
        parameters: Vec<WithInfo<Type>>,
    },
    Function {
        inputs: Vec<WithInfo<Type>>,
        output: WithInfo<Box<Type>>,
    },
    Tuple(Vec<WithInfo<Type>>),
    Block(WithInfo<Box<Type>>),
    Intrinsic,
    Message {
        message: WithInfo<String>,
        inputs: Vec<WithInfo<Type>>,
    },
    Equal {
        left: WithInfo<Box<Type>>,
        right: WithInfo<Box<Type>>,
    },
}

impl DefaultFromInfo for Type {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: Type::Error,
        }
    }
}

pub fn r#type() -> Rule<Type> {
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

pub fn placeholder_type() -> Rule<Type> {
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

pub fn declared_type() -> Rule<Type> {
    Rule::switch(
        SyntaxKind::DeclaredType,
        [
            || {
                name()
                    .wrapped()
                    .map(SyntaxKind::DeclaredType, |name| Type::Declared {
                        name,
                        parameters: Vec::new(),
                    })
            },
            || {
                Rule::list_prefix(
                    SyntaxKind::DeclaredType,
                    || name().wrapped(),
                    r#type,
                    |_, info, name, types, _| WithInfo {
                        info,
                        item: Type::Declared {
                            name,
                            parameters: types,
                        },
                    },
                )
            },
        ],
    )
    .named("A declared type, optionally with parameters.")
}

pub fn function_type() -> Rule<Type> {
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

pub fn tuple_type() -> Rule<Type> {
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

pub fn block_type() -> Rule<Type> {
    Rule::switch(
        SyntaxKind::BlockType,
        [|| {
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
                                    info: Location::clone(&info),
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
        }],
    )
    .named("A type whose value is computed from a block expression.")
}

pub fn intrinsic_type() -> Rule<Type> {
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

pub fn message_type() -> Rule<Type> {
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

pub fn equal_type() -> Rule<Type> {
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
