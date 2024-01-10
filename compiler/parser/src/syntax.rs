//! Convert a [`reader`](crate::reader) syntax tree into a [`wipple_syntax::syntax`] tree.

use crate::{
    grammar::grammar,
    reader::{Node, TokenKind},
};
use serde::{Deserialize, Serialize};
use std::ops::Range;
use wipple_syntax::{syntax, BinaryOperator};
use wipple_util::WithInfo;

/// A sample driver useful for debugging.
#[derive(Debug, Clone)]
pub struct Driver;

impl wipple_syntax::Driver for Driver {
    type Info = self::Info;
    type Number = String;

    fn merge_info(left: Self::Info, right: Self::Info) -> Self::Info {
        Info {
            span: left.span.start..right.span.end,
        }
    }
}

#[cfg(test)]
fn test_info<D: wipple_syntax::Driver, T>(span: Range<u32>, item: T) -> WithInfo<D::Info, T>
where
    D::Info: From<Info>,
{
    WithInfo {
        info: Info { span }.into(),
        item,
    }
}

#[cfg(test)]
fn test_grammar<D: wipple_syntax::Driver, T: std::fmt::Debug + PartialEq>(
    parse: fn(span: Range<u32>, node: Node<'_>, errors: &mut Vec<Error>) -> WithInfo<D::Info, T>,
    code: &str,
    expected: T,
) where
    D::Info: From<Info>,
{
    let result = crate::reader::tokenize(code);
    assert!(result.errors.is_empty(), "error tokenizing");

    let result = crate::reader::read(
        result.tokens,
        crate::reader::ReadOptions {
            strip_comments: true,
        },
    );

    assert!(result.errors.is_empty(), "error reading");

    let top_level = match result.top_level {
        Node::Block(_, mut nodes) => {
            assert!(nodes.len() == 1, "`code` must be a single statement");

            match nodes.pop().unwrap() {
                Node::List(_, mut elements) if elements.len() == 1 => elements.pop().unwrap(),
                node => node,
            }
        }
        _ => panic!("expected `read` to return a block"),
    };

    let mut errors = Vec::new();
    let actual = parse(0..0, top_level, &mut errors);

    assert!(errors.is_empty(), "error parsing");

    assert_eq!(actual.item, expected);
}

/// The [`wipple_syntax::Driver::Info`] returned by [`parse_top_level`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Info {
    /// The location of the item in the source code.
    pub span: Range<u32>,
}

/// An error occurring during [`parse`].
#[derive(Debug, Clone, Serialize)]
pub struct Error {
    /// The location in the source code where the error occurred.
    pub span: Range<u32>,

    /// The kind of syntax that was expected at this location.
    pub expected: SyntaxKind,
}

/// The kind of [`Error`].
#[allow(missing_docs)]
#[derive(Debug, Clone, Serialize)]
pub enum SyntaxKind {
    Name,
    Text,
    Block,
    Instance,
    TypeParameter,
    Trait,
    Statement,
    Pattern,
    Expression,
    Number,
    Type,
    TypeMember,
    Arm,
    TypeRepresentation,
    Nothing,
}

/// The result of [`parse`].
#[derive(Debug)]
pub struct Result<D: wipple_syntax::Driver> {
    /// The top-level program.
    pub top_level: WithInfo<D::Info, syntax::TopLevel<D>>,

    /// Any errors encountered while parsing the program.
    pub errors: Vec<Error>,
}

/// Convert a syntax tree into a [`wipple_syntax::syntax::TopLevel`].
pub fn parse<D: wipple_syntax::Driver>(_driver: &D, node: Node<'_>) -> Result<D>
where
    D::Info: From<Info>,
{
    let mut errors = Vec::new();

    let top_level = match node {
        Node::Block(span, statements) => WithInfo {
            info: Info { span }.into(),
            item: syntax::TopLevel {
                statements: statements
                    .into_iter()
                    .map(|statement| {
                        parse_statement(statement.span().unwrap().clone(), statement, &mut errors)
                    })
                    .collect(),
            },
        },
        _ => {
            let span = node.span().cloned().unwrap_or(0..0);

            errors.push(Error {
                span: span.clone(),
                expected: SyntaxKind::Block,
            });

            WithInfo {
                info: Info { span }.into(),
                item: syntax::TopLevel {
                    statements: Vec::new(),
                },
            }
        }
    };

    Result { top_level, errors }
}

fn parse_statement<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> WithInfo<D::Info, syntax::Statement<D>>
where
    D::Info: From<Info>,
{
    grammar! {
        static TYPE_OR_TRAIT_DECLARATION = r#"
            (non-associative-binary-operator
                ":"
                (list
                    (variable . "name"))
                (or
                    (non-associative-binary-operator
                        "=>"
                        (or
                            (non-associative-binary-operator
                                "where"
                                (repeat-list . (variable . "parameter"))
                                (repeat-list . (variable . "bound")))
                            (repeat-list . (variable . "parameter")))
                        (variable . "declaration"))
                    (variable . "declaration")))
        "#;

        static INSTANCE_DECLARATION = r#"
            (non-associative-binary-operator
                ":"
                (or
                    (non-associative-binary-operator
                        "=>"
                        (or
                            (non-associative-binary-operator
                                "where"
                                (repeat-list . (variable . "parameter"))
                                (repeat-list . (variable . "bound")))
                            (repeat-list . (variable . "parameter")))
                        (list
                            (symbol . "instance")
                            (variable . "instance")))
                    (list
                        (symbol . "instance")
                        (variable . "instance")))
                (variable . "body"))
        "#;


        static LANGUAGE_DECLARATION = r#"
            (non-associative-binary-operator
                ":"
                (list
                    (symbol . "language")
                    (variable . "name"))
                (list
                    (variable . "item")))
        "#;

        static CONSTANT_DECLARATION = r#"
            (non-associative-binary-operator
                "::"
                (variable . "name")
                (or
                    (non-associative-binary-operator
                        "=>"
                        (or
                            (non-associative-binary-operator
                                "where"
                                (repeat-list . (variable . "parameter"))
                                (repeat-list . (variable . "bound")))
                            (repeat-list . (variable . "parameter")))
                        (variable . "type"))
                    (variable . "type")))
        "#;

        static ASSIGNMENT = r#"
            (non-associative-binary-operator
                ":"
                (variable . "pattern")
                (variable . "value"))
        "#;
    }

    let type_or_trait_declaration = |errors: &mut Vec<_>| {
        TYPE_OR_TRAIT_DECLARATION.parse(&node).and_then(|mut vars| {
            let name = vars.remove("name").unwrap().pop().unwrap();
            let parameters = vars.remove("parameter").unwrap_or_default();
            let bounds = vars.remove("bound").unwrap_or_default();
            let declaration = vars.remove("declaration").unwrap().pop().unwrap();

            grammar! {
                static TYPE_DECLARATION = r#"
                    (or
                        (list
                            (symbol . "type"))
                        (list
                            (symbol . "type")
                            (variable . "representation")))
                "#;

                static TRAIT_DECLARATION = r#"
                    (or
                        (list
                            (symbol . "trait"))
                        (list
                            (symbol . "trait")
                            (variable . "type")))
                "#;
            }

            let name = match name {
                Node::Token(token) => match token.kind {
                    TokenKind::Symbol(name) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span,
                            expected: SyntaxKind::Name,
                        });

                        return Some(syntax::Statement::Error);
                    }
                },
                _ => {
                    errors.push(Error {
                        span: name.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Name,
                    });

                    return Some(syntax::Statement::Error);
                }
            };

            let parameters = parse_type_function(span.clone(), parameters, bounds, errors);

            let type_declaration = |errors| {
                TYPE_DECLARATION.parse(&declaration).map(|mut vars| {
                    let representation = match vars
                        .remove("representation")
                        .map(|mut value| value.pop().unwrap())
                    {
                        Some(representation) => parse_type_representation(
                            representation
                                .span()
                                .cloned()
                                .unwrap_or_else(|| span.clone()),
                            representation,
                            errors,
                        ),
                        None => WithInfo {
                            info: Info {
                                span: declaration.span().cloned().unwrap_or_else(|| span.clone()),
                            }
                            .into(),
                            item: syntax::TypeRepresentation::Marker,
                        },
                    };

                    syntax::Statement::TypeDeclaration {
                        name: name.clone(),
                        parameters: parameters.clone(),
                        representation,
                    }
                })
            };

            let trait_declaration = |errors| {
                TRAIT_DECLARATION.parse(&declaration).map(|mut vars| {
                    let r#type = match vars.remove("type").map(|mut value| value.pop().unwrap()) {
                        Some(r#type) => parse_type(
                            r#type.span().cloned().unwrap_or_else(|| span.clone()),
                            r#type,
                            errors,
                        ),
                        None => WithInfo {
                            info: Info {
                                span: declaration.span().cloned().unwrap_or_else(|| span.clone()),
                            }
                            .into(),
                            item: syntax::Type::Error,
                        },
                    };

                    syntax::Statement::TraitDeclaration {
                        name: name.clone(),
                        parameters: parameters.clone(),
                        r#type,
                    }
                })
            };

            type_declaration(errors).or_else(|| trait_declaration(errors))
        })
    };

    let instance_declaration = |errors: &mut Vec<Error>| {
        INSTANCE_DECLARATION.parse(&node).map(|mut vars| {
            let parameters = vars.remove("parameter").unwrap_or_default();
            let bounds = vars.remove("bound").unwrap_or_default();
            let instance = vars.remove("instance").unwrap().pop().unwrap();
            let body = vars.remove("body").unwrap().pop().unwrap();

            let parameters = parse_type_function(span.clone(), parameters, bounds, errors);

            let instance = match parse_instance(
                instance.span().cloned().unwrap_or_else(|| span.clone()),
                instance,
                errors,
            ) {
                Some(instance) => instance,
                None => return syntax::Statement::Error,
            };

            let body = parse_expression(
                body.span().cloned().unwrap_or_else(|| span.clone()),
                body,
                errors,
            );

            syntax::Statement::InstanceDeclaration {
                parameters,
                instance,
                body,
            }
        })
    };

    let language_declaration = |errors: &mut Vec<Error>| {
        LANGUAGE_DECLARATION.parse(&node).map(|mut vars| {
            let name = vars.remove("name").unwrap().pop().unwrap();
            let item = vars.remove("item").unwrap().pop().unwrap();

            let name = match name {
                Node::Token(token) => match token.kind {
                    TokenKind::Text(name) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span,
                            expected: SyntaxKind::Text,
                        });

                        return syntax::Statement::Error;
                    }
                },
                _ => {
                    errors.push(Error {
                        span: name.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Text,
                    });

                    return syntax::Statement::Error;
                }
            };

            let item = match item {
                Node::Token(token) => match token.kind {
                    TokenKind::Symbol(name) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span,
                            expected: SyntaxKind::Name,
                        });

                        return syntax::Statement::Error;
                    }
                },
                _ => {
                    errors.push(Error {
                        span: item.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Name,
                    });

                    return syntax::Statement::Error;
                }
            };

            syntax::Statement::LanguageDeclaration { name, item }
        })
    };

    let constant_declaration = |errors: &mut Vec<_>| {
        CONSTANT_DECLARATION.parse(&node).map(|mut vars| {
            let name = vars.remove("name").unwrap().pop().unwrap();
            let parameters = vars.remove("parameter").unwrap_or_default();
            let bounds = vars.remove("bound").unwrap_or_default();
            let r#type = vars.remove("type").unwrap().pop().unwrap();

            let name = match name {
                Node::List(span, mut elements) => {
                    if elements.len() != 1 {
                        errors.push(Error {
                            span,
                            expected: SyntaxKind::Name,
                        });

                        return syntax::Statement::Error;
                    }

                    let token = elements.pop().unwrap();

                    match token {
                        Node::Token(token) => match token.kind {
                            TokenKind::Symbol(name) => WithInfo {
                                info: Info { span: token.span }.into(),
                                item: name.to_string(),
                            },
                            _ => {
                                errors.push(Error {
                                    span: token.span,
                                    expected: SyntaxKind::Name,
                                });

                                return syntax::Statement::Error;
                            }
                        },
                        _ => {
                            errors.push(Error {
                                span: token.span().cloned().unwrap_or(span),
                                expected: SyntaxKind::Name,
                            });

                            return syntax::Statement::Error;
                        }
                    }
                }
                _ => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Name,
                    });

                    return syntax::Statement::Error;
                }
            };

            let parameters = parse_type_function(span.clone(), parameters, bounds, errors);

            let r#type = parse_type(
                r#type.span().cloned().unwrap_or_else(|| span.clone()),
                r#type,
                errors,
            );

            syntax::Statement::ConstantDeclaration {
                name,
                parameters,
                r#type,
            }
        })
    };

    let assignment = |errors: &mut Vec<_>| {
        ASSIGNMENT.parse(&node).map(|mut vars| {
            let pattern = vars.remove("pattern").unwrap().pop().unwrap();
            let value = vars.remove("value").unwrap().pop().unwrap();

            let pattern = parse_pattern(
                pattern.span().cloned().unwrap_or_else(|| span.clone()),
                pattern,
                errors,
            );

            let value = parse_expression(
                value.span().cloned().unwrap_or_else(|| span.clone()),
                value,
                errors,
            );

            syntax::Statement::Assignment { pattern, value }
        })
    };

    let statement = type_or_trait_declaration(errors)
        .or_else(|| instance_declaration(errors))
        .or_else(|| constant_declaration(errors))
        .or_else(|| language_declaration(errors))
        .or_else(|| assignment(errors))
        .unwrap_or_else(|| {
            syntax::Statement::Expression(parse_expression(span.clone(), node, errors))
        });

    WithInfo {
        info: Info { span }.into(),
        item: statement,
    }
}

fn parse_expression<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> WithInfo<D::Info, syntax::Expression<D>>
where
    D::Info: From<Info>,
{
    macro_rules! binary_expression_operator {
        ($operator:literal, $wrapper:ident) => {{
            grammar! {
                static OPERATOR_EXPRESSION = format!(
                    r#"
                        (or
                            (binary-operator
                                "{operator}"
                                unapplied)
                            (binary-operator
                                "{operator}"
                                (partially-applied-left .
                                    (variable . "left")))
                            (binary-operator
                                "{operator}"
                                (partially-applied-right .
                                    (variable . "right")))
                            (binary-operator
                                "{operator}"
                                (applied
                                    (variable . "left")
                                    (variable . "right"))))
                    "#,
                    operator = $operator,
                );
            }

            |errors: &mut Vec<_>| {
                OPERATOR_EXPRESSION.parse(&node).map(|mut vars| {
                    let left = vars
                        .remove("left")
                        .map(|mut value| value.pop().unwrap())
                        .map(|left| {
                            parse_expression(
                                left.span().cloned().unwrap_or_else(|| span.clone()),
                                left,
                                errors,
                            )
                        });

                    let right = vars
                        .remove("right")
                        .map(|mut value| value.pop().unwrap())
                        .map(|right| {
                            parse_expression(
                                right.span().cloned().unwrap_or_else(|| span.clone()),
                                right,
                                errors,
                            )
                        });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Expression::BinaryOperator {
                            operator: WithInfo {
                                info: Info { span: span.clone() }.into(),
                                item: BinaryOperator::$wrapper,
                            },
                            left: left.map(|left| left.boxed()),
                            right: right.map(|right| right.boxed()),
                        },
                    }
                })
            }
        }};
    }

    grammar! {
        static ANNOTATE_EXPRESSION = r#"
            (non-associative-binary-operator
                "::"
                (variable . "value")
                (variable . "type"))
        "#;

        static FUNCTION_EXPRESSION = r#"
            (or
                (binary-operator
                    "->"
                    unapplied)
                (binary-operator
                    "->"
                    (partially-applied-left .
                        (variable . "pattern")))
                (binary-operator
                    "->"
                    (partially-applied-right .
                        (variable . "body")))
                (binary-operator
                    "->"
                    (applied
                        (variable . "pattern")
                        (variable . "body"))))
        "#;

        static COLLECTION_EXPRESSION = r#"
            (or
                (variadic-operator
                    ","
                    (applied . (variable . "elements")))
                (variadic-operator
                    ","
                    unapplied))
        "#;

        static TUPLE_EXPRESSION = r#"
            (or
                (variadic-operator
                    ";"
                    (applied . (variable . "elements")))
                (variadic-operator
                    ";"
                    unapplied))
        "#;

        static OF_EXPRESSION = r#"
            (non-associative-binary-operator
                "of"
                (variable . "member")
                (variable . "type"))
        "#;

        static APPLY_EXPRESSION = r#"
            (or
                (binary-operator
                    "."
                    unapplied)
                (binary-operator
                    "."
                    (partially-applied-left .
                        (variable . "input")))
                (binary-operator
                    "."
                    (partially-applied-right .
                        (variable . "function")))
                (binary-operator
                    "."
                    (applied
                        (variable . "input")
                        (variable . "function"))))
        "#;

        static STRUCTURE_EXPRESSION = r#"
            (repeat-block .
                (or
                    (non-associative-binary-operator
                        ":"
                        (variable . "field")
                        (variable . "value"))
                    (variable . "name")))
        "#;

        static INTRINSIC_EXPRESSION = r#"
            (prefix-list
                (symbol . "intrinsic")
                (variable . "name"))
        "#;

        static SEMANTICS_EXPRESSION = r#"
            (prefix-list
                (symbol . "semantics")
                (variable . "name"))
        "#;

        static WHEN_EXPRESSION = r#"
            (prefix-list
                (symbol . "when")
                (variable . "input")
                (repeat-block . (variable . "arm")))
        "#;

        static FORMAT_EXPRESSION = r#"
            (prefix-list
                (variable . "text"))
        "#;
    }

    let annotate = |errors: &mut Vec<_>| {
        ANNOTATE_EXPRESSION.parse(&node).map(|mut vars| {
            let value = vars.remove("value").unwrap().pop().unwrap();
            let r#type = vars.remove("type").unwrap().pop().unwrap();

            let value = parse_expression(
                value.span().cloned().unwrap_or_else(|| span.clone()),
                value,
                errors,
            );

            let r#type = parse_type(
                r#type.span().cloned().unwrap_or_else(|| span.clone()),
                r#type,
                errors,
            );

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Annotate {
                    value: value.boxed(),
                    r#type,
                },
            }
        })
    };

    let function = |errors: &mut Vec<_>| {
        FUNCTION_EXPRESSION.parse(&node).map(|mut vars| {
            let pattern = match vars.remove("pattern").map(|mut value| value.pop().unwrap()) {
                Some(pattern) => parse_pattern(
                    pattern.span().cloned().unwrap_or_else(|| span.clone()),
                    pattern,
                    errors,
                ),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Pattern,
                    });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Pattern::Error,
                    }
                }
            };

            let body = match vars.remove("body").map(|mut value| value.pop().unwrap()) {
                Some(expression) => parse_expression(
                    expression.span().cloned().unwrap_or_else(|| span.clone()),
                    expression,
                    errors,
                ),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Expression,
                    });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Expression::Error,
                    }
                }
            };

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Function {
                    pattern,
                    body: body.boxed(),
                },
            }
        })
    };

    let collection = |errors: &mut Vec<_>| {
        COLLECTION_EXPRESSION.parse(&node).map(|mut vars| {
            let elements = vars
                .remove("elements")
                .unwrap_or_default()
                .into_iter()
                .map(|element| {
                    parse_expression(
                        element.span().cloned().unwrap_or_else(|| span.clone()),
                        element,
                        errors,
                    )
                })
                .collect();

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Collection(elements),
            }
        })
    };

    let tuple = |errors: &mut Vec<_>| {
        TUPLE_EXPRESSION.parse(&node).map(|mut vars| {
            let elements = vars
                .remove("elements")
                .unwrap_or_default()
                .into_iter()
                .map(|element| {
                    parse_expression(
                        element.span().cloned().unwrap_or_else(|| span.clone()),
                        element,
                        errors,
                    )
                })
                .collect();

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Tuple(elements),
            }
        })
    };

    let structure = |errors: &mut Vec<_>| {
        STRUCTURE_EXPRESSION.parse(&node).and_then(|mut vars| {
            let names = vars.remove("name").unwrap_or_default();
            let fields = vars.remove("field").unwrap_or_default();
            let values = vars.remove("value").unwrap_or_default();

            if names.is_empty() && fields.is_empty() {
                return None;
            }

            Some(WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Structure(
                    names
                        .into_iter()
                        .map(|name| match name {
                            Node::Token(token) => match token.kind {
                                TokenKind::Symbol(name) => Some(WithInfo {
                                    info: Info {
                                        span: token.span.clone(),
                                    }
                                    .into(),
                                    item: syntax::FieldValue {
                                        name: WithInfo {
                                            info: Info {
                                                span: token.span.clone(),
                                            }
                                            .into(),
                                            item: name.to_string(),
                                        },
                                        value: WithInfo {
                                            info: Info { span: token.span }.into(),
                                            item: syntax::Expression::Name(name.to_string()),
                                        },
                                    },
                                }),
                                _ => None,
                            },
                            _ => None,
                        })
                        .chain(fields.into_iter().zip(values).map(|(field, value)| {
                            let name = match field {
                                Node::Token(token) => match token.kind {
                                    TokenKind::Symbol(name) => WithInfo {
                                        info: Info { span: token.span }.into(),
                                        item: name.to_string(),
                                    },
                                    _ => return None,
                                },
                                _ => return None,
                            };

                            let value = parse_expression(
                                value.span().cloned().unwrap_or_else(|| span.clone()),
                                value,
                                errors,
                            );

                            Some(WithInfo {
                                info: Info { span: span.clone() }.into(),
                                item: syntax::FieldValue { name, value },
                            })
                        }))
                        .collect::<Option<Vec<_>>>()?,
                ),
            })
        })
    };

    let intrinsic = |errors: &mut Vec<_>| {
        INTRINSIC_EXPRESSION.parse(&node).map(|mut vars| {
            let name = vars.remove("name").map(|mut value| value.pop().unwrap());
            let inputs = vars.remove("__extra").unwrap();

            let name = match name {
                Some(Node::Token(token)) => match token.kind {
                    TokenKind::Text(name) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span,
                            expected: SyntaxKind::Text,
                        });

                        return WithInfo {
                            info: Info { span: span.clone() }.into(),
                            item: syntax::Expression::Error,
                        };
                    }
                },
                _ => {
                    errors.push(Error {
                        span: name
                            .and_then(|name| name.span().cloned())
                            .unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Text,
                    });

                    return WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Expression::Error,
                    };
                }
            };

            let inputs = inputs
                .into_iter()
                .map(|input| {
                    parse_expression(
                        input.span().cloned().unwrap_or_else(|| span.clone()),
                        input,
                        errors,
                    )
                })
                .collect();

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Intrinsic { name, inputs },
            }
        })
    };

    let semantics = |errors: &mut Vec<_>| {
        SEMANTICS_EXPRESSION.parse(&node).map(|mut vars| {
            let name = vars.remove("name").map(|mut value| value.pop().unwrap());
            let mut inputs = vars.remove("__extra").unwrap().into_iter();

            let name = match name {
                Some(Node::Token(token)) => match token.kind {
                    TokenKind::Text(name) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span,
                            expected: SyntaxKind::Text,
                        });

                        return WithInfo {
                            info: Info { span: span.clone() }.into(),
                            item: syntax::Expression::Error,
                        };
                    }
                },
                _ => {
                    errors.push(Error {
                        span: name
                            .and_then(|name| name.span().cloned())
                            .unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Text,
                    });

                    return WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Expression::Error,
                    };
                }
            };

            let body = match inputs.next() {
                Some(body) => parse_expression(
                    body.span().cloned().unwrap_or_else(|| span.clone()),
                    body,
                    errors,
                ),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Expression,
                    });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Expression::Error,
                    }
                }
            };

            for input in inputs {
                errors.push(Error {
                    span: input.span().cloned().unwrap_or_else(|| span.clone()),
                    expected: SyntaxKind::Nothing,
                });
            }

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Semantics {
                    name,
                    body: body.boxed(),
                },
            }
        })
    };

    let when = |errors: &mut Vec<_>| {
        WHEN_EXPRESSION.parse(&node).map(|mut vars| {
            let input = vars.remove("input").map(|mut value| value.pop().unwrap());
            let arms = vars.remove("arm");
            let extra = vars.remove("__extra").unwrap();

            let input = match input {
                Some(input) => parse_expression(
                    input.span().cloned().unwrap_or_else(|| span.clone()),
                    input,
                    errors,
                ),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Expression,
                    });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Expression::Error,
                    }
                }
            };

            let arms = match arms {
                Some(arms) => arms
                    .into_iter()
                    .filter_map(|arm| {
                        parse_arm(
                            arm.span().cloned().unwrap_or_else(|| span.clone()),
                            arm,
                            errors,
                        )
                    })
                    .collect(),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Block,
                    });

                    Vec::new()
                }
            };

            for node in extra {
                errors.push(Error {
                    span: node.span().cloned().unwrap_or_else(|| span.clone()),
                    expected: SyntaxKind::Nothing,
                });
            }

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::When {
                    input: input.boxed(),
                    arms,
                },
            }
        })
    };

    let format = |errors: &mut Vec<_>| {
        FORMAT_EXPRESSION.parse(&node).and_then(|mut vars| {
            let text = vars.remove("text").map(|mut value| value.pop().unwrap());
            let inputs = vars.remove("__extra").unwrap();

            let text = match text {
                Some(Node::Token(token)) => match token.kind {
                    TokenKind::Text(name) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: name.to_string(),
                    },
                    _ => return None,
                },
                _ => return None,
            };

            if inputs.is_empty() {
                return Some(text.map(syntax::Expression::Text));
            }

            let inputs = inputs
                .into_iter()
                .map(|input| {
                    parse_expression(
                        input.span().cloned().unwrap_or_else(|| span.clone()),
                        input,
                        errors,
                    )
                })
                .collect();

            Some(WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Format { text, inputs },
            })
        })
    };

    let apply = |errors: &mut Vec<_>| {
        APPLY_EXPRESSION.parse(&node).map(|mut vars| {
            let input = vars
                .remove("input")
                .map(|mut value| value.pop().unwrap())
                .map(|input| {
                    parse_expression(
                        input.span().cloned().unwrap_or_else(|| span.clone()),
                        input,
                        errors,
                    )
                });

            let function = vars
                .remove("function")
                .map(|mut value| value.pop().unwrap())
                .map(|function| {
                    parse_expression(
                        function.span().cloned().unwrap_or_else(|| span.clone()),
                        function,
                        errors,
                    )
                });

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Apply {
                    input: input.map(|input| input.boxed()),
                    function: function.map(|function| function.boxed()),
                },
            }
        })
    };

    let or = binary_expression_operator!("or", Or);
    let and = binary_expression_operator!("and", And);
    let compose = binary_expression_operator!("|", Compose);
    let less_than = binary_expression_operator!("<", LessThan);
    let less_than_or_equal = binary_expression_operator!("<=", LessThanOrEqual);
    let greater_than = binary_expression_operator!(">", GreaterThan);
    let greater_than_or_equal = binary_expression_operator!(">=", GreaterThanOrEqual);
    let equal = binary_expression_operator!("=", Equal);
    let not_equal = binary_expression_operator!("/=", NotEqual);
    let add = binary_expression_operator!("+", Add);
    let subtract = binary_expression_operator!("-", Subtract);
    let multiply = binary_expression_operator!("*", Multiply);
    let divide = binary_expression_operator!("/", Divide);
    let remainder = binary_expression_operator!("%", Remainder);
    let power = binary_expression_operator!("^", Power);

    let terminal = |errors: &mut Vec<Error>| match &node {
        Node::Token(token) => match token.kind {
            TokenKind::Symbol(name) => WithInfo {
                info: Info {
                    span: token.span.clone(),
                }
                .into(),
                item: syntax::Expression::Name(String::from(name)),
            },
            TokenKind::Number(number) => WithInfo {
                info: Info {
                    span: token.span.clone(),
                }
                .into(),
                item: match number.parse() {
                    Ok(number) => syntax::Expression::Number(number),
                    Err(_) => {
                        errors.push(Error {
                            span: span.clone(),
                            expected: SyntaxKind::Number,
                        });

                        syntax::Expression::Error
                    }
                },
            },
            TokenKind::Text(text) | TokenKind::Asset(text) => WithInfo {
                info: Info {
                    span: token.span.clone(),
                }
                .into(),
                item: syntax::Expression::Text(String::from(text)),
            },
            _ => {
                errors.push(Error {
                    span: span.clone(),
                    expected: SyntaxKind::Expression,
                });

                WithInfo {
                    info: Info {
                        span: token.span.clone(),
                    }
                    .into(),
                    item: syntax::Expression::Error,
                }
            }
        },
        Node::List(span, elements) => {
            let mut elements = elements.iter().map(|expression| {
                parse_expression::<D>(
                    expression.span().cloned().unwrap_or_else(|| span.clone()),
                    expression.clone(),
                    errors,
                )
            });

            if let Some(first) = elements.next() {
                elements.fold(first, |result, next| WithInfo {
                    info: D::merge_info(result.info.clone(), next.info.clone()),
                    item: syntax::Expression::Call {
                        function: result.boxed(),
                        input: next.boxed(),
                    },
                })
            } else {
                WithInfo {
                    info: Info { span: span.clone() }.into(),
                    item: syntax::Expression::Unit,
                }
            }
        }
        Node::Block(span, statements) => WithInfo {
            info: Info { span: span.clone() }.into(),
            item: syntax::Expression::Block(
                statements
                    .iter()
                    .map(|statement| {
                        parse_statement(
                            statement.span().cloned().unwrap_or_else(|| span.clone()),
                            statement.clone(),
                            errors,
                        )
                    })
                    .collect(),
            ),
        },
        _ => {
            errors.push(Error {
                span: span.clone(),
                expected: SyntaxKind::Expression,
            });

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Error,
            }
        }
    };

    annotate(errors)
        .or_else(|| function(errors))
        .or_else(|| collection(errors))
        .or_else(|| tuple(errors))
        .or_else(|| structure(errors))
        .or_else(|| intrinsic(errors))
        .or_else(|| semantics(errors))
        .or_else(|| when(errors))
        .or_else(|| format(errors))
        .or_else(|| apply(errors))
        .or_else(|| or(errors))
        .or_else(|| and(errors))
        .or_else(|| compose(errors))
        .or_else(|| less_than(errors))
        .or_else(|| less_than_or_equal(errors))
        .or_else(|| greater_than(errors))
        .or_else(|| greater_than_or_equal(errors))
        .or_else(|| equal(errors))
        .or_else(|| not_equal(errors))
        .or_else(|| add(errors))
        .or_else(|| subtract(errors))
        .or_else(|| multiply(errors))
        .or_else(|| divide(errors))
        .or_else(|| remainder(errors))
        .or_else(|| power(errors))
        .unwrap_or_else(|| terminal(errors))
}

fn parse_type_function<D: wipple_syntax::Driver>(
    span: Range<u32>,
    parameters: Vec<Node<'_>>,
    bounds: Vec<Node<'_>>,
    errors: &mut Vec<Error>,
) -> WithInfo<D::Info, syntax::TypeFunction<D>>
where
    D::Info: From<Info>,
{
    let parameters = parameters
        .into_iter()
        .filter_map(|element| parse_type_parameter(span.clone(), element, errors))
        .collect();

    let bounds = bounds
        .into_iter()
        .filter_map(|element| parse_instance(span.clone(), element, errors))
        .collect();

    WithInfo {
        info: Info { span }.into(),
        item: syntax::TypeFunction { parameters, bounds },
    }
}

fn parse_type_parameter<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> Option<WithInfo<D::Info, syntax::TypeParameter<D>>>
where
    D::Info: From<Info>,
{
    grammar! {
        static TYPE_PARAMETER = r#"
            (or
                (non-associative-binary-operator
                    ":"
                    (or
                        (list
                            (match "infer" (symbol . "infer"))
                            (variable . "name"))
                        (list
                            (variable . "name")))
                    (variable . "default"))
                (list
                    (match "infer" (symbol . "infer"))
                    (variable . "name"))
                (variable . "name"))
        "#;
    }

    let mut vars = match TYPE_PARAMETER.parse(&node) {
        Some(vars) => vars,
        None => {
            errors.push(Error {
                span,
                expected: SyntaxKind::TypeParameter,
            });

            return None;
        }
    };

    let infer = vars
        .remove("infer")
        .map(|mut infer| infer.pop().unwrap())
        .map(|infer| WithInfo {
            info: Info {
                span: infer.span().cloned().unwrap_or_else(|| span.clone()),
            }
            .into(),
            item: (),
        });

    let name = vars.remove("name").unwrap().pop().unwrap();

    let default = vars
        .remove("default")
        .map(|mut default| default.pop().unwrap());

    let name = match name {
        Node::Token(token) => match token.kind {
            TokenKind::Symbol(name) => WithInfo {
                info: Info { span: token.span }.into(),
                item: name.to_string(),
            },
            _ => {
                errors.push(Error {
                    span: token.span,
                    expected: SyntaxKind::Name,
                });

                return None;
            }
        },
        _ => {
            errors.push(Error {
                span: name.span().cloned().unwrap_or(span),
                expected: SyntaxKind::Name,
            });

            return None;
        }
    };

    let default = default.map(|default| {
        parse_type(
            default.span().cloned().unwrap_or_else(|| span.clone()),
            default,
            errors,
        )
    });

    Some(WithInfo {
        info: Info { span }.into(),
        item: syntax::TypeParameter {
            name,
            infer,
            default,
        },
    })
}

fn parse_instance<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> Option<WithInfo<D::Info, syntax::Instance<D>>>
where
    D::Info: From<Info>,
{
    match node {
        Node::Token(token) => match token.kind {
            TokenKind::Symbol(symbol) => Some(WithInfo {
                info: Info { span }.into(),
                item: syntax::Instance {
                    r#trait: WithInfo {
                        info: Info { span: token.span }.into(),
                        item: String::from(symbol),
                    },
                    parameters: Vec::new(),
                },
            }),
            _ => {
                errors.push(Error {
                    span: token.span,
                    expected: SyntaxKind::Instance,
                });

                None
            }
        },
        Node::List(span, elements) => {
            let mut elements = elements.into_iter();

            let r#trait = match elements.next() {
                Some(Node::Token(token)) => match token.kind {
                    TokenKind::Symbol(symbol) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: String::from(symbol),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span,
                            expected: SyntaxKind::Instance,
                        });

                        return None;
                    }
                },
                _ => {
                    errors.push(Error {
                        span,
                        expected: SyntaxKind::Trait,
                    });

                    return None;
                }
            };

            let parameters = elements
                .map(|node| {
                    parse_type(
                        node.span().cloned().unwrap_or_else(|| span.clone()),
                        node,
                        errors,
                    )
                })
                .collect::<Vec<_>>();

            Some(WithInfo {
                info: Info { span }.into(),
                item: syntax::Instance {
                    r#trait,
                    parameters,
                },
            })
        }
        _ => {
            errors.push(Error {
                span,
                expected: SyntaxKind::Instance,
            });

            None
        }
    }
}

fn parse_type_representation<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> WithInfo<D::Info, syntax::TypeRepresentation<D>>
where
    D::Info: From<Info>,
{
    grammar! {
        static SIMPLE_ENUMERATION_TYPE_REPRESENTATION = r#"
            (or
                (block
                    (repeat-list . (variable . "variant")))
                (repeat-list . (variable . "variant")))
        "#;

        static COMPOUND_TYPE_REPRESENTATION = r#"
            (repeat-block . (variable . "member"))
        "#;
    }

    let simple_enumeration = |errors: &mut Vec<Error>| {
        SIMPLE_ENUMERATION_TYPE_REPRESENTATION
            .parse(&node)
            .map(|mut vars| {
                let variants = vars
                    .remove("variant")
                    .unwrap()
                    .into_iter()
                    .filter_map(|variant| match variant {
                        Node::Token(token) => match token.kind {
                            TokenKind::Symbol(name) => Some(WithInfo {
                                info: Info { span: token.span }.into(),
                                item: name.to_string(),
                            }),
                            _ => {
                                errors.push(Error {
                                    span: token.span,
                                    expected: SyntaxKind::Name,
                                });

                                None
                            }
                        },
                        _ => {
                            errors.push(Error {
                                span: variant.span().cloned().unwrap_or_else(|| span.clone()),
                                expected: SyntaxKind::Name,
                            });

                            None
                        }
                    })
                    .collect();

                WithInfo {
                    info: Info { span: span.clone() }.into(),
                    item: syntax::TypeRepresentation::SimpleEnumeration(variants),
                }
            })
    };

    let compound = |errors: &mut Vec<Error>| {
        COMPOUND_TYPE_REPRESENTATION.parse(&node).map(|mut vars| {
            let members = vars
                .remove("member")
                .unwrap()
                .into_iter()
                .filter_map(|member| {
                    parse_type_member(
                        member.span().cloned().unwrap_or_else(|| span.clone()),
                        member,
                        errors,
                    )
                })
                .collect();

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::TypeRepresentation::Compound(members),
            }
        })
    };

    simple_enumeration(errors)
        .or_else(|| compound(errors))
        .unwrap_or_else(|| {
            errors.push(Error {
                span: span.clone(),
                expected: SyntaxKind::TypeRepresentation,
            });

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::TypeRepresentation::Marker,
            }
        })
}

fn parse_type_member<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> Option<WithInfo<D::Info, syntax::TypeMember<D>>>
where
    D::Info: From<Info>,
{
    grammar! {
        static FIELD_EXPRESSION = r#"
            (non-associative-binary-operator
                "::"
                (list
                    (variable . "name"))
                (variable . "type"))
        "#;

        static VARIANT_EXPRESSION = r#"
            (or
                (list
                    (variable . "name"))
                (prefix-list
                    (variable . "name")))
        "#;
    }

    let field = |errors: &mut Vec<Error>| {
        FIELD_EXPRESSION.parse(&node).map(|mut vars| {
            let name = vars.remove("name").unwrap().pop().unwrap();
            let r#type = vars.remove("type").unwrap().pop().unwrap();

            let name = match name {
                Node::Token(token) => match token.kind {
                    TokenKind::Symbol(name) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span,
                            expected: SyntaxKind::Name,
                        });

                        return None;
                    }
                },
                _ => {
                    errors.push(Error {
                        span: name.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Name,
                    });

                    return None;
                }
            };

            let r#type = parse_type(
                r#type.span().cloned().unwrap_or_else(|| span.clone()),
                r#type,
                errors,
            );

            Some(WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::TypeMember {
                    name,
                    kind: syntax::TypeMemberKind::Field(r#type),
                },
            })
        })
    };

    let variant = |errors: &mut Vec<Error>| {
        VARIANT_EXPRESSION.parse(&node).map(|mut vars| {
            let name = vars.remove("name").unwrap().pop().unwrap();
            let types = vars.remove("__extra").unwrap_or_default();

            let name = match name {
                Node::Token(token) => match token.kind {
                    TokenKind::Symbol(name) => WithInfo {
                        info: Info { span: token.span }.into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span,
                            expected: SyntaxKind::Name,
                        });

                        return None;
                    }
                },
                _ => {
                    errors.push(Error {
                        span: name.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Name,
                    });

                    return None;
                }
            };

            let types = types
                .into_iter()
                .map(|r#type| {
                    parse_type(
                        r#type.span().cloned().unwrap_or_else(|| span.clone()),
                        r#type,
                        errors,
                    )
                })
                .collect();

            Some(WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::TypeMember {
                    name,
                    kind: syntax::TypeMemberKind::Variant(types),
                },
            })
        })
    };

    field(errors)
        .or_else(|| variant(errors))
        .unwrap_or_else(|| {
            errors.push(Error {
                span: span.clone(),
                expected: SyntaxKind::TypeMember,
            });

            None
        })
}

fn parse_type<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> WithInfo<D::Info, syntax::Type<D>>
where
    D::Info: From<Info>,
{
    grammar! {
        static FUNCTION_TYPE = r#"
            (or
                (binary-operator
                    "->"
                    unapplied)
                (binary-operator
                    "->"
                    (partially-applied-left .
                        (variable . "input")))
                (binary-operator
                    "->"
                    (partially-applied-right .
                        (variable . "output")))
                (binary-operator
                    "->"
                    (applied
                        (variable . "input")
                        (variable . "output"))))
        "#;
    }

    let function = |errors: &mut Vec<_>| {
        FUNCTION_TYPE.parse(&node).map(|mut vars| {
            let input = match vars.remove("input").map(|mut value| value.pop().unwrap()) {
                Some(input) => parse_type(
                    input.span().cloned().unwrap_or_else(|| span.clone()),
                    input,
                    errors,
                ),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Type,
                    });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Type::Error,
                    }
                }
            };

            let output = match vars.remove("output").map(|mut value| value.pop().unwrap()) {
                Some(output) => parse_type(
                    output.span().cloned().unwrap_or_else(|| span.clone()),
                    output,
                    errors,
                ),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Type,
                    });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Type::Error,
                    }
                }
            };

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Type::Function {
                    input: input.boxed(),
                    output: output.boxed(),
                },
            }
        })
    };

    let terminal = |errors: &mut Vec<Error>| match &node {
        Node::Token(token) => match token.kind {
            TokenKind::Symbol(name) => WithInfo {
                info: Info {
                    span: token.span.clone(),
                }
                .into(),
                item: syntax::Type::Declared {
                    name: WithInfo {
                        info: Info {
                            span: token.span.clone(),
                        }
                        .into(),
                        item: String::from(name),
                    },
                    parameters: Vec::new(),
                },
            },
            _ => {
                errors.push(Error {
                    span: span.clone(),
                    expected: SyntaxKind::Type,
                });

                WithInfo {
                    info: Info {
                        span: token.span.clone(),
                    }
                    .into(),
                    item: syntax::Type::Error,
                }
            }
        },
        Node::List(span, elements) => {
            let mut elements = elements.iter();

            let name = match elements.next() {
                Some(Node::Token(token)) => match token.kind {
                    TokenKind::Symbol(name) => WithInfo {
                        info: Info {
                            span: token.span.clone(),
                        }
                        .into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span.clone(),
                            expected: SyntaxKind::Name,
                        });

                        return WithInfo {
                            info: Info { span: span.clone() }.into(),
                            item: syntax::Type::Error,
                        };
                    }
                },
                Some(token) => {
                    errors.push(Error {
                        span: token.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Type,
                    });

                    return WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Type::Error,
                    };
                }
                None => {
                    return WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Type::Unit,
                    };
                }
            };

            let parameters = elements
                .map(|expression| {
                    parse_type::<D>(
                        expression.span().cloned().unwrap_or_else(|| span.clone()),
                        expression.clone(),
                        errors,
                    )
                })
                .collect();

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Type::Declared { name, parameters },
            }
        }
        _ => {
            errors.push(Error {
                span: span.clone(),
                expected: SyntaxKind::Type,
            });

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Type::Error,
            }
        }
    };

    function(errors).unwrap_or_else(|| terminal(errors))
}

fn parse_pattern<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> WithInfo<D::Info, syntax::Pattern<D>>
where
    D::Info: From<Info>,
{
    grammar! {
        static OR_PATTERN = r#"
            (or
                (binary-operator
                    "or"
                    unapplied)
                (binary-operator
                    "or"
                    (partially-applied-left .
                        (or
                            (list
                                (variable . "left"))
                            (variable . "left"))))
                (binary-operator
                    "or"
                    (partially-applied-right .
                        (or
                            (list
                                (variable . "right"))
                            (variable . "right"))))
                (binary-operator
                    "or"
                    (applied
                        (or
                            (list
                                (variable . "left"))
                            (variable . "left"))
                        (or
                            (list
                                (variable . "right"))
                            (variable . "right")))))
        "#;

        static DESTRUCTURE_PATTERN = r#"
            (repeat-block .
                (or
                    (non-associative-binary-operator
                        ":"
                        (variable . "field")
                        (variable . "pattern"))
                    (variable . "name")))
        "#;
    }

    let or = |errors: &mut Vec<_>| {
        OR_PATTERN.parse(&node).map(|mut vars| {
            let left = match vars.remove("left").map(|mut value| value.pop().unwrap()) {
                Some(left) => parse_pattern(
                    left.span().cloned().unwrap_or_else(|| span.clone()),
                    left,
                    errors,
                ),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Pattern,
                    });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Pattern::Error,
                    }
                }
            };

            let right = match vars.remove("right").map(|mut value| value.pop().unwrap()) {
                Some(right) => parse_pattern(
                    right.span().cloned().unwrap_or_else(|| span.clone()),
                    right,
                    errors,
                ),
                None => {
                    errors.push(Error {
                        span: span.clone(),
                        expected: SyntaxKind::Pattern,
                    });

                    WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Pattern::Error,
                    }
                }
            };

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Pattern::Or {
                    left: left.boxed(),
                    right: right.boxed(),
                },
            }
        })
    };

    let destructure = |errors: &mut Vec<_>| {
        DESTRUCTURE_PATTERN.parse(&node).map(|mut vars| {
            let names = vars.remove("name").unwrap_or_default();
            let fields = vars.remove("field").unwrap_or_default();
            let values = vars.remove("value").unwrap_or_default();

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Pattern::Destructure(
                    names
                        .into_iter()
                        .filter_map(|name| match name {
                            Node::Token(token) => match token.kind {
                                TokenKind::Symbol(name) => Some(WithInfo {
                                    info: Info {
                                        span: token.span.clone(),
                                    }
                                    .into(),
                                    item: syntax::FieldPattern {
                                        name: WithInfo {
                                            info: Info {
                                                span: token.span.clone(),
                                            }
                                            .into(),
                                            item: name.to_string(),
                                        },
                                        pattern: WithInfo {
                                            info: Info { span: token.span }.into(),
                                            item: syntax::Pattern::Name(name.to_string()),
                                        },
                                    },
                                }),
                                _ => None,
                            },
                            _ => None,
                        })
                        .chain(
                            fields
                                .into_iter()
                                .zip(values)
                                .filter_map(|(field, pattern)| {
                                    let name = match field {
                                        Node::Token(token) => match token.kind {
                                            TokenKind::Symbol(name) => WithInfo {
                                                info: Info { span: token.span }.into(),
                                                item: name.to_string(),
                                            },
                                            _ => return None,
                                        },
                                        _ => return None,
                                    };

                                    let pattern = parse_pattern(
                                        pattern.span().cloned().unwrap_or_else(|| span.clone()),
                                        pattern,
                                        errors,
                                    );

                                    Some(WithInfo {
                                        info: Info { span: span.clone() }.into(),
                                        item: syntax::FieldPattern { name, pattern },
                                    })
                                }),
                        )
                        .collect(),
                ),
            }
        })
    };

    let terminal = |errors: &mut Vec<Error>| match &node {
        Node::Token(token) => match token.kind {
            TokenKind::Symbol(name) => WithInfo {
                info: Info {
                    span: token.span.clone(),
                }
                .into(),
                item: syntax::Pattern::VariantOrName(String::from(name)),
            },
            TokenKind::Number(number) => WithInfo {
                info: Info {
                    span: token.span.clone(),
                }
                .into(),
                item: match number.parse() {
                    Ok(number) => syntax::Pattern::Number(number),
                    Err(_) => {
                        errors.push(Error {
                            span: span.clone(),
                            expected: SyntaxKind::Number,
                        });

                        syntax::Pattern::Error
                    }
                },
            },
            TokenKind::Text(text) | TokenKind::Asset(text) => WithInfo {
                info: Info {
                    span: token.span.clone(),
                }
                .into(),
                item: syntax::Pattern::Text(String::from(text)),
            },
            _ => {
                errors.push(Error {
                    span: span.clone(),
                    expected: SyntaxKind::Type,
                });

                WithInfo {
                    info: Info {
                        span: token.span.clone(),
                    }
                    .into(),
                    item: syntax::Pattern::Error,
                }
            }
        },
        Node::List(span, elements) => {
            let mut elements = elements.iter();

            let variant = match elements.next() {
                Some(Node::Token(token)) => match token.kind {
                    TokenKind::Symbol(name) => WithInfo {
                        info: Info {
                            span: token.span.clone(),
                        }
                        .into(),
                        item: name.to_string(),
                    },
                    _ => {
                        errors.push(Error {
                            span: token.span.clone(),
                            expected: SyntaxKind::Name,
                        });

                        return WithInfo {
                            info: Info { span: span.clone() }.into(),
                            item: syntax::Pattern::Error,
                        };
                    }
                },
                Some(token) => {
                    errors.push(Error {
                        span: token.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Name,
                    });

                    return WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Pattern::Error,
                    };
                }
                None => {
                    return WithInfo {
                        info: Info { span: span.clone() }.into(),
                        item: syntax::Pattern::Unit,
                    };
                }
            };

            let value_patterns = elements
                .map(|pattern| {
                    parse_pattern::<D>(
                        pattern.span().cloned().unwrap_or_else(|| span.clone()),
                        pattern.clone(),
                        errors,
                    )
                })
                .collect::<Vec<_>>();

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: if value_patterns.is_empty() {
                    syntax::Pattern::VariantOrName(variant.item)
                } else {
                    syntax::Pattern::Variant {
                        variant,
                        value_patterns,
                    }
                },
            }
        }
        _ => {
            errors.push(Error {
                span: span.clone(),
                expected: SyntaxKind::Pattern,
            });

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Pattern::Error,
            }
        }
    };

    or(errors)
        .or_else(|| destructure(errors))
        .unwrap_or_else(|| terminal(errors))
}

fn parse_arm<D: wipple_syntax::Driver>(
    span: Range<u32>,
    node: Node<'_>,
    errors: &mut Vec<Error>,
) -> Option<WithInfo<D::Info, syntax::Arm<D>>>
where
    D::Info: From<Info>,
{
    grammar! {
        static ARM = r#"
            (or
                (binary-operator
                    "->"
                    unapplied)
                (binary-operator
                    "->"
                    (partially-applied-left .
                        (or
                            (non-associative-binary-operator
                                "where"
                                (variable . "pattern")
                                (variable . "condition"))
                            (variable . "pattern"))))
                (binary-operator
                    "->"
                    (partially-applied-right .
                        (variable . "body")))
                (binary-operator
                    "->"
                    (applied
                        (or
                            (non-associative-binary-operator
                                "where"
                                (variable . "pattern")
                                (variable . "condition"))
                            (variable . "pattern"))
                        (variable . "body"))))
        "#;
    }

    let mut vars = match ARM.parse(&node) {
        Some(vars) => vars,
        None => {
            errors.push(Error {
                span,
                expected: SyntaxKind::Arm,
            });

            return None;
        }
    };

    let pattern = vars.remove("pattern").map(|mut value| value.pop().unwrap());
    let condition = vars
        .remove("condition")
        .map(|mut value| value.pop().unwrap());
    let body = vars.remove("body").map(|mut value| value.pop().unwrap());

    let pattern = match pattern {
        Some(pattern) => parse_pattern(
            pattern.span().cloned().unwrap_or_else(|| span.clone()),
            pattern,
            errors,
        ),
        None => {
            errors.push(Error {
                span: span.clone(),
                expected: SyntaxKind::Pattern,
            });

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Pattern::Error,
            }
        }
    };

    let condition = condition.map(|condition| {
        parse_expression(
            condition.span().cloned().unwrap_or_else(|| span.clone()),
            condition,
            errors,
        )
    });

    let body = match body {
        Some(body) => parse_expression(
            body.span().cloned().unwrap_or_else(|| span.clone()),
            body,
            errors,
        ),
        None => {
            errors.push(Error {
                span: span.clone(),
                expected: SyntaxKind::Expression,
            });

            WithInfo {
                info: Info { span: span.clone() }.into(),
                item: syntax::Expression::Error,
            }
        }
    };

    Some(WithInfo {
        info: Info { span }.into(),
        item: syntax::Arm {
            pattern,
            condition,
            body,
        },
    })
}

#[cfg(test)]
mod type_parameter_tests {
    use super::*;

    #[test]
    fn test_name() {
        let code = "A";

        let expected = syntax::TypeParameter {
            name: test_info::<Driver, _>(0..1, String::from("A")),
            infer: None,
            default: None,
        };

        test_grammar::<Driver, _>(
            |span, node, errors| parse_type_parameter::<Driver>(span, node, errors).unwrap(),
            code,
            expected,
        );
    }

    #[test]
    fn test_infer() {
        let code = "infer A";

        let expected = syntax::TypeParameter {
            name: test_info::<Driver, _>(6..7, String::from("A")),
            infer: Some(test_info::<Driver, _>(0..5, ())),
            default: None,
        };

        test_grammar::<Driver, _>(
            |span, node, errors| parse_type_parameter::<Driver>(span, node, errors).unwrap(),
            code,
            expected,
        );
    }

    #[test]
    fn test_default() {
        let code = "A : Foo";

        let expected = syntax::TypeParameter {
            name: test_info::<Driver, _>(0..1, String::from("A")),
            infer: None,
            default: None,
        };

        test_grammar::<Driver, _>(
            |span, node, errors| parse_type_parameter::<Driver>(span, node, errors).unwrap(),
            code,
            expected,
        );
    }

    #[test]
    fn test_infer_default() {
        let code = "infer A : Foo";

        let expected = syntax::TypeParameter {
            name: test_info::<Driver, _>(0..1, String::from("A")),
            infer: None,
            default: None,
        };

        test_grammar::<Driver, _>(
            |span, node, errors| parse_type_parameter::<Driver>(span, node, errors).unwrap(),
            code,
            expected,
        );
    }
}

#[cfg(test)]
mod instance_tests {
    use super::*;

    #[test]
    fn test_without_parameters() {
        let code = "Add";

        let expected = syntax::Instance {
            r#trait: test_info::<Driver, _>(0..3, String::from("Add")),
            parameters: Vec::new(),
        };

        test_grammar::<Driver, _>(
            |span, node, errors| parse_instance::<Driver>(span, node, errors).unwrap(),
            code,
            expected,
        );
    }

    #[test]
    fn test_with_parameters() {
        let code = "Add Number Number Number";

        let expected = syntax::Instance {
            r#trait: test_info::<Driver, _>(0..3, String::from("Show")),
            parameters: vec![
                test_info::<Driver, _>(
                    4..10,
                    syntax::Type::Declared {
                        name: test_info::<Driver, _>(4..10, String::from("Number")),
                        parameters: Vec::new(),
                    },
                ),
                test_info::<Driver, _>(
                    11..17,
                    syntax::Type::Declared {
                        name: test_info::<Driver, _>(4..10, String::from("Number")),
                        parameters: Vec::new(),
                    },
                ),
                test_info::<Driver, _>(
                    18..24,
                    syntax::Type::Declared {
                        name: test_info::<Driver, _>(4..10, String::from("Number")),
                        parameters: Vec::new(),
                    },
                ),
            ],
        };

        test_grammar::<Driver, _>(
            |span, node, errors| parse_instance::<Driver>(span, node, errors).unwrap(),
            code,
            expected,
        );
    }
}
