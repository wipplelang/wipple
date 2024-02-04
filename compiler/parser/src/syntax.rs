//! Convert a [`reader`](crate::reader) syntax tree into a [`wipple_syntax::syntax`] tree.

use crate::{
    grammar::grammar,
    reader::{Documentation, Node, TokenKind},
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

    fn file_path(&self) -> String {
        String::from("test")
    }

    fn visible_path(&self) -> String {
        String::from("test")
    }

    fn merge_info(left: Self::Info, right: Self::Info) -> Self::Info {
        Info {
            path: left.path,
            visible_path: left.visible_path,
            span: left.span.start..right.span.end,
            documentation: left
                .documentation
                .into_iter()
                .chain(right.documentation)
                .collect(),
        }
    }
}

#[cfg(test)]
fn test_info<D: wipple_syntax::Driver, T>(span: Range<u32>, item: T) -> WithInfo<D::Info, T>
where
    D::Info: From<Info>,
{
    use wipple_syntax::Driver;

    WithInfo {
        info: Info {
            path: Driver.file_path(),
            visible_path: Driver.visible_path(),
            span,
            documentation: Vec::new(),
        }
        .into(),
        item,
    }
}

#[cfg(test)]
fn test_grammar<T: std::fmt::Debug + PartialEq>(
    parse: fn(
        span: Range<u32>,
        node: Node<'_>,
        parser: &mut Parser<'_, Driver>,
    ) -> WithInfo<<Driver as wipple_syntax::Driver>::Info, T>,
    code: &str,
    expected: T,
) {
    let result = crate::reader::tokenize(code);
    assert!(result.diagnostics.is_empty(), "error tokenizing");

    let result = crate::reader::read_top_level(
        result.tokens,
        crate::reader::ReadOptions {
            strip_comments: true,
        },
    );

    assert!(result.diagnostics.is_empty(), "error reading");

    let top_level = match result.node {
        Node::Block(_, mut nodes) => {
            assert!(nodes.len() == 1, "`code` must be a single statement");

            match nodes.pop().unwrap() {
                (_, Node::List(_, mut elements)) if elements.len() == 1 => elements.pop().unwrap(),
                (_, node) => node,
            }
        }
        _ => panic!("expected `read` to return a block"),
    };

    let mut parser = Parser {
        driver: &Driver,
        errors: Vec::new(),
    };

    let actual = parse(0..0, top_level, &mut parser);

    assert!(parser.errors.is_empty(), "error parsing");

    assert_eq!(actual.item, expected);
}

/// The [`wipple_syntax::Driver::Info`] returned by [`parse_top_level`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Info {
    /// The path to the source file.
    pub path: String,

    /// The path rendered in diagnostics.
    pub visible_path: String,

    /// The location of the item in the source code.
    pub span: Range<u32>,

    /// Any documentation associated with the item.
    pub documentation: Vec<Documentation>,
}

impl Info {
    /// Check if `span` is within the span of `other`.
    pub fn span_is_within(&self, other: &Self) -> bool {
        self.span.start >= other.span.start && self.span.end <= other.span.end
    }
}

/// An error occurring during [`parse`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Diagnostic {
    /// The location in the source code where the error occurred.
    pub span: Range<u32>,

    /// The kind of syntax that was expected at this location.
    pub expected: SyntaxKind,
}

/// The kind of [`Error`].
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SyntaxKind {
    Name,
    Text,
    Block,
    Instance,
    TypeParameter,
    Trait,
    Pattern,
    Expression,
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
    pub diagnostics: Vec<Diagnostic>,
}

/// Convert a syntax tree into a [`wipple_syntax::syntax::TopLevel`].
pub fn parse<D: wipple_syntax::Driver>(driver: &D, node: Node<'_>) -> Result<D>
where
    D::Info: From<Info>,
{
    let mut parser = Parser {
        driver,
        errors: Vec::new(),
    };

    let top_level = match node {
        Node::Block(span, statements) => WithInfo {
            info: Info {
                path: driver.file_path(),
                visible_path: driver.visible_path(),
                span,
                documentation: Vec::new(),
            }
            .into(),
            item: syntax::TopLevel {
                statements: statements
                    .into_iter()
                    .map(|(documentation, statement)| {
                        parser.parse_statement(
                            statement.span().cloned().unwrap_or(0..0),
                            documentation,
                            statement,
                        )
                    })
                    .collect(),
            },
        },
        _ => {
            let span = node.span().cloned().unwrap_or(0..0);

            parser.errors.push(Diagnostic {
                span: span.clone(),
                expected: SyntaxKind::Block,
            });

            WithInfo {
                info: Info {
                    path: driver.file_path(),
                    visible_path: driver.visible_path(),
                    span,
                    documentation: Vec::new(),
                }
                .into(),
                item: syntax::TopLevel {
                    statements: Vec::new(),
                },
            }
        }
    };

    Result {
        top_level,
        diagnostics: parser.errors,
    }
}

struct Parser<'a, D: wipple_syntax::Driver> {
    driver: &'a D,
    errors: Vec<Diagnostic>,
}

impl<'a, D: wipple_syntax::Driver> Parser<'a, D> {
    fn parse_statement(
        &mut self,
        span: Range<u32>,
        documentation: Vec<Documentation>,
        node: Node<'_>,
    ) -> WithInfo<D::Info, syntax::Statement<D>>
    where
        D::Info: From<Info>,
    {
        grammar! {
            static INSTANCE_DECLARATION = r#"
                (non-associative-binary-operator
                    ":"
                    (or
                        (non-associative-binary-operator
                            "=>"
                            (or
                                (non-associative-binary-operator
                                    "where"
                                    (or
                                        (repeat-list . (variable . "parameter"))
                                        (variable . "parameter"))
                                    (or
                                        (repeat-list . (variable . "bound"))
                                        (variable . "bound")))
                                (or
                                    (repeat-list . (variable . "parameter"))
                                    (variable . "parameter")))
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
                        (variable . "kind")
                        (variable . "name"))
                    (variable . "item"))
            "#;

            static TYPE_OR_TRAIT_DECLARATION = r#"
                (non-associative-binary-operator
                    ":"
                    (variable . "name")
                    (or
                        (non-associative-binary-operator
                            "=>"
                            (or
                                (non-associative-binary-operator
                                    "where"
                                    (or
                                        (repeat-list . (variable . "parameter"))
                                        (variable . "parameter"))
                                    (or
                                        (repeat-list . (variable . "bound"))
                                        (variable . "bound")))
                                (or
                                    (repeat-list . (variable . "parameter"))
                                    (variable . "parameter")))
                            (variable . "declaration"))
                        (variable . "declaration")))
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
                                    (or
                                        (repeat-list . (variable . "parameter"))
                                        (variable . "parameter"))
                                    (or
                                        (repeat-list . (variable . "bound"))
                                        (variable . "bound")))
                                (or
                                    (repeat-list . (variable . "parameter"))
                                    (variable . "parameter")))
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

        let instance_declaration = |parser: &mut Self| {
            INSTANCE_DECLARATION.parse(&node).map(|mut vars| {
                let parameters = vars.remove("parameter").unwrap_or_default();
                let bounds = vars.remove("bound").unwrap_or_default();
                let instance = vars.remove("instance").unwrap().pop().unwrap();
                let body = vars.remove("body").unwrap().pop().unwrap();

                let parameters = parser.parse_type_function(span.clone(), parameters, bounds);

                let instance = match parser.parse_instance(
                    instance.span().cloned().unwrap_or_else(|| span.clone()),
                    instance,
                ) {
                    Some(instance) => instance,
                    None => return syntax::Statement::Error,
                };

                let body = parser
                    .parse_expression(body.span().cloned().unwrap_or_else(|| span.clone()), body);

                syntax::Statement::InstanceDeclaration {
                    parameters,
                    instance,
                    body,
                }
            })
        };

        let language_declaration = |parser: &mut Self| {
            LANGUAGE_DECLARATION.parse(&node).map(|mut vars| {
                let name = vars.remove("name").unwrap().pop().unwrap();
                let kind = vars.remove("kind").unwrap().pop().unwrap();
                let item = vars.remove("item").unwrap().pop().unwrap();

                let name = match name {
                    Node::Token(token) => match token.kind {
                        TokenKind::Text(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span,
                                expected: SyntaxKind::Text,
                            });

                            return syntax::Statement::Error;
                        }
                    },
                    _ => {
                        parser.errors.push(Diagnostic {
                            span: name.span().cloned().unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Text,
                        });

                        return syntax::Statement::Error;
                    }
                };

                let kind = match kind {
                    Node::Token(token) => match token.kind {
                        TokenKind::Symbol(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: match name.as_ref() {
                                "type" => syntax::LanguageDeclarationKind::Type,
                                "trait" => syntax::LanguageDeclarationKind::Trait,
                                "constant" => syntax::LanguageDeclarationKind::Constant,
                                _ => {
                                    parser.errors.push(Diagnostic {
                                        span: token.span,
                                        expected: SyntaxKind::Name,
                                    });

                                    return syntax::Statement::Error;
                                }
                            },
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span,
                                expected: SyntaxKind::Name,
                            });

                            return syntax::Statement::Error;
                        }
                    },
                    _ => {
                        parser.errors.push(Diagnostic {
                            span: item.span().cloned().unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Name,
                        });

                        return syntax::Statement::Error;
                    }
                };

                let item = match item {
                    Node::Token(token) => match token.kind {
                        TokenKind::Symbol(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span,
                                expected: SyntaxKind::Name,
                            });

                            return syntax::Statement::Error;
                        }
                    },
                    _ => {
                        parser.errors.push(Diagnostic {
                            span: item.span().cloned().unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Name,
                        });

                        return syntax::Statement::Error;
                    }
                };

                syntax::Statement::LanguageDeclaration { name, kind, item }
            })
        };

        let type_or_trait_declaration = |parser: &mut Self| {
            TYPE_OR_TRAIT_DECLARATION.parse(&node).and_then(|mut vars| {
                let name = vars.remove("name").unwrap().pop().unwrap();
                let parameters = vars.remove("parameter").unwrap_or_default();
                let bounds = vars.remove("bound").unwrap_or_default();
                let declaration = vars.remove("declaration").unwrap().pop().unwrap();

                grammar! {
                    static TYPE_DECLARATION = r#"
                    (or
                        (symbol . "type")
                        (list
                            (symbol . "type")
                            (variable . "representation")))
                "#;

                    static TRAIT_DECLARATION = r#"
                    (or
                        (symbol . "trait")
                        (list
                            (symbol . "trait")
                            (variable . "type")))
                "#;
                }

                let name = |parser: &mut Self| match &name {
                    Node::Token(token) => match &token.kind {
                        TokenKind::Symbol(name) => Some(WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        }),
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span.clone(),
                                expected: SyntaxKind::Name,
                            });

                            None
                        }
                    },
                    _ => {
                        parser.errors.push(Diagnostic {
                            span: name.span().cloned().unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Name,
                        });

                        None
                    }
                };

                let parameters = parser.parse_type_function(span.clone(), parameters, bounds);

                let type_declaration = |parser: &mut Self| {
                    TYPE_DECLARATION.parse(&declaration).map(|mut vars| {
                        let name = match name(parser) {
                            Some(name) => name,
                            None => return syntax::Statement::Error,
                        };

                        let representation = match vars
                            .remove("representation")
                            .map(|mut value| value.pop().unwrap())
                        {
                            Some(representation) => parser.parse_type_representation(
                                representation
                                    .span()
                                    .cloned()
                                    .unwrap_or_else(|| span.clone()),
                                representation,
                            ),
                            None => WithInfo {
                                info: Info {
                                    path: parser.driver.file_path(),
                                    visible_path: parser.driver.visible_path(),
                                    span: declaration
                                        .span()
                                        .cloned()
                                        .unwrap_or_else(|| span.clone()),
                                    documentation: Vec::new(),
                                }
                                .into(),
                                item: syntax::TypeRepresentation::Opaque,
                            },
                        };

                        syntax::Statement::TypeDeclaration {
                            name,
                            parameters: parameters.clone(),
                            representation,
                        }
                    })
                };

                let trait_declaration = |parser: &mut Self| {
                    TRAIT_DECLARATION.parse(&declaration).map(|mut vars| {
                        let name = match name(parser) {
                            Some(name) => name,
                            None => return syntax::Statement::Error,
                        };

                        let r#type = match vars.remove("type").map(|mut value| value.pop().unwrap())
                        {
                            Some(r#type) => parser.parse_type(
                                r#type.span().cloned().unwrap_or_else(|| span.clone()),
                                r#type,
                            ),
                            None => {
                                parser.errors.push(Diagnostic {
                                    span: declaration
                                        .span()
                                        .cloned()
                                        .unwrap_or_else(|| span.clone()),
                                    expected: SyntaxKind::Type,
                                });

                                WithInfo {
                                    info: Info {
                                        path: self.driver.file_path(),
                                        visible_path: self.driver.visible_path(),
                                        span: declaration
                                            .span()
                                            .cloned()
                                            .unwrap_or_else(|| span.clone()),
                                        documentation: Vec::new(),
                                    }
                                    .into(),
                                    item: syntax::Type::Error,
                                }
                            }
                        };

                        syntax::Statement::TraitDeclaration {
                            name,
                            parameters: parameters.clone(),
                            r#type,
                        }
                    })
                };

                type_declaration(parser).or_else(|| trait_declaration(parser))
            })
        };

        let constant_declaration = |parser: &mut Self| {
            CONSTANT_DECLARATION.parse(&node).and_then(|mut vars| {
                let name = vars.remove("name").unwrap().pop().unwrap();
                let parameters = vars.remove("parameter").unwrap_or_default();
                let bounds = vars.remove("bound").unwrap_or_default();
                let r#type = vars.remove("type").unwrap().pop().unwrap();

                let name = match name {
                    Node::Token(token) => match token.kind {
                        TokenKind::Symbol(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => return None,
                    },
                    _ => return None,
                };

                let parameters = parser.parse_type_function(span.clone(), parameters, bounds);

                let r#type = parser.parse_type(
                    r#type.span().cloned().unwrap_or_else(|| span.clone()),
                    r#type,
                );

                Some(syntax::Statement::ConstantDeclaration {
                    name,
                    parameters,
                    r#type,
                })
            })
        };

        let assignment = |parser: &mut Self| {
            ASSIGNMENT.parse(&node).map(|mut vars| {
                let pattern = vars.remove("pattern").unwrap().pop().unwrap();
                let value = vars.remove("value").unwrap().pop().unwrap();

                let pattern = parser.parse_pattern(
                    pattern.span().cloned().unwrap_or_else(|| span.clone()),
                    pattern,
                );

                let value = parser
                    .parse_expression(value.span().cloned().unwrap_or_else(|| span.clone()), value);

                syntax::Statement::Assignment { pattern, value }
            })
        };

        let statement = instance_declaration(self)
            .or_else(|| language_declaration(self))
            .or_else(|| type_or_trait_declaration(self))
            .or_else(|| constant_declaration(self))
            .or_else(|| assignment(self))
            .unwrap_or_else(|| {
                syntax::Statement::Expression(self.parse_expression(span.clone(), node))
            });

        WithInfo {
            info: Info {
                path: self.driver.file_path(),
                visible_path: self.driver.visible_path(),
                span,
                documentation,
            }
            .into(),
            item: statement,
        }
    }

    fn parse_expression(
        &mut self,
        span: Range<u32>,
        node: Node<'_>,
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

                |parser: &mut Self| {
                    OPERATOR_EXPRESSION.parse(&node).map(|mut vars| {
                        let left = vars
                            .remove("left")
                            .map(|mut value| value.pop().unwrap())
                            .map(|left| {
                                parser.parse_expression(
                                    left.span().cloned().unwrap_or_else(|| span.clone()),
                                    left,
                                )
                            });

                        let right = vars
                            .remove("right")
                            .map(|mut value| value.pop().unwrap())
                            .map(|right| {
                                parser.parse_expression(
                                    right.span().cloned().unwrap_or_else(|| span.clone()),
                                    right,
                                )
                            });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Expression::BinaryOperator {
                                operator: WithInfo {
                                    info: Info {
                                        path: parser.driver.file_path(),
                                        visible_path: parser.driver.visible_path(),
                                        span: span.clone(),
                                        documentation: Vec::new(),
                                    }
                                    .into(),
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
                        (or
                            (match "pattern" (list))
                            (repeat-list . (variable . "pattern"))
                            (variable . "pattern"))))
                (binary-operator
                    "->"
                    (partially-applied-right .
                        (variable . "body")))
                (binary-operator
                    "->"
                    (applied
                        (or
                            (match "pattern" (list))
                            (repeat-list . (variable . "pattern"))
                            (variable . "pattern"))
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

        static COMPOSE_EXPRESSION = r#"
            (or
                (binary-operator
                    "|"
                    unapplied)
                (binary-operator
                    "|"
                    (partially-applied-left .
                        (variable . "left")))
                (binary-operator
                    "|"
                    (partially-applied-right .
                        (variable . "right")))
                (binary-operator
                    "|"
                    (applied
                        (variable . "left")
                        (variable . "right"))))
        "#;

        static IS_EXPRESSION = r#"
            (non-associative-binary-operator
                "is"
                (variable . "input")
                (variable . "pattern"))
        "#;

        static AS_EXPRESSION = r#"
            (or
                (binary-operator
                    "as"
                    unapplied)
                (binary-operator
                    "as"
                    (partially-applied-left .
                        (variable . "value")))
                (binary-operator
                    "as"
                    (partially-applied-right .
                        (variable . "value")))
                (binary-operator
                    "as"
                    (applied
                        (variable . "value")
                        (variable . "type"))))
        "#;

            static STRUCTURE_EXPRESSION = r#"
            (or
                (non-associative-binary-operator
                    ":"
                    (variable . "field")
                    (variable . "value"))
                (repeat-block .
                    (non-associative-binary-operator
                        ":"
                        (variable . "field")
                        (variable . "value"))))
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

        let annotate = |parser: &mut Self| {
            ANNOTATE_EXPRESSION.parse(&node).map(|mut vars| {
                let value = vars.remove("value").unwrap().pop().unwrap();
                let r#type = vars.remove("type").unwrap().pop().unwrap();

                let value = parser
                    .parse_expression(value.span().cloned().unwrap_or_else(|| span.clone()), value);

                let r#type = parser.parse_type(
                    r#type.span().cloned().unwrap_or_else(|| span.clone()),
                    r#type,
                );

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Annotate {
                        value: value.boxed(),
                        r#type,
                    },
                }
            })
        };

        let function = |parser: &mut Self| {
            FUNCTION_EXPRESSION.parse(&node).map(|mut vars| {
                let patterns = match vars.remove("pattern") {
                    Some(patterns) => patterns
                        .into_iter()
                        .map(|pattern| {
                            parser.parse_pattern(
                                pattern.span().cloned().unwrap_or_else(|| span.clone()),
                                pattern,
                            )
                        })
                        .collect::<Vec<_>>(),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Pattern,
                        });

                        vec![WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Pattern::Error,
                        }]
                    }
                };

                let body = match vars.remove("body").map(|mut value| value.pop().unwrap()) {
                    Some(expression) => parser.parse_expression(
                        expression.span().cloned().unwrap_or_else(|| span.clone()),
                        expression,
                    ),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Expression,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Expression::Error,
                        }
                    }
                };

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: patterns.into_iter().rfold(body.item, |body, pattern| {
                        let body = pattern.replace(body).boxed();
                        syntax::Expression::Function { pattern, body }
                    }),
                }
            })
        };

        let is = |parser: &mut Self| {
            IS_EXPRESSION.parse(&node).map(|mut vars| {
                let value = vars
                    .remove("input")
                    .map(|mut value| value.pop().unwrap())
                    .map(|expression| {
                        parser
                            .parse_expression(
                                expression.span().cloned().unwrap_or_else(|| span.clone()),
                                expression,
                            )
                            .boxed()
                    });

                let pattern = match vars.remove("pattern").map(|mut value| value.pop().unwrap()) {
                    Some(pattern) => parser.parse_pattern(
                        pattern.span().cloned().unwrap_or_else(|| span.clone()),
                        pattern,
                    ),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Pattern,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Pattern::Error,
                        }
                    }
                };

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Is { value, pattern },
                }
            })
        };

        let collection = |parser: &mut Self| {
            COLLECTION_EXPRESSION.parse(&node).map(|mut vars| {
                let elements = vars
                    .remove("elements")
                    .unwrap_or_default()
                    .into_iter()
                    .map(|element| {
                        parser.parse_expression(
                            element.span().cloned().unwrap_or_else(|| span.clone()),
                            element,
                        )
                    })
                    .collect();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Collection(elements),
                }
            })
        };

        let tuple = |parser: &mut Self| {
            TUPLE_EXPRESSION.parse(&node).map(|mut vars| {
                let elements = vars
                    .remove("elements")
                    .unwrap_or_default()
                    .into_iter()
                    .map(|element| {
                        parser.parse_expression(
                            element.span().cloned().unwrap_or_else(|| span.clone()),
                            element,
                        )
                    })
                    .collect();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Tuple(elements),
                }
            })
        };

        let structure = |parser: &mut Self| {
            STRUCTURE_EXPRESSION.parse(&node).and_then(|mut vars| {
                let fields = vars.remove("field").unwrap_or_default();
                let values = vars.remove("value").unwrap_or_default();

                if fields.is_empty() {
                    return None;
                }

                Some(WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Structure(
                        fields
                            .into_iter()
                            .zip(values)
                            .map(|(field, value)| {
                                let name = match field {
                                    Node::Token(token) => match token.kind {
                                        TokenKind::Symbol(name) => WithInfo {
                                            info: Info {
                                                path: parser.driver.file_path(),
                                                visible_path: parser.driver.visible_path(),
                                                span: token.span,
                                                documentation: Vec::new(),
                                            }
                                            .into(),
                                            item: name.to_string(),
                                        },
                                        _ => return None,
                                    },
                                    _ => return None,
                                };

                                let value = parser.parse_expression(
                                    value.span().cloned().unwrap_or_else(|| span.clone()),
                                    value,
                                );

                                Some(WithInfo {
                                    info: Info {
                                        path: parser.driver.file_path(),
                                        visible_path: parser.driver.visible_path(),
                                        span: span.clone(),
                                        documentation: Vec::new(),
                                    }
                                    .into(),
                                    item: syntax::FieldValue { name, value },
                                })
                            })
                            .collect::<Option<Vec<_>>>()?,
                    ),
                })
            })
        };

        let intrinsic = |parser: &mut Self| {
            INTRINSIC_EXPRESSION.parse(&node).map(|mut vars| {
                let name = vars.remove("name").map(|mut value| value.pop().unwrap());
                let inputs = vars.remove("__extra").unwrap();

                let name = match name {
                    Some(Node::Token(token)) => match token.kind {
                        TokenKind::Text(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span,
                                expected: SyntaxKind::Text,
                            });

                            return WithInfo {
                                info: Info {
                                    path: parser.driver.file_path(),
                                    visible_path: parser.driver.visible_path(),
                                    span: span.clone(),
                                    documentation: Vec::new(),
                                }
                                .into(),
                                item: syntax::Expression::Error,
                            };
                        }
                    },
                    _ => {
                        parser.errors.push(Diagnostic {
                            span: name
                                .and_then(|name| name.span().cloned())
                                .unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Text,
                        });

                        return WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Expression::Error,
                        };
                    }
                };

                let inputs = inputs
                    .into_iter()
                    .map(|input| {
                        parser.parse_expression(
                            input.span().cloned().unwrap_or_else(|| span.clone()),
                            input,
                        )
                    })
                    .collect();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Intrinsic { name, inputs },
                }
            })
        };

        let semantics = |parser: &mut Self| {
            SEMANTICS_EXPRESSION.parse(&node).map(|mut vars| {
                let name = vars.remove("name").map(|mut value| value.pop().unwrap());
                let mut inputs = vars.remove("__extra").unwrap().into_iter();

                let name = match name {
                    Some(Node::Token(token)) => match token.kind {
                        TokenKind::Text(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span,
                                expected: SyntaxKind::Text,
                            });

                            return WithInfo {
                                info: Info {
                                    path: parser.driver.file_path(),
                                    visible_path: parser.driver.visible_path(),
                                    span: span.clone(),
                                    documentation: Vec::new(),
                                }
                                .into(),
                                item: syntax::Expression::Error,
                            };
                        }
                    },
                    _ => {
                        parser.errors.push(Diagnostic {
                            span: name
                                .and_then(|name| name.span().cloned())
                                .unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Text,
                        });

                        return WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Expression::Error,
                        };
                    }
                };

                let body = match inputs.next() {
                    Some(body) => parser.parse_expression(
                        body.span().cloned().unwrap_or_else(|| span.clone()),
                        body,
                    ),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Expression,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Expression::Error,
                        }
                    }
                };

                for input in inputs {
                    parser.errors.push(Diagnostic {
                        span: input.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Nothing,
                    });
                }

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Semantics {
                        name,
                        body: body.boxed(),
                    },
                }
            })
        };

        let when = |parser: &mut Self| {
            WHEN_EXPRESSION.parse(&node).map(|mut vars| {
                let input = vars.remove("input").map(|mut value| value.pop().unwrap());
                let arms = vars.remove("arm");
                let extra = vars.remove("__extra").unwrap();

                let input = match input {
                    Some(input) => parser.parse_expression(
                        input.span().cloned().unwrap_or_else(|| span.clone()),
                        input,
                    ),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Expression,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Expression::Error,
                        }
                    }
                };

                let arms = match arms {
                    Some(arms) => arms
                        .into_iter()
                        .filter_map(|arm| {
                            parser
                                .parse_arm(arm.span().cloned().unwrap_or_else(|| span.clone()), arm)
                        })
                        .collect(),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Block,
                        });

                        Vec::new()
                    }
                };

                for node in extra {
                    parser.errors.push(Diagnostic {
                        span: node.span().cloned().unwrap_or_else(|| span.clone()),
                        expected: SyntaxKind::Nothing,
                    });
                }

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::When {
                        input: input.boxed(),
                        arms,
                    },
                }
            })
        };

        let format = |parser: &mut Self| {
            FORMAT_EXPRESSION.parse(&node).and_then(|mut vars| {
                let text = vars.remove("text").map(|mut value| value.pop().unwrap());
                let inputs = vars.remove("__extra").unwrap();

                let text = match text {
                    Some(Node::Token(token)) => match token.kind {
                        TokenKind::Text(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
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
                        parser.parse_expression(
                            input.span().cloned().unwrap_or_else(|| span.clone()),
                            input,
                        )
                    })
                    .collect();

                Some(WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Format { text, inputs },
                })
            })
        };

        let apply = |parser: &mut Self| {
            APPLY_EXPRESSION.parse(&node).map(|mut vars| {
                let input = vars
                    .remove("input")
                    .map(|mut value| value.pop().unwrap())
                    .map(|input| {
                        parser.parse_expression(
                            input.span().cloned().unwrap_or_else(|| span.clone()),
                            input,
                        )
                    });

                let function = vars
                    .remove("function")
                    .map(|mut value| value.pop().unwrap())
                    .map(|function| {
                        parser.parse_expression(
                            function.span().cloned().unwrap_or_else(|| span.clone()),
                            function,
                        )
                    });

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Apply {
                        input: input.map(|input| input.boxed()),
                        function: function.map(|function| function.boxed()),
                    },
                }
            })
        };

        let compose = |parser: &mut Self| {
            COMPOSE_EXPRESSION.parse(&node).map(|mut vars| {
                let left = vars
                    .remove("left")
                    .map(|mut value| value.pop().unwrap())
                    .map(|left| {
                        parser.parse_expression(
                            left.span().cloned().unwrap_or_else(|| span.clone()),
                            left,
                        )
                    });

                let right = vars
                    .remove("right")
                    .map(|mut value| value.pop().unwrap())
                    .map(|right| {
                        parser.parse_expression(
                            right.span().cloned().unwrap_or_else(|| span.clone()),
                            right,
                        )
                    });

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Compose {
                        outer: right.map(|right| right.boxed()),
                        inner: left.map(|left| left.boxed()),
                    },
                }
            })
        };

        let r#as = |parser: &mut Self| {
            AS_EXPRESSION.parse(&node).map(|mut vars| {
                let value = vars
                    .remove("value")
                    .map(|mut value| value.pop().unwrap())
                    .map(|value| {
                        parser.parse_expression(
                            value.span().cloned().unwrap_or_else(|| span.clone()),
                            value,
                        )
                    });

                let r#type = vars
                    .remove("type")
                    .map(|mut r#type| r#type.pop().unwrap())
                    .map(|r#type| {
                        parser.parse_type(
                            r#type.span().cloned().unwrap_or_else(|| span.clone()),
                            r#type,
                        )
                    });

                let r#type = match r#type {
                    Some(r#type) => r#type,
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Block,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Type::Error,
                        }
                    }
                };

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::As {
                        value: value.map(|value| value.boxed()),
                        r#type,
                    },
                }
            })
        };

        let or = binary_expression_operator!("or", Or);
        let and = binary_expression_operator!("and", And);
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
        let to = binary_expression_operator!("to", To);
        let by = binary_expression_operator!("by", By);

        let terminal = |parser: &mut Self| match &node {
            Node::Token(token) => match &token.kind {
                TokenKind::Symbol(name) => WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: token.span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Name(name.to_string()),
                },
                TokenKind::Number(number) => WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: token.span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Number(number.to_string()),
                },
                TokenKind::Text(text) | TokenKind::Asset(text) => WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: token.span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Text(text.to_string()),
                },
                _ => {
                    parser.errors.push(Diagnostic {
                        span: span.clone(),
                        expected: SyntaxKind::Expression,
                    });

                    WithInfo {
                        info: Info {
                            path: parser.driver.file_path(),
                            visible_path: parser.driver.visible_path(),
                            span: token.span.clone(),
                            documentation: Vec::new(),
                        }
                        .into(),
                        item: syntax::Expression::Error,
                    }
                }
            },
            Node::List(span, elements) => {
                let mut elements = elements.iter().map(|expression| {
                    parser.parse_expression(
                        expression.span().cloned().unwrap_or_else(|| span.clone()),
                        expression.clone(),
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
                        info: Info {
                            path: parser.driver.file_path(),
                            visible_path: parser.driver.visible_path(),
                            span: span.clone(),
                            documentation: Vec::new(),
                        }
                        .into(),
                        item: syntax::Expression::Unit,
                    }
                }
            }
            Node::Block(span, statements) => WithInfo {
                info: Info {
                    path: parser.driver.file_path(),
                    visible_path: parser.driver.visible_path(),
                    span: span.clone(),
                    documentation: Vec::new(),
                }
                .into(),
                item: syntax::Expression::Block(
                    statements
                        .iter()
                        .map(|(documentation, statement)| {
                            parser.parse_statement(
                                statement.span().cloned().unwrap_or_else(|| span.clone()),
                                documentation.clone(),
                                statement.clone(),
                            )
                        })
                        .collect(),
                ),
            },
            _ => {
                parser.errors.push(Diagnostic {
                    span: span.clone(),
                    expected: SyntaxKind::Expression,
                });

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Error,
                }
            }
        };

        annotate(self)
            .or_else(|| function(self))
            .or_else(|| is(self))
            .or_else(|| collection(self))
            .or_else(|| tuple(self))
            .or_else(|| structure(self))
            .or_else(|| intrinsic(self))
            .or_else(|| semantics(self))
            .or_else(|| when(self))
            .or_else(|| format(self))
            .or_else(|| apply(self))
            .or_else(|| or(self))
            .or_else(|| and(self))
            .or_else(|| compose(self))
            .or_else(|| r#as(self))
            .or_else(|| less_than(self))
            .or_else(|| less_than_or_equal(self))
            .or_else(|| greater_than(self))
            .or_else(|| greater_than_or_equal(self))
            .or_else(|| equal(self))
            .or_else(|| not_equal(self))
            .or_else(|| add(self))
            .or_else(|| subtract(self))
            .or_else(|| multiply(self))
            .or_else(|| divide(self))
            .or_else(|| remainder(self))
            .or_else(|| power(self))
            .or_else(|| to(self))
            .or_else(|| by(self))
            .unwrap_or_else(|| terminal(self))
    }

    fn parse_type_function(
        &mut self,
        span: Range<u32>,
        parameters: Vec<Node<'_>>,
        bounds: Vec<Node<'_>>,
    ) -> WithInfo<D::Info, syntax::TypeFunction<D>>
    where
        D::Info: From<Info>,
    {
        let parameters = parameters
            .into_iter()
            .filter_map(|element| self.parse_type_parameter(span.clone(), element))
            .collect();

        // Special case for a single bound wrapped in parentheses
        let bounds = if !bounds.is_empty() && !matches!(bounds.first().unwrap(), Node::List(..)) {
            vec![Node::List(span.clone(), bounds)]
        } else {
            bounds
        };

        let bounds = bounds
            .into_iter()
            .filter_map(|element| self.parse_instance(span.clone(), element))
            .collect();

        WithInfo {
            info: Info {
                path: self.driver.file_path(),
                visible_path: self.driver.visible_path(),
                span,
                documentation: Vec::new(),
            }
            .into(),
            item: syntax::TypeFunction { parameters, bounds },
        }
    }

    fn parse_type_parameter(
        &mut self,
        span: Range<u32>,
        node: Node<'_>,
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
                        (variable . "name"))
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
                self.errors.push(Diagnostic {
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
                    path: self.driver.file_path(),
                    visible_path: self.driver.visible_path(),
                    span: infer.span().cloned().unwrap_or_else(|| span.clone()),
                    documentation: Vec::new(),
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
                    info: Info {
                        path: self.driver.file_path(),
                        visible_path: self.driver.visible_path(),
                        span: token.span,
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: name.to_string(),
                },
                _ => {
                    self.errors.push(Diagnostic {
                        span: token.span,
                        expected: SyntaxKind::Name,
                    });

                    return None;
                }
            },
            _ => {
                self.errors.push(Diagnostic {
                    span: name.span().cloned().unwrap_or(span),
                    expected: SyntaxKind::Name,
                });

                return None;
            }
        };

        let default = default.map(|default| {
            self.parse_type(
                default.span().cloned().unwrap_or_else(|| span.clone()),
                default,
            )
        });

        Some(WithInfo {
            info: Info {
                path: self.driver.file_path(),
                visible_path: self.driver.visible_path(),
                span,
                documentation: Vec::new(),
            }
            .into(),
            item: syntax::TypeParameter {
                name,
                infer,
                default,
            },
        })
    }

    fn parse_instance(
        &mut self,
        span: Range<u32>,
        node: Node<'_>,
    ) -> Option<WithInfo<D::Info, syntax::Instance<D>>>
    where
        D::Info: From<Info>,
    {
        match node {
            Node::Token(token) => match token.kind {
                TokenKind::Symbol(symbol) => Some(WithInfo {
                    info: Info {
                        path: self.driver.file_path(),
                        visible_path: self.driver.visible_path(),
                        span,
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Instance {
                        r#trait: WithInfo {
                            info: Info {
                                path: self.driver.file_path(),
                                visible_path: self.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: String::from(symbol),
                        },
                        parameters: Vec::new(),
                    },
                }),
                _ => {
                    self.errors.push(Diagnostic {
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
                            info: Info {
                                path: self.driver.file_path(),
                                visible_path: self.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: String::from(symbol),
                        },
                        _ => {
                            self.errors.push(Diagnostic {
                                span: token.span,
                                expected: SyntaxKind::Instance,
                            });

                            return None;
                        }
                    },
                    _ => {
                        self.errors.push(Diagnostic {
                            span,
                            expected: SyntaxKind::Trait,
                        });

                        return None;
                    }
                };

                let parameters = elements
                    .map(|node| {
                        self.parse_type(node.span().cloned().unwrap_or_else(|| span.clone()), node)
                    })
                    .collect::<Vec<_>>();

                Some(WithInfo {
                    info: Info {
                        path: self.driver.file_path(),
                        visible_path: self.driver.visible_path(),
                        span,
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Instance {
                        r#trait,
                        parameters,
                    },
                })
            }
            _ => {
                self.errors.push(Diagnostic {
                    span,
                    expected: SyntaxKind::Instance,
                });

                None
            }
        }
    }

    fn parse_type_representation(
        &mut self,
        span: Range<u32>,
        node: Node<'_>,
    ) -> WithInfo<D::Info, syntax::TypeRepresentation<D>>
    where
        D::Info: From<Info>,
    {
        grammar! {
            static SIMPLE_ENUMERATION_TYPE_REPRESENTATION = r#"
            (or
                (repeat-list . (variable . "variant"))
                (block
                    (repeat-list . (variable . "variant"))))
        "#;

            static COMPOUND_TYPE_REPRESENTATION = r#"
            (or
                (repeat-block . (variable . "member"))
                (variable . "member"))
        "#;
        }

        let simple_enumeration = |parser: &mut Self| {
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
                                    info: Info {
                                        path: parser.driver.file_path(),
                                        visible_path: parser.driver.visible_path(),
                                        span: token.span,
                                        documentation: Vec::new(),
                                    }
                                    .into(),
                                    item: name.to_string(),
                                }),
                                _ => {
                                    parser.errors.push(Diagnostic {
                                        span: token.span,
                                        expected: SyntaxKind::Name,
                                    });

                                    None
                                }
                            },
                            _ => {
                                parser.errors.push(Diagnostic {
                                    span: variant.span().cloned().unwrap_or_else(|| span.clone()),
                                    expected: SyntaxKind::Name,
                                });

                                None
                            }
                        })
                        .collect();

                    WithInfo {
                        info: Info {
                            path: parser.driver.file_path(),
                            visible_path: parser.driver.visible_path(),
                            span: span.clone(),
                            documentation: Vec::new(),
                        }
                        .into(),
                        item: syntax::TypeRepresentation::SimpleEnumeration(variants),
                    }
                })
        };

        let compound = |parser: &mut Self| {
            COMPOUND_TYPE_REPRESENTATION.parse(&node).map(|mut vars| {
                let members = vars
                    .remove("member")
                    .unwrap()
                    .into_iter()
                    .filter_map(|member| {
                        parser.parse_type_member(
                            member.span().cloned().unwrap_or_else(|| span.clone()),
                            member,
                        )
                    })
                    .collect();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::TypeRepresentation::Compound(members),
                }
            })
        };

        simple_enumeration(self)
            .or_else(|| compound(self))
            .unwrap_or_else(|| {
                self.errors.push(Diagnostic {
                    span: span.clone(),
                    expected: SyntaxKind::TypeRepresentation,
                });

                WithInfo {
                    info: Info {
                        path: self.driver.file_path(),
                        visible_path: self.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::TypeRepresentation::Opaque,
                }
            })
    }

    fn parse_type_member(
        &mut self,
        span: Range<u32>,
        node: Node<'_>,
    ) -> Option<WithInfo<D::Info, syntax::TypeMember<D>>>
    where
        D::Info: From<Info>,
    {
        grammar! {
            static FIELD_EXPRESSION = r#"
            (non-associative-binary-operator
                "::"
                (variable . "name")
                (variable . "type"))
        "#;

            static VARIANT_EXPRESSION = r#"
            (or
                (prefix-list
                    (variable . "name"))
                (variable . "name"))
        "#;
        }

        let field = |parser: &mut Self| {
            FIELD_EXPRESSION.parse(&node).map(|mut vars| {
                let name = vars.remove("name").unwrap().pop().unwrap();
                let r#type = vars.remove("type").unwrap().pop().unwrap();

                let name = match name {
                    Node::Token(token) => match token.kind {
                        TokenKind::Symbol(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span,
                                expected: SyntaxKind::Name,
                            });

                            return None;
                        }
                    },
                    _ => {
                        parser.errors.push(Diagnostic {
                            span: name.span().cloned().unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Name,
                        });

                        return None;
                    }
                };

                let r#type = parser.parse_type(
                    r#type.span().cloned().unwrap_or_else(|| span.clone()),
                    r#type,
                );

                Some(WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::TypeMember {
                        name,
                        kind: syntax::TypeMemberKind::Field(r#type),
                    },
                })
            })
        };

        let variant = |parser: &mut Self| {
            VARIANT_EXPRESSION.parse(&node).map(|mut vars| {
                let name = vars.remove("name").unwrap().pop().unwrap();
                let types = vars.remove("__extra").unwrap_or_default();

                let name = match name {
                    Node::Token(token) => match token.kind {
                        TokenKind::Symbol(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span,
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span,
                                expected: SyntaxKind::Name,
                            });

                            return None;
                        }
                    },
                    _ => {
                        parser.errors.push(Diagnostic {
                            span: name.span().cloned().unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Name,
                        });

                        return None;
                    }
                };

                let types = types
                    .into_iter()
                    .map(|r#type| {
                        parser.parse_type(
                            r#type.span().cloned().unwrap_or_else(|| span.clone()),
                            r#type,
                        )
                    })
                    .collect();

                Some(WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::TypeMember {
                        name,
                        kind: syntax::TypeMemberKind::Variant(types),
                    },
                })
            })
        };

        field(self).or_else(|| variant(self)).unwrap_or_else(|| {
            self.errors.push(Diagnostic {
                span: span.clone(),
                expected: SyntaxKind::TypeMember,
            });

            None
        })
    }

    fn parse_type(&mut self, span: Range<u32>, node: Node<'_>) -> WithInfo<D::Info, syntax::Type<D>>
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

            static TUPLE_TYPE = r#"
                (or
                    (variadic-operator
                        ";"
                        (applied . (variable . "elements")))
                    (variadic-operator
                        ";"
                        unapplied))
            "#;

            static LAZY_TYPE = r#"
                (list
                    (symbol . "defer")
                    (variable . "type"))
            "#;
        }

        let function = |parser: &mut Self| {
            FUNCTION_TYPE.parse(&node).map(|mut vars| {
                let input = match vars.remove("input").map(|mut value| value.pop().unwrap()) {
                    Some(input) => parser
                        .parse_type(input.span().cloned().unwrap_or_else(|| span.clone()), input),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Type,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Type::Error,
                        }
                    }
                };

                let output = match vars.remove("output").map(|mut value| value.pop().unwrap()) {
                    Some(output) => parser.parse_type(
                        output.span().cloned().unwrap_or_else(|| span.clone()),
                        output,
                    ),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Type,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Type::Error,
                        }
                    }
                };

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Type::Function {
                        input: input.boxed(),
                        output: output.boxed(),
                    },
                }
            })
        };

        let tuple = |parser: &mut Self| {
            TUPLE_TYPE.parse(&node).map(|mut vars| {
                let elements = vars
                    .remove("elements")
                    .unwrap_or_default()
                    .into_iter()
                    .map(|element| {
                        parser.parse_type(
                            element.span().cloned().unwrap_or_else(|| span.clone()),
                            element,
                        )
                    })
                    .collect();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Type::Tuple(elements),
                }
            })
        };

        let defer = |parser: &mut Self| {
            LAZY_TYPE.parse(&node).map(|mut vars| {
                let r#type = vars.remove("type").unwrap().pop().unwrap();

                let r#type = parser.parse_type(
                    r#type.span().cloned().unwrap_or_else(|| span.clone()),
                    r#type,
                );

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Type::Deferred(r#type.boxed()),
                }
            })
        };

        let terminal = |parser: &mut Self| match &node {
            Node::Token(token) => match &token.kind {
                TokenKind::Symbol(name) => WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: token.span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: if name == "_" {
                        syntax::Type::Placeholder
                    } else {
                        syntax::Type::Declared {
                            name: WithInfo {
                                info: Info {
                                    path: parser.driver.file_path(),
                                    visible_path: parser.driver.visible_path(),
                                    span: token.span.clone(),
                                    documentation: Vec::new(),
                                }
                                .into(),
                                item: name.to_string(),
                            },
                            parameters: Vec::new(),
                        }
                    },
                },
                _ => {
                    parser.errors.push(Diagnostic {
                        span: span.clone(),
                        expected: SyntaxKind::Type,
                    });

                    WithInfo {
                        info: Info {
                            path: parser.driver.file_path(),
                            visible_path: parser.driver.visible_path(),
                            span: token.span.clone(),
                            documentation: Vec::new(),
                        }
                        .into(),
                        item: syntax::Type::Error,
                    }
                }
            },
            Node::List(span, elements) => {
                let mut elements = elements.iter();

                let name = match elements.next() {
                    Some(Node::Token(token)) => match &token.kind {
                        TokenKind::Symbol(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span.clone(),
                                expected: SyntaxKind::Name,
                            });

                            return WithInfo {
                                info: Info {
                                    path: parser.driver.file_path(),
                                    visible_path: parser.driver.visible_path(),
                                    span: span.clone(),
                                    documentation: Vec::new(),
                                }
                                .into(),
                                item: syntax::Type::Error,
                            };
                        }
                    },
                    Some(token) => {
                        parser.errors.push(Diagnostic {
                            span: token.span().cloned().unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Type,
                        });

                        return WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Type::Error,
                        };
                    }
                    None => {
                        return WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Type::Unit,
                        };
                    }
                };

                let parameters = elements
                    .map(|expression| {
                        parser.parse_type(
                            expression.span().cloned().unwrap_or_else(|| span.clone()),
                            expression.clone(),
                        )
                    })
                    .collect();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Type::Declared { name, parameters },
                }
            }
            _ => {
                parser.errors.push(Diagnostic {
                    span: span.clone(),
                    expected: SyntaxKind::Type,
                });

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Type::Error,
                }
            }
        };

        function(self)
            .or_else(|| tuple(self))
            .or_else(|| defer(self))
            .unwrap_or_else(|| terminal(self))
    }

    fn parse_pattern(
        &mut self,
        span: Range<u32>,
        node: Node<'_>,
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
                        (variable . "left")))
                (binary-operator
                    "or"
                    (partially-applied-right .
                        (variable . "right")))
                (binary-operator
                    "or"
                    (applied
                        (variable . "left")
                        (variable . "right"))))
        "#;

        static DESTRUCTURE_PATTERN = r#"
            (or
                (non-associative-binary-operator
                    ":"
                    (variable . "field")
                    (variable . "pattern"))
                (repeat-block .
                    (non-associative-binary-operator
                        ":"
                        (variable . "field")
                        (variable . "pattern"))))
        "#;


        static TUPLE_PATTERN = r#"
            (or
                (variadic-operator
                    ";"
                    (applied . (variable . "elements")))
                (variadic-operator
                    ";"
                    unapplied))
        "#;
        }

        let or = |parser: &mut Self| {
            OR_PATTERN.parse(&node).map(|mut vars| {
                let left = match vars.remove("left").map(|mut value| value.pop().unwrap()) {
                    Some(left) => parser
                        .parse_pattern(left.span().cloned().unwrap_or_else(|| span.clone()), left),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Pattern,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Pattern::Error,
                        }
                    }
                };

                let right = match vars.remove("right").map(|mut value| value.pop().unwrap()) {
                    Some(right) => parser.parse_pattern(
                        right.span().cloned().unwrap_or_else(|| span.clone()),
                        right,
                    ),
                    None => {
                        parser.errors.push(Diagnostic {
                            span: span.clone(),
                            expected: SyntaxKind::Pattern,
                        });

                        WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Pattern::Error,
                        }
                    }
                };

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Pattern::Or {
                        left: left.boxed(),
                        right: right.boxed(),
                    },
                }
            })
        };

        let destructure = |parser: &mut Self| {
            DESTRUCTURE_PATTERN.parse(&node).map(|mut vars| {
                let fields = vars.remove("field").unwrap_or_default();
                let patterns = vars.remove("pattern").unwrap_or_default();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Pattern::Destructure(
                        fields
                            .into_iter()
                            .zip(patterns)
                            .filter_map(|(field, pattern)| {
                                let name = match field {
                                    Node::Token(token) => match token.kind {
                                        TokenKind::Symbol(name) => WithInfo {
                                            info: Info {
                                                path: parser.driver.file_path(),
                                                visible_path: parser.driver.visible_path(),
                                                span: token.span,
                                                documentation: Vec::new(),
                                            }
                                            .into(),
                                            item: name.to_string(),
                                        },
                                        _ => {
                                            parser.errors.push(Diagnostic {
                                                span: span.clone(),
                                                expected: SyntaxKind::Name,
                                            });

                                            return None;
                                        }
                                    },
                                    _ => {
                                        parser.errors.push(Diagnostic {
                                            span: span.clone(),
                                            expected: SyntaxKind::Name,
                                        });

                                        return None;
                                    }
                                };

                                let pattern = parser.parse_pattern(
                                    pattern.span().cloned().unwrap_or_else(|| span.clone()),
                                    pattern,
                                );

                                Some(WithInfo {
                                    info: Info {
                                        path: parser.driver.file_path(),
                                        visible_path: parser.driver.visible_path(),
                                        span: span.clone(),
                                        documentation: Vec::new(),
                                    }
                                    .into(),
                                    item: syntax::FieldPattern { name, pattern },
                                })
                            })
                            .collect(),
                    ),
                }
            })
        };

        let tuple = |parser: &mut Self| {
            TUPLE_PATTERN.parse(&node).map(|mut vars| {
                let elements = vars
                    .remove("elements")
                    .unwrap_or_default()
                    .into_iter()
                    .map(|element| {
                        parser.parse_pattern(
                            element.span().cloned().unwrap_or_else(|| span.clone()),
                            element,
                        )
                    })
                    .collect();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Pattern::Tuple(elements),
                }
            })
        };

        let terminal = |parser: &mut Self| match &node {
            Node::Token(token) => match &token.kind {
                TokenKind::Symbol(name) => WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: token.span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: if name == "_" {
                        syntax::Pattern::Wildcard
                    } else {
                        syntax::Pattern::VariantOrName(name.to_string())
                    },
                },
                TokenKind::Number(number) => WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: token.span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Pattern::Number(number.to_string()),
                },
                TokenKind::Text(text) | TokenKind::Asset(text) => WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: token.span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Pattern::Text(text.to_string()),
                },
                _ => {
                    parser.errors.push(Diagnostic {
                        span: span.clone(),
                        expected: SyntaxKind::Type,
                    });

                    WithInfo {
                        info: Info {
                            path: parser.driver.file_path(),
                            visible_path: parser.driver.visible_path(),
                            span: token.span.clone(),
                            documentation: Vec::new(),
                        }
                        .into(),
                        item: syntax::Pattern::Error,
                    }
                }
            },
            Node::List(span, elements) => {
                let mut elements = elements.iter();

                let variant = match elements.next() {
                    Some(Node::Token(token)) => match &token.kind {
                        TokenKind::Symbol(name) => WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: token.span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: name.to_string(),
                        },
                        _ => {
                            parser.errors.push(Diagnostic {
                                span: token.span.clone(),
                                expected: SyntaxKind::Name,
                            });

                            return WithInfo {
                                info: Info {
                                    path: parser.driver.file_path(),
                                    visible_path: parser.driver.visible_path(),
                                    span: span.clone(),
                                    documentation: Vec::new(),
                                }
                                .into(),
                                item: syntax::Pattern::Error,
                            };
                        }
                    },
                    Some(token) => {
                        parser.errors.push(Diagnostic {
                            span: token.span().cloned().unwrap_or_else(|| span.clone()),
                            expected: SyntaxKind::Name,
                        });

                        return WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Pattern::Error,
                        };
                    }
                    None => {
                        return WithInfo {
                            info: Info {
                                path: parser.driver.file_path(),
                                visible_path: parser.driver.visible_path(),
                                span: span.clone(),
                                documentation: Vec::new(),
                            }
                            .into(),
                            item: syntax::Pattern::Unit,
                        };
                    }
                };

                let value_patterns = elements
                    .map(|pattern| {
                        parser.parse_pattern(
                            pattern.span().cloned().unwrap_or_else(|| span.clone()),
                            pattern.clone(),
                        )
                    })
                    .collect::<Vec<_>>();

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
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
                parser.errors.push(Diagnostic {
                    span: span.clone(),
                    expected: SyntaxKind::Pattern,
                });

                WithInfo {
                    info: Info {
                        path: parser.driver.file_path(),
                        visible_path: parser.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Pattern::Error,
                }
            }
        };

        or(self)
            .or_else(|| destructure(self))
            .or_else(|| tuple(self))
            .unwrap_or_else(|| terminal(self))
    }

    fn parse_arm(
        &mut self,
        span: Range<u32>,
        node: Node<'_>,
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
                self.errors.push(Diagnostic {
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
            Some(pattern) => self.parse_pattern(
                pattern.span().cloned().unwrap_or_else(|| span.clone()),
                pattern,
            ),
            None => {
                self.errors.push(Diagnostic {
                    span: span.clone(),
                    expected: SyntaxKind::Pattern,
                });

                WithInfo {
                    info: Info {
                        path: self.driver.file_path(),
                        visible_path: self.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Pattern::Error,
                }
            }
        };

        let condition = condition.map(|condition| {
            self.parse_expression(
                condition.span().cloned().unwrap_or_else(|| span.clone()),
                condition,
            )
        });

        let body = match body {
            Some(body) => {
                self.parse_expression(body.span().cloned().unwrap_or_else(|| span.clone()), body)
            }
            None => {
                self.errors.push(Diagnostic {
                    span: span.clone(),
                    expected: SyntaxKind::Expression,
                });

                WithInfo {
                    info: Info {
                        path: self.driver.file_path(),
                        visible_path: self.driver.visible_path(),
                        span: span.clone(),
                        documentation: Vec::new(),
                    }
                    .into(),
                    item: syntax::Expression::Error,
                }
            }
        };

        Some(WithInfo {
            info: Info {
                path: self.driver.file_path(),
                visible_path: self.driver.visible_path(),
                span,
                documentation: Vec::new(),
            }
            .into(),
            item: syntax::Arm {
                pattern,
                condition,
                body,
            },
        })
    }
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

        test_grammar(
            |span, node, parser| parser.parse_type_parameter(span, node).unwrap(),
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

        test_grammar(
            |span, node, parser| parser.parse_type_parameter(span, node).unwrap(),
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

        test_grammar(
            |span, node, parser| parser.parse_type_parameter(span, node).unwrap(),
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

        test_grammar(
            |span, node, parser| parser.parse_type_parameter(span, node).unwrap(),
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

        test_grammar(
            |span, node, parser| parser.parse_instance(span, node).unwrap(),
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

        test_grammar(
            |span, node, parser| parser.parse_instance(span, node).unwrap(),
            code,
            expected,
        );
    }
}
