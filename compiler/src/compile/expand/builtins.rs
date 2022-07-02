use crate::{
    compile::expand::{
        Expander, LanguageItem, Node, NodeKind, Operator, OperatorPrecedence, Scope, ScopeValue,
        Template,
    },
    diagnostics::*,
    helpers::InternedString,
    parse::Span,
    FilePath, Loader, TemplateId,
};
use once_cell::sync::OnceCell;
use std::{collections::VecDeque, str::FromStr};

pub(super) fn load_builtins<L: Loader>(expander: &mut Expander<L>, scope: &Scope) {
    macro_rules! once_id {
        ($kind:ident) => {{
            paste::paste! {
                static ID: OnceCell<[<$kind:camel Id>]> = OnceCell::new();
                *ID.get_or_init(|| expander.compiler.[<new_ $kind _id>]())
            }
        }};
    }

    let mut scope_values = scope.values.lock();

    // `:` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new(":"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Assignment,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`:` operator"),
            |expander, span, mut inputs, _, _, scope| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    match rhs.kind {
                        NodeKind::Template(inputs, body) => {
                            let name = match lhs.kind {
                                NodeKind::Name(name) => name,
                                _ => {
                                    expander.compiler.diagnostics.add(Diagnostic::error(
                                        "template declaration must be assigned to a name",
                                        vec![Note::primary(lhs.span, "try providing a name here")],
                                    ));

                                    return Node {
                                        span,
                                        kind: NodeKind::Error,
                                    };
                                }
                            };

                            let id = expander.compiler.new_template_id();

                            expander
                                .declarations
                                .lock()
                                .templates
                                .insert(id, Template::syntax(rhs.span, inputs, *body));

                            scope.values.lock().insert(name, ScopeValue::Template(id));

                            Node {
                                span,
                                kind: NodeKind::Placeholder,
                            }
                        }
                        NodeKind::Operator(precedence, inputs, body) => {
                            let name = match lhs.kind {
                                NodeKind::Name(name) => name,
                                _ => {
                                    expander.compiler.diagnostics.add(Diagnostic::error(
                                        "operator declaration must be assigned to a name",
                                        vec![Note::primary(lhs.span, "try providing a name here")],
                                    ));

                                    return Node {
                                        span,
                                        kind: NodeKind::Error,
                                    };
                                }
                            };

                            let id = expander.compiler.new_template_id();

                            expander
                                .declarations
                                .lock()
                                .templates
                                .insert(id, Template::syntax(rhs.span, inputs, *body));

                            scope.values.lock().insert(
                                name,
                                ScopeValue::Operator(Operator {
                                    precedence,
                                    template: id,
                                }),
                            );

                            Node {
                                span,
                                kind: NodeKind::Placeholder,
                            }
                        }
                        _ => Node {
                            span,
                            kind: NodeKind::Assign(Box::new(lhs), Box::new(rhs)),
                        },
                    }
                })
            },
        ),
    );

    // `->` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new("->"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Function,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`->` operator"),
            |_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Function(Box::new(lhs), Box::new(rhs)),
                    }
                })
            },
        ),
    );

    // `=>` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new("=>"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::TypeFunction,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`=>` operator"),
            |_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::TypeFunction(Box::new(lhs), Box::new(rhs)),
                    }
                })
            },
        ),
    );

    // `where` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new("where"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Where,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`where` operator"),
            |_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Where(Box::new(lhs), Box::new(rhs)),
                    }
                })
            },
        ),
    );

    // `::` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new("::"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Annotation,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`::` operator"),
            |_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Annotate(Box::new(lhs), Box::new(rhs)),
                    }
                })
            },
        ),
    );

    // `~>` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new("~>"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Function,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`~>` operator"),
            |expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    let inputs = match lhs.kind {
                        NodeKind::Error => {
                            return Node {
                                span,
                                kind: NodeKind::Error,
                            }
                        }
                        NodeKind::Empty => Vec::new(),
                        NodeKind::Name(name) => vec![name],
                        NodeKind::List(names) => names
                            .into_iter()
                            .filter_map(|node| match node.kind {
                                NodeKind::Error => None,
                                NodeKind::Name(name) => Some(name),
                                _ => {
                                    expander.compiler.diagnostics.add(Diagnostic::error(
                                        "expected name of template input",
                                        vec![Note::primary(node.span, "expected name here")],
                                    ));

                                    None
                                }
                            })
                            .collect(),
                        _ => {
                            expander.compiler.diagnostics.add(Diagnostic::error(
                                "expected template inputs",
                                vec![Note::primary(span, "try providing some names")],
                            ));

                            return Node {
                                span,
                                kind: NodeKind::Error,
                            };
                        }
                    };

                    Node {
                        span,
                        kind: NodeKind::Template(inputs, Box::new(rhs)),
                    }
                })
            },
        ),
    );

    // `operator` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new("operator"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Function,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(Span::builtin("`operator` operator"), |expander, span, mut inputs, _, _, _| Box::pin(async move { let rhs = inputs.pop().unwrap();
            let lhs = inputs.pop().unwrap();

            let precedence = match lhs.kind {
                NodeKind::Name(name) => match name.as_str() {
                    "power" => OperatorPrecedence::Power,
                    "multiplication" => OperatorPrecedence::Multiplication,
                    "addition" => OperatorPrecedence::Addition,
                    "comparison" => OperatorPrecedence::Comparison,
                    "conjunction" => OperatorPrecedence::Conjunction,
                    "disjunction" => OperatorPrecedence::Disjunction,
                    "dot" => OperatorPrecedence::Dot,
                    _ => {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "invalid precedence name",
                            vec![Note::primary(
                                lhs.span,
                                "try providing a valid precedence name, like `function` or `addition`",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }
                }
                _ => {
                    expander.compiler.diagnostics.add(Diagnostic::error(
                        "expected precedence on left-hand side of operator declaration",
                        vec![Note::primary(lhs.span, "expected a valid precedence name here, like `function` or `addition`")],
                    ));

                    return Node {
                        span,
                        kind: NodeKind::Error,
                    };
                }
            };

            let (inputs, body) = match rhs.kind {
                NodeKind::Template(inputs, body) => (inputs, body),
                _ => {
                    expander.compiler.diagnostics.add(Diagnostic::error(
                        "expected template on right-hand side of operator declaration",
                        vec![Note::primary(rhs.span, "expected a template here")],
                    ));

                    return Node {
                        span,
                        kind: NodeKind::Error,
                    };
                }
            };

            Node {
                span,
                kind: NodeKind::Operator(precedence, inputs, body),
            } })),
    );

    // `or` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new("or"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Disjunction,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`or` operator"),
            |_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Or(Box::new(lhs), Box::new(rhs)),
                    }
                })
            },
        ),
    );

    // `,` operator

    let id = once_id!(template);

    scope_values.insert(
        InternedString::new(","),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Comma,
            template: id,
        }),
    );

    expander.declarations.lock().templates.insert(
        id,
        Template::function(Span::builtin("`,` operator"), |_, span, inputs, _, _, _| {
            Box::pin(async move {
                Node {
                    span,
                    kind: NodeKind::Tuple(inputs),
                }
            })
        }),
    );

    // `use` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("use"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`use` template"),
            |expander, span, mut inputs, _, _, scope| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.report_wrong_template_arity("use", span, inputs.len(), 1);

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let input = inputs.pop().unwrap();

                    match input.kind {
                        NodeKind::Text(path) => {
                            if let Some(file) =
                                (expander.load)(&expander.compiler, FilePath::Path(path)).await
                            {
                                expander.add_dependency(file, scope);
                            }

                            Node {
                                span,
                                kind: NodeKind::Placeholder,
                            }
                        }
                        _ => Node {
                            span,
                            kind: NodeKind::Use(Box::new(input)),
                        },
                    }
                })
            },
        ),
    );

    // `external` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("external"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`external` template"),
            |expander, span, inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() < 2 {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected at least 2 inputs to template `external`",
                            vec![Note::primary(
                                span,
                                "`external` requires a namespace and identifier",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let mut inputs = VecDeque::from(inputs);

                    let namespace = inputs.pop_front().unwrap();
                    let identifier = inputs.pop_front().unwrap();
                    let inputs = Vec::from(inputs);

                    Node {
                        span,
                        kind: NodeKind::External(Box::new(namespace), Box::new(identifier), inputs),
                    }
                })
            },
        ),
    );

    // `type` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("type"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(Span::builtin("`type` template"), |expander, span, mut inputs, _, _, _| Box::pin(async move { if inputs.is_empty() {
                return Node { span, kind: NodeKind::Type(None) }
            }

            if inputs.len() != 1 {
                expander.compiler.diagnostics.add(Diagnostic::error(
                    "expected 0 or 1 inputs to template `type`",
                    vec![Note::primary(
                        span,
                        "`type` may be used standalone, or with a block denoting the type's fields or variants",
                    )],
                ));

                return Node {
                    span,
                    kind: NodeKind::Error,
                };
            }

            let block = inputs.pop().unwrap();

            let fields = match block.kind {
                NodeKind::Block(statements) => statements,
                _ => {
                    expander.compiler.diagnostics.add(Diagnostic::error(
                        "expected a block here",
                        vec![Note::primary(
                            block.span,
                            "`when` requires a block containing functions",
                        )],
                    ));

                    return Node {
                        span,
                        kind: NodeKind::Error,
                    };
                }
            };

            Node {
                span,
                kind: NodeKind::Type(Some(fields)),
            } })),
    );

    // `trait` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("trait"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`trait` template"),
            |expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected 1 input to template `trait`",
                            vec![Note::primary(span, "`trait` requires a type")],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let ty = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Trait(Box::new(ty)),
                    }
                })
            },
        ),
    );

    // `instance` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("instance"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`instance` template"),
            |expander, span, inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.is_empty() {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected at least 1 input to template `instance`",
                            vec![Note::primary(
                                span,
                                "`instance` requires the name of a trait and its parameters",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let mut inputs = VecDeque::from(inputs);

                    let trait_name = inputs.pop_front().unwrap();
                    let parameters = Vec::from(inputs);

                    Node {
                        span,
                        kind: NodeKind::Instance(Box::new(trait_name), parameters),
                    }
                })
            },
        ),
    );

    // `format` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("format"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`format` template"),
            move |expander, span, inputs, _, _, _| Box::pin(async move { if inputs.is_empty() {
                    expander.compiler.diagnostics.add(Diagnostic::error(
                        "expected at least 1 input to template `format`",
                        vec![Note::primary(
                            span,
                            "`instance` requires text containing `_` placeholders",
                        )],
                    ));

                    return Node {
                        span,
                        kind: NodeKind::Error,
                    };
                }

                let mut inputs = VecDeque::from(inputs);

                let format_text = inputs.pop_front().unwrap();

                if let NodeKind::Text(text) = format_text.kind {
                    let placeholder_count = text.split('_').count() - 1;

                    if placeholder_count != inputs.len() {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "wrong number of inputs to `format` text",
                            vec![Note::primary(
                                span,
                                format!(
                                    "text contains {} placeholders, but {} inputs were provided",
                                    placeholder_count,
                                    inputs.len()
                                ),
                            )],
                        ));
                    }
                } else {
                    expander.compiler.diagnostics.add(Diagnostic::error(
                        "expected text",
                        vec![Note::primary(
                            span,
                            "`instance` requires text containing `_` placeholders",
                        )],
                    ));

                    return Node {
                        span,
                        kind: NodeKind::Error,
                    };
                };

                Node {
                    span,
                    kind: NodeKind::Annotate(
                        Box::new(Node {
                            span,
                            kind: NodeKind::External(
                                Box::new(Node {
                                    span: Span::builtin("\"builtin\" namespace in call to `external` in `format` template"),
                                    kind: NodeKind::Text(InternedString::new("builtin")),
                                }),
                                Box::new(Node {
                                    span: Span::builtin("\"format\" identifier in call to `external` in `format` template"),
                                    kind: NodeKind::Text(InternedString::new("format")),
                                }),
                                vec![
                                    format_text,
                                    Node {
                                        span: Span::builtin("generated list as input to `format`"),
                                        kind: NodeKind::ListLiteral(
                                            inputs
                                                .into_iter()
                                                .map(|input| Node {
                                                    span: input.span,
                                                    kind: NodeKind::List(vec![
                                                        Node {
                                                            span: input.span,
                                                            kind: NodeKind::Name(
                                                                InternedString::new("Show"),
                                                            ),
                                                        },
                                                        input,
                                                    ]),
                                                })
                                                .collect(),
                                        ),
                                    },
                                ],
                            ),
                        }),
                        Box::new(Node {
                            span: Span::builtin("`Text` type in type annotation in output of `format`"),
                            kind: NodeKind::Name(InternedString::new("Text")),
                        }),
                    ),
                } }),
        ),
    );

    // `when` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("when"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`when` template"),
            |expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 2 {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected 2 inputs to template `when`",
                            vec![Note::primary(
                                span,
                                "`when` accepts a value to match and a block containing functions",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let block = inputs.pop().unwrap();
                    let input = inputs.pop().unwrap();

                    let block = match block.kind {
                        NodeKind::Block(statements) => statements,
                        _ => {
                            expander.compiler.diagnostics.add(Diagnostic::error(
                                "expected a block here",
                                vec![Note::primary(
                                block.span,
                                "`type` requires a block denoting the type's fields or variants",
                            )],
                            ));

                            return Node {
                                span,
                                kind: NodeKind::Error,
                            };
                        }
                    };

                    Node {
                        span,
                        kind: NodeKind::When(Box::new(input), block),
                    }
                })
            },
        ),
    );

    // `return` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("return"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`return` template"),
            |expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected 1 input to template `return`",
                            vec![Note::primary(
                                span,
                                "`return` accepts a value to exit a function early with",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let value = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Return(Box::new(value)),
                    }
                })
            },
        ),
    );

    // `loop` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("loop"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`loop` template"),
            |expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected 1 input to template `loop`",
                            vec![Note::primary(
                                span,
                                "`loop` accepts a value to evaluate repeatedly",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let value = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Loop(Box::new(value)),
                    }
                })
            },
        ),
    );

    // `break` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("break"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`break` template"),
            |expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected 1 input to template `break`",
                            vec![Note::primary(
                                span,
                                "`break` accepts a value to exit a loop with",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let value = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Break(Box::new(value)),
                    }
                })
            },
        ),
    );

    // `continue` template

    let id = once_id!(template);

    scope_values.insert(InternedString::new("continue"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`continue` template"),
            |expander, span, inputs, _, _, _| {
                Box::pin(async move {
                    if !inputs.is_empty() {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "`continue` does not accept inputs",
                            vec![Note::primary(span, "try removing these inputs")],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    Node {
                        span,
                        kind: NodeKind::Continue,
                    }
                })
            },
        ),
    );

    // `language` attribute

    let id = once_id!(template);

    scope_values.insert(InternedString::new("language"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`language` attribute"),
            |expander, span, mut inputs, _, attributes, _| {
                Box::pin(async move {
                    if inputs.len() != 2 {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected 2 inputs to template `language`",
                            vec![Note::primary(
                                span,
                                "`language` accepts the name of a language item and a declaration",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let node = inputs.pop().unwrap();
                    let item = inputs.pop().unwrap();

                    let name = match item.kind {
                        NodeKind::Text(text) => text,
                        _ => {
                            expander.compiler.diagnostics.add(Diagnostic::error(
                                "`language` expects a text value",
                                vec![Note::primary(item.span, "expected text here")],
                            ));

                            return node;
                        }
                    };

                    let language_item = match LanguageItem::from_str(&name) {
                        Ok(item) => item,
                        Err(_) => {
                            expander.compiler.diagnostics.add(Diagnostic::error(
                                "invalid `language` item",
                                vec![Note::primary(
                                    item.span,
                                    "expected a valid `language` item here",
                                )],
                            ));

                            return node;
                        }
                    };

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.diagnostics.add(Diagnostic::error(
                                "`language` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    format!(
                                        r#"try putting this between brackets: (`[language "{}"]`)"#,
                                        name
                                    ),
                                )],
                            ));

                            return node;
                        }
                    };

                    if attributes.language_item.is_some() {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "`language` item is already set for this statement",
                            vec![Note::primary(
                                span,
                                "cannot use more than one `language` item per statement",
                            )],
                        ));

                        return node;
                    }

                    attributes.language_item = Some(language_item);

                    node
                })
            },
        ),
    );

    // `help` attribute

    let id = once_id!(template);

    scope_values.insert(InternedString::new("help"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`help` attribute"),
            |expander, span, mut inputs, _, attributes, _| {
                Box::pin(async move {
                    if inputs.len() != 2 {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "expected 2 inputs to template `help`",
                            vec![Note::primary(
                                span,
                                "`help` accepts documentation text and a declaration",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let node = inputs.pop().unwrap();
                    let item = inputs.pop().unwrap();

                    let doc = match item.kind {
                        NodeKind::Text(text) => text,
                        _ => {
                            expander.compiler.diagnostics.add(Diagnostic::error(
                                "`help` expects a text value",
                                vec![Note::primary(item.span, "expected text here")],
                            ));

                            return node;
                        }
                    };

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.diagnostics.add(Diagnostic::error(
                                "`help` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    r#"try putting this between brackets: (`[help "..."]`)"#,
                                )],
                            ));

                            return node;
                        }
                    };

                    attributes.help.push_front(doc);

                    node
                })
            },
        ),
    );

    // `no-std` attribute

    let id = once_id!(template);

    scope_values.insert(InternedString::new("no-std"), ScopeValue::Template(id));

    expander.declarations.lock().templates.insert(
        id,
        Template::function(
            Span::builtin("`no-std` attribute"),
            |expander, span, inputs, attributes, _, _| {
                Box::pin(async move {
                    if !inputs.is_empty() {
                        expander.compiler.diagnostics.add(Diagnostic::error(
                            "`no-std` attribute does not accept inputs",
                            vec![Note::primary(
                                inputs
                                    .first()
                                    .unwrap()
                                    .span
                                    .with_end(inputs.last().unwrap().span.end),
                                "try removing these",
                            )],
                        ));

                        return Node {
                            span,
                            kind: NodeKind::Error,
                        };
                    }

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.diagnostics.add(Diagnostic::error(
                                "`no-std` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    r#"try putting this between double brackets: (`[[no-std]]`)"#,
                                )],
                            ));

                            return Node {
                                span,
                                kind: NodeKind::Error,
                            };
                        }
                    };

                    attributes.no_std = true;

                    Node {
                        span,
                        kind: NodeKind::Empty,
                    }
                })
            },
        ),
    );
}
