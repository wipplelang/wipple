use crate::{
    analysis::expand::{
        Expander, LanguageItem, Node, NodeKind, Operator, OperatorPrecedence, Scope, ScopeValue,
        Template, TemplateDeclaration,
    },
    diagnostics::*,
    helpers::InternedString,
    parse::Span,
    FilePath,
};
use std::{
    collections::{HashMap, VecDeque},
    str::FromStr,
};

pub(super) fn load_builtins(expander: &mut Expander, file: FilePath, scope: &Scope) {
    let mut scope_values = scope.values.lock();
    let mut declarations = expander.declarations.lock();

    // `:` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::Assignment,
        template: id,
    };

    scope_values.insert(InternedString::new(":"), ScopeValue::Operator(operator));

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            ":",
            Span::builtin("`:` operator"),
            Template::function(|expander, span, mut inputs, _, _, scope| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    match rhs.kind {
                        NodeKind::Template(inputs, body) => {
                            let name = match lhs.kind {
                                NodeKind::Name(name) => name,
                                _ => {
                                    expander.compiler.add_error(
                                        "template declaration must be assigned to a name",
                                        vec![Note::primary(lhs.span, "try providing a name here")],
                                    );

                                    return Node {
                                        span,
                                        kind: NodeKind::error(expander.compiler),
                                    };
                                }
                            };

                            let id = expander.compiler.new_template_id_in(span.path);

                            expander.declarations.lock().templates.insert(
                                id,
                                TemplateDeclaration::new(
                                    name,
                                    lhs.span,
                                    Template::syntax(inputs, *body),
                                ),
                            );

                            scope.values.lock().insert(name, ScopeValue::Template(id));

                            Node {
                                span,
                                kind: NodeKind::TemplateDeclaration(id),
                            }
                        }
                        NodeKind::Operator(precedence, inputs, body) => {
                            let name = match lhs.kind {
                                NodeKind::Name(name) => name,
                                _ => {
                                    expander.compiler.add_error(
                                        "operator declaration must be assigned to a name",
                                        vec![Note::primary(lhs.span, "try providing a name here")],
                                    );

                                    return Node {
                                        span,
                                        kind: NodeKind::error(expander.compiler),
                                    };
                                }
                            };

                            let id = expander.compiler.new_template_id_in(span.path);

                            expander.declarations.lock().templates.insert(
                                id,
                                TemplateDeclaration::new(
                                    name,
                                    lhs.span,
                                    Template::syntax(inputs, *body),
                                ),
                            );

                            expander.declarations.lock().operators.insert(
                                id,
                                Operator {
                                    precedence,
                                    template: id,
                                },
                            );

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
                        NodeKind::UseFile(path) => {
                            // TODO: Merge this logic with destructuring
                            let mut imports = HashMap::new();

                            let mut insert_import = |name, span| {
                                if imports.contains_key(&name) {
                                    expander.compiler.add_error(
                                        "duplicate import",
                                        vec![Note::primary(span, "this name is already imported")],
                                    );

                                    return;
                                }

                                imports.insert(name, span);
                            };

                            let report_invalid_import = |span| {
                                expander.compiler.add_error(
                                    "only names may be specified in a destructuring pattern",
                                    vec![Note::primary(span, "expected name here")],
                                );
                            };

                            match lhs.kind {
                                NodeKind::Block(statements) => {
                                    for statement in statements {
                                        match statement.node.kind {
                                            NodeKind::Name(name) => {
                                                insert_import(name, statement.node.span)
                                            }
                                            NodeKind::List(nodes) => {
                                                for node in nodes {
                                                    match node.kind {
                                                        NodeKind::Name(name) => {
                                                            insert_import(name, node.span)
                                                        }
                                                        NodeKind::Error(_) => {}
                                                        _ => report_invalid_import(node.span),
                                                    }
                                                }
                                            }
                                            NodeKind::Error(_) => {}
                                            _ => report_invalid_import(statement.node.span),
                                        }
                                    }
                                }
                                NodeKind::Error(_) => {}
                                _ => {
                                    todo!();
                                }
                            };

                            if let Some(path) = path {
                                expander.dependencies.lock().get_mut(&path).unwrap().1 =
                                    Some(imports);
                            }

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
            }),
        ),
    );

    // `->` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::Function,
        template: id,
    };

    scope_values.insert(InternedString::new("->"), ScopeValue::Operator(operator));

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "->",
            Span::builtin("`->` operator"),
            Template::function(|_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Function(Box::new(lhs), Box::new(rhs)),
                    }
                })
            }),
        ),
    );

    // `=>` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::TypeFunction,
        template: id,
    };

    scope_values.insert(InternedString::new("=>"), ScopeValue::Operator(operator));

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "=>",
            Span::builtin("`=>` operator"),
            Template::function(|_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::TypeFunction(Box::new(lhs), Box::new(rhs)),
                    }
                })
            }),
        ),
    );

    // `where` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::Where,
        template: id,
    };

    scope_values.insert(InternedString::new("where"), ScopeValue::Operator(operator));

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "where",
            Span::builtin("`where` operator"),
            Template::function(|_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Where(Box::new(lhs), Box::new(rhs)),
                    }
                })
            }),
        ),
    );

    // `::` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::Annotation,
        template: id,
    };

    scope_values.insert(InternedString::new("::"), ScopeValue::Operator(operator));

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "::",
            Span::builtin("`::` operator"),
            Template::function(|_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Annotate(Box::new(lhs), Box::new(rhs)),
                    }
                })
            }),
        ),
    );

    // `~>` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::Function,
        template: id,
    };

    scope_values.insert(InternedString::new("~>"), ScopeValue::Operator(operator));

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "~>",
            Span::builtin("`~>` operator"),
            Template::function(|expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    let inputs = match lhs.kind {
                        NodeKind::Error(trace) => {
                            return Node {
                                span,
                                kind: NodeKind::Error(trace),
                            }
                        }
                        NodeKind::Empty => Vec::new(),
                        NodeKind::Name(name) => vec![name],
                        NodeKind::List(names) => names
                            .into_iter()
                            .filter_map(|node| match node.kind {
                                NodeKind::Error(_) => None,
                                NodeKind::Name(name) => Some(name),
                                _ => {
                                    expander.compiler.add_error(
                                        "expected name of template input",
                                        vec![Note::primary(node.span, "expected name here")],
                                    );

                                    None
                                }
                            })
                            .collect(),
                        _ => {
                            expander.compiler.add_error(
                                "expected template inputs",
                                vec![Note::primary(span, "try providing some names")],
                            );

                            return Node {
                                span,
                                kind: NodeKind::error(expander.compiler),
                            };
                        }
                    };

                    Node {
                        span,
                        kind: NodeKind::Template(inputs, Box::new(rhs)),
                    }
                })
            }),
        ),
    );

    // `operator` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::Function,
        template: id,
    };

    scope_values.insert(
        InternedString::new("operator"),
        ScopeValue::Operator(operator),
    );

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "operator",
            Span::builtin("`operator` operator"),
            Template::function(|expander, span, mut inputs, _, _, _| Box::pin(async move {
                let rhs = inputs.pop().unwrap();
                let lhs = inputs.pop().unwrap();

                let precedence = match lhs.kind {
                    NodeKind::Name(name) => match name.as_str() {
                        "cast" => OperatorPrecedence::Cast,
                        "power" => OperatorPrecedence::Power,
                        "multiplication" => OperatorPrecedence::Multiplication,
                        "addition" => OperatorPrecedence::Addition,
                        "comparison" => OperatorPrecedence::Comparison,
                        "conjunction" => OperatorPrecedence::Conjunction,
                        "disjunction" => OperatorPrecedence::Disjunction,
                        "accessor" => OperatorPrecedence::Accessor,
                        "dot" => OperatorPrecedence::Dot,
                        "comma" => OperatorPrecedence::Comma,
                        _ => {
                            expander.compiler.add_error(
                                "invalid precedence name", vec![Note::primary(
                                    lhs.span,
                                    "try providing a valid precedence name, like `addition` or `comparison`",
                                )],
                            );

                            return Node {
                                span,
                                kind: NodeKind::error(expander.compiler),
                            };
                        }
                    }
                    _ => {
                        expander.compiler.add_error(
                            "expected precedence on left-hand side of operator declaration", vec![Note::primary(lhs.span, "expected a valid precedence name here, like `function` or `addition`")],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }
                };

                let (inputs, body) = match rhs.kind {
                    NodeKind::Template(inputs, body) => (inputs, body),
                    _ => {
                        expander.compiler.add_error(
                            "expected template on right-hand side of operator declaration", vec![Note::primary(rhs.span, "expected a template here")],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }
                };

                Node {
                    span,
                    kind: NodeKind::Operator(precedence, inputs, body),
                }
            })),
        ),
    );

    // `or` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::Disjunction,
        template: id,
    };

    scope_values.insert(InternedString::new("or"), ScopeValue::Operator(operator));

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "or",
            Span::builtin("`or` operator"),
            Template::function(|_, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    let rhs = inputs.pop().unwrap();
                    let lhs = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::Or(Box::new(lhs), Box::new(rhs)),
                    }
                })
            }),
        ),
    );

    // `,` operator

    let id = expander.compiler.new_template_id_in(file);

    let operator = Operator {
        precedence: OperatorPrecedence::Comma,
        template: id,
    };

    scope_values.insert(InternedString::new(","), ScopeValue::Operator(operator));

    declarations.operators.insert(id, operator);

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            ",",
            Span::builtin("`,` operator"),
            Template::function(|_, span, inputs, _, _, _| {
                Box::pin(async move {
                    Node {
                        span,
                        kind: NodeKind::Tuple(inputs),
                    }
                })
            }),
        ),
    );

    // `use` template

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("use"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::keyword(
            "use",
            Span::builtin("`use` template"),
            Template::function(|expander, span, mut inputs, _, _, scope| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.report_wrong_template_arity("use", span, inputs.len(), 1);

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let input = inputs.pop().unwrap();

                    match input.kind {
                        NodeKind::Text(path) => {
                            let mut resolved_path = None;
                            if let Some(file) =
                                (expander.load)(expander.compiler, span, FilePath::Path(path)).await
                            {
                                resolved_path = Some(file.path);
                                expander.add_dependency(file, scope);
                            }

                            Node {
                                span,
                                kind: NodeKind::UseFile(resolved_path),
                            }
                        }
                        _ => Node {
                            span,
                            kind: NodeKind::UseExpr(Box::new(input)),
                        },
                    }
                })
            }),
        ),
    );

    // `external` template

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("external"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::keyword(
            "external",
            Span::builtin("`external` template"),
            Template::function(|expander, span, inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() < 2 {
                        expander.compiler.add_error(
                            "expected at least 2 inputs to template `external`",
                            vec![Note::primary(
                                span,
                                "`external` requires an ABI and identifier",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let mut inputs = VecDeque::from(inputs);

                    let lib = inputs.pop_front().unwrap();
                    let identifier = inputs.pop_front().unwrap();
                    let inputs = Vec::from(inputs);

                    Node {
                        span,
                        kind: NodeKind::External(Box::new(lib), Box::new(identifier), inputs),
                    }
                })
            }),
        ),
    );

    // `type` template

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("type"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::keyword(
            "type",
            Span::builtin("`type` template"),
            Template::function(|expander, span, mut inputs, _, _, _| Box::pin(async move {
                if inputs.is_empty() {
                    return Node { span, kind: NodeKind::Type(None) }
                }

                if inputs.len() != 1 {
                    expander.compiler.add_error(
                        "expected 0 or 1 inputs to template `type`", vec![Note::primary(
                            span,
                            "`type` may be used standalone, or with a block denoting the type's fields or variants",
                        )],
                    );

                    return Node {
                        span,
                        kind: NodeKind::error(expander.compiler),
                    };
                }

                let block = inputs.pop().unwrap();

                let fields = match block.kind {
                    NodeKind::Block(statements) => statements,
                    _ => {
                        expander.compiler.add_error(
                            "expected a block here", vec![Note::primary(
                                block.span,
                                "`when` requires a block containing functions",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }
                };

                Node {
                    span,
                    kind: NodeKind::Type(Some(fields)),
                }
            })),
        ),
    );

    // `trait` template

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("trait"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::keyword(
            "trait",
            Span::builtin("`trait` template"),
            Template::function(|expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() > 1 {
                        expander.compiler.add_error(
                            "expected 0 or 1 inputs to template `trait`",
                            vec![Note::primary(
                                span,
                                "`trait` may be used standalone or with a type",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let ty = inputs.pop();

                    Node {
                        span,
                        kind: NodeKind::Trait(ty.map(Box::new)),
                    }
                })
            }),
        ),
    );

    // `instance` template

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("instance"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::keyword(
            "instance",
            Span::builtin("`instance` template"),
            Template::function(|expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.add_error(
                            "expected 1 input to template `instance`",
                            vec![Note::primary(span, "`instance` requires a trait")],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let input = inputs.pop().unwrap();

                    let (trait_name, parameters) = match input.kind {
                        NodeKind::Name(_) => (input, Vec::new()),
                        NodeKind::List(inputs) if inputs.len() > 1 => {
                            let mut inputs = VecDeque::from(inputs);
                            (inputs.pop_front().unwrap(), Vec::from(inputs))
                        }
                        _ => {
                            expander.compiler.add_error(
                                "malformed `instance` declaration",
                                vec![Note::primary(span, "`instance` requires a trait")],
                            );

                            return Node {
                                span,
                                kind: NodeKind::error(expander.compiler),
                            };
                        }
                    };

                    Node {
                        span,
                        kind: NodeKind::Instance(Box::new(trait_name), parameters),
                    }
                })
            }),
        ),
    );

    // `format` template

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("format"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "format",
            Span::builtin("`format` template"),
            Template::function(|expander, span, inputs, _, _, _| Box::pin(async move {
                if inputs.is_empty() {
                    expander.compiler.add_error(
                        "expected at least 1 input to template `format`", vec![Note::primary(
                            span,
                            "`instance` requires text containing `_` placeholders",
                        )],
                    );

                    return Node {
                        span,
                        kind: NodeKind::error(expander.compiler),
                    };
                }

                let mut inputs = VecDeque::from(inputs);

                let format_text = inputs.pop_front().unwrap();

                if let NodeKind::Text(text) = format_text.kind {
                    let placeholder_count = text.split('_').count() - 1;

                    if placeholder_count != inputs.len() {
                        expander.compiler.add_error(
                            "wrong number of inputs to `format` text", vec![Note::primary(
                                span,
                                format!(
                                    "text contains {} placeholders, but {} inputs were provided",
                                    placeholder_count,
                                    inputs.len()
                                ),
                            )],
                        );
                    }
                } else {
                    expander.compiler.add_error(
                        "expected text", vec![Note::primary(
                            span,
                            "`instance` requires text containing `_` placeholders",
                        )],
                    );

                    return Node {
                        span,
                        kind: NodeKind::error(expander.compiler),
                    };
                };

                Node {
                    span,
                    kind: NodeKind::Annotate(
                        Box::new(Node {
                            span,
                            kind: NodeKind::External(
                                Box::new(Node {
                                    span,
                                    kind: NodeKind::Text(InternedString::new("runtime")),
                                }),
                                Box::new(Node {
                                    span,
                                    kind: NodeKind::Text(InternedString::new("format")),
                                }),
                                vec![
                                    format_text,
                                    Node {
                                        span,
                                        kind: NodeKind::Annotate(
                                            Box::new(Node {
                                                span,
                                                kind: NodeKind::List(vec![
                                                    Node {
                                                        span,
                                                        kind: NodeKind::Name(InternedString::new("list")),
                                                    },
                                                    Node {
                                                        span,
                                                        kind: NodeKind::Tuple(
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
                                                    }
                                                ]),
                                            }),
                                            Box::new(Node {
                                                span,
                                                kind: NodeKind::List(vec![
                                                    Node {
                                                        span,
                                                        kind: NodeKind::Name(InternedString::new("List")),
                                                    },
                                                    Node {
                                                        span,
                                                        kind: NodeKind::Name(InternedString::new("Text")),
                                                    },
                                                ]),
                                            }),
                                        ),
                                    }
                                ],
                            ),
                        }),
                        Box::new(Node {
                            span,
                            kind: NodeKind::Name(InternedString::new("Text")),
                        }),
                    ),
                }
            })),
        ),
    );

    // `end` template

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("end"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "end",
            Span::builtin("`end` template"),
            Template::function(|expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.add_error(
                            "expected 1 input to template `end`",
                            vec![Note::primary(
                                span,
                                "`end` accepts a value to exit a block with",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let value = inputs.pop().unwrap();

                    Node {
                        span,
                        kind: NodeKind::End(Box::new(value)),
                    }
                })
            }),
        ),
    );

    // `when` template

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("when"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::keyword(
            "when",
            Span::builtin("`when` template"),
            Template::function(|expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 2 {
                        expander.compiler.add_error(
                            "expected 2 inputs to template `when`", vec![Note::primary(
                                span,
                                "`when` accepts a value to match and a block containing functions",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let block = inputs.pop().unwrap();
                    let input = inputs.pop().unwrap();

                    let block = match block.kind {
                        NodeKind::Block(statements) => statements,
                        _ => {
                            expander.compiler.add_error(
                                "expected a block here", vec![Note::primary(
                                    block.span,
                                    "`type` requires a block denoting the type's fields or variants",
                                )],
                            );

                            return Node {
                                span,
                                kind: NodeKind::error(expander.compiler),
                            };
                        }
                    };

                    Node {
                        span,
                        kind: NodeKind::When(Box::new(input), block),
                    }
                })
            }),
        ),
    );

    // `language` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("language"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "language",
            Span::builtin("`language` attribute"),
            Template::function(|expander, span, mut inputs, _, attributes, _| {
                Box::pin(async move {
                    if inputs.len() != 2 {
                        expander.compiler.add_error(
                            "expected 2 inputs to template `language`",
                            vec![Note::primary(
                                span,
                                "`language` accepts the name of a language item and a declaration",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let node = inputs.pop().unwrap();
                    let item = inputs.pop().unwrap();

                    let name = match item.kind {
                        NodeKind::Text(text) => text,
                        _ => {
                            expander.compiler.add_error(
                                "`language` expects a text value",
                                vec![Note::primary(item.span, "expected text here")],
                            );

                            return node;
                        }
                    };

                    let language_item = match LanguageItem::from_str(&name) {
                        Ok(item) => item,
                        Err(_) => {
                            expander.compiler.add_error(
                                "invalid `language` item",
                                vec![Note::primary(
                                    item.span,
                                    "expected a valid `language` item here",
                                )],
                            );

                            return node;
                        }
                    };

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.add_error(
                                "`language` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    format!(
                                        r#"try putting this between brackets: (`[language "{}"]`)"#,
                                        name
                                    ),
                                )],
                            );

                            return node;
                        }
                    };

                    if attributes.language_item.is_some() {
                        expander.compiler.add_error(
                            "`language` item is already set for this statement",
                            vec![Note::primary(
                                span,
                                "cannot use more than one `language` item per statement",
                            )],
                        );

                        return node;
                    }

                    attributes.language_item = Some(language_item);

                    node
                })
            }),
        ),
    );

    // `help` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("help"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "help",
            Span::builtin("`help` attribute"),
            Template::function(|expander, span, mut inputs, _, attributes, _| {
                Box::pin(async move {
                    if inputs.len() != 2 {
                        expander.compiler.add_error(
                            "expected 2 inputs to template `help`",
                            vec![Note::primary(
                                span,
                                "`help` accepts documentation text and a declaration",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let node = inputs.pop().unwrap();
                    let item = inputs.pop().unwrap();

                    let doc = match item.kind {
                        NodeKind::Text(text) => text,
                        _ => {
                            expander.compiler.add_error(
                                "`help` expects a text value",
                                vec![Note::primary(item.span, "expected text here")],
                            );

                            return node;
                        }
                    };

                    match attributes {
                        Some(attributes) => {
                            attributes.help.push_front(doc);
                        }
                        None => {
                            if let NodeKind::TemplateDeclaration(id) = item.kind {
                                expander
                                    .declarations
                                    .lock()
                                    .templates
                                    .get_mut(&id)
                                    .unwrap()
                                    .attributes
                                    .help
                                    .push_front(doc);
                            } else {
                                expander.compiler.add_error(
                                    "`help` may only be used as an attribute",
                                    vec![Note::primary(
                                        span,
                                        r#"try putting this between brackets: (`[help "..."]`)"#,
                                    )],
                                );
                            }
                        }
                    }

                    node
                })
            }),
        ),
    );

    // `on-unimplemented` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(
        InternedString::new("on-unimplemented"),
        ScopeValue::Template(id),
    );

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "on-unimplemented",
            Span::builtin("`on-unimplemented` attribute"),
            Template::function(|expander, span, mut inputs, _, attributes, _| {
                Box::pin(async move {
                    if inputs.len() != 2 {
                        expander.compiler.add_error(
                            "expected 2 inputs to template `on-unimplemented`", vec![Note::primary(
                                span,
                                "`on-unimplemented` accepts a message and a declaration",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let node = inputs.pop().unwrap();
                    let item = inputs.pop().unwrap();

                    let message = match item.kind {
                        NodeKind::Text(text) => text,
                        _ => {
                            expander.compiler.add_error(
                                "`on-unimplemented` expects a text value", vec![Note::primary(item.span, "expected text here")],
                            );

                            return node;
                        }
                    };

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.add_error(
                                "`on-unimplemented` may only be used as an attribute", vec![Note::primary(
                                    span,
                                    r#"try putting this between brackets: (`[on-unimplemented "..."]`)"#,
                                )],
                            );

                            return node;
                        }
                    };

                    if attributes.on_unimplemented.is_some() {
                        expander.compiler.add_error(
                            "`on-unimplemented` item is already set for this statement", vec![Note::primary(
                                span,
                                "cannot use more than one `on-unimplemented` item per statement",
                            )],
                        );

                        return node;
                    }

                    attributes.on_unimplemented = Some(message);

                    node
                })
            },
        ))
    );

    // `on-mismatch` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("on-mismatch"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "on-mismatch",
            Span::builtin("`on-mismatch` attribute"),
            Template::function(|expander, span, mut inputs, _, attributes, _| {
                Box::pin(async move {
                    if !matches!(inputs.len(), 2..=3) {
                        expander.compiler.add_error(
                            "expected 2-3 inputs to template `on-mismatch`",
                            vec![Note::primary(
                                span,
                                "`on-mismatch` accepts a type parameter, message and a declaration",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let node = inputs.pop().unwrap();
                    let (type_parameter, message) = match inputs.len() {
                        1 => (None, inputs.pop().unwrap()),
                        2 => {
                            let message = inputs.pop().unwrap();
                            let type_parameter = inputs.pop().unwrap();
                            (Some(type_parameter), message)
                        }
                        _ => unreachable!(),
                    };

                    let type_parameter = match type_parameter {
                        Some(node) => match node.kind {
                            NodeKind::Name(name) => Some((node.span, name)),
                            _ => {
                                expander.compiler.add_error(
                                    "`on-mismatch` expects a type parameter name",
                                    vec![Note::primary(
                                        message.span,
                                        "expected type parameter here",
                                    )],
                                );

                                return node;
                            }
                        },
                        None => None,
                    };

                    let message = match message.kind {
                        NodeKind::Text(text) => text,
                        _ => {
                            expander.compiler.add_error(
                                "`on-mismatch` expects a text value",
                                vec![Note::primary(message.span, "expected text here")],
                            );

                            return node;
                        }
                    };

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.add_error(
                                "`on-mismatch` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    r#"try putting this between brackets: (`[on-mismatch "..."]`)"#,
                                )],
                            );

                            return node;
                        }
                    };

                    attributes.on_mismatch.push_front((type_parameter, message));

                    node
                })
            }),
        ),
    );

    // `no-std` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("no-std"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "no-std",
            Span::builtin("`no-std` attribute"),
            Template::function(|expander, span, inputs, attributes, _, _| {
                Box::pin(async move {
                    if !inputs.is_empty() {
                        expander.compiler.add_error(
                            "`no-std` attribute does not accept inputs",
                            vec![Note::primary(
                                inputs
                                    .first()
                                    .unwrap()
                                    .span
                                    .with_end(inputs.last().unwrap().span.end),
                                "try removing these",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.add_error(
                                "`no-std` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    r#"try putting this between double brackets: (`[[no-std]]`)"#,
                                )],
                            );

                            return Node {
                                span,
                                kind: NodeKind::error(expander.compiler),
                            };
                        }
                    };

                    attributes.no_std = true;

                    Node {
                        span,
                        kind: NodeKind::Empty,
                    }
                })
            }),
        ),
    );

    // `recursion-limit` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(
        InternedString::new("recursion-limit"),
        ScopeValue::Template(id),
    );

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "recursion-limit",
            Span::builtin("`recursion-limit` attribute"),
            Template::function(|expander, span, mut inputs, attributes, _, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.add_error(
                            "`recursion-limit` attribute accepts 1 input",
                            vec![Note::primary(
                                inputs
                                    .first()
                                    .unwrap()
                                    .span
                                    .with_end(inputs.last().unwrap().span.end),
                                "`recursion-limit` accepts an integer",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let node = inputs.pop().unwrap();

                    let limit = match (|| {
                        match node.kind {
                            NodeKind::Number(n) => {
                                if let Ok(n) = n.parse::<usize>() {
                                    return Some(n);
                                }
                            }
                            _ => {}
                        }

                        expander.compiler.add_error(
                            "`recursion-limit` expects a positive integer",
                            vec![Note::primary(
                                node.span,
                                "this is not an integer",
                            )],
                        );

                        None
                    })() {
                        Some(n) => n,
                        None => return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        }
                    };


                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.add_error(
                                "`recursion-limit` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    r#"try putting this between double brackets: (`[[recursion-limit]]`)"#,
                                )],
                            );

                            return Node {
                                span,
                                kind: NodeKind::error(expander.compiler),
                            };
                        }
                    };

                    attributes.recursion_limit = Some(limit);

                    Node {
                        span,
                        kind: NodeKind::Empty,
                    }
                })
            }),
        ),
    );

    // `keyword` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("keyword"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "keyword",
            Span::builtin("`keyword` attribute"),
            Template::function(|expander, span, mut inputs, _, _, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.add_error(
                            "expected 1 input to template `keyword`",
                            vec![Note::primary(
                                span,
                                "`keyword` accepts a template declaration",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let node = inputs.pop().unwrap();

                    let template = match node.kind {
                        NodeKind::TemplateDeclaration(id) => id,
                        _ => {
                            expander.compiler.add_error(
                                "`keyword` may only be used on a template declaration",
                                vec![Note::primary(span, "expected a template declaration here")],
                            );

                            return Node {
                                span,
                                kind: NodeKind::error(expander.compiler),
                            };
                        }
                    };

                    let mut declarations = expander.declarations.lock();
                    let decl = declarations.templates.get_mut(&template).unwrap();

                    if decl.attributes.keyword {
                        expander.compiler.add_error(
                            "cannot apply `keyword` attribute multiple times",
                            vec![Note::primary(span, "try removing this")],
                        );

                        return node;
                    }

                    decl.attributes.keyword = true;

                    node
                })
            }),
        ),
    );

    // `specialize` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(InternedString::new("specialize"), ScopeValue::Template(id));

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "specialize",
            Span::builtin("`specialize` attribute"),
            Template::function(|expander, span, mut inputs, _, attributes, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.add_error(
                            "expected 1 input to template `keyword`",
                            vec![Note::primary(
                                span,
                                "`keyword` accepts a constant declaration",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let node = inputs.pop().unwrap();

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.add_error(
                                "`specialize` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    r#"try putting this between brackets: (`[specialize]`)"#,
                                )],
                            );

                            return node;
                        }
                    };

                    if attributes.specialize {
                        expander.compiler.add_error(
                            "`specialize` attribute is already set for this statement",
                            vec![Note::primary(
                                span,
                                "cannot use more than one `specialize` attribute per statement",
                            )],
                        );

                        return node;
                    }

                    attributes.specialize = true;

                    node
                })
            }),
        ),
    );

    // `allow-overlapping-instances` attribute

    let id = expander.compiler.new_template_id_in(file);

    scope_values.insert(
        InternedString::new("allow-overlapping-instances"),
        ScopeValue::Template(id),
    );

    declarations.templates.insert(
        id,
        TemplateDeclaration::new(
            "allow-overlapping-instances",
            Span::builtin("`allow-overlapping-instances` attribute"),
            Template::function(|expander, span, mut inputs, _, attributes, _| {
                Box::pin(async move {
                    if inputs.len() != 1 {
                        expander.compiler.add_error(
                            "expected 1 input to template `allow-overlapping-instances`",
                            vec![Note::primary(
                                span,
                                "`allow-overlapping-instances` accepts a trait declaration",
                            )],
                        );

                        return Node {
                            span,
                            kind: NodeKind::error(expander.compiler),
                        };
                    }

                    let node = inputs.pop().unwrap();

                    let attributes = match attributes {
                        Some(attributes) => attributes,
                        None => {
                            expander.compiler.add_error(
                                "`allow-overlapping-instances` may only be used as an attribute",
                                vec![Note::primary(
                                    span,
                                    r#"try putting this between brackets: (`[allow-overlapping-instances]`)"#,
                                )],
                            );

                            return node;
                        }
                    };

                    if attributes.specialize {
                        expander.compiler.add_error(
                            "`allow-overlapping-instances` attribute is already set for this statement",
                            vec![Note::primary(
                                span,
                                "cannot use more than one `allow-overlapping-instances` attribute per statement",
                            )],
                        );

                        return node;
                    }

                    attributes.allow_overlapping_instances = true;

                    node
                })
            }),
        ),
    );
}
