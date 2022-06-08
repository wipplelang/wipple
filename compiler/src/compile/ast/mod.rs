use crate::{
    compile::expand::{self, Node, NodeKind},
    diagnostics::*,
    helpers::InternedString,
    parse::Span,
    Compiler, FilePath,
};
use rust_decimal::Decimal;

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FileAttribute {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Type((Span, InternedString), TypeDeclaration),
    Trait((Span, InternedString), TraitDeclaration),
    Constant((Span, InternedString), ConstantDeclaration),
    Instance(Instance),
    Assign(Pattern, Expression),
    Use((Span, InternedString)),
    Expression(ExpressionKind),
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_parameters: Vec<TypeAnnotation>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Error,
    Unit,
    Name(InternedString),
    Number(Decimal),
    Text(InternedString),
    Block(Vec<Statement>),
    Call(Box<Expression>, Vec<Expression>),
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
    ListLiteral(Vec<Expression>),
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub span: Span,
    pub kind: TypeAnnotationKind,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotationKind {
    Error,
    Placeholder,
    Unit,
    Named(InternedString, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Error,
    Wildcard,
    Unit,
    Name(InternedString),
    Destructure(Vec<(Span, InternedString)>),
    Variant((Span, InternedString), Vec<Pattern>),
    Annotate(Box<Pattern>, TypeAnnotation),
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Marker,
    Structure(Vec<DataField>),
    Enumeration(Vec<DataVariant>),
}

#[derive(Debug, Clone)]
pub struct DataField {
    pub name: InternedString,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct DataVariant {
    pub name: InternedString,
    pub span: Span,
    pub values: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub parameters: Vec<TypeAnnotation>,
}

impl<L> Compiler<L> {
    pub fn build_ast(&mut self, file: expand::File) -> File {
        File {
            path: file.path,
            span: file.span,
            statements: file
                .statements
                .into_iter()
                .map(|node| self.build_statement(node))
                .collect(),
        }
    }

    fn build_statement(&mut self, node: Node) -> Statement {
        Statement {
            span: node.span,
            kind: (|| match node.kind {
                NodeKind::Assign(pattern, value) => match pattern.kind {
                    NodeKind::TypeFunction(lhs, rhs) => {
                        let (params, bounds) = match self.build_type_function(*lhs) {
                            Some((params, bounds)) => (params, bounds),
                            None => return StatementKind::Expression(ExpressionKind::Error),
                        };

                        match rhs.kind {
                            NodeKind::Instance(trait_name, trait_parameters) => {
                                match self.build_instance(
                                    params,
                                    bounds,
                                    *trait_name,
                                    trait_parameters,
                                    *value,
                                ) {
                                    Some(instance) => StatementKind::Instance(instance),
                                    None => StatementKind::Expression(ExpressionKind::Error),
                                }
                            }
                            _ => {
                                self.diagnostics.add(Diagnostic::error(
                                    "expected instance here",
                                    vec![Note::primary(
                                        rhs.span,
                                        "try adding `instance` to the left-hand side of `:`",
                                    )],
                                ));

                                StatementKind::Expression(ExpressionKind::Error)
                            }
                        }
                    }
                    NodeKind::Instance(trait_name, trait_parameters) => {
                        match self.build_instance(
                            Vec::new(),
                            Vec::new(),
                            *trait_name,
                            trait_parameters,
                            *value,
                        ) {
                            Some(instance) => StatementKind::Instance(instance),
                            None => StatementKind::Expression(ExpressionKind::Error),
                        }
                    }
                    _ => {
                        let pattern = self.build_pattern(*pattern);

                        match value.kind {
                            NodeKind::Assign(_, _) => {
                                self.diagnostics.add(Diagnostic::error(
                                    "expected expression, found assignment",
                                    vec![Note::primary(
                                        value.span,
                                        "try moving this variable assignment onto its own line",
                                    )],
                                ));

                                StatementKind::Expression(ExpressionKind::Error)
                            }
                            NodeKind::Template(_, _) => unreachable!("unexpanded template"),
                            NodeKind::Type(fields) => {
                                let name = match pattern.kind {
                                    PatternKind::Name(name) => name,
                                    _ => {
                                        self.diagnostics.add(Diagnostic::error(
                                            "type declaration must be assigned to a name",
                                            vec![Note::primary(
                                                value.span,
                                                "try providing a name here",
                                            )],
                                        ));

                                        return StatementKind::Expression(ExpressionKind::Error);
                                    }
                                };

                                match self.build_type_declaration(value.span, fields) {
                                    Some(kind) => StatementKind::Type(
                                        (pattern.span, name),
                                        TypeDeclaration {
                                            parameters: Vec::new(),
                                            bounds: Vec::new(),
                                            kind,
                                        },
                                    ),
                                    None => StatementKind::Expression(ExpressionKind::Error),
                                }
                            }
                            NodeKind::Trait(ty) => {
                                let name = match pattern.kind {
                                    PatternKind::Name(name) => name,
                                    _ => {
                                        self.diagnostics.add(Diagnostic::error(
                                            "trait declaration must be assigned to a name",
                                            vec![Note::primary(
                                                value.span,
                                                "try providing a name here",
                                            )],
                                        ));

                                        return StatementKind::Expression(ExpressionKind::Error);
                                    }
                                };

                                StatementKind::Trait(
                                    (pattern.span, name),
                                    TraitDeclaration {
                                        parameters: Vec::new(),
                                        bounds: Vec::new(),
                                        ty: self.build_type_annotation(*ty),
                                    },
                                )
                            }
                            NodeKind::TypeFunction(parameters, node) => {
                                let name = match pattern.kind {
                                    PatternKind::Name(name) => name,
                                    _ => {
                                        self.diagnostics.add(Diagnostic::error(
                                            "type or trait declaration must be assigned to a name",
                                            vec![Note::primary(
                                                value.span,
                                                "try providing a name here",
                                            )],
                                        ));

                                        return StatementKind::Expression(ExpressionKind::Error);
                                    }
                                };

                                let (parameters, bounds) =
                                    match self.build_type_function(*parameters) {
                                        Some((parameters, bounds)) => (parameters, bounds),
                                        None => {
                                            return StatementKind::Expression(ExpressionKind::Error)
                                        }
                                    };

                                match node.kind {
                                    NodeKind::Type(fields) => {
                                        match self.build_type_declaration(node.span, fields) {
                                            Some(kind) => StatementKind::Type(
                                                (pattern.span, name),
                                                TypeDeclaration {
                                                    parameters,
                                                    bounds,
                                                    kind,
                                                },
                                            ),
                                            None => {
                                                StatementKind::Expression(ExpressionKind::Error)
                                            }
                                        }
                                    }
                                    NodeKind::Trait(ty) => StatementKind::Trait(
                                        (pattern.span, name),
                                        TraitDeclaration {
                                            parameters,
                                            bounds,
                                            ty: self.build_type_annotation(*ty),
                                        },
                                    ),
                                    _ => {
                                        self.diagnostics.add(Diagnostic::error(
                                            "expected type or trait declaration in type function",
                                            vec![Note::primary(
                                                node.span,
                                                "only types and traits may have type parameters",
                                            )],
                                        ));

                                        StatementKind::Expression(ExpressionKind::Error)
                                    }
                                }
                            }
                            _ => StatementKind::Assign(pattern, self.build_expression(*value)),
                        }
                    }
                },
                NodeKind::Template(_, _) => unreachable!(),
                NodeKind::Annotate(expr, ty) => {
                    let name = match expr.kind {
                        NodeKind::Name(name) => (expr.span, name),
                        _ => {
                            return StatementKind::Expression(
                                self.build_expression(Node {
                                    span: node.span,
                                    kind: NodeKind::Annotate(expr, ty),
                                })
                                .kind,
                            )
                        }
                    };

                    let (parameters, bounds, ty) = match ty.kind {
                        NodeKind::TypeFunction(parameters, ty) => {
                            let (parameters, bounds) = match self.build_type_function(*parameters) {
                                Some((parameters, bounds)) => (parameters, bounds),
                                None => return StatementKind::Expression(ExpressionKind::Error),
                            };

                            (parameters, bounds, self.build_type_annotation(*ty))
                        }
                        _ => (Vec::new(), Vec::new(), self.build_type_annotation(*ty)),
                    };

                    StatementKind::Constant(
                        name,
                        ConstantDeclaration {
                            parameters,
                            bounds,
                            ty,
                        },
                    )
                }
                NodeKind::Use(expr) => {
                    let name = match expr.kind {
                        NodeKind::Name(name) => name,
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "`use` expects a path to a file or a name of a type",
                                vec![Note::primary(expr.span, "expected a path or type here")],
                            ));

                            return StatementKind::Expression(ExpressionKind::Error);
                        }
                    };

                    StatementKind::Use((expr.span, name))
                }
                _ => StatementKind::Expression(self.build_expression(node).kind),
            })(),
        }
    }

    fn build_instance(
        &mut self,
        parameters: Vec<TypeParameter>,
        bounds: Vec<Bound>,
        trait_name: Node,
        trait_parameters: Vec<Node>,
        value: Node,
    ) -> Option<Instance> {
        let trait_span = trait_name.span;
        let trait_name = match trait_name.kind {
            NodeKind::Error => return None,
            NodeKind::Name(name) => name,
            _ => {
                self.diagnostics.add(Diagnostic::error(
                    "expected trait name",
                    vec![Note::primary(
                        trait_name.span,
                        "`instance` requires the name of a trait",
                    )],
                ));

                return None;
            }
        };

        let trait_parameters = trait_parameters
            .into_iter()
            .map(|node| self.build_type_annotation(node))
            .collect();

        let value = self.build_expression(value);

        Some(Instance {
            parameters,
            bounds,
            trait_span,
            trait_name,
            trait_parameters,
            value,
        })
    }

    fn build_expression(&mut self, node: Node) -> Expression {
        Expression {
            span: node.span,
            kind: (|| match node.kind {
                NodeKind::Error => ExpressionKind::Error,
                NodeKind::Empty => ExpressionKind::Unit,
                NodeKind::Name(name) => ExpressionKind::Name(name),
                NodeKind::Text(text) => ExpressionKind::Text(text),
                NodeKind::Number(number) => ExpressionKind::Number(number),
                NodeKind::List(exprs) => {
                    let mut exprs = exprs.into_iter();

                    ExpressionKind::Call(
                        Box::new(self.build_expression(exprs.next().unwrap())),
                        exprs.map(|expr| self.build_expression(expr)).collect(),
                    )
                }
                NodeKind::Block(statements) => ExpressionKind::Block(
                    statements
                        .into_iter()
                        .map(|node| self.build_statement(node))
                        .collect(),
                ),
                NodeKind::Function(input, body) => ExpressionKind::Function(
                    self.build_pattern(*input),
                    Box::new(self.build_expression(*body)),
                ),
                NodeKind::External(namespace, identifier, inputs) => {
                    let namespace = match namespace.kind {
                        NodeKind::Error => return ExpressionKind::Error,
                        NodeKind::Text(text) => text,
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected text here",
                                vec![Note::primary(
                                    namespace.span,
                                    "`external` requires a namespace",
                                )],
                            ));

                            return ExpressionKind::Error;
                        }
                    };

                    let identifier = match identifier.kind {
                        NodeKind::Error => return ExpressionKind::Error,
                        NodeKind::Text(text) => text,
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected text here",
                                vec![Note::primary(
                                    identifier.span,
                                    "`external` requires an identifier",
                                )],
                            ));

                            return ExpressionKind::Error;
                        }
                    };

                    ExpressionKind::External(
                        namespace,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|node| self.build_expression(node))
                            .collect(),
                    )
                }
                NodeKind::Annotate(value, ty) => ExpressionKind::Annotate(
                    Box::new(self.build_expression(*value)),
                    self.build_type_annotation(*ty),
                ),
                NodeKind::ListLiteral(items) => ExpressionKind::ListLiteral(
                    items
                        .into_iter()
                        .map(|node| self.build_expression(node))
                        .collect(),
                ),
                NodeKind::When(input, block) => {
                    let input = self.build_expression(*input);

                    let arms = block
                        .into_iter()
                        .filter_map(|node| {
                            let expr = self.build_expression(node);

                            match expr.kind {
                                ExpressionKind::Function(pattern, body) => Some(Arm {
                                    span: expr.span,
                                    pattern,
                                    body: *body,
                                }),
                                _ => {
                                    self.diagnostics.add(Diagnostic::error(
                                        "expected function",
                                        vec![Note::primary(expr.span, "this is not a function")],
                                    ));

                                    None
                                }
                            }
                        })
                        .collect();

                    ExpressionKind::When(Box::new(input), arms)
                }
                _ => {
                    self.diagnostics.add(Diagnostic::error(
                        "expected expression",
                        vec![Note::primary(node.span, "this is not an expression")],
                    ));

                    ExpressionKind::Error
                }
            })(),
        }
    }

    fn build_pattern(&mut self, node: Node) -> Pattern {
        Pattern {
            span: node.span,
            kind: (|| match node.kind {
                NodeKind::Error => PatternKind::Error,
                NodeKind::Underscore => PatternKind::Wildcard,
                NodeKind::Name(name) => PatternKind::Name(name),
                NodeKind::Block(mut statements) => {
                    // TODO: Support renaming destructured fields
                    if statements.len() > 1 {
                        self.diagnostics.add(Diagnostic::error(
                            "destructuring pattern does not yet support multiple lines",
                            vec![Note::primary(
                                node.span,
                                "try grouping all of these onto a single line",
                            )],
                        ));

                        return PatternKind::Error;
                    }

                    let statement = match statements.pop() {
                        Some(statement) => statement,
                        None => return PatternKind::Destructure(Vec::new()),
                    };

                    match statement.kind {
                        NodeKind::Error => PatternKind::Error,
                        NodeKind::List(nodes) => PatternKind::Destructure(
                            nodes
                                .into_iter()
                                .filter_map(|node| {
                                    let name = match node.kind {
                                        NodeKind::Error => return None,
                                        NodeKind::Name(name) => name,
                                        _ => {
                                            self.diagnostics.add(Diagnostic::error(
                                                "invalid pattern in destructuring pattern",
                                                vec![Note::primary(
                                                    node.span,
                                                    "expected name here",
                                                )],
                                            ));

                                            return None;
                                        }
                                    };

                                    Some((node.span, name))
                                })
                                .collect(),
                        ),
                        NodeKind::Name(name) => {
                            PatternKind::Destructure(vec![(statement.span, name)])
                        }
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "invalid pattern in destructuring pattern",
                                vec![Note::primary(node.span, "try removing this")],
                            ));

                            PatternKind::Error
                        }
                    }
                }
                NodeKind::List(nodes) => {
                    let mut nodes = nodes.into_iter();

                    let name = nodes.next().unwrap();
                    let name_span = name.span;
                    let name = match name.kind {
                        NodeKind::Name(name) => name,
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected variant name",
                                vec![Note::primary(
                                    node.span,
                                    "only variants may be used in this kind of pattern",
                                )],
                            ));

                            return PatternKind::Error;
                        }
                    };

                    let rest = nodes.map(|node| self.build_pattern(node)).collect();

                    PatternKind::Variant((name_span, name), rest)
                }
                NodeKind::Empty => PatternKind::Unit,
                NodeKind::Annotate(node, ty) => {
                    let inner = self.build_pattern(*node);
                    let ty = self.build_type_annotation(*ty);

                    PatternKind::Annotate(Box::new(inner), ty)
                }
                _ => {
                    self.diagnostics.add(Diagnostic::error(
                        "expected pattern",
                        vec![Note::primary(
                            node.span,
                            "values may not appear on the left-hand side of a variable assignment",
                        )],
                    ));

                    PatternKind::Error
                }
            })(),
        }
    }

    fn build_type_annotation(&mut self, node: Node) -> TypeAnnotation {
        TypeAnnotation {
            span: node.span,
            kind: match node.kind {
                NodeKind::Empty => TypeAnnotationKind::Unit,
                NodeKind::Underscore => TypeAnnotationKind::Placeholder,
                NodeKind::Name(name) => TypeAnnotationKind::Named(name, Vec::new()), // TODO: Parameters
                NodeKind::Function(input, output) => TypeAnnotationKind::Function(
                    Box::new(self.build_type_annotation(*input)),
                    Box::new(self.build_type_annotation(*output)),
                ),
                NodeKind::List(nodes) => {
                    let mut nodes = nodes.into_iter();
                    let ty = nodes.next().unwrap();

                    match ty.kind {
                        NodeKind::Name(name) => TypeAnnotationKind::Named(
                            name,
                            nodes.map(|node| self.build_type_annotation(node)).collect(),
                        ),
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected type",
                                vec![Note::primary(ty.span, "this is not a type")],
                            ));

                            TypeAnnotationKind::Error
                        }
                    }
                }
                _ => {
                    self.diagnostics.add(Diagnostic::error(
                        "expected type",
                        vec![Note::primary(node.span, "this is not a type")],
                    ));

                    TypeAnnotationKind::Error
                }
            },
        }
    }

    fn build_type_function(&mut self, lhs: Node) -> Option<(Vec<TypeParameter>, Vec<Bound>)> {
        macro_rules! build_parameter_list {
            ($tys:expr) => {
                $tys.into_iter()
                    .map(|node| match node.kind {
                        NodeKind::Error => None,
                        NodeKind::Name(name) => Some(TypeParameter {
                            span: node.span,
                            name,
                        }),
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected type parameter",
                                vec![Note::primary(node.span, "try removing this")],
                            ));

                            None
                        }
                    })
                    .collect::<Option<Vec<_>>>()
            };
        }

        macro_rules! build_bound {
            ($span:expr, $list:expr) => {
                (|| {
                    let mut list = $list.into_iter();

                    let trait_name = list.next().unwrap();
                    let trait_span = trait_name.span;
                    let trait_name = match trait_name.kind {
                        NodeKind::Error => return None,
                        NodeKind::Name(name) => name,
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected trait name in `where` clause",
                                vec![Note::primary(trait_name.span, "try adding a name here")],
                            ));

                            return None;
                        }
                    };

                    let parameters = list.map(|node| self.build_type_annotation(node)).collect();

                    Some(Bound {
                        span: $span,
                        trait_span,
                        trait_name,
                        parameters,
                    })
                })()
            };
        }

        match lhs.kind {
            NodeKind::Error => None,
            NodeKind::Name(name) => Some((
                vec![TypeParameter {
                    span: lhs.span,
                    name,
                }],
                Vec::new(),
            )),
            NodeKind::List(tys) => Some((build_parameter_list!(tys)?, Vec::new())),
            NodeKind::WhereClause(lhs, bounds) => {
                let tys = match lhs.kind {
                    NodeKind::Error => return None,
                    NodeKind::Name(name) => vec![TypeParameter {
                        span: lhs.span,
                        name,
                    }],
                    NodeKind::List(tys) => build_parameter_list!(tys)?,
                    _ => {
                        self.diagnostics.add(Diagnostic::error(
                            "expected type parameters on left-hand side of `where` clause",
                            vec![Note::primary(
                                lhs.span,
                                "try providing a list of names here",
                            )],
                        ));

                        return None;
                    }
                };

                let bounds_span = bounds.span;

                let bounds = (|| {
                    match bounds.kind {
                        NodeKind::List(bounds) => {
                            if bounds
                                .iter()
                                .any(|bound| matches!(bound.kind, NodeKind::List(_)))
                            {
                                // The bounds clause matches the pattern `(T A) (T B) ...`
                                bounds
                                    .into_iter()
                                    .map(|bound| match bound.kind {
                                        NodeKind::Name(trait_name) => Some(Bound {
                                            span: bound.span,
                                            trait_span: bound.span,
                                            trait_name,
                                            parameters: Vec::new(),
                                        }),
                                        NodeKind::List(list) => build_bound!(bound.span, list),
                                        _ => {
                                            self.diagnostics.add(Diagnostic::error(
                                                "expected bound",
                                                vec![Note::primary(
                                                    bounds_span,
                                                    "`where` bound must be in the format `(T A B ...)`",
                                                )],
                                            ));

                                            None
                                        }
                                    })
                                    .collect::<Option<_>>()
                            } else {
                                // The bounds clause matches the pattern `T A B ...`
                                Some(vec![build_bound!(bounds_span, bounds)?])
                            }
                        }
                        NodeKind::Name(trait_name) => {
                            // The bounds clause matches the pattern `T`
                            Some(vec![Bound {
                                span: bounds_span,
                                trait_span: bounds_span,
                                trait_name,
                                parameters: Vec::new(),
                            }])
                        }
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected bounds",
                                vec![Note::primary(
                                    bounds_span,
                                    "`where` bounds must be in the format `(T A) (T B) ...`",
                                )],
                            ));

                            None
                        }
                    }
                })()?;

                Some((tys, bounds))
            }
            _ => {
                self.diagnostics.add(Diagnostic::error(
                    "expected type parameters",
                    vec![Note::primary(
                        lhs.span,
                        "try providing a list of names and optionally a `where` clause",
                    )],
                ));

                None
            }
        }
    }

    fn build_type_declaration(
        &mut self,
        span: Span,
        fields: Option<Vec<Node>>,
    ) -> Option<TypeKind> {
        let fields = match fields {
            Some(fields) => fields,
            None => return Some(TypeKind::Marker),
        };

        if fields.is_empty() {
            self.diagnostics.add(Diagnostic::error(
                "type must not be empty",
                vec![Note::primary(
                    span,
                    "try adding some fields or variants here",
                )],
            ));

            return None;
        }

        enum FieldKind {
            Field((Span, InternedString), TypeAnnotation),
            Variant((Span, InternedString), Vec<TypeAnnotation>),
        }

        let mut fields = match fields
            .into_iter()
            .map(|field| {
                let span = field.span;

                match field.kind {
                    NodeKind::Error => None,
                    NodeKind::Name(name) => Some(FieldKind::Variant((span, name), Vec::new())),
                    NodeKind::List(nodes) => {
                        let mut nodes = nodes.into_iter();

                        let name = nodes.next().unwrap();
                        let name_span = name.span;
                        let name = match name.kind {
                            NodeKind::Name(name) => name,
                            _ => {
                                self.diagnostics.add(Diagnostic::error(
                                    "expected a name here",
                                    vec![Note::primary(
                                        name.span,
                                        "variants must be in the form `Name Type ...`",
                                    )],
                                ));

                                return None;
                            }
                        };

                        let tys = nodes
                            .map(|node| self.build_type_annotation(node))
                            .collect::<Vec<_>>();

                        Some(FieldKind::Variant((name_span, name), tys))
                    }
                    NodeKind::Annotate(name, ty) => {
                        let name = match name.kind {
                            NodeKind::Name(name) => name,
                            _ => {
                                self.diagnostics.add(Diagnostic::error(
                                    "expected a name here",
                                    vec![Note::primary(
                                        name.span,
                                        "fields must be in the form `name :: Type`",
                                    )],
                                ));

                                return None;
                            }
                        };

                        let ty = self.build_type_annotation(*ty);

                        Some(FieldKind::Field((span, name), ty))
                    }
                    _ => {
                        self.diagnostics.add(Diagnostic::error(
                            "expected a field or variant declaration",
                            vec![Note::primary(
                                span,
                                "fields must be in the form `name :: Type`",
                            )],
                        ));

                        None
                    }
                }
            })
            .collect::<Option<Vec<_>>>()
        {
            Some(fields) if !fields.is_empty() => fields.into_iter(),
            _ => return None,
        };

        let mut kind = match fields.next().unwrap() {
            FieldKind::Field((_, name), ty) => TypeKind::Structure(vec![DataField { name, ty }]),
            FieldKind::Variant((span, name), values) => {
                TypeKind::Enumeration(vec![DataVariant { name, span, values }])
            }
        };

        for field in fields {
            match field {
                FieldKind::Field((span, name), ty) => {
                    let fields = match &mut kind {
                        TypeKind::Structure(fields) => fields,
                        TypeKind::Enumeration(_) => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected field, found variant",
                                vec![Note::primary(span, "expected a field here")],
                            ));

                            continue;
                        }
                        _ => unreachable!(),
                    };

                    fields.push(DataField { name, ty });
                }
                FieldKind::Variant((span, name), values) => {
                    let variants = match &mut kind {
                        TypeKind::Structure(_) => {
                            self.diagnostics.add(Diagnostic::error(
                                "expected variant, found field",
                                vec![Note::primary(span, "expected a variant here")],
                            ));

                            continue;
                        }
                        TypeKind::Enumeration(variants) => variants,
                        _ => unreachable!(),
                    };

                    variants.push(DataVariant { name, span, values });
                }
            }
        }

        Some(kind)
    }
}
