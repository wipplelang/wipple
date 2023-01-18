use crate::{
    analysis::expand::{self, Node, NodeKind},
    diagnostics::*,
    helpers::{Backtrace, InternedString},
    parse::Span,
    Compiler, FilePath,
};

pub use crate::analysis::expand::{Declarations, FileAttributes, StatementAttributes};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub attributes: FileAttributes,
    pub declarations: Declarations<expand::Template>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub attributes: StatementAttributes,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Empty,
    Declaration(Declaration),
    Assign(Pattern, Expression),
    Use((Span, InternedString)),
    Expression(ExpressionKind),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Type((Span, InternedString), TypeDeclaration),
    Trait((Span, InternedString), TraitDeclaration),
    Constant((Span, InternedString), ConstantDeclaration),
    Instance(Instance),
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_parameters: Vec<TypeAnnotation>,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Error(Backtrace),
    Name(InternedString),
    Number(InternedString),
    Text(InternedString),
    Block(Vec<Statement>),
    End(Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
    Tuple(Vec<Expression>),
}

impl ExpressionKind {
    fn error(compiler: &Compiler) -> Self {
        ExpressionKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Span,
    pub attributes: StatementAttributes,
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
    Named(InternedString, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
    Tuple(Vec<TypeAnnotation>),
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
    Error(Backtrace),
    Wildcard,
    Number(InternedString),
    Text(InternedString),
    Name(InternedString),
    Destructure(Vec<(InternedString, Pattern)>),
    Variant((Span, InternedString), Vec<Pattern>),
    Annotate(Box<Pattern>, TypeAnnotation),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Expression>),
    Tuple(Vec<Pattern>),
}

impl PatternKind {
    fn error(compiler: &Compiler) -> Self {
        PatternKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub ty: Option<TypeAnnotation>,
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
    pub attributes: StatementAttributes,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct DataVariant {
    pub name: InternedString,
    pub span: Span,
    pub attributes: StatementAttributes,
    pub values: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub name: InternedString,
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

impl Compiler<'_> {
    pub(crate) fn build_ast(&self, file: expand::File) -> File {
        File {
            path: file.path,
            span: file.span,
            attributes: file.attributes,
            declarations: file.declarations,
            statements: file
                .statements
                .into_iter()
                .map(|node| self.build_statement(node))
                .collect(),
        }
    }

    fn build_statement(&self, statement: expand::Statement) -> Statement {
        Statement {
            span: statement.node.span,
            attributes: statement.attributes,
            kind: if statement.treat_as_expr {
                StatementKind::Expression(self.build_expression(statement.node).kind)
            } else {
                (|| match statement.node.kind {
                    NodeKind::Assign(pattern, value) => match pattern.kind {
                        NodeKind::TypeFunction(lhs, rhs) => {
                            let (params, bounds) = match self.build_type_function(*lhs) {
                                Some((params, bounds)) => (params, bounds),
                                None => {
                                    return StatementKind::Expression(ExpressionKind::error(self))
                                }
                            };

                            match rhs.kind {
                                NodeKind::Instance(trait_name, trait_parameters) => {
                                    match self.build_instance(
                                        params,
                                        bounds,
                                        *trait_name,
                                        trait_parameters,
                                        Some(*value),
                                    ) {
                                        Some(instance) => StatementKind::Declaration(
                                            Declaration::Instance(instance),
                                        ),
                                        None => {
                                            StatementKind::Expression(ExpressionKind::error(self))
                                        }
                                    }
                                }
                                _ => {
                                    self.add_error(
                                        "expected instance here",
                                        vec![Note::primary(
                                            rhs.span,
                                            "try adding `instance` to the left-hand side of `:`",
                                        )],
                                    );

                                    StatementKind::Expression(ExpressionKind::error(self))
                                }
                            }
                        }
                        NodeKind::Instance(trait_name, trait_parameters) => {
                            match self.build_instance(
                                Vec::new(),
                                Vec::new(),
                                *trait_name,
                                trait_parameters,
                                Some(*value),
                            ) {
                                Some(instance) => {
                                    StatementKind::Declaration(Declaration::Instance(instance))
                                }
                                None => StatementKind::Expression(ExpressionKind::error(self)),
                            }
                        }
                        _ => {
                            let pattern = self.build_pattern(*pattern);

                            match value.kind {
                                NodeKind::Assign(_, _) => {
                                    self.add_error(
                                        "expected expression, found assignment",
                                        vec![Note::primary(
                                            value.span,
                                            "try moving this variable assignment onto its own line",
                                        )],
                                    );

                                    StatementKind::Expression(ExpressionKind::error(self))
                                }
                                NodeKind::Template(_, _) => {
                                    self.add_error(
                                        "unexpanded template",
                                        vec![Note::primary(
                                            value.span,
                                            "this template was never expanded",
                                        )],
                                    );

                                    StatementKind::Expression(ExpressionKind::error(self))
                                }
                                NodeKind::Type(fields) => {
                                    let name = match pattern.kind {
                                        PatternKind::Name(name) => name,
                                        _ => {
                                            self.add_error(
                                                "type declaration must be assigned to a name",
                                                vec![Note::primary(
                                                    value.span,
                                                    "try providing a name here",
                                                )],
                                            );

                                            return StatementKind::Expression(
                                                ExpressionKind::error(self),
                                            );
                                        }
                                    };

                                    match self.build_type_declaration(value.span, fields) {
                                        Some(kind) => {
                                            StatementKind::Declaration(Declaration::Type(
                                                (pattern.span, name),
                                                TypeDeclaration {
                                                    parameters: Vec::new(),
                                                    kind,
                                                },
                                            ))
                                        }
                                        None => {
                                            StatementKind::Expression(ExpressionKind::error(self))
                                        }
                                    }
                                }
                                NodeKind::Trait(ty) => {
                                    let name = match pattern.kind {
                                        PatternKind::Name(name) => name,
                                        _ => {
                                            self.add_error(
                                                "trait declaration must be assigned to a name",
                                                vec![Note::primary(
                                                    value.span,
                                                    "try providing a name here",
                                                )],
                                            );

                                            return StatementKind::Expression(
                                                ExpressionKind::error(self),
                                            );
                                        }
                                    };

                                    StatementKind::Declaration(Declaration::Trait(
                                        (pattern.span, name),
                                        TraitDeclaration {
                                            parameters: Vec::new(),
                                            ty: ty.map(|ty| self.build_type_annotation(*ty)),
                                        },
                                    ))
                                }
                                NodeKind::TypeFunction(parameters, node) => {
                                    let name = match pattern.kind {
                                        PatternKind::Name(name) => name,
                                        _ => {
                                            self.add_error(
                                                "type or trait declaration must be assigned to a name", vec![Note::primary(
                                                    value.span,
                                                    "try providing a name here",
                                                )],
                                            );

                                            return StatementKind::Expression(
                                                ExpressionKind::error(self),
                                            );
                                        }
                                    };

                                    let (parameters, bounds) =
                                        match self.build_type_function(*parameters) {
                                            Some((parameters, bounds)) => (parameters, bounds),
                                            None => {
                                                return StatementKind::Expression(
                                                    ExpressionKind::error(self),
                                                )
                                            }
                                        };

                                    let check_bounds = |kind: &str| {
                                        if !bounds.is_empty() {
                                            self.add_error(
                                                format!("bounds are not allowed on {}", kind),
                                                vec![Note::primary(
                                                    bounds
                                                        .first()
                                                        .unwrap()
                                                        .span
                                                        .with_end(bounds.last().unwrap().span.end),
                                                    "try moving these to the respective functions instead",
                                                )],
                                            );
                                        }
                                    };

                                    match node.kind {
                                        NodeKind::Type(fields) => {
                                            check_bounds("types");

                                            match self.build_type_declaration(node.span, fields) {
                                                Some(kind) => {
                                                    StatementKind::Declaration(Declaration::Type(
                                                        (pattern.span, name),
                                                        TypeDeclaration { parameters, kind },
                                                    ))
                                                }
                                                None => StatementKind::Expression(
                                                    ExpressionKind::error(self),
                                                ),
                                            }
                                        }
                                        NodeKind::Trait(ty) => {
                                            check_bounds("traits");

                                            StatementKind::Declaration(Declaration::Trait(
                                                (pattern.span, name),
                                                TraitDeclaration {
                                                    parameters,
                                                    ty: ty
                                                        .map(|ty| self.build_type_annotation(*ty)),
                                                },
                                            ))
                                        }
                                        _ => {
                                            self.add_error(
                                                "expected type or trait declaration in type function", vec![Note::primary(
                                                    node.span,
                                                    "only types and traits may have type parameters",
                                                )],
                                            );

                                            StatementKind::Expression(ExpressionKind::error(self))
                                        }
                                    }
                                }
                                _ => StatementKind::Assign(pattern, self.build_expression(*value)),
                            }
                        }
                    },
                    NodeKind::Template(_, _) => {
                        // Templates are stripped during the expansion phase
                        StatementKind::Empty
                    }
                    NodeKind::Annotate(expr, ty) => {
                        let (span, name) = match expr.kind {
                            NodeKind::Name(name) => (expr.span, name),
                            _ => {
                                return StatementKind::Expression(
                                    self.build_expression(Node {
                                        span: statement.node.span,
                                        kind: NodeKind::Annotate(expr, ty),
                                    })
                                    .kind,
                                )
                            }
                        };

                        let (parameters, bounds, ty) = match ty.kind {
                            NodeKind::TypeFunction(parameters, ty) => {
                                let (parameters, bounds) =
                                    match self.build_type_function(*parameters) {
                                        Some((parameters, bounds)) => (parameters, bounds),
                                        None => {
                                            return StatementKind::Expression(
                                                ExpressionKind::error(self),
                                            )
                                        }
                                    };

                                (parameters, bounds, self.build_type_annotation(*ty))
                            }
                            _ => (Vec::new(), Vec::new(), self.build_type_annotation(*ty)),
                        };

                        StatementKind::Declaration(Declaration::Constant(
                            (span, name),
                            ConstantDeclaration {
                                name,
                                parameters,
                                bounds,
                                ty,
                            },
                        ))
                    }
                    NodeKind::UseFile(_) => StatementKind::Empty,
                    NodeKind::UseExpr(expr) => {
                        let name = match expr.kind {
                            NodeKind::Name(name) => name,
                            _ => {
                                self.add_error(
                                    "`use` expects a path to a file or a name of a type",
                                    vec![Note::primary(expr.span, "expected a path or type here")],
                                );

                                return StatementKind::Expression(ExpressionKind::error(self));
                            }
                        };

                        StatementKind::Use((expr.span, name))
                    }
                    NodeKind::TypeFunction(lhs, rhs) => {
                        let (params, bounds) = match self.build_type_function(*lhs) {
                            Some((params, bounds)) => (params, bounds),
                            None => return StatementKind::Expression(ExpressionKind::error(self)),
                        };

                        match rhs.kind {
                            NodeKind::Instance(trait_name, trait_parameters) => {
                                match self.build_instance(
                                    params,
                                    bounds,
                                    *trait_name,
                                    trait_parameters,
                                    None,
                                ) {
                                    Some(instance) => {
                                        StatementKind::Declaration(Declaration::Instance(instance))
                                    }
                                    None => StatementKind::Expression(ExpressionKind::error(self)),
                                }
                            }
                            _ => {
                                self.add_error(
                                    "expected instance here",
                                    vec![Note::primary(
                                        rhs.span,
                                        "try adding `instance` to the left-hand side of `:`",
                                    )],
                                );

                                StatementKind::Expression(ExpressionKind::error(self))
                            }
                        }
                    }
                    NodeKind::Instance(trait_name, trait_parameters) => {
                        match self.build_instance(
                            Vec::new(),
                            Vec::new(),
                            *trait_name,
                            trait_parameters,
                            None,
                        ) {
                            Some(instance) => {
                                StatementKind::Declaration(Declaration::Instance(instance))
                            }
                            None => StatementKind::Expression(ExpressionKind::error(self)),
                        }
                    }
                    _ => StatementKind::Expression(self.build_expression(statement.node).kind),
                })()
            },
        }
    }

    fn build_instance(
        &self,
        parameters: Vec<TypeParameter>,
        bounds: Vec<Bound>,
        trait_name: Node,
        trait_parameters: Vec<Node>,
        value: Option<Node>,
    ) -> Option<Instance> {
        let trait_span = trait_name.span;
        let trait_name = match trait_name.kind {
            NodeKind::Error(_) => return None,
            NodeKind::Name(name) => name,
            _ => {
                self.add_error(
                    "expected trait name",
                    vec![Note::primary(
                        trait_name.span,
                        "`instance` requires the name of a trait",
                    )],
                );

                return None;
            }
        };

        let trait_parameters = trait_parameters
            .into_iter()
            .map(|node| self.build_type_annotation(node))
            .collect();

        let value = value.map(|value| self.build_expression(value));

        Some(Instance {
            parameters,
            bounds,
            trait_span,
            trait_name,
            trait_parameters,
            value,
        })
    }

    fn build_expression(&self, node: Node) -> Expression {
        Expression {
            span: node.span,
            kind: (|| match node.kind {
                NodeKind::Error(trace) => ExpressionKind::Error(trace),
                NodeKind::Empty => ExpressionKind::Tuple(Vec::new()),
                NodeKind::Name(name) => ExpressionKind::Name(name),
                NodeKind::Text(text) => ExpressionKind::Text(text),
                NodeKind::Number(number) => ExpressionKind::Number(number),
                NodeKind::List(exprs) => {
                    let mut exprs = exprs.into_iter();
                    match exprs.next() {
                        Some(func) => ExpressionKind::Call(
                            Box::new(self.build_expression(func)),
                            exprs.map(|expr| self.build_expression(expr)).collect(),
                        ),
                        None => ExpressionKind::Tuple(Vec::new()),
                    }
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
                NodeKind::External(lib, identifier, inputs) => {
                    let lib = match lib.kind {
                        NodeKind::Error(trace) => return ExpressionKind::Error(trace),
                        NodeKind::Text(text) => text,
                        _ => {
                            self.add_error(
                                "expected text here",
                                vec![Note::primary(lib.span, "`external` requires an ABI")],
                            );

                            return ExpressionKind::error(self);
                        }
                    };

                    let identifier = match identifier.kind {
                        NodeKind::Error(trace) => return ExpressionKind::Error(trace),
                        NodeKind::Text(text) => text,
                        _ => {
                            self.add_error(
                                "expected text here",
                                vec![Note::primary(
                                    identifier.span,
                                    "`external` requires an identifier",
                                )],
                            );

                            return ExpressionKind::error(self);
                        }
                    };

                    ExpressionKind::External(
                        lib,
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
                NodeKind::When(input, block) => {
                    let input = self.build_expression(*input);

                    let arms = block
                        .into_iter()
                        .filter_map(|statement| {
                            let expr = self.build_expression(statement.node);

                            match expr.kind {
                                ExpressionKind::Function(pattern, body) => Some(Arm {
                                    span: expr.span,
                                    attributes: statement.attributes,
                                    pattern,
                                    body: *body,
                                }),
                                _ => {
                                    self.add_error(
                                        "expected function",
                                        vec![Note::primary(expr.span, "this is not a function")],
                                    );

                                    None
                                }
                            }
                        })
                        .collect();

                    ExpressionKind::When(Box::new(input), arms)
                }
                NodeKind::Or(lhs, rhs) => {
                    // Use the `Or` trait defined in the prelude

                    let rhs = self.build_expression(*rhs);

                    ExpressionKind::Call(
                        Box::new(Expression {
                            span: Span::builtin("`Or` trait"),
                            kind: ExpressionKind::Name(InternedString::new("Or")),
                        }),
                        vec![
                            self.build_expression(*lhs),
                            Expression {
                                span: rhs.span,
                                kind: ExpressionKind::Function(
                                    Pattern {
                                        span: rhs.span,
                                        kind: PatternKind::Tuple(Vec::new()),
                                    },
                                    Box::new(rhs),
                                ),
                            },
                        ],
                    )
                }
                NodeKind::Tuple(nodes) => ExpressionKind::Tuple(
                    nodes
                        .into_iter()
                        .map(|node| self.build_expression(node))
                        .collect(),
                ),
                NodeKind::End(value) => {
                    ExpressionKind::End(Box::new(self.build_expression(*value)))
                }
                _ => {
                    self.add_error(
                        "expected expression",
                        vec![Note::primary(node.span, "this is not an expression")],
                    );

                    ExpressionKind::error(self)
                }
            })(),
        }
    }

    fn build_pattern(&self, node: Node) -> Pattern {
        Pattern {
            span: node.span,
            kind: (|| {
                match node.kind {
                    NodeKind::Error(trace) => PatternKind::Error(trace),
                    NodeKind::Underscore => PatternKind::Wildcard,
                    NodeKind::Name(name) => PatternKind::Name(name),
                    NodeKind::Block(statements) => {
                        PatternKind::Destructure(
                            statements
                                .into_iter()
                                .filter_map(
                                    |statement| -> Option<
                                        Box<dyn Iterator<Item = (InternedString, Pattern)>>,
                                    > {
                                        if statement.attributes != StatementAttributes::default() {
                                            self.add_error(
                                                "attributes are not supported inside patterns", vec![Note::primary(
                                                    statement.node.span,
                                                    "try removing this",
                                                )],
                                            );
                                        }

                                        match statement.node.kind {
                                            NodeKind::Error(_) => None,
                                            NodeKind::List(nodes) => Some(Box::new(
                                                nodes.into_iter().filter_map(|node| {
                                                    let (name, pattern) = match node.kind {
                                                        NodeKind::Error(_) => return None,
                                                        NodeKind::Name(name) => (
                                                            name,
                                                            Pattern {
                                                                span: node.span,
                                                                kind: PatternKind::Name(name),
                                                            },
                                                        ),
                                                        _ => {
                                                            self.add_error(
                                                                "invalid pattern in destructuring pattern", vec![Note::primary(
                                                                    node.span,
                                                                    "expected name here",
                                                                )],
                                                            );

                                                            return None;
                                                        }
                                                    };

                                                    Some((name, pattern))
                                                }),
                                            )),
                                            NodeKind::Name(name) => {
                                                Some(Box::new(std::iter::once((
                                                    name,
                                                    Pattern {
                                                        span: statement.node.span,
                                                        kind: PatternKind::Name(name),
                                                    },
                                                ))))
                                            }
                                            NodeKind::Assign(left, right) => {
                                                let name = match left.kind {
                                                    NodeKind::Name(name) => name,
                                                    _ => {
                                                        self.add_error(
                                                            "invalid pattern in destructuring pattern", vec![Note::primary(
                                                                left.span,
                                                                "expected name here",
                                                            )],
                                                        );

                                                        return None;
                                                    }
                                                };

                                                let pattern = self.build_pattern(*right);

                                                Some(Box::new(std::iter::once((name, pattern))))
                                            }
                                            _ => {
                                                self.add_error(
                                                    "invalid pattern in destructuring pattern", vec![Note::primary(
                                                        node.span,
                                                        "try removing this",
                                                    )],
                                                );

                                                None
                                            }
                                        }
                                    },
                                )
                                .flatten()
                                .collect(),
                        )
                    }
                    NodeKind::List(nodes) => {
                        let mut nodes = nodes.into_iter();

                        let name = match nodes.next() {
                            Some(node) => node,
                            None => {
                                self.add_error(
                                    "expected variant name", vec![Note::primary(
                                        node.span,
                                        "only variants may be used in this kind of pattern",
                                    )],
                                );

                                return PatternKind::error(self);
                            },
                        };

                        let name_span = name.span;

                        let name = match name.kind {
                            NodeKind::Name(name) => name,
                            _ => {
                                self.add_error(
                                    "expected variant name", vec![Note::primary(
                                        node.span,
                                        "only variants may be used in this kind of pattern",
                                    )],
                                );

                                return PatternKind::error(self);
                            }
                        };

                        let rest = nodes.map(|node| self.build_pattern(node)).collect();

                        PatternKind::Variant((name_span, name), rest)
                    }
                    NodeKind::Empty => PatternKind::Tuple(Vec::new()),
                    NodeKind::Number(number) => PatternKind::Number(number),
                    NodeKind::Text(text) => PatternKind::Text(text),
                    NodeKind::Annotate(node, ty) => {
                        let inner = self.build_pattern(*node);
                        let ty = self.build_type_annotation(*ty);

                        PatternKind::Annotate(Box::new(inner), ty)
                    }
                    NodeKind::Or(lhs, rhs) => PatternKind::Or(
                        Box::new(self.build_pattern(*lhs)),
                        Box::new(self.build_pattern(*rhs)),
                    ),
                    NodeKind::Where(pattern, condition) => PatternKind::Where(
                        Box::new(self.build_pattern(*pattern)),
                        Box::new(self.build_expression(*condition)),
                    ),
                    NodeKind::Tuple(nodes) => PatternKind::Tuple(
                        nodes
                            .into_iter()
                            .map(|node| self.build_pattern(node))
                            .collect(),
                    ),
                    _ => {
                        self.add_error(
                            "expected pattern",
                            vec![Note::primary(
                                node.span,
                                "values may not appear on the left-hand side of a variable assignment",
                            )],
                        );

                        PatternKind::error(self)
                    }
                }
            })(),
        }
    }

    fn build_type_annotation(&self, node: Node) -> TypeAnnotation {
        TypeAnnotation {
            span: node.span,
            kind: (|| match node.kind {
                NodeKind::Empty => TypeAnnotationKind::Tuple(Vec::new()),
                NodeKind::Underscore => TypeAnnotationKind::Placeholder,
                NodeKind::Name(name) => TypeAnnotationKind::Named(name, Vec::new()),
                NodeKind::Function(input, output) => TypeAnnotationKind::Function(
                    Box::new(self.build_type_annotation(*input)),
                    Box::new(self.build_type_annotation(*output)),
                ),
                NodeKind::List(nodes) => {
                    let mut nodes = nodes.into_iter();

                    let ty = match nodes.next() {
                        Some(ty) => ty,
                        None => return TypeAnnotationKind::Tuple(Vec::new()),
                    };

                    let name = match ty.kind {
                        NodeKind::Name(name) => name,
                        _ => {
                            self.add_error(
                                "expected type",
                                vec![Note::primary(ty.span, "this is not a type")],
                            );

                            return TypeAnnotationKind::Error;
                        }
                    };

                    TypeAnnotationKind::Named(
                        name,
                        nodes.map(|node| self.build_type_annotation(node)).collect(),
                    )
                }
                NodeKind::Tuple(nodes) => TypeAnnotationKind::Tuple(
                    nodes
                        .into_iter()
                        .map(|node| self.build_type_annotation(node))
                        .collect(),
                ),
                _ => {
                    self.add_error(
                        "expected type",
                        vec![Note::primary(node.span, "this is not a type")],
                    );

                    TypeAnnotationKind::Error
                }
            })(),
        }
    }

    fn build_type_function(&self, lhs: Node) -> Option<(Vec<TypeParameter>, Vec<Bound>)> {
        macro_rules! build_parameter_list {
            ($tys:expr) => {
                $tys.into_iter()
                    .map(|node| match node.kind {
                        NodeKind::Error(_) => None,
                        NodeKind::Name(name) => Some(TypeParameter {
                            span: node.span,
                            name,
                        }),
                        _ => {
                            self.add_error(
                                "expected type parameter",
                                vec![Note::primary(node.span, "try removing this")],
                            );

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
                        NodeKind::Error(_) => return None,
                        NodeKind::Name(name) => name,
                        _ => {
                            self.add_error(
                                "expected trait name in `where` clause",
                                vec![Note::primary(trait_name.span, "try adding a name here")],
                            );

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
            NodeKind::Error(_) => None,
            NodeKind::Name(name) => Some((
                vec![TypeParameter {
                    span: lhs.span,
                    name,
                }],
                Vec::new(),
            )),
            NodeKind::List(tys) => Some((build_parameter_list!(tys)?, Vec::new())),
            NodeKind::Where(lhs, bounds) => {
                let tys = match lhs.kind {
                    NodeKind::Error(_) => return None,
                    NodeKind::Name(name) => vec![TypeParameter {
                        span: lhs.span,
                        name,
                    }],
                    NodeKind::List(tys) => build_parameter_list!(tys)?,
                    _ => {
                        self.add_error(
                            "expected type parameters on left-hand side of `where` clause",
                            vec![Note::primary(
                                lhs.span,
                                "try providing a list of names here",
                            )],
                        );

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
                                            self.add_error(
                                                "expected bound", vec![Note::primary(
                                                    bounds_span,
                                                    "`where` bound must be in the format `(T A B ...)`",
                                                )],
                                            );

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
                            self.add_error(
                                "expected bounds",
                                vec![Note::primary(
                                    bounds_span,
                                    "`where` bounds must be in the format `(T A) (T B) ...`",
                                )],
                            );

                            None
                        }
                    }
                })()?;

                Some((tys, bounds))
            }
            _ => {
                self.add_error(
                    "expected type parameters",
                    vec![Note::primary(
                        lhs.span,
                        "try providing a list of names and optionally a `where` clause",
                    )],
                );

                None
            }
        }
    }

    fn build_type_declaration(
        &self,
        span: Span,
        fields: Option<Vec<expand::Statement>>,
    ) -> Option<TypeKind> {
        let fields = match fields {
            Some(fields) => fields,
            None => return Some(TypeKind::Marker),
        };

        if fields.is_empty() {
            self.add_error(
                "type must not be empty",
                vec![Note::primary(
                    span,
                    "try adding some fields or variants here",
                )],
            );

            return None;
        }

        struct Field {
            span: Span,
            attributes: StatementAttributes,
            name: InternedString,
            kind: FieldKind,
        }

        enum FieldKind {
            Field(TypeAnnotation),
            Variant(Vec<TypeAnnotation>),
        }

        let mut fields = match fields
            .into_iter()
            .map(|field| {
                let span = field.node.span;

                match field.node.kind {
                    NodeKind::Error(_) => None,
                    NodeKind::Name(name) => Some(Field {
                        span,
                        attributes: field.attributes,
                        name,
                        kind: FieldKind::Variant(Vec::new()),
                    }),
                    NodeKind::List(nodes) => {
                        let mut nodes = nodes.into_iter();

                        let name = nodes.next().unwrap();
                        let name_span = name.span;
                        let name = match name.kind {
                            NodeKind::Name(name) => name,
                            _ => {
                                self.add_error(
                                    "expected a name here",
                                    vec![Note::primary(
                                        name.span,
                                        "variants must be in the form `Name Type ...`",
                                    )],
                                );

                                return None;
                            }
                        };

                        let tys = nodes
                            .map(|node| self.build_type_annotation(node))
                            .collect::<Vec<_>>();

                        Some(Field {
                            span: name_span,
                            attributes: field.attributes,
                            name,
                            kind: FieldKind::Variant(tys),
                        })
                    }
                    NodeKind::Annotate(name, ty) => {
                        let name = match name.kind {
                            NodeKind::Name(name) => name,
                            _ => {
                                self.add_error(
                                    "expected a name here",
                                    vec![Note::primary(
                                        name.span,
                                        "fields must be in the form `name :: Type`",
                                    )],
                                );

                                return None;
                            }
                        };

                        let ty = self.build_type_annotation(*ty);

                        Some(Field {
                            span,
                            attributes: field.attributes,
                            name,
                            kind: FieldKind::Field(ty),
                        })
                    }
                    _ => {
                        self.add_error(
                            "expected a field or variant declaration",
                            vec![Note::primary(
                                span,
                                "fields must be in the form `name :: Type`",
                            )],
                        );

                        None
                    }
                }
            })
            .collect::<Option<Vec<_>>>()
        {
            Some(fields) if !fields.is_empty() => fields.into_iter(),
            _ => return None,
        };

        let first = fields.next().unwrap();

        let mut kind = match first.kind {
            FieldKind::Field(ty) => TypeKind::Structure(vec![DataField {
                name: first.name,
                attributes: first.attributes,
                ty,
            }]),
            FieldKind::Variant(values) => TypeKind::Enumeration(vec![DataVariant {
                name: first.name,
                span: first.span,
                attributes: first.attributes,
                values,
            }]),
        };

        for field in fields {
            match field.kind {
                FieldKind::Field(ty) => {
                    let fields = match &mut kind {
                        TypeKind::Structure(fields) => fields,
                        TypeKind::Enumeration(_) => {
                            self.add_error(
                                "expected field, found variant",
                                vec![Note::primary(field.span, "expected a field here")],
                            );

                            continue;
                        }
                        _ => unreachable!(),
                    };

                    fields.push(DataField {
                        name: field.name,
                        attributes: field.attributes,
                        ty,
                    });
                }
                FieldKind::Variant(values) => {
                    let variants = match &mut kind {
                        TypeKind::Structure(_) => {
                            self.add_error(
                                "expected variant, found field",
                                vec![Note::primary(field.span, "expected a variant here")],
                            );

                            continue;
                        }
                        TypeKind::Enumeration(variants) => variants,
                        _ => unreachable!(),
                    };

                    variants.push(DataVariant {
                        name: field.name,
                        span: field.span,
                        attributes: field.attributes,
                        values,
                    });
                }
            }
        }

        Some(kind)
    }
}
