use crate::{
    analysis::expand_v2 as expand,
    diagnostics::*,
    helpers::{Backtrace, InternedString},
    parse::Span,
    Compiler, FilePath, ScopeId, TemplateId,
};
use std::collections::{BTreeMap, HashSet};

pub use expand::{FileAttributes, StatementAttributes, SyntaxDeclarationAttributes};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub attributes: FileAttributes,
    pub syntax_declarations: BTreeMap<TemplateId, SyntaxDeclaration>,
    pub scope: ScopeId,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct SyntaxDeclaration {
    pub name: InternedString,
    pub span: Span,
    pub uses: HashSet<Span>,
    pub operator: bool,
    pub attributes: SyntaxDeclarationAttributes,
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
    pub scope: ScopeId,
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
    Name(ScopeId, InternedString),
    Number(InternedString),
    Text(InternedString),
    Block(ScopeId, Vec<Statement>),
    End(Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Function(ScopeId, Pattern, Box<Expression>),
    When(Box<Expression>, ScopeId, Vec<Arm>),
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
    pub scope: ScopeId,
    pub parameters: Vec<TypeParameter>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub scope: ScopeId,
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
    pub scope: ScopeId,
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
            path: file.span.path,
            span: file.span,
            attributes: file.attributes,
            syntax_declarations: self.build_syntax_declarations(file.declarations),
            scope: file.scope,
            statements: file
                .statements
                .into_iter()
                .map(|expr| self.build_statement(expr))
                .collect(),
        }
    }

    fn build_syntax_declarations(
        &self,
        declarations: expand::Declarations,
    ) -> BTreeMap<TemplateId, SyntaxDeclaration> {
        todo!()
    }

    fn build_statement(&self, statement: expand::Statement) -> Statement {
        let attributes = StatementAttributes::default();

        for attribute in statement.attributes {}

        Statement {
            span: statement.span,
            attributes,
            kind: (|| match statement.expr.kind {
                expand::ExpressionKind::Assign(pattern, value) => match pattern.kind {
                    expand::ExpressionKind::TypeFunction(scope, lhs, rhs) => {
                        let (params, bounds) = match self.build_type_function(*lhs) {
                            Some((params, bounds)) => (params, bounds),
                            None => return StatementKind::Expression(ExpressionKind::error(self)),
                        };

                        match rhs.kind {
                            expand::ExpressionKind::Instance(tr) => {
                                match self.build_instance(params, bounds, *tr, Some(*value)) {
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
                    expand::ExpressionKind::Instance(tr) => {
                        match self.build_instance(Vec::new(), Vec::new(), *tr, Some(*value)) {
                            Some(instance) => {
                                StatementKind::Declaration(Declaration::Instance(instance))
                            }
                            None => StatementKind::Expression(ExpressionKind::error(self)),
                        }
                    }
                    _ => {
                        let pattern = self.build_pattern(*pattern);

                        match value.kind {
                            expand::ExpressionKind::Assign(_, _) => {
                                self.add_error(
                                    "expected expression, found assignment",
                                    vec![Note::primary(
                                        value.span,
                                        "try moving this variable assignment onto its own line",
                                    )],
                                );

                                StatementKind::Expression(ExpressionKind::error(self))
                            }
                            expand::ExpressionKind::Template(_, _) => {
                                self.add_error(
                                    "unexpanded template",
                                    vec![Note::primary(
                                        value.span,
                                        "this template was never expanded",
                                    )],
                                );

                                StatementKind::Expression(ExpressionKind::error(self))
                            }
                            expand::ExpressionKind::Type(fields) => {
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

                                        return StatementKind::Expression(ExpressionKind::error(
                                            self,
                                        ));
                                    }
                                };

                                match self.build_type_declaration(value.span, fields) {
                                    Some(kind) => StatementKind::Declaration(Declaration::Type(
                                        (pattern.span, name),
                                        TypeDeclaration {
                                            parameters: Vec::new(),
                                            kind,
                                        },
                                    )),
                                    None => StatementKind::Expression(ExpressionKind::error(self)),
                                }
                            }
                            expand::ExpressionKind::Trait(ty) => {
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

                                        return StatementKind::Expression(ExpressionKind::error(
                                            self,
                                        ));
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
                            expand::ExpressionKind::TypeFunction(parameters, expr) => {
                                let name = match pattern.kind {
                                    PatternKind::Name(name) => name,
                                    _ => {
                                        self.add_error(
                                            "type or trait declaration must be assigned to a name",
                                            vec![Note::primary(
                                                value.span,
                                                "try providing a name here",
                                            )],
                                        );

                                        return StatementKind::Expression(ExpressionKind::error(
                                            self,
                                        ));
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

                                match expr.kind {
                                    expand::ExpressionKind::Type(fields) => {
                                        check_bounds("types");

                                        match self.build_type_declaration(expr.span, fields) {
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
                                    expand::ExpressionKind::Trait(ty) => {
                                        check_bounds("traits");

                                        StatementKind::Declaration(Declaration::Trait(
                                            (pattern.span, name),
                                            TraitDeclaration {
                                                parameters,
                                                ty: ty.map(|ty| self.build_type_annotation(*ty)),
                                            },
                                        ))
                                    }
                                    _ => {
                                        self.add_error(
                                            "expected type or trait declaration in type function",
                                            vec![Note::primary(
                                                expr.span,
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
                expand::ExpressionKind::Annotate(expr, ty) => {
                    let (span, name) = match expr.kind {
                        expand::ExpressionKind::Name(name) => (expr.span, name),
                        _ => {
                            return StatementKind::Expression(
                                self.build_expression(expand::Expression {
                                    span: statement.expr.span,
                                    kind: expand::ExpressionKind::Annotate(expr, ty),
                                })
                                .kind,
                            )
                        }
                    };

                    let (parameters, bounds, ty) = match ty.kind {
                        expand::ExpressionKind::TypeFunction(parameters, ty) => {
                            let (parameters, bounds) = match self.build_type_function(*parameters) {
                                Some((parameters, bounds)) => (parameters, bounds),
                                None => {
                                    return StatementKind::Expression(ExpressionKind::error(self))
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
                expand::ExpressionKind::UseFile(_) => StatementKind::Empty,
                expand::ExpressionKind::UseExpr(expr) => {
                    let name = match expr.kind {
                        expand::ExpressionKind::Name(name) => name,
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
                expand::ExpressionKind::TypeFunction(lhs, rhs) => {
                    let (params, bounds) = match self.build_type_function(*lhs) {
                        Some((params, bounds)) => (params, bounds),
                        None => return StatementKind::Expression(ExpressionKind::error(self)),
                    };

                    match rhs.kind {
                        expand::ExpressionKind::Instance(trait_name, trait_parameters) => {
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
                expand::ExpressionKind::Instance(trait_name, trait_parameters) => {
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
                _ => StatementKind::Expression(self.build_expression(statement.expr).kind),
            })(),
        }
    }

    fn build_instance(
        &self,
        parameters: Vec<TypeParameter>,
        bounds: Vec<Bound>,
        tr: expand::Expression,
        value: Option<expand::Expression>,
    ) -> Option<Instance> {
        let annotation = self.build_type_annotation(tr);

        let (trait_name, trait_parameters) = match annotation.kind {
            TypeAnnotationKind::Named(name, params) => (name, params),
            _ => {
                self.add_error(
                    "expected trait",
                    vec![Note::primary(
                        annotation.span,
                        "`instance` requires the name of a trait and its parameters",
                    )],
                );

                return None;
            }
        };

        let value = value.map(|value| self.build_expression(value));

        Some(Instance {
            parameters,
            bounds,
            trait_span: annotation.span,
            trait_name,
            trait_parameters,
            value,
        })
    }

    fn build_expression(&self, expr: expand::Expression) -> Expression {
        Expression {
            span: expr.span,
            kind: (|| match expr.kind {
                expand::ExpressionKind::Error(trace) => ExpressionKind::Error(trace),
                expand::ExpressionKind::Empty => ExpressionKind::Tuple(Vec::new()),
                expand::ExpressionKind::Name(name) => ExpressionKind::Name(name),
                expand::ExpressionKind::Text(text) => ExpressionKind::Text(text),
                expand::ExpressionKind::Number(number) => ExpressionKind::Number(number),
                expand::ExpressionKind::List(exprs) => {
                    let mut exprs = exprs.into_iter();
                    match exprs.next() {
                        Some(func) => ExpressionKind::Call(
                            Box::new(self.build_expression(func)),
                            exprs.map(|expr| self.build_expression(expr)).collect(),
                        ),
                        None => ExpressionKind::Tuple(Vec::new()),
                    }
                }
                expand::ExpressionKind::Block(statements) => ExpressionKind::Block(
                    statements
                        .into_iter()
                        .map(|expr| self.build_statement(expr))
                        .collect(),
                ),
                expand::ExpressionKind::Function(input, body) => ExpressionKind::Function(
                    self.build_pattern(*input),
                    Box::new(self.build_expression(*body)),
                ),
                expand::ExpressionKind::External(lib, identifier, inputs) => {
                    let lib = match lib.kind {
                        expand::ExpressionKind::Error(trace) => {
                            return ExpressionKind::Error(trace)
                        }
                        expand::ExpressionKind::Text(text) => text,
                        _ => {
                            self.add_error(
                                "expected text here",
                                vec![Note::primary(lib.span, "`external` requires an ABI")],
                            );

                            return ExpressionKind::error(self);
                        }
                    };

                    let identifier = match identifier.kind {
                        expand::ExpressionKind::Error(trace) => {
                            return ExpressionKind::Error(trace)
                        }
                        expand::ExpressionKind::Text(text) => text,
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
                            .map(|expr| self.build_expression(expr))
                            .collect(),
                    )
                }
                expand::ExpressionKind::Annotate(value, ty) => ExpressionKind::Annotate(
                    Box::new(self.build_expression(*value)),
                    self.build_type_annotation(*ty),
                ),
                expand::ExpressionKind::When(input, block) => {
                    let input = self.build_expression(*input);

                    let arms = block
                        .into_iter()
                        .filter_map(|statement| {
                            let expr = self.build_expression(statement.expr);

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
                expand::ExpressionKind::Or(lhs, rhs) => {
                    // Use the `Or` trait defined in the prelude

                    let rhs = self.build_expression(*rhs);

                    ExpressionKind::Call(
                        Box::new(Expression {
                            span: Span::builtin(),
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
                expand::ExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                    exprs
                        .into_iter()
                        .map(|expr| self.build_expression(expr))
                        .collect(),
                ),
                expand::ExpressionKind::End(value) => {
                    ExpressionKind::End(Box::new(self.build_expression(*value)))
                }
                _ => {
                    self.add_error(
                        "expected expression",
                        vec![Note::primary(expr.span, "this is not an expression")],
                    );

                    ExpressionKind::error(self)
                }
            })(),
        }
    }

    fn build_pattern(&self, expr: expand::Expression) -> Pattern {
        Pattern {
            span: expr.span,
            kind: (|| {
                match expr.kind {
                    expand::ExpressionKind::Error(trace) => PatternKind::Error(trace),
                    expand::ExpressionKind::Underscore => PatternKind::Wildcard,
                    expand::ExpressionKind::Name(name) => PatternKind::Name(name),
                    expand::ExpressionKind::Block(statements) => {
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
                                                    statement.expr.span,
                                                    "try removing this",
                                                )],
                                            );
                                        }

                                        match statement.expr.kind {
                                            expand::ExpressionKind::Error(_) => None,
                                            expand::ExpressionKind::List(exprs) => Some(Box::new(
                                                exprs.into_iter().filter_map(|expr| {
                                                    let (name, pattern) = match expr.kind {
                                                        expand::ExpressionKind::Error(_) => return None,
                                                        expand::ExpressionKind::Name(name) => (
                                                            name,
                                                            Pattern {
                                                                span: expr.span,
                                                                kind: PatternKind::Name(name),
                                                            },
                                                        ),
                                                        _ => {
                                                            self.add_error(
                                                                "invalid pattern in destructuring pattern", vec![Note::primary(
                                                                    expr.span,
                                                                    "expected name here",
                                                                )],
                                                            );

                                                            return None;
                                                        }
                                                    };

                                                    Some((name, pattern))
                                                }),
                                            )),
                                            expand::ExpressionKind::Name(name) => {
                                                Some(Box::new(std::iter::once((
                                                    name,
                                                    Pattern {
                                                        span: statement.expr.span,
                                                        kind: PatternKind::Name(name),
                                                    },
                                                ))))
                                            }
                                            expand::ExpressionKind::Assign(left, right) => {
                                                let name = match left.kind {
                                                    expand::ExpressionKind::Name(name) => name,
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
                                                        expr.span,
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
                    expand::ExpressionKind::List(exprs) => {
                        let mut exprs = exprs.into_iter();

                        let name = match exprs.next() {
                            Some(expr) => expr,
                            None => {
                                self.add_error(
                                    "expected variant name", vec![Note::primary(
                                        expr.span,
                                        "only variants may be used in this kind of pattern",
                                    )],
                                );

                                return PatternKind::error(self);
                            },
                        };

                        let name_span = name.span;

                        let name = match name.kind {
                            expand::ExpressionKind::Name(name) => name,
                            _ => {
                                self.add_error(
                                    "expected variant name", vec![Note::primary(
                                        expr.span,
                                        "only variants may be used in this kind of pattern",
                                    )],
                                );

                                return PatternKind::error(self);
                            }
                        };

                        let rest = exprs.map(|expr| self.build_pattern(expr)).collect();

                        PatternKind::Variant((name_span, name), rest)
                    }
                    expand::ExpressionKind::Empty => PatternKind::Tuple(Vec::new()),
                    expand::ExpressionKind::Number(number) => PatternKind::Number(number),
                    expand::ExpressionKind::Text(text) => PatternKind::Text(text),
                    expand::ExpressionKind::Annotate(expr, ty) => {
                        let inner = self.build_pattern(*expr);
                        let ty = self.build_type_annotation(*ty);

                        PatternKind::Annotate(Box::new(inner), ty)
                    }
                    expand::ExpressionKind::Or(lhs, rhs) => PatternKind::Or(
                        Box::new(self.build_pattern(*lhs)),
                        Box::new(self.build_pattern(*rhs)),
                    ),
                    expand::ExpressionKind::Where(pattern, condition) => PatternKind::Where(
                        Box::new(self.build_pattern(*pattern)),
                        Box::new(self.build_expression(*condition)),
                    ),
                    expand::ExpressionKind::Tuple(exprs) => PatternKind::Tuple(
                        exprs
                            .into_iter()
                            .map(|expr| self.build_pattern(expr))
                            .collect(),
                    ),
                    _ => {
                        self.add_error(
                            "expected pattern",
                            vec![Note::primary(
                                expr.span,
                                "values may not appear on the left-hand side of a variable assignment",
                            )],
                        );

                        PatternKind::error(self)
                    }
                }
            })(),
        }
    }

    fn build_type_annotation(&self, expr: expand::Expression) -> TypeAnnotation {
        TypeAnnotation {
            span: expr.span,
            kind: (|| match expr.kind {
                expand::ExpressionKind::Empty => TypeAnnotationKind::Tuple(Vec::new()),
                expand::ExpressionKind::Underscore => TypeAnnotationKind::Placeholder,
                expand::ExpressionKind::Name(name) => TypeAnnotationKind::Named(name, Vec::new()),
                expand::ExpressionKind::Function(input, output) => TypeAnnotationKind::Function(
                    Box::new(self.build_type_annotation(*input)),
                    Box::new(self.build_type_annotation(*output)),
                ),
                expand::ExpressionKind::List(exprs) => {
                    let mut exprs = exprs.into_iter();

                    let ty = match exprs.next() {
                        Some(ty) => ty,
                        None => return TypeAnnotationKind::Tuple(Vec::new()),
                    };

                    let name = match ty.kind {
                        expand::ExpressionKind::Name(name) => name,
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
                        exprs.map(|expr| self.build_type_annotation(expr)).collect(),
                    )
                }
                expand::ExpressionKind::Tuple(exprs) => TypeAnnotationKind::Tuple(
                    exprs
                        .into_iter()
                        .map(|expr| self.build_type_annotation(expr))
                        .collect(),
                ),
                _ => {
                    self.add_error(
                        "expected type",
                        vec![Note::primary(expr.span, "this is not a type")],
                    );

                    TypeAnnotationKind::Error
                }
            })(),
        }
    }

    fn build_type_function(
        &self,
        lhs: expand::Expression,
    ) -> Option<(Vec<TypeParameter>, Vec<Bound>)> {
        macro_rules! build_parameter_list {
            ($tys:expr) => {
                $tys.into_iter()
                    .map(|expr| match expr.kind {
                        expand::ExpressionKind::Error(_) => None,
                        expand::ExpressionKind::Name(name) => Some(TypeParameter {
                            span: expr.span,
                            name,
                        }),
                        _ => {
                            self.add_error(
                                "expected type parameter",
                                vec![Note::primary(expr.span, "try removing this")],
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
                        expand::ExpressionKind::Error(_) => return None,
                        expand::ExpressionKind::Name(name) => name,
                        _ => {
                            self.add_error(
                                "expected trait name in `where` clause",
                                vec![Note::primary(trait_name.span, "try adding a name here")],
                            );

                            return None;
                        }
                    };

                    let parameters = list.map(|expr| self.build_type_annotation(expr)).collect();

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
            expand::ExpressionKind::Error(_) => None,
            expand::ExpressionKind::Name(name) => Some((
                vec![TypeParameter {
                    span: lhs.span,
                    name,
                }],
                Vec::new(),
            )),
            expand::ExpressionKind::List(tys) => Some((build_parameter_list!(tys)?, Vec::new())),
            expand::ExpressionKind::Where(lhs, bounds) => {
                let tys = match lhs.kind {
                    expand::ExpressionKind::Error(_) => return None,
                    expand::ExpressionKind::Name(name) => vec![TypeParameter {
                        span: lhs.span,
                        name,
                    }],
                    expand::ExpressionKind::List(tys) => build_parameter_list!(tys)?,
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
                        expand::ExpressionKind::List(bounds) => {
                            if bounds
                                .iter()
                                .any(|bound| matches!(bound.kind, expand::ExpressionKind::List(_)))
                            {
                                // The bounds clause matches the pattern `(T A) (T B) ...`
                                bounds
                                    .into_iter()
                                    .map(|bound| match bound.kind {
                                        expand::ExpressionKind::Name(trait_name) => Some(Bound {
                                            span: bound.span,
                                            trait_span: bound.span,
                                            trait_name,
                                            parameters: Vec::new(),
                                        }),
                                        expand::ExpressionKind::List(list) => build_bound!(bound.span, list),
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
                        expand::ExpressionKind::Name(trait_name) => {
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
                let span = field.expr.span;

                match field.expr.kind {
                    expand::ExpressionKind::Error(_) => None,
                    expand::ExpressionKind::Name(name) => Some(Field {
                        span,
                        attributes: field.attributes,
                        name,
                        kind: FieldKind::Variant(Vec::new()),
                    }),
                    expand::ExpressionKind::List(exprs) => {
                        let mut exprs = exprs.into_iter();

                        let name = exprs.next().unwrap();
                        let name_span = name.span;
                        let name = match name.kind {
                            expand::ExpressionKind::Name(name) => name,
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

                        let tys = exprs
                            .map(|expr| self.build_type_annotation(expr))
                            .collect::<Vec<_>>();

                        Some(Field {
                            span: name_span,
                            attributes: field.attributes,
                            name,
                            kind: FieldKind::Variant(tys),
                        })
                    }
                    expand::ExpressionKind::Annotate(name, ty) => {
                        let name = match name.kind {
                            expand::ExpressionKind::Name(name) => name,
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
