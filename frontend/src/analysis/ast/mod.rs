use crate::{
    analysis::expand,
    diagnostics::*,
    helpers::{Backtrace, InternedString},
    parse::Span,
    Compiler, FilePath, ScopeId, TemplateId,
};
use std::collections::{BTreeMap, HashSet};

pub use expand::{FileAttributes, StatementAttributes, SyntaxDeclarationAttributes, TypeParameter};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub attributes: FileAttributes,
    pub syntax_declarations: BTreeMap<TemplateId, SyntaxDeclaration>,
    pub root_scope: ScopeId,
    pub scopes: BTreeMap<ScopeId, (Option<Span>, Option<ScopeId>)>,
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
    Use((Span, ScopeId, InternedString)),
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
    pub parameters: Option<(ScopeId, Vec<TypeParameter>, Vec<Bound>)>,
    pub trait_span: Span,
    pub trait_scope: ScopeId,
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
    Named(ScopeId, InternedString, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
    Tuple(Vec<TypeAnnotation>),
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

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub parameters: Option<Vec<TypeParameter>>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub parameters: Option<Vec<TypeParameter>>,
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
    pub parameters: Option<(ScopeId, Vec<TypeParameter>, Vec<Bound>)>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
    pub trait_span: Span,
    pub trait_scope: ScopeId,
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
            root_scope: file.root_scope,
            scopes: file.scopes,
            statements: file
                .statements
                .into_iter()
                .filter_map(|expr| self.build_statement(expr, file.root_scope))
                .collect(),
        }
    }

    fn build_syntax_declarations(
        &self,
        declarations: expand::Declarations,
    ) -> BTreeMap<TemplateId, SyntaxDeclaration> {
        let mut syntax_declarations = BTreeMap::new();

        for (id, operator) in declarations.operators {
            syntax_declarations.insert(
                id,
                SyntaxDeclaration {
                    name: operator.name,
                    span: operator.span,
                    uses: operator.uses,
                    operator: true,
                    attributes: operator.attributes,
                },
            );
        }

        for (id, syntax) in declarations.syntaxes {
            syntax_declarations.insert(
                id,
                SyntaxDeclaration {
                    name: syntax.name,
                    span: syntax.span,
                    uses: syntax.uses,
                    operator: false,
                    attributes: syntax.attributes,
                },
            );
        }

        syntax_declarations
    }

    fn build_statement(
        &self,
        statement: expand::Statement,
        file_scope: ScopeId,
    ) -> Option<Statement> {
        Some(Statement {
            span: statement.span,
            attributes: statement.attributes,
            kind: (|| {
                Some(match statement.expr.kind {
                    expand::ExpressionKind::EmptySideEffect => return None,
                    expand::ExpressionKind::AssignToPattern(pattern, value) => {
                        self.build_pattern_assignment(pattern, *value, file_scope)?
                    }
                    expand::ExpressionKind::Assign(pattern, value) => {
                        self.build_assignment(*pattern, *value, file_scope)?
                    }
                    expand::ExpressionKind::Annotate(expr, ty) => {
                        let (span, name) = match expr.kind {
                            expand::ExpressionKind::Name(_, name) => (expr.span, name),
                            _ => {
                                return Some(StatementKind::Expression(
                                    self.build_expression(
                                        expand::Expression {
                                            span: statement.expr.span,
                                            kind: expand::ExpressionKind::Annotate(expr, ty),
                                        },
                                        file_scope,
                                    )
                                    .kind,
                                ));
                            }
                        };

                        let (parameters, ty) = match ty.kind {
                            expand::ExpressionKind::TypeFunction(
                                type_function_scope,
                                (parameters, bounds),
                                ty,
                            ) => (
                                Some((
                                    type_function_scope.unwrap(),
                                    parameters,
                                    self.build_bounds(bounds),
                                )),
                                self.build_type_annotation(*ty),
                            ),
                            _ => (None, self.build_type_annotation(*ty)),
                        };

                        StatementKind::Declaration(Declaration::Constant(
                            (span, name),
                            ConstantDeclaration {
                                name,
                                parameters,
                                ty,
                            },
                        ))
                    }
                    expand::ExpressionKind::Use(expr) => {
                        let (scope, name) = match expr.kind {
                            expand::ExpressionKind::Name(scope, name) => (scope, name),
                            _ => {
                                self.add_error(
                                    "`use` expects a path to a file or a name of a type",
                                    vec![Note::primary(expr.span, "expected a path or type here")],
                                );

                                return Some(StatementKind::Expression(ExpressionKind::error(
                                    self,
                                )));
                            }
                        };

                        StatementKind::Use((expr.span, scope.unwrap(), name))
                    }
                    expand::ExpressionKind::TypeFunction(scope, (params, bounds), rhs) => {
                        let scope = scope.unwrap();

                        let bounds = self.build_bounds(bounds);

                        match rhs.kind {
                            expand::ExpressionKind::Instance(tr) => {
                                match self.build_instance(
                                    Some((scope, params, bounds)),
                                    *tr,
                                    None,
                                    file_scope,
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
                    expand::ExpressionKind::Instance(tr) => {
                        match self.build_instance(None, *tr, None, file_scope) {
                            Some(instance) => {
                                StatementKind::Declaration(Declaration::Instance(instance))
                            }
                            None => StatementKind::Expression(ExpressionKind::error(self)),
                        }
                    }
                    _ => StatementKind::Expression(
                        self.build_expression(statement.expr, file_scope).kind,
                    ),
                })
            })()?,
        })
    }

    fn build_assignment(
        &self,
        pattern: expand::Expression,
        value: expand::Expression,
        file_scope: ScopeId,
    ) -> Option<StatementKind> {
        Some(match pattern.kind {
            expand::ExpressionKind::TypeFunction(scope, (params, bounds), rhs) => {
                let scope = scope.unwrap();

                let bounds = self.build_bounds(bounds);

                match rhs.kind {
                    expand::ExpressionKind::Instance(tr) => {
                        match self.build_instance(
                            Some((scope, params, bounds)),
                            *tr,
                            Some(value),
                            file_scope,
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
            expand::ExpressionKind::Instance(tr) => {
                match self.build_instance(None, *tr, Some(value), file_scope) {
                    Some(instance) => StatementKind::Declaration(Declaration::Instance(instance)),
                    None => StatementKind::Expression(ExpressionKind::error(self)),
                }
            }
            _ => {
                let pattern = self
                    .parse_pattern_from_expander_expr(pattern, true)
                    .unwrap_or_else(|expr| expand::Pattern {
                        span: expr.span,
                        kind: expand::PatternKind::error(self),
                    });

                self.build_pattern_assignment(pattern, value, file_scope)
                    .unwrap_or_else(|| StatementKind::Expression(ExpressionKind::error(self)))
            }
        })
    }

    fn build_pattern_assignment(
        &self,
        pattern: expand::Pattern,
        value: expand::Expression,
        file_scope: ScopeId,
    ) -> Option<StatementKind> {
        let pattern = self.build_pattern(pattern, file_scope);

        Some(match value.kind {
            expand::ExpressionKind::AssignToPattern(_, _)
            | expand::ExpressionKind::Assign(_, _) => {
                self.add_error(
                    "expected expression, found assignment",
                    vec![Note::primary(
                        value.span,
                        "try moving this variable assignment onto its own line",
                    )],
                );

                return None;
            }
            expand::ExpressionKind::Type(fields) => {
                let name = match pattern.kind {
                    PatternKind::Name(name) => name,
                    _ => {
                        self.add_error(
                            "type declaration must be assigned to a name",
                            vec![Note::primary(value.span, "try providing a name here")],
                        );

                        return None;
                    }
                };

                match self.build_type_declaration(value.span, fields.map(|expr| *expr)) {
                    Some(kind) => StatementKind::Declaration(Declaration::Type(
                        (pattern.span, name),
                        TypeDeclaration {
                            parameters: None,
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
                            vec![Note::primary(value.span, "try providing a name here")],
                        );

                        return Some(StatementKind::Expression(ExpressionKind::error(self)));
                    }
                };

                StatementKind::Declaration(Declaration::Trait(
                    (pattern.span, name),
                    TraitDeclaration {
                        parameters: None,
                        ty: ty.map(|ty| self.build_type_annotation(*ty)),
                    },
                ))
            }
            expand::ExpressionKind::TypeFunction(_, (parameters, bounds), expr) => {
                let name = match pattern.kind {
                    PatternKind::Name(name) => name,
                    _ => {
                        self.add_error(
                            "type or trait declaration must be assigned to a name",
                            vec![Note::primary(value.span, "try providing a name here")],
                        );

                        return Some(StatementKind::Expression(ExpressionKind::error(self)));
                    }
                };

                let bounds = self.build_bounds(bounds);

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

                        match self.build_type_declaration(expr.span, fields.map(|expr| *expr)) {
                            Some(kind) => StatementKind::Declaration(Declaration::Type(
                                (pattern.span, name),
                                TypeDeclaration {
                                    parameters: Some(parameters),
                                    kind,
                                },
                            )),
                            None => StatementKind::Expression(ExpressionKind::error(self)),
                        }
                    }
                    expand::ExpressionKind::Trait(ty) => {
                        check_bounds("traits");

                        StatementKind::Declaration(Declaration::Trait(
                            (pattern.span, name),
                            TraitDeclaration {
                                parameters: Some(parameters),
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
            _ => StatementKind::Assign(pattern, self.build_expression(value, file_scope)),
        })
    }

    fn build_instance(
        &self,
        parameters: Option<(ScopeId, Vec<TypeParameter>, Vec<Bound>)>,
        tr: expand::Expression,
        value: Option<expand::Expression>,
        file_scope: ScopeId,
    ) -> Option<Instance> {
        let annotation = self.build_type_annotation(tr);

        let (trait_scope, trait_name, trait_parameters) = match annotation.kind {
            TypeAnnotationKind::Named(scope, name, params) => (scope, name, params),
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

        let value = value.map(|value| self.build_expression(value, file_scope));

        Some(Instance {
            parameters,
            trait_span: annotation.span,
            trait_scope,
            trait_name,
            trait_parameters,
            value,
        })
    }

    fn build_expression(&self, expr: expand::Expression, file_scope: ScopeId) -> Expression {
        Expression {
            span: expr.span,
            kind: (|| match expr.kind {
                expand::ExpressionKind::Error(trace) => ExpressionKind::Error(trace),
                expand::ExpressionKind::Name(scope, name) => {
                    ExpressionKind::Name(scope.unwrap(), name)
                }
                expand::ExpressionKind::Text(text) => ExpressionKind::Text(text),
                expand::ExpressionKind::Number(number) => ExpressionKind::Number(number),
                expand::ExpressionKind::List(exprs) => {
                    let mut exprs = exprs.into_iter();
                    match exprs.next() {
                        Some(func) => ExpressionKind::Call(
                            Box::new(self.build_expression(func, file_scope)),
                            exprs
                                .map(|expr| self.build_expression(expr, file_scope))
                                .collect(),
                        ),
                        None => ExpressionKind::Tuple(Vec::new()),
                    }
                }
                expand::ExpressionKind::Block(scope, statements) => ExpressionKind::Block(
                    scope.unwrap(),
                    statements
                        .into_iter()
                        .filter_map(|expr| self.build_statement(expr, file_scope))
                        .collect(),
                ),
                expand::ExpressionKind::Function(Some((scope, input, body)), _) => {
                    ExpressionKind::Function(
                        scope.unwrap(),
                        self.build_pattern(input, file_scope),
                        Box::new(self.build_expression(*body, file_scope)),
                    )
                }
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
                            .map(|expr| self.build_expression(expr, file_scope))
                            .collect(),
                    )
                }
                expand::ExpressionKind::Annotate(value, ty) => ExpressionKind::Annotate(
                    Box::new(self.build_expression(*value, file_scope)),
                    self.build_type_annotation(*ty),
                ),
                expand::ExpressionKind::When(input, scope, block) => {
                    let input = self.build_expression(*input, file_scope);

                    let block = match block.kind {
                        expand::ExpressionKind::Block(_, statements) => statements,
                        _ => {
                            self.add_error(
                                "expected a block in `when` expression",
                                vec![Note::primary(block.span, "try wrapping this in a block")],
                            );

                            return ExpressionKind::error(self);
                        }
                    };

                    let arms = block
                        .into_iter()
                        .filter_map(|statement| {
                            let expr = self.build_expression(statement.expr, file_scope);

                            match expr.kind {
                                ExpressionKind::Function(_, pattern, body) => Some(Arm {
                                    span: expr.span,
                                    attributes: Default::default(), // TODO: Handle attributes
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

                    ExpressionKind::When(Box::new(input), scope.unwrap(), arms)
                }
                expand::ExpressionKind::Or(lhs, rhs) => {
                    // Use the `Or` trait defined in the prelude

                    let rhs = self.build_expression(*rhs, file_scope);

                    ExpressionKind::Call(
                        Box::new(Expression {
                            span: Span::builtin(),
                            kind: ExpressionKind::Name(file_scope, InternedString::new("Or")),
                        }),
                        vec![
                            self.build_expression(*lhs, file_scope),
                            Expression {
                                span: rhs.span,
                                kind: ExpressionKind::Function(
                                    file_scope,
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
                        .map(|expr| self.build_expression(expr, file_scope))
                        .collect(),
                ),
                expand::ExpressionKind::End(value) => {
                    ExpressionKind::End(Box::new(self.build_expression(*value, file_scope)))
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

    fn build_pattern(&self, pattern: expand::Pattern, file_scope: ScopeId) -> Pattern {
        Pattern {
            span: pattern.span,
            kind: match pattern.kind {
                expand::PatternKind::Error(trace) => PatternKind::Error(trace),
                expand::PatternKind::Wildcard => PatternKind::Wildcard,
                expand::PatternKind::Number(number) => PatternKind::Number(number),
                expand::PatternKind::Text(text) => PatternKind::Text(text),
                expand::PatternKind::Name(name) => PatternKind::Name(name),
                expand::PatternKind::Destructure(fields) => PatternKind::Destructure(
                    fields
                        .into_iter()
                        .map(|(name, pattern)| (name, self.build_pattern(pattern, file_scope)))
                        .collect(),
                ),
                expand::PatternKind::Variant((span, name), values) => PatternKind::Variant(
                    (span, name),
                    values
                        .into_iter()
                        .map(|pattern| self.build_pattern(pattern, file_scope))
                        .collect(),
                ),
                expand::PatternKind::Annotate(pattern, ty) => PatternKind::Annotate(
                    Box::new(self.build_pattern(*pattern, file_scope)),
                    self.build_type_annotation(*ty),
                ),
                expand::PatternKind::Or(lhs, rhs) => PatternKind::Or(
                    Box::new(self.build_pattern(*lhs, file_scope)),
                    Box::new(self.build_pattern(*rhs, file_scope)),
                ),
                expand::PatternKind::Where(pattern, condition) => PatternKind::Where(
                    Box::new(self.build_pattern(*pattern, file_scope)),
                    Box::new(self.build_expression(*condition, file_scope)),
                ),
                expand::PatternKind::Tuple(patterns) => PatternKind::Tuple(
                    patterns
                        .into_iter()
                        .map(|pattern| self.build_pattern(pattern, file_scope))
                        .collect(),
                ),
            },
        }
    }

    fn build_type_annotation(&self, expr: expand::Expression) -> TypeAnnotation {
        TypeAnnotation {
            span: expr.span,
            kind: (|| match expr.kind {
                expand::ExpressionKind::Underscore => TypeAnnotationKind::Placeholder,
                expand::ExpressionKind::Name(scope, name) => {
                    TypeAnnotationKind::Named(scope.unwrap(), name, Vec::new())
                }
                expand::ExpressionKind::Function(_, (input, output)) => {
                    TypeAnnotationKind::Function(
                        Box::new(self.build_type_annotation(*input)),
                        Box::new(self.build_type_annotation(*output)),
                    )
                }
                expand::ExpressionKind::List(exprs) => {
                    let mut exprs = exprs.into_iter();

                    let ty = match exprs.next() {
                        Some(ty) => ty,
                        None => return TypeAnnotationKind::Tuple(Vec::new()),
                    };

                    let (scope, name) = match ty.kind {
                        expand::ExpressionKind::Name(scope, name) => (scope, name),
                        _ => {
                            self.add_error(
                                "expected type",
                                vec![Note::primary(ty.span, "this is not a type")],
                            );

                            return TypeAnnotationKind::Error;
                        }
                    };

                    TypeAnnotationKind::Named(
                        scope.unwrap(),
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

    fn build_bounds(&self, bounds: Vec<expand::Bound>) -> Vec<Bound> {
        bounds
            .into_iter()
            .map(|bound| Bound {
                span: bound.span,
                trait_span: bound.trait_span,
                trait_scope: bound.trait_scope,
                trait_name: bound.trait_name,
                parameters: bound
                    .parameters
                    .into_iter()
                    .map(|param| self.build_type_annotation(param))
                    .collect(),
            })
            .collect()
    }

    fn build_type_declaration(
        &self,
        span: Span,
        fields: Option<expand::Expression>,
    ) -> Option<TypeKind> {
        let fields = match fields {
            Some(fields) => fields,
            None => return Some(TypeKind::Marker),
        };

        let fields = match fields.kind {
            expand::ExpressionKind::Block(_, statements) => statements,
            _ => {
                self.add_error(
                    "expected a block in `type` declaration",
                    vec![Note::primary(fields.span, "try wrapping this in a block")],
                );

                return None;
            }
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
                    expand::ExpressionKind::Name(_, name) => Some(Field {
                        span,
                        attributes: Default::default(), // TODO: Handle attributes
                        name,
                        kind: FieldKind::Variant(Vec::new()),
                    }),
                    expand::ExpressionKind::List(exprs) => {
                        let mut exprs = exprs.into_iter();

                        let name = exprs.next().unwrap();
                        let name_span = name.span;
                        let name = match name.kind {
                            expand::ExpressionKind::Name(_, name) => name,
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
                            attributes: Default::default(), // TODO: Handle attributes
                            name,
                            kind: FieldKind::Variant(tys),
                        })
                    }
                    expand::ExpressionKind::Annotate(name, ty) => {
                        let name = match name.kind {
                            expand::ExpressionKind::Name(_, name) => name,
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
                            attributes: Default::default(), // TODO: Handle attributes
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
