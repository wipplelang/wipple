use crate::{
    diagnostics::*,
    helpers::{ConstantId, FileId, InternedString, TraitId, TypeId, VariableId},
    parser::{self, Span},
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct File {
    pub name: InternedString,
    pub span: Span,
    pub declarations: Declarations,
    pub block: Vec<Expression>,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: HashMap<TypeId, Declaration<Type>>,
    pub traits: HashMap<TraitId, Declaration<Trait>>,
    pub constants: HashMap<ConstantId, Declaration<Constant>>,
    pub variables: HashMap<VariableId, Declaration<()>>,
}

#[derive(Debug, Clone)]
pub struct Declaration<T> {
    pub name: InternedString,
    pub span: Span,
    pub value: T,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub parameters: Vec<TypeParameter>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Marker,
    Alias(TypeAnnotation),
    Structure(Vec<TypeField>, HashMap<InternedString, usize>),
    Enumeration(Vec<TypeVariant>, HashMap<InternedString, usize>),
}

#[derive(Debug, Clone)]
pub struct TypeField {
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct TypeVariant {
    pub values: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub parameters: HashMap<TypeId, Vec<TraitId>>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub parameters: HashMap<TypeId, Vec<TraitId>>,
    pub ty: TypeAnnotation,
    pub value: Rc<RefCell<Option<Expression>>>,
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
    Marker(TypeId),
    Constant(ConstantId),
    Variable(VariableId),
    Text(InternedString),
    Number(f64),
    Block(Vec<Expression>),
    Call(Box<Expression>, Box<Expression>),
    Function(Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    Annotate(Box<Expression>, TypeAnnotation),
    Initialize(VariableId, Box<Expression>),
    FunctionInput,
    // TODO: Instantiation
}

impl Expression {
    fn error(span: Span) -> Self {
        Expression {
            span,
            kind: ExpressionKind::Error,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Binding(VariableId),
    // TODO: Support complex paths (data fields, variants)
    Wildcard,
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
    Named(TypeId, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
}

impl TypeAnnotation {
    fn error(span: Span) -> Self {
        TypeAnnotation {
            span,
            kind: TypeAnnotationKind::Error,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub span: Span,
    pub kind: TypeParameterKind,
}

#[derive(Debug, Clone)]
pub enum TypeParameterKind {
    Named(TypeId),
    Constrained(Vec<TraitId>, TypeId),
}

#[derive(Debug, Clone)]
pub enum Path {
    Type(FileId, TypeId),
    Variable(FileId, VariableId),
    // Components are resolved during type checking
    Member(VariableId, Vec<parser::PathComponent>),
}

pub fn lower(file: parser::File, diagnostics: &mut Diagnostics) -> Option<File> {
    // TODO: Handle file attributes (ie. loading prelude)
    let scope = Scope::default();

    let mut declarations = Declarations::default();
    let mut info = Info {
        declarations: &mut declarations,
        diagnostics,
        used_types: Default::default(),
        used_traits: Default::default(),
        used_constants: Default::default(),
        used_variables: Default::default(),
    };

    let block = lower_block(file.statements, &scope, &mut info);

    let mut all_constants_initialized = true;
    for constant in info.declarations.constants.values() {
        if constant.value.value.borrow().is_none() {
            info.diagnostics.add(Diagnostic::error(
                "uninitialized constant",
                vec![Note::primary(
                    constant.span,
                    format!("`{}` is never initialized with a value", constant.name),
                )],
            ));

            all_constants_initialized = false;
        }
    }

    macro_rules! warn_unused {
        ($($x:ident => $used:ident,)*) => {
            $(
                for (id, declaration) in &info.declarations.$x {
                    if !info.$used.contains(id) {
                        info.diagnostics.add(Diagnostic::warning(
                            "unused variable",
                            vec![Note::primary(
                                declaration.span,
                                format!("`{}` is unused", declaration.name),
                            )],
                        ))
                    }
                }
            )*
        };
    }

    warn_unused!(
        types => used_types,
        traits => used_traits,
        constants => used_constants,
        variables => used_variables,
    );

    let file = File {
        name: file.name,
        span: file.span,
        declarations,
        block,
    };

    all_constants_initialized.then(|| file)
}

#[derive(Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    values: RefCell<HashMap<InternedString, ScopeValue>>,
    used_variables: Option<RefCell<HashSet<VariableId>>>,
}

#[derive(Debug, Clone, Copy)]
enum ScopeValue {
    Type(TypeId),
    Trait(TraitId),
    Constant(ConstantId),
    Variable(VariableId),
}

impl<'a> Scope<'a> {
    fn get(&'a self, name: InternedString) -> Option<ScopeValue> {
        let mut parent = Some(self);
        let mut result = None;
        let mut used_variables = Vec::new();

        while let Some(scope) = parent {
            if let Some(u) = &scope.used_variables {
                used_variables.push(u);
            }

            if let Some(value) = scope.values.borrow().get(&name).cloned() {
                result = Some(value);
                break;
            }

            parent = scope.parent;
        }

        if let Some(ScopeValue::Variable(id)) = result {
            for u in used_variables {
                u.borrow_mut().insert(id);
            }
        }

        result
    }
}

struct Info<'a> {
    declarations: &'a mut Declarations,
    diagnostics: &'a mut Diagnostics,
    used_types: HashSet<TypeId>,
    used_traits: HashSet<TraitId>,
    used_constants: HashSet<ConstantId>,
    used_variables: HashSet<VariableId>,
}

fn lower_block(
    statements: Vec<parser::Statement>,
    scope: &Scope,
    info: &mut Info,
) -> Vec<Expression> {
    let scope = Scope {
        parent: Some(scope),
        ..Default::default()
    };

    statements
        .into_iter()
        .filter_map(|statement| lower_statement(statement, &scope, info))
        .collect()
}

fn lower_statement(
    statement: parser::Statement,
    scope: &Scope,
    info: &mut Info,
) -> Option<Expression> {
    let defined = |name| scope.values.borrow().contains_key(&name);

    match statement.kind {
        parser::StatementKind::Type((span, name), ty) => {
            if defined(name) {
                info.diagnostics.add(Diagnostic::error(
                    format!("`{name}` is already defined"),
                    vec![Note::primary(span, "try assigning to a different name")],
                ));

                return None;
            }

            if !ty.parameters.is_empty() {
                info.diagnostics.add(Diagnostic::error(
                    "type parameters are currently unsupported",
                    vec![Note::primary(
                        Span::join(
                            ty.parameters.first().unwrap().span,
                            ty.parameters.last().unwrap().span,
                        ),
                        "try removing these parameters",
                    )],
                ));

                return None;
            }

            let id = TypeId::new();
            scope.values.borrow_mut().insert(name, ScopeValue::Type(id));

            let ty = Type {
                parameters: Vec::new(),
                kind: match ty.kind {
                    parser::TypeKind::Marker => TypeKind::Marker,
                    parser::TypeKind::Alias(alias) => {
                        TypeKind::Alias(lower_type_annotation(alias, scope, info))
                    }
                    parser::TypeKind::Structure(fields) => {
                        let mut field_tys = Vec::with_capacity(fields.len());
                        let mut field_names = HashMap::with_capacity(fields.len());
                        for (index, field) in fields.into_iter().enumerate() {
                            field_tys.push(TypeField {
                                ty: lower_type_annotation(field.ty, scope, info),
                            });

                            field_names.insert(field.name, index);
                        }

                        TypeKind::Structure(field_tys, field_names)
                    }
                    parser::TypeKind::Enumeration(variants) => {
                        let mut variant_tys = Vec::with_capacity(variants.len());
                        let mut variant_names = HashMap::with_capacity(variants.len());
                        for (index, variant) in variants.into_iter().enumerate() {
                            variant_tys.push(TypeVariant {
                                values: variant
                                    .values
                                    .into_iter()
                                    .map(|ty| lower_type_annotation(ty, scope, info))
                                    .collect(),
                            });

                            variant_names.insert(variant.name, index);
                        }

                        TypeKind::Enumeration(variant_tys, variant_names)
                    }
                },
            };

            info.declarations.types.insert(
                id,
                Declaration {
                    name,
                    span,
                    value: ty,
                },
            );

            None
        }
        parser::StatementKind::Trait((span, name), declaration) => {
            if defined(name) {
                info.diagnostics.add(Diagnostic::error(
                    format!("`{name}` is already defined"),
                    vec![Note::primary(span, "try assigning to a different name")],
                ));

                return None;
            }

            let r#trait = {
                let scope = Scope {
                    parent: Some(scope),
                    ..Default::default()
                };

                let parameters = declaration
                    .parameters
                    .into_iter()
                    .map(|parameter| {
                        let name = parameter.name;
                        let parameter = lower_type_parameter(parameter, &scope, info)?;

                        let id = TypeId::new();
                        scope.values.borrow_mut().insert(name, ScopeValue::Type(id));

                        Some((id, parameter))
                    })
                    .collect::<Option<_>>()?;

                Trait {
                    parameters,
                    ty: lower_type_annotation(declaration.ty, &scope, info),
                }
            };

            let id = TraitId::new();
            scope
                .values
                .borrow_mut()
                .insert(name, ScopeValue::Trait(id));

            info.declarations.traits.insert(
                id,
                Declaration {
                    name,
                    span,
                    value: r#trait,
                },
            );

            None
        }
        parser::StatementKind::Constant((span, name), declaration) => {
            if defined(name) {
                info.diagnostics.add(Diagnostic::error(
                    format!("`{name}` is already defined"),
                    vec![Note::primary(span, "try assigning to a different name")],
                ));

                return Some(Expression::error(span));
            }

            let constant = {
                let scope = Scope {
                    parent: Some(scope),
                    ..Default::default()
                };

                let parameters = declaration
                    .parameters
                    .into_iter()
                    .map(|parameter| {
                        let name = parameter.name;
                        let parameter = lower_type_parameter(parameter, &scope, info)?;

                        let id = TypeId::new();
                        scope.values.borrow_mut().insert(name, ScopeValue::Type(id));

                        Some((id, parameter))
                    })
                    .collect::<Option<_>>()?;

                Constant {
                    parameters,
                    ty: lower_type_annotation(declaration.ty, &scope, info),
                    value: Default::default(),
                }
            };

            let id = ConstantId::new();
            scope
                .values
                .borrow_mut()
                .insert(name, ScopeValue::Constant(id));

            info.declarations.constants.insert(
                id,
                Declaration {
                    name,
                    span,
                    value: constant,
                },
            );

            None
        }
        parser::StatementKind::Implementation(_) => None, // TODO
        parser::StatementKind::Assign(pattern, expr) => match &pattern.kind {
            parser::PatternKind::Path(path) => {
                if !path.components.is_empty() {
                    info.diagnostics.add(Diagnostic::error(
                        "cannot assign to a path",
                        vec![Note::primary(expr.span, "try assigning to a name instead")],
                    ));

                    return None;
                }

                let name = path.base;
                let mut associated_constant = None;

                match scope.values.borrow().get(&name).cloned() {
                    Some(ScopeValue::Type(_) | ScopeValue::Trait(_)) => {
                        info.diagnostics.add(Diagnostic::error(
                            format!("`{name}` is already defined"),
                            vec![Note::primary(
                                statement.span,
                                "try assigning to a different name",
                            )],
                        ));

                        return None;
                    }
                    Some(ScopeValue::Constant(id)) => {
                        let c = info
                            .declarations
                            .constants
                            .get(&id)
                            .unwrap()
                            .value
                            .value
                            .clone();

                        if let Some(associated_constant) = c.borrow().as_ref() {
                            info.diagnostics.add(Diagnostic::error(
                                format!("constant `{name}` is already defined"),
                                vec![
                                    Note::primary(
                                        statement.span,
                                        "cannot assign to a constant more than once",
                                    ),
                                    Note::secondary(
                                        associated_constant.span,
                                        "first initialization",
                                    ),
                                ],
                            ));

                            return None;
                        }

                        associated_constant = Some(c);
                    }
                    Some(ScopeValue::Variable(_)) | None => {}
                }

                let value = lower_expr(expr, scope, info);

                if let Some(associated_constant) = associated_constant {
                    // TODO: Check that expression doesn't refer to local variables
                    associated_constant.replace(Some(value));
                    None
                } else {
                    let id = VariableId::new();
                    scope
                        .values
                        .borrow_mut()
                        .insert(name, ScopeValue::Variable(id));

                    info.declarations.variables.insert(
                        id,
                        Declaration {
                            name,
                            span: pattern.span,
                            value: (),
                        },
                    );

                    Some(Expression {
                        span: statement.span,
                        kind: ExpressionKind::Initialize(id, Box::new(value)),
                    })
                }
            }
            parser::PatternKind::Wildcard => Some(lower_expr(expr, scope, info)),
        },
        parser::StatementKind::Expression(expr) => Some(lower_expr(expr, scope, info)),
    }
}

fn lower_expr(expr: parser::Expression, scope: &Scope, info: &mut Info) -> Expression {
    let kind = match expr.kind {
        parser::ExpressionKind::Unit => ExpressionKind::Unit,
        parser::ExpressionKind::Text(text) => ExpressionKind::Text(text),
        parser::ExpressionKind::Number(number) => ExpressionKind::Number(number),
        parser::ExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
        parser::ExpressionKind::Path(path) => {
            if !path.components.is_empty() {
                info.diagnostics.add(Diagnostic::error(
                    "subpaths are currently unsupported",
                    vec![Note::primary(expr.span, "try simplifying this expression")],
                ));

                return Expression::error(expr.span);
            }

            let name = path.base;

            match resolve_value(expr.span, name, scope, info) {
                Some(value) => value,
                None => {
                    info.diagnostics.add(Diagnostic::error(
                        format!("cannot find variable `{name}`"),
                        vec![Note::primary(expr.span, "no such variable")],
                    ));

                    return Expression::error(expr.span);
                }
            }
        }
        parser::ExpressionKind::Block(statements) => {
            let scope = Scope {
                parent: Some(scope),
                ..Default::default()
            };

            let block = lower_block(statements, &scope, info);

            ExpressionKind::Block(block)
        }
        parser::ExpressionKind::Call(function, input) => {
            if let parser::ExpressionKind::Path(path) = &function.kind {
                if let Some(ScopeValue::Type(_ty)) = scope.values.borrow().get(&path.base) {
                    // TODO Handle instantiation (also for enum variants)

                    info.diagnostics.add(Diagnostic::error(
                        "instantiation is currently unsupported",
                        vec![Note::primary(function.span, "try removing this expression")],
                    ));

                    return Expression::error(expr.span);
                }
            }

            let function = lower_expr(*function, scope, info);
            let input = lower_expr(*input, scope, info);

            ExpressionKind::Call(Box::new(function), Box::new(input))
        }
        parser::ExpressionKind::Function(input, body) => {
            let scope = Scope {
                parent: Some(scope),
                used_variables: Some(Default::default()),
                ..Default::default()
            };

            // Desugar function input pattern matching
            let input_span = input.span;
            let initialization = lower_statement(
                parser::Statement {
                    span: input_span,
                    kind: parser::StatementKind::Assign(
                        input,
                        parser::Expression {
                            span: input_span,
                            kind: parser::ExpressionKind::FunctionInput,
                        },
                    ),
                },
                &scope,
                info,
            );

            let body = lower_expr(*body, &scope, info);

            let block = initialization.into_iter().chain(Some(body)).collect();

            ExpressionKind::Function(Box::new(Expression {
                span: expr.span,
                kind: ExpressionKind::Block(block),
            }))
        }
        parser::ExpressionKind::When(_, _) => return Expression::error(expr.span), // TODO
        parser::ExpressionKind::Annotate(expr, ty) => ExpressionKind::Annotate(
            Box::new(lower_expr(*expr, scope, info)),
            lower_type_annotation(ty, scope, info),
        ),
    };

    Expression {
        span: expr.span,
        kind,
    }
}

fn lower_type_annotation(
    ty: parser::TypeAnnotation,
    scope: &Scope,
    info: &mut Info,
) -> TypeAnnotation {
    let kind = match ty.kind {
        parser::TypeAnnotationKind::Placeholder => TypeAnnotationKind::Placeholder,
        parser::TypeAnnotationKind::Unit => TypeAnnotationKind::Unit,
        parser::TypeAnnotationKind::Path(path, parameters) => {
            if !path.components.is_empty() {
                info.diagnostics.add(Diagnostic::error(
                    "subpaths are currently unsupported",
                    vec![Note::primary(ty.span, "try simplifying this type")],
                ));

                return TypeAnnotation::error(ty.span);
            }

            let name = path.base;

            let ty = match resolve_type(name, scope) {
                Some(ty) => ty,
                None => {
                    info.diagnostics.add(Diagnostic::error(
                        format!("cannot find type `{}`", path.base),
                        vec![Note::primary(ty.span, "no such type")],
                    ));

                    return TypeAnnotation::error(ty.span);
                }
            };

            let parameters = parameters
                .into_iter()
                .map(|parameter| lower_type_annotation(parameter, scope, info))
                .collect();

            TypeAnnotationKind::Named(ty, parameters)
        }
        parser::TypeAnnotationKind::Function(input, output) => TypeAnnotationKind::Function(
            Box::new(lower_type_annotation(*input, scope, info)),
            Box::new(lower_type_annotation(*output, scope, info)),
        ),
    };

    TypeAnnotation {
        span: ty.span,
        kind,
    }
}

fn lower_type_parameter(
    parameter: parser::TypeParameter,
    scope: &Scope,
    info: &mut Info,
) -> Option<Vec<TraitId>> {
    parameter
        .constraints
        .into_iter()
        .map(|(span, constraint)| match scope.get(constraint) {
            Some(ScopeValue::Trait(id)) => {
                info.used_traits.insert(id);
                Some(id)
            }
            Some(_) => {
                info.diagnostics.add(Diagnostic::error(
                    "invalid constraint",
                    vec![Note::primary(span, "expected a trait")],
                ));

                None
            }
            None => {
                info.diagnostics.add(Diagnostic::error(
                    format!("cannot find trait `{constraint}`"),
                    vec![Note::primary(span, "no such trait")],
                ));

                None
            }
        })
        .collect()
}

fn resolve_value(
    span: Span,
    name: InternedString,
    scope: &Scope,
    info: &mut Info,
) -> Option<ExpressionKind> {
    match scope.get(name) {
        Some(ScopeValue::Type(id)) => {
            fn resolve(span: Span, id: TypeId, info: &mut Info) -> Option<ExpressionKind> {
                match info.declarations.types.get(&id).unwrap().value.kind {
                    TypeKind::Marker => {
                        info.used_types.insert(id);
                        Some(ExpressionKind::Marker(id))
                    }
                    TypeKind::Alias(TypeAnnotation {
                        kind: TypeAnnotationKind::Named(alias_id, _),
                        ..
                    }) => {
                        // TODO: Maybe perform this check earlier?
                        if alias_id == id {
                            info.diagnostics.add(Diagnostic::error(
                                "cannot instantiate recursive type",
                                vec![Note::primary(
                                    span,
                                    "this type references itself and cannot be constructed",
                                )],
                            ));

                            return Some(ExpressionKind::Error);
                        } else {
                            info.used_types.insert(id);
                            resolve(span, alias_id, info)
                        }
                    }
                    _ => {
                        info.diagnostics.add(Diagnostic::error(
                            "cannot use type as value",
                            vec![Note::primary(span, "try instantiating the type")],
                        ));

                        return Some(ExpressionKind::Error);
                    }
                }
            }

            resolve(span, id, info)
        }
        Some(ScopeValue::Trait(_)) => {
            info.diagnostics.add(Diagnostic::error(
                "cannot use trait as value",
                vec![Note::primary(
                    span,
                    "try instantiating a type implementing this trait instead",
                )],
            ));

            return Some(ExpressionKind::Error);
        }
        Some(ScopeValue::Constant(id)) => {
            info.used_constants.insert(id);
            Some(ExpressionKind::Constant(id))
        }
        Some(ScopeValue::Variable(id)) => {
            info.used_variables.insert(id);
            Some(ExpressionKind::Variable(id))
        }
        None => None,
    }
}

fn resolve_type(name: InternedString, scope: &Scope) -> Option<TypeId> {
    scope.get(name).and_then(|value| match value {
        ScopeValue::Type(id) => Some(id),
        _ => None,
    })
}
