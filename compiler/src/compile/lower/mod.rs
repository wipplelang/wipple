mod builtins;
mod operators;

use crate::{
    diagnostics::*,
    helpers::{ConstantId, InternedString, OperatorId, TypeId, TypeParameterId, VariableId},
    parser::{self, Span},
    Compiler, FilePath, Loader,
};
use builtins::load_builtins;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub complete: bool,
    pub declarations: Declarations,
    pub exported: HashMap<InternedString, ScopeValue>,
    pub block: Vec<Expression>,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: HashMap<TypeId, Declaration<TypeId, Type>>,
    pub type_parameters: HashMap<TypeParameterId, Declaration<TypeParameterId, ()>>,
    pub operators: HashMap<OperatorId, Declaration<OperatorId, Operator>>,
    pub constants: HashMap<ConstantId, Declaration<ConstantId, Constant>>,
    pub variables: HashMap<VariableId, Declaration<VariableId, ()>>,
}

#[derive(Debug, Clone)]
pub enum Declaration<Id, T> {
    Local(DeclarationKind<T>),
    Dependency(DeclarationKind<Id>),
    Builtin(DeclarationKind<T>),
}

#[derive(Debug, Clone)]
pub struct DeclarationKind<T> {
    pub name: InternedString,
    pub span: Span,
    pub value: T,
}

impl<Id: Eq + Hash, T> Declaration<Id, T> {
    pub fn name(&self) -> InternedString {
        match self {
            Declaration::Local(decl) => decl.name,
            Declaration::Dependency(decl) => decl.name,
            Declaration::Builtin(decl) => decl.name,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Declaration::Local(decl) => decl.span,
            Declaration::Dependency(decl) => decl.span,
            Declaration::Builtin(decl) => decl.span,
        }
    }
}

pub trait CopyDeclaration {
    /// Whether or not the declaration should be copied when `use`d
    const COPY: bool;
}

impl CopyDeclaration for Declaration<TypeId, Type> {
    const COPY: bool = false;
}

impl CopyDeclaration for Declaration<TypeParameterId, ()> {
    const COPY: bool = false;
}

impl CopyDeclaration for Declaration<OperatorId, Operator> {
    const COPY: bool = true;
}

impl CopyDeclaration for Declaration<ConstantId, Constant> {
    const COPY: bool = false;
}

impl CopyDeclaration for Declaration<VariableId, ()> {
    const COPY: bool = false;
}

impl<Id: Eq + Hash, T> Declaration<Id, T>
where
    Self: CopyDeclaration,
{
    pub fn should_copy(&self) -> bool {
        Self::COPY
    }
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
    Builtin(BuiltinType),
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
pub enum BuiltinType {
    Number,
    Text,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Operator {
    pub precedence: OperatorPrecedence,
    pub body: ConstantId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[repr(u8)]
pub enum OperatorPrecedence {
    Assignment = 9,
    Function = 8,
    Field = 7,
    Annotation = 6,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub parameters: Vec<TypeParameter>,
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
    Number(Decimal),
    Block(Vec<Expression>, HashMap<InternedString, ScopeValue>),
    Call(Box<Expression>, Box<Expression>),
    Function(Box<Expression>, HashSet<VariableId>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
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
    Parameter(TypeParameterId),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParameter {
    pub span: Span,
    pub id: TypeParameterId,
}

impl<L: Loader> Compiler<L> {
    pub fn lower(&mut self, file: parser::File, dependencies: Vec<&File>) -> File {
        let scope = Scope::default();

        // TODO: Handle file attributes

        let mut info = Info {
            declarations: Default::default(),
        };

        load_builtins(&scope, &mut info);

        for dependency in dependencies {
            macro_rules! merge_dependency {
                ($($kind:ident),* $(,)?) => {
                    $(
                        for (&id, decl) in &dependency.declarations.$kind {
                            let decl = if decl.should_copy() {
                                decl.clone()
                            } else {
                                Declaration::Dependency(DeclarationKind {
                                    name: decl.name(),
                                    span: decl.span(),
                                    value: id,
                                })
                            };

                            info.declarations.$kind.insert(id, decl);
                        }
                    )*
                };
            }

            merge_dependency!(types, type_parameters, operators, constants, variables);

            scope
                .values
                .borrow_mut()
                .extend(dependency.exported.clone());
        }

        let block = file
            .statements
            .into_iter()
            .filter_map(|statement| self.lower_statement(statement, &scope, &mut info))
            .collect();

        let mut all_constants_initialized = true;
        for constant in info.declarations.constants.values() {
            if let Declaration::Local(decl) | Declaration::Builtin(decl) = constant {
                if decl.value.value.borrow().is_none() {
                    self.diagnostics.add(Diagnostic::error(
                        "uninitialized constant",
                        vec![Note::primary(
                            decl.span,
                            format!("`{}` is never initialized with a value", decl.name),
                        )],
                    ));

                    all_constants_initialized = false;
                }
            }
        }

        File {
            path: file.path,
            span: file.span,
            complete: all_constants_initialized,
            declarations: info.declarations,
            exported: scope.values.take(),
            block,
        }
    }
}

#[derive(Debug, Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    values: RefCell<HashMap<InternedString, ScopeValue>>,
    used_variables: Option<RefCell<HashSet<VariableId>>>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ScopeValue {
    Type(TypeId),
    TypeParameter(TypeParameterId),
    Operator(OperatorId),
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

struct Info {
    declarations: Declarations,
}

impl<L: Loader> Compiler<L> {
    fn lower_block(
        &mut self,
        statements: Vec<parser::Statement>,
        scope: &Scope,
        info: &mut Info,
    ) -> (Vec<Expression>, HashMap<InternedString, ScopeValue>) {
        let scope = Scope {
            parent: Some(scope),
            ..Default::default()
        };

        let statements = statements
            .into_iter()
            .filter_map(|statement| self.lower_statement(statement, &scope, info))
            .collect();

        (statements, scope.values.into_inner())
    }

    fn lower_statement(
        &mut self,
        statement: parser::Statement,
        scope: &Scope,
        info: &mut Info,
    ) -> Option<Expression> {
        let defined = |name| scope.values.borrow().contains_key(&name);

        match statement.kind {
            parser::StatementKind::Type((span, name), ty) => {
                if defined(name) {
                    self.diagnostics.add(Diagnostic::error(
                        format!("`{name}` is already defined"),
                        vec![Note::primary(span, "try assigning to a different name")],
                    ));

                    return None;
                }

                if !ty.parameters.is_empty() {
                    self.diagnostics.add(Diagnostic::error(
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
                            TypeKind::Alias(self.lower_type_annotation(alias, scope, info))
                        }
                        parser::TypeKind::Structure(fields) => {
                            let mut field_tys = Vec::with_capacity(fields.len());
                            let mut field_names = HashMap::with_capacity(fields.len());
                            for (index, field) in fields.into_iter().enumerate() {
                                field_tys.push(TypeField {
                                    ty: self.lower_type_annotation(field.ty, scope, info),
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
                                        .map(|ty| self.lower_type_annotation(ty, scope, info))
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
                    Declaration::Local(DeclarationKind {
                        name,
                        span,
                        value: ty,
                    }),
                );

                None
            }
            parser::StatementKind::Constant((span, name), declaration) => {
                if defined(name) {
                    self.diagnostics.add(Diagnostic::error(
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
                            let id = TypeParameterId::new();

                            info.declarations.type_parameters.insert(
                                id,
                                Declaration::Local(DeclarationKind {
                                    name: parameter.name,
                                    span: parameter.span,
                                    value: (),
                                }),
                            );

                            scope
                                .values
                                .borrow_mut()
                                .insert(parameter.name, ScopeValue::TypeParameter(id));

                            TypeParameter {
                                span: parameter.span,
                                id,
                            }
                        })
                        .collect();

                    Constant {
                        parameters,
                        ty: self.lower_type_annotation(declaration.ty, &scope, info),
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
                    Declaration::Local(DeclarationKind {
                        name,
                        span,
                        value: constant,
                    }),
                );

                None
            }
            parser::StatementKind::Implementation(_) => None, // TODO
            parser::StatementKind::Assign(pattern, expr) => match &pattern.kind {
                parser::PatternKind::Name(name) => {
                    let mut associated_constant = None;

                    match scope.values.borrow().get(name).cloned() {
                        Some(
                            ScopeValue::Type(_)
                            | ScopeValue::TypeParameter(_)
                            | ScopeValue::Operator(_),
                        ) => {
                            self.diagnostics.add(Diagnostic::error(
                                format!("`{name}` is already defined"),
                                vec![Note::primary(
                                    statement.span,
                                    "try assigning to a different name",
                                )],
                            ));

                            return None;
                        }
                        Some(ScopeValue::Constant(id)) => {
                            let c = match info.declarations.constants.get(&id).unwrap() {
                                Declaration::Local(decl) | Declaration::Builtin(decl) => {
                                    decl.value.value.clone()
                                }
                                Declaration::Dependency(decl) => {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!("constant `{name}` is defined in another file"),
                                        vec![
                                            Note::primary(
                                                statement.span,
                                                "constants must be initialized in the same file in which they are declared",
                                            ),
                                            Note::secondary(
                                                decl.span,
                                                "declaration here",
                                            ),
                                        ],
                                    ));

                                    return None;
                                }
                            };

                            if let Some(associated_constant) = c.borrow().as_ref() {
                                self.diagnostics.add(Diagnostic::error(
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

                    let value = self.lower_expr(expr, scope, info);

                    if let Some(associated_constant) = associated_constant {
                        // TODO: Check that expression doesn't refer to local variables
                        associated_constant.replace(Some(value));
                        None
                    } else {
                        let id = VariableId::new();
                        scope
                            .values
                            .borrow_mut()
                            .insert(*name, ScopeValue::Variable(id));

                        info.declarations.variables.insert(
                            id,
                            Declaration::Local(DeclarationKind {
                                name: *name,
                                span: pattern.span,
                                value: (),
                            }),
                        );

                        Some(Expression {
                            span: statement.span,
                            kind: ExpressionKind::Initialize(id, Box::new(value)),
                        })
                    }
                }
                parser::PatternKind::Wildcard => Some(self.lower_expr(expr, scope, info)),
            },
            parser::StatementKind::Expression(expr) => Some(self.lower_expr(expr, scope, info)),
        }
    }

    fn lower_expr(
        &mut self,
        expr: parser::Expression,
        scope: &Scope,
        info: &mut Info,
    ) -> Expression {
        let kind = match expr.kind {
            parser::ExpressionKind::Unit => ExpressionKind::Unit,
            parser::ExpressionKind::Text(text) => ExpressionKind::Text(text),
            parser::ExpressionKind::Number(number) => ExpressionKind::Number(number),
            parser::ExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
            parser::ExpressionKind::Name(name) => {
                match self.resolve_value(expr.span, name, scope, info) {
                    Some(value) => value,
                    None => {
                        self.diagnostics.add(Diagnostic::error(
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

                let (block, declarations) = self.lower_block(statements, &scope, info);

                ExpressionKind::Block(block, declarations)
            }
            parser::ExpressionKind::List(exprs) => self.lower_operators(exprs, scope, info).kind,
            parser::ExpressionKind::Function(input, body) => {
                let scope = Scope {
                    parent: Some(scope),
                    used_variables: Some(Default::default()),
                    ..Default::default()
                };

                // Desugar function input pattern matching
                let input_span = input.span;
                let initialization = self.lower_statement(
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

                let body = self.lower_expr(*body, &scope, info);

                let block = initialization.into_iter().chain(Some(body)).collect();

                ExpressionKind::Function(
                    Box::new(Expression {
                        span: expr.span,
                        kind: ExpressionKind::Block(block, scope.values.into_inner()),
                    }),
                    scope.used_variables.unwrap().take(),
                )
            }
            parser::ExpressionKind::When(_, _) => return Expression::error(expr.span), // TODO
            parser::ExpressionKind::External(namespace, identifier, inputs) => {
                let inputs = inputs
                    .into_iter()
                    .map(|expr| self.lower_expr(expr, scope, info))
                    .collect();

                ExpressionKind::External(namespace, identifier, inputs)
            }
            parser::ExpressionKind::Annotate(expr, ty) => ExpressionKind::Annotate(
                Box::new(self.lower_expr(*expr, scope, info)),
                self.lower_type_annotation(ty, scope, info),
            ),
        };

        Expression {
            span: expr.span,
            kind,
        }
    }

    fn lower_type_annotation(
        &mut self,
        ty: parser::TypeAnnotation,
        scope: &Scope,
        info: &mut Info,
    ) -> TypeAnnotation {
        let kind = match ty.kind {
            parser::TypeAnnotationKind::Placeholder => TypeAnnotationKind::Placeholder,
            parser::TypeAnnotationKind::Unit => TypeAnnotationKind::Unit,
            parser::TypeAnnotationKind::Named(name, parameters) => {
                match self.resolve_type(name, scope) {
                    Some(Ok(ty)) => {
                        let parameters = parameters
                            .into_iter()
                            .map(|parameter| self.lower_type_annotation(parameter, scope, info))
                            .collect();

                        TypeAnnotationKind::Named(ty, parameters)
                    }
                    Some(Err(param)) => {
                        if !parameters.is_empty() {
                            self.diagnostics.add(Diagnostic::error(
                                "type parameters cannot have parameters themselves",
                                vec![Note::primary(
                                    ty.span,
                                    format!(
                                        "try writing `{}` on its own, with no parameters",
                                        name
                                    ),
                                )],
                            ));
                        }

                        TypeAnnotationKind::Parameter(param)
                    }
                    None => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot find type `{}`", name),
                            vec![Note::primary(ty.span, "no such type")],
                        ));

                        return TypeAnnotation::error(ty.span);
                    }
                }
            }
            parser::TypeAnnotationKind::Function(input, output) => TypeAnnotationKind::Function(
                Box::new(self.lower_type_annotation(*input, scope, info)),
                Box::new(self.lower_type_annotation(*output, scope, info)),
            ),
        };

        TypeAnnotation {
            span: ty.span,
            kind,
        }
    }

    fn resolve_value(
        &mut self,
        span: Span,
        name: InternedString,
        scope: &Scope,
        info: &mut Info,
    ) -> Option<ExpressionKind> {
        match scope.get(name) {
            Some(ScopeValue::Type(id)) => {
                fn resolve<L: Loader>(
                    compiler: &mut Compiler<L>,
                    span: Span,
                    id: TypeId,
                    info: &mut Info,
                ) -> Option<ExpressionKind> {
                    match info.declarations.types.get(&id).unwrap() {
                        Declaration::Local(decl) | Declaration::Builtin(decl) => {
                            match decl.value.kind {
                                TypeKind::Marker => Some(ExpressionKind::Marker(id)),
                                TypeKind::Alias(TypeAnnotation {
                                    kind: TypeAnnotationKind::Named(alias_id, _),
                                    ..
                                }) => {
                                    // TODO: Maybe perform this check earlier?
                                    if alias_id == id {
                                        compiler.diagnostics.add(Diagnostic::error(
                                            "cannot instantiate recursive type",
                                            vec![Note::primary(
                                                span,
                                                "this type references itself and cannot be constructed",
                                            )],
                                        ));

                                        Some(ExpressionKind::Error)
                                    } else {
                                        resolve(compiler, span, alias_id, info)
                                    }
                                }
                                TypeKind::Builtin(_) => {
                                    compiler.diagnostics.add(Diagnostic::error(
                                        "cannot use builtin type as value",
                                        vec![Note::primary(span, "try using a literal instead")],
                                    ));

                                    Some(ExpressionKind::Error)
                                }
                                _ => {
                                    compiler.diagnostics.add(Diagnostic::error(
                                        "cannot use type as value",
                                        vec![Note::primary(span, "try instantiating the type")],
                                    ));

                                    Some(ExpressionKind::Error)
                                }
                            }
                        }
                        Declaration::Dependency(_) => Some(ExpressionKind::Marker(id)),
                    }
                }

                resolve(self, span, id, info)
            }
            Some(ScopeValue::TypeParameter(_)) => {
                self.diagnostics.add(Diagnostic::error(
                    "cannot use type parameter as value",
                    vec![Note::primary(
                        span,
                        "type parameters cannot be instantiated because the actual type is not known here",
                    )],
                ));

                Some(ExpressionKind::Error)
            }
            Some(ScopeValue::Operator(_)) => {
                self.diagnostics.add(Diagnostic::error(
                    "cannot use operator as value",
                    vec![Note::primary(
                        span,
                        "try adding inputs to the left and right sides of this operator",
                    )],
                ));

                Some(ExpressionKind::Error)
            }
            Some(ScopeValue::Constant(id)) => Some(ExpressionKind::Constant(id)),
            Some(ScopeValue::Variable(id)) => Some(ExpressionKind::Variable(id)),
            None => None,
        }
    }

    fn resolve_type(
        &mut self,
        name: InternedString,
        scope: &Scope,
    ) -> Option<Result<TypeId, TypeParameterId>> {
        scope.get(name).and_then(|value| match value {
            ScopeValue::Type(id) => Some(Ok(id)),
            ScopeValue::TypeParameter(id) => Some(Err(id)),
            _ => None,
        })
    }
}
