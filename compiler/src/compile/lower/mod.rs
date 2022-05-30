mod builtins;

use crate::{
    compile::ast, diagnostics::*, helpers::InternedString, parse::Span, Compiler, FilePath,
    GenericConstantId, Loader, TemplateId, TraitId, TypeId, TypeParameterId, VariableId,
};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap},
    hash::Hash,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct File {
    pub span: Span,
    pub complete: bool,
    pub declarations: Declarations,
    pub exported: HashMap<InternedString, ScopeValue>,
    pub block: Vec<Expression>,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: BTreeMap<TypeId, Declaration<Type>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<()>>,
    pub traits: BTreeMap<TraitId, Declaration<Trait>>,
    pub builtin_types: BTreeMap<BuiltinType, Declaration<()>>,
    pub constants: BTreeMap<GenericConstantId, Declaration<Constant>>,
    pub instances: BTreeMap<GenericConstantId, Declaration<Instance>>,
    pub variables: BTreeMap<VariableId, Declaration<()>>,
}

#[derive(Debug, Clone)]
pub struct Declaration<T> {
    pub name: InternedString,
    pub span: Span,
    pub value: T,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub params: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Marker,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum BuiltinType {
    Unit,
    Number,
    Text,
    List,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub parameters: Vec<TypeParameter>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Rc<RefCell<Option<Expression>>>,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
    pub tr: TraitId,
    pub parameters: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub tr: TraitId,
    pub parameters: Vec<TypeAnnotation>,
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
    Marker(TypeId),
    Constant(GenericConstantId),
    Trait(TraitId),
    Variable(VariableId),
    Text(InternedString),
    Number(Decimal),
    Block(Vec<Expression>, HashMap<InternedString, ScopeValue>),
    Call(Box<Expression>, Box<Expression>),
    Function(Box<Expression>, Vec<(VariableId, Span)>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
    Initialize(VariableId, Box<Expression>),
    FunctionInput,
    Member(Box<Expression>, InternedString),
    Instantiate(TypeId, Vec<(InternedString, Expression)>),
    ListLiteral(Vec<Expression>),
    Variant(TypeId, usize, Vec<Expression>),
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeAnnotation {
    pub span: Span,
    pub kind: TypeAnnotationKind,
}

impl PartialEq for TypeAnnotation {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for TypeAnnotation {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeAnnotationKind {
    Error,
    Placeholder,
    Named(TypeId, Vec<TypeAnnotation>),
    Parameter(TypeParameterId, Vec<TypeAnnotation>),
    Builtin(BuiltinType, Vec<TypeAnnotation>),
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
    pub name: InternedString,
    pub id: TypeParameterId,
}

impl<L: Loader> Compiler<L> {
    pub fn lower(&mut self, file: ast::File, dependencies: Vec<Rc<File>>) -> File {
        let scope = Scope::default();

        // TODO: Handle file attributes

        let mut info = Info::default();

        self.load_builtins(&scope, &mut info);

        for dependency in dependencies {
            macro_rules! merge_dependency {
                ($($kind:ident),* $(,)?) => {
                    $(
                        info.declarations
                            .$kind
                            .extend(dependency.declarations.$kind.clone());
                    )*
                };
            }

            merge_dependency!(
                types,
                type_parameters,
                traits,
                builtin_types,
                constants,
                instances,
                variables,
            );

            scope
                .values
                .borrow_mut()
                .extend(dependency.exported.clone());
        }

        let file_scope = scope.child();

        let block = file
            .statements
            .into_iter()
            .flat_map(|statement| self.lower_statement(statement, &file_scope, &mut info))
            .collect();

        let mut complete = true;
        for constant in info.declarations.constants.values() {
            if constant.value.value.borrow().as_ref().is_none() {
                self.diagnostics.add(Diagnostic::error(
                    "uninitialized constant",
                    vec![Note::primary(
                        constant.span,
                        format!("`{}` is never initialized with a value", constant.name),
                    )],
                ));

                complete = false;
            }
        }

        File {
            span: file.span,
            complete,
            declarations: info.declarations,
            exported: file_scope.values.take(),
            block,
        }
    }
}

#[derive(Debug, Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    values: RefCell<HashMap<InternedString, ScopeValue>>,
    used_variables: Option<RefCell<Vec<(VariableId, Span)>>>,
    function_inputs: RefCell<BTreeSet<VariableId>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ScopeValue {
    Type(TypeId),
    BuiltinType(BuiltinType),
    Trait(TraitId),
    TypeParameter(TypeParameterId),
    Operator(TemplateId),
    Constant(GenericConstantId),
    Variable(VariableId),
}

impl<'a> Scope<'a> {
    fn child(&'a self) -> Scope<'a> {
        Scope {
            parent: Some(self),
            ..Default::default()
        }
    }

    fn get(&'a self, name: InternedString, source_span: Span) -> Option<ScopeValue> {
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
                u.borrow_mut().push((id, source_span));
            }
        }

        result
    }

    fn variables_used_by_ancestors(&self) -> Vec<(VariableId, Span)> {
        let mut parent = self.parent;
        let mut used_variables = Vec::new();
        while let Some(scope) = parent {
            if let Some(u) = &scope.used_variables {
                used_variables.append(&mut u.clone().into_inner());
            }

            parent = scope.parent;
        }

        used_variables
    }
}

#[derive(Default)]
struct Info {
    declarations: Declarations,
}

impl<L: Loader> Compiler<L> {
    fn lower_block(
        &mut self,
        statements: Vec<ast::Statement>,
        scope: &Scope,
        info: &mut Info,
    ) -> (Vec<Expression>, HashMap<InternedString, ScopeValue>) {
        let scope = scope.child();

        let statements = statements
            .into_iter()
            .flat_map(|statement| self.lower_statement(statement, &scope, info))
            .collect();

        (statements, scope.values.into_inner())
    }

    fn lower_statement(
        &mut self,
        statement: ast::Statement,
        scope: &Scope,
        info: &mut Info,
    ) -> Vec<Expression> {
        let defined = |name| scope.values.borrow().contains_key(&name);

        match statement.kind {
            ast::StatementKind::Type((span, name), ty) => {
                if defined(name) {
                    self.diagnostics.add(Diagnostic::error(
                        format!("`{name}` is already defined"),
                        vec![Note::primary(span, "try assigning to a different name")],
                    ));

                    return vec![Expression::error(statement.span)];
                }

                let id = self.new_type_id();
                scope.values.borrow_mut().insert(name, ScopeValue::Type(id));

                let params = ty
                    .parameters
                    .into_iter()
                    .map(|param| {
                        let id = self.new_type_parameter_id();

                        scope
                            .values
                            .borrow_mut()
                            .insert(param.name, ScopeValue::TypeParameter(id));

                        info.declarations.type_parameters.insert(
                            id,
                            Declaration {
                                name: param.name,
                                span: param.span,
                                value: (),
                            },
                        );

                        TypeParameter {
                            span: param.span,
                            name: param.name,
                            id,
                        }
                    })
                    .collect();

                let bounds = ty
                    .bounds
                    .into_iter()
                    .flat_map(|bound| {
                        let tr = match scope.get(bound.trait_name, bound.trait_span) {
                            Some(value) => match value {
                                ScopeValue::Trait(tr) => tr,
                                _ => {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!("`{}` is not a trait", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "expected a trait here",
                                        )],
                                    ));

                                    return None;
                                }
                            },
                            None => {
                                self.diagnostics.add(Diagnostic::error(
                                    format!("cannot find `{}`", bound.trait_name),
                                    vec![Note::primary(
                                        bound.trait_span,
                                        "this name is not defined",
                                    )],
                                ));

                                return None;
                            }
                        };

                        Some(Bound {
                            span,
                            tr,
                            parameters: bound
                                .parameters
                                .into_iter()
                                .map(|param| self.lower_type_annotation(param, scope, info))
                                .collect(),
                        })
                    })
                    .collect();

                let ty = match ty.kind {
                    ast::TypeKind::Marker => Type {
                        kind: TypeKind::Marker,
                        params,
                        bounds,
                    },
                    ast::TypeKind::Structure(fields) => {
                        let mut field_tys = Vec::with_capacity(fields.len());
                        let mut field_names = HashMap::with_capacity(fields.len());
                        for (index, field) in fields.into_iter().enumerate() {
                            field_tys.push(TypeField {
                                ty: self.lower_type_annotation(field.ty, scope, info),
                            });

                            field_names.insert(field.name, index);
                        }

                        Type {
                            kind: TypeKind::Structure(field_tys, field_names),
                            params,
                            bounds,
                        }
                    }
                    ast::TypeKind::Enumeration(variants) => {
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

                        Type {
                            kind: TypeKind::Enumeration(variant_tys, variant_names),
                            params,
                            bounds,
                        }
                    }
                };

                info.declarations.types.insert(
                    id,
                    Declaration {
                        name,
                        span: statement.span,
                        value: ty,
                    },
                );

                Vec::new()
            }
            ast::StatementKind::Trait((span, name), declaration) => {
                if defined(name) {
                    self.diagnostics.add(Diagnostic::error(
                        format!("`{name}` is already defined"),
                        vec![Note::primary(span, "try assigning to a different name")],
                    ));

                    return vec![Expression::error(statement.span)];
                }

                let tr = {
                    let scope = scope.child();

                    let parameters = self.with_parameters(declaration.parameters, &scope, info);

                    Trait {
                        parameters,
                        ty: self.lower_type_annotation(declaration.ty, &scope, info),
                    }
                };

                let id = self.new_trait_id();
                scope
                    .values
                    .borrow_mut()
                    .insert(name, ScopeValue::Trait(id));

                info.declarations.traits.insert(
                    id,
                    Declaration {
                        name,
                        span: statement.span,
                        value: tr,
                    },
                );

                Vec::new()
            }
            ast::StatementKind::Constant((span, name), declaration) => {
                if defined(name) {
                    self.diagnostics.add(Diagnostic::error(
                        format!("`{name}` is already defined"),
                        vec![Note::primary(span, "try assigning to a different name")],
                    ));

                    return vec![Expression::error(statement.span)];
                }

                let constant = {
                    let scope = scope.child();

                    let parameters = self.with_parameters(declaration.parameters, &scope, info);

                    let bounds = declaration
                        .bounds
                        .into_iter()
                        .map(|bound| {
                            let tr = match scope.get(bound.trait_name, bound.trait_span) {
                                Some(ScopeValue::Trait(tr)) => tr,
                                Some(_) => {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!("`{}` is not a trait", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "expected a trait here",
                                        )],
                                    ));

                                    return Err(vec![Expression::error(statement.span)]);
                                }
                                None => {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!("cannot find `{}`", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "this name is not defined",
                                        )],
                                    ));

                                    return Err(vec![Expression::error(statement.span)]);
                                }
                            };

                            let parameters = bound
                                .parameters
                                .into_iter()
                                .map(|ty| self.lower_type_annotation(ty, &scope, info))
                                .collect();

                            Ok(Bound {
                                span: bound.span,
                                tr,
                                parameters,
                            })
                        })
                        .collect::<Result<_, _>>();

                    let bounds = match bounds {
                        Ok(bounds) => bounds,
                        Err(error) => return error,
                    };

                    Constant {
                        parameters,
                        bounds,
                        ty: self.lower_type_annotation(declaration.ty, &scope, info),
                        value: Default::default(),
                    }
                };

                let id = self.new_generic_constant_id();
                scope
                    .values
                    .borrow_mut()
                    .insert(name, ScopeValue::Constant(id));

                info.declarations.constants.insert(
                    id,
                    Declaration {
                        name,
                        span: statement.span,
                        value: constant,
                    },
                );

                Vec::new()
            }
            ast::StatementKind::Instance(decl) => {
                let tr = match scope.get(decl.trait_name, decl.trait_span) {
                    Some(ScopeValue::Trait(tr)) => tr,
                    Some(_) => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("`{}` is not a trait", decl.trait_name),
                            vec![Note::primary(decl.trait_span, "expected a trait here")],
                        ));

                        return vec![Expression::error(statement.span)];
                    }
                    None => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot find `{}`", decl.trait_name),
                            vec![Note::primary(decl.trait_span, "this name is not defined")],
                        ));

                        return vec![Expression::error(statement.span)];
                    }
                };

                let parameters = decl
                    .trait_parameters
                    .into_iter()
                    .map(|ty| self.lower_type_annotation(ty, scope, info))
                    .collect();

                if !decl.parameters.is_empty() {
                    self.diagnostics.add(Diagnostic::error(
                        "generic instance declarations are not currently supported",
                        vec![Note::primary(
                            Span::join(
                                decl.parameters.first().unwrap().span,
                                decl.parameters.last().unwrap().span,
                            ),
                            "try removing this",
                        )],
                    ));

                    return vec![Expression::error(statement.span)];
                }

                let value = self.lower_expr(decl.value, scope, info);
                self.validate_constant(&value);

                let instance = Instance {
                    tr,
                    parameters,
                    value,
                };

                let id = self.new_generic_constant_id();
                info.declarations.instances.insert(
                    id,
                    Declaration {
                        name: InternedString::new("instance"),
                        span: statement.span,
                        value: instance,
                    },
                );

                Vec::new()
            }
            ast::StatementKind::Assign(pattern, expr) => match &pattern.kind {
                ast::PatternKind::Error => vec![Expression::error(statement.span)],
                ast::PatternKind::Name(name) => {
                    let mut associated_constant = None;

                    match scope.values.borrow().get(name).cloned() {
                        Some(
                            ScopeValue::Type(_)
                            | ScopeValue::BuiltinType(_)
                            | ScopeValue::Trait(_)
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

                            return vec![Expression::error(statement.span)];
                        }
                        Some(ScopeValue::Constant(id)) => {
                            let decl = info.declarations.constants.get(&id).unwrap();
                            let (parameters, c) =
                                (decl.value.parameters.clone(), decl.value.value.clone());

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

                                return vec![Expression::error(statement.span)];
                            }

                            associated_constant = Some((parameters, c));
                        }
                        Some(ScopeValue::Variable(_)) | None => {}
                    }

                    if let Some((associated_parameters, associated_constant)) = associated_constant
                    {
                        let scope = scope.child();

                        for parameter in associated_parameters {
                            scope
                                .values
                                .borrow_mut()
                                .insert(parameter.name, ScopeValue::TypeParameter(parameter.id));
                        }

                        let value = self.lower_expr(expr, &scope, info);
                        self.validate_constant(&value);

                        associated_constant.replace(Some(value));
                        Vec::new()
                    } else {
                        let value = self.lower_expr(expr, scope, info);

                        let id = self.new_variable_id();
                        scope
                            .values
                            .borrow_mut()
                            .insert(*name, ScopeValue::Variable(id));

                        if matches!(value.kind, ExpressionKind::FunctionInput) {
                            scope.function_inputs.borrow_mut().insert(id);
                        }

                        info.declarations.variables.insert(
                            id,
                            Declaration {
                                name: *name,
                                span: pattern.span,
                                value: (),
                            },
                        );

                        vec![Expression {
                            span: statement.span,
                            kind: ExpressionKind::Initialize(id, Box::new(value)),
                        }]
                    }
                }
                ast::PatternKind::Destructure(names) => {
                    let value = self.lower_expr(expr, scope, info);
                    let value_span = value.span;

                    let id = self.new_variable_id();

                    let value_is_function_input =
                        matches!(value.kind, ExpressionKind::FunctionInput);

                    if value_is_function_input {
                        scope.function_inputs.borrow_mut().insert(id);
                    }

                    info.declarations.variables.insert(
                        id,
                        Declaration {
                            name: InternedString::new("<destructured>"),
                            span: pattern.span,
                            value: (),
                        },
                    );

                    let mut exprs = vec![Expression {
                        span: statement.span,
                        kind: ExpressionKind::Initialize(id, Box::new(value)),
                    }];

                    for (span, name) in names.iter().copied() {
                        let statement = ast::Statement {
                            span,
                            kind: ast::StatementKind::Assign(
                                ast::Pattern {
                                    span,
                                    kind: ast::PatternKind::Name(name),
                                },
                                ast::Expression {
                                    span: value_span,
                                    kind: ast::ExpressionKind::Member(
                                        Box::new(ast::Expression {
                                            span: value_span,
                                            kind: ast::ExpressionKind::Variable(id),
                                        }),
                                        name,
                                    ),
                                },
                            ),
                        };

                        let mut assignment = self.lower_statement(statement, scope, info);

                        if value_is_function_input {
                            // HACK: Extract the assigned variable IDs from the statement
                            for statement in &assignment {
                                if let ExpressionKind::Initialize(id, _) = &statement.kind {
                                    scope.function_inputs.borrow_mut().insert(*id);
                                }
                            }
                        }

                        exprs.append(&mut assignment);
                    }

                    exprs
                }
                ast::PatternKind::Wildcard => vec![self.lower_expr(expr, scope, info)],
            },
            ast::StatementKind::Expression(expr) => {
                vec![self.lower_expr(
                    ast::Expression {
                        span: statement.span,
                        kind: expr,
                    },
                    scope,
                    info,
                )]
            }
        }
    }

    fn validate_constant(&mut self, value: &Expression) {
        // Check that expression doesn't capture variables
        if let ExpressionKind::Function(_, captures) = &value.kind {
            if !captures.is_empty() {
                self.diagnostics.add(Diagnostic::error(
                    "expected constant value",
                    std::iter::once(Note::primary(
                        value.span,
                        "this value captures variables and cannot be used as a constant",
                    ))
                    .chain(
                        captures
                            .iter()
                            .map(|(_, span)| Note::secondary(*span, "captured variable")),
                    )
                    .collect(),
                ));
            }
        }
    }

    fn lower_expr(&mut self, expr: ast::Expression, scope: &Scope, info: &mut Info) -> Expression {
        macro_rules! function_call {
            ($function:expr, $inputs:expr) => {
                $inputs
                    .into_iter()
                    .fold(self.lower_expr($function, scope, info), |result, next| {
                        Expression {
                            span: Span::join(result.span, next.span),
                            kind: ExpressionKind::Call(
                                Box::new(result),
                                Box::new(self.lower_expr(next, scope, info)),
                            ),
                        }
                    })
            };
        }

        let kind = match expr.kind {
            ast::ExpressionKind::Error => ExpressionKind::Error,
            ast::ExpressionKind::Unit => ExpressionKind::Unit,
            ast::ExpressionKind::Text(text) => ExpressionKind::Text(text),
            ast::ExpressionKind::Number(number) => ExpressionKind::Number(number),
            ast::ExpressionKind::Name(name) => {
                match self.resolve_value(expr.span, name, scope, info) {
                    Some(value) => value,
                    None => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot find `{name}`"),
                            vec![Note::primary(expr.span, "this name is not defined")],
                        ));

                        return Expression::error(expr.span);
                    }
                }
            }
            ast::ExpressionKind::Block(statements) => {
                let scope = scope.child();
                let (block, declarations) = self.lower_block(statements, &scope, info);
                ExpressionKind::Block(block, declarations)
            }
            ast::ExpressionKind::Call(function, inputs) => match &function.kind {
                ast::ExpressionKind::Name(ty_name) => {
                    let input = inputs.first().unwrap();

                    match scope.get(*ty_name, function.span) {
                        Some(ScopeValue::Type(id)) => match &input.kind {
                            ast::ExpressionKind::Block(statements) => {
                                if inputs.len() > 1 {
                                    self.diagnostics.add(Diagnostic::error(
                                        "too many inputs in structure instantiation",
                                        vec![Note::primary(
                                            Span::join(
                                                inputs.first().unwrap().span,
                                                inputs.last().unwrap().span,
                                            ),
                                            "this structure requires a single block containing its fields",
                                        )],
                                    ));
                                }

                                let fields = statements
                                .iter()
                                .filter_map(|s| match &s.kind {
                                    ast::StatementKind::Assign(pattern, expr) => match &pattern.kind {
                                        ast::PatternKind::Name(name) => Some((*name, expr)),
                                        _ => None,
                                    },
                                    _ => {
                                        self.diagnostics.add(Diagnostic::error(
                                            "structure instantiation may not contain executable statements",
                                            vec![Note::primary(
                                                s.span,
                                                "try removing this",
                                            )]
                                        ));

                                        None
                                    }
                                })
                                .collect::<Vec<_>>()
                                .into_iter()
                                .map(|(name, value)| {
                                    (name, self.lower_expr(value.clone(), scope, info))
                                })
                                .collect();

                                let ty = &info.declarations.types.get(&id).unwrap().value;
                                if !matches!(ty.kind, TypeKind::Structure(_, _)) {
                                    self.diagnostics.add(Diagnostic::error(
                                        "only structures may be instantiated like this",
                                        vec![Note::primary(function.span, "not a structure")],
                                    ));

                                    return Expression::error(expr.span);
                                }

                                ExpressionKind::Instantiate(id, fields)
                            }
                            ast::ExpressionKind::Name(name) => {
                                let (variant_types, variants) = match info
                                    .declarations
                                    .types
                                    .get(&id)
                                    .unwrap()
                                    .value
                                    .kind
                                    .clone()
                                {
                                    TypeKind::Enumeration(types, variants) => (types, variants),
                                    _ => {
                                        self.diagnostics.add(Diagnostic::error(
                                            "only enumerations may be instantiated like this",
                                            vec![Note::primary(
                                                function.span,
                                                "not an enumeration",
                                            )],
                                        ));

                                        return Expression::error(expr.span);
                                    }
                                };

                                let index = match variants.get(name) {
                                    Some(index) => *index,
                                    None => {
                                        self.diagnostics.add(Diagnostic::error(
                                            format!(
                                                "enumeration `{}` does not declare a variant named `{}`",
                                                ty_name,
                                                name
                                            ),
                                            vec![Note::primary(input.span, "no such variant")],
                                        ));

                                        return Expression::error(expr.span);
                                    }
                                };

                                let inputs = inputs
                                    .iter()
                                    .skip(1)
                                    .map(|expr| self.lower_expr(expr.clone(), scope, info))
                                    .collect::<Vec<_>>();

                                let expected_input_count = variant_types[index].values.len();

                                if inputs.len() != expected_input_count {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!(
                                            "variant expects {} inputs, but only {} were given",
                                            expected_input_count,
                                            inputs.len(),
                                        ),
                                        vec![Note::primary(
                                            Span::join(inputs[2].span, inputs.last().unwrap().span),
                                            if expected_input_count > inputs.len() {
                                                "try removing some of these values"
                                            } else {
                                                "try adding some values"
                                            },
                                        )],
                                    ));

                                    return Expression::error(expr.span);
                                }

                                ExpressionKind::Variant(id, index, inputs)
                            }
                            _ => function_call!(*function, inputs).kind,
                        },
                        Some(ScopeValue::TypeParameter(_)) => {
                            self.diagnostics.add(Diagnostic::error(
                                "cannot instantiate type parameter",
                                vec![Note::primary(
                                    function.span,
                                    "the actual type this represents is not known here",
                                )],
                            ));

                            ExpressionKind::Error
                        }
                        Some(ScopeValue::BuiltinType(_)) => {
                            self.diagnostics.add(Diagnostic::error(
                                "cannot instantiate builtin type",
                                vec![Note::primary(function.span, "try usng a literal instead")],
                            ));

                            ExpressionKind::Error
                        }
                        _ => function_call!(*function, inputs).kind,
                    }
                }
                _ => function_call!(*function, inputs).kind,
            },
            ast::ExpressionKind::Function(input, body) => {
                let scope = Scope {
                    used_variables: Some(Default::default()),
                    ..scope.child()
                };

                // Desugar function input pattern matching
                let input_span = input.span;
                let initialization = self.lower_statement(
                    ast::Statement {
                        span: input_span,
                        kind: ast::StatementKind::Assign(
                            input,
                            ast::Expression {
                                span: input_span,
                                kind: ast::ExpressionKind::FunctionInput,
                            },
                        ),
                    },
                    &scope,
                    info,
                );

                let body = self.lower_expr(*body, &scope, info);

                let block = initialization
                    .into_iter()
                    .chain(std::iter::once(body))
                    .collect();

                let captures = scope
                    .variables_used_by_ancestors()
                    .into_iter()
                    .chain(scope.used_variables.unwrap().take().into_iter().filter_map(
                        |(var, span)| {
                            (!scope.function_inputs.borrow().contains(&var)).then(|| (var, span))
                        },
                    ))
                    .collect();

                ExpressionKind::Function(
                    Box::new(Expression {
                        span: expr.span,
                        kind: ExpressionKind::Block(block, scope.values.into_inner()),
                    }),
                    captures,
                )
            }
            ast::ExpressionKind::When(_, _) => return Expression::error(expr.span), // TODO
            ast::ExpressionKind::External(namespace, identifier, inputs) => {
                ExpressionKind::External(
                    namespace,
                    identifier,
                    inputs
                        .into_iter()
                        .map(|expr| self.lower_expr(expr, scope, info))
                        .collect(),
                )
            }
            ast::ExpressionKind::Annotate(expr, ty) => ExpressionKind::Annotate(
                Box::new(self.lower_expr(*expr, scope, info)),
                self.lower_type_annotation(ty, scope, info),
            ),
            ast::ExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
            ast::ExpressionKind::Variable(id) => ExpressionKind::Variable(id),
            ast::ExpressionKind::Member(expr, name) => {
                ExpressionKind::Member(Box::new(self.lower_expr(*expr, scope, info)), name)
            }
            ast::ExpressionKind::ListLiteral(items) => ExpressionKind::ListLiteral(
                items
                    .into_iter()
                    .map(|expr| self.lower_expr(expr, scope, info))
                    .collect(),
            ),
        };

        Expression {
            span: expr.span,
            kind,
        }
    }

    fn lower_type_annotation(
        &mut self,
        ty: ast::TypeAnnotation,
        scope: &Scope,
        info: &mut Info,
    ) -> TypeAnnotation {
        let kind = match ty.kind {
            ast::TypeAnnotationKind::Error => TypeAnnotationKind::Error,
            ast::TypeAnnotationKind::Placeholder => TypeAnnotationKind::Placeholder,
            ast::TypeAnnotationKind::Unit => {
                TypeAnnotationKind::Builtin(BuiltinType::Unit, Vec::new())
            }
            ast::TypeAnnotationKind::Named(name, parameters) => {
                let parameters = parameters
                    .into_iter()
                    .map(|parameter| self.lower_type_annotation(parameter, scope, info))
                    .collect();

                match scope.get(name, ty.span) {
                    Some(ScopeValue::Type(ty)) => TypeAnnotationKind::Named(ty, parameters),
                    Some(ScopeValue::TypeParameter(param)) => {
                        TypeAnnotationKind::Parameter(param, parameters)
                    }
                    Some(ScopeValue::BuiltinType(builtin)) => {
                        TypeAnnotationKind::Builtin(builtin, parameters)
                    }
                    _ => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot find type `{}`", name),
                            vec![Note::primary(ty.span, "no such type")],
                        ));

                        return TypeAnnotation::error(ty.span);
                    }
                }
            }
            ast::TypeAnnotationKind::Function(input, output) => TypeAnnotationKind::Function(
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
        match scope.get(name, span) {
            Some(ScopeValue::Type(id)) => {
                match info.declarations.types.get(&id).unwrap().value.kind {
                    TypeKind::Marker => Some(ExpressionKind::Marker(id)),
                    _ => {
                        self.diagnostics.add(Diagnostic::error(
                            "cannot use type as value",
                            vec![Note::primary(span, "try instantiating the type")],
                        ));

                        Some(ExpressionKind::Error)
                    }
                }
            }
            Some(ScopeValue::BuiltinType(_)) => {
                self.diagnostics.add(Diagnostic::error(
                    "cannot use builtin type as value",
                    vec![Note::primary(span, "try using a literal instead")],
                ));

                Some(ExpressionKind::Error)
            }
            Some(ScopeValue::Trait(id)) => Some(ExpressionKind::Trait(id)),
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

    fn with_parameters(
        &mut self,
        parameters: Vec<ast::TypeParameter>,
        scope: &Scope,
        info: &mut Info,
    ) -> Vec<TypeParameter> {
        parameters
            .into_iter()
            .map(|parameter| {
                let id = self.new_type_parameter_id();

                info.declarations.type_parameters.insert(
                    id,
                    Declaration {
                        name: parameter.name,
                        span: parameter.span,
                        value: (),
                    },
                );

                scope
                    .values
                    .borrow_mut()
                    .insert(parameter.name, ScopeValue::TypeParameter(id));

                TypeParameter {
                    span: parameter.span,
                    name: parameter.name,
                    id,
                }
            })
            .collect()
    }
}
