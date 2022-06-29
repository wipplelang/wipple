mod builtins;

use crate::{
    compile::{ast, expand},
    diagnostics::*,
    helpers::InternedString,
    parse::Span,
    BuiltinTypeId, Compiler, GenericConstantId, TemplateId, TraitId, TypeId, TypeParameterId,
    VariableId,
};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    hash::Hash,
    mem,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct File {
    pub span: Span,
    pub declarations: Declarations,
    pub global_attributes: FileAttributes,
    pub exported: HashMap<InternedString, ScopeValue>,
    pub block: Vec<Expression>,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: BTreeMap<TypeId, Declaration<Type>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<()>>,
    pub traits: BTreeMap<TraitId, Declaration<Trait>>,
    pub builtin_types: BTreeMap<BuiltinTypeId, Declaration<BuiltinType>>,
    pub constants: BTreeMap<GenericConstantId, Declaration<Constant>>,
    pub instances: BTreeMap<GenericConstantId, Declaration<Instance>>,
    pub variables: BTreeMap<VariableId, Declaration<()>>,
}

#[derive(Debug, Clone)]
pub struct Declaration<T> {
    pub name: Option<InternedString>,
    pub span: Span,
    pub value: T,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DeclarationAttributes {
    pub help: Vec<InternedString>,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub params: Vec<TypeParameterId>,
    pub attributes: DeclarationAttributes,
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
    pub attributes: DeclarationAttributes,
}

#[derive(Debug, Clone)]
pub struct TypeVariant {
    pub constructor: GenericConstantId,
    pub tys: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct BuiltinType {
    pub kind: BuiltinTypeKind,
    pub attributes: DeclarationAttributes,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum BuiltinTypeKind {
    Never,
    Unit,
    Number,
    Boolean,
    Text,
    List,
    Mutable,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub parameters: Vec<TypeParameterId>,
    pub ty: TypeAnnotation,
    pub attributes: DeclarationAttributes,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Rc<RefCell<Option<Expression>>>,
    pub attributes: DeclarationAttributes,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
    pub tr: TraitId,
    pub parameters: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub params: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub tr: TraitId,
    pub trait_params: Vec<TypeAnnotation>,
    pub value: Expression,
}

#[derive(Debug, Clone, Default)]
pub struct FileAttributes {
    pub language_items: LanguageItems,
}

#[derive(Debug, Clone, Default)]
pub struct LanguageItems {
    pub boolean: Option<TypeId>,
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
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
    Initialize(Pattern, Box<Expression>),
    Instantiate(TypeId, Vec<(InternedString, Expression)>),
    ListLiteral(Vec<Expression>),
    Variant(TypeId, usize, Vec<Expression>),
    Return(Box<Expression>),
    Loop(Box<Expression>),
    Break(Box<Expression>),
    Continue,
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
    Error,
    Wildcard,
    Unit,
    Number(Decimal),
    Text(InternedString),
    Variable(VariableId),
    Destructure(HashMap<InternedString, Pattern>),
    Variant(TypeId, usize, Vec<Pattern>),
    Annotate(Box<Pattern>, TypeAnnotation),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Expression>),
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
    Named(TypeId, Vec<TypeAnnotation>),
    Parameter(TypeParameterId),
    Builtin(BuiltinTypeId, Vec<TypeAnnotation>),
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

impl<L> Compiler<'_, L> {
    pub fn lower(&mut self, file: ast::File, dependencies: Vec<Rc<File>>) -> File {
        let scope = Scope::root(ScopeKind::Block);

        let mut info = Info {
            declarations: Default::default(),
            attributes: Default::default(),
        };

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

            info.attributes.merge(&dependency.global_attributes);

            scope
                .values
                .borrow_mut()
                .extend(dependency.exported.clone());
        }

        let block = file
            .statements
            .into_iter()
            .flat_map(|statement| self.lower_statement(statement, &scope, &mut info))
            .collect();

        for constant in info.declarations.constants.values() {
            if constant.value.value.borrow().as_ref().is_none() {
                self.diagnostics.add(Diagnostic::error(
                    "uninitialized constant",
                    vec![Note::primary(
                        constant.span,
                        format!(
                            "`{}` is never initialized with a value",
                            constant.name.unwrap()
                        ),
                    )],
                ));
            }
        }

        File {
            span: file.span,
            declarations: info.declarations,
            global_attributes: info.attributes,
            exported: scope.values.take(),
            block,
        }
    }
}

impl FileAttributes {
    fn merge(&mut self, other: &Self) {
        self.language_items.merge(&other.language_items);
    }
}

impl LanguageItems {
    fn merge(&mut self, other: &Self) {
        if let Some(ty) = other.boolean {
            self.boolean = Some(ty);
        }
    }
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    kind: ScopeKind,
    parent: Option<&'a Scope<'a>>,
    values: RefCell<HashMap<InternedString, ScopeValue>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Block,
    Function,
    Loop,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ScopeValue {
    Type(TypeId),
    BuiltinType(BuiltinTypeId),
    Trait(TraitId),
    TypeParameter(TypeParameterId),
    Operator(TemplateId),
    Constant(GenericConstantId, Option<(TypeId, usize)>),
    Variable(VariableId),
}

impl<'a> Scope<'a> {
    fn root(kind: ScopeKind) -> Self {
        Scope {
            kind,
            parent: None,
            values: Default::default(),
        }
    }

    fn child(&'a self, kind: ScopeKind) -> Self {
        Scope {
            kind,
            parent: Some(self),
            values: Default::default(),
        }
    }

    fn get(&'a self, name: InternedString) -> Option<ScopeValue> {
        let mut parent = Some(self);
        let mut result = None;

        while let Some(scope) = parent {
            if let Some(value) = scope.values.borrow().get(&name).cloned() {
                result = Some(value);
                break;
            }

            parent = scope.parent;
        }

        result
    }

    fn is_within_function(&'a self) -> bool {
        let mut parent = Some(self);
        let mut is_within_function = false;
        while let Some(scope) = parent {
            if let ScopeKind::Function = scope.kind {
                is_within_function = true;
                break;
            }

            parent = scope.parent;
        }

        is_within_function
    }

    fn is_within_loop(&'a self) -> bool {
        let mut parent = Some(self);
        let mut is_within_loop = false;
        while let Some(scope) = parent {
            if let ScopeKind::Function = scope.kind {
                is_within_loop = true;
                break;
            }

            match scope.kind {
                ScopeKind::Block => {}
                ScopeKind::Function => break,
                ScopeKind::Loop => {
                    is_within_loop = true;
                    break;
                }
            }

            parent = scope.parent;
        }

        is_within_loop
    }
}

struct Info {
    declarations: Declarations,
    attributes: FileAttributes,
}

impl<L> Compiler<'_, L> {
    fn lower_block(
        &mut self,
        statements: Vec<ast::Statement>,
        scope: &Scope,
        info: &mut Info,
    ) -> (Vec<Expression>, HashMap<InternedString, ScopeValue>) {
        let scope = scope.child(ScopeKind::Block);

        let statements = statements
            .into_iter()
            .flat_map(|statement| self.lower_statement(statement, &scope, info))
            .collect();

        (statements, scope.values.into_inner())
    }

    fn lower_statement(
        &mut self,
        mut statement: ast::Statement,
        scope: &Scope,
        info: &mut Info,
    ) -> Option<Expression> {
        let (scope_value, expr) = match statement.kind {
            ast::StatementKind::Type((span, name), ty) => {
                let id = self.new_type_id();
                scope.values.borrow_mut().insert(name, ScopeValue::Type(id));

                let parameters = ty
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
                                name: Some(param.name),
                                span: param.span,
                                value: (),
                            },
                        );

                        id
                    })
                    .collect();

                if !ty.bounds.is_empty() {
                    self.diagnostics.add(Diagnostic::error(
                        "bounds are not allowed on types",
                        vec![Note::primary(
                            ty.bounds
                                .first()
                                .unwrap()
                                .span
                                .with_end(ty.bounds.last().unwrap().span.end),
                            "try moving these to the respective functions instead",
                        )],
                    ));
                }

                let ty = match ty.kind {
                    ast::TypeKind::Marker => Type {
                        kind: TypeKind::Marker,
                        params: parameters,
                        attributes: self.lower_decl_attributes(&mut statement.attributes),
                    },
                    ast::TypeKind::Structure(fields) => {
                        let mut field_tys = Vec::with_capacity(fields.len());
                        let mut field_names = HashMap::with_capacity(fields.len());
                        for (index, mut field) in fields.into_iter().enumerate() {
                            field_tys.push(TypeField {
                                ty: self.lower_type_annotation(field.ty, scope, info),
                                attributes: self.lower_decl_attributes(&mut field.attributes),
                            });

                            field_names.insert(field.name, index);
                        }

                        Type {
                            kind: TypeKind::Structure(field_tys, field_names),
                            params: parameters,
                            attributes: self.lower_decl_attributes(&mut statement.attributes),
                        }
                    }
                    ast::TypeKind::Enumeration(variants) => {
                        let mut variant_tys = Vec::with_capacity(variants.len());
                        let mut variant_names = HashMap::with_capacity(variants.len());
                        for (index, mut variant) in variants.into_iter().enumerate() {
                            let tys = variant
                                .values
                                .into_iter()
                                .map(|ty| self.lower_type_annotation(ty, scope, info))
                                .collect::<Vec<_>>();

                            let constructor_id = self.new_generic_constant_id();

                            let constructor_ty = tys.iter().fold(
                                TypeAnnotation {
                                    span: variant.span,
                                    kind: TypeAnnotationKind::Named(
                                        id,
                                        parameters
                                            .iter()
                                            .map(|param| TypeAnnotation {
                                                span,
                                                kind: TypeAnnotationKind::Parameter(*param),
                                            })
                                            .collect(),
                                    ),
                                },
                                |result, next| TypeAnnotation {
                                    span: variant.span,
                                    kind: TypeAnnotationKind::Function(
                                        Box::new(next.clone()),
                                        Box::new(result),
                                    ),
                                },
                            );

                            let variables = tys
                                .iter()
                                .map(|ty| {
                                    let var = self.new_variable_id();

                                    info.declarations.variables.insert(
                                        var,
                                        Declaration {
                                            name: Some(name),
                                            span: ty.span,
                                            value: (),
                                        },
                                    );

                                    (ty.span, var)
                                })
                                .collect::<Vec<_>>();

                            let result = Expression {
                                span: variant.span,
                                kind: ExpressionKind::Variant(
                                    id,
                                    index,
                                    variables
                                        .iter()
                                        .map(|(span, var)| Expression {
                                            span: *span,
                                            kind: ExpressionKind::Variable(*var),
                                        })
                                        .collect(),
                                ),
                            };

                            let constructor =
                                variables
                                    .iter()
                                    .fold(result, |result, (span, var)| Expression {
                                        span: variant.span,
                                        kind: ExpressionKind::Function(
                                            Pattern {
                                                span: *span,
                                                kind: PatternKind::Variable(*var),
                                            },
                                            Box::new(result),
                                        ),
                                    });

                            info.declarations.constants.insert(
                                constructor_id,
                                Declaration {
                                    name: Some(variant.name),
                                    span: variant.span,
                                    value: Constant {
                                        parameters: parameters.clone(),
                                        bounds: Vec::new(),
                                        ty: constructor_ty,
                                        value: Rc::new(RefCell::new(Some(constructor))),
                                        attributes: self
                                            .lower_decl_attributes(&mut variant.attributes),
                                    },
                                },
                            );

                            variant_tys.push(TypeVariant {
                                constructor: constructor_id,
                                tys,
                            });

                            variant_names.insert(variant.name, index);
                        }

                        Type {
                            kind: TypeKind::Enumeration(variant_tys, variant_names),
                            params: parameters,
                            attributes: self.lower_decl_attributes(&mut statement.attributes),
                        }
                    }
                };

                info.declarations.types.insert(
                    id,
                    Declaration {
                        name: Some(name),
                        span: statement.span,
                        value: ty,
                    },
                );

                (Some(ScopeValue::Type(id)), None)
            }
            ast::StatementKind::Trait((_span, name), declaration) => {
                let tr = {
                    let scope = scope.child(ScopeKind::Block);

                    let parameters = self.with_parameters(declaration.parameters, &scope, info);

                    Trait {
                        parameters,
                        ty: self.lower_type_annotation(declaration.ty, &scope, info),
                        attributes: self.lower_decl_attributes(&mut statement.attributes),
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
                        name: Some(name),
                        span: statement.span,
                        value: tr,
                    },
                );

                (Some(ScopeValue::Trait(id)), None)
            }
            ast::StatementKind::Constant((_span, name), declaration) => {
                let constant = {
                    let scope = scope.child(ScopeKind::Block);

                    let parameters = self.with_parameters(declaration.parameters, &scope, info);

                    let bounds = declaration
                        .bounds
                        .into_iter()
                        .map(|bound| {
                            let tr = match scope.get(bound.trait_name) {
                                Some(ScopeValue::Trait(tr)) => tr,
                                Some(_) => {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!("`{}` is not a trait", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "expected a trait here",
                                        )],
                                    ));

                                    return Err(Expression::error(statement.span));
                                }
                                None => {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!("cannot find `{}`", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "this name is not defined",
                                        )],
                                    ));

                                    return Err(Expression::error(statement.span));
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
                        Err(error) => return Some(error),
                    };

                    Constant {
                        parameters,
                        bounds,
                        ty: self.lower_type_annotation(declaration.ty, &scope, info),
                        value: Default::default(),
                        attributes: self.lower_decl_attributes(&mut statement.attributes),
                    }
                };

                let id = self.new_generic_constant_id();
                scope
                    .values
                    .borrow_mut()
                    .insert(name, ScopeValue::Constant(id, None));

                info.declarations.constants.insert(
                    id,
                    Declaration {
                        name: Some(name),
                        span: statement.span,
                        value: constant,
                    },
                );

                (Some(ScopeValue::Constant(id, None)), None)
            }
            ast::StatementKind::Instance(decl) => {
                let instance = {
                    let scope = scope.child(ScopeKind::Block);

                    let params = self.with_parameters(decl.parameters, &scope, info);

                    let bounds = decl
                        .bounds
                        .into_iter()
                        .map(|bound| {
                            let tr = match scope.get(bound.trait_name) {
                                Some(ScopeValue::Trait(tr)) => tr,
                                Some(_) => {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!("`{}` is not a trait", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "expected a trait here",
                                        )],
                                    ));

                                    return Err(Expression::error(statement.span));
                                }
                                None => {
                                    self.diagnostics.add(Diagnostic::error(
                                        format!("cannot find `{}`", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "this name is not defined",
                                        )],
                                    ));

                                    return Err(Expression::error(statement.span));
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
                        Err(error) => return Some(error),
                    };

                    let tr = match scope.get(decl.trait_name) {
                        Some(ScopeValue::Trait(tr)) => tr,
                        Some(_) => {
                            self.diagnostics.add(Diagnostic::error(
                                format!("`{}` is not a trait", decl.trait_name),
                                vec![Note::primary(decl.trait_span, "expected a trait here")],
                            ));

                            return Some(Expression::error(statement.span));
                        }
                        None => {
                            self.diagnostics.add(Diagnostic::error(
                                format!("cannot find `{}`", decl.trait_name),
                                vec![Note::primary(decl.trait_span, "this name is not defined")],
                            ));

                            return Some(Expression::error(statement.span));
                        }
                    };

                    let trait_params = decl
                        .trait_parameters
                        .into_iter()
                        .map(|ty| self.lower_type_annotation(ty, &scope, info))
                        .collect();

                    let value = self.lower_expr(decl.value, &scope, info);

                    Instance {
                        params,
                        bounds,
                        tr,
                        trait_params,
                        value,
                    }
                };

                let id = self.new_generic_constant_id();
                info.declarations.instances.insert(
                    id,
                    Declaration {
                        name: None,
                        span: statement.span,
                        value: instance,
                    },
                );

                (Some(ScopeValue::Constant(id, None)), None)
            }
            ast::StatementKind::Assign(pattern, expr) => (|| {
                macro_rules! assign_pattern {
                    () => {{
                        let value = self.lower_expr(expr, scope, info);
                        let pattern = self.lower_pattern(pattern, scope, info);

                        (
                            None,
                            Some(Expression {
                                span: statement.span,
                                kind: ExpressionKind::Initialize(pattern, Box::new(value)),
                            }),
                        )
                    }};
                }

                match &pattern.kind {
                    ast::PatternKind::Name(name) => {
                        let mut associated_constant = None;

                        let c = scope.values.borrow().get(name).cloned();
                        if let Some(ScopeValue::Constant(id, _)) = c {
                            let decl = info.declarations.constants.get(&id).unwrap();
                            let (parameters, c) =
                                (decl.value.parameters.clone(), decl.value.value.clone());

                            let constant_already_assigned = {
                                let c = c.borrow();
                                c.is_some()
                            };

                            if constant_already_assigned {
                                return assign_pattern!();
                            }

                            associated_constant = Some((parameters, c));
                        }

                        if let Some((associated_parameters, associated_constant)) =
                            associated_constant
                        {
                            let scope = scope.child(ScopeKind::Block);

                            for id in associated_parameters {
                                let parameter = info.declarations.type_parameters.get(&id).unwrap();

                                scope
                                    .values
                                    .borrow_mut()
                                    .insert(parameter.name.unwrap(), ScopeValue::TypeParameter(id));
                            }

                            let value = self.lower_expr(expr, &scope, info);
                            associated_constant.replace(Some(value));
                            (None, None)
                        } else {
                            assign_pattern!()
                        }
                    }
                    _ => assign_pattern!(),
                }
            })(),
            ast::StatementKind::Use((span, name)) => {
                let ty = match scope.get(name) {
                    Some(ScopeValue::Type(ty)) => ty,
                    Some(_) => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("`{}` is not a type", name),
                            vec![Note::primary(span, "expected a type here")],
                        ));

                        return Some(Expression::error(statement.span));
                    }
                    None => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot find `{}`", name),
                            vec![Note::primary(span, "this name is not defined")],
                        ));

                        return Some(Expression::error(statement.span));
                    }
                };

                let (constructors, names) =
                    match &info.declarations.types.get(&ty).unwrap().value.kind {
                        TypeKind::Enumeration(constructors, names) => (constructors, names),
                        _ => {
                            self.diagnostics.add(Diagnostic::error(
                                "only enumerations may be `use`d",
                                vec![Note::primary(
                                    span,
                                    format!("`{}` is not an enumeration", name),
                                )],
                            ));

                            return Some(Expression::error(statement.span));
                        }
                    };

                for (name, index) in names {
                    let variant = constructors[*index].constructor;

                    scope
                        .values
                        .borrow_mut()
                        .insert(*name, ScopeValue::Constant(variant, Some((ty, *index))));
                }

                (None, None)
            }
            ast::StatementKind::Expression(expr) => (
                None,
                Some(self.lower_expr(
                    ast::Expression {
                        span: statement.span,
                        kind: expr,
                    },
                    scope,
                    info,
                )),
            ),
        };

        (|| {
            if let Some(language_item) = statement.attributes.language_item {
                match language_item {
                    expand::LanguageItem::Boolean => {
                        let ty = match scope_value {
                            Some(ScopeValue::Type(id)) => id,
                            _ => {
                                self.diagnostics.add(Diagnostic::error(
                                    "`boolean` language item expects a type",
                                    vec![Note::primary(
                                        statement.span,
                                        "expected type declaration here",
                                    )],
                                ));

                                return;
                            }
                        };

                        if info.attributes.language_items.boolean.is_some() {
                            self.diagnostics.add(Diagnostic::error(
                                "`language` item may only be defined once",
                                vec![Note::primary(
                                    statement.span,
                                    "`language` item already defined elsewhere",
                                )],
                            ));

                            return;
                        }

                        info.attributes.language_items.boolean = Some(ty);
                    }
                }
            }
        })();

        expr
    }

    fn lower_decl_attributes(
        &mut self,
        statement_attributes: &mut ast::StatementAttributes,
    ) -> DeclarationAttributes {
        // TODO: Raise errors for misused attributes

        DeclarationAttributes {
            help: mem::take(&mut statement_attributes.help)
                .into_iter()
                .collect(),
        }
    }

    fn lower_expr(&mut self, expr: ast::Expression, scope: &Scope, info: &mut Info) -> Expression {
        macro_rules! function_call {
            ($function:expr, $inputs:expr) => {
                $inputs
                    .into_iter()
                    .fold($function, |result, next| Expression {
                        span: Span::join(result.span, next.span),
                        kind: ExpressionKind::Call(
                            Box::new(result),
                            Box::new(self.lower_expr(next, scope, info)),
                        ),
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
                let scope = scope.child(ScopeKind::Block);
                let (block, declarations) = self.lower_block(statements, &scope, info);
                ExpressionKind::Block(block, declarations)
            }
            ast::ExpressionKind::Call(function, inputs) => match &function.kind {
                ast::ExpressionKind::Name(ty_name) => {
                    let input = inputs.first().unwrap();

                    match scope.get(*ty_name) {
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
                                            _ => {
                                                self.diagnostics.add(Diagnostic::error(
                                                    "structure instantiation may not contain complex patterns",
                                                    vec![Note::primary(
                                                        s.span,
                                                        "try splitting this pattern into multiple names",
                                                    )]
                                                ));

                                                None
                                            },
                                        },
                                        // TODO: 'use' inside instantiation
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
                                let ty_decl = info.declarations.types.get(&id).unwrap();

                                let (variant_types, variants) = match &ty_decl.value.kind {
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

                                function_call!(
                                    Expression {
                                        span: expr.span,
                                        kind: ExpressionKind::Constant(
                                            variant_types[index].constructor
                                        )
                                    },
                                    inputs.into_iter().skip(1)
                                )
                                .kind
                            }
                            _ => {
                                function_call!(self.lower_expr(*function, scope, info), inputs).kind
                            }
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
                                vec![Note::primary(function.span, "try using a literal instead")],
                            ));

                            ExpressionKind::Error
                        }
                        _ => function_call!(self.lower_expr(*function, scope, info), inputs).kind,
                    }
                }
                _ => function_call!(self.lower_expr(*function, scope, info), inputs).kind,
            },
            ast::ExpressionKind::Function(input, body) => {
                let scope = scope.child(ScopeKind::Function);

                let pattern = self.lower_pattern(input, &scope, info);

                let body = self.lower_expr(*body, &scope, info);

                ExpressionKind::Function(
                    pattern,
                    Box::new(Expression {
                        span: expr.span,
                        kind: ExpressionKind::Block(vec![body], scope.values.into_inner()),
                    }),
                )
            }
            ast::ExpressionKind::When(input, arms) => ExpressionKind::When(
                Box::new(self.lower_expr(*input, scope, info)),
                arms.into_iter()
                    .map(|arm| Arm {
                        span: arm.span,
                        pattern: self.lower_pattern(arm.pattern, scope, info),
                        body: self.lower_expr(arm.body, scope, info),
                    })
                    .collect(),
            ),
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
            ast::ExpressionKind::ListLiteral(items) => ExpressionKind::ListLiteral(
                items
                    .into_iter()
                    .map(|expr| self.lower_expr(expr, scope, info))
                    .collect(),
            ),
            ast::ExpressionKind::Return(value) => {
                if !scope.is_within_function() {
                    self.diagnostics.add(Diagnostic::error(
                        "`return` outside function",
                        vec![Note::primary(
                            expr.span,
                            "try moving this inside a function",
                        )],
                    ));
                }

                ExpressionKind::Return(Box::new(self.lower_expr(*value, scope, info)))
            }
            ast::ExpressionKind::Loop(body) => {
                let scope = scope.child(ScopeKind::Loop);
                let body = self.lower_expr(*body, &scope, info);
                ExpressionKind::Loop(Box::new(body))
            }
            ast::ExpressionKind::Break(value) => {
                if !scope.is_within_loop() {
                    self.diagnostics.add(Diagnostic::error(
                        "`break` outside loop",
                        vec![Note::primary(expr.span, "try moving this inside a loop")],
                    ));
                }

                ExpressionKind::Break(Box::new(self.lower_expr(*value, scope, info)))
            }
            ast::ExpressionKind::Continue => {
                if !scope.is_within_loop() {
                    self.diagnostics.add(Diagnostic::error(
                        "`continue` outside loop",
                        vec![Note::primary(expr.span, "try moving this inside a loop")],
                    ));
                }

                ExpressionKind::Continue
            }
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
                // FIXME: Use language item instead of hardcoded ID
                TypeAnnotationKind::Builtin(BuiltinTypeId(0), Vec::new())
            }
            ast::TypeAnnotationKind::Named(name, parameters) => {
                let parameters = parameters
                    .into_iter()
                    .map(|parameter| self.lower_type_annotation(parameter, scope, info))
                    .collect();

                match scope.get(name) {
                    Some(ScopeValue::Type(ty)) => TypeAnnotationKind::Named(ty, parameters),
                    Some(ScopeValue::TypeParameter(param)) => {
                        if !parameters.is_empty() {
                            // TODO: Higher-kinded types
                            self.diagnostics.add(Diagnostic::error(
                                "higher-kinded types are not yet supported",
                                vec![Note::primary(
                                    ty.span,
                                    "try writing this on its own, with no parameters",
                                )],
                            ));
                        }

                        TypeAnnotationKind::Parameter(param)
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

    fn lower_pattern(&mut self, pattern: ast::Pattern, scope: &Scope, info: &mut Info) -> Pattern {
        let kind = (|| match pattern.kind {
            ast::PatternKind::Error => PatternKind::Error,
            ast::PatternKind::Wildcard => PatternKind::Wildcard,
            ast::PatternKind::Unit => PatternKind::Unit,
            ast::PatternKind::Number(number) => PatternKind::Number(number),
            ast::PatternKind::Text(text) => PatternKind::Text(text),
            ast::PatternKind::Name(name) => match scope.get(name) {
                Some(ScopeValue::Constant(_, Some((ty, variant)))) => {
                    PatternKind::Variant(ty, variant, Vec::new())
                }
                _ => {
                    let var = self.new_variable_id();

                    scope
                        .values
                        .borrow_mut()
                        .insert(name, ScopeValue::Variable(var));

                    info.declarations.variables.insert(
                        var,
                        Declaration {
                            name: Some(name),
                            span: pattern.span,
                            value: (),
                        },
                    );

                    PatternKind::Variable(var)
                }
            },
            ast::PatternKind::Destructure(fields) => PatternKind::Destructure(
                fields
                    .into_iter()
                    .map(|(span, name)| {
                        (
                            name,
                            self.lower_pattern(
                                ast::Pattern {
                                    span,
                                    kind: ast::PatternKind::Name(name),
                                },
                                scope,
                                info,
                            ),
                        )
                    })
                    .collect(),
            ),
            ast::PatternKind::Variant((name_span, name), values) => {
                let first = match scope.get(name) {
                    Some(name) => name,
                    None => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot find `{}`", name),
                            vec![Note::primary(name_span, "this name is not defined")],
                        ));

                        return PatternKind::Error;
                    }
                };

                let mut values = values.into_iter();

                match first {
                    ScopeValue::Type(ty) => {
                        let variants = match &info.declarations.types.get(&ty).unwrap().value.kind {
                            TypeKind::Enumeration(_, variants) => variants,
                            _ => {
                                self.diagnostics.add(Diagnostic::error(
                                    format!("cannot use `{}` in pattern", name),
                                    vec![Note::primary(
                                        name_span,
                                        "only enumeration types may be used in patterns",
                                    )],
                                ));

                                return PatternKind::Error;
                            }
                        };

                        let second = match values.next() {
                            Some(value) => value,
                            None => {
                                self.diagnostics.add(Diagnostic::error(
                                    "incomplete pattern",
                                    vec![Note::primary(
                                        name_span,
                                        "expected a variant name after this",
                                    )],
                                ));

                                return PatternKind::Error;
                            }
                        };

                        let variant_name = match second.kind {
                            ast::PatternKind::Name(name) => name,
                            _ => {
                                self.diagnostics.add(Diagnostic::error(
                                    "invalid pattern",
                                    vec![Note::primary(
                                        second.span,
                                        "expected a variant name here",
                                    )],
                                ));

                                return PatternKind::Error;
                            }
                        };

                        let variant = match variants.get(&variant_name) {
                            Some(variant) => *variant,
                            None => {
                                self.diagnostics.add(Diagnostic::error(
                                    format!(
                                        "enumeration `{}` does not declare a variant named `{}`",
                                        name, variant_name
                                    ),
                                    vec![Note::primary(second.span, "no such variant")],
                                ));

                                return PatternKind::Error;
                            }
                        };

                        PatternKind::Variant(
                            ty,
                            variant,
                            values
                                .map(|value| self.lower_pattern(value, scope, info))
                                .collect(),
                        )
                    }
                    ScopeValue::Constant(_, Some((ty, variant))) => PatternKind::Variant(
                        ty,
                        variant,
                        values
                            .map(|value| self.lower_pattern(value, scope, info))
                            .collect(),
                    ),
                    _ => {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot use `{}` in pattern", name),
                            vec![Note::primary(name_span, "expected a type or variant here")],
                        ));

                        PatternKind::Error
                    }
                }
            }
            ast::PatternKind::Annotate(inner_pattern, ty) => PatternKind::Annotate(
                Box::new(self.lower_pattern(*inner_pattern, scope, info)),
                self.lower_type_annotation(ty, scope, info),
            ),
            ast::PatternKind::Or(lhs, rhs) => PatternKind::Or(
                Box::new(self.lower_pattern(*lhs, scope, info)),
                Box::new(self.lower_pattern(*rhs, scope, info)),
            ),
            ast::PatternKind::Where(pattern, condition) => PatternKind::Where(
                Box::new(self.lower_pattern(*pattern, scope, info)),
                Box::new(self.lower_expr(*condition, scope, info)),
            ),
        })();

        Pattern {
            span: pattern.span,
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
            Some(ScopeValue::Constant(id, _)) => Some(ExpressionKind::Constant(id)),
            Some(ScopeValue::Variable(id)) => Some(ExpressionKind::Variable(id)),
            None => None,
        }
    }

    fn with_parameters(
        &mut self,
        parameters: Vec<ast::TypeParameter>,
        scope: &Scope,
        info: &mut Info,
    ) -> Vec<TypeParameterId> {
        parameters
            .into_iter()
            .map(|parameter| {
                let id = self.new_type_parameter_id();

                info.declarations.type_parameters.insert(
                    id,
                    Declaration {
                        name: Some(parameter.name),
                        span: parameter.span,
                        value: (),
                    },
                );

                scope
                    .values
                    .borrow_mut()
                    .insert(parameter.name, ScopeValue::TypeParameter(id));

                id
            })
            .collect()
    }
}
