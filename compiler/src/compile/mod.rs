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

#[derive(Debug)]
pub struct File {
    pub name: InternedString,
    pub span: Span,
    pub declarations: Declarations,
    pub block: Block,
}

#[derive(Debug, Default)]
pub struct Declarations {
    pub types: HashMap<TypeId, Type>,
    pub constants: HashMap<ConstantId, Constant>,
}

#[derive(Debug)]
pub struct Declaration {
    pub span: Span,
    pub kind: DeclarationKind,
}

#[derive(Debug)]
pub enum DeclarationKind {
    Data(Type),
    Function(Function),
    Constant(Constant),
}

#[derive(Debug)]
pub struct Type {
    pub parameters: Vec<TypeParameter>,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Marker,
    Alias(TypeAnnotation),
    Structure(Vec<TypeField>, HashMap<InternedString, usize>),
    Enumeration(Vec<TypeVariant>, HashMap<InternedString, usize>),
}

#[derive(Debug)]
pub struct TypeField {
    pub ty: TypeAnnotation,
}

#[derive(Debug)]
pub struct TypeVariant {
    pub values: Vec<TypeAnnotation>,
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<TypeParameter>,
    pub input: Pattern,
    pub body: Expression,
}

#[derive(Debug)]
pub struct Constant {
    pub ty: TypeAnnotation,
    pub value: Rc<RefCell<Option<Expression>>>,
}

#[derive(Debug)]
pub struct Block {
    pub span: Span,
    pub statements: Vec<Expression>,
}

#[derive(Debug)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Unit,
    Constant(ConstantId),
    Variable(VariableId),
    Text(InternedString),
    Number(f64),
    Marker(TypeId),
    Block(Block),
    Call(Box<Expression>, Box<Expression>),
    Function(Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    Annotate(Box<Expression>, TypeAnnotation),
    Initialize(VariableId, Box<Expression>),
    FunctionInput,
    // TODO: Instantiation
}

#[derive(Debug)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Binding(VariableId),
    // TODO: Support complex paths (data fields, variants)
    Wildcard,
}

#[derive(Debug)]
pub struct TypeAnnotation {
    pub span: Span,
    pub kind: TypeAnnotationKind,
}

#[derive(Debug)]
pub enum TypeAnnotationKind {
    Placeholder,
    Named(TypeId, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
}

#[derive(Debug)]
pub struct TypeParameter {
    pub span: Span,
    pub kind: TypeParameterKind,
}

#[derive(Debug)]
pub enum TypeParameterKind {
    Named(TypeId),
    Constrained(Vec<TraitId>, TypeId),
}

#[derive(Debug)]
pub enum Path {
    Type(FileId, TypeId),
    Variable(FileId, VariableId),
    // Components are resolved during type checking
    Member(VariableId, Vec<parser::PathComponent>),
}

pub fn compile(file: parser::File, diagnostics: &mut Diagnostics) -> Option<File> {
    // TODO: Handle file attributes (ie. loading prelude)
    let scope = Scope::default();

    let mut declarations = Declarations::default();

    let block = compile_block(
        file.span,
        file.statements,
        &scope,
        &mut declarations,
        diagnostics,
    )?;

    let file = File {
        name: file.name,
        span: file.span,
        declarations,
        block,
    };

    Some(file)
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

fn compile_block(
    span: Span,
    statements: Vec<parser::Statement>,
    scope: &Scope,
    declarations: &mut Declarations,
    diagnostics: &mut Diagnostics,
) -> Option<Block> {
    let scope = Scope {
        parent: Some(scope),
        ..Default::default()
    };

    let statements = statements
        .into_iter()
        .filter_map(|statement| compile_statement(statement, &scope, declarations, diagnostics))
        .flatten()
        .collect();

    Some(Block { span, statements })
}

fn compile_statement(
    statement: parser::Statement,
    scope: &Scope,
    declarations: &mut Declarations,
    diagnostics: &mut Diagnostics,
) -> Option<Option<Expression>> {
    let defined = |name| scope.values.borrow().contains_key(&name);

    match statement.kind {
        parser::StatementKind::Type(name, ty) => {
            if defined(name) {
                diagnostics.add(Diagnostic::error(
                    format!("`{name}` is already defined"),
                    vec![Note::primary(
                        statement.span,
                        "try assigning to a different name",
                    )],
                ));

                return None;
            }

            if !ty.parameters.is_empty() {
                diagnostics.add(Diagnostic::error(
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
                kind: match &ty.kind {
                    parser::TypeKind::Marker => TypeKind::Marker,
                    parser::TypeKind::Alias(alias) => {
                        TypeKind::Alias(compile_type_annotation(alias, scope, diagnostics)?)
                    }
                    parser::TypeKind::Structure(fields) => TypeKind::Structure(
                        fields
                            .iter()
                            .map(|field| {
                                Some(TypeField {
                                    ty: compile_type_annotation(&field.ty, scope, diagnostics)?,
                                })
                            })
                            .collect::<Option<_>>()?,
                        fields
                            .iter()
                            .enumerate()
                            .map(|(index, field)| (field.name, index))
                            .collect(),
                    ),
                    parser::TypeKind::Enumeration(variants) => TypeKind::Enumeration(
                        variants
                            .iter()
                            .map(|variant| {
                                Some(TypeVariant {
                                    values: variant
                                        .values
                                        .iter()
                                        .map(|ty| compile_type_annotation(ty, scope, diagnostics))
                                        .collect::<Option<_>>()?,
                                })
                            })
                            .collect::<Option<_>>()?,
                        variants
                            .iter()
                            .enumerate()
                            .map(|(index, variant)| (variant.name, index))
                            .collect(),
                    ),
                },
            };

            declarations.types.insert(id, ty);

            Some(None)
        }
        parser::StatementKind::Trait(_, _) => None, // TODO
        parser::StatementKind::Constant(name, declaration) => {
            if defined(name) {
                diagnostics.add(Diagnostic::error(
                    format!("`{name}` is already defined"),
                    vec![Note::primary(
                        statement.span,
                        "try assigning to a different name",
                    )],
                ));

                return None;
            }

            if !declaration.parameters.is_empty() {
                diagnostics.add(Diagnostic::error(
                    "type parameters are currently unsupported",
                    vec![Note::primary(
                        Span::join(
                            declaration.parameters.first().unwrap().span,
                            declaration.parameters.last().unwrap().span,
                        ),
                        "try removing these parameters",
                    )],
                ));

                return None;
            }

            let constant = Constant {
                ty: compile_type_annotation(&declaration.ty, scope, diagnostics)?,
                value: Default::default(),
            };

            let id = ConstantId::new();
            scope
                .values
                .borrow_mut()
                .insert(name, ScopeValue::Constant(id));

            declarations.constants.insert(id, constant);

            Some(None)
        }
        parser::StatementKind::Implementation(_) => None, // TODO
        parser::StatementKind::Assign(pattern, expr) => match &pattern.kind {
            parser::PatternKind::Path(path) => {
                if !path.components.is_empty() {
                    diagnostics.add(Diagnostic::error(
                        "cannot assign to a path",
                        vec![Note::primary(expr.span, "try assigning to a name instead")],
                    ));

                    return None;
                }

                let name = path.base;
                let mut associated_constant = None;

                match scope.values.borrow().get(&name).cloned() {
                    Some(ScopeValue::Type(_)) => {
                        diagnostics.add(Diagnostic::error(
                            format!("`{name}` is already defined"),
                            vec![Note::primary(
                                statement.span,
                                "try assigning to a different name",
                            )],
                        ));

                        return None;
                    }
                    Some(ScopeValue::Constant(id)) => {
                        let c = declarations.constants.get(&id).unwrap().value.clone();

                        if let Some(associated_constant) = c.borrow().as_ref() {
                            diagnostics.add(Diagnostic::error(
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
                    _ => {}
                }

                let expr_span = expr.span;

                let value = compile_expr(expr, scope, declarations, diagnostics)?;

                Some(if let Some(associated_constant) = associated_constant {
                    associated_constant.replace(Some(value));
                    None
                } else {
                    let id = VariableId::new();
                    scope
                        .values
                        .borrow_mut()
                        .insert(name, ScopeValue::Variable(id));

                    Some(Expression {
                        span: expr_span,
                        kind: ExpressionKind::Initialize(id, Box::new(value)),
                    })
                })
            }
            parser::PatternKind::Wildcard => {
                compile_expr(expr, scope, declarations, diagnostics).map(Some)
            }
        },
        parser::StatementKind::Expression(expr) => {
            compile_expr(expr, scope, declarations, diagnostics).map(Some)
        }
    }
}

fn compile_expr(
    expr: parser::Expression,
    scope: &Scope,
    declarations: &mut Declarations,
    diagnostics: &mut Diagnostics,
) -> Option<Expression> {
    let kind = match expr.kind {
        parser::ExpressionKind::Unit => ExpressionKind::Unit,
        parser::ExpressionKind::Text(text) => ExpressionKind::Text(text),
        parser::ExpressionKind::Number(number) => ExpressionKind::Number(number),
        parser::ExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
        parser::ExpressionKind::Path(path) => {
            if !path.components.is_empty() {
                diagnostics.add(Diagnostic::error(
                    "subpaths are currently unsupported",
                    vec![Note::primary(expr.span, "try simplifying this expression")],
                ));

                return None;
            }

            let name = path.base;

            match resolve_value(expr.span, name, scope, declarations, diagnostics)? {
                Some(value) => value,
                None => {
                    diagnostics.add(Diagnostic::error(
                        format!("cannot find variable `{name}`"),
                        vec![Note::primary(expr.span, "no such variable")],
                    ));

                    return None;
                }
            }
        }
        parser::ExpressionKind::Block(statements) => {
            let scope = Scope {
                parent: Some(scope),
                ..Default::default()
            };

            let block = compile_block(expr.span, statements, &scope, declarations, diagnostics)?;

            ExpressionKind::Block(block)
        }
        parser::ExpressionKind::Call(function, input) => {
            if let parser::ExpressionKind::Path(path) = &function.kind {
                if let Some(ScopeValue::Type(_ty)) = scope.values.borrow().get(&path.base) {
                    // TODO Handle instantiation (also for enum variants)

                    diagnostics.add(Diagnostic::error(
                        "instantiation is currently unsupported",
                        vec![Note::primary(function.span, "try removing this expression")],
                    ));

                    return None;
                }
            }

            let function = compile_expr(*function, scope, declarations, diagnostics)?;
            let input = compile_expr(*input, scope, declarations, diagnostics)?;

            ExpressionKind::Call(Box::new(function), Box::new(input))
        }
        parser::ExpressionKind::Function(input, body) => {
            let scope = Scope {
                parent: Some(scope),
                used_variables: Some(Default::default()),
                ..Default::default()
            };

            let mut block = Block {
                span: body.span,
                statements: Vec::with_capacity(2),
            };

            // Desugar function input pattern matching
            let input_span = input.span;
            if let Some(initialization) = compile_statement(
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
                declarations,
                diagnostics,
            )? {
                block.statements.push(initialization)
            }

            let body = compile_expr(*body, &scope, declarations, diagnostics)?;
            block.statements.push(body);

            block.statements.shrink_to_fit();

            ExpressionKind::Function(Box::new(Expression {
                span: expr.span,
                kind: ExpressionKind::Block(block),
            }))
        }
        parser::ExpressionKind::When(_, _) => return None, // TODO
        parser::ExpressionKind::Annotate(_, _) => return None, // TODO
    };

    Some(Expression {
        span: expr.span,
        kind,
    })
}

fn compile_type_annotation(
    ty: &parser::TypeAnnotation,
    scope: &Scope,
    diagnostics: &mut Diagnostics,
) -> Option<TypeAnnotation> {
    let kind = match &ty.kind {
        parser::TypeAnnotationKind::Placeholder => TypeAnnotationKind::Placeholder,
        parser::TypeAnnotationKind::Path(path, parameters) => {
            if !path.components.is_empty() {
                diagnostics.add(Diagnostic::error(
                    "subpaths are currently unsupported",
                    vec![Note::primary(ty.span, "try simplifying this type")],
                ));

                return None;
            }

            let name = path.base;

            let ty = match resolve_type(name, scope) {
                Some(ty) => ty,
                None => {
                    diagnostics.add(Diagnostic::error(
                        format!("cannot find type `{}`", path.base),
                        vec![Note::primary(ty.span, "no such type")],
                    ));

                    return None;
                }
            };

            let parameters = parameters
                .iter()
                .map(|parameter| compile_type_annotation(parameter, scope, diagnostics))
                .collect::<Option<_>>()?;

            TypeAnnotationKind::Named(ty, parameters)
        }
        parser::TypeAnnotationKind::Function(input, output) => TypeAnnotationKind::Function(
            Box::new(compile_type_annotation(input, scope, diagnostics)?),
            Box::new(compile_type_annotation(output, scope, diagnostics)?),
        ),
    };

    Some(TypeAnnotation {
        span: ty.span,
        kind,
    })
}

fn resolve_value(
    span: Span,
    name: InternedString,
    scope: &Scope,
    declarations: &mut Declarations,
    diagnostics: &mut Diagnostics,
) -> Option<Option<ExpressionKind>> {
    Some(match scope.get(name) {
        Some(ScopeValue::Type(id)) => {
            fn resolve(
                span: Span,
                id: TypeId,
                declarations: &mut Declarations,
                diagnostics: &mut Diagnostics,
            ) -> Option<Option<ExpressionKind>> {
                Some(match declarations.types.get(&id).unwrap().kind {
                    TypeKind::Marker => Some(ExpressionKind::Marker(id)),
                    TypeKind::Alias(TypeAnnotation {
                        kind: TypeAnnotationKind::Named(alias_id, _),
                        ..
                    }) => {
                        // TODO: Maybe perform this check earlier?
                        if alias_id == id {
                            diagnostics.add(Diagnostic::error(
                                "cannot instantiate recursive type",
                                vec![Note::primary(
                                    span,
                                    "this type references itself and cannot be constructed",
                                )],
                            ));

                            return None;
                        } else {
                            resolve(span, alias_id, declarations, diagnostics)?
                        }
                    }
                    _ => {
                        diagnostics.add(Diagnostic::error(
                            "cannot use data constructor as value",
                            vec![Note::primary(span, "try instantiating the data structure")],
                        ));

                        return None;
                    }
                })
            }

            resolve(span, id, declarations, diagnostics)?
        }
        Some(ScopeValue::Constant(id)) => Some(ExpressionKind::Constant(id)),
        Some(ScopeValue::Variable(id)) => Some(ExpressionKind::Variable(id)),
        None => None,
    })
}

fn resolve_type(name: InternedString, scope: &Scope) -> Option<TypeId> {
    scope.get(name).and_then(|value| match value {
        ScopeValue::Type(id) => Some(id),
        _ => None,
    })
}
