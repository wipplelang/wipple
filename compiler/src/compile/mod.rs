use crate::{
    diagnostics::*,
    helpers::InternedString,
    parser::{self, Span},
};
use std::{cell::RefCell, collections::HashMap};

pub type TypeId = usize; // TODO
pub type TraitId = usize; // TODO
pub type VariableId = usize; // TODO
pub type FileId = usize; // TODO
pub type DataId = usize; // TODO
pub type FunctionId = usize; // TODO
pub type ConstantId = usize; // TODO

#[derive(Debug)]
pub struct File {
    pub name: InternedString,
    pub span: Span,
    pub declarations: Declarations,
    pub block: Block,
}

#[derive(Debug, Default)]
pub struct Declarations {
    pub datas: HashMap<DataId, Data>,
    pub functions: HashMap<FunctionId, Function>,
    pub constants: HashMap<ConstantId, Constant>,
}

#[derive(Debug)]
pub struct Declaration {
    pub span: Span,
    pub kind: DeclarationKind,
}

#[derive(Debug)]
pub enum DeclarationKind {
    Data(Data),
    Function(Function),
    Constant(Constant),
}

#[derive(Debug)]
pub struct Data {
    pub parameters: Vec<TypeParameter>,
    pub kind: DataKind,
}

#[derive(Debug)]
pub enum DataKind {
    Marker,
    Structure(Vec<DataField>, HashMap<InternedString, usize>),
    Enumeration(Vec<DataVariant>, HashMap<InternedString, usize>),
}

#[derive(Debug)]
pub struct DataField {
    pub ty: Type,
}

#[derive(Debug)]
pub struct DataVariant {
    pub values: Vec<Type>,
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<TypeParameter>,
    pub input: Pattern,
    pub body: Expression,
}

#[derive(Debug)]
pub struct Constant {
    pub ty: Type,
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
    Function(FunctionId),
    Constant(ConstantId),
    Variable(VariableId),
    Text(InternedString),
    Number(f64),
    Block(Block),
    Call(Box<Expression>, Box<Expression>),
    Closure(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    Annotate(Box<Expression>, Type),
    Initialize(VariableId, Box<Expression>),
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
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Placeholder,
    Named(TypeId, Vec<Type>),
    Function(Box<Type>, Box<Type>),
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

pub fn compile(file: &parser::File, diagnostics: &mut Diagnostics) -> Option<File> {
    // TODO: Handle file attributes (ie. loading prelude)
    let scope = Scope::default();

    let mut declarations = Declarations::default();

    let block = compile_block(
        file.span,
        &file.statements,
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
    datas: RefCell<HashMap<InternedString, DataId>>,
    functions: RefCell<HashMap<InternedString, FunctionId>>,
    constants: RefCell<HashMap<InternedString, ConstantId>>,
    variables: RefCell<HashMap<InternedString, VariableId>>,
}

#[derive(Debug)]
struct Variable {
    // TODO
}

fn compile_block(
    span: Span,
    statements: &[parser::Statement],
    scope: &Scope,
    declarations: &mut Declarations,
    diagnostics: &mut Diagnostics,
) -> Option<Block> {
    let scope = Scope {
        parent: Some(scope),
        ..Default::default()
    };

    let statements = statements
        .iter()
        .filter_map(|statement| compile_statement(statement, &scope, declarations, diagnostics))
        .flatten()
        .collect();

    Some(Block { span, statements })
}

fn compile_statement(
    statement: &parser::Statement,
    scope: &Scope,
    declarations: &mut Declarations,
    diagnostics: &mut Diagnostics,
) -> Option<Option<Expression>> {
    let defined_as_constant = |name| {
        scope.datas.borrow().contains_key(name)
            || scope.functions.borrow().contains_key(name)
            || scope.constants.borrow().contains_key(name)
    };

    let defined = |name| defined_as_constant(name) || scope.variables.borrow().contains_key(name);

    match &statement.kind {
        parser::StatementKind::Data(name, data) => {
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

            if !data.parameters.is_empty() {
                diagnostics.add(Diagnostic::error(
                    "type parameters are currently unsupported",
                    vec![Note::primary(
                        Span::join(
                            data.parameters.first().unwrap().span,
                            data.parameters.last().unwrap().span,
                        ),
                        "try removing these parameters",
                    )],
                ));

                return None;
            }

            let id = DataId::default(); // FIXME: Generate fresh ID
            scope.datas.borrow_mut().insert(*name, id);

            let data = Data {
                parameters: Vec::new(),
                kind: match &data.kind {
                    parser::DataKind::Marker => DataKind::Marker,
                    parser::DataKind::Structure(fields) => DataKind::Structure(
                        fields
                            .iter()
                            .map(|field| {
                                Some(DataField {
                                    ty: compile_type(&field.ty, scope, diagnostics)?,
                                })
                            })
                            .collect::<Option<_>>()?,
                        fields
                            .iter()
                            .enumerate()
                            .map(|(index, field)| (field.name, index))
                            .collect(),
                    ),
                    parser::DataKind::Enumeration(variants) => DataKind::Enumeration(
                        variants
                            .iter()
                            .map(|variant| {
                                Some(DataVariant {
                                    values: variant
                                        .values
                                        .iter()
                                        .map(|ty| compile_type(ty, scope, diagnostics))
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

            declarations.datas.insert(id, data);

            Some(None)
        }
        parser::StatementKind::Function(_, _) => None, // TODO
        parser::StatementKind::Template(_, _) => None, // TODO
        parser::StatementKind::Constant(_, _) => None, // TODO
        parser::StatementKind::Assign(pattern, expr) => {
            match &pattern.kind {
                parser::PatternKind::Path(path) => {
                    if !path.components.is_empty() {
                        diagnostics.add(Diagnostic::error(
                            "cannot assign to a path",
                            vec![Note::primary(expr.span, "try assigning to a name instead")],
                        ));

                        return None;
                    }

                    let name = path.base;

                    if defined_as_constant(&name) {
                        diagnostics.add(Diagnostic::error(
                            format!("`{name}` is already defined"),
                            vec![Note::primary(
                                statement.span,
                                "try assigning to a different name",
                            )],
                        ));

                        return None;
                    }

                    let value = compile_expr(expr, scope, declarations, diagnostics)?;

                    let id = VariableId::default(); // FIXME: Generate fresh ID
                    scope.variables.borrow_mut().insert(name, id);

                    Some(Some(Expression {
                        span: expr.span,
                        kind: ExpressionKind::Initialize(id, Box::new(value)),
                    }))
                }
                parser::PatternKind::Wildcard => {
                    compile_expr(expr, scope, declarations, diagnostics).map(Some)
                }
            }
        }
        parser::StatementKind::Expression(expr) => {
            compile_expr(expr, scope, declarations, diagnostics).map(Some)
        }
    }
}

fn compile_expr(
    expr: &parser::Expression,
    scope: &Scope,
    declarations: &mut Declarations,
    diagnostics: &mut Diagnostics,
) -> Option<Expression> {
    let kind = match &expr.kind {
        parser::ExpressionKind::Unit => ExpressionKind::Unit,
        parser::ExpressionKind::Text(text) => ExpressionKind::Text(*text),
        parser::ExpressionKind::Number(number) => ExpressionKind::Number(*number),
        parser::ExpressionKind::Path(path) => {
            if !path.components.is_empty() {
                diagnostics.add(Diagnostic::error(
                    "subpaths are currently unsupported",
                    vec![Note::primary(expr.span, "try simplifying this expression")],
                ));

                return None;
            }

            let name = path.base;

            match resolve_value(expr.span, name, scope, diagnostics)? {
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
                if let Some(_ty) = scope.datas.borrow().get(&path.base) {
                    // TODO Handle instantiation (also for enum variants)

                    diagnostics.add(Diagnostic::error(
                        "instantiation is currently unsupported",
                        vec![Note::primary(function.span, "try removing this expression")],
                    ));

                    return None;
                }
            }

            let function = compile_expr(function, scope, declarations, diagnostics)?;
            let input = compile_expr(input, scope, declarations, diagnostics)?;

            ExpressionKind::Call(Box::new(function), Box::new(input))
        }
        parser::ExpressionKind::Closure(input, body) => {
            let input = compile_pattern(input, scope, diagnostics)?;
            let body = compile_expr(body, scope, declarations, diagnostics)?;

            ExpressionKind::Closure(input, Box::new(body))
        }
        parser::ExpressionKind::When(_, _) => return None, // TODO
        parser::ExpressionKind::Annotate(_, _) => return None, // TODO
    };

    Some(Expression {
        span: expr.span,
        kind,
    })
}

fn compile_type(ty: &parser::Type, scope: &Scope, diagnostics: &mut Diagnostics) -> Option<Type> {
    let kind = match &ty.kind {
        parser::TypeKind::Placeholder => TypeKind::Placeholder,
        parser::TypeKind::Path(path, parameters) => {
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
                .map(|parameter| compile_type(parameter, scope, diagnostics))
                .collect::<Option<_>>()?;

            TypeKind::Named(ty, parameters)
        }
        parser::TypeKind::Function(input, output) => TypeKind::Function(
            Box::new(compile_type(input, scope, diagnostics)?),
            Box::new(compile_type(output, scope, diagnostics)?),
        ),
    };

    Some(Type {
        span: ty.span,
        kind,
    })
}

fn compile_pattern(
    pattern: &parser::Pattern,
    scope: &Scope,
    diagnostics: &mut Diagnostics,
) -> Option<Pattern> {
    let kind = match &pattern.kind {
        parser::PatternKind::Path(path) => {
            todo!();
        }
        parser::PatternKind::Wildcard => PatternKind::Wildcard,
    };

    Some(Pattern {
        span: pattern.span,
        kind,
    })
}

fn resolve_value(
    span: Span,
    name: InternedString,
    scope: &Scope,
    diagnostics: &mut Diagnostics,
) -> Option<Option<ExpressionKind>> {
    let mut parent = Some(scope);
    let mut result = None;
    while let Some(scope) = parent {
        if scope.datas.borrow().get(&name).is_some() {
            diagnostics.add(Diagnostic::error(
                "cannot use data constructor as value",
                vec![Note::primary(span, "try instantiating the data structure")],
            ));

            return None;
        } else if let Some(function) = scope.functions.borrow().get(&name) {
            result = Some(ExpressionKind::Function(*function));
            break;
        } else if let Some(constant) = scope.constants.borrow().get(&name) {
            result = Some(ExpressionKind::Constant(*constant));
            break;
        } else if let Some(variable) = scope.variables.borrow().get(&name) {
            result = Some(ExpressionKind::Variable(*variable));
            break;
        }

        parent = scope.parent;
    }

    Some(result)
}

fn resolve_type(name: InternedString, scope: &Scope) -> Option<usize> {
    let mut parent = Some(scope);
    let mut result = None;
    while let Some(scope) = parent {
        if let Some(ty) = scope.datas.borrow().get(&name).copied() {
            result = Some(ty);
            break;
        }

        parent = scope.parent;
    }

    result
}
