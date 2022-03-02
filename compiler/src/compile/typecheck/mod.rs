mod engine;

pub use engine::Type;

use crate::{
    compile::lower,
    diagnostics::*,
    helpers::{ConstantId, InternedString, TraitId, TypeId, VariableId},
    parser::Span,
};
use engine::*;
use lazy_static::lazy_static;
use std::{collections::HashMap, hash::Hash};

#[derive(Debug)]
pub struct File {
    pub name: InternedString,
    pub span: Span,
    pub well_typed: bool,
    pub block: Vec<Expression>,
    pub declarations: Declarations,
}

#[derive(Debug)]
pub struct Expression {
    pub span: Span,
    pub ty: Type,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Error,
    Marker,
    Constant(ConstantId),
    Variable(VariableId),
    Text(InternedString),
    Number(f64),
    Block(Vec<Expression>),
    Call(Box<Expression>, Box<Expression>),
    Function(Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    Initialize(VariableId, Box<Expression>),
    FunctionInput,
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

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: HashMap<TypeId, Declaration<()>>,
    pub traits: HashMap<TraitId, Declaration<()>>,
    pub constants: HashMap<ConstantId, Declaration<Type>>,
    pub variables: HashMap<VariableId, Declaration<Type>>,
}

#[derive(Debug, Clone)]
pub struct Declaration<T> {
    pub name: InternedString,
    pub span: Span,
    pub ty: T,
}

impl File {
    pub fn traverse(&mut self, mut f: impl FnMut(&mut Expression)) {
        for statement in &mut self.block {
            statement.traverse_inner(&mut f);
        }
    }
}

impl Expression {
    pub fn traverse(&mut self, mut f: impl FnMut(&mut Expression)) {
        self.traverse_inner(&mut f);
    }

    fn traverse_inner(&mut self, f: &mut impl FnMut(&mut Expression)) {
        use ExpressionKind::*;

        f(self);

        match &mut self.kind {
            Block(statements) => {
                for statement in statements {
                    statement.traverse_inner(f);
                }
            }
            Call(function, input) => {
                function.traverse_inner(f);
                input.traverse_inner(f);
            }
            Function(body) => body.traverse_inner(f),
            When(_, _) => todo!(),
            Initialize(_, value) => value.traverse_inner(f),
            _ => {}
        }
    }
}

pub fn typecheck(file: &lower::File, diagnostics: &mut Diagnostics) -> File {
    let mut typechecker = Typechecker {
        well_typed: true,
        ctx: Default::default(),
        declarations: &file.declarations,
        variables: Default::default(),
        constants: Default::default(),
        function_inputs: Default::default(),
        diagnostics,
    };

    let (_, block) = typechecker.typecheck_block(&file.block);

    let declarations = Declarations {
        types: file
            .declarations
            .types
            .iter()
            .map(|(&id, decl)| {
                let decl = Declaration {
                    name: decl.name,
                    span: decl.span,
                    ty: (),
                };

                (id, decl)
            })
            .collect(),
        traits: file
            .declarations
            .traits
            .iter()
            .map(|(&id, decl)| {
                let decl = Declaration {
                    name: decl.name,
                    span: decl.span,
                    ty: (),
                };

                (id, decl)
            })
            .collect(),
        constants: file
            .declarations
            .constants
            .iter()
            .map(|(&id, decl)| {
                let decl = Declaration {
                    name: decl.name,
                    span: decl.span,
                    ty: typechecker.constants.get(&id).unwrap().ty.clone(),
                };

                (id, decl)
            })
            .collect(),
        variables: file
            .declarations
            .variables
            .iter()
            .map(|(&id, decl)| {
                let decl = Declaration {
                    name: decl.name,
                    span: decl.span,
                    ty: typechecker.variables.get(&id).unwrap().clone(),
                };

                (id, decl)
            })
            .collect(),
    };

    let mut file = File {
        span: file.span,
        name: file.name,
        well_typed: true,
        block,
        declarations,
    };

    // Only report missing types if there aren't already type errors
    let no_type_errors = typechecker.well_typed;

    file.traverse(|expr| {
        expr.ty.apply(&typechecker.ctx);

        if no_type_errors {
            if let Type::Variable(_) = expr.ty {
                typechecker.diagnostics.add(Diagnostic::error(
                    "cannot determine the type of this expression",
                    vec![Note::primary(
                        expr.span,
                        "try annotating the type with '::'",
                    )],
                ));

                typechecker.well_typed = false;
            }
        }
    });

    file.well_typed = typechecker.well_typed;

    file
}

macro_rules! builtin_types {
    ($($x:ident => $name:literal),* $(,)?) => {
        struct BuiltinTypes {
            $($x: Type,)*
        }

        lazy_static! {
            static ref BUILTIN_TYPES: BuiltinTypes = BuiltinTypes {
                $($x: Type::Named(TypeId::new(), Vec::new()),)*
            };
        }

        impl BuiltinTypes {
            fn name(&self, id: TypeId) -> Option<&'static str> {
                if false {
                    unreachable!()
                } $(else if id == self.$x.id().unwrap() {
                    Some($name)
                })* else {
                    None
                }
            }
        }
    };
}

builtin_types! {
    unit => "()",
    number => "Number",
    text => "Text",
}

struct Typechecker<'a> {
    well_typed: bool,
    ctx: Context,
    declarations: &'a lower::Declarations,
    variables: HashMap<VariableId, Type>,
    constants: HashMap<ConstantId, Expression>, // TODO: SCHEMES
    function_inputs: Vec<TypeVariable>,
    diagnostics: &'a mut Diagnostics,
}

impl<'a> Typechecker<'a> {
    fn typecheck_expr(&mut self, expr: &lower::Expression) -> Expression {
        let error = || Expression {
            span: expr.span,
            ty: Type::Bottom,
            kind: ExpressionKind::Error,
        };

        let (ty, kind) = match &expr.kind {
            lower::ExpressionKind::Error => return error(),
            lower::ExpressionKind::Unit => (BUILTIN_TYPES.unit.clone(), ExpressionKind::Marker),
            lower::ExpressionKind::Marker(ty) => {
                (self.convert_type_id(*ty), ExpressionKind::Marker)
            }
            lower::ExpressionKind::Constant(id) => {
                let constant = self.declarations.constants.get(id).unwrap();
                let value = constant.value.value.take().expect("uninitialized constant");

                let value = self.typecheck_expr(&value);
                let ty = value.ty.clone();

                self.constants.insert(*id, value);

                (ty, ExpressionKind::Constant(*id))
            }
            lower::ExpressionKind::Variable(id) => {
                let ty = self
                    .variables
                    .get(id)
                    .expect("uninitialized variable")
                    .clone();

                (ty, ExpressionKind::Variable(*id))
            }
            lower::ExpressionKind::Text(text) => {
                (BUILTIN_TYPES.text.clone(), ExpressionKind::Text(*text))
            }
            lower::ExpressionKind::Number(number) => (
                BUILTIN_TYPES.number.clone(),
                ExpressionKind::Number(*number),
            ),
            lower::ExpressionKind::Block(statements) => {
                let (ty, statements) = self.typecheck_block(statements);
                (ty, ExpressionKind::Block(statements))
            }
            lower::ExpressionKind::Call(function, input) => {
                let function = self.typecheck_expr(function);
                let input = self.typecheck_expr(input);

                let output_ty = Type::Variable(self.ctx.new_variable());

                if let Err(errors) = self.ctx.unify(
                    function.ty.clone(),
                    Type::Function(Box::new(input.ty.clone()), Box::new(output_ty.clone())),
                ) {
                    self.report_type_errors(expr, errors);
                    return error();
                }

                (
                    output_ty,
                    ExpressionKind::Call(Box::new(function), Box::new(input)),
                )
            }
            lower::ExpressionKind::Function(body) => {
                let body = self.typecheck_expr(body);

                let input_var = self
                    .function_inputs
                    .pop()
                    .expect("function body does not refer to function input");

                (
                    Type::Function(
                        Box::new(Type::Variable(input_var)),
                        Box::new(body.ty.clone()),
                    ),
                    ExpressionKind::Function(Box::new(body)),
                )
            }
            lower::ExpressionKind::When(_, _) => todo!(),
            lower::ExpressionKind::Annotate(expr, ty) => {
                let ty = self.convert_type_annotation(ty);
                let inferred_expr = self.typecheck_expr(expr);

                if let Err(errors) = self.ctx.unify(inferred_expr.ty.clone(), ty) {
                    self.report_type_errors(expr, errors);
                    return error();
                }

                (inferred_expr.ty, inferred_expr.kind)
            }
            lower::ExpressionKind::Initialize(id, value) => {
                let value = self.typecheck_expr(value);
                self.variables.insert(*id, value.ty.clone());

                (
                    BUILTIN_TYPES.unit.clone(),
                    ExpressionKind::Initialize(*id, Box::new(value)),
                )
            }
            lower::ExpressionKind::FunctionInput => {
                let var = self.ctx.new_variable();
                self.function_inputs.push(var);

                (Type::Variable(var), ExpressionKind::FunctionInput)
            }
        };

        Expression {
            span: expr.span,
            ty,
            kind,
        }
    }

    fn typecheck_block(&mut self, statements: &[lower::Expression]) -> (Type, Vec<Expression>) {
        let statements = statements
            .iter()
            .map(|statement| self.typecheck_expr(statement))
            .collect::<Vec<_>>();

        let ty = statements
            .last()
            .map(|statement| statement.ty.clone())
            .unwrap_or_else(|| BUILTIN_TYPES.unit.clone());

        (ty, statements)
    }

    fn convert_type_id(&mut self, id: TypeId) -> Type {
        let declaration = self.declarations.types.get(&id).unwrap().clone();

        match declaration.value.kind {
            lower::TypeKind::Marker
            | lower::TypeKind::Structure(_, _)
            | lower::TypeKind::Enumeration(_, _) => Type::Named(id, Vec::new()),
            lower::TypeKind::Alias(annotation) => self.convert_type_annotation(&annotation),
        }
    }

    fn convert_type_annotation(&mut self, annotation: &lower::TypeAnnotation) -> Type {
        match &annotation.kind {
            lower::TypeAnnotationKind::Error => Type::Bottom,
            lower::TypeAnnotationKind::Placeholder => Type::Variable(self.ctx.new_variable()),
            lower::TypeAnnotationKind::Unit => BUILTIN_TYPES.unit.clone(),
            lower::TypeAnnotationKind::Named(id, params) => Type::Named(
                *id,
                params
                    .iter()
                    .map(|annotation| self.convert_type_annotation(annotation))
                    .collect(),
            ),
            lower::TypeAnnotationKind::Function(input, output) => Type::Function(
                Box::new(self.convert_type_annotation(input)),
                Box::new(self.convert_type_annotation(output)),
            ),
        }
    }

    fn report_type_errors(&mut self, expr: &lower::Expression, errors: Vec<UnificationError>) {
        self.well_typed = false;

        for error in errors {
            let diagnostic = match error {
                UnificationError::Recursive(_) => Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Recursive type",
                    vec![Note::primary(
                        expr.span,
                        "The type of this references itself",
                    )],
                ),
                UnificationError::Mismatch(found, expected) => Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Mismatched types",
                    vec![Note::primary(
                        expr.span,
                        format!(
                            "Expected {}, found {}",
                            format_type(&expected, &self.declarations.types, |decl| decl
                                .name
                                .as_str()),
                            format_type(&found, &self.declarations.types, |decl| decl
                                .name
                                .as_str())
                        ),
                    )],
                ),
            };

            self.diagnostics.add(diagnostic);
        }
    }
}

pub fn format_type_scheme<T>(
    ty: &Scheme,
    types: &HashMap<TypeId, T>,
    name_extractor: impl Fn(&T) -> &str,
) -> String {
    match ty {
        Scheme::Type(ty) => format_type(ty, types, name_extractor),
        Scheme::ForAll(forall) => {
            let mut names = Vec::new();
            for &var in &forall.vars {
                names.push((var, name_for_index(names.len())));
            }

            format!(
                "{}{}",
                names
                    .iter()
                    .map(|(_, t)| t.clone() + " => ")
                    .collect::<String>(),
                format_type_with(
                    &forall.ty,
                    types,
                    name_extractor,
                    &names.into_iter().collect()
                )
            )
        }
    }
}

pub fn format_type<T>(
    ty: &Type,
    types: &HashMap<TypeId, T>,
    name_extractor: impl Fn(&T) -> &str,
) -> String {
    format_type_with(ty, types, name_extractor, &HashMap::new())
}

fn format_type_with<T>(
    ty: &Type,
    types: &HashMap<TypeId, T>,
    name_extractor: impl Fn(&T) -> &str,
    names: &HashMap<TypeVariable, String>,
) -> String {
    fn format_type<T>(
        ty: &Type,
        types: &HashMap<TypeId, T>,
        name_extractor: &impl Fn(&T) -> &str,
        is_top_level: bool,
        is_return: bool,
        names: &HashMap<TypeVariable, String>,
    ) -> String {
        match ty {
            Type::Variable(var) => names.get(var).cloned().unwrap_or_else(|| String::from("_")),
            Type::Bottom => String::from("!"),
            Type::Named(id, params) => {
                let name = types
                    .get(id)
                    .map(name_extractor)
                    .or_else(|| BUILTIN_TYPES.name(*id))
                    .expect("unregistered type");

                let params = params
                    .iter()
                    .map(|ty| {
                        String::from(" ")
                            + &format_type(ty, types, name_extractor, false, true, names)
                    })
                    .collect::<String>();

                if is_top_level {
                    format!("{name}{params}")
                } else {
                    format!("({name}{params})")
                }
            }
            Type::Function(input, output) => {
                let input = format_type(input, types, name_extractor, true, false, names);
                let output = format_type(output, types, name_extractor, true, true, names);

                if is_top_level && is_return {
                    format!("{input} -> {output}")
                } else {
                    format!("({input} -> {output})")
                }
            }
        }
    }

    format_type(ty, types, &name_extractor, true, true, names)
}

fn name_for_index(index: usize) -> String {
    const LETTERS: [char; 26] = [
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
        'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    ];

    LETTERS
        .get(index)
        .copied()
        .map(String::from)
        .unwrap_or_else(|| format!("T{}", index + 1))
}
