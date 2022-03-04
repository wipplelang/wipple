mod engine;

pub use engine::Type;

use crate::{
    compile::lower,
    diagnostics::*,
    helpers::{ConstantId, InternedString, TraitId, TypeId, VariableId},
    parser::Span,
    Compiler, FilePath, Loader,
};
use engine::*;
use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    mem,
};

#[derive(Debug)]
pub struct Program {
    pub well_typed: bool,
    pub body: Vec<Expression>,
    pub declarations: Declarations,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub ty: Type,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
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
    External(InternedString, InternedString),
    Initialize(VariableId, Box<Expression>),
    FunctionInput,
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
    Placeholder,
    Named(TypeId, Vec<TypeAnnotation>),
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

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: HashMap<TypeId, Declaration<()>>,
    pub traits: HashMap<TraitId, Declaration<()>>,
    pub constants: HashMap<ConstantId, Declaration<Expression>>,
    pub variables: HashMap<VariableId, Declaration<Type>>,
}

#[derive(Debug, Clone)]
pub struct Declaration<T> {
    pub file: FilePath,
    pub name: InternedString,
    pub span: Span,
    pub value: T,
}

impl Program {
    pub fn traverse(&mut self, mut f: impl FnMut(&mut Expression)) {
        for statement in &mut self.body {
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

impl<L: Loader> Compiler<L> {
    pub fn typecheck(&mut self, mut files: Vec<lower::File>) -> Program {
        let mut typechecker = Typechecker {
            well_typed: true,
            ctx: Default::default(),
            variables: Default::default(),
            constants: Default::default(),
            function_inputs: Default::default(),
            diagnostics: &mut self.diagnostics,
            used_types: Default::default(),
            used_traits: Default::default(),
            used_constants: Default::default(),
            used_variables: Default::default(),
        };

        let mut body = Vec::new();

        for file in &mut files {
            for (id, decl) in &file.declarations.constants {
                if let lower::Declaration::Local(decl) = decl {
                    let mut expr = typechecker
                        .typecheck_expr(decl.value.value.borrow().as_ref().unwrap(), file);

                    // TODO: Handle type parameters (`decl.value.parameters`)
                    let ty = typechecker.convert_type_annotation(&decl.value.ty);
                    if let Err(errors) = typechecker.ctx.unify(expr.ty.clone(), ty.clone()) {
                        typechecker.report_type_errors(expr.span, errors, file);
                    }

                    expr.ty = ty;

                    typechecker.constants.insert(*id, expr);
                }
            }

            let (_, block) = typechecker.typecheck_block(&mem::take(&mut file.block), file);

            body.extend(block);
        }

        // Only report missing types if there aren't already type errors
        let no_type_errors = typechecker.well_typed;

        let mut ensure_well_typed = |expr: &mut Expression| {
            expr.ty.apply(&typechecker.ctx);

            if no_type_errors && !expr.ty.vars().is_empty() {
                typechecker.diagnostics.add(Diagnostic::error(
                    "cannot determine the type of this expression",
                    vec![Note::primary(
                        expr.span,
                        "try annotating the type with '::'",
                    )],
                ));

                typechecker.well_typed = false;
            }
        };

        let mut declarations = Declarations::default();

        for file in files {
            for (id, decl) in file.declarations.types {
                if let lower::Declaration::Local(decl) = decl {
                    declarations.types.insert(
                        id,
                        Declaration {
                            file: file.path,
                            name: decl.name,
                            span: decl.span,
                            value: (),
                        },
                    );
                }
            }

            for (id, decl) in file.declarations.traits {
                if let lower::Declaration::Local(decl) = decl {
                    declarations.traits.insert(
                        id,
                        Declaration {
                            file: file.path,
                            name: decl.name,
                            span: decl.span,
                            value: (),
                        },
                    );
                }
            }

            for (id, decl) in file.declarations.constants {
                if let lower::Declaration::Local(decl) = decl {
                    let mut expr = typechecker.constants.get(&id).unwrap().clone();
                    ensure_well_typed(&mut expr);

                    declarations.constants.insert(
                        id,
                        Declaration {
                            file: file.path,
                            name: decl.name,
                            span: decl.span,
                            value: expr,
                        },
                    );
                }
            }

            for (id, decl) in file.declarations.variables {
                if let lower::Declaration::Local(decl) = decl {
                    let ty = typechecker.variables.get(&id).unwrap().clone();

                    let mut temp_expr = Expression {
                        span: decl.span,
                        ty,
                        kind: ExpressionKind::Error, // unused
                    };

                    ensure_well_typed(&mut temp_expr);

                    declarations.variables.insert(
                        id,
                        Declaration {
                            file: file.path,
                            name: decl.name,
                            span: decl.span,
                            value: temp_expr.ty,
                        },
                    );
                }
            }
        }

        let mut program = Program {
            well_typed: true,
            body,
            declarations,
        };

        program.traverse(ensure_well_typed);
        program.well_typed = typechecker.well_typed;

        macro_rules! warn_unused {
            ($($x:ident => ($name:literal, $used:ident),)*) => {
                $(
                    for (id, decl) in &program.declarations.$x {
                        if !typechecker.$used.contains(id) {
                            typechecker.diagnostics.add(Diagnostic::warning(
                                format!("unused {}", $name),
                                vec![Note::primary(
                                    decl.span,
                                    format!("`{}` is unused", decl.name),
                                )],
                            ))
                        }
                    }
                )*
            };
        }

        warn_unused!(
            types => ("type", used_types),
            traits => ("trait", used_traits),
            constants => ("constant", used_constants),
            variables => ("variable", used_variables),
        );

        program
    }
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
    variables: HashMap<VariableId, Type>,
    constants: HashMap<ConstantId, Expression>, // TODO: SCHEMES
    function_inputs: Vec<TypeVariable>,
    diagnostics: &'a mut Diagnostics,
    used_types: HashSet<TypeId>,
    used_traits: HashSet<TraitId>,
    used_constants: HashSet<ConstantId>,
    used_variables: HashSet<VariableId>,
}

impl<'a> Typechecker<'a> {
    fn typecheck_expr(&mut self, expr: &lower::Expression, file: &lower::File) -> Expression {
        let error = || Expression {
            span: expr.span,
            ty: Type::Bottom,
            kind: ExpressionKind::Error,
        };

        let (ty, kind) = match &expr.kind {
            lower::ExpressionKind::Error => {
                self.well_typed = false; // signal that type errors should be suppressed
                return error();
            }
            lower::ExpressionKind::Unit => (BUILTIN_TYPES.unit.clone(), ExpressionKind::Marker),
            lower::ExpressionKind::Marker(ty) => {
                (self.convert_type_id(*ty, file), ExpressionKind::Marker)
            }
            lower::ExpressionKind::Constant(id) => {
                let constant = file.declarations.constants.get(id).unwrap();
                self.used_constants.insert(*id);

                let ty = match constant {
                    lower::Declaration::Local(decl) => {
                        let value = decl.value.value.borrow();
                        let value = value.as_ref().expect("uninitialized constant");

                        let value = self.typecheck_expr(value, file);
                        let ty = value.ty.clone();

                        self.constants.insert(*id, value);

                        ty
                    }
                    lower::Declaration::Dependency(decl) => {
                        self.constants.get(&decl.value).unwrap().ty.clone()
                    }
                };

                (ty, ExpressionKind::Constant(*id))
            }
            lower::ExpressionKind::Variable(id) => {
                let ty = self
                    .variables
                    .get(id)
                    .expect("uninitialized variable")
                    .clone();

                self.used_variables.insert(*id);

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
                let (ty, statements) = self.typecheck_block(statements, file);
                (ty, ExpressionKind::Block(statements))
            }
            lower::ExpressionKind::Call(function, input) => {
                let function = self.typecheck_expr(function, file);
                let input = self.typecheck_expr(input, file);

                let output_ty = Type::Variable(self.ctx.new_variable());

                if let Err(errors) = self.ctx.unify(
                    function.ty.clone(),
                    Type::Function(Box::new(input.ty.clone()), Box::new(output_ty.clone())),
                ) {
                    self.report_type_errors(expr.span, errors, file);
                    return error();
                }

                (
                    output_ty,
                    ExpressionKind::Call(Box::new(function), Box::new(input)),
                )
            }
            lower::ExpressionKind::Function(body) => {
                let body = self.typecheck_expr(body, file);

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
            lower::ExpressionKind::External(namespace, identifier) => (
                Type::Variable(self.ctx.new_variable()),
                ExpressionKind::External(*namespace, *identifier),
            ),
            lower::ExpressionKind::Annotate(expr, ty) => {
                let ty = self.convert_type_annotation(ty);
                let inferred_expr = self.typecheck_expr(expr, file);

                if let Err(errors) = self.ctx.unify(inferred_expr.ty.clone(), ty) {
                    self.report_type_errors(expr.span, errors, file);
                    return error();
                }

                (inferred_expr.ty, inferred_expr.kind)
            }
            lower::ExpressionKind::Initialize(id, value) => {
                let value = self.typecheck_expr(value, file);
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

    fn typecheck_block(
        &mut self,
        statements: &[lower::Expression],
        file: &lower::File,
    ) -> (Type, Vec<Expression>) {
        let statements = statements
            .iter()
            .map(|statement| self.typecheck_expr(statement, file))
            .collect::<Vec<_>>();

        let ty = statements
            .last()
            .map(|statement| statement.ty.clone())
            .unwrap_or_else(|| BUILTIN_TYPES.unit.clone());

        (ty, statements)
    }

    fn convert_type_id(&mut self, id: TypeId, file: &lower::File) -> Type {
        let decl = file.declarations.types.get(&id).unwrap().clone();
        self.used_types.insert(id);

        match decl {
            lower::Declaration::Local(decl) => match decl.value.kind {
                lower::TypeKind::Marker
                | lower::TypeKind::Structure(_, _)
                | lower::TypeKind::Enumeration(_, _) => Type::Named(id, Vec::new()), // TODO: parameters
                lower::TypeKind::Alias(annotation) => self.convert_type_annotation(&annotation),
            },
            lower::Declaration::Dependency(_) => Type::Named(id, Vec::new()), // TODO: parameters
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

    fn report_type_errors(
        &mut self,
        span: Span,
        errors: Vec<UnificationError>,
        file: &lower::File,
    ) {
        self.well_typed = false;

        for error in errors {
            let diagnostic = match error {
                UnificationError::Recursive(_) => Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Recursive type",
                    vec![Note::primary(span, "The type of this references itself")],
                ),
                UnificationError::Mismatch(found, expected) => Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Mismatched types",
                    vec![Note::primary(
                        span,
                        format!(
                            "Expected {}, found {}",
                            format_type(&expected, &file.declarations.types, |decl| decl
                                .name()
                                .as_str()),
                            format_type(&found, &file.declarations.types, |decl| decl
                                .name()
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
