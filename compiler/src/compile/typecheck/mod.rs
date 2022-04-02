mod engine;

pub use engine::{Scheme, Type};

use crate::{
    compile::lower,
    diagnostics::*,
    helpers::{ConstantId, InternedString, TypeId, TypeParameterId, VariableId},
    parser::Span,
    Compiler, FilePath, Loader,
};
use engine::*;
use lazy_static::lazy_static;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    mem,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub well_typed: bool,
    pub body: Vec<Expression>,
    pub declarations: Declarations,
    pub top_level: HashMap<InternedString, lower::ScopeValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expression {
    pub span: Span,
    pub scheme: Scheme,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExpressionKind {
    Error,
    Marker,
    Constant(ConstantId),
    Variable(VariableId),
    Text(InternedString),
    Number(Decimal),
    Block(Vec<Expression>, HashMap<InternedString, lower::ScopeValue>),
    Call(Box<Expression>, Box<Expression>),
    Member(Box<Expression>, usize),
    Function(Box<Expression>, HashSet<VariableId>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Initialize(VariableId, Box<Expression>),
    Structure(Vec<Expression>),
    FunctionInput,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PatternKind {
    Binding(VariableId),
    // TODO: Support complex paths (data fields, variants)
    Wildcard,
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub span: Span,
    pub id: TypeId,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Declarations {
    pub types: HashMap<TypeId, Declaration<()>>,
    pub type_parameters: HashMap<TypeParameterId, Declaration<()>>,
    pub constants: HashMap<ConstantId, Declaration<Expression>>,
    pub variables: HashMap<VariableId, Declaration<Scheme>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Declaration<T> {
    pub file: FilePath,
    pub name: InternedString,
    pub span: Span,
    pub value: T,
}

impl Program {
    pub fn traverse(&mut self, mut f: impl FnMut(&mut Expression)) {
        for statement in &mut self.body {
            statement.traverse_mut_inner(&mut f);
        }
    }
}

macro_rules! traverse_impl {
    ($f:expr, $expr:expr, $traverse:ident, &$($mut:tt)?) => {{
        use ExpressionKind::*;

        let f = $f;
        let expr = $expr;

        f(expr);

        match &$($mut)? expr.kind {
            Block(statements, _) => {
                for statement in statements {
                    statement.$traverse(f);
                }
            }
            Call(function, input) => {
                function.$traverse(f);
                input.$traverse(f);
            }
            Function(body, _) => body.$traverse(f),
            When(_, _) => todo!(),
            Initialize(_, value) => value.$traverse(f),
            External(_, _, inputs) => {
                for input in inputs {
                    input.$traverse(f);
                }
            }
            _ => {}
        }
    }};
}

impl Expression {
    pub fn traverse(&self, mut f: impl FnMut(&Expression)) {
        self.traverse_inner(&mut f);
    }

    fn traverse_inner(&self, f: &mut impl FnMut(&Expression)) {
        traverse_impl!(f, self, traverse_inner, &)
    }

    pub fn traverse_mut(&mut self, mut f: impl FnMut(&mut Expression)) {
        self.traverse_mut_inner(&mut f);
    }

    fn traverse_mut_inner(&mut self, f: &mut impl FnMut(&mut Expression)) {
        traverse_impl!(f, self, traverse_mut_inner, &mut)
    }

    pub fn contains_error(&self) -> bool {
        let mut contains_error = false;

        self.traverse(|expr| {
            if matches!(expr.kind, ExpressionKind::Error) {
                contains_error = true;
            }
        });

        contains_error
    }
}

impl<L: Loader> Compiler<L> {
    pub fn typecheck(&mut self, mut files: Vec<lower::File>) -> Program {
        let mut typechecker = Typechecker {
            well_typed: true,
            ctx: Default::default(),
            variables: Default::default(),
            constants: Default::default(),
            structures: Default::default(),
            function_inputs: Default::default(),
            diagnostics: &mut self.diagnostics,
            used_types: Default::default(),
            used_constants: Default::default(),
            used_variables: Default::default(),
        };

        let mut body = Vec::new();

        for file in &mut files {
            for (&id, decl) in &file.declarations.constants {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    let scheme = typechecker
                        .convert_constant_type_annotation(&decl.value.ty, &decl.value.parameters);

                    typechecker.constants.insert(id, scheme);
                }
            }

            for (&id, decl) in &file.declarations.types {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    match &decl.value.kind {
                        lower::TypeKind::Structure(fields, field_names) => {
                            #[allow(clippy::map_entry)] // typechecker is borrowed twice otherwise
                            if !typechecker.structures.contains_key(&id) {
                                let fields = field_names
                                    .iter()
                                    .map(|(name, index)| {
                                        (
                                            *name,
                                            (
                                                *index,
                                                typechecker
                                                    .convert_type_annotation(&fields[*index].ty),
                                            ),
                                        )
                                    })
                                    .collect();

                                typechecker.structures.insert(id, fields);
                            }
                        }
                        lower::TypeKind::Enumeration(_, _) => todo!("enumerations"),
                        _ => {}
                    }
                }
            }

            let (_, block) = typechecker.typecheck_block(&mem::take(&mut file.block), file);
            body.extend(block);
        }

        macro_rules! ensure_well_typed {
            ($expr:expr) => {{
                let expr = $expr;

                expr.scheme.apply(&typechecker.ctx);

                if !expr.scheme.vars().is_empty() {
                    typechecker.well_typed = false;

                    if !expr.contains_error() {
                        typechecker.diagnostics.add(Diagnostic::error(
                            "cannot determine the type of this expression",
                            vec![Note::primary(
                                expr.span,
                                "try annotating the type with '::'",
                            )],
                        ));
                    }
                }
            }};
        }

        let mut declarations = Declarations::default();
        for file in &mut files {
            for (id, decl) in file.declarations.types.clone() {
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

            for (id, decl) in file.declarations.type_parameters.clone() {
                if let lower::Declaration::Local(decl) = decl {
                    declarations.type_parameters.insert(
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

            for (id, decl) in file.declarations.constants.clone() {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    let scheme = typechecker.constants.get(&id).unwrap().clone();

                    let mut value =
                        typechecker.typecheck_expr(&decl.value.value.take().unwrap(), file);

                    {
                        let ty = match &scheme {
                            Scheme::Type(ty) => ty.clone(),
                            Scheme::ForAll(forall) => forall.ty.clone(),
                        };

                        let value_ty = value.scheme.instantiate(&mut typechecker.ctx);

                        if let Err(errors) = typechecker.ctx.unify(value_ty, ty) {
                            typechecker.report_type_error(value.span, errors, file);
                        }
                    }

                    value.scheme = scheme;
                    value.traverse_mut(|expr| ensure_well_typed!(expr));

                    declarations.constants.insert(
                        id,
                        Declaration {
                            file: file.path,
                            name: decl.name,
                            span: decl.span,
                            value,
                        },
                    );
                }
            }

            for (id, decl) in file.declarations.variables.clone() {
                if let lower::Declaration::Local(decl) = decl {
                    let ty = typechecker.variables.get(&id).unwrap().clone();

                    let mut temp_expr = Expression {
                        span: decl.span,
                        scheme: ty,
                        kind: ExpressionKind::Error, // unused
                    };

                    ensure_well_typed!(&mut temp_expr);

                    declarations.variables.insert(
                        id,
                        Declaration {
                            file: file.path,
                            name: decl.name,
                            span: decl.span,
                            value: temp_expr.scheme,
                        },
                    );
                }
            }
        }

        let mut top_level = HashMap::new();
        for file in &files {
            top_level.extend(&file.exported);
        }

        let mut program = Program {
            well_typed: true,
            body,
            declarations,
            top_level,
        };

        program.traverse(|expr| ensure_well_typed!(expr));
        program.well_typed = typechecker.well_typed;

        // TODO: Make this the default (remove option)
        if self.options.warn_unused_variables {
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
                constants => ("constant", used_constants),
                variables => ("variable", used_variables),
            );
        }

        program
    }
}

macro_rules! builtin_types {
    ($($x:ident => $name:literal),* $(,)?) => {
        pub struct BuiltinTypes {
            $(pub $x: Type,)*
        }

        lazy_static! {
            pub static ref BUILTIN_TYPES: BuiltinTypes = BuiltinTypes {
                $($x: Type::Named(TypeId::new(), Vec::new()),)*
            };
        }

        impl BuiltinTypes {
            pub fn name(&self, id: TypeId) -> Option<&'static str> {
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
    variables: HashMap<VariableId, Scheme>,
    constants: HashMap<ConstantId, Scheme>,
    structures: HashMap<TypeId, HashMap<InternedString, (usize, Type)>>,
    // TODO: enumerations
    function_inputs: Vec<TypeVariable>,
    diagnostics: &'a mut Diagnostics,
    used_types: HashSet<TypeId>,
    used_constants: HashSet<ConstantId>,
    used_variables: HashSet<VariableId>,
}

impl<'a> Typechecker<'a> {
    fn typecheck_expr(&mut self, expr: &lower::Expression, file: &lower::File) -> Expression {
        let error = || Expression {
            span: expr.span,
            scheme: Scheme::Type(Type::Bottom),
            kind: ExpressionKind::Error,
        };

        let (scheme, kind) = match &expr.kind {
            lower::ExpressionKind::Error => {
                self.well_typed = false; // signal that the program contains errors
                return error();
            }
            lower::ExpressionKind::Unit => (
                Scheme::Type(BUILTIN_TYPES.unit.clone()),
                ExpressionKind::Marker,
            ),
            lower::ExpressionKind::Marker(ty) => (
                Scheme::Type(self.convert_type_id(*ty, file)),
                ExpressionKind::Marker,
            ),
            lower::ExpressionKind::Constant(id) => {
                let scheme = self
                    .constants
                    .get(id)
                    .expect("uninitialized constant")
                    .clone();

                self.used_constants.insert(*id);

                (scheme, ExpressionKind::Constant(*id))
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
            lower::ExpressionKind::Text(text) => (
                Scheme::Type(BUILTIN_TYPES.text.clone()),
                ExpressionKind::Text(*text),
            ),
            lower::ExpressionKind::Number(number) => (
                Scheme::Type(BUILTIN_TYPES.number.clone()),
                ExpressionKind::Number(*number),
            ),
            lower::ExpressionKind::Block(statements, declarations) => {
                let (ty, statements) = self.typecheck_block(statements, file);

                (ty, ExpressionKind::Block(statements, declarations.clone()))
            }
            lower::ExpressionKind::CallOrMember(lhs, input, member_name) => {
                let lhs = self.typecheck_expr(lhs, file);

                if let Some(member_name) = member_name {
                    let mut lhs_ty = lhs.scheme.instantiate(&mut self.ctx);
                    lhs_ty.apply(&self.ctx);

                    if let Type::Named(id, _) = lhs_ty {
                        if let Some(members) = self.structures.get(&id) {
                            match members.get(member_name) {
                                Some((index, ty)) => {
                                    return Expression {
                                        span: expr.span,
                                        scheme: Scheme::Type(ty.clone()),
                                        kind: ExpressionKind::Member(Box::new(lhs), *index),
                                    };
                                }
                                None => {
                                    self.well_typed = false;

                                    self.diagnostics.add(Diagnostic::error(
                                        format!(
                                            "type `{}` has no member `{}`",
                                            file.declarations.types.get(&id).unwrap().name(),
                                            member_name
                                        ),
                                        vec![Note::primary(expr.span, "invalid member")],
                                    ));

                                    return error();
                                }
                            }
                        }
                    } else {
                        self.diagnostics.add(Diagnostic::error(
                            format!("cannot find variable `{member_name}`"),
                            vec![Note::primary(input.span, "no such variable")],
                        ));
                    }
                }

                let function = lhs;
                let input = self.typecheck_expr(input, file);

                let output_ty = Type::Variable(self.ctx.new_variable());

                let function_ty = function.scheme.instantiate(&mut self.ctx);
                let input_ty = input.scheme.instantiate(&mut self.ctx);

                if let Err(errors) = self.ctx.unify(
                    function_ty,
                    Type::Function(Box::new(input_ty), Box::new(output_ty.clone())),
                ) {
                    self.report_type_error(function.span, errors, file);
                    return error();
                }

                (
                    Scheme::Type(output_ty),
                    ExpressionKind::Call(Box::new(function), Box::new(input)),
                )
            }
            lower::ExpressionKind::Function(body, captures) => {
                let body = self.typecheck_expr(body, file);

                let body_ty = body.scheme.instantiate(&mut self.ctx);

                let input_var = self
                    .function_inputs
                    .pop()
                    .expect("function body does not refer to function input");

                (
                    Scheme::Type(Type::Function(
                        Box::new(Type::Variable(input_var)),
                        Box::new(body_ty),
                    )),
                    ExpressionKind::Function(
                        Box::new(body),
                        captures.iter().map(|(var, _)| *var).collect(),
                    ),
                )
            }
            lower::ExpressionKind::When(_, _) => todo!(),
            lower::ExpressionKind::External(namespace, identifier, inputs) => {
                let inputs = inputs
                    .iter()
                    .map(|expr| self.typecheck_expr(expr, file))
                    .collect();

                (
                    Scheme::Type(Type::Variable(self.ctx.new_variable())),
                    ExpressionKind::External(*namespace, *identifier, inputs),
                )
            }
            lower::ExpressionKind::Annotate(expr, ty) => {
                let ty = self.convert_type_annotation(ty);
                let inferred_expr = self.typecheck_expr(expr, file);

                let inferred_expr_ty = inferred_expr.scheme.instantiate(&mut self.ctx);

                if let Err(errors) = self.ctx.unify(inferred_expr_ty.clone(), ty) {
                    self.report_type_error(expr.span, errors, file);
                    return error();
                }

                (Scheme::Type(inferred_expr_ty), inferred_expr.kind)
            }
            lower::ExpressionKind::Initialize(id, value) => {
                let value = self.typecheck_expr(value, file);
                self.variables.insert(*id, value.scheme.clone());

                (
                    Scheme::Type(BUILTIN_TYPES.unit.clone()),
                    ExpressionKind::Initialize(*id, Box::new(value)),
                )
            }
            lower::ExpressionKind::FunctionInput => {
                let var = self.ctx.new_variable();
                self.function_inputs.push(var);

                (
                    Scheme::Type(Type::Variable(var)),
                    ExpressionKind::FunctionInput,
                )
            }
            lower::ExpressionKind::Instantiate(ty, fields) => {
                let fields_by_name = match self.structures.get(ty) {
                    Some(structure) => structure.clone(),
                    None => {
                        self.diagnostics.add(Diagnostic::error(
                            "only data types may be instantiated like this",
                            vec![Note::primary(expr.span, "this is not a data type")],
                        ));

                        return error();
                    }
                };

                let mut fields_by_index = fields_by_name.iter().collect::<Vec<_>>();
                fields_by_index.sort_by_key(|(_, (index, _))| *index);

                let mut unpopulated_fields = vec![None; fields_by_index.len()];
                let mut extra_fields = Vec::new();

                for (name, expr) in fields {
                    let (index, ty) = match fields_by_name.get(name) {
                        Some((index, ty)) => (*index, ty.clone()),
                        None => {
                            extra_fields.push(name);
                            continue;
                        }
                    };

                    let value = self.typecheck_expr(expr, file);
                    let value_ty = value.scheme.instantiate(&mut self.ctx);

                    if let Err(errors) = self.ctx.unify(value_ty, ty) {
                        self.report_type_error(expr.span, errors, file);
                        return error();
                    }

                    unpopulated_fields[index] = Some(value);
                }

                if !extra_fields.is_empty() {
                    self.diagnostics.add(Diagnostic::error(
                        "extra fields",
                        vec![Note::primary(
                            expr.span,
                            format!(
                                "try removing {}",
                                extra_fields
                                    .into_iter()
                                    .map(|field| format!("`{}`", field))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        )],
                    ));

                    return error();
                }

                let mut missing_fields = Vec::new();

                let fields = unpopulated_fields
                    .into_iter()
                    .enumerate()
                    .filter_map(|(index, field)| {
                        if field.is_none() {
                            missing_fields.push(*fields_by_index[index].0);
                        }

                        field
                    })
                    .collect::<Vec<_>>();

                if !missing_fields.is_empty() {
                    self.diagnostics.add(Diagnostic::error(
                        "missing fields",
                        vec![Note::primary(
                            expr.span,
                            format!(
                                "try adding {}",
                                missing_fields
                                    .into_iter()
                                    .map(|field| format!("`{}`", field))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        )],
                    ));

                    return error();
                }

                (
                    Scheme::Type(Type::Named(*ty, Vec::new())),
                    ExpressionKind::Structure(fields),
                )
            }
        };

        Expression {
            span: expr.span,
            scheme,
            kind,
        }
    }

    fn typecheck_block(
        &mut self,
        statements: &[lower::Expression],
        file: &lower::File,
    ) -> (Scheme, Vec<Expression>) {
        let statements = statements
            .iter()
            .map(|statement| self.typecheck_expr(statement, file))
            .collect::<Vec<_>>();

        let ty = statements
            .last()
            .map(|statement| statement.scheme.clone())
            .unwrap_or_else(|| Scheme::Type(BUILTIN_TYPES.unit.clone()));

        (ty, statements)
    }

    fn convert_type_id(&mut self, id: TypeId, file: &lower::File) -> Type {
        let decl = file.declarations.types.get(&id).unwrap().clone();
        self.used_types.insert(id);

        match decl {
            lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) => {
                match decl.value.kind {
                    lower::TypeKind::Marker
                    | lower::TypeKind::Structure(_, _)
                    | lower::TypeKind::Enumeration(_, _) => Type::Named(id, Vec::new()), // TODO: parameters
                    lower::TypeKind::Alias(annotation) => self.convert_type_annotation(&annotation),
                    lower::TypeKind::Builtin(ty) => match ty {
                        lower::BuiltinType::Number => BUILTIN_TYPES.number.clone(),
                        lower::BuiltinType::Text => BUILTIN_TYPES.text.clone(),
                    },
                }
            }
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
            lower::TypeAnnotationKind::Parameter(id) => Type::Parameter(*id),
            lower::TypeAnnotationKind::Function(input, output) => Type::Function(
                Box::new(self.convert_type_annotation(input)),
                Box::new(self.convert_type_annotation(output)),
            ),
        }
    }

    fn convert_constant_type_annotation(
        &mut self,
        annotation: &lower::TypeAnnotation,
        parameters: &[lower::TypeParameter],
    ) -> Scheme {
        let ty = self.convert_type_annotation(annotation);

        let params = parameters
            .iter()
            .map(|param| param.id)
            .collect::<HashSet<_>>();

        if params.is_empty() {
            Scheme::Type(ty)
        } else {
            Scheme::ForAll(ForAll { params, ty })
        }
    }

    fn report_type_error(&mut self, span: Span, error: UnificationError, file: &lower::File) {
        self.well_typed = false;

        let type_names = |name| {
            file.declarations
                .types
                .get(&name)
                .map(|decl| decl.name().to_string())
                .unwrap_or_else(|| BUILTIN_TYPES.name(name).unwrap().to_string())
        };

        let param_names = |param| {
            file.declarations
                .type_parameters
                .get(&param)
                .unwrap()
                .name()
                .to_string()
        };

        let diagnostic = match error {
            UnificationError::Recursive(_) => Diagnostic::new(
                DiagnosticLevel::Error,
                "recursive type",
                vec![Note::primary(span, "the type of this references itself")],
            ),
            UnificationError::Mismatch(actual, expected) => Diagnostic::new(
                DiagnosticLevel::Error,
                "mismatched types",
                vec![Note::primary(
                    span,
                    format!(
                        "expected `{}`, but found `{}`",
                        format_type(&expected, type_names, param_names),
                        format_type(&actual, type_names, param_names)
                    ),
                )],
            ),
        };

        self.diagnostics.add(diagnostic);
    }
}

pub fn format_type_scheme(
    ty: &Scheme,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    match ty {
        Scheme::Type(ty) => format_type(ty, type_names, param_names),
        Scheme::ForAll(forall) => {
            let mut names = Vec::new();
            for &var in &forall.params {
                names.push(var);
            }

            format!(
                "{}=> {}",
                names
                    .iter()
                    .map(|param| param_names(*param) + " ")
                    .collect::<String>(),
                format_type_with(&forall.ty, type_names, param_names)
            )
        }
    }
}

pub fn format_type(
    ty: &Type,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    format_type_with(ty, type_names, param_names)
}

fn format_type_with(
    ty: &Type,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    fn format_type(
        ty: &Type,
        type_names: &impl Fn(TypeId) -> String,
        param_names: &impl Fn(TypeParameterId) -> String,
        is_top_level: bool,
        is_return: bool,
    ) -> String {
        match ty {
            Type::Variable(_) => String::from("_"),
            Type::Parameter(param) => param_names(*param),
            Type::Bottom => String::from("!"),
            Type::Named(id, params) => {
                let name = type_names(*id);

                let params = params
                    .iter()
                    .map(|ty| {
                        String::from(" ") + &format_type(ty, type_names, param_names, false, true)
                    })
                    .collect::<String>();

                if is_top_level {
                    format!("{name}{params}")
                } else {
                    format!("({name}{params})")
                }
            }
            Type::Function(input, output) => {
                let input = format_type(input, type_names, param_names, true, false);
                let output = format_type(output, type_names, param_names, true, true);

                if is_top_level && is_return {
                    format!("{input} -> {output}")
                } else {
                    format!("({input} -> {output})")
                }
            }
        }
    }

    format_type(ty, &type_names, &param_names, true, true)
}
