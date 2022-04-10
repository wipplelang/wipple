mod engine;

pub use engine::{BuiltinType, Scheme, Type};

use crate::{
    compile::lower, diagnostics::*, helpers::InternedString, parser::Span, Compiler, ConstantId,
    FilePath, Loader, TraitId, TypeId, TypeParameterId, VariableId,
};
use engine::*;
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
    pub trait_types: HashMap<TypeVariable, TraitId>,
    pub top_level: HashMap<InternedString, lower::ScopeValue>,
}

macro_rules! expr {
    ($vis:vis $($prefix:ident)? $(, { $($kinds:tt)* }, { $($props:tt)* })?) => {
        paste::paste! {
            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis struct [<$($prefix)? Expression>] {
                $vis span: Span,
                $vis scheme: [<$($prefix)? Scheme>],
                $vis kind: [<$($prefix)? ExpressionKind>],
                $($($props)*)?
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis enum [<$($prefix)? ExpressionKind>] {
                Error,
                Marker,
                Constant(ConstantId),
                Variable(VariableId),
                Text(InternedString),
                Number(Decimal),
                Block(
                    Vec<[<$($prefix)? Expression>]>,
                    HashMap<InternedString, lower::ScopeValue>,
                ),
                Call(Box<[<$($prefix)? Expression>]>, Box<[<$($prefix)? Expression>]>),
                Member(Box<[<$($prefix)? Expression>]>, usize),
                Function(Box<[<$($prefix)? Expression>]>, HashSet<VariableId>),
                When(Box<[<$($prefix)? Expression>]>, Vec<[<$($prefix)? Arm>]>),
                External(InternedString, InternedString, Vec<[<$($prefix)? Expression>]>),
                Initialize(VariableId, Box<[<$($prefix)? Expression>]>),
                Structure(Vec<[<$($prefix)? Expression>]>),
                FunctionInput,
                $($($kinds)*)?
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis struct [<$($prefix)? Arm>] {
                $vis span: Span,
                $vis pattern: [<$($prefix)? Pattern>],
                $vis body: [<$($prefix)? Expression>],
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis struct [<$($prefix)? Pattern>] {
                $vis span: Span,
                $vis kind: [<$($prefix)? PatternKind>],
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis enum [<$($prefix)? PatternKind>] {
                Binding(VariableId),
                // TODO: Support complex paths (data fields, variants)
                Wildcard,
            }
        }
    };
}

expr!(Unresolved, { Trait(TraitId) }, { pending_instance: Option<(TraitId, UnresolvedType)> });
expr!(pub);

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub span: Span,
    pub id: TypeId,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Declarations {
    pub types: HashMap<TypeId, Declaration<()>>,
    pub type_parameters: HashMap<TypeParameterId, Declaration<()>>,
    pub traits: HashMap<TraitId, Declaration<()>>,
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

impl<L: Loader> Compiler<L> {
    pub fn typecheck(&mut self, mut files: Vec<lower::File>) -> Option<Program> {
        let mut typechecker = Typechecker {
            well_typed: true,
            ctx: Default::default(),
            variables: Default::default(),
            constants: Default::default(),
            structures: Default::default(),
            traits: Default::default(),
            trait_types: Default::default(),
            function_inputs: Default::default(),
            compiler: self,
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
                    match &decl.value {
                        lower::Type::Structure(fields, field_names) => {
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
                        lower::Type::Enumeration(_, _) => todo!("enumerations"),
                        _ => {}
                    }
                }
            }

            for (&id, decl) in &file.declarations.traits {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    let scheme = typechecker
                        .convert_constant_type_annotation(&decl.value.ty, &decl.value.parameters);

                    typechecker.traits.insert(id, scheme);
                }
            }

            for (&id, decl) in &file.declarations.instances {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    let value = typechecker.typecheck_expr(&decl.value.value, file);

                    let trait_ty = typechecker
                        .traits
                        .get(&decl.value.tr)
                        .unwrap()
                        .clone()
                        .instantiate(&mut typechecker.ctx);

                    let value_ty = value.scheme.instantiate(&mut typechecker.ctx);

                    if let Err(errors) = typechecker.ctx.unify(value_ty, trait_ty) {
                        typechecker.report_type_error(value.span, errors, file);
                    }

                    typechecker.constants.insert(id, value.scheme.clone());

                    typechecker.ctx.register(
                        decl.value.tr,
                        Instance {
                            scheme: value.scheme,
                            constant: id,
                        },
                    );
                }
            }

            let (_, block) = typechecker.typecheck_block(&mem::take(&mut file.block), file);
            body.push((file.clone(), block));
        }

        let mut declarations = Declarations::default();
        for file in &files {
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

            for (id, decl) in file.declarations.traits.clone() {
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

            macro_rules! handle_constant {
                ($id:expr, $decl:ident, $value:expr) => {{
                    let id = $id;
                    let value = $value;

                    let scheme = typechecker.constants.get(&id).unwrap().clone();

                    let mut value = typechecker.typecheck_expr(&value, file);

                    {
                        let ty = match &scheme {
                            UnresolvedScheme::Type(ty) => ty.clone(),
                            UnresolvedScheme::ForAll(forall) => forall.ty.clone(),
                        };

                        let value_ty = value.scheme.instantiate(&mut typechecker.ctx);

                        if let Err(errors) = typechecker.ctx.unify(value_ty, ty) {
                            typechecker.report_type_error(value.span, errors, file);
                        }
                    }

                    value.scheme = scheme;

                    let value = match typechecker.finalize(value) {
                        Ok(value) => value,
                        Err((span, error)) => {
                            typechecker.report_finalize_error(span, error, file);
                            Expression {
                                span,
                                scheme: Scheme::Type(Type::Bottom(true)),
                                kind: ExpressionKind::Error,
                            }
                        }
                    };

                    declarations.constants.insert(
                        id,
                        Declaration {
                            file: file.path,
                            name: $decl.name,
                            span: $decl.span,
                            value,
                        },
                    );
                }};
            }

            for (id, decl) in file.declarations.constants.clone() {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    handle_constant!(id, decl, decl.value.value.take().unwrap());
                }
            }

            for (id, decl) in file.declarations.instances.clone() {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    handle_constant!(id, decl, decl.value.value);
                }
            }
        }

        let mut top_level = HashMap::new();
        for file in &files {
            top_level.extend(&file.exported);
        }

        match body
            .iter()
            .map(|(file, expr)| {
                expr.clone()
                    .into_iter()
                    .map(|expr| {
                        let expr = typechecker.finalize(expr)?;

                        expr.traverse(|expr| {
                            if let ExpressionKind::Initialize(id, expr) = &expr.kind {
                                let decl = file.declarations.variables.get(id).unwrap();

                                declarations.variables.insert(
                                    *id,
                                    Declaration {
                                        file: file.path,
                                        name: decl.name(),
                                        span: decl.span(),
                                        value: expr.scheme.clone(),
                                    },
                                );
                            }
                        });

                        Ok(expr)
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(|(span, error)| (span, error, file as &_))
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|body| body.into_iter().flatten().collect::<Vec<_>>())
        {
            Ok(body) => {
                let program = Program {
                    well_typed: typechecker.well_typed,
                    body,
                    declarations,
                    trait_types: typechecker.trait_types.1,
                    top_level,
                };

                // TODO: Make this the default (remove option)
                // TODO: Move this to its own 'passes' section of the compiler
                if typechecker.compiler.options.warn_unused_variables {
                    macro_rules! warn_unused {
                        ($($x:ident => ($name:literal, $used:ident),)*) => {
                            $(
                                for (id, decl) in &program.declarations.$x {
                                    if !typechecker.$used.contains(id) {
                                        typechecker.compiler.diagnostics.add(Diagnostic::warning(
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

                Some(program)
            }
            Err((span, error, file)) => {
                typechecker.report_finalize_error(span, error, file);

                None
            }
        }
    }
}

struct Typechecker<'a, L: Loader> {
    well_typed: bool,
    ctx: Context,
    variables: HashMap<VariableId, UnresolvedScheme>,
    constants: HashMap<ConstantId, UnresolvedScheme>,
    structures: HashMap<TypeId, HashMap<InternedString, (usize, UnresolvedType)>>,
    traits: HashMap<TraitId, UnresolvedScheme>,
    trait_types: (
        HashMap<TraitId, TypeVariable>,
        HashMap<TypeVariable, TraitId>,
    ),
    // TODO: enumerations
    function_inputs: Vec<TypeVariable>,
    compiler: &'a mut Compiler<L>,
    used_types: HashSet<TypeId>,
    used_constants: HashSet<ConstantId>,
    used_variables: HashSet<VariableId>,
}

impl<'a, L: Loader> Typechecker<'a, L> {
    fn typecheck_expr(
        &mut self,
        expr: &lower::Expression,
        file: &lower::File,
    ) -> UnresolvedExpression {
        let error = || UnresolvedExpression {
            span: expr.span,
            scheme: UnresolvedScheme::Type(UnresolvedType::Bottom(true)),
            kind: UnresolvedExpressionKind::Error,
            pending_instance: None,
        };

        let (scheme, kind, pending_instance) = match &expr.kind {
            lower::ExpressionKind::Error => {
                self.well_typed = false; // signal that the program contains errors
                return error();
            }
            lower::ExpressionKind::Unit => (
                UnresolvedScheme::Type(UnresolvedType::Builtin(BuiltinType::Unit)),
                UnresolvedExpressionKind::Marker,
                None,
            ),
            lower::ExpressionKind::Marker(ty) => (
                UnresolvedScheme::Type(self.convert_type_id(*ty, file)),
                UnresolvedExpressionKind::Marker,
                None,
            ),
            lower::ExpressionKind::Constant(id) => {
                let scheme = self
                    .constants
                    .get(id)
                    .expect("uninitialized constant")
                    .clone();

                self.used_constants.insert(*id);

                (scheme, UnresolvedExpressionKind::Constant(*id), None)
            }
            lower::ExpressionKind::Trait(id) => {
                let var = self.ctx.new_variable();
                self.trait_types.0.insert(*id, var);
                self.trait_types.1.insert(var, *id);

                (
                    UnresolvedScheme::Type(UnresolvedType::Variable(var)),
                    UnresolvedExpressionKind::Trait(*id),
                    None,
                )
            }
            lower::ExpressionKind::Variable(id) => {
                let ty = self
                    .variables
                    .get(id)
                    .expect("uninitialized variable")
                    .clone();

                self.used_variables.insert(*id);

                (ty, UnresolvedExpressionKind::Variable(*id), None)
            }
            lower::ExpressionKind::Text(text) => (
                UnresolvedScheme::Type(UnresolvedType::Builtin(BuiltinType::Text)),
                UnresolvedExpressionKind::Text(*text),
                None,
            ),
            lower::ExpressionKind::Number(number) => (
                UnresolvedScheme::Type(UnresolvedType::Builtin(BuiltinType::Number)),
                UnresolvedExpressionKind::Number(*number),
                None,
            ),
            lower::ExpressionKind::Block(statements, declarations) => {
                let (ty, statements) = self.typecheck_block(statements, file);

                (
                    ty,
                    UnresolvedExpressionKind::Block(statements, declarations.clone()),
                    None,
                )
            }
            lower::ExpressionKind::Call(lhs, input) => {
                let function = self.typecheck_expr(lhs, file);
                let input = self.typecheck_expr(input, file);

                let output_ty = UnresolvedType::Variable(self.ctx.new_variable());

                let function_ty = function.scheme.instantiate(&mut self.ctx);
                let input_ty = input.scheme.instantiate(&mut self.ctx);

                if let Err(errors) = self.ctx.unify(
                    function_ty,
                    UnresolvedType::Function(Box::new(input_ty), Box::new(output_ty.clone())),
                ) {
                    self.report_type_error(function.span, errors, file);
                    return error();
                }

                (
                    UnresolvedScheme::Type(output_ty),
                    UnresolvedExpressionKind::Call(Box::new(function), Box::new(input)),
                    None,
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
                    UnresolvedScheme::Type(UnresolvedType::Function(
                        Box::new(UnresolvedType::Variable(input_var)),
                        Box::new(body_ty),
                    )),
                    UnresolvedExpressionKind::Function(
                        Box::new(body),
                        captures.iter().map(|(var, _)| *var).collect(),
                    ),
                    None,
                )
            }
            lower::ExpressionKind::When(_, _) => todo!(),
            lower::ExpressionKind::External(namespace, identifier, inputs) => {
                let inputs = inputs
                    .iter()
                    .map(|expr| self.typecheck_expr(expr, file))
                    .collect();

                (
                    UnresolvedScheme::Type(UnresolvedType::Variable(self.ctx.new_variable())),
                    UnresolvedExpressionKind::External(*namespace, *identifier, inputs),
                    None,
                )
            }
            lower::ExpressionKind::Annotate(expr, ty) => {
                let ty = self.convert_type_annotation(ty);
                let expr = self.typecheck_expr(expr, file);

                let expr_ty = expr.scheme.instantiate(&mut self.ctx);

                if let Err(errors) = self.ctx.unify(expr_ty, ty.clone()) {
                    self.report_type_error(expr.span, errors, file);
                    return error();
                }

                (UnresolvedScheme::Type(ty), expr.kind, None)
            }
            lower::ExpressionKind::Initialize(id, value) => {
                let value = self.typecheck_expr(value, file);
                self.variables.insert(*id, value.scheme.clone());

                (
                    UnresolvedScheme::Type(UnresolvedType::Builtin(BuiltinType::Unit)),
                    UnresolvedExpressionKind::Initialize(*id, Box::new(value)),
                    None,
                )
            }
            lower::ExpressionKind::FunctionInput => {
                let var = self.ctx.new_variable();
                self.function_inputs.push(var);

                (
                    UnresolvedScheme::Type(UnresolvedType::Variable(var)),
                    UnresolvedExpressionKind::FunctionInput,
                    None,
                )
            }
            lower::ExpressionKind::Instantiate(ty, fields) => {
                let fields_by_name = match self.structures.get(ty) {
                    Some(structure) => structure.clone(),
                    None => {
                        self.compiler.diagnostics.add(Diagnostic::error(
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
                    self.compiler.diagnostics.add(Diagnostic::error(
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
                    self.compiler.diagnostics.add(Diagnostic::error(
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
                    UnresolvedScheme::Type(UnresolvedType::Named(*ty)), // TODO: parameters
                    UnresolvedExpressionKind::Structure(fields),
                    None,
                )
            }
        };

        UnresolvedExpression {
            span: expr.span,
            scheme,
            kind,
            pending_instance,
        }
    }

    fn typecheck_block(
        &mut self,
        statements: &[lower::Expression],
        file: &lower::File,
    ) -> (UnresolvedScheme, Vec<UnresolvedExpression>) {
        let statements = statements
            .iter()
            .map(|statement| self.typecheck_expr(statement, file))
            .collect::<Vec<_>>();

        let ty = statements
            .last()
            .map(|statement| statement.scheme.clone())
            .unwrap_or_else(|| UnresolvedScheme::Type(UnresolvedType::Builtin(BuiltinType::Unit)));

        (ty, statements)
    }

    fn convert_type_id(&mut self, id: TypeId, file: &lower::File) -> UnresolvedType {
        let decl = file.declarations.types.get(&id).unwrap().clone();
        self.used_types.insert(id);

        match decl {
            lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) => {
                match decl.value {
                    lower::Type::Marker
                    | lower::Type::Structure(_, _)
                    | lower::Type::Enumeration(_, _) => UnresolvedType::Named(id), // TODO: parameters
                    lower::Type::Alias(annotation) => self.convert_type_annotation(&annotation),
                }
            }
            lower::Declaration::Dependency(_) => UnresolvedType::Named(id), // TODO: parameters
        }
    }

    fn finalize(
        &mut self,
        expr: UnresolvedExpression,
    ) -> Result<Expression, (Span, FinalizeError)> {
        Ok(Expression {
            span: expr.span,
            kind: match expr.kind {
                UnresolvedExpressionKind::Error => ExpressionKind::Error,
                UnresolvedExpressionKind::Marker => ExpressionKind::Marker,
                UnresolvedExpressionKind::Constant(constant) => ExpressionKind::Constant(constant),
                UnresolvedExpressionKind::Variable(var) => ExpressionKind::Variable(var),
                UnresolvedExpressionKind::Text(text) => ExpressionKind::Text(text),
                UnresolvedExpressionKind::Number(number) => ExpressionKind::Number(number),
                UnresolvedExpressionKind::Block(statements, scope) => ExpressionKind::Block(
                    statements
                        .into_iter()
                        .map(|expr| self.finalize(expr))
                        .collect::<Result<_, _>>()?,
                    scope,
                ),
                UnresolvedExpressionKind::Call(func, input) => ExpressionKind::Call(
                    Box::new(self.finalize(*func)?),
                    Box::new(self.finalize(*input)?),
                ),
                UnresolvedExpressionKind::Member(expr, index) => {
                    ExpressionKind::Member(Box::new(self.finalize(*expr)?), index)
                }
                UnresolvedExpressionKind::Function(body, captures) => {
                    ExpressionKind::Function(Box::new(self.finalize(*body)?), captures)
                }
                UnresolvedExpressionKind::When(_, _) => todo!(),
                UnresolvedExpressionKind::External(namespace, identifier, inputs) => {
                    ExpressionKind::External(
                        namespace,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|expr| self.finalize(expr))
                            .collect::<Result<_, _>>()?,
                    )
                }
                UnresolvedExpressionKind::Initialize(variable, value) => {
                    ExpressionKind::Initialize(variable, Box::new(self.finalize(*value)?))
                }
                UnresolvedExpressionKind::Structure(fields) => ExpressionKind::Structure(
                    fields
                        .into_iter()
                        .map(|expr| self.finalize(expr))
                        .collect::<Result<_, _>>()?,
                ),
                UnresolvedExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
                UnresolvedExpressionKind::Trait(id) => {
                    let var = *self.trait_types.0.get(&id).unwrap();

                    let mut ty = UnresolvedType::Variable(var);
                    ty.apply(&self.ctx);

                    match self.ctx.instance_for(id, ty) {
                        Ok(instance) => ExpressionKind::Constant(instance),
                        Err(error) => return Err((expr.span, error)),
                    }
                }
            },
            // the scheme must be resolved after the kind because the kind may
            // be a trait, whose type is unified with the original type variable
            scheme: expr
                .scheme
                .finalize(&self.ctx)
                .map_err(|error| (expr.span, error))?,
        })
    }

    fn convert_type_annotation(&mut self, annotation: &lower::TypeAnnotation) -> UnresolvedType {
        match &annotation.kind {
            lower::TypeAnnotationKind::Error => UnresolvedType::Bottom(true),
            lower::TypeAnnotationKind::Placeholder => {
                UnresolvedType::Variable(self.ctx.new_variable())
            }
            lower::TypeAnnotationKind::Unit => UnresolvedType::Builtin(BuiltinType::Unit),
            lower::TypeAnnotationKind::Named(id, _params) => UnresolvedType::Named(*id), // TODO: parameters
            lower::TypeAnnotationKind::Parameter(id) => UnresolvedType::Parameter(*id),
            lower::TypeAnnotationKind::Builtin(builtin) => UnresolvedType::Builtin(match builtin {
                lower::BuiltinType::Unit => BuiltinType::Unit,
                lower::BuiltinType::Number => BuiltinType::Number,
                lower::BuiltinType::Text => BuiltinType::Text,
            }),
            lower::TypeAnnotationKind::Function(input, output) => UnresolvedType::Function(
                Box::new(self.convert_type_annotation(input)),
                Box::new(self.convert_type_annotation(output)),
            ),
        }
    }

    fn convert_constant_type_annotation(
        &mut self,
        annotation: &lower::TypeAnnotation,
        parameters: &[lower::TypeParameter],
    ) -> UnresolvedScheme {
        let ty = self.convert_type_annotation(annotation);

        let params = parameters
            .iter()
            .map(|param| param.id)
            .collect::<HashSet<_>>();

        if params.is_empty() {
            UnresolvedScheme::Type(ty)
        } else {
            UnresolvedScheme::ForAll(UnresolvedForAll { params, ty })
        }
    }
}

macro_rules! format_support {
    ($typechecker:expr, $file:expr) => {{
        let typechecker = $typechecker;
        let file = $file;

        let type_names = |name| {
            file.declarations
                .types
                .get(&name)
                .unwrap()
                .name()
                .to_string()
        };

        let param_names = |param| {
            file.declarations
                .type_parameters
                .get(&param)
                .unwrap()
                .name()
                .to_string()
        };

        let trait_names = |var| {
            typechecker
                .trait_types
                .1
                .get(&var)
                .map(|tr| file.declarations.traits.get(tr).unwrap().name().to_string())
        };

        (type_names, param_names, trait_names)
    }};
}

impl<'a, L: Loader> Typechecker<'a, L> {
    fn report_type_error(&mut self, span: Span, error: UnificationError, file: &lower::File) {
        self.well_typed = false;

        if let UnificationError::Mismatch(actual, expected) = &error {
            if actual.contains_error() || expected.contains_error() {
                return;
            }
        }

        let (type_names, param_names, trait_names) = format_support!(&self, file);

        let diagnostic = match error {
            UnificationError::Recursive(_) => Diagnostic::error(
                "recursive type",
                vec![Note::primary(span, "the type of this references itself")],
            ),
            UnificationError::Mismatch(actual, expected) => Diagnostic::error(
                "mismatched types",
                vec![Note::primary(
                    span,
                    format!(
                        "expected `{}`, but found `{}`",
                        format_type(&expected, type_names, param_names, trait_names),
                        format_type(&actual, type_names, param_names, trait_names)
                    ),
                )],
            ),
        };

        self.compiler.diagnostics.add(diagnostic);
    }

    fn report_finalize_error(&mut self, span: Span, error: FinalizeError, file: &lower::File) {
        self.well_typed = false;

        let (type_names, param_names, trait_names) = format_support!(&self, file);

        let diagnostic = match error {
            FinalizeError::UnknownType => Diagnostic::error(
                "could not determine the type of this expression",
                vec![Note::primary(span, "try annotating the type with '::'")],
            ),
            FinalizeError::MissingInstance(tr, ty) => Diagnostic::error(
                "missing instance",
                vec![Note::primary(
                    span,
                    format!(
                        "could not find instance of `{}` for type `{}`",
                        file.declarations.traits.get(&tr).unwrap().name(),
                        format_type(&ty, type_names, param_names, trait_names)
                    ),
                )],
            ),
        };

        self.compiler.diagnostics.add(diagnostic);
    }
}

impl Program {
    pub fn traverse(&mut self, mut f: impl FnMut(&mut Expression)) {
        for statement in &mut self.body {
            statement.traverse_mut_inner(&mut f);
        }
    }
}

macro_rules! traverse_impl {
    ($kind:ident, $f:expr, $expr:expr, $traverse:ident, &$($mut:tt)?) => {{
        use $kind::*;

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

macro_rules! traverse {
    ($($prefix:ident)?) => {
        paste::paste! {
            #[allow(unused)]
            impl [<$($prefix)? Expression>] {
                pub fn traverse(&self, mut f: impl FnMut(&Self)) {
                    self.traverse_inner(&mut f);
                }

                fn traverse_inner(&self, f: &mut impl FnMut(&Self)) {
                    traverse_impl!([<$($prefix)? ExpressionKind>], f, self, traverse_inner, &)
                }

                pub fn traverse_mut(&mut self, mut f: impl FnMut(&mut Self)) {
                    self.traverse_mut_inner(&mut f);
                }

                fn traverse_mut_inner(&mut self, f: &mut impl FnMut(&mut Self)) {
                    traverse_impl!([<$($prefix)? ExpressionKind>], f, self, traverse_mut_inner, &mut)
                }

                pub fn contains_error(&self) -> bool {
                    let mut contains_error = false;

                    self.traverse(|expr| {
                        if matches!(expr.kind, [<$($prefix)? ExpressionKind>]::Error) {
                            contains_error = true;
                        }
                    });

                    contains_error
                }
            }
        }
    }
}

traverse!(Unresolved);
traverse!();

pub fn format_type_scheme(
    ty: &UnresolvedScheme,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
    trait_names: impl Fn(TypeVariable) -> Option<String>,
) -> String {
    match ty {
        UnresolvedScheme::Type(ty) => format_type(ty, type_names, param_names, trait_names),
        UnresolvedScheme::ForAll(forall) => {
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
                format_type_with(&forall.ty, type_names, param_names, trait_names)
            )
        }
    }
}

pub fn format_type(
    ty: &UnresolvedType,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
    trait_names: impl Fn(TypeVariable) -> Option<String>,
) -> String {
    format_type_with(ty, type_names, param_names, trait_names)
}

fn format_type_with(
    ty: &UnresolvedType,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
    trait_names: impl Fn(TypeVariable) -> Option<String>,
) -> String {
    fn format_type(
        ty: &UnresolvedType,
        type_names: &impl Fn(TypeId) -> String,
        param_names: &impl Fn(TypeParameterId) -> String,
        trait_names: &impl Fn(TypeVariable) -> Option<String>,
        is_top_level: bool,
        is_return: bool,
    ) -> String {
        match ty {
            UnresolvedType::Variable(var) => trait_names(*var).unwrap_or_else(|| String::from("_")),
            UnresolvedType::Parameter(param) => param_names(*param),
            UnresolvedType::Bottom(_) => String::from("!"),
            UnresolvedType::Named(id) => {
                let name = type_names(*id);

                if is_top_level {
                    name
                } else {
                    format!("({name})")
                }
            }
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::Unit => String::from("()"),
                BuiltinType::Text => String::from("Text"),
                BuiltinType::Number => String::from("Number"),
            },
            UnresolvedType::Function(input, output) => {
                let input = format_type(input, type_names, param_names, trait_names, true, false);
                let output = format_type(output, type_names, param_names, trait_names, true, true);

                if is_top_level && is_return {
                    format!("{input} -> {output}")
                } else {
                    format!("({input} -> {output})")
                }
            }
        }
    }

    format_type(ty, &type_names, &param_names, &trait_names, true, true)
}
