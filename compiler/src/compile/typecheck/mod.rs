#![allow(clippy::type_complexity)]

mod engine;
mod format;
mod traverse;

pub use engine::{BuiltinType, Type};
pub use format::format_type;

use crate::{
    compile::lower, diagnostics::*, helpers::InternedString, parse::Span, Compiler, FilePath,
    GenericConstantId, Loader, MonomorphizedConstantId, TraitId, TypeId, TypeParameterId,
    VariableId,
};
use engine::*;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap},
    mem,
    rc::Rc,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub well_typed: bool,
    pub body: Vec<Expression>,
    pub declarations: Declarations<Expression, Type>,
    pub top_level: HashMap<InternedString, lower::ScopeValue>,
}

macro_rules! expr {
    ($vis:vis, $prefix:literal, $type:ident, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis struct [<$prefix Expression>] {
                $vis span: Span,
                $vis ty: $type,
                $vis kind: [<$prefix ExpressionKind>],
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis enum [<$prefix ExpressionKind>] {
                Marker,
                Variable(VariableId),
                Text(InternedString),
                Number(Decimal),
                Block(
                    Vec<[<$prefix Expression>]>,
                    HashMap<InternedString, lower::ScopeValue>,
                ),
                Call(Box<[<$prefix Expression>]>, Box<[<$prefix Expression>]>),
                Function(Box<[<$prefix Expression>]>, BTreeSet<VariableId>),
                When(Box<[<$prefix Expression>]>, Vec<[<$prefix Arm>]>),
                External(InternedString, InternedString, Vec<[<$prefix Expression>]>),
                Initialize(VariableId, Box<[<$prefix Expression>]>),
                Structure(Vec<[<$prefix Expression>]>),
                FunctionInput,
                $($kinds)*
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis struct [<$prefix Arm>] {
                $vis span: Span,
                $vis pattern: [<$prefix Pattern>],
                $vis body: [<$prefix Expression>],
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis struct [<$prefix Pattern>] {
                $vis span: Span,
                $vis kind: [<$prefix PatternKind>],
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis enum [<$prefix PatternKind>] {
                Binding(VariableId),
                // TODO: Support complex paths (data fields, variants)
                Wildcard,
            }
        }
    };
}

expr!(, "Unresolved", UnresolvedType, {
    Error,
    Trait(TraitId),
    Constant(GenericConstantId),
    Member(Box<UnresolvedExpression>, InternedString),
});

expr!(, "Monomorphized", UnresolvedType, {
    Error,
    Constant(MonomorphizedConstantId),
    Member(Box<MonomorphizedExpression>, usize),
});

expr!(pub, "", Type, {
    Constant(MonomorphizedConstantId),
    Member(Box<Expression>, usize),
});

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Declarations<Expr, Ty> {
    pub types: BTreeMap<TypeId, Declaration<()>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<()>>,
    pub traits: BTreeMap<TraitId, Declaration<(UnresolvedType, Vec<TypeParameterId>)>>,
    pub generic_constants: Vec<Declaration<Expr>>,
    pub monomorphized_constants: BTreeMap<MonomorphizedConstantId, Declaration<Expr>>,
    pub variables: BTreeMap<VariableId, Declaration<Ty>>,
}

impl<Expr, Ty> Default for Declarations<Expr, Ty> {
    fn default() -> Self {
        Self {
            types: Default::default(),
            type_parameters: Default::default(),
            traits: Default::default(),
            generic_constants: Default::default(),
            monomorphized_constants: Default::default(),
            variables: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Declaration<T> {
    pub name: InternedString,
    pub span: Span,
    pub value: T,
}

pub enum Progress {
    Typechecking {
        path: FilePath,
        current: usize,
        total: usize,
    },
    Finalizing,
}

impl<L: Loader> Compiler<L> {
    pub fn typecheck(&mut self, files: Vec<lower::File>) -> Option<Program> {
        self.typecheck_with_progress(files, |_| {})
    }

    pub fn typecheck_with_progress(
        &mut self,
        files: Vec<lower::File>,
        mut progress: impl FnMut(Progress),
    ) -> Option<Program> {
        let mut files = files
            .into_iter()
            .map(|file| Rc::new(RefCell::new(file)))
            .collect::<Vec<_>>();

        let mut typechecker = Typechecker {
            well_typed: true,
            ctx: Default::default(),
            variables: Default::default(),
            structures: Default::default(),
            traits: Default::default(),
            generic_constants: Default::default(),
            monomorphized_constants: Default::default(),
            declared_instances: Default::default(),
            bound_instances: Default::default(),
            function_inputs: Default::default(),
            declarations: Default::default(),
            compiler: self,
        };

        let total_files = files.len();

        // Copy declarations

        for file in &files {
            for (id, decl) in file.borrow().declarations.types.clone() {
                if let lower::Declaration::Local(decl) = decl {
                    typechecker.declarations.types.insert(
                        id,
                        Declaration {
                            name: decl.name,
                            span: decl.span,
                            value: (),
                        },
                    );
                }
            }

            for (id, decl) in file.borrow().declarations.type_parameters.clone() {
                if let lower::Declaration::Local(decl) = decl {
                    typechecker.declarations.type_parameters.insert(
                        id,
                        Declaration {
                            name: decl.name,
                            span: decl.span,
                            value: (),
                        },
                    );
                }
            }

            for (id, decl) in file.borrow().declarations.traits.clone() {
                if let lower::Declaration::Local(decl) = decl {
                    let ty = typechecker.convert_type_annotation(&decl.value.ty);

                    let parameters = decl
                        .value
                        .parameters
                        .into_iter()
                        .map(|param| param.id)
                        .collect();

                    typechecker.declarations.traits.insert(
                        id,
                        Declaration {
                            name: decl.name,
                            span: decl.span,
                            value: (ty, parameters),
                        },
                    );
                }
            }
        }

        // Typecheck expressions in the program and collect constraints

        let mut body = Vec::new();
        for (index, file) in files.iter_mut().enumerate() {
            progress(Progress::Typechecking {
                path: file.borrow().span.path,
                current: index + 1,
                total: total_files,
            });

            for (&id, decl) in &file.borrow().declarations.types {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    match &decl.value {
                        lower::Type::Structure(fields, field_names) => {
                            #[allow(clippy::map_entry)] // `typechecker` is borrowed twice otherwise
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

            for (&id, decl) in &file.borrow().declarations.traits {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    let scheme = typechecker.convert_type_annotation(&decl.value.ty);
                    let params = decl.value.parameters.iter().map(|param| param.id).collect();
                    typechecker.traits.insert(id, (scheme, params));
                }
            }

            for (&id, decl) in &file.borrow().declarations.constants {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    let value = decl.value.value.borrow().as_ref().unwrap().clone();
                    let generic_ty = typechecker.convert_type_annotation(&decl.value.ty);

                    let bounds = decl
                        .value
                        .bounds
                        .iter()
                        .map(|bound| {
                            let (mut trait_ty, params) =
                                typechecker.traits.get(&bound.tr).unwrap().clone();

                            let substitutions = params
                                .into_iter()
                                .zip(&bound.parameters)
                                .map(|(param, ty)| (param, typechecker.convert_type_annotation(ty)))
                                .collect();

                            trait_ty.instantiate_with(&substitutions);

                            (bound.tr, trait_ty, bound.span)
                        })
                        .collect::<Vec<_>>();

                    typechecker.generic_constants.insert(
                        id,
                        Constant {
                            file: file.clone(),
                            decl: Declaration {
                                name: decl.name,
                                span: decl.span,
                                value,
                            },
                            generic_ty,
                            bounds,
                        },
                    );
                }
            }

            for decl in file.borrow().declarations.instances.values() {
                if let lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) = decl {
                    let value = decl.value.value.clone();

                    let (mut trait_ty, params) =
                        typechecker.traits.get(&decl.value.tr).unwrap().clone();

                    let substitutions = params
                        .into_iter()
                        .zip(&decl.value.parameters)
                        .map(|(param, ty)| (param, typechecker.convert_type_annotation(ty)))
                        .collect();

                    trait_ty.instantiate_with(&substitutions);

                    let generic_instance_id = typechecker.compiler.new_generic_constant_id();

                    typechecker
                        .declared_instances
                        .entry(decl.value.tr)
                        .or_default()
                        .push(generic_instance_id);

                    typechecker.generic_constants.insert(
                        generic_instance_id,
                        Constant {
                            file: file.clone(),
                            decl: Declaration {
                                name: decl.name,
                                span: decl.span,
                                value,
                            },
                            generic_ty: trait_ty,
                            bounds: Vec::new(), // TODO: Generic instances
                        },
                    );
                }
            }

            let block = mem::take(&mut file.borrow_mut().block);
            let (_, block) = typechecker.typecheck_block(&block, file, false);
            body.push((file.clone(), block));
        }

        let prev_bound_instances = typechecker.bound_instances.clone();

        for constant in typechecker.generic_constants.clone().into_values() {
            let mut body = typechecker.typecheck_expr(&constant.decl.value, &constant.file, false);

            if let Err(error) = typechecker.ctx.unify(body.ty.clone(), constant.generic_ty) {
                typechecker.report_type_error(error, body.span);
            }

            body.traverse_mut(|expr| expr.ty.apply(&typechecker.ctx));

            // Register dummy generic instances so finalization works -- these
            // constants will never actually be used
            for (tr, ty, span) in constant.bounds {
                let dummy_id = typechecker.compiler.new_monomorphized_constant_id();

                typechecker.monomorphized_constants.insert(
                    dummy_id,
                    (
                        span,
                        Constant {
                            file: constant.file.clone(),
                            decl: Declaration {
                                name: constant.decl.name,
                                span,
                                value: UnresolvedExpression {
                                    span: constant.decl.value.span,
                                    ty,
                                    kind: UnresolvedExpressionKind::Error,
                                },
                            },
                            generic_ty: (),
                            // TODO: Generic instances (do we even need to add
                            // bounds here?)
                            bounds: Default::default(),
                        },
                    ),
                );

                typechecker
                    .bound_instances
                    .entry(tr)
                    .or_default()
                    .push(dummy_id);
            }

            let body_span = body.span;
            let body = match typechecker.monomorphize(body) {
                Ok(expr) => expr,
                Err(error) => {
                    typechecker.report_type_error(error, body_span);

                    MonomorphizedExpression {
                        span: body_span,
                        ty: UnresolvedType::Bottom(BottomTypeReason::Error),
                        kind: MonomorphizedExpressionKind::Error,
                    }
                }
            };

            typechecker
                .declarations
                .generic_constants
                .push(Declaration {
                    name: constant.decl.name,
                    span: constant.decl.span,
                    value: body,
                });
        }

        typechecker.bound_instances = prev_bound_instances;

        // Finalize the types of all values in the program, resolving traits and
        // raising an error for unresolved type variables

        progress(Progress::Finalizing);

        let mut top_level = HashMap::new();
        for file in &files {
            top_level.extend(&file.borrow().exported);
        }

        let body = match body
            .iter()
            .map(|(file, exprs)| {
                exprs
                    .iter()
                    .cloned()
                    .map(|expr| {
                        let span = expr.span;

                        let expr = typechecker
                            .monomorphize(expr)
                            .map_err(|error| (error, span))?;

                        expr.traverse(|expr| {
                            if let MonomorphizedExpressionKind::Initialize(id, expr) = &expr.kind {
                                let file = file.borrow();

                                if let Some(decl) = file.declarations.variables.get(id) {
                                    typechecker.declarations.variables.insert(
                                        *id,
                                        Declaration {
                                            name: decl.name(),
                                            span: decl.span(),
                                            value: expr.ty.clone(),
                                        },
                                    );
                                }
                            }
                        });

                        Ok(expr)
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|body| body.into_iter().flatten().collect::<Vec<_>>())
        {
            Ok(body) => body,
            Err((error, span)) => {
                typechecker.report_type_error(error, span);
                return None;
            }
        };

        let mut already_checked = BTreeSet::new();
        loop {
            if already_checked.is_superset(
                &typechecker
                    .monomorphized_constants
                    .keys()
                    .copied()
                    .collect(),
            ) {
                break;
            }

            for (id, (span, constant)) in typechecker.monomorphized_constants.clone() {
                if already_checked.contains(&id) {
                    continue;
                }

                already_checked.insert(id);

                let mut ty = constant.decl.value.ty.clone();
                ty.apply(&typechecker.ctx);

                if ty.params().is_empty() {
                    for (tr, mut ty, span) in constant.bounds.clone() {
                        ty.apply(&typechecker.ctx);

                        let instance_id = match typechecker.instance_for(tr, ty.clone(), span) {
                            Ok(id) => id,
                            Err(error) => {
                                typechecker.report_type_error(error, span);
                                continue;
                            }
                        };

                        typechecker
                            .bound_instances
                            .entry(tr)
                            .or_default()
                            .push(instance_id);
                    }

                    let body = match typechecker.monomorphize(constant.decl.value.clone()) {
                        Ok(value) => value,
                        Err(error) => {
                            typechecker.report_type_error(error, span);
                            continue;
                        }
                    };

                    typechecker.declarations.monomorphized_constants.insert(
                        id,
                        Declaration {
                            name: constant.decl.name,
                            span,
                            value: body,
                        },
                    );
                }
            }
        }

        // Finalize values

        let declarations = match (|| {
            Ok(Declarations {
                types: typechecker.declarations.types.clone(),
                type_parameters: typechecker.declarations.type_parameters.clone(),
                traits: typechecker.declarations.traits.clone(),
                generic_constants: typechecker
                    .declarations
                    .generic_constants
                    .clone()
                    .into_iter()
                    .map(|decl| {
                        Ok(Declaration {
                            name: decl.name,
                            span: decl.span,
                            value: typechecker
                                .finalize(decl.value, true)
                                .map_err(|error| (error, decl.span))?,
                        })
                    })
                    .collect::<Result<_, _>>()?,
                monomorphized_constants: typechecker
                    .declarations
                    .monomorphized_constants
                    .clone()
                    .into_iter()
                    .map(|(id, decl)| {
                        Ok((
                            id,
                            Declaration {
                                name: decl.name,
                                span: decl.span,
                                value: typechecker
                                    .finalize(decl.value, false)
                                    .map_err(|error| (error, decl.span))?,
                            },
                        ))
                    })
                    .collect::<Result<_, _>>()?,
                variables: typechecker
                    .declarations
                    .variables
                    .clone()
                    .into_iter()
                    .map(|(id, decl)| {
                        Ok((
                            id,
                            Declaration {
                                name: decl.name,
                                span: decl.span,
                                value: decl
                                    .value
                                    .finalize(&typechecker.ctx, true)
                                    .ok_or((TypeError::UnresolvedType, decl.span))?,
                            },
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            })
        })() {
            Ok(declarations) => declarations,
            Err((error, span)) => {
                typechecker.report_type_error(error, span);
                Declarations::default()
            }
        };

        let body = match body
            .into_iter()
            .map(|expr| {
                let span = expr.span;

                typechecker
                    .finalize(expr, false)
                    .map_err(|error| (error, span))
            })
            .collect::<Result<_, _>>()
        {
            Ok(declarations) => declarations,
            Err((error, span)) => {
                typechecker.report_type_error(error, span);
                Vec::new()
            }
        };

        // Build the final program

        Some(Program {
            well_typed: typechecker.well_typed,
            body,
            declarations,
            top_level,
        })
    }
}

type Bound = (TraitId, UnresolvedType, Span);

#[derive(Debug, Clone)]
struct Constant<T, Ty> {
    file: Rc<RefCell<lower::File>>,
    decl: Declaration<T>,
    generic_ty: Ty,
    bounds: Vec<Bound>,
}

struct Typechecker<'a, L: Loader> {
    well_typed: bool,
    ctx: Context,
    variables: BTreeMap<VariableId, UnresolvedType>,
    traits: BTreeMap<TraitId, (UnresolvedType, Vec<TypeParameterId>)>,
    structures: BTreeMap<TypeId, HashMap<InternedString, (usize, UnresolvedType)>>,
    // TODO: enumerations
    generic_constants: BTreeMap<GenericConstantId, Constant<lower::Expression, UnresolvedType>>,
    monomorphized_constants:
        BTreeMap<MonomorphizedConstantId, (Span, Constant<UnresolvedExpression, ()>)>,
    declared_instances: BTreeMap<TraitId, Vec<GenericConstantId>>,
    bound_instances: BTreeMap<TraitId, Vec<MonomorphizedConstantId>>,
    function_inputs: Vec<TypeVariable>,
    declarations: Declarations<MonomorphizedExpression, UnresolvedType>,
    compiler: &'a mut Compiler<L>,
}

impl<'a, L: Loader> Typechecker<'a, L> {
    fn typecheck_expr(
        &mut self,
        expr: &lower::Expression,
        file: &Rc<RefCell<lower::File>>,
        suppress_errors: bool,
    ) -> UnresolvedExpression {
        let error_expr = || UnresolvedExpression {
            span: expr.span,
            ty: UnresolvedType::Bottom(BottomTypeReason::Error),
            kind: UnresolvedExpressionKind::Error,
        };

        let (ty, kind) = match &expr.kind {
            lower::ExpressionKind::Error => {
                self.well_typed = false; // signal that the program contains errors
                return error_expr();
            }
            lower::ExpressionKind::Unit => (
                UnresolvedType::Builtin(BuiltinType::Unit),
                UnresolvedExpressionKind::Marker,
            ),
            lower::ExpressionKind::Marker(ty) => (
                self.convert_type_id(*ty, &file.borrow()),
                UnresolvedExpressionKind::Marker,
            ),
            lower::ExpressionKind::Constant(id) => (
                UnresolvedType::Variable(self.ctx.new_variable()),
                UnresolvedExpressionKind::Constant(*id),
            ),
            lower::ExpressionKind::Trait(id) => (
                UnresolvedType::Variable(self.ctx.new_variable()),
                UnresolvedExpressionKind::Trait(*id),
            ),
            lower::ExpressionKind::Variable(id) => {
                let ty = self
                    .variables
                    .get(id)
                    .expect("uninitialized variable")
                    .clone();

                (ty, UnresolvedExpressionKind::Variable(*id))
            }
            lower::ExpressionKind::Text(text) => (
                UnresolvedType::Builtin(BuiltinType::Text),
                UnresolvedExpressionKind::Text(*text),
            ),
            lower::ExpressionKind::Number(number) => (
                UnresolvedType::Builtin(BuiltinType::Number),
                UnresolvedExpressionKind::Number(*number),
            ),
            lower::ExpressionKind::Block(statements, declarations) => {
                let (ty, statements) = self.typecheck_block(statements, file, suppress_errors);

                (
                    ty,
                    UnresolvedExpressionKind::Block(statements, declarations.clone()),
                )
            }
            lower::ExpressionKind::Call(lhs, input) => {
                let function = self.typecheck_expr(lhs, file, suppress_errors);
                let input = self.typecheck_expr(input, file, suppress_errors);

                let output_ty = UnresolvedType::Variable(self.ctx.new_variable());

                match self.ctx.unify(
                    function.ty.clone(),
                    UnresolvedType::Function(
                        Box::new(input.ty.clone()),
                        Box::new(output_ty.clone()),
                    ),
                ) {
                    Ok(ty) => ty,
                    Err(error) => {
                        if !suppress_errors {
                            self.report_type_error(error, expr.span);
                        }

                        return error_expr();
                    }
                };

                (
                    output_ty,
                    UnresolvedExpressionKind::Call(Box::new(function), Box::new(input)),
                )
            }
            lower::ExpressionKind::Function(body, captures) => {
                let body = self.typecheck_expr(body, file, suppress_errors);

                let input_var = self
                    .function_inputs
                    .pop()
                    .expect("function body does not refer to function input");

                (
                    UnresolvedType::Function(
                        Box::new(UnresolvedType::Variable(input_var)),
                        Box::new(body.ty.clone()),
                    ),
                    UnresolvedExpressionKind::Function(
                        Box::new(body),
                        captures.iter().map(|(var, _)| *var).collect(),
                    ),
                )
            }
            lower::ExpressionKind::When(_, _) => todo!(),
            lower::ExpressionKind::External(namespace, identifier, inputs) => {
                let inputs = inputs
                    .iter()
                    .map(|expr| self.typecheck_expr(expr, file, suppress_errors))
                    .collect();

                (
                    UnresolvedType::Variable(self.ctx.new_variable()),
                    UnresolvedExpressionKind::External(*namespace, *identifier, inputs),
                )
            }
            lower::ExpressionKind::Annotate(expr, ty) => {
                let ty = self.convert_type_annotation(ty);
                let expr = self.typecheck_expr(expr, file, suppress_errors);

                match self.ctx.unify(expr.ty, ty.clone()) {
                    Ok(ty) => ty,
                    Err(error) => {
                        if !suppress_errors {
                            self.report_type_error(error, expr.span);
                        }

                        return error_expr();
                    }
                };

                (ty, expr.kind)
            }
            lower::ExpressionKind::Initialize(id, value) => {
                let value = self.typecheck_expr(value, file, suppress_errors);
                self.variables.insert(*id, value.ty.clone());

                (
                    UnresolvedType::Builtin(BuiltinType::Unit),
                    UnresolvedExpressionKind::Initialize(*id, Box::new(value)),
                )
            }
            lower::ExpressionKind::FunctionInput => {
                let var = self.ctx.new_variable();
                self.function_inputs.push(var);

                (
                    UnresolvedType::Variable(var),
                    UnresolvedExpressionKind::FunctionInput,
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

                        return error_expr();
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

                    let value = self.typecheck_expr(expr, file, suppress_errors);

                    match self.ctx.unify(value.ty.clone(), ty) {
                        Ok(ty) => ty,
                        Err(errors) => {
                            if !suppress_errors {
                                self.report_type_error(errors, expr.span);
                            }

                            return error_expr();
                        }
                    };

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

                    return error_expr();
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

                    return error_expr();
                }

                (
                    UnresolvedType::Named(*ty), // TODO: parameters
                    UnresolvedExpressionKind::Structure(fields),
                )
            }
            lower::ExpressionKind::Member(expr, name) => {
                let expr = self.typecheck_expr(expr, file, suppress_errors);

                (
                    UnresolvedType::Variable(self.ctx.new_variable()),
                    UnresolvedExpressionKind::Member(Box::new(expr), *name),
                )
            }
        };

        UnresolvedExpression {
            span: expr.span,
            ty,
            kind,
        }
    }

    fn typecheck_block(
        &mut self,
        statements: &[lower::Expression],
        file: &Rc<RefCell<lower::File>>,
        suppress_errors: bool,
    ) -> (UnresolvedType, Vec<UnresolvedExpression>) {
        let statements = statements
            .iter()
            .map(|statement| self.typecheck_expr(statement, file, suppress_errors))
            .collect::<Vec<_>>();

        let ty = statements
            .last()
            .map(|statement| statement.ty.clone())
            .unwrap_or(UnresolvedType::Builtin(BuiltinType::Unit));

        (ty, statements)
    }

    fn convert_type_id(&mut self, id: TypeId, file: &lower::File) -> UnresolvedType {
        let decl = file.declarations.types.get(&id).unwrap().clone();

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

    fn monomorphize_constant(
        &mut self,
        id: GenericConstantId,
        span: Span,
    ) -> (MonomorphizedConstantId, UnresolvedType) {
        let mut constant = self.generic_constants.get(&id).unwrap().clone();

        let body = constant.decl.value;
        let mut body = self.typecheck_expr(&body, &constant.file, true);

        if self
            .ctx
            .unify(body.ty.clone(), constant.generic_ty)
            .is_err()
        {
            body = UnresolvedExpression {
                span: body.span,
                ty: UnresolvedType::Bottom(BottomTypeReason::Error),
                kind: UnresolvedExpressionKind::Error,
            }
        }

        let mut substitutions = BTreeMap::new();
        let mut add_substitutions = |ty: &mut UnresolvedType| {
            ty.apply(&self.ctx);

            for param in ty.params() {
                substitutions
                    .entry(param)
                    .or_insert_with(|| UnresolvedType::Variable(self.ctx.new_variable()));
            }

            ty.instantiate_with(&substitutions);
        };

        body.traverse_mut(|expr| add_substitutions(&mut expr.ty));

        for (_, ty, _) in &mut constant.bounds {
            add_substitutions(ty);
        }

        let ty = body.ty.clone();

        let monomorphized_id = self.compiler.new_monomorphized_constant_id();

        self.monomorphized_constants.insert(
            monomorphized_id,
            (
                span,
                Constant {
                    file: constant.file,
                    decl: Declaration {
                        name: constant.decl.name,
                        span: constant.decl.span,
                        value: body,
                    },
                    generic_ty: (),
                    bounds: constant.bounds,
                },
            ),
        );

        (monomorphized_id, ty)
    }

    fn monomorphize(
        &mut self,
        expr: UnresolvedExpression,
    ) -> Result<MonomorphizedExpression, TypeError> {
        Ok(MonomorphizedExpression {
            span: expr.span,
            ty: expr.ty.clone(),
            kind: (|| {
                Ok(match expr.kind {
                    UnresolvedExpressionKind::Error => MonomorphizedExpressionKind::Error,
                    UnresolvedExpressionKind::Marker => MonomorphizedExpressionKind::Marker,
                    UnresolvedExpressionKind::Constant(id) => {
                        let (monomorphized_id, ty) = self.monomorphize_constant(id, expr.span);

                        if let Err(error) = self.ctx.unify(ty, expr.ty.clone()) {
                            self.report_type_error(error, expr.span);
                        }

                        MonomorphizedExpressionKind::Constant(monomorphized_id)
                    }
                    UnresolvedExpressionKind::Variable(var) => {
                        MonomorphizedExpressionKind::Variable(var)
                    }
                    UnresolvedExpressionKind::Text(text) => MonomorphizedExpressionKind::Text(text),
                    UnresolvedExpressionKind::Number(number) => {
                        MonomorphizedExpressionKind::Number(number)
                    }
                    UnresolvedExpressionKind::Block(statements, scope) => {
                        MonomorphizedExpressionKind::Block(
                            statements
                                .into_iter()
                                .map(|expr| self.monomorphize(expr))
                                .collect::<Result<_, _>>()?,
                            scope,
                        )
                    }
                    UnresolvedExpressionKind::Call(func, input) => {
                        MonomorphizedExpressionKind::Call(
                            Box::new(self.monomorphize(*func)?),
                            Box::new(self.monomorphize(*input)?),
                        )
                    }
                    UnresolvedExpressionKind::Member(value, name) => {
                        let mut ty = value.ty.clone();
                        ty.apply(&self.ctx);

                        let id = match ty {
                            UnresolvedType::Named(ty) => ty,
                            _ => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("cannot access member '{}' of this value", name),
                                    vec![Note::primary(expr.span, "value is not a data structure")],
                                ));

                                return Ok(MonomorphizedExpressionKind::Error);
                            }
                        };

                        let structure = match self.structures.get(&id) {
                            Some(ty) => ty,
                            None => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("cannot access member '{}' of this value", name),
                                    vec![Note::primary(expr.span, "value is not a data structure")],
                                ));

                                return Ok(MonomorphizedExpressionKind::Error);
                            }
                        };

                        let (&index, member_ty) = match structure.get(&name) {
                            Some((index, ty)) => (index, ty),
                            None => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("value has no member named '{}'", name),
                                    vec![Note::primary(expr.span, "no such member")],
                                ));

                                return Ok(MonomorphizedExpressionKind::Error);
                            }
                        };

                        if let Err(error) = self.ctx.unify(member_ty.clone(), expr.ty.clone()) {
                            self.report_type_error(error, expr.span);
                        }

                        MonomorphizedExpressionKind::Member(
                            Box::new(self.monomorphize(*value)?),
                            index,
                        )
                    }
                    UnresolvedExpressionKind::Function(body, captures) => {
                        MonomorphizedExpressionKind::Function(
                            Box::new(self.monomorphize(*body)?),
                            captures,
                        )
                    }
                    UnresolvedExpressionKind::When(_, _) => todo!(),
                    UnresolvedExpressionKind::External(namespace, identifier, inputs) => {
                        MonomorphizedExpressionKind::External(
                            namespace,
                            identifier,
                            inputs
                                .into_iter()
                                .map(|expr| self.monomorphize(expr))
                                .collect::<Result<_, _>>()?,
                        )
                    }
                    UnresolvedExpressionKind::Initialize(variable, value) => {
                        MonomorphizedExpressionKind::Initialize(
                            variable,
                            Box::new(self.monomorphize(*value)?),
                        )
                    }
                    UnresolvedExpressionKind::Structure(fields) => {
                        MonomorphizedExpressionKind::Structure(
                            fields
                                .into_iter()
                                .map(|expr| self.monomorphize(expr))
                                .collect::<Result<_, _>>()?,
                        )
                    }
                    UnresolvedExpressionKind::FunctionInput => {
                        MonomorphizedExpressionKind::FunctionInput
                    }
                    UnresolvedExpressionKind::Trait(tr) => MonomorphizedExpressionKind::Constant(
                        self.instance_for(tr, expr.ty.clone(), expr.span)?,
                    ),
                })
            })()?,
        })
    }

    fn finalize(
        &mut self,
        expr: MonomorphizedExpression,
        generic: bool,
    ) -> Result<Expression, TypeError> {
        Ok(Expression {
            span: expr.span,
            kind: match expr.kind {
                MonomorphizedExpressionKind::Error => return Err(TypeError::ErrorExpression),
                MonomorphizedExpressionKind::Marker => ExpressionKind::Marker,
                MonomorphizedExpressionKind::Constant(id) => ExpressionKind::Constant(id),
                MonomorphizedExpressionKind::Variable(var) => ExpressionKind::Variable(var),
                MonomorphizedExpressionKind::Text(text) => ExpressionKind::Text(text),
                MonomorphizedExpressionKind::Number(number) => ExpressionKind::Number(number),
                MonomorphizedExpressionKind::Block(statements, scope) => ExpressionKind::Block(
                    statements
                        .into_iter()
                        .map(|expr| self.finalize(expr, generic))
                        .collect::<Result<_, _>>()?,
                    scope,
                ),
                MonomorphizedExpressionKind::Call(func, input) => ExpressionKind::Call(
                    Box::new(self.finalize(*func, generic)?),
                    Box::new(self.finalize(*input, generic)?),
                ),
                MonomorphizedExpressionKind::Member(expr, index) => {
                    ExpressionKind::Member(Box::new(self.finalize(*expr, generic)?), index)
                }
                MonomorphizedExpressionKind::Function(body, captures) => {
                    ExpressionKind::Function(Box::new(self.finalize(*body, generic)?), captures)
                }
                MonomorphizedExpressionKind::When(_, _) => todo!(),
                MonomorphizedExpressionKind::External(namespace, identifier, inputs) => {
                    ExpressionKind::External(
                        namespace,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|expr| self.finalize(expr, generic))
                            .collect::<Result<_, _>>()?,
                    )
                }
                MonomorphizedExpressionKind::Initialize(variable, value) => {
                    ExpressionKind::Initialize(variable, Box::new(self.finalize(*value, generic)?))
                }
                MonomorphizedExpressionKind::Structure(fields) => ExpressionKind::Structure(
                    fields
                        .into_iter()
                        .map(|expr| self.finalize(expr, generic))
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
            },
            // The type must be resolved after the kind because the kind may be
            // a trait, whose type is unified with the original type variable
            ty: expr
                .ty
                .finalize(&self.ctx, generic)
                .ok_or(TypeError::UnresolvedType)?,
        })
    }

    fn instance_for(
        &mut self,
        tr: TraitId,
        mut ty: UnresolvedType,
        span: Span,
    ) -> Result<MonomorphizedConstantId, TypeError> {
        ty.apply(&self.ctx);

        let bound_instances = self.bound_instances.get(&tr).cloned().unwrap_or_default();

        for instance in bound_instances.into_iter().rev() {
            let (_, constant) = self.monomorphized_constants.get(&instance).unwrap();

            let mut ctx = self.ctx.clone();

            if ctx
                .unify_generic(ty.clone(), constant.decl.value.ty.clone())
                .is_ok()
            {
                self.ctx = ctx;
                return Ok(instance);
            }
        }

        let declared_instances = self
            .declared_instances
            .get(&tr)
            .cloned()
            .unwrap_or_default();

        let mut candidates = Vec::new();

        for instance in declared_instances {
            let constant = self.generic_constants.get(&instance).unwrap().clone();

            let mut ctx = self.ctx.clone();

            if ctx
                .unify_generic(ty.clone(), constant.generic_ty.clone())
                .is_ok()
            {
                candidates.push((ctx, instance));
            }
        }

        match candidates.len() {
            0 => Err(TypeError::MissingInstance(tr, ty)),
            1 => {
                let (ctx, instance) = candidates.pop().unwrap();
                self.ctx = ctx;

                let (monomorphized_id, _) = self.monomorphize_constant(instance, span);
                Ok(monomorphized_id)
            }
            _ => Err(TypeError::AmbiguousTrait(
                tr,
                candidates
                    .into_iter()
                    .map(|(_, instance)| instance)
                    .collect(),
            )),
        }
    }

    fn convert_type_annotation(&mut self, annotation: &lower::TypeAnnotation) -> UnresolvedType {
        match &annotation.kind {
            lower::TypeAnnotationKind::Error => UnresolvedType::Bottom(BottomTypeReason::Error),
            lower::TypeAnnotationKind::Placeholder => {
                UnresolvedType::Variable(self.ctx.new_variable())
            }
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

    fn report_type_error(&mut self, error: TypeError, span: Span) {
        self.well_typed = false;

        if let TypeError::Mismatch(actual, expected) = &error {
            if actual.contains_error() || expected.contains_error() {
                return;
            }
        }

        macro_rules! getter {
            ($x:ident) => {
                |id| self.declarations.$x.get(&id).unwrap().name.to_string()
            };
        }

        let type_names = getter!(types);
        let param_names = getter!(type_parameters);
        let trait_names = getter!(traits);

        let diagnostic = match error {
            TypeError::ErrorExpression => return,
            TypeError::Recursive(_) => Diagnostic::error(
                "recursive type",
                vec![Note::primary(span, "the type of this references itself")],
            ),
            TypeError::Mismatch(actual, expected) => Diagnostic::error(
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
            TypeError::MissingInstance(tr, ty) => Diagnostic::error(
                "missing instance",
                vec![Note::primary(
                    span,
                    format!(
                        "could not find instance of `{}` for type `{}`",
                        trait_names(tr),
                        format_type(&ty, type_names, param_names)
                    ),
                )],
            ),
            TypeError::AmbiguousTrait(_, candidates) => Diagnostic::error(
                "could not determine the type of this expression",
                std::iter::once(Note::primary(span, "try annotating the type with `::`"))
                    .chain(candidates.into_iter().map(|instance| {
                        let instance = self.generic_constants.get(&instance).unwrap();
                        Note::secondary(instance.decl.span, "this instance could apply")
                    }))
                    .collect(),
            ),
            TypeError::UnresolvedType => Diagnostic::error(
                "could not determine the type of this expression",
                vec![Note::primary(span, "try annotating the type with `::`")],
            ),
        };

        self.compiler.diagnostics.add(diagnostic);
    }
}
