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
                Variant(usize, Vec<[<$prefix Expression>]>),
                FunctionInput,
                ListLiteral(Vec<[<$prefix Expression>]>),
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

impl UnresolvedExpression {
    fn error(span: Span) -> Self {
        UnresolvedExpression {
            span,
            ty: UnresolvedType::Bottom(BottomTypeReason::Error),
            kind: UnresolvedExpressionKind::Error,
        }
    }
}

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
            traits: Default::default(),
            types: Default::default(),
            generic_constants: Default::default(),
            monomorphized_constants: Default::default(),
            queued_type_bounds: Default::default(),
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
                typechecker
                    .declarations
                    .types
                    .entry(id)
                    .or_insert(Declaration {
                        name: decl.name,
                        span: decl.span,
                        value: (),
                    });
            }

            for (id, decl) in file.borrow().declarations.type_parameters.clone() {
                typechecker
                    .declarations
                    .type_parameters
                    .entry(id)
                    .or_insert(Declaration {
                        name: decl.name,
                        span: decl.span,
                        value: (),
                    });
            }

            for (id, decl) in file.borrow().declarations.traits.clone() {
                if typechecker.declarations.traits.contains_key(&id) {
                    continue;
                }

                let ty = typechecker.convert_type_annotation(&decl.value.ty);

                let parameters = decl.value.parameters;

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

        // Typecheck expressions in the program and collect constraints

        let mut body = Vec::new();
        for (index, file) in files.iter_mut().enumerate() {
            progress(Progress::Typechecking {
                path: file.borrow().span.path,
                current: index + 1,
                total: total_files,
            });

            for (&id, decl) in &file.borrow().declarations.types {
                if typechecker.types.contains_key(&id) {
                    continue;
                }

                let params = decl.value.params.clone();

                let bounds = decl
                    .value
                    .bounds
                    .iter()
                    .map(|bound| {
                        let mut tr = typechecker.traits.get(&bound.tr).unwrap().clone();

                        let substitutions = tr
                            .params
                            .into_iter()
                            .zip(&bound.parameters)
                            .map(|(param, ty)| (param, typechecker.convert_type_annotation(ty)))
                            .collect();

                        tr.ty.instantiate_with(&substitutions);

                        (bound.tr, tr.ty, bound.span)
                    })
                    .collect();

                #[allow(clippy::map_entry)] // `typechecker` is borrowed twice otherwise
                if !typechecker.types.contains_key(&id) {
                    match &decl.value.kind {
                        lower::TypeKind::Structure(fields, field_names) => {
                            let fields = field_names
                                .iter()
                                .map(|(name, index)| {
                                    (
                                        *name,
                                        (
                                            *index,
                                            typechecker.convert_type_annotation(&fields[*index].ty),
                                        ),
                                    )
                                })
                                .collect();

                            typechecker.types.insert(
                                id,
                                TypeDefinition {
                                    kind: TypeDefinitionKind::Structure(fields),
                                    params,
                                    bounds,
                                },
                            );
                        }
                        lower::TypeKind::Enumeration(variants, _) => {
                            let variant_tys = variants
                                .iter()
                                .map(|(id, tys)| {
                                    (
                                        *id,
                                        tys.iter()
                                            .map(|ty| typechecker.convert_type_annotation(ty))
                                            .collect(),
                                    )
                                })
                                .collect();

                            typechecker.types.insert(
                                id,
                                TypeDefinition {
                                    kind: TypeDefinitionKind::Enumeration(variant_tys),
                                    params,
                                    bounds,
                                },
                            );
                        }
                        lower::TypeKind::Marker => {
                            typechecker.types.insert(
                                id,
                                TypeDefinition {
                                    kind: TypeDefinitionKind::Marker,
                                    params,
                                    bounds,
                                },
                            );
                        }
                    }
                }
            }

            for (&id, decl) in &file.borrow().declarations.traits {
                if typechecker.traits.contains_key(&id) {
                    continue;
                }

                let ty = typechecker.convert_type_annotation(&decl.value.ty);
                let params = decl.value.parameters.clone();

                typechecker
                    .traits
                    .insert(id, TraitDefinition { ty, params });
            }

            for (&id, decl) in &file.borrow().declarations.constants {
                if typechecker.generic_constants.contains_key(&id) {
                    continue;
                }

                let value = decl.value.value.borrow().as_ref().unwrap().clone();
                let generic_ty = typechecker.convert_type_annotation(&decl.value.ty);

                let bounds = decl
                    .value
                    .bounds
                    .iter()
                    .map(|bound| {
                        let mut tr = typechecker.traits.get(&bound.tr).unwrap().clone();

                        let substitutions = tr
                            .params
                            .into_iter()
                            .zip(&bound.parameters)
                            .map(|(param, ty)| (param, typechecker.convert_type_annotation(ty)))
                            .collect();

                        tr.ty.instantiate_with(&substitutions);

                        (bound.tr, tr.ty, bound.span)
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

            for (id, decl) in file.borrow().declarations.instances.clone() {
                if typechecker.generic_constants.contains_key(&id) {
                    continue;
                }

                let value = decl.value.value.clone();

                let mut tr = typechecker.traits.get(&decl.value.tr).unwrap().clone();

                let substitutions = tr
                    .params
                    .into_iter()
                    .zip(&decl.value.parameters)
                    .map(|(param, ty)| (param, typechecker.convert_type_annotation(ty)))
                    .collect();

                tr.ty.instantiate_with(&substitutions);

                typechecker
                    .declared_instances
                    .entry(decl.value.tr)
                    .or_default()
                    .push(id);

                typechecker.generic_constants.insert(
                    id,
                    Constant {
                        file: file.clone(),
                        decl: Declaration {
                            name: decl.name,
                            span: decl.span,
                            value,
                        },
                        generic_ty: tr.ty,
                        bounds: Vec::new(), // TODO: Generic instances
                    },
                );
            }

            let block = mem::take(&mut file.borrow_mut().block);
            let (_, block) = typechecker.typecheck_block(&block, file, false);
            body.push((file.clone(), block));
        }

        for (tr, mut ty, span) in typechecker.queued_type_bounds.clone() {
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

        let prev_bound_instances = typechecker.bound_instances.clone();

        for constant in typechecker.generic_constants.clone().into_values() {
            let body = typechecker.typecheck_expr(&constant.decl.value, &constant.file, false);

            if let Err(error) = typechecker.ctx.unify(body.ty.clone(), constant.generic_ty) {
                typechecker.report_type_error(error, body.span);
            }

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
            top_level.extend(file.borrow().exported.clone());
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

                        typechecker.process_variable_initializations(&expr, &file.borrow());

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

                    typechecker.process_variable_initializations(&body, &constant.file.borrow());

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
                                .finalize_generic(decl.value.clone())
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
                                    .finalize(decl.value)
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
                typechecker.finalize(expr).map_err(|error| (error, span))
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

#[derive(Debug, Clone)]
struct TraitDefinition {
    ty: UnresolvedType,
    params: Vec<TypeParameterId>,
    // bounds: Vec<Bound>, // TODO
}

#[derive(Debug, Clone)]
struct TypeDefinition {
    kind: TypeDefinitionKind,
    params: Vec<TypeParameterId>,
    bounds: Vec<Bound>,
}

#[derive(Debug, Clone)]
enum TypeDefinitionKind {
    Marker,
    Structure(HashMap<InternedString, (usize, UnresolvedType)>),
    Enumeration(Vec<(GenericConstantId, Vec<UnresolvedType>)>),
}

struct Typechecker<'a, L: Loader> {
    well_typed: bool,
    ctx: Context,
    variables: BTreeMap<VariableId, UnresolvedType>,
    traits: BTreeMap<TraitId, TraitDefinition>,
    types: BTreeMap<TypeId, TypeDefinition>,
    generic_constants: BTreeMap<GenericConstantId, Constant<lower::Expression, UnresolvedType>>,
    monomorphized_constants:
        BTreeMap<MonomorphizedConstantId, (Span, Constant<UnresolvedExpression, ()>)>,
    queued_type_bounds: Vec<Bound>,
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
        match &expr.kind {
            lower::ExpressionKind::Error => {
                self.well_typed = false; // signal that the program contains errors
                UnresolvedExpression::error(expr.span)
            }
            lower::ExpressionKind::Unit => UnresolvedExpression {
                span: expr.span,
                ty: UnresolvedType::Builtin(BuiltinType::Unit),
                kind: UnresolvedExpressionKind::Marker,
            },
            lower::ExpressionKind::Marker(id) => {
                let mut marker = self.types.get(id).unwrap().clone();

                let mut ty = UnresolvedType::Named(
                    *id,
                    marker
                        .params
                        .into_iter()
                        .map(UnresolvedType::Parameter)
                        .collect(),
                );

                let mut substitutions = BTreeMap::new();
                self.add_substitutions(&mut ty, &mut substitutions);

                for (_, ty, _) in &mut marker.bounds {
                    self.add_substitutions(ty, &mut substitutions);
                }

                self.queued_type_bounds.append(&mut marker.bounds);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Marker,
                }
            }
            lower::ExpressionKind::Constant(id) => UnresolvedExpression {
                span: expr.span,
                ty: UnresolvedType::Variable(self.ctx.new_variable()),
                kind: UnresolvedExpressionKind::Constant(*id),
            },
            lower::ExpressionKind::Trait(id) => UnresolvedExpression {
                span: expr.span,
                ty: UnresolvedType::Variable(self.ctx.new_variable()),
                kind: UnresolvedExpressionKind::Trait(*id),
            },
            lower::ExpressionKind::Variable(id) => {
                let ty = self
                    .variables
                    .get(id)
                    .expect("uninitialized variable")
                    .clone();

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Variable(*id),
                }
            }
            lower::ExpressionKind::Text(text) => UnresolvedExpression {
                span: expr.span,
                ty: UnresolvedType::Builtin(BuiltinType::Text),
                kind: UnresolvedExpressionKind::Text(*text),
            },
            lower::ExpressionKind::Number(number) => UnresolvedExpression {
                span: expr.span,
                ty: UnresolvedType::Builtin(BuiltinType::Number),
                kind: UnresolvedExpressionKind::Number(*number),
            },
            lower::ExpressionKind::Block(statements, declarations) => {
                let (ty, statements) = self.typecheck_block(statements, file, suppress_errors);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Block(statements, declarations.clone()),
                }
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

                        return UnresolvedExpression::error(expr.span);
                    }
                };

                UnresolvedExpression {
                    span: expr.span,
                    ty: output_ty,
                    kind: UnresolvedExpressionKind::Call(Box::new(function), Box::new(input)),
                }
            }
            lower::ExpressionKind::Function(body, captures) => {
                let body = self.typecheck_expr(body, file, suppress_errors);

                let input_var = self
                    .function_inputs
                    .pop()
                    .expect("function body does not refer to function input");

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Function(
                        Box::new(UnresolvedType::Variable(input_var)),
                        Box::new(body.ty.clone()),
                    ),
                    kind: UnresolvedExpressionKind::Function(
                        Box::new(body),
                        captures.iter().map(|(var, _)| *var).collect(),
                    ),
                }
            }
            lower::ExpressionKind::When(_, _) => todo!(),
            lower::ExpressionKind::External(namespace, identifier, inputs) => {
                let inputs = inputs
                    .iter()
                    .map(|expr| self.typecheck_expr(expr, file, suppress_errors))
                    .collect();

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Variable(self.ctx.new_variable()),
                    kind: UnresolvedExpressionKind::External(*namespace, *identifier, inputs),
                }
            }
            lower::ExpressionKind::Annotate(expr, ty) => {
                let ty = self.convert_type_annotation(ty);
                let value = self.typecheck_expr(expr, file, suppress_errors);

                if let Err(error) = self.ctx.unify(value.ty, ty.clone()) {
                    if !suppress_errors {
                        self.report_type_error(error, value.span);
                    }

                    return UnresolvedExpression::error(expr.span);
                };

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: value.kind,
                }
            }
            lower::ExpressionKind::Initialize(id, value) => {
                let value = self.typecheck_expr(value, file, suppress_errors);
                self.variables.insert(*id, value.ty.clone());

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Builtin(BuiltinType::Unit),
                    kind: UnresolvedExpressionKind::Initialize(*id, Box::new(value)),
                }
            }
            lower::ExpressionKind::FunctionInput => {
                let var = self.ctx.new_variable();
                self.function_inputs.push(var);

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Variable(var),
                    kind: UnresolvedExpressionKind::FunctionInput,
                }
            }
            lower::ExpressionKind::Instantiate(id, fields) => {
                let mut structure = self.types.get(id).unwrap().clone();

                let mut structure_fields = match &structure.kind {
                    TypeDefinitionKind::Structure(fields) => fields.clone(),
                    _ => unreachable!(), // or do we need to display an error like above?
                };

                let mut ty = UnresolvedType::Named(
                    *id,
                    structure
                        .params
                        .into_iter()
                        .map(UnresolvedType::Parameter)
                        .collect(),
                );

                let mut substitutions = BTreeMap::new();
                self.add_substitutions(&mut ty, &mut substitutions);

                for (_, ty, _) in &mut structure.bounds {
                    self.add_substitutions(ty, &mut substitutions);
                }

                self.queued_type_bounds.append(&mut structure.bounds);

                for (_, ty) in structure_fields.values_mut() {
                    self.add_substitutions(ty, &mut substitutions);
                }

                let mut fields_by_index = structure_fields.iter().collect::<Vec<_>>();
                fields_by_index.sort_by_key(|(_, (index, _))| *index);

                let mut unpopulated_fields = vec![None; fields_by_index.len()];
                let mut extra_fields = Vec::new();

                for (name, expr) in fields {
                    let (index, ty) = match structure_fields.get(name) {
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

                            return UnresolvedExpression::error(expr.span);
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

                    return UnresolvedExpression::error(expr.span);
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

                    return UnresolvedExpression::error(expr.span);
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Structure(fields),
                }
            }
            lower::ExpressionKind::Member(expr, name) => {
                let value = self.typecheck_expr(expr, file, suppress_errors);

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Variable(self.ctx.new_variable()),
                    kind: UnresolvedExpressionKind::Member(Box::new(value), *name),
                }
            }
            lower::ExpressionKind::ListLiteral(items) => {
                let items = items
                    .iter()
                    .map(|expr| self.typecheck_expr(expr, file, suppress_errors))
                    .collect::<Vec<_>>();

                let ty = if let Some(item) = items.first() {
                    let ty = item.ty.clone();
                    for item in items.iter().skip(1) {
                        if let Err(error) = self.ctx.unify(ty.clone(), item.ty.clone()) {
                            if !suppress_errors {
                                self.report_type_error(error, item.span);
                            }
                        };
                    }

                    ty
                } else {
                    UnresolvedType::Bottom(BottomTypeReason::Annotated)
                };

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Builtin(BuiltinType::List(Box::new(ty))),
                    kind: UnresolvedExpressionKind::ListLiteral(items),
                }
            }
            lower::ExpressionKind::Variant(id, index, values) => {
                let mut enumeration = self.types.get(id).unwrap().clone();

                let (mut variant_constructor, variant_tys) = match &enumeration.kind {
                    TypeDefinitionKind::Enumeration(variants) => {
                        let (id, tys) = &variants[*index];
                        (self.generic_constants.get(id).unwrap().clone(), tys.clone())
                    }
                    _ => unreachable!(), // or do we need to display an error like above?
                };

                let mut ty = UnresolvedType::Named(
                    *id,
                    enumeration
                        .params
                        .into_iter()
                        .map(UnresolvedType::Parameter)
                        .collect(),
                );

                let mut substitutions = BTreeMap::new();
                self.add_substitutions(&mut ty, &mut substitutions);

                for (_, ty, _) in &mut variant_constructor.bounds {
                    self.add_substitutions(ty, &mut substitutions);
                }

                self.queued_type_bounds.append(&mut enumeration.bounds);

                let values = values
                    .iter()
                    .zip(variant_tys)
                    .map(|(expr, ty)| {
                        let value = self.typecheck_expr(expr, file, suppress_errors);

                        if let Err(errors) = self.ctx.unify(value.ty.clone(), ty) {
                            if !suppress_errors {
                                self.report_type_error(errors, expr.span);
                            }

                            return UnresolvedExpression::error(expr.span);
                        };

                        value
                    })
                    .collect();

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Variant(*index, values),
                }
            }
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

        body.traverse_mut(|expr| self.add_substitutions(&mut expr.ty, &mut substitutions));

        for (_, ty, _) in &mut constant.bounds {
            self.add_substitutions(ty, &mut substitutions);
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

                        if let Err(error) = self.ctx.unify(ty, expr.ty) {
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
                        // Monomorphize in reverse order so that inner traits
                        // are resolved first, eg. in the case of `T2 (T1 x)`
                        let input = self.monomorphize(*input)?;
                        let func = self.monomorphize(*func)?;

                        MonomorphizedExpressionKind::Call(Box::new(func), Box::new(input))
                    }
                    UnresolvedExpressionKind::Member(value, name) => {
                        let mut ty = value.ty.clone();
                        ty.apply(&self.ctx);

                        let (id, params) = match ty {
                            UnresolvedType::Named(id, params) => (id, params),
                            _ => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("cannot access member '{}' of this value", name),
                                    vec![Note::primary(expr.span, "value is not a data structure")],
                                ));

                                return Ok(MonomorphizedExpressionKind::Error);
                            }
                        };

                        let structure = match self.types.get(&id) {
                            Some(ty) => ty,
                            None => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("cannot access member '{}' of this value", name),
                                    vec![Note::primary(expr.span, "value is not a data structure")],
                                ));

                                return Ok(MonomorphizedExpressionKind::Error);
                            }
                        };

                        let structure_fields = match &structure.kind {
                            TypeDefinitionKind::Structure(fields) => fields.clone(),
                            _ => unreachable!(), // or do we need to display an error like above?
                        };

                        let (index, mut member_ty) = match structure_fields.get(&name) {
                            Some((index, ty)) => (*index, ty.clone()),
                            None => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("value has no member named '{}'", name),
                                    vec![Note::primary(expr.span, "no such member")],
                                ));

                                return Ok(MonomorphizedExpressionKind::Error);
                            }
                        };

                        let substitutions = structure
                            .params
                            .iter()
                            .copied()
                            .zip(params)
                            .collect::<BTreeMap<_, _>>();

                        member_ty.instantiate_with(&substitutions);

                        if let Err(error) = self.ctx.unify(member_ty, expr.ty) {
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
                    UnresolvedExpressionKind::Variant(index, values) => {
                        MonomorphizedExpressionKind::Variant(
                            index,
                            values
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
                    UnresolvedExpressionKind::ListLiteral(items) => {
                        MonomorphizedExpressionKind::ListLiteral(
                            items
                                .into_iter()
                                .map(|expr| self.monomorphize(expr))
                                .collect::<Result<_, _>>()?,
                        )
                    }
                })
            })()?,
        })
    }

    fn add_substitutions(
        &mut self,
        ty: &mut UnresolvedType,
        substitutions: &mut BTreeMap<TypeParameterId, UnresolvedType>,
    ) {
        ty.apply(&self.ctx);

        for param in ty.params() {
            substitutions
                .entry(param)
                .or_insert_with(|| UnresolvedType::Variable(self.ctx.new_variable()));
        }

        ty.instantiate_with(substitutions);
    }

    fn finalize(&mut self, expr: MonomorphizedExpression) -> Result<Expression, TypeError> {
        self.finalize_internal(expr, false)
    }

    fn finalize_generic(&mut self, expr: MonomorphizedExpression) -> Result<Expression, TypeError> {
        self.finalize_internal(expr, true)
    }

    fn finalize_internal(
        &mut self,
        expr: MonomorphizedExpression,
        generic: bool,
    ) -> Result<Expression, TypeError> {
        Ok(Expression {
            span: expr.span,
            ty: expr
                .ty
                .finalize(&self.ctx, generic)
                .ok_or(TypeError::UnresolvedType)?,
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
                        .map(|expr| self.finalize_internal(expr, generic))
                        .collect::<Result<_, _>>()?,
                    scope,
                ),
                MonomorphizedExpressionKind::Call(func, input) => ExpressionKind::Call(
                    Box::new(self.finalize_internal(*func, generic)?),
                    Box::new(self.finalize_internal(*input, generic)?),
                ),
                MonomorphizedExpressionKind::Member(expr, index) => {
                    ExpressionKind::Member(Box::new(self.finalize_internal(*expr, generic)?), index)
                }
                MonomorphizedExpressionKind::Function(body, captures) => ExpressionKind::Function(
                    Box::new(self.finalize_internal(*body, generic)?),
                    captures,
                ),
                MonomorphizedExpressionKind::When(_, _) => todo!(),
                MonomorphizedExpressionKind::External(namespace, identifier, inputs) => {
                    ExpressionKind::External(
                        namespace,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|expr| self.finalize_internal(expr, generic))
                            .collect::<Result<_, _>>()?,
                    )
                }
                MonomorphizedExpressionKind::Initialize(variable, value) => {
                    ExpressionKind::Initialize(
                        variable,
                        Box::new(self.finalize_internal(*value, generic)?),
                    )
                }
                MonomorphizedExpressionKind::Structure(fields) => ExpressionKind::Structure(
                    fields
                        .into_iter()
                        .map(|expr| self.finalize_internal(expr, generic))
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedExpressionKind::Variant(index, values) => ExpressionKind::Variant(
                    index,
                    values
                        .into_iter()
                        .map(|expr| self.finalize_internal(expr, generic))
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
                MonomorphizedExpressionKind::ListLiteral(items) => ExpressionKind::ListLiteral(
                    items
                        .into_iter()
                        .map(|expr| self.finalize_internal(expr, generic))
                        .collect::<Result<_, _>>()?,
                ),
            },
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

                let (monomorphized_id, instance_ty) = self.monomorphize_constant(instance, span);

                self.ctx
                    .unify(ty, instance_ty)
                    .expect("type wasn't unified above");

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

    fn process_variable_initializations(
        &mut self,
        expr: &MonomorphizedExpression,
        file: &lower::File,
    ) {
        expr.traverse(|expr| {
            if let MonomorphizedExpressionKind::Initialize(id, value) = &expr.kind {
                let decl = match file.declarations.variables.get(id) {
                    Some(decl) => decl,
                    None => return,
                };

                self.declarations.variables.insert(
                    *id,
                    Declaration {
                        name: decl.name,
                        span: decl.span,
                        value: value.ty.clone(),
                    },
                );
            }
        });
    }

    fn convert_type_annotation(&mut self, annotation: &lower::TypeAnnotation) -> UnresolvedType {
        match &annotation.kind {
            lower::TypeAnnotationKind::Error => UnresolvedType::Bottom(BottomTypeReason::Error),
            lower::TypeAnnotationKind::Placeholder => {
                UnresolvedType::Variable(self.ctx.new_variable())
            }
            lower::TypeAnnotationKind::Named(id, params) => UnresolvedType::Named(
                *id,
                params
                    .iter()
                    .map(|param| self.convert_type_annotation(param))
                    .collect(),
            ),
            lower::TypeAnnotationKind::Parameter(id) => UnresolvedType::Parameter(*id),
            lower::TypeAnnotationKind::Builtin(builtin, parameters) => {
                UnresolvedType::Builtin(match builtin {
                    lower::BuiltinType::Unit => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`()` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        BuiltinType::Unit
                    }
                    lower::BuiltinType::Number => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Number` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        BuiltinType::Number
                    }
                    lower::BuiltinType::Text => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Text` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        BuiltinType::Text
                    }
                    lower::BuiltinType::List => {
                        if parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`List` accepts 1 parameter, but none were provided",
                                vec![Note::primary(
                                    annotation.span,
                                    "try adding `_` here to infer the type of `Element`",
                                )],
                            ));

                            BuiltinType::List(Box::new(UnresolvedType::Bottom(
                                BottomTypeReason::Error,
                            )))
                        } else {
                            if parameters.len() > 1 {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!(
                                        "`List` accepts 1 parameter, but {} were provided",
                                        parameters.len()
                                    ),
                                    vec![Note::primary(
                                        annotation.span,
                                        "try removing some of these",
                                    )],
                                ));
                            }

                            BuiltinType::List(Box::new(
                                self.convert_type_annotation(parameters.first().unwrap()),
                            ))
                        }
                    }
                })
            }
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
                        format_type(expected, type_names, param_names),
                        format_type(actual, type_names, param_names)
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
                        format_type(ty, type_names, param_names)
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
