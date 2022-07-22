#![allow(clippy::type_complexity)]

mod engine;
mod format;
mod traverse;

pub use engine::{BottomTypeReason, BuiltinType, Type, TypeStructure};
pub use format::{format_type, FormattableType};

use crate::{
    analysis::lower, diagnostics::*, helpers::InternedString, parse::Span, Compiler, FilePath,
    GenericConstantId, Loader, MonomorphizedConstantId, TraitId, TypeId, TypeParameterId,
    VariableId,
};
use engine::*;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap},
    mem,
    rc::Rc,
};

#[cfg(debug_assertions)]
use backtrace::Backtrace;

#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Expression>,
    pub declarations: Declarations<Expression, Type>,
    pub top_level: HashMap<InternedString, lower::ScopeValue>,
}

macro_rules! expr {
    ($vis:vis, $prefix:literal, $type:ident, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone)]
            $vis struct [<$prefix Expression>] {
                $vis span: Span,
                $vis ty: $type,
                $vis kind: [<$prefix ExpressionKind>],
            }

            #[derive(Debug, Clone)]
            $vis enum [<$prefix ExpressionKind>] {
                Marker,
                Variable(VariableId),
                Text(InternedString),
                Number(f64),
                Block(Vec<[<$prefix Expression>]>),
                Call(Box<[<$prefix Expression>]>, Box<[<$prefix Expression>]>),
                Function([<$prefix Pattern>], Box<[<$prefix Expression>]>),
                When(Box<[<$prefix Expression>]>, Vec<[<$prefix Arm>]>),
                External(InternedString, InternedString, Vec<[<$prefix Expression>]>),
                Initialize([<$prefix Pattern>], Box<[<$prefix Expression>]>),
                Structure(Vec<[<$prefix Expression>]>),
                Variant(usize, Vec<[<$prefix Expression>]>),
                Return(Box<[<$prefix Expression>]>),
                Loop(Box<[<$prefix Expression>]>),
                Break(Box<[<$prefix Expression>]>),
                Continue,
                Tuple(Vec<[<$prefix Expression>]>),
                $($kinds)*
            }

            #[derive(Debug, Clone)]
            $vis struct [<$prefix Arm>] {
                $vis span: Span,
                $vis pattern: [<$prefix Pattern>],
                $vis body: [<$prefix Expression>],
            }
        }
    };
}

macro_rules! pattern {
    ($vis:vis, $prefix:literal, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone)]
            $vis struct [<$prefix Pattern>] {
                $vis span: Span,
                $vis kind: [<$prefix PatternKind>],
            }

            #[derive(Debug, Clone)]
            $vis enum [<$prefix PatternKind>] {
                Wildcard,
                Number(f64),
                Text(InternedString),
                Variable(VariableId),
                Or(Box<[<$prefix Pattern>]>, Box<[<$prefix Pattern>]>),
                Where(Box<[<$prefix Pattern>]>, Box<[<$prefix Expression>]>),
                Tuple(Vec<[<$prefix Pattern>]>),
                $($kinds)*
            }
        }
    };
}

expr!(, "Unresolved", UnresolvedType, {
    Error,
    Trait(TraitId),
    Constant(GenericConstantId),
});

expr!(, "Monomorphized", UnresolvedType, {
    Error,
    Constant(MonomorphizedConstantId),
});

expr!(pub, "", Type, {
    Constant(MonomorphizedConstantId),
});

pattern!(, "Unresolved", {
    Error,
    Destructure(HashMap<InternedString, UnresolvedPattern>),
    Variant(TypeId, usize, Vec<UnresolvedPattern>),
    Annotate(Box<UnresolvedPattern>, UnresolvedType),
});

pattern!(, "Monomorphized", {
    Destructure(BTreeMap<usize, MonomorphizedPattern>),
    Variant(usize, Vec<MonomorphizedPattern>),
});

pattern!(pub, "", {
    Destructure(BTreeMap<usize, Pattern>),
    Variant(usize, Vec<Pattern>)
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

#[derive(Debug, Clone)]
pub struct Declarations<Expr, Ty, File = ()> {
    pub types: BTreeMap<TypeId, Declaration<TypeDeclaration>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<()>>,
    pub traits: BTreeMap<TraitId, Declaration<TraitDeclaration>>,
    pub generic_constants: BTreeMap<GenericConstantId, GenericConstantDeclaration<Expr, File>>,
    pub monomorphized_constants:
        BTreeMap<MonomorphizedConstantId, (File, GenericConstantId, Declaration<Expr>)>,
    pub variables: BTreeMap<VariableId, Declaration<Ty>>,
}

impl<Expr, Ty, File> Default for Declarations<Expr, Ty, File> {
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

#[derive(Debug, Clone)]
pub struct Declaration<T> {
    pub name: Option<InternedString>,
    pub span: Span,
    pub value: T,
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub attributes: lower::DeclarationAttributes,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub ty: UnresolvedType,
    pub params: Vec<TypeParameterId>,
    pub attributes: lower::TraitAttributes,
}

#[derive(Debug, Clone)]
pub struct GenericConstantDeclaration<Expr, File> {
    pub file: File,
    pub decl: Declaration<Expr>,
    pub attributes: Option<lower::DeclarationAttributes>,
}

#[derive(Debug)]
pub enum Progress {
    Typechecking {
        path: FilePath,
        current: usize,
        total: usize,
    },
    Finalizing,
}

impl<L: Loader> Compiler<L> {
    pub fn typecheck(&self, files: Vec<lower::File>) -> Option<Program> {
        self.typecheck_with_progress(files, |_| {})
    }

    pub fn typecheck_with_progress(
        &self,
        files: Vec<lower::File>,
        mut progress: impl FnMut(Progress),
    ) -> Option<Program> {
        let mut files = files
            .into_iter()
            .map(|file| Rc::new(RefCell::new(file)))
            .collect::<Vec<_>>();

        let mut typechecker = Typechecker {
            ctx: Default::default(),
            errors: Default::default(),
            variables: Default::default(),
            return_tys: Default::default(),
            loop_tys: Default::default(),
            traits: Default::default(),
            types: Default::default(),
            generic_constants: Default::default(),
            monomorphized_constants: Default::default(),
            declared_instances: Default::default(),
            bound_instances: Default::default(),
            declarations: Default::default(),
            compiler: self.clone(),
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
                        value: TypeDeclaration {
                            attributes: decl.value.attributes,
                        },
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

                let ty = typechecker.convert_type_annotation(&decl.value.ty, file);

                let params = decl.value.parameters;

                typechecker.declarations.traits.insert(
                    id,
                    Declaration {
                        name: decl.name,
                        span: decl.span,
                        value: TraitDeclaration {
                            ty,
                            params,
                            attributes: decl.value.attributes,
                        },
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

                #[allow(clippy::map_entry)] // `typechecker` is borrowed twice otherwise
                if !typechecker.types.contains_key(&id) {
                    match &decl.value.kind {
                        lower::TypeKind::Structure(fields, field_names) => {
                            let field_tys = fields
                                .iter()
                                .map(|field| typechecker.convert_type_annotation(&field.ty, file))
                                .collect();

                            typechecker.types.insert(
                                id,
                                TypeDefinition {
                                    kind: TypeDefinitionKind::Structure(
                                        field_tys,
                                        field_names.clone(),
                                    ),
                                    params,
                                },
                            );
                        }
                        lower::TypeKind::Enumeration(variants, _) => {
                            let variant_tys = variants
                                .iter()
                                .map(|variant| {
                                    variant
                                        .tys
                                        .iter()
                                        .map(|ty| typechecker.convert_type_annotation(ty, file))
                                        .collect()
                                })
                                .collect();

                            let variant_constructors =
                                variants.iter().map(|variant| variant.constructor).collect();

                            typechecker.types.insert(
                                id,
                                TypeDefinition {
                                    kind: TypeDefinitionKind::Enumeration(
                                        variant_tys,
                                        variant_constructors,
                                    ),
                                    params,
                                },
                            );
                        }
                        lower::TypeKind::Marker => {
                            typechecker.types.insert(
                                id,
                                TypeDefinition {
                                    kind: TypeDefinitionKind::Marker,
                                    params,
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

                let ty = typechecker.convert_type_annotation(&decl.value.ty, file);
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
                let generic_ty = typechecker.convert_type_annotation(&decl.value.ty, file);

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
                            .map(|(param, ty)| {
                                (param, typechecker.convert_type_annotation(ty, file))
                            })
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
                        attributes: Some(decl.value.attributes.clone()),
                    },
                );
            }

            for (id, decl) in file.borrow().declarations.instances.clone() {
                if typechecker.generic_constants.contains_key(&id) {
                    continue;
                }

                let value = decl.value.value.clone();

                let mut tr = typechecker.traits.get(&decl.value.tr).unwrap().clone();

                let mut substitutions = tr
                    .params
                    .into_iter()
                    .zip(&decl.value.trait_params)
                    .map(|(param, ty)| (param, typechecker.convert_type_annotation(ty, file)))
                    .collect();

                typechecker.add_substitutions(&mut tr.ty, &mut substitutions);

                // Check if the instance collides with any other instances --
                // there's no need to check the bounds because there's no way to
                // ensure a type doesn't satisfy the bounds specified in both
                // instances

                let colliding_instances = typechecker
                    .declared_instances
                    .get(&decl.value.tr)
                    .cloned()
                    .unwrap_or_default()
                    .into_iter()
                    .filter_map(|id| {
                        let other = typechecker.generic_constants.get(&id).unwrap();

                        let mut temp_ctx = typechecker.ctx.clone();
                        temp_ctx
                            .unify(tr.ty.clone(), other.generic_ty.clone())
                            .is_ok()
                            .then(|| other.decl.span)
                    })
                    .collect::<Vec<_>>();

                if !colliding_instances.is_empty() {
                    self.diagnostics.add(Diagnostic::error(
                        format!(
                            "this instance collides with {} other instances",
                            colliding_instances.len()
                        ),
                        std::iter::once(Note::primary(
                            decl.span,
                            if decl.value.bounds.is_empty() {
                                "try making this instance more specific"
                            } else {
                                "this instance may have different bounds than the others, but one type could satisfy the bounds on more than one of these instances simultaneously"
                            },
                        ))
                        .chain(colliding_instances.into_iter().map(|span| {
                            Note::primary(span, "this instance could apply to the same type(s)")
                        }))
                        .collect(),
                    ));
                }

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
                            .map(|(param, ty)| {
                                (param, typechecker.convert_type_annotation(ty, file))
                            })
                            .collect();

                        tr.ty.instantiate_with(&substitutions);

                        (bound.tr, tr.ty, bound.span)
                    })
                    .collect::<Vec<_>>();

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
                        bounds,
                        attributes: None,
                    },
                );
            }

            let block = mem::take(&mut file.borrow_mut().block);
            let (_, block) = typechecker.typecheck_block(&block, file, false);
            body.push((file.clone(), block));
        }

        for (id, constant) in typechecker.generic_constants.clone() {
            let prev_bound_instances = typechecker.bound_instances.clone();
            let prev_monomorphized_constants = typechecker.monomorphized_constants.clone();

            let body = typechecker.typecheck_expr(&constant.decl.value, &constant.file, false);

            if let Err(error) = typechecker.ctx.unify(body.ty.clone(), constant.generic_ty) {
                typechecker.errors.push(Error::new(error, body.span));
            }

            // Register dummy generic instances so finalization works -- these
            // constants will never actually be used
            for (tr, ty, span) in constant.bounds {
                let dummy_id = typechecker.compiler.new_generic_constant_id();

                typechecker.generic_constants.insert(
                    dummy_id,
                    Constant {
                        file: constant.file.clone(),
                        decl: Declaration {
                            name: None,
                            span,
                            value: lower::Expression {
                                span: constant.decl.value.span,
                                kind: lower::ExpressionKind::Error,
                            },
                        },
                        generic_ty: ty,
                        bounds: Vec::new(),
                        attributes: None,
                    },
                );

                typechecker
                    .bound_instances
                    .entry(tr)
                    .or_default()
                    .push(dummy_id);
            }

            let body_span = body.span;
            let body = match typechecker.monomorphize(body, &constant.file, None) {
                Ok(expr) => expr,
                Err(error) => {
                    typechecker.errors.push(error);

                    MonomorphizedExpression {
                        span: body_span,
                        ty: UnresolvedType::Bottom(BottomTypeReason::Error),
                        kind: MonomorphizedExpressionKind::Error,
                    }
                }
            };

            typechecker.declarations.generic_constants.insert(
                id,
                GenericConstantDeclaration {
                    file: constant.file,
                    decl: Declaration {
                        name: constant.decl.name,
                        span: constant.decl.span,
                        value: body,
                    },
                    attributes: constant.attributes,
                },
            );

            typechecker.bound_instances = prev_bound_instances;
            typechecker.monomorphized_constants = prev_monomorphized_constants;
        }

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
                        let expr = typechecker.monomorphize(expr, file, None)?;
                        Ok((file.clone(), expr))
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|body| body.into_iter().flatten().collect::<Vec<_>>())
        {
            Ok(body) => Some(body),
            Err(error) => {
                typechecker.errors.push(error);
                None
            }
        };

        loop {
            let mut new = false;
            for (id, (span, generic_id, constant)) in typechecker.monomorphized_constants.clone() {
                if typechecker
                    .declarations
                    .monomorphized_constants
                    .contains_key(&id)
                {
                    continue;
                }

                new = true;

                let mut ty = constant.decl.value.ty.clone();
                ty.apply(&typechecker.ctx);

                for (tr, mut ty, span) in constant.bounds.clone() {
                    ty.apply(&typechecker.ctx);

                    let instance_id = match typechecker.instance_for(tr, ty.clone()) {
                        Ok(id) => id,
                        Err(error) => {
                            typechecker.errors.push(Error::new(error, span));
                            continue;
                        }
                    };

                    typechecker
                        .bound_instances
                        .entry(tr)
                        .or_default()
                        .push(instance_id);
                }

                let body = match typechecker.monomorphize(
                    constant.decl.value.clone(),
                    &constant.file,
                    Some((generic_id, id)),
                ) {
                    Ok(value) => value,
                    Err(error) => {
                        typechecker.errors.push(error);

                        MonomorphizedExpression {
                            span,
                            ty,
                            kind: MonomorphizedExpressionKind::Error,
                        }
                    }
                };

                typechecker.declarations.monomorphized_constants.insert(
                    id,
                    (
                        constant.file,
                        generic_id,
                        Declaration {
                            name: constant.decl.name,
                            span,
                            value: body,
                        },
                    ),
                );
            }

            if !new {
                break;
            }
        }

        // Finalize values

        let mut declarations = match (|| {
            Ok(Declarations {
                types: typechecker.declarations.types.clone(),
                type_parameters: typechecker.declarations.type_parameters.clone(),
                traits: typechecker.declarations.traits.clone(),
                generic_constants: typechecker
                    .declarations
                    .generic_constants
                    .clone()
                    .into_iter()
                    .map(|(id, constant)| {
                        Ok((
                            id,
                            GenericConstantDeclaration {
                                file: (),
                                decl: Declaration {
                                    name: constant.decl.name,
                                    span: constant.decl.span,
                                    value: typechecker.finalize_generic(
                                        constant.decl.value.clone(),
                                        &constant.file,
                                    )?,
                                },
                                attributes: constant.attributes,
                            },
                        ))
                    })
                    .collect::<Result<_, _>>()?,
                monomorphized_constants: typechecker
                    .declarations
                    .monomorphized_constants
                    .clone()
                    .into_iter()
                    .map(|(id, (file, generic_id, decl))| {
                        Ok((
                            id,
                            (
                                (),
                                generic_id,
                                Declaration {
                                    name: decl.name,
                                    span: decl.span,
                                    value: typechecker.finalize(decl.value, &file)?,
                                },
                            ),
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
                                value: typechecker
                                    .variables
                                    .get(&id)
                                    .unwrap()
                                    .clone()
                                    .finalize(&typechecker.ctx, true)
                                    .ok_or_else(|| {
                                        Error::new(TypeError::UnresolvedType, decl.span)
                                    })?,
                            },
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            })
        })() {
            Ok(declarations) => declarations,
            Err(error) => {
                typechecker.errors.push(error);
                Declarations::default()
            }
        };

        let mut body = body
            .map(|body| {
                match body
                    .into_iter()
                    .map(|(file, expr)| typechecker.finalize(expr, &file))
                    .collect::<Result<_, _>>()
                {
                    Ok(declarations) => declarations,
                    Err(error) => {
                        typechecker.errors.push(error);
                        Vec::new()
                    }
                }
            })
            .unwrap_or_default();

        // Consolidate constants based on their type

        let mut cache = HashMap::new();
        let mut cached = BTreeSet::new();
        let mut map = BTreeMap::new();

        for (id, ((), generic_id, constant)) in &declarations.monomorphized_constants {
            let cached_id = *cache
                .entry((generic_id, constant.value.ty.clone()))
                .or_insert_with(|| {
                    cached.insert(*id);
                    *id
                });

            map.insert(*id, cached_id);
        }

        for id in declarations
            .monomorphized_constants
            .keys()
            .cloned()
            .collect::<Vec<_>>()
        {
            if !cached.contains(&id) {
                declarations.monomorphized_constants.remove(&id);
            }
        }

        let update = |expr: &mut Expression| {
            if let ExpressionKind::Constant(id) = &mut expr.kind {
                if let Some(cached_id) = map.get(id) {
                    *id = *cached_id;
                }
            }
        };

        for ((), _, constant) in declarations.monomorphized_constants.values_mut() {
            constant.value.traverse_mut(update)
        }

        for expr in &mut body {
            expr.traverse_mut(update);
        }

        // Build the final program

        let success = typechecker.errors.is_empty();

        let report = |error: Error, typechecker: &Typechecker<L>| {
            #[allow(unused_mut)]
            let mut diagnostic = match error.error {
                TypeError::ErrorExpression => return,
                TypeError::Recursive(_) => Diagnostic::error(
                    "recursive type",
                    vec![Note::primary(
                        error.span,
                        "the type of this references itself",
                    )],
                ),
                TypeError::Mismatch(actual, expected) => Diagnostic::error(
                    "mismatched types",
                    vec![Note::primary(
                        error.span,
                        format!(
                            "expected {}, but found {}",
                            typechecker.format_type(expected, true),
                            typechecker.format_type(actual, true)
                        ),
                    )],
                ),
                TypeError::MissingInstance(id, params) => {
                    let tr = typechecker.declarations.traits.get(&id).unwrap();

                    Diagnostic::error(
                        "missing instance",
                        std::iter::once(Note::primary(
                            error.span,
                            format!(
                                "could not find instance {}",
                                typechecker.format_type(FormattableType::Trait(id, params), true)
                            ),
                        ))
                        .chain(
                            tr.value
                                .attributes
                                .on_unimplemented
                                .as_ref()
                                .map(|message| Note::secondary(error.span, message)),
                        )
                        .collect(),
                    )
                }
                TypeError::AmbiguousTrait(_, candidates) => Diagnostic::error(
                    "could not determine the type of this expression",
                    std::iter::once(Note::primary(
                        error.span,
                        "try annotating the type with `::`",
                    ))
                    .chain(candidates.into_iter().map(|id| {
                        let instance = typechecker.generic_constants.get(&id).unwrap();
                        Note::secondary(instance.decl.span, "this instance could apply")
                    }))
                    .collect(),
                ),
                TypeError::UnresolvedType => Diagnostic::error(
                    "could not determine the type of this expression",
                    vec![Note::primary(
                        error.span,
                        "try annotating the type with `::`",
                    )],
                ),
            };

            #[cfg(debug_assertions)]
            {
                diagnostic.trace = error.trace;
            }

            typechecker.compiler.diagnostics.add(diagnostic);
        };

        let (unresolved_type_errors, other_errors): (Vec<_>, Vec<_>) =
            mem::take(&mut typechecker.errors)
                .into_iter()
                .partition(|e| matches!(e.error, TypeError::UnresolvedType));

        let should_report_unresolved_type_errors = other_errors.is_empty();

        for error in other_errors {
            report(error, &typechecker);
        }

        if should_report_unresolved_type_errors {
            for error in unresolved_type_errors {
                report(error, &typechecker);
            }
        }

        success.then(|| Program {
            body,
            declarations,
            top_level,
        })
    }
}

type Bound = (TraitId, UnresolvedType, Span);

#[derive(Debug, Clone)]
struct Constant<T, Ty, Attrs = ()> {
    file: Rc<RefCell<lower::File>>,
    decl: Declaration<T>,
    generic_ty: Ty,
    bounds: Vec<Bound>,
    attributes: Option<Attrs>,
}

#[derive(Debug, Clone)]
struct TraitDefinition {
    ty: UnresolvedType,
    params: Vec<TypeParameterId>,
}

#[derive(Debug, Clone)]
struct TypeDefinition {
    kind: TypeDefinitionKind,
    params: Vec<TypeParameterId>,
}

#[derive(Debug, Clone)]
enum TypeDefinitionKind {
    Marker,
    Structure(Vec<UnresolvedType>, HashMap<InternedString, usize>),
    Enumeration(Vec<Vec<UnresolvedType>>, Vec<GenericConstantId>),
}

#[derive(Debug, Clone)]
struct Error {
    error: TypeError,
    span: Span,

    #[cfg(debug_assertions)]
    trace: Backtrace,
}

impl Error {
    fn new(error: TypeError, span: Span) -> Self {
        Error {
            error,
            span,

            #[cfg(debug_assertions)]
            trace: Backtrace::new(),
        }
    }
}

struct Typechecker<L: Loader> {
    ctx: Context,
    errors: Vec<Error>,
    variables: BTreeMap<VariableId, UnresolvedType>,
    return_tys: Vec<Option<UnresolvedType>>,
    loop_tys: Vec<Option<UnresolvedType>>,
    traits: BTreeMap<TraitId, TraitDefinition>,
    types: BTreeMap<TypeId, TypeDefinition>,
    generic_constants: BTreeMap<
        GenericConstantId,
        Constant<lower::Expression, UnresolvedType, lower::DeclarationAttributes>,
    >,
    monomorphized_constants: BTreeMap<
        MonomorphizedConstantId,
        (Span, GenericConstantId, Constant<UnresolvedExpression, ()>),
    >,
    declared_instances: BTreeMap<TraitId, Vec<GenericConstantId>>,
    bound_instances: BTreeMap<TraitId, Vec<GenericConstantId>>,
    declarations: Declarations<MonomorphizedExpression, UnresolvedType, Rc<RefCell<lower::File>>>,
    compiler: Compiler<L>,
}

impl<L: Loader> Typechecker<L> {
    fn typecheck_expr(
        &mut self,
        expr: &lower::Expression,
        file: &Rc<RefCell<lower::File>>,
        suppress_errors: bool,
    ) -> UnresolvedExpression {
        match &expr.kind {
            lower::ExpressionKind::Error => UnresolvedExpression::error(expr.span),
            lower::ExpressionKind::Marker(id) => {
                let marker = self.types.get(id).unwrap().clone();

                let mut ty = UnresolvedType::Named(
                    *id,
                    marker
                        .params
                        .into_iter()
                        .map(UnresolvedType::Parameter)
                        .collect(),
                    TypeStructure::Marker,
                );

                let mut substitutions = BTreeMap::new();
                self.add_substitutions(&mut ty, &mut substitutions);

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
            lower::ExpressionKind::Variable(var) => {
                let ty = self
                    .variables
                    .get(var)
                    .expect("uninitialized variable")
                    .clone();

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Variable(*var),
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
            lower::ExpressionKind::Block(statements) => {
                let (ty, statements) = self.typecheck_block(statements, file, suppress_errors);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Block(statements),
                }
            }
            lower::ExpressionKind::Call(function, input) => {
                let function = self.typecheck_expr(function, file, suppress_errors);
                let input = self.typecheck_expr(input, file, suppress_errors);

                let input_ty = UnresolvedType::Variable(self.ctx.new_variable());
                let output_ty = UnresolvedType::Variable(self.ctx.new_variable());

                if let Err(error) = self.ctx.unify(
                    function.ty.clone(),
                    UnresolvedType::Function(
                        Box::new(input_ty.clone()),
                        Box::new(output_ty.clone()),
                    ),
                ) {
                    if !suppress_errors {
                        self.errors.push(Error::new(error, function.span));
                    }

                    return UnresolvedExpression::error(expr.span);
                };

                if let Err(error) = self.ctx.unify(input.ty.clone(), input_ty) {
                    if !suppress_errors {
                        self.errors.push(Error::new(error, input.span));
                    }

                    return UnresolvedExpression::error(expr.span);
                };

                UnresolvedExpression {
                    span: expr.span,
                    ty: output_ty,
                    kind: UnresolvedExpressionKind::Call(Box::new(function), Box::new(input)),
                }
            }
            lower::ExpressionKind::Function(pattern, body) => {
                let input_ty = UnresolvedType::Variable(self.ctx.new_variable());

                let pattern =
                    self.typecheck_pattern(pattern, input_ty.clone(), file, suppress_errors);

                self.return_tys.push(None);

                let body = self.typecheck_expr(body, file, suppress_errors);

                let return_ty = self
                    .return_tys
                    .pop()
                    .unwrap()
                    .unwrap_or_else(|| UnresolvedType::Variable(self.ctx.new_variable()));

                if let Err(errors) = self.ctx.unify(body.ty.clone(), return_ty.clone()) {
                    if !suppress_errors {
                        self.errors.push(Error::new(errors, body.span));
                    }

                    return UnresolvedExpression::error(expr.span);
                };

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Function(Box::new(input_ty), Box::new(return_ty)),
                    kind: UnresolvedExpressionKind::Function(pattern, Box::new(body)),
                }
            }
            lower::ExpressionKind::When(input, arms) => {
                let input = self.typecheck_expr(input, file, suppress_errors);

                let arms = arms
                    .iter()
                    .map(|arm| self.typecheck_arm(arm, input.ty.clone(), file, suppress_errors))
                    .collect::<Vec<_>>();

                let ty = {
                    let first_type = arms.iter().find_map(|arm| {
                        let mut ty = arm.body.ty.clone();
                        ty.apply(&self.ctx);
                        (!matches!(ty, UnresolvedType::Bottom(_))).then(|| ty)
                    });

                    if let Some(first_type) = first_type {
                        for arm in &arms {
                            if let Err(error) =
                                self.ctx.unify(arm.body.ty.clone(), first_type.clone())
                            {
                                if !suppress_errors {
                                    self.errors.push(Error::new(error, arm.body.span));
                                }
                            };
                        }

                        first_type
                    } else {
                        UnresolvedType::Bottom(BottomTypeReason::Annotated)
                    }
                };

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::When(Box::new(input), arms),
                }
            }
            lower::ExpressionKind::External(abi, identifier, inputs) => {
                let inputs = inputs
                    .iter()
                    .map(|expr| self.typecheck_expr(expr, file, suppress_errors))
                    .collect();

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Variable(self.ctx.new_variable()),
                    kind: UnresolvedExpressionKind::External(*abi, *identifier, inputs),
                }
            }
            lower::ExpressionKind::Annotate(expr, ty) => {
                let ty = self.convert_type_annotation(ty, file);
                let value = self.typecheck_expr(expr, file, suppress_errors);

                if let Err(error) = self.ctx.unify(value.ty, ty.clone()) {
                    if !suppress_errors {
                        self.errors.push(Error::new(error, value.span));
                    }

                    return UnresolvedExpression::error(expr.span);
                };

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: value.kind,
                }
            }
            lower::ExpressionKind::Initialize(pattern, value) => {
                let value = self.typecheck_expr(value, file, suppress_errors);

                let pattern =
                    self.typecheck_pattern(pattern, value.ty.clone(), file, suppress_errors);

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Tuple(Vec::new()),
                    kind: UnresolvedExpressionKind::Initialize(pattern, Box::new(value)),
                }
            }
            lower::ExpressionKind::Instantiate(id, fields) => {
                let structure = self.types.get(id).unwrap().clone();

                let (mut structure_field_tys, structure_field_names) = match &structure.kind {
                    TypeDefinitionKind::Structure(field_tys, field_names) => {
                        (field_tys.clone(), field_names.clone())
                    }
                    _ => unreachable!(), // or do we need to display an error like above?
                };

                let mut ty = UnresolvedType::Named(
                    *id,
                    structure
                        .params
                        .into_iter()
                        .map(UnresolvedType::Parameter)
                        .collect(),
                    TypeStructure::Structure(structure_field_tys.clone()),
                );

                let mut substitutions = BTreeMap::new();
                self.add_substitutions(&mut ty, &mut substitutions);

                for index in structure_field_names.values() {
                    self.add_substitutions(&mut structure_field_tys[*index], &mut substitutions);
                }

                let mut fields_by_index = structure_field_names.iter().collect::<Vec<_>>();
                fields_by_index.sort_by_key(|(_, index)| *index);

                let mut unpopulated_fields = vec![None; fields_by_index.len()];
                let mut extra_fields = Vec::new();

                for (name, expr) in fields {
                    let (index, ty) = match structure_field_names.get(name) {
                        Some(index) => (*index, structure_field_tys[*index].clone()),
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
                                self.errors.push(Error::new(errors, expr.span));
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
            lower::ExpressionKind::Variant(id, index, values) => {
                let enumeration = self.types.get(id).unwrap().clone();

                let (variants_tys, mut variant_constructor) = match &enumeration.kind {
                    TypeDefinitionKind::Enumeration(variants_tys, variants_constructors) => {
                        let id = &variants_constructors[*index];

                        (
                            variants_tys.clone(),
                            self.generic_constants.get(id).unwrap().clone(),
                        )
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
                    TypeStructure::Enumeration(variants_tys.clone()),
                );

                let mut substitutions = BTreeMap::new();
                self.add_substitutions(&mut ty, &mut substitutions);

                for (_, ty, _) in &mut variant_constructor.bounds {
                    self.add_substitutions(ty, &mut substitutions);
                }

                let mut variant_tys = variants_tys[*index].clone();

                for ty in &mut variant_tys {
                    self.add_substitutions(ty, &mut substitutions);
                }

                let values = values
                    .iter()
                    .zip(variant_tys)
                    .map(|(expr, ty)| {
                        let value = self.typecheck_expr(expr, file, suppress_errors);

                        if let Err(errors) = self.ctx.unify(value.ty.clone(), ty) {
                            if !suppress_errors {
                                self.errors.push(Error::new(errors, expr.span));
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
            lower::ExpressionKind::Return(value) => {
                let value = self.typecheck_expr(value, file, suppress_errors);

                let return_ty = match self.return_tys.last_mut() {
                    Some(ty) => ty,
                    None => return UnresolvedExpression::error(expr.span),
                };

                if let Some(return_ty) = return_ty {
                    if let Err(errors) = self.ctx.unify(return_ty.clone(), value.ty.clone()) {
                        if !suppress_errors {
                            self.errors.push(Error::new(errors, expr.span));
                        }

                        return UnresolvedExpression::error(expr.span);
                    }
                } else {
                    *return_ty = Some(value.ty.clone());
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Bottom(BottomTypeReason::Annotated),
                    kind: UnresolvedExpressionKind::Return(Box::new(value)),
                }
            }
            lower::ExpressionKind::Loop(body) => {
                self.loop_tys.push(None);

                let body = self.typecheck_expr(body, file, suppress_errors);

                let loop_ty = self
                    .loop_tys
                    .pop()
                    .unwrap()
                    .unwrap_or_else(|| UnresolvedType::Variable(self.ctx.new_variable()));

                if let Err(errors) = self
                    .ctx
                    .unify(body.ty.clone(), UnresolvedType::Tuple(Vec::new()))
                {
                    if !suppress_errors {
                        self.errors.push(Error::new(errors, expr.span));
                    }

                    return UnresolvedExpression::error(expr.span);
                };

                UnresolvedExpression {
                    span: expr.span,
                    ty: loop_ty,
                    kind: UnresolvedExpressionKind::Loop(Box::new(body)),
                }
            }
            lower::ExpressionKind::Break(value) => {
                let value = self.typecheck_expr(value, file, suppress_errors);

                let loop_ty = match self.loop_tys.last_mut() {
                    Some(ty) => ty,
                    None => return UnresolvedExpression::error(expr.span),
                };

                if let Some(loop_ty) = loop_ty {
                    if let Err(errors) = self.ctx.unify(loop_ty.clone(), value.ty.clone()) {
                        if !suppress_errors {
                            self.errors.push(Error::new(errors, expr.span));
                        }

                        return UnresolvedExpression::error(expr.span);
                    }
                } else {
                    *loop_ty = Some(value.ty.clone());
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Bottom(BottomTypeReason::Annotated),
                    kind: UnresolvedExpressionKind::Break(Box::new(value)),
                }
            }
            lower::ExpressionKind::Continue => UnresolvedExpression {
                span: expr.span,
                ty: UnresolvedType::Bottom(BottomTypeReason::Annotated),
                kind: UnresolvedExpressionKind::Continue,
            },
            lower::ExpressionKind::Tuple(exprs) => {
                let exprs = exprs
                    .iter()
                    .map(|expr| self.typecheck_expr(expr, file, suppress_errors))
                    .collect::<Vec<_>>();

                let ty = UnresolvedType::Tuple(exprs.iter().map(|expr| expr.ty.clone()).collect());

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Tuple(exprs),
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
            .unwrap_or(UnresolvedType::Tuple(Vec::new()));

        (ty, statements)
    }

    fn typecheck_arm(
        &mut self,
        arm: &lower::Arm,
        ty: UnresolvedType,
        file: &Rc<RefCell<lower::File>>,
        suppress_errors: bool,
    ) -> UnresolvedArm {
        UnresolvedArm {
            span: arm.span,
            pattern: self.typecheck_pattern(&arm.pattern, ty, file, suppress_errors),
            body: self.typecheck_expr(&arm.body, file, suppress_errors),
        }
    }

    fn typecheck_pattern(
        &mut self,
        pattern: &lower::Pattern,
        ty: UnresolvedType,
        file: &Rc<RefCell<lower::File>>,
        suppress_errors: bool,
    ) -> UnresolvedPattern {
        fn typecheck_pattern<L: Loader>(
            tc: &mut Typechecker<L>,
            pattern: &lower::Pattern,
            ty: Option<UnresolvedType>,
            file: &Rc<RefCell<lower::File>>,
            suppress_errors: bool,
        ) -> UnresolvedPattern {
            let kind = match &pattern.kind {
                lower::PatternKind::Error => UnresolvedPatternKind::Error,
                lower::PatternKind::Wildcard => UnresolvedPatternKind::Wildcard,
                lower::PatternKind::Number(number) => UnresolvedPatternKind::Number(*number),
                lower::PatternKind::Text(text) => UnresolvedPatternKind::Text(*text),
                lower::PatternKind::Variable(var) => {
                    tc.variables.insert(
                        *var,
                        ty.unwrap_or_else(|| UnresolvedType::Variable(tc.ctx.new_variable())),
                    );

                    UnresolvedPatternKind::Variable(*var)
                }
                lower::PatternKind::Destructure(fields) => UnresolvedPatternKind::Destructure(
                    fields
                        .iter()
                        .map(|(name, pattern)| {
                            (
                                *name,
                                typecheck_pattern(tc, pattern, None, file, suppress_errors),
                            )
                        })
                        .collect(),
                ),
                lower::PatternKind::Variant(ty, variant, values) => UnresolvedPatternKind::Variant(
                    *ty,
                    *variant,
                    values
                        .iter()
                        .map(|pattern| typecheck_pattern(tc, pattern, None, file, suppress_errors))
                        .collect(),
                ),
                lower::PatternKind::Annotate(inner, ty) => UnresolvedPatternKind::Annotate(
                    Box::new(typecheck_pattern(tc, inner, None, file, suppress_errors)),
                    tc.convert_type_annotation(ty, file),
                ),
                lower::PatternKind::Or(lhs, rhs) => UnresolvedPatternKind::Or(
                    Box::new(typecheck_pattern(tc, lhs, None, file, suppress_errors)),
                    Box::new(typecheck_pattern(tc, rhs, None, file, suppress_errors)),
                ),
                lower::PatternKind::Where(pattern, condition) => UnresolvedPatternKind::Where(
                    Box::new(typecheck_pattern(tc, pattern, None, file, suppress_errors)),
                    Box::new(tc.typecheck_expr(condition, file, suppress_errors)),
                ),
                lower::PatternKind::Tuple(patterns) => UnresolvedPatternKind::Tuple(
                    patterns
                        .iter()
                        .map(|pattern| typecheck_pattern(tc, pattern, None, file, suppress_errors))
                        .collect(),
                ),
            };

            UnresolvedPattern {
                span: pattern.span,
                kind,
            }
        }

        typecheck_pattern(self, pattern, Some(ty), file, suppress_errors)
    }

    fn monomorphize_constant(
        &mut self,
        id: GenericConstantId,
        span: Span,
    ) -> (MonomorphizedConstantId, UnresolvedType) {
        let mut constant = self.generic_constants.get(&id).unwrap().clone();
        let body = constant.decl.value;

        let mut body = self.typecheck_expr(&body, &constant.file, true);

        self.ctx
            .unify(body.ty.clone(), constant.generic_ty)
            .expect("failed to unify constant body with its generic type");

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
                id,
                Constant {
                    file: constant.file,
                    decl: Declaration {
                        name: constant.decl.name,
                        span: constant.decl.span,
                        value: body,
                    },
                    generic_ty: (),
                    bounds: constant.bounds,
                    attributes: None,
                },
            ),
        );

        (monomorphized_id, ty)
    }

    fn monomorphize_arm(
        &mut self,
        arm: UnresolvedArm,
        ty: UnresolvedType,
        match_set: &mut MatchSet,
        file: &Rc<RefCell<lower::File>>,
        inside_generic_constant: Option<(GenericConstantId, MonomorphizedConstantId)>,
    ) -> Result<MonomorphizedArm, Error> {
        Ok(MonomorphizedArm {
            span: arm.span,
            pattern: self
                .monomorphize_pattern(arm.pattern, ty, match_set, file, inside_generic_constant)
                .ok_or_else(|| Error::new(TypeError::ErrorExpression, arm.span))?,
            body: self.monomorphize(arm.body, file, inside_generic_constant)?,
        })
    }

    fn monomorphize_pattern(
        &mut self,
        pattern: UnresolvedPattern,
        mut ty: UnresolvedType,
        match_set: &mut MatchSet,
        file: &Rc<RefCell<lower::File>>,
        inside_generic_constant: Option<(GenericConstantId, MonomorphizedConstantId)>,
    ) -> Option<MonomorphizedPattern> {
        ty.apply(&self.ctx);

        let kind = (|| match pattern.kind {
            UnresolvedPatternKind::Error => {
                match_set.set_matched(true);
                None
            }
            UnresolvedPatternKind::Number(number) => {
                if let Err(error) = self
                    .ctx
                    .unify(ty, UnresolvedType::Builtin(BuiltinType::Number))
                {
                    self.errors.push(Error::new(error, pattern.span));
                }

                Some(MonomorphizedPatternKind::Number(number))
            }
            UnresolvedPatternKind::Text(text) => {
                if let Err(error) = self
                    .ctx
                    .unify(ty, UnresolvedType::Builtin(BuiltinType::Text))
                {
                    self.errors.push(Error::new(error, pattern.span));
                }

                Some(MonomorphizedPatternKind::Text(text))
            }
            UnresolvedPatternKind::Wildcard => {
                match_set.set_matched(true);
                Some(MonomorphizedPatternKind::Wildcard)
            }
            UnresolvedPatternKind::Variable(var) => {
                match_set.set_matched(true);

                let mut var_ty = self
                    .variables
                    .get(&var)
                    .expect("uninitialized variable")
                    .clone();

                let mut substitutions = BTreeMap::new();
                self.add_substitutions(&mut var_ty, &mut substitutions);

                let ctx = self.ctx.clone();
                if let Err(error) = self.ctx.unify(var_ty.clone(), ty.clone()).or_else(|_| {
                    // HACK: Try the other way around if unification doesn't
                    // work the first time (remove if this causes unsoundness)
                    self.ctx = ctx;
                    self.ctx.unify(ty.clone(), var_ty)
                }) {
                    self.errors.push(Error::new(error, pattern.span));
                }

                let decl = file
                    .borrow()
                    .declarations
                    .variables
                    .get(&var)
                    .unwrap()
                    .clone();

                self.declarations.variables.insert(
                    var,
                    Declaration {
                        name: decl.name,
                        span: decl.span,
                        value: ty.clone(),
                    },
                );

                Some(MonomorphizedPatternKind::Variable(var))
            }
            UnresolvedPatternKind::Destructure(fields) => {
                let (id, params) = match ty.clone() {
                    UnresolvedType::Named(id, params, _) => (id, params),
                    _ => {
                        self.compiler.diagnostics.add(Diagnostic::error(
                            "cannot destructure this value",
                            vec![Note::primary(pattern.span, "value is not a data structure")],
                        ));

                        return None;
                    }
                };

                let structure = match self.types.get(&id) {
                    Some(ty) => ty,
                    None => {
                        self.compiler.diagnostics.add(Diagnostic::error(
                            "cannot destructure this value",
                            vec![Note::primary(pattern.span, "value is not a data structure")],
                        ));

                        return None;
                    }
                };

                let (structure_field_tys, structure_field_names) = match &structure.kind {
                    TypeDefinitionKind::Structure(field_tys, field_names) => {
                        (field_tys.clone(), field_names.clone())
                    }
                    _ => unreachable!(), // or do we need to display an error like above?
                };

                let substitutions = structure
                    .params
                    .iter()
                    .copied()
                    .zip(params)
                    .collect::<BTreeMap<_, _>>();

                let field_match_sets = match match_set {
                    MatchSet::Structure(fields) => fields,
                    _ => {
                        ty.apply(&self.ctx);
                        *match_set = self.match_set_from(&ty).unwrap();

                        match match_set {
                            MatchSet::Structure(fields) => fields,
                            _ => unreachable!(),
                        }
                    }
                };

                let fields = fields
                    .into_iter()
                    .filter_map(|(name, pattern)| {
                        let index = match structure_field_names.get(&name) {
                            Some(index) => *index,
                            None => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("value has no member named '{}'", name),
                                    vec![Note::primary(pattern.span, "no such member")],
                                ));

                                return None;
                            }
                        };

                        let mut member_ty = structure_field_tys[index].clone();
                        member_ty.instantiate_with(&substitutions);

                        let (matches, match_set) = &mut field_match_sets[index];
                        *matches = true;

                        let pattern = self.monomorphize_pattern(
                            pattern,
                            member_ty,
                            match_set,
                            file,
                            inside_generic_constant,
                        )?;

                        Some((index, pattern))
                    })
                    .collect();

                for (matches, field_match_set) in field_match_sets {
                    if *matches {
                        continue;
                    }

                    *matches = true;
                    field_match_set.set_matched(true);
                }

                Some(MonomorphizedPatternKind::Destructure(fields))
            }
            UnresolvedPatternKind::Variant(variant_ty, variant, values) => {
                let (id, params) = match &ty {
                    UnresolvedType::Named(id, params, _) => (id, params),
                    _ => {
                        self.compiler.diagnostics.add(Diagnostic::error(
                            "cannot apply pattern to this value",
                            vec![Note::primary(pattern.span, "value is not an enumeration")],
                        ));

                        return None;
                    }
                };

                let enumeration = match self.types.get(id) {
                    Some(ty) => ty,
                    None => {
                        self.compiler.diagnostics.add(Diagnostic::error(
                            "cannot apply pattern to this value",
                            vec![Note::primary(pattern.span, "value is not an enumeration")],
                        ));

                        return None;
                    }
                };

                let mut variant_tys = match &enumeration.kind {
                    TypeDefinitionKind::Enumeration(variant_tys, _) => variant_tys[variant].clone(),
                    _ => unreachable!(), // or do we need to display an error like above?
                };

                let substitutions = enumeration
                    .params
                    .iter()
                    .copied()
                    .zip(params.iter().cloned())
                    .collect::<BTreeMap<_, _>>();

                for ty in &mut variant_tys {
                    ty.instantiate_with(&substitutions);
                }

                if let Err(error) = self.ctx.unify(
                    ty.clone(),
                    UnresolvedType::Named(
                        variant_ty,
                        enumeration
                            .params
                            .iter()
                            .map(|param| substitutions.get(param).unwrap().clone())
                            .collect(),
                        // HACK: Optimization because unification doesn't take structure into
                        // account -- the structure can be applied during finalization
                        TypeStructure::Marker,
                    ),
                ) {
                    self.errors.push(Error::new(error, pattern.span));
                    return None;
                }

                let (matches, variant_match_sets) = match match_set {
                    MatchSet::Enumeration(variants) => &mut variants[variant],
                    _ => {
                        ty.apply(&self.ctx);
                        *match_set = self.match_set_from(&ty).unwrap();

                        match match_set {
                            MatchSet::Enumeration(variants) => &mut variants[variant],
                            _ => unreachable!(),
                        }
                    }
                };

                *matches = true;

                let pattern = MonomorphizedPatternKind::Variant(
                    variant,
                    values
                        .into_iter()
                        .zip(variant_tys)
                        .zip(variant_match_sets)
                        .map(|((pattern, variant_ty), match_set)| {
                            *matches = true;

                            self.monomorphize_pattern(
                                pattern,
                                variant_ty,
                                match_set,
                                file,
                                inside_generic_constant,
                            )
                        })
                        .collect::<Option<_>>()?,
                );

                Some(pattern)
            }
            UnresolvedPatternKind::Annotate(inner, inner_ty) => {
                if let Err(error) = self.ctx.unify(ty.clone(), inner_ty) {
                    self.errors.push(Error::new(error, pattern.span));
                }

                Some(
                    self.monomorphize_pattern(
                        *inner,
                        ty,
                        match_set,
                        file,
                        inside_generic_constant,
                    )?
                    .kind,
                )
            }
            UnresolvedPatternKind::Or(lhs, rhs) => Some(MonomorphizedPatternKind::Or(
                Box::new(self.monomorphize_pattern(
                    *lhs,
                    ty.clone(),
                    match_set,
                    file,
                    inside_generic_constant,
                )?),
                Box::new(self.monomorphize_pattern(
                    *rhs,
                    ty,
                    match_set,
                    file,
                    inside_generic_constant,
                )?),
            )),
            UnresolvedPatternKind::Where(pattern, condition) => {
                let pattern = self.monomorphize_pattern(
                    *pattern,
                    ty,
                    match_set,
                    file,
                    inside_generic_constant,
                )?;

                let condition_span = condition.span;
                let condition = match self.monomorphize(*condition, file, inside_generic_constant) {
                    Ok(expr) => expr,
                    Err(error) => {
                        self.errors.push(error);
                        return None;
                    }
                };

                if let Some(boolean_ty) = file.borrow().global_attributes.language_items.boolean {
                    if let Err(error) = self.ctx.unify(
                        condition.ty.clone(),
                        UnresolvedType::Named(
                            boolean_ty,
                            Vec::new(),
                            // HACK: Optimization because unification doesn't take structure into
                            // account -- the structure can be applied during finalization
                            TypeStructure::Marker,
                        ),
                    ) {
                        self.errors.push(Error::new(error, condition.span));
                    }
                } else {
                    self.compiler.diagnostics.add(Diagnostic::error(
                        "cannot find `boolean` language item",
                        vec![Note::primary(
                            condition_span,
                            "typechecking this condition requires the `boolean` language item",
                        )],
                    ))
                }

                match_set.set_matched(false);

                Some(MonomorphizedPatternKind::Where(
                    Box::new(pattern),
                    Box::new(condition),
                ))
            }
            UnresolvedPatternKind::Tuple(patterns) => {
                let tys = patterns
                    .iter()
                    .map(|_| UnresolvedType::Variable(self.ctx.new_variable()))
                    .collect::<Vec<_>>();

                if let Err(error) = self
                    .ctx
                    .unify(ty.clone(), UnresolvedType::Tuple(tys.clone()))
                {
                    self.errors.push(Error::new(error, pattern.span));
                    return None;
                }

                let match_sets = match match_set {
                    MatchSet::Tuple(sets) => sets,
                    _ => {
                        ty.apply(&self.ctx);
                        *match_set = self.match_set_from(&ty).unwrap();

                        match match_set {
                            MatchSet::Tuple(sets) => sets,
                            _ => unreachable!(),
                        }
                    }
                };

                Some(MonomorphizedPatternKind::Tuple(
                    patterns
                        .into_iter()
                        .zip(tys)
                        .zip(match_sets)
                        .map(|((pattern, ty), match_set)| {
                            self.monomorphize_pattern(
                                pattern,
                                ty,
                                match_set,
                                file,
                                inside_generic_constant,
                            )
                        })
                        .collect::<Option<_>>()?,
                ))
            }
        })()?;

        Some(MonomorphizedPattern {
            span: pattern.span,
            kind,
        })
    }

    fn monomorphize(
        &mut self,
        mut expr: UnresolvedExpression,
        file: &Rc<RefCell<lower::File>>,
        inside_generic_constant: Option<(GenericConstantId, MonomorphizedConstantId)>,
    ) -> Result<MonomorphizedExpression, Error> {
        expr.ty.apply(&self.ctx);

        Ok(MonomorphizedExpression {
            span: expr.span,
            ty: expr.ty.clone(),
            kind: (|| {
                Ok(match expr.kind {
                    UnresolvedExpressionKind::Error => MonomorphizedExpressionKind::Error,
                    UnresolvedExpressionKind::Marker => MonomorphizedExpressionKind::Marker,
                    UnresolvedExpressionKind::Constant(id) => {
                        if let Some((inside_generic_id, inside_monomorphized_id)) =
                            inside_generic_constant
                        {
                            if inside_generic_id == id {
                                return Ok(MonomorphizedExpressionKind::Constant(
                                    inside_monomorphized_id,
                                ));
                            }
                        }

                        let (monomorphized_id, ty) = self.monomorphize_constant(id, expr.span);

                        if let Err(error) = self.ctx.unify(ty, expr.ty) {
                            self.errors.push(Error::new(error, expr.span));
                        }

                        MonomorphizedExpressionKind::Constant(monomorphized_id)
                    }
                    UnresolvedExpressionKind::Variable(var) => {
                        let mut ty = self
                            .variables
                            .get(&var)
                            .expect("uninitialized variable")
                            .clone();

                        let mut substitutions = BTreeMap::new();
                        self.add_substitutions(&mut ty, &mut substitutions);

                        if let Err(error) = self.ctx.unify(expr.ty, ty) {
                            self.errors.push(Error::new(error, expr.span));
                        }

                        MonomorphizedExpressionKind::Variable(var)
                    }
                    UnresolvedExpressionKind::Text(text) => MonomorphizedExpressionKind::Text(text),
                    UnresolvedExpressionKind::Number(number) => {
                        MonomorphizedExpressionKind::Number(number)
                    }
                    UnresolvedExpressionKind::Block(statements) => {
                        MonomorphizedExpressionKind::Block(
                            statements
                                .into_iter()
                                .map(|expr| self.monomorphize(expr, file, inside_generic_constant))
                                .collect::<Result<_, _>>()?,
                        )
                    }
                    UnresolvedExpressionKind::Call(func, input) => {
                        // Monomorphize in reverse order so that inner traits
                        // are resolved first, eg. in the case of `T2 (T1 x)`
                        let input = self.monomorphize(*input, file, inside_generic_constant)?;
                        let func = self.monomorphize(*func, file, inside_generic_constant)?;

                        MonomorphizedExpressionKind::Call(Box::new(func), Box::new(input))
                    }
                    UnresolvedExpressionKind::Function(pattern, body) => {
                        let mut input_ty = match expr.ty {
                            UnresolvedType::Function(input, _) => *input,
                            _ => unreachable!(),
                        };

                        input_ty.apply(&self.ctx);

                        let mut match_set = self
                            .match_set_from(&input_ty)
                            .map_err(|error| Error::new(error, expr.span))?;

                        let pattern_span = pattern.span;
                        let pattern = self
                            .monomorphize_pattern(
                                pattern,
                                input_ty,
                                &mut match_set,
                                file,
                                inside_generic_constant,
                            )
                            .ok_or_else(|| Error::new(TypeError::ErrorExpression, pattern_span))?;

                        self.assert_matched(&match_set, pattern.span, true)?;

                        MonomorphizedExpressionKind::Function(
                            pattern,
                            Box::new(self.monomorphize(*body, file, inside_generic_constant)?),
                        )
                    }
                    UnresolvedExpressionKind::When(input, arms) => {
                        let mut input = self.monomorphize(*input, file, inside_generic_constant)?;
                        input.ty.apply(&self.ctx);

                        let mut match_set = self
                            .match_set_from(&input.ty)
                            .map_err(|error| Error::new(error, expr.span))?;

                        let arms = arms
                            .into_iter()
                            .map(|arm| {
                                self.monomorphize_arm(
                                    arm,
                                    input.ty.clone(),
                                    &mut match_set,
                                    file,
                                    inside_generic_constant,
                                )
                            })
                            .collect::<Result<_, _>>()?;

                        self.assert_matched(&match_set, expr.span, false)?;

                        MonomorphizedExpressionKind::When(Box::new(input), arms)
                    }
                    UnresolvedExpressionKind::External(abi, identifier, inputs) => {
                        MonomorphizedExpressionKind::External(
                            abi,
                            identifier,
                            inputs
                                .into_iter()
                                .map(|expr| self.monomorphize(expr, file, inside_generic_constant))
                                .collect::<Result<_, _>>()?,
                        )
                    }
                    UnresolvedExpressionKind::Initialize(pattern, value) => {
                        // Resolve the right-hand side first
                        let mut value = self.monomorphize(*value, file, inside_generic_constant)?;
                        value.ty.apply(&self.ctx);

                        let mut match_set = self
                            .match_set_from(&value.ty)
                            .map_err(|error| Error::new(error, expr.span))?;

                        let pattern = self
                            .monomorphize_pattern(
                                pattern,
                                value.ty.clone(),
                                &mut match_set,
                                file,
                                inside_generic_constant,
                            )
                            .ok_or_else(|| Error::new(TypeError::ErrorExpression, value.span))?;

                        self.assert_matched(&match_set, pattern.span, true)?;

                        MonomorphizedExpressionKind::Initialize(pattern, Box::new(value))
                    }
                    UnresolvedExpressionKind::Structure(fields) => {
                        MonomorphizedExpressionKind::Structure(
                            fields
                                .into_iter()
                                .map(|expr| self.monomorphize(expr, file, inside_generic_constant))
                                .collect::<Result<_, _>>()?,
                        )
                    }
                    UnresolvedExpressionKind::Variant(index, values) => {
                        MonomorphizedExpressionKind::Variant(
                            index,
                            values
                                .into_iter()
                                .map(|expr| self.monomorphize(expr, file, inside_generic_constant))
                                .collect::<Result<_, _>>()?,
                        )
                    }
                    UnresolvedExpressionKind::Trait(tr) => {
                        let instance = self
                            .instance_for(tr, expr.ty.clone())
                            .map_err(|error| Error::new(error, expr.span))?;

                        let (monomorphized_instance, ty) =
                            self.monomorphize_constant(instance, expr.span);

                        if let Err(error) = self.ctx.unify(ty, expr.ty.clone()) {
                            self.errors.push(Error::new(error, expr.span));
                        }

                        MonomorphizedExpressionKind::Constant(monomorphized_instance)
                    }
                    UnresolvedExpressionKind::Return(value) => MonomorphizedExpressionKind::Return(
                        Box::new(self.monomorphize(*value, file, inside_generic_constant)?),
                    ),
                    UnresolvedExpressionKind::Loop(body) => MonomorphizedExpressionKind::Loop(
                        Box::new(self.monomorphize(*body, file, inside_generic_constant)?),
                    ),
                    UnresolvedExpressionKind::Break(value) => MonomorphizedExpressionKind::Break(
                        Box::new(self.monomorphize(*value, file, inside_generic_constant)?),
                    ),
                    UnresolvedExpressionKind::Continue => MonomorphizedExpressionKind::Continue,
                    UnresolvedExpressionKind::Tuple(exprs) => MonomorphizedExpressionKind::Tuple(
                        exprs
                            .into_iter()
                            .map(|expr| self.monomorphize(expr, file, inside_generic_constant))
                            .collect::<Result<_, _>>()?,
                    ),
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

    fn finalize(
        &mut self,
        expr: MonomorphizedExpression,
        file: &Rc<RefCell<lower::File>>,
    ) -> Result<Expression, Error> {
        self.finalize_internal(expr, false, file)
    }

    fn finalize_generic(
        &mut self,
        expr: MonomorphizedExpression,
        file: &Rc<RefCell<lower::File>>,
    ) -> Result<Expression, Error> {
        self.finalize_internal(expr, true, file)
    }

    fn finalize_internal(
        &mut self,
        expr: MonomorphizedExpression,
        generic: bool,
        file: &Rc<RefCell<lower::File>>,
    ) -> Result<Expression, Error> {
        Ok(Expression {
            span: expr.span,
            ty: expr
                .ty
                .finalize(&self.ctx, generic)
                .ok_or_else(|| Error::new(TypeError::UnresolvedType, expr.span))?,
            kind: match expr.kind {
                MonomorphizedExpressionKind::Error => {
                    return Err(Error::new(TypeError::ErrorExpression, expr.span));
                }
                MonomorphizedExpressionKind::Marker => ExpressionKind::Marker,
                MonomorphizedExpressionKind::Constant(id) => ExpressionKind::Constant(id),
                MonomorphizedExpressionKind::Variable(var) => ExpressionKind::Variable(var),
                MonomorphizedExpressionKind::Text(text) => ExpressionKind::Text(text),
                MonomorphizedExpressionKind::Number(number) => ExpressionKind::Number(number),
                MonomorphizedExpressionKind::Block(statements) => ExpressionKind::Block(
                    statements
                        .into_iter()
                        .map(|expr| self.finalize_internal(expr, generic, file))
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedExpressionKind::Call(func, input) => ExpressionKind::Call(
                    Box::new(self.finalize_internal(*func, generic, file)?),
                    Box::new(self.finalize_internal(*input, generic, file)?),
                ),
                MonomorphizedExpressionKind::Function(pattern, body) => ExpressionKind::Function(
                    self.finalize_pattern(pattern, generic, file)?,
                    Box::new(self.finalize_internal(*body, generic, file)?),
                ),
                MonomorphizedExpressionKind::When(input, arms) => ExpressionKind::When(
                    Box::new(self.finalize_internal(*input, generic, file)?),
                    arms.into_iter()
                        .map(|arm| {
                            Ok(Arm {
                                span: arm.span,
                                pattern: self.finalize_pattern(arm.pattern, generic, file)?,
                                body: self.finalize_internal(arm.body, generic, file)?,
                            })
                        })
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedExpressionKind::External(abi, identifier, inputs) => {
                    ExpressionKind::External(
                        abi,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|expr| self.finalize_internal(expr, generic, file))
                            .collect::<Result<_, _>>()?,
                    )
                }
                MonomorphizedExpressionKind::Initialize(pattern, value) => {
                    ExpressionKind::Initialize(
                        self.finalize_pattern(pattern, generic, file)?,
                        Box::new(self.finalize_internal(*value, generic, file)?),
                    )
                }
                MonomorphizedExpressionKind::Structure(fields) => ExpressionKind::Structure(
                    fields
                        .into_iter()
                        .map(|expr| self.finalize_internal(expr, generic, file))
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedExpressionKind::Variant(index, values) => ExpressionKind::Variant(
                    index,
                    values
                        .into_iter()
                        .map(|expr| self.finalize_internal(expr, generic, file))
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedExpressionKind::Return(value) => {
                    ExpressionKind::Return(Box::new(self.finalize_internal(*value, generic, file)?))
                }
                MonomorphizedExpressionKind::Loop(body) => {
                    ExpressionKind::Loop(Box::new(self.finalize_internal(*body, generic, file)?))
                }
                MonomorphizedExpressionKind::Break(value) => {
                    ExpressionKind::Break(Box::new(self.finalize_internal(*value, generic, file)?))
                }
                MonomorphizedExpressionKind::Continue => ExpressionKind::Continue,
                MonomorphizedExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                    exprs
                        .into_iter()
                        .map(|expr| self.finalize_internal(expr, generic, file))
                        .collect::<Result<_, _>>()?,
                ),
            },
        })
    }

    fn finalize_pattern(
        &mut self,
        pattern: MonomorphizedPattern,
        generic: bool,
        file: &Rc<RefCell<lower::File>>,
    ) -> Result<Pattern, Error> {
        Ok(Pattern {
            span: pattern.span,
            kind: match pattern.kind {
                MonomorphizedPatternKind::Wildcard => PatternKind::Wildcard,
                MonomorphizedPatternKind::Number(number) => PatternKind::Number(number),
                MonomorphizedPatternKind::Text(text) => PatternKind::Text(text),
                MonomorphizedPatternKind::Variable(var) => PatternKind::Variable(var),
                MonomorphizedPatternKind::Destructure(fields) => PatternKind::Destructure(
                    fields
                        .into_iter()
                        .map(|(index, field)| {
                            Ok((index, self.finalize_pattern(field, generic, file)?))
                        })
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedPatternKind::Variant(index, values) => PatternKind::Variant(
                    index,
                    values
                        .into_iter()
                        .map(|value| self.finalize_pattern(value, generic, file))
                        .collect::<Result<_, _>>()?,
                ),
                MonomorphizedPatternKind::Or(lhs, rhs) => PatternKind::Or(
                    Box::new(self.finalize_pattern(*lhs, generic, file)?),
                    Box::new(self.finalize_pattern(*rhs, generic, file)?),
                ),
                MonomorphizedPatternKind::Where(pattern, condition) => PatternKind::Where(
                    Box::new(self.finalize_pattern(*pattern, generic, file)?),
                    Box::new(self.finalize_internal(*condition, generic, file)?),
                ),
                MonomorphizedPatternKind::Tuple(patterns) => PatternKind::Tuple(
                    patterns
                        .into_iter()
                        .map(|pattern| self.finalize_pattern(pattern, generic, file))
                        .collect::<Result<_, _>>()?,
                ),
            },
        })
    }

    fn instance_for(
        &mut self,
        tr: TraitId,
        mut ty: UnresolvedType,
    ) -> Result<GenericConstantId, TypeError> {
        ty.apply(&self.ctx);

        let tr_decl = self.traits.get(&tr).unwrap().clone();
        let mut temp_ctx = self.ctx.clone();
        temp_ctx.unify(ty.clone(), tr_decl.ty.clone())?;

        macro_rules! find_instance {
            ($instances:expr, $unify:ident, $transform:expr,) => {{
                let instances = $instances;

                let mut candidates = Vec::new();

                for id in instances {
                    let instance = self.generic_constants.get(&id).unwrap().clone();
                    let instance_ty = $transform(instance.generic_ty);

                    let mut ctx = self.ctx.clone();
                    if ctx.$unify(ty.clone(), instance_ty).is_ok() {
                        candidates.push((ctx, id));
                    }
                }

                candidates.dedup_by_key(|(_, id)| *id);

                match candidates.len() {
                    0 => {}
                    1 => {
                        let (ctx, id) = candidates.pop().unwrap();
                        self.ctx = ctx;

                        return Ok(id);
                    }
                    _ => {
                        return Err(TypeError::AmbiguousTrait(
                            tr,
                            candidates.into_iter().map(|(_, id)| id).collect(),
                        ));
                    }
                }
            }};
        }

        find_instance!(
            self.bound_instances.get(&tr).cloned().unwrap_or_default(),
            unify_generic,
            |ty| ty,
        );

        find_instance!(
            self.declared_instances
                .get(&tr)
                .cloned()
                .unwrap_or_default(),
            unify,
            |mut ty| {
                self.add_substitutions(&mut ty, &mut BTreeMap::new());
                ty
            },
        );

        let params = self.ctx.unify_params(ty, tr_decl.ty.clone()).0;

        let params = tr_decl
            .params
            .into_iter()
            .map(|param| {
                params
                    .get(&param)
                    .cloned()
                    .unwrap_or_else(|| UnresolvedType::Variable(self.ctx.new_variable()))
            })
            .collect();

        Err(TypeError::MissingInstance(tr, params))
    }

    fn convert_type_annotation(
        &mut self,
        annotation: &lower::TypeAnnotation,
        file: &Rc<RefCell<lower::File>>,
    ) -> UnresolvedType {
        match &annotation.kind {
            lower::TypeAnnotationKind::Error => UnresolvedType::Bottom(BottomTypeReason::Error),
            lower::TypeAnnotationKind::Placeholder => {
                UnresolvedType::Variable(self.ctx.new_variable())
            }
            lower::TypeAnnotationKind::Named(id, params) => {
                let file_ = file.borrow();
                let ty = file_.declarations.types.get(id).unwrap();

                let params = params
                    .iter()
                    .map(|param| self.convert_type_annotation(param, file))
                    .collect::<Vec<_>>();

                let substitutions = ty
                    .value
                    .params
                    .iter()
                    .copied()
                    .zip(params.iter().cloned())
                    .collect::<BTreeMap<_, _>>();

                let mut convert_and_instantiate = |ty| {
                    let mut ty = self.convert_type_annotation(ty, file);
                    ty.instantiate_with(&substitutions);
                    ty
                };

                let structure = match &ty.value.kind {
                    lower::TypeKind::Marker => TypeStructure::Marker,
                    lower::TypeKind::Structure(fields, _) => TypeStructure::Structure(
                        fields
                            .iter()
                            .map(|field| convert_and_instantiate(&field.ty))
                            .collect(),
                    ),
                    lower::TypeKind::Enumeration(variants, _) => TypeStructure::Enumeration(
                        variants
                            .iter()
                            .map(|variant| {
                                variant
                                    .tys
                                    .iter()
                                    .map(&mut convert_and_instantiate)
                                    .collect()
                            })
                            .collect(),
                    ),
                };

                UnresolvedType::Named(*id, params, structure)
            }
            lower::TypeAnnotationKind::Parameter(id) => UnresolvedType::Parameter(*id),
            lower::TypeAnnotationKind::Builtin(id, parameters) => {
                let builtin_ty = file
                    .borrow()
                    .declarations
                    .builtin_types
                    .get(id)
                    .unwrap_or_else(|| {
                        panic!(
                            "builtin type {:?} not found in {:#?}",
                            id,
                            file.borrow().declarations.builtin_types
                        )
                    })
                    .clone();

                match builtin_ty.value.kind {
                    lower::BuiltinTypeKind::Never => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`!` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        UnresolvedType::Bottom(BottomTypeReason::Annotated)
                    }
                    lower::BuiltinTypeKind::Number => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Number` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        UnresolvedType::Builtin(BuiltinType::Number)
                    }
                    lower::BuiltinTypeKind::Text => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Text` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        UnresolvedType::Builtin(BuiltinType::Text)
                    }
                    lower::BuiltinTypeKind::Boolean => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Boolean` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        UnresolvedType::Builtin(BuiltinType::Number)
                    }
                    lower::BuiltinTypeKind::List => {
                        if parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`List` accepts 1 parameter, but none were provided",
                                vec![Note::primary(
                                    annotation.span,
                                    "try adding `_` here to infer the type of `Element`",
                                )],
                            ));

                            UnresolvedType::Builtin(BuiltinType::List(Box::new(
                                UnresolvedType::Bottom(BottomTypeReason::Error),
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

                            UnresolvedType::Builtin(BuiltinType::List(Box::new(
                                self.convert_type_annotation(parameters.first().unwrap(), file),
                            )))
                        }
                    }
                    lower::BuiltinTypeKind::Mutable => {
                        if parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Mutable` accepts 1 parameter, but none were provided",
                                vec![Note::primary(
                                    annotation.span,
                                    "try adding `_` here to infer the type of `Value`",
                                )],
                            ));

                            UnresolvedType::Builtin(BuiltinType::Mutable(Box::new(
                                UnresolvedType::Bottom(BottomTypeReason::Error),
                            )))
                        } else {
                            if parameters.len() > 1 {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!(
                                        "`Mutable` accepts 1 parameter, but {} were provided",
                                        parameters.len()
                                    ),
                                    vec![Note::primary(
                                        annotation.span,
                                        "try removing some of these",
                                    )],
                                ));
                            }

                            UnresolvedType::Builtin(BuiltinType::Mutable(Box::new(
                                self.convert_type_annotation(parameters.first().unwrap(), file),
                            )))
                        }
                    }
                }
            }
            lower::TypeAnnotationKind::Function(input, output) => UnresolvedType::Function(
                Box::new(self.convert_type_annotation(input, file)),
                Box::new(self.convert_type_annotation(output, file)),
            ),
            lower::TypeAnnotationKind::Tuple(tys) => UnresolvedType::Tuple(
                tys.iter()
                    .map(|ty| self.convert_type_annotation(ty, file))
                    .collect(),
            ),
        }
    }

    fn format_type(&self, ty: impl Into<FormattableType>, surround_in_backticks: bool) -> String {
        macro_rules! getter {
            ($x:ident) => {
                |id| {
                    self.declarations
                        .$x
                        .get(&id)
                        .unwrap()
                        .name
                        .map_or("<unknown>", |name| name.as_str())
                        .to_string()
                }
            };
        }

        let type_names = getter!(types);
        let trait_names = getter!(traits);
        let param_names = getter!(type_parameters);

        format_type(
            ty,
            type_names,
            trait_names,
            param_names,
            surround_in_backticks,
        )
    }
}

#[derive(Debug, Clone)]
enum MatchSet {
    Never,
    Marker(bool),
    Structure(Vec<(bool, MatchSet)>),
    Enumeration(Vec<(bool, Vec<MatchSet>)>),
    Tuple(Vec<MatchSet>),
}

impl<L: Loader> Typechecker<L> {
    fn match_set_from(&mut self, ty: &UnresolvedType) -> Result<MatchSet, TypeError> {
        match &ty {
            UnresolvedType::Named(id, _, _) => {
                let kind = self.types.get(id).unwrap().kind.clone();

                match kind {
                    TypeDefinitionKind::Marker => Ok(MatchSet::Marker(false)),
                    TypeDefinitionKind::Structure(fields, _) => Ok(MatchSet::Structure(
                        fields
                            .iter()
                            .map(|ty| Ok((false, self.match_set_from(ty)?)))
                            .collect::<Result<_, _>>()?,
                    )),
                    TypeDefinitionKind::Enumeration(variants, _) => Ok(MatchSet::Enumeration(
                        variants
                            .iter()
                            .map(|tys| {
                                Ok((
                                    false,
                                    tys.iter()
                                        .map(|ty| self.match_set_from(ty))
                                        .collect::<Result<_, _>>()?,
                                ))
                            })
                            .collect::<Result<_, _>>()?,
                    )),
                }
            }
            UnresolvedType::Tuple(tys) => Ok(MatchSet::Tuple(
                tys.iter()
                    .map(|ty| self.match_set_from(ty))
                    .collect::<Result<_, _>>()?,
            )),
            UnresolvedType::Bottom(_) => Ok(MatchSet::Never),
            _ => Ok(MatchSet::Marker(false)),
        }
    }

    fn assert_matched(
        &mut self,
        match_set: &MatchSet,
        span: Span,
        for_exhaustive_pattern: bool,
    ) -> Result<(), Error> {
        if match_set.is_matched() {
            Ok(())
        } else {
            if for_exhaustive_pattern {
                self.compiler.diagnostics.add(Diagnostic::error(
                    "pattern is not exhaustive",
                    vec![Note::primary(
                        span,
                        "this pattern does not handle all possible values",
                    )],
                ));
            } else {
                self.compiler.diagnostics.add(Diagnostic::error(
                    "`when` expression is not exhaustive",
                    vec![Note::primary(
                        span,
                        "try adding some more patterns to cover all possible values",
                    )],
                ));
            }

            Err(Error::new(TypeError::ErrorExpression, span))
        }
    }
}

impl MatchSet {
    fn is_matched(&self) -> bool {
        match self {
            MatchSet::Never => true,
            MatchSet::Marker(matches) => *matches,
            MatchSet::Structure(fields) => fields
                .iter()
                .all(|(matches, field)| *matches && field.is_matched()),
            MatchSet::Enumeration(variants) => variants.iter().all(|(matches, variant)| {
                *matches && variant.iter().all(|variant| variant.is_matched())
            }),
            MatchSet::Tuple(sets) => sets.iter().all(|set| set.is_matched()),
        }
    }

    fn set_matched(&mut self, is_matched: bool) {
        match self {
            MatchSet::Marker(matches) => *matches = is_matched,
            MatchSet::Structure(fields) => {
                for (matches, field) in fields {
                    *matches = is_matched;
                    field.set_matched(is_matched);
                }
            }
            MatchSet::Enumeration(variants) => {
                for (matches, variant) in variants {
                    *matches = is_matched;
                    for match_set in variant {
                        match_set.set_matched(is_matched);
                    }
                }
            }
            MatchSet::Tuple(sets) => {
                for set in sets {
                    set.set_matched(is_matched);
                }
            }
            _ => {}
        }
    }
}
