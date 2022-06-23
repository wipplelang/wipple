#![allow(clippy::type_complexity)]

mod engine;
mod format;
mod traverse;

pub use engine::{BuiltinType, Type};
pub use format::format_type;

use crate::{
    compile::lower, diagnostics::*, helpers::InternedString, parse::Span, Compiler, FilePath,
    GenericConstantId, MonomorphizedConstantId, TraitId, TypeId, TypeParameterId, VariableId,
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
                Function([<$prefix Pattern>], Box<[<$prefix Expression>]>),
                When(Box<[<$prefix Expression>]>, Vec<[<$prefix Arm>]>),
                External(InternedString, InternedString, Vec<[<$prefix Expression>]>),
                Initialize([<$prefix Pattern>], Box<[<$prefix Expression>]>),
                Structure(Vec<[<$prefix Expression>]>),
                Variant(usize, Vec<[<$prefix Expression>]>),
                ListLiteral(Vec<[<$prefix Expression>]>),
                Return(Box<[<$prefix Expression>]>),
                Loop(Box<[<$prefix Expression>]>),
                Break(Box<[<$prefix Expression>]>),
                Continue,
                $($kinds)*
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
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
            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis struct [<$prefix Pattern>] {
                $vis span: Span,
                $vis kind: [<$prefix PatternKind>],
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis enum [<$prefix PatternKind>] {
                Wildcard,
                Number(Decimal),
                Text(InternedString),
                Variable(VariableId),
                Or(Box<[<$prefix Pattern>]>, Box<[<$prefix Pattern>]>),
                Where(Box<[<$prefix Pattern>]>, Box<[<$prefix Expression>]>),
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
    Unit,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Declarations<Expr, Ty, File = ()> {
    pub types: BTreeMap<TypeId, Declaration<TypeDeclaration>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<()>>,
    pub traits: BTreeMap<TraitId, Declaration<TraitDeclaration>>,
    pub generic_constants: BTreeMap<GenericConstantId, GenericConstantDeclaration<Expr, File>>,
    pub monomorphized_constants: BTreeMap<MonomorphizedConstantId, (File, Declaration<Expr>)>,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Declaration<T> {
    pub name: Option<InternedString>,
    pub span: Span,
    pub value: T,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeDeclaration {
    pub attributes: lower::DeclarationAttributes,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitDeclaration {
    pub ty: UnresolvedType,
    pub params: Vec<TypeParameterId>,
    pub attributes: lower::DeclarationAttributes,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericConstantDeclaration<Expr, File> {
    pub file: File,
    pub decl: Declaration<Expr>,
    pub attributes: Option<lower::DeclarationAttributes>,
}

pub enum Progress {
    Typechecking {
        path: FilePath,
        current: usize,
        total: usize,
    },
    Finalizing,
}

impl<L> Compiler<L> {
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
            return_tys: Default::default(),
            loop_tys: Default::default(),
            traits: Default::default(),
            types: Default::default(),
            generic_constants: Default::default(),
            monomorphized_constants: Default::default(),
            declared_instances: Default::default(),
            bound_instances: Default::default(),
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
                            let fields = field_names
                                .iter()
                                .map(|(name, index)| {
                                    (
                                        *name,
                                        (
                                            *index,
                                            typechecker
                                                .convert_type_annotation(&fields[*index].ty, file),
                                        ),
                                    )
                                })
                                .collect();

                            typechecker.types.insert(
                                id,
                                TypeDefinition {
                                    kind: TypeDefinitionKind::Structure(fields),
                                    params,
                                },
                            );
                        }
                        lower::TypeKind::Enumeration(variants, _) => {
                            let variant_tys = variants
                                .iter()
                                .map(|variant| {
                                    (
                                        variant.constructor,
                                        variant
                                            .tys
                                            .iter()
                                            .map(|ty| typechecker.convert_type_annotation(ty, file))
                                            .collect(),
                                    )
                                })
                                .collect();

                            typechecker.types.insert(
                                id,
                                TypeDefinition {
                                    kind: TypeDefinitionKind::Enumeration(variant_tys),
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

                let substitutions = tr
                    .params
                    .into_iter()
                    .zip(&decl.value.trait_params)
                    .map(|(param, ty)| (param, typechecker.convert_type_annotation(ty, file)))
                    .collect();

                tr.ty.instantiate_with(&substitutions);

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
                typechecker.report_type_error(error, body.span);
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
                    typechecker.report_type_error(error, body_span);

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
                        let span = expr.span;

                        let expr = typechecker
                            .monomorphize(expr, file, None)
                            .map_err(|error| (error, span))?;

                        Ok((file.clone(), expr))
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

            for (id, (span, generic_id, constant)) in typechecker.monomorphized_constants.clone() {
                if already_checked.contains(&id) {
                    continue;
                }

                already_checked.insert(id);

                let mut ty = constant.decl.value.ty.clone();
                ty.apply(&typechecker.ctx);

                for (tr, mut ty, span) in constant.bounds.clone() {
                    ty.apply(&typechecker.ctx);

                    let instance_id = match typechecker.instance_for(tr, ty.clone()) {
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

                let body = match typechecker.monomorphize(
                    constant.decl.value.clone(),
                    &constant.file,
                    Some((generic_id, id)),
                ) {
                    Ok(value) => value,
                    Err(error) => {
                        typechecker.report_type_error(error, span);
                        continue;
                    }
                };

                typechecker.declarations.monomorphized_constants.insert(
                    id,
                    (
                        constant.file,
                        Declaration {
                            name: constant.decl.name,
                            span,
                            value: body,
                        },
                    ),
                );
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
                    .map(|(id, constant)| {
                        Ok((
                            id,
                            GenericConstantDeclaration {
                                file: (),
                                decl: Declaration {
                                    name: constant.decl.name,
                                    span: constant.decl.span,
                                    value: typechecker
                                        .finalize_generic(
                                            constant.decl.value.clone(),
                                            &constant.file,
                                        )
                                        .map_err(|error| (error, constant.decl.span))?,
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
                    .map(|(id, (file, decl))| {
                        Ok((
                            id,
                            (
                                (),
                                Declaration {
                                    name: decl.name,
                                    span: decl.span,
                                    value: typechecker
                                        .finalize(decl.value, &file)
                                        .map_err(|error| (error, decl.span))?,
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
            .map(|(file, expr)| {
                let span = expr.span;

                typechecker
                    .finalize(expr, &file)
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
    // bounds: Vec<Bound>, // TODO
}

#[derive(Debug, Clone)]
struct TypeDefinition {
    kind: TypeDefinitionKind,
    params: Vec<TypeParameterId>,
}

#[derive(Debug, Clone)]
enum TypeDefinitionKind {
    Marker,
    Structure(HashMap<InternedString, (usize, UnresolvedType)>),
    Enumeration(Vec<(GenericConstantId, Vec<UnresolvedType>)>),
}

struct Typechecker<'a, L> {
    well_typed: bool,
    ctx: Context,
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
    compiler: &'a mut Compiler<L>,
}

impl<'a, L> Typechecker<'a, L> {
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
                let marker = self.types.get(id).unwrap().clone();

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
            lower::ExpressionKind::Block(statements, declarations) => {
                let (ty, statements) = self.typecheck_block(statements, file, suppress_errors);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Block(statements, declarations.clone()),
                }
            }
            lower::ExpressionKind::Call(function, input) => {
                let function = self.typecheck_expr(function, file, suppress_errors);
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
                        self.report_type_error(errors, expr.span);
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
                                    self.report_type_error(error, arm.body.span);
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
                let ty = self.convert_type_annotation(ty, file);
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
            lower::ExpressionKind::Initialize(pattern, value) => {
                let value = self.typecheck_expr(value, file, suppress_errors);

                let pattern =
                    self.typecheck_pattern(pattern, value.ty.clone(), file, suppress_errors);

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Builtin(BuiltinType::Unit),
                    kind: UnresolvedExpressionKind::Initialize(pattern, Box::new(value)),
                }
            }
            lower::ExpressionKind::Instantiate(id, fields) => {
                let structure = self.types.get(id).unwrap().clone();

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
            lower::ExpressionKind::ListLiteral(items) => {
                let items = items
                    .iter()
                    .map(|expr| self.typecheck_expr(expr, file, suppress_errors))
                    .collect::<Vec<_>>();

                let mut item_tys = items.iter().map(|item| (item.span, item.ty.clone()));

                let ty = item_tys
                    .next()
                    .map(|(_, ty)| ty)
                    .unwrap_or_else(|| UnresolvedType::Variable(self.ctx.new_variable()));

                for (span, item_ty) in item_tys {
                    if let Err(error) = self.ctx.unify(item_ty, ty.clone()) {
                        if !suppress_errors {
                            self.report_type_error(error, span);
                        }
                    };
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty: UnresolvedType::Builtin(BuiltinType::List(Box::new(ty))),
                    kind: UnresolvedExpressionKind::ListLiteral(items),
                }
            }
            lower::ExpressionKind::Variant(id, index, values) => {
                let enumeration = self.types.get(id).unwrap().clone();

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
            lower::ExpressionKind::Return(value) => {
                let value = self.typecheck_expr(value, file, suppress_errors);

                let return_ty = match self.return_tys.last_mut() {
                    Some(ty) => ty,
                    None => return UnresolvedExpression::error(expr.span),
                };

                if let Some(return_ty) = return_ty {
                    if let Err(errors) = self.ctx.unify(return_ty.clone(), value.ty.clone()) {
                        if !suppress_errors {
                            self.report_type_error(errors, expr.span);
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
                    .unify(body.ty.clone(), UnresolvedType::Builtin(BuiltinType::Unit))
                {
                    if !suppress_errors {
                        self.report_type_error(errors, expr.span);
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
                            self.report_type_error(errors, expr.span);
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
        fn typecheck_pattern<L>(
            tc: &mut Typechecker<L>,
            pattern: &lower::Pattern,
            ty: Option<UnresolvedType>,
            file: &Rc<RefCell<lower::File>>,
            suppress_errors: bool,
        ) -> UnresolvedPattern {
            let kind = match &pattern.kind {
                lower::PatternKind::Error => UnresolvedPatternKind::Error,
                lower::PatternKind::Wildcard => UnresolvedPatternKind::Wildcard,
                lower::PatternKind::Unit => UnresolvedPatternKind::Unit,
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
        file: &Rc<RefCell<lower::File>>,
        inside_generic_constant: Option<(GenericConstantId, MonomorphizedConstantId)>,
    ) -> Result<MonomorphizedArm, TypeError> {
        Ok(MonomorphizedArm {
            span: arm.span,
            pattern: self
                .monomorphize_pattern(arm.pattern, ty, file, inside_generic_constant)
                .ok_or(TypeError::ErrorExpression)?,
            body: self.monomorphize(arm.body, file, inside_generic_constant)?,
        })
    }

    fn monomorphize_pattern(
        &mut self,
        pattern: UnresolvedPattern,
        mut ty: UnresolvedType,
        file: &Rc<RefCell<lower::File>>,
        inside_generic_constant: Option<(GenericConstantId, MonomorphizedConstantId)>,
    ) -> Option<MonomorphizedPattern> {
        ty.apply(&self.ctx);

        let kind = (|| match pattern.kind {
            UnresolvedPatternKind::Error => None,
            UnresolvedPatternKind::Unit => {
                if let Err(error) = self
                    .ctx
                    .unify(ty, UnresolvedType::Builtin(BuiltinType::Unit))
                {
                    self.report_type_error(error, pattern.span);
                }

                Some(MonomorphizedPatternKind::Wildcard)
            }
            UnresolvedPatternKind::Number(number) => {
                if let Err(error) = self
                    .ctx
                    .unify(ty, UnresolvedType::Builtin(BuiltinType::Number))
                {
                    self.report_type_error(error, pattern.span);
                }

                Some(MonomorphizedPatternKind::Number(number))
            }
            UnresolvedPatternKind::Text(text) => {
                if let Err(error) = self
                    .ctx
                    .unify(ty, UnresolvedType::Builtin(BuiltinType::Text))
                {
                    self.report_type_error(error, pattern.span);
                }

                Some(MonomorphizedPatternKind::Text(text))
            }
            UnresolvedPatternKind::Wildcard => Some(MonomorphizedPatternKind::Wildcard),
            UnresolvedPatternKind::Variable(var) => {
                let var_ty = self
                    .variables
                    .get(&var)
                    .expect("uninitialized variable")
                    .clone();

                let ctx = self.ctx.clone();
                if let Err(error) = self.ctx.unify(var_ty.clone(), ty.clone()).or_else(|_| {
                    // HACK: Try the other way around if unification doesn't
                    // work the first time
                    // FIXME: Remove if this causes unsoundness
                    self.ctx = ctx;
                    self.ctx.unify(ty.clone(), var_ty)
                }) {
                    self.report_type_error(error, pattern.span);
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
                let (id, params) = match ty {
                    UnresolvedType::Named(id, params) => (id, params),
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

                let structure_fields = match &structure.kind {
                    TypeDefinitionKind::Structure(fields) => fields.clone(),
                    _ => unreachable!(), // or do we need to display an error like above?
                };

                let substitutions = structure
                    .params
                    .iter()
                    .copied()
                    .zip(params)
                    .collect::<BTreeMap<_, _>>();

                let fields = fields
                    .into_iter()
                    .filter_map(|(name, pattern)| {
                        let (index, mut member_ty) = match structure_fields.get(&name) {
                            Some((index, ty)) => (*index, ty.clone()),
                            None => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("value has no member named '{}'", name),
                                    vec![Note::primary(pattern.span, "no such member")],
                                ));

                                return None;
                            }
                        };

                        member_ty.instantiate_with(&substitutions);

                        let pattern = self.monomorphize_pattern(
                            pattern,
                            member_ty,
                            file,
                            inside_generic_constant,
                        )?;

                        Some((index, pattern))
                    })
                    .collect();

                Some(MonomorphizedPatternKind::Destructure(fields))
            }
            UnresolvedPatternKind::Variant(variant_ty, variant, values) => {
                let (id, params) = match &ty {
                    UnresolvedType::Named(id, params) => (id, params),
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

                let (_, mut variant_tys) = match &enumeration.kind {
                    TypeDefinitionKind::Enumeration(variants) => variants[variant].clone(),
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
                    ty,
                    UnresolvedType::Named(
                        variant_ty,
                        enumeration
                            .params
                            .iter()
                            .map(|param| substitutions.get(param).unwrap().clone())
                            .collect(),
                    ),
                ) {
                    self.report_type_error(error, pattern.span);
                }

                let pattern = MonomorphizedPatternKind::Variant(
                    variant,
                    values
                        .into_iter()
                        .zip(variant_tys)
                        .map(|(pattern, variant_ty)| {
                            self.monomorphize_pattern(
                                pattern,
                                variant_ty,
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
                    self.report_type_error(error, pattern.span);
                }

                Some(
                    self.monomorphize_pattern(*inner, ty, file, inside_generic_constant)?
                        .kind,
                )
            }
            UnresolvedPatternKind::Or(lhs, rhs) => Some(MonomorphizedPatternKind::Or(
                Box::new(self.monomorphize_pattern(
                    *lhs,
                    ty.clone(),
                    file,
                    inside_generic_constant,
                )?),
                Box::new(self.monomorphize_pattern(*rhs, ty, file, inside_generic_constant)?),
            )),
            UnresolvedPatternKind::Where(pattern, condition) => {
                let pattern =
                    self.monomorphize_pattern(*pattern, ty, file, inside_generic_constant)?;

                let condition_span = condition.span;
                let condition = match self.monomorphize(*condition, file, inside_generic_constant) {
                    Ok(expr) => expr,
                    Err(error) => {
                        self.report_type_error(error, condition_span);
                        return None;
                    }
                };

                if let Some(boolean_ty) = file.borrow().global_attributes.language_items.boolean {
                    if let Err(error) = self.ctx.unify(
                        condition.ty.clone(),
                        UnresolvedType::Named(boolean_ty, Vec::new()),
                    ) {
                        self.report_type_error(error, condition.span);
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

                Some(MonomorphizedPatternKind::Where(
                    Box::new(pattern),
                    Box::new(condition),
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
    ) -> Result<MonomorphizedExpression, TypeError> {
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
                            self.report_type_error(error, expr.span);
                        }

                        MonomorphizedExpressionKind::Constant(monomorphized_id)
                    }
                    UnresolvedExpressionKind::Variable(var) => {
                        let ty = self
                            .variables
                            .get(&var)
                            .expect("uninitialized variable")
                            .clone();

                        if let Err(error) = self.ctx.unify(expr.ty, ty) {
                            self.report_type_error(error, expr.span);
                        }

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
                                .map(|expr| self.monomorphize(expr, file, inside_generic_constant))
                                .collect::<Result<_, _>>()?,
                            scope,
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
                        let input_ty = match expr.ty {
                            UnresolvedType::Function(input, _) => *input,
                            _ => unreachable!(),
                        };

                        let pattern = self
                            .monomorphize_pattern(pattern, input_ty, file, inside_generic_constant)
                            .ok_or(TypeError::ErrorExpression)?;

                        MonomorphizedExpressionKind::Function(
                            pattern,
                            Box::new(self.monomorphize(*body, file, inside_generic_constant)?),
                        )
                    }
                    UnresolvedExpressionKind::When(input, arms) => {
                        let input = self.monomorphize(*input, file, inside_generic_constant)?;

                        let arms = arms
                            .into_iter()
                            .map(|arm| {
                                self.monomorphize_arm(
                                    arm,
                                    input.ty.clone(),
                                    file,
                                    inside_generic_constant,
                                )
                            })
                            .collect::<Result<_, _>>()?;

                        MonomorphizedExpressionKind::When(Box::new(input), arms)
                    }
                    UnresolvedExpressionKind::External(namespace, identifier, inputs) => {
                        MonomorphizedExpressionKind::External(
                            namespace,
                            identifier,
                            inputs
                                .into_iter()
                                .map(|expr| self.monomorphize(expr, file, inside_generic_constant))
                                .collect::<Result<_, _>>()?,
                        )
                    }
                    UnresolvedExpressionKind::Initialize(pattern, value) => {
                        // Resolve the right-hand side first
                        let value = self.monomorphize(*value, file, inside_generic_constant)?;

                        let pattern = self
                            .monomorphize_pattern(
                                pattern,
                                value.ty.clone(),
                                file,
                                inside_generic_constant,
                            )
                            .ok_or(TypeError::ErrorExpression)?;

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
                        let instance = self.instance_for(tr, expr.ty.clone())?;

                        let (monomorphized_instance, ty) =
                            self.monomorphize_constant(instance, expr.span);

                        if let Err(error) = self.ctx.unify(ty, expr.ty.clone()) {
                            self.report_type_error(error, expr.span);
                        }

                        MonomorphizedExpressionKind::Constant(monomorphized_instance)
                    }
                    UnresolvedExpressionKind::ListLiteral(items) => {
                        MonomorphizedExpressionKind::ListLiteral(
                            items
                                .into_iter()
                                .map(|expr| self.monomorphize(expr, file, inside_generic_constant))
                                .collect::<Result<_, _>>()?,
                        )
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
    ) -> Result<Expression, TypeError> {
        self.finalize_internal(expr, false, file)
    }

    fn finalize_generic(
        &mut self,
        expr: MonomorphizedExpression,
        file: &Rc<RefCell<lower::File>>,
    ) -> Result<Expression, TypeError> {
        self.finalize_internal(expr, true, file)
    }

    fn finalize_internal(
        &mut self,
        expr: MonomorphizedExpression,
        generic: bool,
        file: &Rc<RefCell<lower::File>>,
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
                        .map(|expr| self.finalize_internal(expr, generic, file))
                        .collect::<Result<_, _>>()?,
                    scope,
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
                MonomorphizedExpressionKind::External(namespace, identifier, inputs) => {
                    ExpressionKind::External(
                        namespace,
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
                MonomorphizedExpressionKind::ListLiteral(items) => ExpressionKind::ListLiteral(
                    items
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
            },
        })
    }

    fn finalize_pattern(
        &mut self,
        pattern: MonomorphizedPattern,
        generic: bool,
        file: &Rc<RefCell<lower::File>>,
    ) -> Result<Pattern, TypeError> {
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
            },
        })
    }

    fn instance_for(
        &mut self,
        tr: TraitId,
        mut ty: UnresolvedType,
    ) -> Result<GenericConstantId, TypeError> {
        ty.apply(&self.ctx);

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
                        eprintln!("AMBIGUITY finding instance of {:?} for {:#?}", tr, ty);

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

        Err(TypeError::MissingInstance(tr, ty))
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
            lower::TypeAnnotationKind::Named(id, params) => UnresolvedType::Named(
                *id,
                params
                    .iter()
                    .map(|param| self.convert_type_annotation(param, file))
                    .collect(),
            ),
            lower::TypeAnnotationKind::Parameter(id) => UnresolvedType::Parameter(*id),
            lower::TypeAnnotationKind::Builtin(id, parameters) => {
                let builtin_ty = file
                    .borrow()
                    .declarations
                    .builtin_types
                    .get(id)
                    .unwrap()
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
                    lower::BuiltinTypeKind::Unit => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`()` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        UnresolvedType::Builtin(BuiltinType::Unit)
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

                            UnresolvedType::Builtin(BuiltinType::List(Box::new(
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
        }
    }

    fn report_type_error(&mut self, error: TypeError, span: Span) {
        self.well_typed = false;

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
                    .chain(candidates.into_iter().map(|id| {
                        let instance = self.generic_constants.get(&id).unwrap();
                        Note::secondary(
                            instance.decl.span,
                            format!("this instance ({:?}) could apply", id),
                        )
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
