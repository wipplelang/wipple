#![allow(clippy::type_complexity)]

mod engine;
mod format;
mod traverse;

pub use engine::{BuiltinType, Type, TypeKind};
pub use format::format_type;

use crate::{
    compile::lower, diagnostics::*, helpers::InternedString, parser::Span, Compiler, ConstantId,
    FilePath, GenericConstantId, Loader, TraitId, TypeId, TypeParameterId, VariableId,
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
    pub declarations: Declarations,
    pub top_level: HashMap<InternedString, lower::ScopeValue>,
}

macro_rules! expr {
    ($vis:vis, $prefix:literal, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis struct [<$prefix Expression>] {
                $vis ty: [<$prefix Type>],
                $vis kind: [<$prefix ExpressionKind>],
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            $vis enum [<$prefix ExpressionKind>] {
                Error,
                Marker,
                Constant(ConstantId),
                Variable(VariableId),
                Text(InternedString),
                Number(Decimal),
                Block(
                    Vec<[<$prefix Expression>]>,
                    HashMap<InternedString, lower::ScopeValue>,
                ),
                Call(Box<[<$prefix Expression>]>, Box<[<$prefix Expression>]>),
                Member(Box<[<$prefix Expression>]>, usize),
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

expr!(, "Unresolved", {
    Trait(TraitId),
});

expr!(pub, "", {});

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Declarations {
    pub types: BTreeMap<TypeId, Declaration<()>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<()>>,
    pub traits: BTreeMap<TraitId, Declaration<(UnresolvedType, Vec<TypeParameterId>)>>,
    pub generic_constants: Vec<Declaration<Expression>>,
    pub monomorphized_constants: BTreeMap<ConstantId, Declaration<Expression>>,
    pub variables: BTreeMap<VariableId, Declaration<Type>>,
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
            compiler: self,
        };

        let total_files = files.len();

        // Copy declarations

        let mut declarations = Declarations::default();
        for file in &files {
            for (id, decl) in file.borrow().declarations.types.clone() {
                if let lower::Declaration::Local(decl) = decl {
                    declarations.types.insert(
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
                    declarations.type_parameters.insert(
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

                    declarations.traits.insert(
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
                    let params = decl
                        .value
                        .parameters
                        .iter()
                        .map(|param| TypeParameter {
                            span: param.span,
                            id: param.id,
                        })
                        .collect();
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

                            (bound.tr, trait_ty)
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
            let (_, block) = typechecker.typecheck_block(file.borrow().span, &block, file, false);
            body.push((file.clone(), block));
        }

        let prev_bound_instances = typechecker.bound_instances.clone();

        for constant in typechecker.generic_constants.clone().into_values() {
            let mut body = typechecker.typecheck_expr(&constant.decl.value, &constant.file, false);

            if let Err(error) = typechecker.ctx.unify(body.ty.clone(), constant.generic_ty) {
                typechecker.report_type_error(error, &constant.file.borrow());
            }

            body.traverse_mut(|expr| expr.ty.apply(&typechecker.ctx));

            // Register dummy generic instances so finalization works -- these
            // constants will never actually be used
            for (tr, ty) in constant.bounds {
                let dummy_id = typechecker.compiler.new_constant_id();

                typechecker.monomorphized_constants.insert(
                    dummy_id,
                    Constant {
                        file: constant.file.clone(),
                        decl: Declaration {
                            name: constant.decl.name,
                            span: constant.decl.span,
                            value: UnresolvedExpression {
                                ty,
                                kind: UnresolvedExpressionKind::Error,
                            },
                        },
                        generic_ty: (),
                        // TODO: Generic instances (do we even need to add
                        // bounds here?)
                        bounds: Default::default(),
                    },
                );

                typechecker
                    .bound_instances
                    .entry(tr)
                    .or_default()
                    .push(dummy_id);
            }

            let body_span = body.ty.span;
            let body = match typechecker.finalize(body, true, &constant.file) {
                Ok(expr) => expr,
                Err(error) => {
                    typechecker.report_type_error(error, &constant.file.borrow());

                    Expression {
                        ty: Type {
                            span: body_span,
                            kind: TypeKind::Bottom(BottomTypeReason::Error),
                        },
                        kind: ExpressionKind::Error,
                    }
                }
            };

            declarations.generic_constants.push(Declaration {
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
                        let expr = typechecker
                            .finalize(expr, false, file)
                            .map_err(|error| (error, file))?;

                        expr.traverse(|expr| {
                            if let ExpressionKind::Initialize(id, expr) = &expr.kind {
                                let file = file.borrow();
                                let decl = file.declarations.variables.get(id).unwrap();

                                declarations.variables.insert(
                                    *id,
                                    Declaration {
                                        name: decl.name(),
                                        span: decl.span(),
                                        value: expr.ty.clone(),
                                    },
                                );
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
            Err((error, file)) => {
                typechecker.report_type_error(error, &file.borrow());
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

            for (id, constant) in typechecker.monomorphized_constants.clone() {
                if already_checked.contains(&id) {
                    continue;
                }

                already_checked.insert(id);

                for (tr, mut ty) in constant.bounds.clone() {
                    ty.apply(&typechecker.ctx);

                    let instance_id = match typechecker.instance_for(tr, ty) {
                        Ok(id) => id,
                        Err(error) => {
                            typechecker.report_type_error(error, &constant.file.borrow());
                            continue;
                        }
                    };

                    typechecker
                        .bound_instances
                        .entry(tr)
                        .or_default()
                        .push(instance_id);
                }

                let body = match typechecker.finalize(
                    constant.decl.value.clone(),
                    true, // constants monomorphized inside generic constants won't be used anyway
                    &constant.file,
                ) {
                    Ok(value) => value,
                    Err(error) => {
                        typechecker.report_type_error(error, &constant.file.borrow());
                        continue;
                    }
                };

                declarations.monomorphized_constants.insert(
                    id,
                    Declaration {
                        name: constant.decl.name,
                        span: constant.decl.span,
                        value: body,
                    },
                );
            }
        }

        // Build the final program

        Some(Program {
            well_typed: typechecker.well_typed,
            body,
            declarations,
            top_level,
        })
    }
}

type Bound = (TraitId, UnresolvedType);

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
    traits: BTreeMap<TraitId, (UnresolvedType, Vec<TypeParameter>)>,
    structures: BTreeMap<TypeId, HashMap<InternedString, (usize, UnresolvedType)>>,
    // TODO: enumerations
    generic_constants: BTreeMap<GenericConstantId, Constant<lower::Expression, UnresolvedType>>,
    monomorphized_constants: BTreeMap<ConstantId, Constant<UnresolvedExpression, ()>>,
    declared_instances: BTreeMap<TraitId, Vec<GenericConstantId>>,
    bound_instances: BTreeMap<TraitId, Vec<ConstantId>>,
    function_inputs: Vec<(Span, TypeVariable)>,
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
            ty: UnresolvedType {
                span: expr.span,
                kind: UnresolvedTypeKind::Bottom(BottomTypeReason::Error),
            },
            kind: UnresolvedExpressionKind::Error,
        };

        let (ty, kind) = match &expr.kind {
            lower::ExpressionKind::Error => {
                self.well_typed = false; // signal that the program contains errors
                return error_expr();
            }
            lower::ExpressionKind::Unit => (
                UnresolvedType {
                    span: expr.span,
                    kind: UnresolvedTypeKind::Builtin(BuiltinType::Unit),
                },
                UnresolvedExpressionKind::Marker,
            ),
            lower::ExpressionKind::Marker(ty) => (
                self.convert_type_id(expr.span, *ty, &file.borrow()),
                UnresolvedExpressionKind::Marker,
            ),
            lower::ExpressionKind::Constant(id) => {
                let (monomorphized_id, ty) = self.monomorphize_constant(id);
                (ty, UnresolvedExpressionKind::Constant(monomorphized_id))
            }
            lower::ExpressionKind::Trait(id) => (
                UnresolvedType {
                    span: expr.span,
                    kind: UnresolvedTypeKind::Variable(self.ctx.new_variable()),
                },
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
                UnresolvedType {
                    span: expr.span,
                    kind: UnresolvedTypeKind::Builtin(BuiltinType::Text),
                },
                UnresolvedExpressionKind::Text(*text),
            ),
            lower::ExpressionKind::Number(number) => (
                UnresolvedType {
                    span: expr.span,
                    kind: UnresolvedTypeKind::Builtin(BuiltinType::Number),
                },
                UnresolvedExpressionKind::Number(*number),
            ),
            lower::ExpressionKind::Block(statements, declarations) => {
                let (ty, statements) =
                    self.typecheck_block(expr.span, statements, file, suppress_errors);

                (
                    ty,
                    UnresolvedExpressionKind::Block(statements, declarations.clone()),
                )
            }
            lower::ExpressionKind::Call(lhs, input) => {
                let function = self.typecheck_expr(lhs, file, suppress_errors);
                let input = self.typecheck_expr(input, file, suppress_errors);

                let output_ty = UnresolvedType {
                    span: expr.span,
                    kind: UnresolvedTypeKind::Variable(self.ctx.new_variable()),
                };

                match self.ctx.unify(
                    function.ty.clone(),
                    UnresolvedType {
                        span: function.ty.span,
                        kind: UnresolvedTypeKind::Function(
                            Box::new(input.ty.clone()),
                            Box::new(output_ty.clone()),
                        ),
                    },
                ) {
                    Ok(ty) => ty,
                    Err(errors) => {
                        if !suppress_errors {
                            self.report_type_error(errors, &file.borrow());
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

                let (input_span, input_var) = self
                    .function_inputs
                    .pop()
                    .expect("function body does not refer to function input");

                (
                    UnresolvedType {
                        span: expr.span,
                        kind: UnresolvedTypeKind::Function(
                            Box::new(UnresolvedType {
                                span: input_span,
                                kind: UnresolvedTypeKind::Variable(input_var),
                            }),
                            Box::new(body.ty.clone()),
                        ),
                    },
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
                    UnresolvedType {
                        span: expr.span,
                        kind: UnresolvedTypeKind::Variable(self.ctx.new_variable()),
                    },
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
                            self.report_type_error(error, &file.borrow());
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
                    UnresolvedType {
                        span: expr.span,
                        kind: UnresolvedTypeKind::Builtin(BuiltinType::Unit),
                    },
                    UnresolvedExpressionKind::Initialize(*id, Box::new(value)),
                )
            }
            lower::ExpressionKind::FunctionInput => {
                let var = self.ctx.new_variable();
                self.function_inputs.push((expr.span, var));

                (
                    UnresolvedType {
                        span: expr.span,
                        kind: UnresolvedTypeKind::Variable(var),
                    },
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
                                self.report_type_error(errors, &file.borrow());
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
                    UnresolvedType {
                        span: expr.span,
                        kind: UnresolvedTypeKind::Named(*ty), // TODO: parameters
                    },
                    UnresolvedExpressionKind::Structure(fields),
                )
            }
        };

        UnresolvedExpression { ty, kind }
    }

    fn typecheck_block(
        &mut self,
        span: Span,
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
            .unwrap_or_else(|| UnresolvedType {
                span,
                kind: UnresolvedTypeKind::Builtin(BuiltinType::Unit),
            });

        (ty, statements)
    }

    fn convert_type_id(&mut self, span: Span, id: TypeId, file: &lower::File) -> UnresolvedType {
        let decl = file.declarations.types.get(&id).unwrap().clone();

        match decl {
            lower::Declaration::Local(decl) | lower::Declaration::Builtin(decl) => {
                match decl.value {
                    lower::Type::Marker
                    | lower::Type::Structure(_, _)
                    | lower::Type::Enumeration(_, _) => UnresolvedType {
                        span,
                        kind: UnresolvedTypeKind::Named(id), // TODO: parameters
                    },
                    lower::Type::Alias(annotation) => self.convert_type_annotation(&annotation),
                }
            }
            lower::Declaration::Dependency(_) => UnresolvedType {
                span,
                kind: UnresolvedTypeKind::Named(id), // TODO: parameters
            },
        }
    }

    fn monomorphize_constant(&mut self, id: &GenericConstantId) -> (ConstantId, UnresolvedType) {
        let mut constant = self.generic_constants.get(id).unwrap().clone();

        let body = constant.decl.value;
        let mut body = self.typecheck_expr(&body, &constant.file, true);

        if self
            .ctx
            .unify(body.ty.clone(), constant.generic_ty)
            .is_err()
        {
            body = UnresolvedExpression {
                ty: UnresolvedType {
                    span: body.ty.span,
                    kind: UnresolvedTypeKind::Bottom(BottomTypeReason::Error),
                },
                kind: UnresolvedExpressionKind::Error,
            }
        }

        let mut substitutions = BTreeMap::new();
        let mut add_substitutions = |ty: &mut UnresolvedType| {
            ty.apply(&self.ctx);

            for param in ty.params() {
                substitutions
                    .entry(param)
                    .or_insert_with(|| UnresolvedType {
                        span: param.span,
                        kind: UnresolvedTypeKind::Variable(self.ctx.new_variable()),
                    });
            }

            ty.instantiate_with(&substitutions);
        };

        body.traverse_mut(|expr| add_substitutions(&mut expr.ty));

        for (_, ty) in &mut constant.bounds {
            add_substitutions(ty);
        }

        let ty = body.ty.clone();

        let monomorphized_id = self.compiler.new_constant_id();

        self.monomorphized_constants.insert(
            monomorphized_id,
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
        );

        (monomorphized_id, ty)
    }

    fn finalize(
        &mut self,
        expr: UnresolvedExpression,
        generic: bool,
        file: &Rc<RefCell<lower::File>>,
    ) -> Result<Expression, TypeError> {
        let expr_span = expr.ty.span;

        Ok(Expression {
            kind: match expr.kind {
                UnresolvedExpressionKind::Error => ExpressionKind::Error,
                UnresolvedExpressionKind::Marker => ExpressionKind::Marker,
                UnresolvedExpressionKind::Constant(id) => ExpressionKind::Constant(id),
                UnresolvedExpressionKind::Variable(var) => ExpressionKind::Variable(var),
                UnresolvedExpressionKind::Text(text) => ExpressionKind::Text(text),
                UnresolvedExpressionKind::Number(number) => ExpressionKind::Number(number),
                UnresolvedExpressionKind::Block(statements, scope) => ExpressionKind::Block(
                    statements
                        .into_iter()
                        .map(|expr| self.finalize(expr, generic, file))
                        .collect::<Result<_, _>>()?,
                    scope,
                ),
                UnresolvedExpressionKind::Call(func, input) => ExpressionKind::Call(
                    Box::new(self.finalize(*func, generic, file)?),
                    Box::new(self.finalize(*input, generic, file)?),
                ),
                UnresolvedExpressionKind::Member(expr, index) => {
                    ExpressionKind::Member(Box::new(self.finalize(*expr, generic, file)?), index)
                }
                UnresolvedExpressionKind::Function(body, captures) => ExpressionKind::Function(
                    Box::new(self.finalize(*body, generic, file)?),
                    captures,
                ),
                UnresolvedExpressionKind::When(_, _) => todo!(),
                UnresolvedExpressionKind::External(namespace, identifier, inputs) => {
                    ExpressionKind::External(
                        namespace,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|expr| self.finalize(expr, generic, file))
                            .collect::<Result<_, _>>()?,
                    )
                }
                UnresolvedExpressionKind::Initialize(variable, value) => {
                    ExpressionKind::Initialize(
                        variable,
                        Box::new(self.finalize(*value, generic, file)?),
                    )
                }
                UnresolvedExpressionKind::Structure(fields) => ExpressionKind::Structure(
                    fields
                        .into_iter()
                        .map(|expr| self.finalize(expr, generic, file))
                        .collect::<Result<_, _>>()?,
                ),
                UnresolvedExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
                UnresolvedExpressionKind::Trait(tr) => {
                    ExpressionKind::Constant(self.instance_for(tr, expr.ty.clone())?)
                }
            },
            // The type must be resolved after the kind because the kind may be
            // a trait, whose type is unified with the original type variable
            ty: expr.ty.finalize(&self.ctx, generic).ok_or(TypeError {
                span: expr_span,
                kind: TypeErrorKind::UnresolvedType,
            })?,
        })
    }

    fn instance_for(
        &mut self,
        tr: TraitId,
        mut ty: UnresolvedType,
    ) -> Result<ConstantId, TypeError> {
        ty.apply(&self.ctx);

        let bound_instances = self.bound_instances.get(&tr).cloned().unwrap_or_default();

        for instance in bound_instances.into_iter().rev() {
            let constant = self.monomorphized_constants.get(&instance).unwrap();

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
            0 => Err(TypeError {
                span: ty.span,
                kind: TypeErrorKind::MissingInstance(tr, ty),
            }),
            1 => {
                let (ctx, instance) = candidates.pop().unwrap();
                self.ctx = ctx;

                let (monomorphized_id, _) = self.monomorphize_constant(&instance);
                Ok(monomorphized_id)
            }
            _ => Err(TypeError {
                span: ty.span,
                kind: TypeErrorKind::AmbiguousTrait(
                    tr,
                    candidates
                        .into_iter()
                        .map(|(_, instance)| instance)
                        .collect(),
                ),
            }),
        }
    }

    fn convert_type_annotation(&mut self, annotation: &lower::TypeAnnotation) -> UnresolvedType {
        UnresolvedType {
            span: annotation.span,
            kind: match &annotation.kind {
                lower::TypeAnnotationKind::Error => {
                    UnresolvedTypeKind::Bottom(BottomTypeReason::Error)
                }
                lower::TypeAnnotationKind::Placeholder => {
                    UnresolvedTypeKind::Variable(self.ctx.new_variable())
                }
                lower::TypeAnnotationKind::Named(id, _params) => UnresolvedTypeKind::Named(*id), // TODO: parameters
                lower::TypeAnnotationKind::Parameter(id) => {
                    UnresolvedTypeKind::Parameter(TypeParameter {
                        span: annotation.span,
                        id: *id,
                    })
                }
                lower::TypeAnnotationKind::Builtin(builtin) => {
                    UnresolvedTypeKind::Builtin(match builtin {
                        lower::BuiltinType::Unit => BuiltinType::Unit,
                        lower::BuiltinType::Number => BuiltinType::Number,
                        lower::BuiltinType::Text => BuiltinType::Text,
                    })
                }
                lower::TypeAnnotationKind::Function(input, output) => UnresolvedTypeKind::Function(
                    Box::new(self.convert_type_annotation(input)),
                    Box::new(self.convert_type_annotation(output)),
                ),
            },
        }
    }

    fn report_type_error(&mut self, error: TypeError, file: &lower::File) {
        self.well_typed = false;

        if let TypeErrorKind::Mismatch(actual, expected) = &error.kind {
            if actual.contains_error() || expected.contains_error() {
                return;
            }
        }

        macro_rules! getter {
            ($x:ident) => {
                |id| file.declarations.$x.get(&id).unwrap().name().to_string()
            };
        }

        let type_names = getter!(types);
        let param_names = getter!(type_parameters);
        let trait_names = getter!(traits);

        let diagnostic = match error.kind {
            TypeErrorKind::Recursive(_) => Diagnostic::error(
                "recursive type",
                vec![Note::primary(
                    error.span,
                    "the type of this references itself",
                )],
            ),
            TypeErrorKind::Mismatch(actual, expected) => Diagnostic::error(
                "mismatched types",
                vec![Note::primary(
                    error.span,
                    format!(
                        "expected `{}`, but found `{}`",
                        format_type(&expected, type_names, param_names),
                        format_type(&actual, type_names, param_names)
                    ),
                )],
            ),
            TypeErrorKind::MissingInstance(tr, ty) => Diagnostic::error(
                "missing instance",
                vec![Note::primary(
                    error.span,
                    format!(
                        "could not find instance of `{}` for type `{}`",
                        trait_names(tr),
                        format_type(&ty, type_names, param_names)
                    ),
                )],
            ),
            TypeErrorKind::AmbiguousTrait(_, candidates) => Diagnostic::error(
                "could not determine the type of this expression",
                std::iter::once(Note::primary(
                    error.span,
                    "try annotating the type with `::`",
                ))
                .chain(candidates.into_iter().map(|instance| {
                    let instance = self.generic_constants.get(&instance).unwrap();
                    Note::secondary(instance.decl.span, "this instance could apply")
                }))
                .collect(),
            ),
            TypeErrorKind::UnresolvedType => Diagnostic::error(
                "could not determine the type of this expression",
                vec![Note::primary(
                    error.span,
                    "try annotating the type with `::`",
                )],
            ),
        };

        self.compiler.diagnostics.add(diagnostic);
    }
}
