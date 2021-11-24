#![allow(clippy::type_complexity)]

mod format;

pub use format::*;

use crate::{
    compile::{File, Info, Item, ItemKind},
    id::{ItemId, TypeId, VariableId},
};
use interned_string::InternedString;
use lazy_static::lazy_static;
use polytype::UnificationError;
use serde::Serialize;
use std::{cell::RefCell, collections::HashMap, mem, sync::Arc};
use wipple_diagnostics::{Diagnostic, DiagnosticLevel, Note};

#[derive(Debug, Clone, Copy, Serialize)]
pub struct TypeName {
    pub id: TypeId,
    pub name: InternedString,
    pub format: TypeNameFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum TypeNameFormat {
    Default,
    Function,
}

impl TypeName {
    pub fn new(name: impl ToString, format: TypeNameFormat) -> Self {
        TypeName::with_id(TypeId::new(), name, format)
    }

    pub fn with_id(id: TypeId, name: impl ToString, format: TypeNameFormat) -> Self {
        TypeName {
            id,
            name: InternedString::new(name.to_string()),
            format,
        }
    }
}

impl PartialEq for TypeName {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TypeName {}

impl polytype::Name for TypeName {
    fn arrow() -> Self {
        unimplemented!()
    }
}

pub type Type = polytype::Type<TypeName>;
pub type TypeSchema = polytype::TypeSchema<TypeName>;
pub type Context = polytype::Context<TypeName>;

pub struct BuiltinTypes {
    pub unit: Type,
    pub number: Type,
    pub text: Type,
}

macro_rules! builtin_type {
    ($name:expr, $format:expr $(, $var:expr)* $(,)?) => {
        Type::Constructed(TypeName::new($name, $format), vec![$($var),*])
    };
}

lazy_static! {
    pub static ref BUILTIN_TYPES: BuiltinTypes = BuiltinTypes {
        unit: builtin_type!("()", TypeNameFormat::Default),
        number: builtin_type!("Number", TypeNameFormat::Default),
        text: builtin_type!("Text", TypeNameFormat::Default),
    };
    static ref FUNCTION_TYPE_ID: TypeId = TypeId::new();
}

pub fn function_type(input: Type, output: Type) -> Type {
    Type::Constructed(
        TypeName::with_id(*FUNCTION_TYPE_ID, "->", TypeNameFormat::Function),
        vec![input, output],
    )
}

pub fn typecheck<'a>(
    info: &'a mut Info<'a>,
) -> Option<(Vec<Arc<File>>, HashMap<ItemId, TypeSchema>)> {
    let mut typechecker = Typechecker::new(info);
    let files = typechecker.info.files.clone();

    for file in &files {
        typechecker.typecheck_block(&file.statements);
    }

    typechecker.success.then(|| {
        let types = typechecker
            .types
            .into_iter()
            .map(move |(item, ty)| {
                let ty = match ty {
                    TypeSchema::Monotype(ty) => TypeSchema::Monotype(ty.apply(&typechecker.ctx)),
                    TypeSchema::Polytype { .. } => ty,
                };

                (item, ty)
            })
            .collect();

        (files, types)
    })
}

pub(crate) fn typechecker_context() -> Arc<RefCell<Context>> {
    // TODO: Make usable across threads
    thread_local! {
        static TYPECHECKER_CONTEXT: Arc<RefCell<Context>> = Default::default();
    }

    TYPECHECKER_CONTEXT.with(Clone::clone)
}

struct Typechecker<'a> {
    info: &'a mut Info<'a>,
    ctx: Context,
    success: bool,
    types: HashMap<ItemId, TypeSchema>,
    variables: HashMap<VariableId, TypeSchema>,
    function_input: Option<usize>,
}

impl<'a> Typechecker<'a> {
    fn new(info: &'a mut Info<'a>) -> Self {
        let ctx = (*typechecker_context()).clone().into_inner();

        Typechecker {
            info,
            ctx,
            success: true,
            types: Default::default(),
            variables: Default::default(),
            function_input: Default::default(),
        }
    }

    fn typecheck_item(&mut self, item: &Item, expected_ty: Option<Type>) -> Option<TypeSchema> {
        let var = self.ctx.new_variable();

        let ty = (|| match &item.kind {
            ItemKind::Unit => Some(TypeSchema::Monotype(BUILTIN_TYPES.unit.clone())),
            ItemKind::Number { .. } => Some(TypeSchema::Monotype(BUILTIN_TYPES.number.clone())),
            ItemKind::Text { .. } => Some(TypeSchema::Monotype(BUILTIN_TYPES.text.clone())),
            ItemKind::Block { statements } => self.typecheck_block(statements),
            ItemKind::Apply { function, input } => {
                let inferred_input_ty = self
                    .typecheck_item(input, None)?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                let function_ty = {
                    let function_ty = function_type(
                        inferred_input_ty.clone(),
                        expected_ty.unwrap_or_else(|| self.ctx.new_variable()),
                    )
                    .apply(&self.ctx);

                    let inferred_function_ty = self
                        .typecheck_item(function, None)?
                        .instantiate(&mut self.ctx)
                        .apply(&self.ctx);

                    if let Err(error) = self.ctx.unify(&function_ty, &inferred_function_ty) {
                        self.report_type_error(function, error);
                        return None;
                    }

                    function_ty.apply(&self.ctx)
                };

                let mut function_associated_types = match function_ty {
                    Type::Constructed(_, associated_types) => associated_types.into_iter(),
                    _ => unreachable!(),
                };

                let input_ty = function_associated_types.next().unwrap().apply(&self.ctx);
                let output_ty = function_associated_types.next().unwrap().apply(&self.ctx);

                if let Err(error) = self.ctx.unify(&input_ty, &inferred_input_ty) {
                    self.report_type_error(input, error);
                    return None;
                }

                Some(TypeSchema::Monotype(output_ty.apply(&self.ctx)))
            }
            ItemKind::Initialize {
                variable, value, ..
            } => {
                let value_ty = self.typecheck_item(value, None)?;
                self.variables.insert(*variable, value_ty);

                Some(TypeSchema::Monotype(BUILTIN_TYPES.unit.clone()))
            }
            ItemKind::Variable { variable } => {
                Some(self.variables.get(variable).cloned().unwrap_or_else(|| {
                    panic!("Variable {:?} used before initialization", variable)
                }))
            }
            ItemKind::Function { body, .. } => {
                let previous_input = mem::take(&mut self.function_input);

                let body_ty = self
                    .typecheck_item(body, None)?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                let input_var = mem::replace(&mut self.function_input, previous_input).unwrap();

                // FIXME: Instead of collecting every single type variable except the function input,
                // implement 'generalize' ourselves
                let vars = self
                    .types
                    .values()
                    .filter_map(|ty| match ty {
                        TypeSchema::Monotype(Type::Variable(var)) if *var != input_var => {
                            Some(*var)
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>();

                Some(
                    function_type(Type::Variable(input_var), body_ty)
                        .apply(&self.ctx)
                        .generalize(&vars),
                )
            }
            ItemKind::FunctionInput => {
                let var = match var {
                    Type::Variable(var) => var,
                    _ => unreachable!(),
                };

                self.function_input = Some(var);
                Some(TypeSchema::Monotype(Type::Variable(var)))
            }
            ItemKind::External { .. } => Some(TypeSchema::Monotype(var.clone())),
            ItemKind::Annotate { item, ty } => {
                let inferred_ty = self
                    .typecheck_item(item, Some(ty.clone()))?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                if let Err(error) = self.ctx.unify(ty, &inferred_ty) {
                    self.report_type_error(item, error);
                    return None;
                }

                let item_ty = ty.apply(&self.ctx);

                Some(TypeSchema::Monotype(item_ty))
            }
        })();

        if let Some(ty) = ty.clone() {
            match &ty {
                TypeSchema::Monotype(ty) => {
                    self.ctx.unify(&var, ty).unwrap();
                    self.types.insert(item.id, TypeSchema::Monotype(var));
                }
                TypeSchema::Polytype { .. } => {
                    self.types.insert(item.id, ty);
                }
            }
        } else {
            self.success = false;
        }

        ty
    }

    fn typecheck_block(&mut self, statements: &[Item]) -> Option<TypeSchema> {
        let mut last_ty = TypeSchema::Monotype(BUILTIN_TYPES.unit.clone());
        let mut success = true;
        for statement in statements {
            last_ty = match self.typecheck_item(statement, None) {
                Some(ty) => ty,
                None => {
                    success = false;
                    continue;
                }
            }
        }

        success.then(|| last_ty.clone())
    }

    fn report_type_error(&mut self, item: &Item, error: UnificationError<TypeName>) {
        let diagnostic = match error {
            UnificationError::Occurs(_) => Diagnostic::new(
                DiagnosticLevel::Error,
                "Recursive type",
                vec![Note::primary(
                    item.debug_info.span,
                    "The type of this references itself",
                )],
            ),
            UnificationError::Failure(expected, found) => Diagnostic::new(
                DiagnosticLevel::Error,
                "Mismatched types",
                vec![Note::primary(
                    item.debug_info.span,
                    format!(
                        "Expected {}, found {}",
                        format_type(&expected),
                        format_type(&found)
                    ),
                )],
            ),
        };

        self.info.diagnostics.add(diagnostic);
    }
}
