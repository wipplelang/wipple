#![allow(clippy::type_complexity)]

use crate::{
    compile::{File, Info, Item, ItemKind},
    id::{ItemId, TypeId, VariableId},
};
use lazy_static::lazy_static;
use polytype::UnificationError;
use std::{collections::HashMap, mem, sync::Arc};
use wipple_diagnostics::{Diagnostic, DiagnosticLevel, Note};

pub type Type = polytype::Type<TypeId>;
pub type TypeSchema = polytype::TypeSchema<TypeId>;
pub type Context = polytype::Context<TypeId>;

impl polytype::Name for TypeId {
    fn arrow() -> Self {
        unimplemented!()
    }
}

pub struct BuiltinTypes {
    pub unit: Type,
    pub number: Type,
    pub text: Type,
}

macro_rules! builtin_type {
    ($($var:expr),* $(,)?) => {
        Type::Constructed(TypeId::new(), vec![$($var),*])
    };
}

lazy_static! {
    pub static ref BUILTIN_TYPES: BuiltinTypes = BuiltinTypes {
        unit: builtin_type!(),
        number: builtin_type!(),
        text: builtin_type!(),
    };
}

pub fn function_type(input: Type, output: Type) -> Type {
    lazy_static! {
        static ref FUNCTION_TYPE_ID: TypeId = TypeId::new();
    }

    Type::Constructed(*FUNCTION_TYPE_ID, vec![input, output])
}

pub fn typecheck<'a>(
    info: &'a mut Info<'a>,
) -> Option<(Vec<Arc<File>>, HashMap<ItemId, TypeSchema>)> {
    let mut typechecker = Typechecker::new(info);
    let files = typechecker.info.files.clone();

    for file in &files {
        typechecker.typecheck_block(&file.statements);
    }

    typechecker.success.then(|| (files, typechecker.types))
}

struct Typechecker<'a> {
    info: &'a mut Info<'a>,
    ctx: Context,
    success: bool,
    types: HashMap<ItemId, TypeSchema>,
    variables: HashMap<VariableId, TypeSchema>,
    function_input: Option<(ItemId, TypeSchema)>,
}

impl<'a> Typechecker<'a> {
    fn new(info: &'a mut Info<'a>) -> Self {
        Typechecker {
            info,
            ctx: Default::default(),
            success: Default::default(),
            types: Default::default(),
            variables: Default::default(),
            function_input: Default::default(),
        }
    }

    fn typecheck_item(&mut self, item: &Item) -> Option<TypeSchema> {
        let ty = (|| match &item.kind {
            ItemKind::Unit => Some(TypeSchema::Monotype(BUILTIN_TYPES.unit.clone())),
            ItemKind::Number { .. } => Some(TypeSchema::Monotype(BUILTIN_TYPES.number.clone())),
            ItemKind::Text { .. } => Some(TypeSchema::Monotype(BUILTIN_TYPES.text.clone())),
            ItemKind::Block { statements } => self.typecheck_block(statements),
            ItemKind::Apply { function, input } => {
                let function_ty = {
                    let function_ty =
                        function_type(self.ctx.new_variable(), self.ctx.new_variable());

                    let inferred_function_ty =
                        self.typecheck_item(function)?.instantiate(&mut self.ctx);

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

                let input_ty = function_associated_types.next().unwrap();
                let output_ty = function_associated_types.next().unwrap();

                let inferred_input_ty = self.typecheck_item(input)?.instantiate(&mut self.ctx);

                if let Err(error) = self.ctx.unify(&input_ty, &inferred_input_ty) {
                    self.report_type_error(input, error);
                    return None;
                }

                Some(TypeSchema::Monotype(output_ty))
            }
            ItemKind::Initialize {
                variable, value, ..
            } => {
                let value_ty = self.typecheck_item(value)?;
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
                let body_ty = self.typecheck_item(body)?.instantiate(&mut self.ctx);
                let input = mem::replace(&mut self.function_input, previous_input);

                let (input_id, input_ty) = input.unwrap();
                self.types.insert(input_id, input_ty.clone());

                let input_ty = input_ty.instantiate(&mut self.ctx);

                Some(function_type(input_ty, body_ty).generalize(&[]))
            }
            ItemKind::FunctionInput => {
                let var = TypeSchema::Monotype(self.ctx.new_variable());
                self.function_input = Some((item.id, var.clone()));
                Some(var)
            }
            ItemKind::External { .. } => Some(TypeSchema::Monotype(self.ctx.new_variable())),
            ItemKind::Annotate { item, ty } => {
                let inferred_ty = self.typecheck_item(item)?.instantiate(&mut self.ctx);

                if let Err(error) = self.ctx.unify(ty, &inferred_ty) {
                    self.report_type_error(item, error);
                    return None;
                }

                let item_ty = ty.apply(&self.ctx);

                Some(TypeSchema::Monotype(item_ty))
            }
        })();

        if let Some(ty) = ty.clone() {
            self.types.insert(item.id, ty);
        } else {
            self.success = false;
        }

        ty
    }

    fn typecheck_block(&mut self, statements: &[Item]) -> Option<TypeSchema> {
        let mut last_ty = TypeSchema::Monotype(BUILTIN_TYPES.unit.clone());
        let mut success = true;
        for statement in statements {
            last_ty = match self.typecheck_item(statement) {
                Some(ty) => ty,
                None => {
                    success = false;
                    continue;
                }
            }
        }

        success.then(|| last_ty.clone())
    }

    fn report_type_error(&mut self, item: &Item, error: UnificationError<TypeId>) {
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

pub fn format_type_schema(ty: &TypeSchema) -> String {
    // TODO
    format!("{:?}", ty)
}

pub fn format_type(ty: &Type) -> String {
    // TODO
    format!("{:?}", ty)
}
