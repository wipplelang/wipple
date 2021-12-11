#![allow(clippy::type_complexity)]

mod format;

pub use format::*;

use crate::*;
use lazy_static::lazy_static;
use polytype::UnificationError;
use serde::Serialize;
use std::{cell::RefCell, collections::HashMap, mem};
use wipple_diagnostics::{Diagnostic, DiagnosticLevel, Note};

#[derive(Debug, Clone, Copy, Serialize)]
pub struct TypeName {
    pub id: TypeId,
    pub name: Option<InternedString>,
    pub format: TypeNameFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum TypeNameFormat {
    Default,
    Function,
}

impl TypeName {
    pub fn new(name: Option<impl ToString>, format: TypeNameFormat) -> Self {
        TypeName::with_id(TypeId::new(), name, format)
    }

    pub fn with_id(id: TypeId, name: Option<impl ToString>, format: TypeNameFormat) -> Self {
        TypeName {
            id,
            name: name.map(|name| InternedString::new(name.to_string())),
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
    pub never: Type,
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
        unit: builtin_type!(Some("()"), TypeNameFormat::Default),
        never: builtin_type!(Some("!"), TypeNameFormat::Default),
        number: builtin_type!(Some("Number"), TypeNameFormat::Default),
        text: builtin_type!(Some("Text"), TypeNameFormat::Default),
    };
    static ref FUNCTION_TYPE_ID: TypeId = TypeId::new();
}

pub fn function_type(input: Type, output: Type) -> Type {
    Type::Constructed(
        TypeName::with_id(*FUNCTION_TYPE_ID, Some("->"), TypeNameFormat::Function),
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
    data_decls: HashMap<TypeId, Vec<Type>>,
    function_input: Option<usize>,
    end_ty: Option<Type>,
    return_ty: Option<Type>,
}

impl<'a> Typechecker<'a> {
    fn new(info: &'a mut Info<'a>) -> Self {
        Typechecker {
            info,
            ctx: (*typechecker_context()).clone().into_inner(),
            success: true,
            types: Default::default(),
            variables: Default::default(),
            data_decls: Default::default(),
            function_input: Default::default(),
            end_ty: Default::default(),
            return_ty: Default::default(),
        }
    }

    fn typecheck_item(&mut self, item: &Item, expected_ty: Option<Type>) -> Option<TypeSchema> {
        let var = self.ctx.new_variable();

        let ty = (|| match &item.kind {
            ItemKind::Unit(_) => Some(TypeSchema::Monotype(BUILTIN_TYPES.unit.clone())),
            ItemKind::Number(_) => Some(TypeSchema::Monotype(BUILTIN_TYPES.number.clone())),
            ItemKind::Text(_) => Some(TypeSchema::Monotype(BUILTIN_TYPES.text.clone())),
            ItemKind::Block(block) => self.typecheck_block(&block.statements),
            ItemKind::Apply(apply) => {
                let inferred_input_ty = self
                    .typecheck_item(&apply.input, None)?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                let function_ty = {
                    let function_ty = function_type(
                        inferred_input_ty.clone(),
                        expected_ty.unwrap_or_else(|| self.ctx.new_variable()),
                    )
                    .apply(&self.ctx);

                    let inferred_function_ty = self
                        .typecheck_item(&apply.function, None)?
                        .instantiate(&mut self.ctx)
                        .apply(&self.ctx);

                    if let Err(error) = self.ctx.unify(&function_ty, &inferred_function_ty) {
                        self.report_type_error(&apply.function, error);
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
                    self.report_type_error(&apply.input, error);
                    return None;
                }

                Some(TypeSchema::Monotype(output_ty.apply(&self.ctx)))
            }
            ItemKind::Initialize(initialize) => {
                let value_ty = self.typecheck_item(&initialize.value, None)?;
                self.variables.insert(initialize.variable, value_ty);

                Some(TypeSchema::Monotype(BUILTIN_TYPES.unit.clone()))
            }
            ItemKind::Variable(variable) => Some(
                self.variables
                    .get(&variable.variable)
                    .cloned()
                    .unwrap_or_else(|| {
                        panic!("Variable {:?} used before initialization", variable)
                    }),
            ),
            ItemKind::Function(function) => {
                let previous_input = mem::take(&mut self.function_input);
                let previous_return_ty = mem::take(&mut self.return_ty);

                let body_ty = self
                    .typecheck_item(&function.body, None)?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                let input_var = mem::replace(&mut self.function_input, previous_input).unwrap();

                let return_ty =
                    mem::replace(&mut self.return_ty, previous_return_ty).unwrap_or(body_ty);

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
                    function_type(Type::Variable(input_var), return_ty)
                        .apply(&self.ctx)
                        .generalize(&vars),
                )
            }
            ItemKind::FunctionInput(_) => {
                let var = match var {
                    Type::Variable(var) => var,
                    _ => unreachable!(),
                };

                self.function_input = Some(var);
                Some(TypeSchema::Monotype(Type::Variable(var)))
            }
            ItemKind::External { .. } => Some(TypeSchema::Monotype(var.clone())),
            ItemKind::Annotate(annotate) => {
                let ty = self.convert_constructor(&annotate.constructor);

                let inferred_ty = self
                    .typecheck_item(&annotate.item, Some(ty.clone()))?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                if let Err(error) = self.ctx.unify(&ty, &inferred_ty) {
                    self.report_type_error(&annotate.item, error);
                    return None;
                }

                let item_ty = ty.apply(&self.ctx);

                Some(TypeSchema::Monotype(item_ty))
            }
            ItemKind::DataDecl(decl) => {
                let field_tys = decl
                    .fields
                    .iter()
                    .map(|field| self.convert_constructor(field))
                    .collect();

                self.data_decls.insert(decl.id, field_tys);

                Some(TypeSchema::Monotype(BUILTIN_TYPES.unit.clone()))
            }
            ItemKind::Data(data) => {
                let mut field_tys = self.data_decls.get(&data.id).unwrap().clone().into_iter();

                let mut success = true;
                for field in data.fields.iter() {
                    let expected_ty = field_tys.next().unwrap();

                    let ty = self
                        .typecheck_item(field, Some(expected_ty.clone()))?
                        .instantiate(&mut self.ctx)
                        .apply(&self.ctx);

                    if let Err(error) = self.ctx.unify(&expected_ty, &ty) {
                        self.report_type_error(field, error);
                        success = false;
                    }
                }

                success.then(|| {
                    TypeSchema::Monotype(Type::Constructed(
                        TypeName::with_id(data.id, None::<String>, TypeNameFormat::Default),
                        Vec::new(), // TODO: Generics
                    ))
                })
            }
            ItemKind::Loop(r#loop) => {
                let previous_end_ty = mem::take(&mut self.end_ty);

                self.typecheck_item(&r#loop.body, None)?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                let end_ty = mem::replace(&mut self.end_ty, previous_end_ty)
                    .unwrap_or_else(|| BUILTIN_TYPES.never.clone());

                Some(TypeSchema::Monotype(end_ty))
            }
            ItemKind::End(end) => {
                let ty = self
                    .typecheck_item(&end.value, None)?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                match self.end_ty.as_mut() {
                    Some(end_ty) => {
                        let end_ty = end_ty.apply(&self.ctx);

                        if let Err(error) = self.ctx.unify(&end_ty, &ty) {
                            self.report_type_error(&end.value, error);
                            return None;
                        }
                    }
                    None => self.end_ty = Some(ty),
                }

                Some(TypeSchema::Monotype(BUILTIN_TYPES.unit.clone()))
            }
            ItemKind::Return(r#return) => {
                let ty = self
                    .typecheck_item(&r#return.value, None)?
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                match self.return_ty.as_mut() {
                    Some(return_ty) => {
                        let return_ty = return_ty.apply(&self.ctx);

                        if let Err(error) = self.ctx.unify(&return_ty, &ty) {
                            self.report_type_error(&r#return.value, error);
                            return None;
                        }
                    }
                    None => self.return_ty = Some(ty),
                }

                Some(TypeSchema::Monotype(BUILTIN_TYPES.unit.clone()))
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
                    item.info.span,
                    "The type of this references itself",
                )],
            ),
            UnificationError::Failure(expected, found) => Diagnostic::new(
                DiagnosticLevel::Error,
                "Mismatched types",
                vec![Note::primary(
                    item.info.span,
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

    fn convert_constructor(&mut self, constructor: &Constructor) -> Type {
        match constructor {
            Constructor::Placeholder => self.ctx.new_variable(),
            Constructor::Number => BUILTIN_TYPES.number.clone(),
            Constructor::Text => BUILTIN_TYPES.text.clone(),
            Constructor::Unit => BUILTIN_TYPES.unit.clone(),
            Constructor::Function { input, output } => function_type(
                self.convert_constructor(input),
                self.convert_constructor(output),
            ),
            Constructor::DataStruct { id, .. } => Type::Constructed(
                TypeName::with_id(*id, None::<String>, TypeNameFormat::Default),
                Vec::new(),
            ),
        }
    }
}
