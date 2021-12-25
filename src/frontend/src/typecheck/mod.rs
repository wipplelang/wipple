#![allow(clippy::type_complexity)]

pub mod format;
pub mod item;

pub use format::*;
pub use item::{Item, *};

use crate::*;
use lazy_static::lazy_static;
use polytype::UnificationError;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    mem,
};
use wipple_diagnostics::{Diagnostic, DiagnosticLevel, Note};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct TypeName {
    pub id: TypeId,
    pub name: Option<InternedString>,
    pub format: TypeNameFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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
    pub static ref FUNCTION_TYPE_ID: TypeId = TypeId::new();
}

pub fn function_type(input: Type, output: Type) -> Type {
    Type::Constructed(
        TypeName::with_id(*FUNCTION_TYPE_ID, Some("->"), TypeNameFormat::Function),
        vec![input, output],
    )
}

pub fn typecheck(info: compile::Info) -> (bool, Item) {
    let block = compile::BlockItem::new(
        info.files
            .into_iter()
            .flat_map(|file| file.statements.clone())
            .collect(),
    );

    let mut typechecker = Typechecker::new(info.diagnostics);

    let info = compile::ItemInfo::new(Span::new(InternedString::new("<root>"), Default::default()));

    let mut item = typechecker.typecheck_block(info, &block.statements);
    item.traverse(|_, ty| match ty {
        TypeSchema::Monotype(ty) => ty.apply_mut(&typechecker.ctx),
        TypeSchema::Polytype { .. } => {}
    });

    (typechecker.well_typed, item)
}

pub(crate) fn typechecker_context() -> Arc<RefCell<Context>> {
    // TODO: Make usable across threads
    thread_local! {
        static TYPECHECKER_CONTEXT: Arc<RefCell<Context>> = Default::default();
    }

    TYPECHECKER_CONTEXT.with(Clone::clone)
}

struct Typechecker<'a> {
    diagnostics: &'a mut Diagnostics,
    ctx: Context,
    well_typed: bool,
    variables: HashMap<VariableId, TypeSchema>,
    data_decls: HashMap<TypeId, BTreeMap<InternedString, Type>>,
    function_input: Option<usize>,
    end_ty: Option<Type>,
    return_ty: Option<Type>,
}

impl<'a> Typechecker<'a> {
    fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Typechecker {
            diagnostics,
            ctx: (*typechecker_context()).clone().into_inner(),
            well_typed: true,
            variables: Default::default(),
            data_decls: Default::default(),
            function_input: Default::default(),
            end_ty: Default::default(),
            return_ty: Default::default(),
        }
    }

    fn typecheck_item(&mut self, item: &compile::Item, expected_ty: Option<Type>) -> Item {
        let var = self.ctx.new_variable();

        let item = (|| match &item.kind {
            compile::ItemKind::Error(_) => Item::error(item.info),
            compile::ItemKind::Unit(_) => Item::unit(item.info),
            compile::ItemKind::Number(number) => Item::number(item.info, number.value),
            compile::ItemKind::Text(text) => Item::text(item.info, text.value),
            compile::ItemKind::Block(block) => self.typecheck_block(item.info, &block.statements),
            compile::ItemKind::Apply(apply) => {
                let inferred_input_item = self.typecheck_item(&apply.input, None);

                let inferred_input_ty = inferred_input_item
                    .ty
                    .instantiate(&mut self.ctx)
                    .apply(&self.ctx);

                let (function_item, function_ty) = {
                    let function_ty = function_type(
                        inferred_input_ty.clone(),
                        expected_ty.unwrap_or_else(|| self.ctx.new_variable()),
                    )
                    .apply(&self.ctx);

                    let function_item = self.typecheck_item(&apply.function, None);

                    let inferred_function_ty =
                        function_item.ty.instantiate(&mut self.ctx).apply(&self.ctx);

                    if let Err(error) = self.ctx.unify(&function_ty, &inferred_function_ty) {
                        self.report_type_error(&apply.function, error);
                        return Item::error(item.info);
                    }

                    (function_item, function_ty.apply(&self.ctx))
                };

                let mut function_associated_types = match function_ty {
                    Type::Constructed(_, associated_types) => associated_types.into_iter(),
                    _ => unreachable!(),
                };

                let input_ty = function_associated_types.next().unwrap().apply(&self.ctx);
                let output_ty = function_associated_types.next().unwrap().apply(&self.ctx);

                if let Err(error) = self.ctx.unify(&input_ty, &inferred_input_ty) {
                    self.report_type_error(&apply.input, error);
                    return Item::error(item.info);
                }

                Item::apply(
                    item.info,
                    TypeSchema::Monotype(output_ty.apply(&self.ctx)),
                    function_item,
                    inferred_input_item,
                )
            }
            compile::ItemKind::Initialize(initialize) => {
                let value_item = self.typecheck_item(&initialize.value, None);

                self.variables
                    .insert(initialize.variable, value_item.ty.clone());

                Item::initialize(
                    item.info,
                    initialize.binding_info,
                    initialize.variable,
                    value_item,
                )
            }
            compile::ItemKind::Variable(variable) => {
                let ty = self
                    .variables
                    .get(&variable.variable)
                    .cloned()
                    .unwrap_or_else(|| {
                        panic!("Variable {:?} used before initialization", variable)
                    });

                Item::variable(item.info, ty, variable.variable)
            }
            compile::ItemKind::Function(function) => {
                let previous_input = mem::take(&mut self.function_input);
                let previous_return_ty = mem::take(&mut self.return_ty);

                let body_item = self.typecheck_item(&function.body, None);
                let body_ty = body_item.ty.instantiate(&mut self.ctx).apply(&self.ctx);

                let input_var = mem::replace(&mut self.function_input, previous_input).unwrap();

                let return_ty =
                    mem::replace(&mut self.return_ty, previous_return_ty).unwrap_or(body_ty);

                let ty = function_type(Type::Variable(input_var), return_ty).apply(&self.ctx);
                let generic = ty.vars().contains(&input_var);

                let mut generalized_ty = TypeSchema::Monotype(ty);
                if generic {
                    generalized_ty = TypeSchema::Polytype {
                        variable: input_var,
                        body: Box::new(generalized_ty),
                    };
                }

                Item::function(
                    item.info,
                    generalized_ty,
                    body_item,
                    function.captures.clone(),
                )
            }
            compile::ItemKind::FunctionInput(_) => {
                let var = match var {
                    Type::Variable(var) => var,
                    _ => unreachable!(),
                };

                self.function_input = Some(var);
                Item::function_input(item.info, TypeSchema::Monotype(Type::Variable(var)))
            }
            compile::ItemKind::External(external) => {
                let inputs = external
                    .inputs
                    .iter()
                    .map(|item| self.typecheck_item(item, None))
                    .collect();

                Item::external(
                    item.info,
                    TypeSchema::Monotype(var.clone()),
                    external.namespace,
                    external.identifier,
                    inputs,
                )
            }
            compile::ItemKind::Annotate(annotate) => {
                let ty = self.convert_constructor(&annotate.constructor);

                let mut inferred_item = self.typecheck_item(&annotate.item, Some(ty.clone()));
                let inferred_ty = inferred_item.ty.instantiate(&mut self.ctx).apply(&self.ctx);

                if let Err(error) = self.ctx.unify(&ty, &inferred_ty) {
                    self.report_type_error(&annotate.item, error);
                    return Item::error(item.info);
                }

                let item_ty = ty.apply(&self.ctx);
                inferred_item.ty = TypeSchema::Monotype(item_ty);

                inferred_item
            }
            compile::ItemKind::DataDecl(decl) => {
                let field_tys = decl
                    .fields
                    .iter()
                    .map(|(&name, field)| (name, self.convert_constructor(field)))
                    .collect();

                self.data_decls.insert(decl.id, field_tys);

                Item::unit(item.info)
            }
            compile::ItemKind::Data(data) => {
                let mut field_tys = self.data_decls.get(&data.id).unwrap().clone().into_iter();

                let fields = data
                    .fields
                    .iter()
                    .map(|field| {
                        let (_, expected_ty) = field_tys.next().unwrap();

                        let item = self.typecheck_item(field, None);
                        let ty = item.ty.instantiate(&mut self.ctx).apply(&self.ctx);

                        if let Err(error) = self.ctx.unify(&expected_ty, &ty) {
                            self.report_type_error(field, error);
                        }

                        item
                    })
                    .collect();

                let ty = TypeSchema::Monotype(Type::Constructed(
                    TypeName::with_id(data.id, None::<String>, TypeNameFormat::Default),
                    Vec::new(), // TODO: Generics
                ));

                Item::data(item.info, ty, data.id, fields)
            }
            compile::ItemKind::Loop(r#loop) => {
                let previous_end_ty = mem::take(&mut self.end_ty);

                let body_item = self.typecheck_item(&r#loop.body, None);
                body_item.ty.instantiate(&mut self.ctx).apply(&self.ctx);

                let end_ty = mem::replace(&mut self.end_ty, previous_end_ty)
                    .unwrap_or_else(|| BUILTIN_TYPES.never.clone());

                Item::r#loop(item.info, TypeSchema::Monotype(end_ty), body_item)
            }
            compile::ItemKind::End(end) => {
                let value_item = self.typecheck_item(&end.value, expected_ty);
                let ty = value_item.ty.instantiate(&mut self.ctx).apply(&self.ctx);

                match self.end_ty.as_mut() {
                    Some(end_ty) => {
                        let end_ty = end_ty.apply(&self.ctx);

                        if let Err(error) = self.ctx.unify(&end_ty, &ty) {
                            self.report_type_error(&end.value, error);
                            return Item::error(item.info);
                        }
                    }
                    None => self.end_ty = Some(ty),
                }

                Item::end(
                    item.info,
                    TypeSchema::Monotype(BUILTIN_TYPES.unit.clone()),
                    value_item,
                )
            }
            compile::ItemKind::Return(r#return) => {
                let value_item = self.typecheck_item(&r#return.value, expected_ty);
                let ty = value_item.ty.instantiate(&mut self.ctx).apply(&self.ctx);

                match self.return_ty.as_mut() {
                    Some(return_ty) => {
                        let return_ty = return_ty.apply(&self.ctx);

                        if let Err(error) = self.ctx.unify(&return_ty, &ty) {
                            self.report_type_error(&r#return.value, error);
                            return Item::error(item.info);
                        }
                    }
                    None => self.return_ty = Some(ty),
                }

                Item::r#return(
                    item.info,
                    TypeSchema::Monotype(BUILTIN_TYPES.unit.clone()),
                    value_item,
                )
            }
            compile::ItemKind::Field(field) => {
                let value_item = self.typecheck_item(&field.value, None);
                let value_ty = value_item.ty.instantiate(&mut self.ctx).apply(&self.ctx);

                let value_ty_id = match value_ty {
                    polytype::Type::Constructed(name, _) => name.id,
                    polytype::Type::Variable(_) => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Cannot access field on this",
                            vec![Note::primary(
                                field.value.info.span,
                                "Could not determine the type of this value",
                            )],
                        ));

                        return Item::error(item.info);
                    }
                };

                let decl = match self.data_decls.get(&value_ty_id) {
                    Some(decl) => decl,
                    None => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Cannot access field on this",
                            vec![Note::primary(
                                field.value.info.span,
                                "Value is not a data structure",
                            )],
                        ));

                        return Item::error(item.info);
                    }
                };

                let (ty, index) = match decl
                    .iter()
                    .enumerate()
                    .find_map(|(index, (&name, ty))| (name == field.name).then(|| (ty, index)))
                {
                    Some(ty) => ty,
                    None => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!(
                                "Value of type '{}' does not have field '{}'",
                                format_type(&value_ty),
                                field.name
                            ),
                            vec![Note::primary(field.name_span, "No such field")],
                        ));

                        return Item::error(item.info);
                    }
                };

                Item::field(
                    item.info,
                    TypeSchema::Monotype(ty.clone()),
                    value_item,
                    index,
                )
            }
        })();

        if matches!(item.kind, ItemKind::Error(_)) {
            self.well_typed = false;
        } else {
            match &item.ty {
                TypeSchema::Monotype(ty) => self.ctx.unify(&var, ty).unwrap(),
                TypeSchema::Polytype { .. } => {}
            }
        }

        item
    }

    fn typecheck_block(
        &mut self,
        compile_info: compile::ItemInfo,
        statements: &[compile::Item],
    ) -> Item {
        let statements = statements
            .iter()
            .map(|statement| self.typecheck_item(statement, None))
            .collect::<Vec<_>>();

        let ty = statements
            .last()
            .map(|statement| statement.ty.clone())
            .unwrap_or_else(|| TypeSchema::Monotype(BUILTIN_TYPES.unit.clone()));

        Item::block(compile_info, ty, statements)
    }

    fn report_type_error(&mut self, item: &compile::Item, error: UnificationError<TypeName>) {
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

        self.diagnostics.add(diagnostic);
    }

    fn convert_constructor(&mut self, constructor: &compile::Constructor) -> Type {
        use compile::Constructor;

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
