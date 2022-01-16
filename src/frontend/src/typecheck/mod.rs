pub mod engine;
pub mod format;
pub mod item;

pub use engine::*;
pub use format::*;
pub use item::{Item, *};

use crate::*;
use lazy_static::lazy_static;
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

pub type Context = engine::Context<TypeName>;
pub type Scheme = engine::Scheme<TypeName>;
pub type Type = engine::Type<TypeName>;
pub type ForAll = engine::ForAll<TypeName>;
pub type UnificationError = engine::UnificationError<TypeName>;

pub struct BuiltinTypes {
    pub never: Type,
    pub unit: Type,
    pub number: Type,
    pub text: Type,
}

fn builtin_type(name: &str, format: TypeNameFormat, bottom: bool) -> Type {
    Type::Constructed {
        id: TypeName::new(Some(name), format),
        params: Vec::new(),
        bottom,
    }
}

lazy_static! {
    pub static ref BUILTIN_TYPES: BuiltinTypes = BuiltinTypes {
        never: builtin_type("!", TypeNameFormat::Default, true),
        unit: builtin_type("()", TypeNameFormat::Default, false),
        number: builtin_type("Number", TypeNameFormat::Default, false),
        text: builtin_type("Text", TypeNameFormat::Default, false),
    };
    pub static ref FUNCTION_TYPE_ID: TypeId = TypeId::new();
}

pub fn function_type(input: Type, output: Type) -> Type {
    Type::Constructed {
        id: TypeName::with_id(*FUNCTION_TYPE_ID, Some("->"), TypeNameFormat::Function),
        params: vec![input, output],
        bottom: false,
    }
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
    item.traverse(|item| item.ty.apply(&typechecker.ctx));

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
    variables: HashMap<VariableId, Scheme>,
    data_decls: HashMap<TypeId, (compile::ItemInfo, BTreeMap<InternedString, Type>)>,
    function_inputs: Vec<TypeVariable>,
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
            function_inputs: Default::default(),
            end_ty: Default::default(),
            return_ty: Default::default(),
        }
    }

    fn typecheck_item(&mut self, item: &compile::Item) -> Item {
        match &item.kind {
            compile::ItemKind::Error(_) => {
                self.well_typed = false;
                Item::error(item.info)
            }
            compile::ItemKind::Unit(_) => Item::unit(item.info),
            compile::ItemKind::Number(number) => Item::number(item.info, number.value),
            compile::ItemKind::Text(text) => Item::text(item.info, text.value),
            compile::ItemKind::Block(block) => self.typecheck_block(item.info, &block.statements),
            compile::ItemKind::Apply(apply) => {
                let function_item = self.typecheck_item(&apply.function);
                let function_ty = function_item.ty.instantiate(&mut self.ctx);

                let input_item = self.typecheck_item(&apply.input);
                let input_ty = input_item.ty.instantiate(&mut self.ctx);

                let output_ty = Type::Variable(self.ctx.new_variable(None));

                if let Err(errors) = self
                    .ctx
                    .unify(function_ty, function_type(input_ty, output_ty.clone()))
                {
                    self.report_type_errors(&apply.function, errors);
                    self.well_typed = false;
                    return Item::error(item.info);
                }

                Item::apply(
                    item.info,
                    Scheme::Type(output_ty),
                    function_item,
                    input_item,
                )
            }
            compile::ItemKind::Initialize(initialize) => {
                let value_item = self.typecheck_item(&initialize.value);

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
                let previous_return_ty = mem::take(&mut self.return_ty);

                let mut body_item = self.typecheck_item(&function.body);
                let body_ty = body_item.ty.instantiate(&mut self.ctx);

                let input_var = self.function_inputs.pop().unwrap();

                let return_ty =
                    mem::replace(&mut self.return_ty, previous_return_ty).unwrap_or(body_ty);

                let ty = function_type(Type::Variable(input_var), return_ty);

                // Only generalize pure functions
                let mut pure = true;
                body_item.traverse(|item| {
                    if let ItemKind::Variable(item) = &item.kind {
                        if function.captures.contains(&item.variable) {
                            pure = false;
                        }
                    }
                });

                Item::function(
                    item.info,
                    if pure {
                        Scheme::ForAll(ty.generalize(&self.ctx))
                    } else {
                        Scheme::Type(ty)
                    },
                    body_item,
                    function.captures.clone(),
                )
            }
            compile::ItemKind::FunctionInput(_) => {
                let var = self.ctx.new_variable(None);

                self.function_inputs.push(var.clone());
                Item::function_input(item.info, Scheme::Type(Type::Variable(var)))
            }
            compile::ItemKind::External(external) => {
                let inputs = external
                    .inputs
                    .iter()
                    .map(|item| self.typecheck_item(item))
                    .collect();

                Item::external(
                    item.info,
                    Scheme::Type(Type::Variable(self.ctx.new_variable(None))),
                    external.namespace,
                    external.identifier,
                    inputs,
                )
            }
            compile::ItemKind::Annotate(annotate) => {
                let ty = self.convert_constructor(&annotate.constructor);

                let inferred_item = self.typecheck_item(&annotate.item);

                let inferred_ty = inferred_item.ty.instantiate(&mut self.ctx);

                if let Err(errors) = self.ctx.unify(ty, inferred_ty) {
                    self.report_type_errors(&annotate.item, errors);
                    self.well_typed = false;
                    return Item::error(item.info);
                }

                inferred_item
            }
            compile::ItemKind::DataDecl(decl) => {
                let field_tys = decl
                    .fields
                    .iter()
                    .map(|(&name, field)| (name, self.convert_constructor(field)))
                    .collect();

                self.data_decls.insert(decl.id, (item.info, field_tys));

                Item::unit(item.info)
            }
            compile::ItemKind::Data(data) => {
                let (data_decl_info, field_tys) = self.data_decls.get(&data.id).unwrap().clone();
                let mut field_tys = field_tys.into_iter();

                let fields = data
                    .fields
                    .iter()
                    .map(|field| {
                        let (_, expected_ty) = field_tys.next().unwrap();

                        let item = self.typecheck_item(field);
                        let ty = item.ty.instantiate(&mut self.ctx);

                        if let Err(errors) = self.ctx.unify(expected_ty, ty) {
                            self.report_type_errors(field, errors);
                        }

                        item
                    })
                    .collect();

                let ty = Scheme::Type(Type::Constructed {
                    id: TypeName::with_id(
                        data.id,
                        data_decl_info.declared_name,
                        TypeNameFormat::Default,
                    ),
                    // TODO: Generics
                    // NOTE: This represents the applied generic arguments (ie.
                    // concrete types, not parameters)
                    params: Vec::new(),
                    bottom: false,
                });

                Item::data(item.info, ty, data.id, fields)
            }
            compile::ItemKind::Loop(r#loop) => {
                let previous_end_ty = mem::take(&mut self.end_ty);

                let body_item = self.typecheck_item(&r#loop.body);

                let end_ty = mem::replace(&mut self.end_ty, previous_end_ty)
                    .unwrap_or_else(|| BUILTIN_TYPES.never.clone());

                Item::r#loop(item.info, Scheme::Type(end_ty), body_item)
            }
            compile::ItemKind::End(end) => {
                let value_item = self.typecheck_item(&end.value);
                let ty = value_item.ty.instantiate(&mut self.ctx);

                match self.end_ty.as_ref() {
                    Some(end_ty) => {
                        if let Err(errors) = self.ctx.unify(end_ty.clone(), ty) {
                            self.report_type_errors(&end.value, errors);
                            self.well_typed = false;
                            return Item::error(item.info);
                        }
                    }
                    None => self.end_ty = Some(ty),
                }

                Item::end(
                    item.info,
                    Scheme::Type(BUILTIN_TYPES.unit.clone()),
                    value_item,
                )
            }
            compile::ItemKind::Return(r#return) => {
                let value_item = self.typecheck_item(&r#return.value);
                let ty = value_item.ty.instantiate(&mut self.ctx);

                match self.return_ty.as_mut() {
                    Some(return_ty) => {
                        if let Err(errors) = self.ctx.unify(return_ty.clone(), ty) {
                            self.report_type_errors(&r#return.value, errors);
                            self.well_typed = false;
                            return Item::error(item.info);
                        }
                    }
                    None => self.return_ty = Some(ty),
                }

                Item::r#return(
                    item.info,
                    Scheme::Type(BUILTIN_TYPES.unit.clone()),
                    value_item,
                )
            }
            compile::ItemKind::Field(field) => {
                let value_item = self.typecheck_item(&field.value);
                let value_ty = value_item.ty.instantiate(&mut self.ctx);

                let value_ty_id = match value_ty {
                    Type::Variable(_) => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Cannot access field on this",
                            vec![Note::primary(
                                field.value.info.span,
                                "Could not determine the type of this value",
                            )],
                        ));

                        self.well_typed = false;
                        return Item::error(item.info);
                    }
                    Type::Constructed { id, .. } => id.id,
                };

                let decl = match self.data_decls.get(&value_ty_id) {
                    Some((_, decl)) => decl,
                    None => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Cannot access field on this",
                            vec![Note::primary(
                                field.value.info.span,
                                "Value is not a data structure",
                            )],
                        ));

                        self.well_typed = false;
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

                        self.well_typed = false;
                        return Item::error(item.info);
                    }
                };

                Item::field(item.info, Scheme::Type(ty.clone()), value_item, index)
            }
        }
    }

    fn typecheck_block(
        &mut self,
        compile_info: compile::ItemInfo,
        statements: &[compile::Item],
    ) -> Item {
        let statements = statements
            .iter()
            .map(|statement| self.typecheck_item(statement))
            .collect::<Vec<_>>();

        let ty = statements
            .last()
            .map(|statement| statement.ty.clone())
            .unwrap_or_else(|| Scheme::Type(BUILTIN_TYPES.unit.clone()));

        Item::block(compile_info, ty, statements)
    }

    fn report_type_errors(&mut self, item: &compile::Item, errors: Vec<UnificationError>) {
        for error in errors {
            let diagnostic = match error {
                UnificationError::Recursive(_) => Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Recursive type",
                    vec![Note::primary(
                        item.info.span,
                        "The type of this references itself",
                    )],
                ),
                UnificationError::Mismatch(found, expected) => Diagnostic::new(
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
    }

    fn convert_constructor(&mut self, constructor: &compile::Constructor) -> Type {
        use compile::Constructor;

        match constructor {
            Constructor::Placeholder => Type::Variable(self.ctx.new_variable(None)),
            Constructor::Parameter(var) => Type::Variable(var.clone()),
            Constructor::Never => BUILTIN_TYPES.never.clone(),
            Constructor::Number => BUILTIN_TYPES.number.clone(),
            Constructor::Text => BUILTIN_TYPES.text.clone(),
            Constructor::Unit => BUILTIN_TYPES.unit.clone(),
            Constructor::Function { input, output } => function_type(
                self.convert_constructor(input),
                self.convert_constructor(output),
            ),
            Constructor::DataStruct { id, .. } => Type::Constructed {
                id: TypeName::with_id(*id, None::<String>, TypeNameFormat::Default),
                params: Vec::new(),
                bottom: false,
            },
        }
    }
}
