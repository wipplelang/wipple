mod item;
mod ty;

pub use item::*;
pub use ty::*;

use crate::{id::*, lower};
use serde::Serialize;
use std::{collections::HashMap, sync::Arc};
use wipple_diagnostics::*;

#[derive(Debug, Clone, Serialize)]
pub struct File {
    pub id: FileId,
    pub statements: Vec<Item>,
}

pub fn typecheck(files: &[Arc<lower::File>], diagnostics: &mut Diagnostics) -> Option<Vec<File>> {
    let mut info = Info::new(diagnostics);

    files
        .iter()
        .map(|file| {
            let mut typed_statements = Some(Vec::with_capacity(file.statements.len()));
            for statement in &file.statements {
                match typecheck_item(statement, &mut info) {
                    Some(typed_statement) => {
                        if let Some(typed_statements) = typed_statements.as_mut() {
                            typed_statements.push(typed_statement);
                        }
                    }
                    None => typed_statements = None,
                }
            }

            Some(File {
                id: file.id,
                statements: typed_statements?,
            })
        })
        .collect()
}

struct Info<'a> {
    diagnostics: &'a mut Diagnostics,
    variables: HashMap<VariableId, Ty>,
    function_input_ty: Option<Ty>,
}

impl<'a> Info<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Info {
            diagnostics,
            variables: Default::default(),
            function_input_ty: None,
        }
    }
}

macro_rules! info {
    ($item:expr, $ty:expr) => {
        ItemInfo::new($item.debug_info.clone(), $ty)
    };
}

#[must_use]
fn typecheck_item(item: &lower::Item, info: &mut Info) -> Option<Item> {
    match &item.kind {
        lower::ItemKind::Unit => Some(Item::unit(info!(item, Ty::unit()))),
        lower::ItemKind::Number { value } => Some(Item::number(info!(item, Ty::number()), *value)),
        lower::ItemKind::Text { value } => Some(Item::text(info!(item, Ty::text()), *value)),
        lower::ItemKind::Block { statements } => {
            // Typecheck each statement; the type of the last statement is the
            // type of the entire block
            let mut last_ty = Ty::unit();
            let mut typed_statements = Some(Vec::with_capacity(statements.len()));
            for statement in statements {
                match typecheck_item(statement, info) {
                    Some(statement) => {
                        if let Some(typed_statements) = typed_statements.as_mut() {
                            last_ty = statement.info.ty.clone();
                            typed_statements.push(statement);
                        }
                    }
                    None => typed_statements = None,
                }
            }

            Some(Item::block(info!(item, last_ty), typed_statements?))
        }
        lower::ItemKind::Apply { function, input } => {
            // Typecheck the input
            let input = typecheck_item(input, info);

            // Typecheck the function
            let function = typecheck_item(function, info)?;

            // Ensure the function has a function type, retrieving the types of
            // its input and body
            let input_ty = Ty::unknown();
            let body_ty = Ty::unknown();
            Ty::function(input_ty.clone(), body_ty.clone()).unify(
                &function.info.ty,
                function.info.info.span,
                info,
            )?;

            let mut input = input?;

            // Ensure the type of the input matches the type of the function's
            // input, including generics
            let substituted_generics =
                input.info.ty.unify(&input_ty, input.info.info.span, info)?;

            // If 'A -> A' has been converted to 'X -> A', convert it further to
            // 'X -> X'
            body_ty.substitute_generics(&substituted_generics);

            Some(Item::apply(
                info!(item, body_ty),
                Box::new(function),
                Box::new(input),
            ))
        }
        lower::ItemKind::Initialize {
            binding_info,
            variable,
            value,
        } => {
            // Typecheck the value
            let value = typecheck_item(value, info)?;

            // Track the type of the variable
            info.variables.insert(*variable, value.info.ty.clone());

            Some(Item::initialize(
                info!(item, Ty::unit()),
                ItemInfo::new(binding_info.clone(), value.info.ty.clone()),
                *variable,
                Box::new(value),
            ))
        }
        lower::ItemKind::Variable { variable } => {
            let ty = info.variables.get(variable).unwrap().clone();
            Some(Item::variable(info!(item, ty), *variable))
        }
        lower::ItemKind::Function { body, captures } => {
            // Track the type of the function's input within the body
            let function_input_ty = Ty::unknown();
            info.function_input_ty = Some(function_input_ty.clone());

            // Typecheck the function body
            let body = typecheck_item(body, info)?;

            // Convert '_ -> X' to 'for A -> A -> X'
            function_input_ty.make_generic();

            Some(Item::function(
                info!(item, Ty::function(function_input_ty, body.info.ty.clone())),
                Box::new(body),
                captures.clone(),
            ))
        }
        lower::ItemKind::FunctionInput => {
            let ty = info.function_input_ty.as_ref().unwrap().clone();
            Some(Item::function_input(info!(item, ty)))
        }
        lower::ItemKind::External {
            namespace,
            identifier,
        } => {
            let ty = Ty::unknown();
            Some(Item::external(info!(item, ty), *namespace, *identifier))
        }
        lower::ItemKind::Annotate { item, ty } => {
            let mut item = typecheck_item(item, info)?;

            let ty = ty.clone();
            let substituted_generics = item.info.ty.unify(&ty, item.info.info.span, info)?;
            ty.substitute_generics(&substituted_generics);
            item.info.ty = ty;

            Some(item)
        }
    }
}

type SubstitutedGenerics = HashMap<(), TyKind>; // TODO: HashMap<GenericId, TyKind>

impl Ty {
    #[must_use]
    fn unify(&mut self, known: &Ty, span: Span, info: &mut Info) -> Option<SubstitutedGenerics> {
        let mut substituted_generics = SubstitutedGenerics::new();

        macro_rules! get {
            ($x:expr) => {
                $x.borrow_mut()
            };
        }

        if matches!(*get!(self.kind), TyKind::Unknown)
            && matches!(*get!(known.kind), TyKind::Unknown)
        {
            // TODO: Check bounds
            self.kind = known.kind.clone();
            return Some(substituted_generics);
        }

        let mut kind = get!(self.kind);
        let mut known_kind = get!(known.kind);

        macro_rules! error {
            () => {{
                drop(kind);
                drop(known_kind);

                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Mismatched types",
                    vec![Note::primary(
                        span,
                        format!("Expected {}, found {}", self, known),
                    )],
                ));

                return None;
            }};
        }

        match &mut *known_kind {
            TyKind::Unknown => error!(),
            TyKind::Generic => match &mut *kind {
                TyKind::Unknown => *kind = TyKind::Generic,
                TyKind::Generic => {} // TODO: Check ID
                // TODO: Check bounds
                _ => {
                    substituted_generics.insert((), kind.clone());
                }
            },
            TyKind::Unit => match &mut *kind {
                TyKind::Unknown => *kind = TyKind::Unit, // TODO: Check bounds
                TyKind::Unit => {}
                _ => error!(),
            },
            TyKind::Number => match &mut *kind {
                TyKind::Unknown => *kind = TyKind::Number, // TODO: Check bounds
                TyKind::Number => {}
                _ => error!(),
            },
            TyKind::Text => match &mut *kind {
                TyKind::Unknown => *kind = TyKind::Text, // TODO: Check bounds
                TyKind::Text => {}
                _ => error!(),
            },
            TyKind::Function {
                input: known_input,
                body: known_body,
            } => match &mut *kind {
                // TODO: Check bounds
                TyKind::Unknown => {
                    let mut input = Ty::unknown();
                    let mut body = Ty::unknown();

                    input.unify(known_input, span, info)?;
                    body.unify(known_body, span, info)?;

                    *kind = TyKind::Function { input, body };
                }
                TyKind::Function { input, body } => {
                    input.unify(known_input, span, info)?;
                    body.unify(known_body, span, info)?;
                }
                _ => error!(),
            },
            TyKind::File { path: known_path } => match &mut *kind {
                TyKind::Unknown => {
                    // TODO: Check bounds
                    *kind = TyKind::File { path: *known_path }
                }
                TyKind::File { path, .. } if path == known_path => {}
                _ => error!(),
            },
        }

        Some(substituted_generics)
    }

    fn make_generic(&self) {
        let kind = &mut *self.kind.borrow_mut();

        if matches!(kind, TyKind::Unknown) {
            *kind = TyKind::Generic
        }
    }

    fn substitute_generics(&self, substituted_generics: &SubstitutedGenerics) {
        let kind = &mut *self.kind.borrow_mut();

        match kind {
            TyKind::Generic => {
                if let Some(substitute) = substituted_generics.get(&()) {
                    *kind = substitute.clone();
                }
            }
            TyKind::Function { input, body } => {
                input.substitute_generics(substituted_generics);
                body.substitute_generics(substituted_generics);
            }
            TyKind::Unknown
            | TyKind::Unit
            | TyKind::Number
            | TyKind::Text
            | TyKind::File { .. } => {}
        }
    }
}
