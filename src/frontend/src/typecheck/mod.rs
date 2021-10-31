mod item;
mod ty;

pub use item::*;
pub use ty::*;

use crate::{id::VariableId, lower};
use std::collections::HashMap;
use wipple_diagnostics::*;

pub fn typecheck(mut item: lower::Item, diagnostics: &mut Diagnostics) -> Option<Item> {
    let mut info = Info::new(diagnostics);
    typecheck_item(&mut item, &mut info)
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

#[must_use]
fn typecheck_item(item: &mut lower::Item, info: &mut Info) -> Option<Item> {
    match &mut item.kind {
        lower::ItemKind::Error => None,
        lower::ItemKind::Unit => Some(Item::unit(item.debug_info, Ty::unit())),
        lower::ItemKind::Number { value } => {
            Some(Item::number(item.debug_info, Ty::number(), *value))
        }
        lower::ItemKind::Text { value } => Some(Item::text(item.debug_info, Ty::text(), *value)),
        lower::ItemKind::Block { statements } => {
            // Typecheck each statement; the type of the last statement is the
            // type of the entire block
            let mut last_ty = Ty::unit();
            let mut typed_statements = Some(Vec::with_capacity(statements.len()));
            for statement in statements {
                match typecheck_item(statement, info) {
                    Some(statement) => {
                        if let Some(typed_statements) = typed_statements.as_mut() {
                            last_ty = statement.ty.clone();
                            typed_statements.push(statement);
                        }
                    }
                    None => typed_statements = None,
                }
            }

            Some(Item::block(item.debug_info, last_ty, typed_statements?))
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
                &function.ty,
                function.debug_info.span,
                info,
            )?;

            let mut input = input?;

            // Ensure the type of the input matches the type of the function's
            // input, including generics
            let substituted_generics = input.ty.unify(&input_ty, input.debug_info.span, info)?;

            // If 'A -> A' has been converted to 'X -> A', convert it further to
            // 'X -> X'
            body_ty.substitute_generics(&substituted_generics);

            Some(Item::apply(
                item.debug_info,
                body_ty,
                Box::new(function),
                Box::new(input),
            ))
        }
        lower::ItemKind::Initialize { variable, value } => {
            // Typecheck the value
            let value = typecheck_item(value, info)?;

            // Track the type of the variable
            info.variables.insert(*variable, value.ty.clone());

            Some(Item::initialize(
                item.debug_info,
                Ty::unit(),
                *variable,
                Box::new(value),
            ))
        }
        lower::ItemKind::Variable { variable } => {
            let ty = info.variables.get(variable).unwrap().clone();
            Some(Item::variable(item.debug_info, ty, *variable))
        }
        lower::ItemKind::Function {
            input_debug_info,
            body,
            captures,
        } => {
            // Track the type of the function's input within the body
            let function_input_ty = Ty::unknown();
            info.function_input_ty = Some(function_input_ty.clone());

            // Typecheck the function body
            let body = typecheck_item(body, info)?;

            // Convert '_ -> X' to 'for A -> A -> X'
            function_input_ty.make_generic();

            Some(Item::function(
                item.debug_info,
                Ty::function(function_input_ty, body.ty.clone()),
                *input_debug_info,
                Box::new(body),
                captures.clone(),
            ))
        }
        lower::ItemKind::FunctionInput => {
            let ty = info.function_input_ty.as_ref().unwrap().clone();
            Some(Item::function_input(item.debug_info, ty))
        }
        lower::ItemKind::External { .. } => todo!(),
        lower::ItemKind::Annotate { item, ty } => {
            let mut item = typecheck_item(item, info)?;

            let substituted_generics = ty.unify(&item.ty, item.debug_info.span, info)?;
            ty.substitute_generics(&substituted_generics);
            item.ty = ty.clone();

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
            TyKind::Unknown | TyKind::Unit | TyKind::Number | TyKind::Text => {}
        }
    }
}
