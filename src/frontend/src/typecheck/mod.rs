use crate::{id::*, lower::*};
use serde::Serialize;
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};
use wipple_diagnostics::*;

#[non_exhaustive]
#[derive(Debug, Clone, Serialize)]
pub struct TypecheckedItem(pub Item);

#[derive(Debug, Clone, Serialize)]
pub struct Ty {
    pub value_span: Span,
    pub kind: Rc<RefCell<TyKind>>,
}

#[derive(Debug, Clone, Serialize)]
pub enum TyKind {
    Unknown, // TODO: Bounds
    Generic, // TODO: Generic ID and bounds
    Unit,    // eventually, more general tuple type
    Number,
    Text,
    Function(Ty, Ty),
}

impl Ty {
    fn new(value_span: Span, kind: TyKind) -> Self {
        Ty {
            value_span,
            kind: Rc::new(RefCell::new(kind)),
        }
    }

    pub(crate) fn unknown(value_span: Span) -> Self {
        Ty::new(value_span, TyKind::Unknown)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.kind.borrow() {
            TyKind::Unknown => write!(f, "_"),
            TyKind::Generic => write!(f, "_"), // TODO
            TyKind::Unit => write!(f, "."),
            TyKind::Number => write!(f, "Number"),
            TyKind::Text => write!(f, "Text"),
            TyKind::Function(input, body) => write!(f, "{} -> {}", input, body),
        }
    }
}

pub fn typecheck(mut item: Item, diagnostics: &mut Diagnostics) -> Option<TypecheckedItem> {
    let mut info = Info::new(diagnostics);
    typecheck_item(&mut item, &mut info)?;
    Some(TypecheckedItem(item))
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
fn typecheck_item(item: &mut Item, info: &mut Info) -> Option<()> {
    match &mut item.kind {
        ItemKind::Error => return None,
        ItemKind::Unit(_) => {
            item.ty.unify(&Ty::new(item.span, TyKind::Unit), info)?;
        }
        ItemKind::Constant(constant_item) => match constant_item.kind {
            ConstantItemKind::Number(_) => {
                item.ty.unify(&Ty::new(item.span, TyKind::Number), info)?;
            }
            ConstantItemKind::Text(_) => {
                item.ty.unify(&Ty::new(item.span, TyKind::Text), info)?;
            }
        },
        ItemKind::Block(block_item) => {
            // Typecheck each statement; the type of the last statement is the
            // type of the entire block
            let mut last_ty = Ty::new(item.span, TyKind::Unit);
            for statement in &mut block_item.statements {
                typecheck_item(statement, info)?;
                last_ty = statement.ty.clone();
            }

            item.ty.unify(&last_ty, info)?;
        }
        ItemKind::Apply(apply_item) => {
            // Typecheck the function
            typecheck_item(&mut apply_item.function, info)?;

            // Ensure the function has a function type, retrieving the types of
            // its input and body
            let input_ty = Ty::new(apply_item.function.span, TyKind::Unknown);
            let body_ty = Ty::new(apply_item.function.span, TyKind::Unknown);
            Ty::new(
                apply_item.function.span,
                TyKind::Function(input_ty.clone(), body_ty.clone()),
            )
            .unify(&apply_item.function.ty, info)?;

            // Typecheck the input
            typecheck_item(&mut apply_item.input, info)?;

            // Ensure the type of the input matches the type of the function's
            // input, including generics
            let substituted_generics = apply_item.input.ty.unify(&input_ty, info)?;

            // If 'A -> A' has been converted to 'X -> A', convert it further to
            // 'X -> X'
            body_ty.substitute_generics(&substituted_generics);

            // Infer the type of the item as the type of the function's body
            item.ty.unify(&body_ty, info)?;
        }
        ItemKind::Initialize(initialize_item) => {
            // Typecheck the value
            typecheck_item(&mut initialize_item.value, info)?;

            // Track the type of the variable
            info.variables
                .insert(initialize_item.variable, initialize_item.value.ty.clone());

            item.ty.unify(&Ty::new(item.span, TyKind::Unit), info)?;
        }
        ItemKind::Variable(variable_item) => {
            item.ty = info.variables.get(&variable_item.variable).unwrap().clone();
        }
        ItemKind::Function(function_item) => {
            // Track the type of the function's input within the body
            debug_assert!(info.function_input_ty.is_none());
            let function_input_ty = Ty::new(function_item.input_span, TyKind::Unknown);
            info.function_input_ty = Some(function_input_ty.clone());

            // Typecheck the function body
            typecheck_item(&mut function_item.body, info)?;

            // Convert '_ -> X' to 'for A -> A -> X'
            function_input_ty.make_generic();

            item.ty.unify(
                &Ty::new(
                    item.span,
                    TyKind::Function(function_input_ty, function_item.body.ty.clone()),
                ),
                info,
            )?;
        }
        ItemKind::FunctionInput(_) => {
            item.ty = info.function_input_ty.as_ref().unwrap().clone();
        }
        ItemKind::External(_) => todo!(),
    }

    Some(())
}

type SubstitutedGenerics = HashMap<(), TyKind>; // TODO: HashMap<GenericId, TyKind>

impl Ty {
    #[must_use]
    fn unify(&mut self, known: &Ty, info: &mut Info) -> Option<SubstitutedGenerics> {
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
                        self.value_span,
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
            TyKind::Function(known_input, known_body) => match &mut *kind {
                // TODO: Check bounds
                TyKind::Unknown => {
                    let mut input = Ty::new(known_input.value_span, TyKind::Unknown);
                    let mut body = Ty::new(known_body.value_span, TyKind::Unknown);

                    input.unify(known_input, info)?;
                    body.unify(known_body, info)?;

                    *kind = TyKind::Function(input, body);
                }
                TyKind::Function(input, body) => {
                    input.unify(known_input, info)?;
                    body.unify(known_body, info)?;
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
            TyKind::Function(input, body) => {
                input.substitute_generics(substituted_generics);
                body.substitute_generics(substituted_generics);
            }
            TyKind::Unknown | TyKind::Unit | TyKind::Number | TyKind::Text => {}
        }
    }
}
