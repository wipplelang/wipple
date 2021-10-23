use crate::{id::*, lower::*};
use serde::Serialize;
use std::{
    collections::{HashMap, VecDeque},
    fmt, mem,
    ops::Add,
};
use wipple_diagnostics::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Type {
    pub value_span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum TypeKind {
    Error,
    Unit, // eventually, more general tuple type
    Number,
    Text,
    Variable(TypeVariableId),
    Function(Box<Type>, Box<Type>),
}

impl Type {
    fn new(value_span: Span, kind: TypeKind) -> Self {
        Type { value_span, kind }
    }

    fn error(value_span: Span) -> Self {
        Type::new(value_span, TypeKind::Error)
    }

    fn unit(value_span: Span) -> Self {
        Type::new(value_span, TypeKind::Unit)
    }

    fn number(value_span: Span) -> Self {
        Type::new(value_span, TypeKind::Number)
    }

    fn text(value_span: Span) -> Self {
        Type::new(value_span, TypeKind::Text)
    }

    pub(crate) fn variable(value_span: Span) -> Self {
        Type::new(value_span, TypeKind::Variable(TypeVariableId::new()))
    }

    fn function(value_span: Span, input: Type, body: Type) -> Self {
        Type::new(
            value_span,
            TypeKind::Function(Box::new(input), Box::new(body)),
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TypeKind::Error => write!(f, "_"),
            TypeKind::Unit => write!(f, "."),
            TypeKind::Number => write!(f, "Number"),
            TypeKind::Text => write!(f, "Text"),
            TypeKind::Variable(_) => write!(f, "_"),
            TypeKind::Function(input, body) => write!(f, "{} -> {}", input, body),
        }
    }
}

pub fn typecheck(item: Item, diagnostics: &mut Diagnostics) -> Result<Item, Item> {
    let mut info = Info {
        variables: Default::default(),
        function_input_ty: None,
    };

    resolve(item, &mut info, diagnostics)
}

struct Info {
    variables: HashMap<VariableId, Type>,
    function_input_ty: Option<Type>,
}

#[derive(Debug, Serialize)]
struct MapSet<T, U>(VecDeque<(T, U)>);

impl<T, U> MapSet<T, U> {
    pub fn new(left: T, right: U) -> Self {
        MapSet(vec![(left, right)].into())
    }

    pub fn empty() -> Self {
        MapSet(VecDeque::new())
    }
}

impl<T, U> Add for MapSet<T, U> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        MapSet(self.0.into_iter().chain(rhs.0).collect())
    }
}

impl<T, U> FromIterator<(T, U)> for MapSet<T, U> {
    fn from_iter<It: IntoIterator<Item = (T, U)>>(iter: It) -> Self {
        MapSet(iter.into_iter().collect())
    }
}

impl<T, U> FromIterator<MapSet<T, U>> for MapSet<T, U> {
    fn from_iter<It: IntoIterator<Item = MapSet<T, U>>>(iter: It) -> Self {
        MapSet(iter.into_iter().flat_map(|c| c.0).collect())
    }
}

type Constraints = MapSet<Type, Type>;
type Constraint = (Type, Type);

type Substitutions = MapSet<TypeVariableId, Type>;
type Substitution = (TypeVariableId, Type);

fn constraints(item: &Item, info: &mut Info, diagnostics: &mut Diagnostics) -> Constraints {
    let ty = item.ty.clone();

    match &item.kind {
        ItemKind::Error => Constraints::new(ty, Type::error(item.span)),
        ItemKind::Unit(_) => Constraints::new(ty, Type::unit(item.span)),
        ItemKind::Constant(constant_item) => match constant_item.kind {
            ConstantItemKind::Number(_) => Constraints::new(ty, Type::number(item.span)),
            ConstantItemKind::Text(_) => Constraints::new(ty, Type::text(item.span)),
        },
        ItemKind::Block(block_item) => {
            let statements = block_item
                .statements
                .iter()
                .map(|item| constraints(item, info, diagnostics))
                .collect::<Constraints>();

            let last_ty = block_item
                .statements
                .last()
                .map(|item| item.ty.clone())
                .unwrap_or_else(|| Type::unit(item.span));

            statements + Constraints::new(ty, last_ty)
        }
        ItemKind::Apply(apply_item) => {
            constraints(&apply_item.function, info, diagnostics)
                + constraints(&apply_item.value, info, diagnostics)
                + Constraints::new(
                    apply_item.function.ty.clone(),
                    Type::function(item.span, apply_item.value.ty.clone(), ty),
                )
        }
        ItemKind::Initialize(initialize_item) => {
            let variable_ty = Type::variable(item.span);

            info.variables
                .insert(initialize_item.variable, variable_ty.clone());

            constraints(&initialize_item.value, info, diagnostics)
                + Constraints::new(ty, Type::unit(item.span))
                + Constraints::new(variable_ty, initialize_item.value.ty.clone())
        }
        ItemKind::Variable(variable_item) => Constraints::new(
            ty,
            info.variables.get(&variable_item.variable).unwrap().clone(),
        ),
        ItemKind::Function(function_item) => {
            let function_input_ty = Type::variable(function_item.input_span);
            info.function_input_ty = Some(function_input_ty.clone());

            constraints(&function_item.body, info, diagnostics)
                + Constraints::new(
                    ty,
                    Type::function(item.span, function_input_ty, function_item.body.ty.clone()),
                )
        }
        ItemKind::FunctionInput(_) => {
            Constraints::new(ty, info.function_input_ty.as_ref().unwrap().clone())
        }
        ItemKind::External(_) => todo!(),
    }
}

fn substitute(ty: Type, substitution: &Substitution) -> Type {
    match ty.kind {
        TypeKind::Error | TypeKind::Unit | TypeKind::Number | TypeKind::Text => ty,
        TypeKind::Variable(var) => {
            if var == substitution.0 {
                substitution.1.clone()
            } else {
                ty
            }
        }
        TypeKind::Function(input, value) => Type::function(
            ty.value_span,
            substitute(*input, substitution),
            substitute(*value, substitution),
        ),
    }
}

fn apply(ty: Type, substitutions: &Substitutions) -> Type {
    substitutions.0.iter().fold(ty, substitute)
}

fn unify_one(constraint: &Constraint, diagnostics: &mut Diagnostics) -> Option<Substitutions> {
    match (&constraint.0.kind, &constraint.1.kind) {
        (TypeKind::Error, _) => None,
        (TypeKind::Unit, TypeKind::Unit)
        | (TypeKind::Number, TypeKind::Number)
        | (TypeKind::Text, TypeKind::Text) => Some(Substitutions::empty()),
        (TypeKind::Function(input_a, body_a), TypeKind::Function(input_b, body_b)) => unify(
            Constraints::new(*input_a.clone(), *input_b.clone())
                + Constraints::new(*body_a.clone(), *body_b.clone()),
            diagnostics,
        ),
        (TypeKind::Variable(var), _) => Some(Substitutions::new(*var, constraint.1.clone())),
        (_, TypeKind::Variable(var)) => Some(Substitutions::new(*var, constraint.0.clone())),
        _ => {
            diagnostics.add(Diagnostic::new(
                DiagnosticLevel::Error,
                "Mismatched types",
                vec![
                    Note::primary(
                        constraint.0.value_span,
                        format!("Found {} here", constraint.0),
                    ),
                    Note::primary(
                        constraint.1.value_span,
                        format!("Found {} here", constraint.1),
                    ),
                ],
            ));

            None
        }
    }
}

fn unify(mut constraints: Constraints, diagnostics: &mut Diagnostics) -> Option<Substitutions> {
    if let Some(head) = constraints.0.pop_front() {
        let head = unify_one(&head, diagnostics)?;

        let tail = constraints
            .0
            .into_iter()
            .map(|(left, right)| {
                let apply = |ty| apply(ty, &head);
                (apply(left), apply(right))
            })
            .collect();

        Some(head + unify(tail, diagnostics)?)
    } else {
        Some(Substitutions::empty())
    }
}

fn resolve(mut root: Item, info: &mut Info, diagnostics: &mut Diagnostics) -> Result<Item, Item> {
    let constraints = constraints(&root, info, diagnostics);
    let substitutions = match unify(constraints, diagnostics) {
        Some(substitutions) => substitutions,
        None => return Err(root),
    };

    fn resolve(item: &mut Item, substitutions: &Substitutions) {
        let ty = mem::replace(&mut item.ty, Type::error(item.span));
        item.ty = apply(ty, substitutions);

        match &mut item.kind {
            ItemKind::Error
            | ItemKind::Unit(_)
            | ItemKind::Constant(_)
            | ItemKind::Variable(_)
            | ItemKind::FunctionInput(_)
            | ItemKind::External(_) => {}
            ItemKind::Block(item) => {
                for statement in &mut item.statements {
                    resolve(statement, substitutions)
                }
            }
            ItemKind::Apply(item) => {
                resolve(&mut item.function, substitutions);
                resolve(&mut item.value, substitutions);
            }
            ItemKind::Initialize(item) => resolve(&mut item.value, substitutions),
            ItemKind::Function(item) => resolve(&mut item.body, substitutions),
        }
    }

    resolve(&mut root, &substitutions);

    Ok(root)
}
