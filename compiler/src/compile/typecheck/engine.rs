use crate::{TypeId, TypeParameterId};
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

#[derive(Debug, Clone, Default)]
pub struct Context {
    substitutions: HashMap<TypeVariable, Type>,
    next_var: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Scheme {
    Type(Type),
    ForAll(ForAll),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Variable(TypeVariable),
    Parameter(TypeParameterId),
    Named(TypeId, Vec<Type>),
    Function(Box<Type>, Box<Type>),
    Builtin(BuiltinType),
    Bottom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVariable(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinType {
    Unit,
    Text,
    Number,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForAll {
    pub params: HashSet<TypeParameterId>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum UnificationError {
    Recursive(TypeVariable),
    Mismatch(Type, Type),
}

impl Context {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_variable(&mut self) -> TypeVariable {
        let var = TypeVariable(self.next_var);
        self.next_var += 1;
        var
    }

    pub fn unify(&mut self, mut actual: Type, mut expected: Type) -> Result<(), UnificationError> {
        actual.apply(self);
        expected.apply(self);

        match (actual, expected) {
            (Type::Variable(var), ty) | (ty, Type::Variable(var)) => {
                if ty.contains(&var) {
                    Err(UnificationError::Recursive(var))
                } else {
                    self.substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (Type::Parameter(_), _) => Ok(()),
            (actual, expected @ Type::Parameter(_)) => {
                Err(UnificationError::Mismatch(actual, expected))
            }
            (Type::Named(actual_id, actual_params), Type::Named(expected_id, expected_params)) => {
                if actual_id != expected_id {
                    return Err(UnificationError::Mismatch(
                        Type::Named(actual_id, actual_params),
                        Type::Named(expected_id, expected_params),
                    ));
                }

                for (actual, expected) in actual_params
                    .clone()
                    .into_iter()
                    .zip(expected_params.clone())
                {
                    if self.unify(actual, expected).is_err() {
                        return Err(UnificationError::Mismatch(
                            Type::Named(actual_id, actual_params),
                            Type::Named(expected_id, expected_params),
                        ));
                    }
                }

                Ok(())
            }
            (
                Type::Function(actual_input, actual_output),
                Type::Function(expected_input, expected_output),
            ) => {
                if self
                    .unify((*actual_input).clone(), (*expected_input).clone())
                    .is_err()
                {
                    return Err(UnificationError::Mismatch(
                        Type::Function(actual_input, actual_output),
                        Type::Function(expected_input, expected_output),
                    ));
                }

                if self
                    .unify((*actual_output).clone(), (*expected_output).clone())
                    .is_err()
                {
                    return Err(UnificationError::Mismatch(
                        Type::Function(actual_input, actual_output),
                        Type::Function(expected_input, expected_output),
                    ));
                }

                Ok(())
            }
            (Type::Builtin(actual), Type::Builtin(expected)) => {
                if actual == expected {
                    Ok(())
                } else {
                    Err(UnificationError::Mismatch(
                        Type::Builtin(actual),
                        Type::Builtin(expected),
                    ))
                }
            }
            (Type::Bottom, _) => Ok(()),
            (actual, expected) => Err(UnificationError::Mismatch(actual, expected)),
        }
    }
}

impl Scheme {
    pub fn apply(&mut self, ctx: &Context) {
        match self {
            Scheme::Type(ty) => ty.apply(ctx),
            Scheme::ForAll(forall) => forall.apply(ctx),
        }
    }

    pub fn instantiate(&self, ctx: &mut Context) -> Type {
        match self {
            Scheme::Type(ty) => ty.clone(),
            Scheme::ForAll(forall) => {
                let mut substitutions = HashMap::new();

                for param in &forall.params {
                    substitutions.insert(param, Type::Variable(ctx.new_variable()));
                }

                fn apply(ty: &mut Type, substitutions: &HashMap<&TypeParameterId, Type>) {
                    match ty {
                        Type::Parameter(param) => {
                            if let Some(substitution) = substitutions.get(param) {
                                *ty = substitution.clone();
                            }
                        }
                        Type::Named(_, params) => {
                            for param in params {
                                apply(param, substitutions);
                            }
                        }
                        Type::Function(input, output) => {
                            apply(input, substitutions);
                            apply(output, substitutions);
                        }
                        Type::Variable(_) | Type::Builtin(_) | Type::Bottom => {}
                    }
                }

                let mut ty = forall.ty.clone();
                apply(&mut ty, &substitutions);

                ty
            }
        }
    }

    pub fn vars(&self) -> HashSet<TypeVariable> {
        match self {
            Scheme::Type(ty) => ty.vars(),
            Scheme::ForAll(forall) => forall.ty.vars(),
        }
    }

    pub fn params(&self) -> Cow<HashSet<TypeParameterId>> {
        match self {
            Scheme::Type(ty) => Cow::Owned(ty.params()),
            Scheme::ForAll(forall) => Cow::Borrowed(&forall.params),
        }
    }
}

impl Type {
    pub fn id(&self) -> Option<TypeId> {
        match self {
            Type::Named(id, _) => Some(*id),
            _ => None,
        }
    }

    pub fn contains(&self, var: &TypeVariable) -> bool {
        match self {
            Type::Variable(v) => v == var,
            Type::Named(_, params) => params.iter().any(|ty| ty.contains(var)),
            Type::Function(input, output) => input.contains(var) || output.contains(var),
            _ => false,
        }
    }

    pub fn apply(&mut self, ctx: &Context) {
        match self {
            Type::Variable(var) => {
                if let Some(ty) = ctx.substitutions.get(var) {
                    *self = ty.clone();
                    self.apply(ctx);
                }
            }
            Type::Named(_, params) => {
                for param in params {
                    param.apply(ctx);
                }
            }
            Type::Function(input, output) => {
                input.apply(ctx);
                output.apply(ctx);
            }
            _ => {}
        }
    }

    pub fn generalize(mut self, ctx: &Context, params: &HashSet<TypeParameterId>) -> Scheme {
        self.apply(ctx);

        let vars = self
            .params()
            .intersection(params)
            .cloned()
            .collect::<HashSet<_>>();

        if vars.is_empty() {
            Scheme::Type(self)
        } else {
            Scheme::ForAll(ForAll {
                params: vars,
                ty: self,
            })
        }
    }

    pub fn vars(&self) -> HashSet<TypeVariable> {
        match self {
            Type::Variable(var) => {
                let mut vars = HashSet::with_capacity(1);
                vars.insert(*var);
                vars
            }
            Type::Named(_, params) => params.iter().flat_map(Self::vars).collect(),
            Type::Function(input, output) => {
                let mut vars = input.vars();
                vars.extend(output.vars());
                vars
            }
            _ => HashSet::new(),
        }
    }

    pub fn params(&self) -> HashSet<TypeParameterId> {
        match self {
            Type::Parameter(params) => {
                let mut vars = HashSet::with_capacity(1);
                vars.insert(*params);
                vars
            }
            Type::Named(_, params) => params.iter().flat_map(Self::params).collect(),
            Type::Function(input, output) => {
                let mut params = input.params();
                params.extend(output.params());
                params
            }
            _ => HashSet::new(),
        }
    }
}

impl ForAll {
    pub fn apply(&mut self, ctx: &Context) {
        self.ty.apply(ctx);
    }
}
