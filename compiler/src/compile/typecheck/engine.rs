use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use crate::helpers::TypeId;

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
    Named(TypeId, Vec<Type>),
    Function(Box<Type>, Box<Type>),
    Bottom,
}

impl Type {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVariable(usize);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForAll {
    pub vars: HashSet<TypeVariable>,
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

    pub fn vars(&self) -> HashSet<TypeVariable> {
        (0..self.next_var).map(TypeVariable).collect()
    }

    pub fn unify(&mut self, mut lhs: Type, mut rhs: Type) -> Result<(), Vec<UnificationError>> {
        lhs.apply(self);
        rhs.apply(self);

        match (lhs, rhs) {
            (Type::Variable(var), ty) | (ty, Type::Variable(var)) => {
                if ty.contains(&var) {
                    Err(vec![UnificationError::Recursive(var)])
                } else {
                    self.substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (Type::Named(lhs_id, lhs_params), Type::Named(rhs_id, rhs_params)) => {
                if lhs_id != rhs_id {
                    return Err(vec![UnificationError::Mismatch(
                        Type::Named(lhs_id, lhs_params),
                        Type::Named(rhs_id, rhs_params),
                    )]);
                }

                let mut errors = Vec::new();

                for (lhs, rhs) in lhs_params.into_iter().zip(rhs_params) {
                    if let Err(mut param_errors) = self.unify(lhs, rhs) {
                        errors.append(&mut param_errors);
                    }
                }

                if errors.is_empty() {
                    Ok(())
                } else {
                    Err(errors)
                }
            }
            (Type::Function(lhs_input, lhs_output), Type::Function(rhs_input, rhs_output)) => {
                let mut errors = Vec::new();

                if let Err(mut input_errors) = self.unify(*lhs_input, *rhs_input) {
                    errors.append(&mut input_errors);
                }

                if let Err(mut output_errors) = self.unify(*lhs_output, *rhs_output) {
                    errors.append(&mut output_errors);
                }

                if errors.is_empty() {
                    Ok(())
                } else {
                    Err(errors)
                }
            }
            (Type::Bottom, _) => Ok(()),
            (lhs, rhs) => Err(vec![UnificationError::Mismatch(lhs, rhs)]),
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
            Scheme::ForAll(forall) => forall.instantiate(ctx),
        }
    }

    pub fn vars(&self) -> HashSet<TypeVariable> {
        match self {
            Scheme::Type(ty) => ty.vars(),
            Scheme::ForAll(forall) => forall.ty.vars().difference(&forall.vars).cloned().collect(),
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
            Type::Bottom => false,
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
            Type::Bottom => {}
        }
    }

    pub fn generalize(mut self, ctx: &Context, ignore: &HashSet<TypeVariable>) -> Scheme {
        self.apply(ctx);

        let vars = self
            .vars()
            .difference(ignore)
            .cloned()
            .collect::<HashSet<_>>();

        dbg!(&vars);

        if vars.is_empty() {
            Scheme::Type(self)
        } else {
            Scheme::ForAll(ForAll { vars, ty: self })
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
            Type::Bottom => HashSet::new(),
        }
    }
}

impl ForAll {
    pub fn apply(&mut self, ctx: &Context) {
        fn apply(ty: &mut Type, ctx: &Context, vars: &HashSet<TypeVariable>) {
            match ty {
                Type::Variable(var) => {
                    if !vars.contains(var) {
                        if let Some(ty2) = ctx.substitutions.get(var) {
                            *ty = ty2.clone();
                            apply(ty, ctx, vars);
                        }
                    }
                }
                Type::Named(_, params) => {
                    for param in params {
                        apply(param, ctx, vars);
                    }
                }
                Type::Function(input, output) => {
                    apply(input, ctx, vars);
                    apply(output, ctx, vars);
                }
                Type::Bottom => {}
            }
        }

        apply(&mut self.ty, ctx, &self.vars)
    }

    pub fn instantiate(&self, ctx: &mut Context) -> Type {
        let mut substitutions = HashMap::new();

        for var in &self.vars {
            substitutions.insert(var, Type::Variable(ctx.new_variable()));
        }

        fn apply(ty: &mut Type, substitutions: &HashMap<&TypeVariable, Type>) {
            match ty {
                Type::Variable(var) => {
                    if let Some(substitution) = substitutions.get(var) {
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
                Type::Bottom => {}
            }
        }

        let mut ty = self.ty.clone();
        apply(&mut ty, &substitutions);

        ty
    }
}
