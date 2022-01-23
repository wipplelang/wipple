use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct Context<Id> {
    substitutions: HashMap<TypeVariable, Type<Id>>,
    next_var: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Scheme<Id> {
    Type(Type<Id>),
    ForAll(ForAll<Id>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type<Id> {
    Variable(TypeVariable),
    Constructed {
        id: Id,
        params: Vec<Type<Id>>,
        bottom: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVariable(u32);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForAll<Id> {
    pub vars: HashSet<TypeVariable>,
    pub ty: Type<Id>,
}

#[derive(Debug, Clone)]
pub enum UnificationError<Id> {
    Recursive(TypeVariable),
    Mismatch(Type<Id>, Type<Id>),
}

impl<Id> Default for Context<Id> {
    fn default() -> Self {
        Context {
            next_var: 0,
            substitutions: HashMap::new(),
        }
    }
}

impl<Id: Clone + Eq> Context<Id> {
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

    pub fn unify(
        &mut self,
        mut lhs: Type<Id>,
        mut rhs: Type<Id>,
    ) -> Result<(), Vec<UnificationError<Id>>> {
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
            (
                Type::Constructed {
                    id: lhs_id,
                    params: lhs_params,
                    bottom: lhs_is_bottom,
                },
                Type::Constructed {
                    id: rhs_id,
                    params: rhs_params,
                    bottom: rhs_is_bottom,
                },
            ) => {
                if lhs_is_bottom {
                    return Ok(());
                }

                if lhs_id != rhs_id {
                    return Err(vec![UnificationError::Mismatch(
                        Type::Constructed {
                            id: lhs_id,
                            params: lhs_params,
                            bottom: lhs_is_bottom,
                        },
                        Type::Constructed {
                            id: rhs_id,
                            params: rhs_params,
                            bottom: rhs_is_bottom,
                        },
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
        }
    }
}

impl<Id: Clone + Eq> Scheme<Id> {
    pub fn apply(&mut self, ctx: &Context<Id>) {
        match self {
            Scheme::Type(ty) => ty.apply(ctx),
            Scheme::ForAll(forall) => forall.apply(ctx),
        }
    }

    pub fn instantiate(&self, ctx: &mut Context<Id>) -> Type<Id> {
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

impl<Id: Clone + Eq> Type<Id> {
    pub fn contains(&self, var: &TypeVariable) -> bool {
        match self {
            Type::Variable(v) => v == var,
            Type::Constructed { params, .. } => params.iter().any(|ty| ty.contains(var)),
        }
    }

    pub fn apply(&mut self, ctx: &Context<Id>) {
        match self {
            Type::Variable(var) => {
                if let Some(ty) = ctx.substitutions.get(var) {
                    *self = ty.clone();
                    self.apply(ctx);
                }
            }
            Type::Constructed { params, .. } => {
                for param in params {
                    param.apply(ctx);
                }
            }
        }
    }

    pub fn generalize(mut self, ctx: &Context<Id>, ignore: &HashSet<TypeVariable>) -> Scheme<Id> {
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
            Type::Constructed { params, .. } => params.iter().flat_map(Self::vars).collect(),
        }
    }
}

impl<Id: Clone + Eq> ForAll<Id> {
    pub fn apply(&mut self, ctx: &Context<Id>) {
        fn apply<Id: Clone + Eq>(
            ty: &mut Type<Id>,
            ctx: &Context<Id>,
            vars: &HashSet<TypeVariable>,
        ) {
            match ty {
                Type::Variable(var) => {
                    if !vars.contains(var) {
                        if let Some(ty2) = ctx.substitutions.get(var) {
                            *ty = ty2.clone();
                            apply(ty, ctx, vars);
                        }
                    }
                }
                Type::Constructed { params, .. } => {
                    for param in params {
                        apply(param, ctx, vars);
                    }
                }
            }
        }

        apply(&mut self.ty, ctx, &self.vars)
    }

    pub fn instantiate(&self, ctx: &mut Context<Id>) -> Type<Id> {
        let mut substitutions = HashMap::new();

        for var in &self.vars {
            substitutions.insert(var, Type::Variable(ctx.new_variable()));
        }

        fn apply<Id: Clone>(ty: &mut Type<Id>, substitutions: &HashMap<&TypeVariable, Type<Id>>) {
            match ty {
                Type::Variable(var) => {
                    if let Some(substitution) = substitutions.get(var) {
                        *ty = substitution.clone();
                    }
                }
                Type::Constructed { params, .. } => {
                    for param in params {
                        apply(param, substitutions);
                    }
                }
            }
        }

        let mut ty = self.ty.clone();
        apply(&mut ty, &substitutions);

        ty
    }
}
