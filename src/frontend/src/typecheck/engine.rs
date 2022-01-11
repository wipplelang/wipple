use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
};

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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeVariable {
    pub index: u32,
}

impl PartialEq for TypeVariable {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for TypeVariable {}

impl Hash for TypeVariable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

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
        let var = TypeVariable {
            index: self.next_var,
        };

        self.next_var += 1;

        var
    }

    pub fn unify(
        &mut self,
        lhs: &Type<Id>,
        rhs: &Type<Id>,
    ) -> Result<(), Vec<UnificationError<Id>>> {
        let lhs = lhs.clone().apply(self);
        let rhs = rhs.clone().apply(self);

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

                for (lhs, rhs) in lhs_params.iter().zip(rhs_params.iter()) {
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
    pub fn instantiate(&self, ctx: &mut Context<Id>) -> Type<Id> {
        match self {
            Scheme::Type(ty) => ty.clone(),
            Scheme::ForAll(forall) => forall.instantiate(ctx),
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

    pub fn apply(&self, ctx: &Context<Id>) -> Self {
        match self {
            Type::Variable(var) => {
                if let Some(ty) = ctx.substitutions.get(var) {
                    ty.clone().apply(ctx)
                } else {
                    Type::Variable(var.clone())
                }
            }
            Type::Constructed { id, params, bottom } => Type::Constructed {
                id: id.clone(),
                params: params.iter().map(|ty| ty.apply(ctx)).collect(),
                bottom: *bottom,
            },
        }
    }

    pub fn apply_mut(&mut self, ctx: &Context<Id>) {
        match self {
            Type::Variable(var) => {
                if let Some(ty) = ctx.substitutions.get(var) {
                    *self = ty.clone();
                    self.apply_mut(ctx);
                }
            }
            Type::Constructed { params, .. } => {
                for param in params {
                    param.apply_mut(ctx);
                }
            }
        }
    }

    pub fn generalize(self) -> ForAll<Id> {
        ForAll {
            vars: self.vars(),
            ty: self,
        }
    }

    fn vars(&self) -> HashSet<TypeVariable> {
        match self {
            Type::Variable(var) => {
                let mut vars = HashSet::with_capacity(1);
                vars.insert(var.clone());
                vars
            }
            Type::Constructed { params, .. } => params.iter().flat_map(Self::vars).collect(),
        }
    }
}

impl<Id: Clone + Eq> ForAll<Id> {
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
