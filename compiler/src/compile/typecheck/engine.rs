use crate::{ConstantId, TraitId, TypeId, TypeParameterId};
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnresolvedScheme {
    Type(UnresolvedType),
    ForAll(UnresolvedForAll),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnresolvedType {
    Variable(TypeVariable),
    Parameter(TypeParameterId),
    Named(TypeId),
    Function(Box<UnresolvedType>, Box<UnresolvedType>),
    Builtin(BuiltinType),
    Bottom(bool),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnresolvedForAll {
    pub params: HashSet<TypeParameterId>,
    pub ty: UnresolvedType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Named(TypeId),
    Function(Box<Type>, Box<Type>),
    Builtin(BuiltinType),
    Bottom(bool),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForAll {
    pub params: HashSet<TypeParameterId>,
    pub ty: Type,
}

impl From<Type> for UnresolvedType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Named(name) => UnresolvedType::Named(name),
            Type::Function(input, output) => {
                UnresolvedType::Function(Box::new((*input).into()), Box::new((*output).into()))
            }
            Type::Builtin(builtin) => UnresolvedType::Builtin(builtin),
            Type::Bottom(is_error) => UnresolvedType::Bottom(is_error),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVariable(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinType {
    Unit,
    Text,
    Number,
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub scheme: UnresolvedScheme,
    pub constant: ConstantId,
}

#[derive(Debug, Clone, Default)]
pub struct Context {
    next_var: usize,
    substitutions: HashMap<TypeVariable, UnresolvedType>,
    instances: HashMap<TraitId, Vec<Instance>>,
}

#[derive(Debug, Clone)]
pub enum UnificationError {
    Recursive(TypeVariable),
    Mismatch(UnresolvedType, UnresolvedType),
}

#[derive(Debug, Clone)]
pub enum FinalizeError {
    UnknownType,
    MissingInstance(TraitId, UnresolvedType),
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

    pub fn register(&mut self, tr: TraitId, instance: Instance) {
        self.instances.entry(tr).or_default().push(instance);
    }

    pub fn unify(
        &mut self,
        mut actual: UnresolvedType,
        mut expected: UnresolvedType,
    ) -> Result<(), UnificationError> {
        actual.apply(self);
        expected.apply(self);

        match (actual, expected) {
            (UnresolvedType::Variable(var), ty) | (ty, UnresolvedType::Variable(var)) => {
                if ty.contains(&var) {
                    Err(UnificationError::Recursive(var))
                } else {
                    // eprintln!("unifying {:?} with {:?}", var, ty);
                    self.substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (UnresolvedType::Parameter(_), _) => Ok(()),
            (actual, expected @ UnresolvedType::Parameter(_)) => {
                Err(UnificationError::Mismatch(actual, expected))
            }
            (UnresolvedType::Named(actual_id), UnresolvedType::Named(expected_id)) => {
                if actual_id == expected_id {
                    Ok(())
                } else {
                    Err(UnificationError::Mismatch(
                        UnresolvedType::Named(actual_id),
                        UnresolvedType::Named(expected_id),
                    ))
                }
            }
            (
                UnresolvedType::Function(actual_input, actual_output),
                UnresolvedType::Function(expected_input, expected_output),
            ) => {
                if self
                    .unify((*actual_input).clone(), (*expected_input).clone())
                    .is_err()
                {
                    return Err(UnificationError::Mismatch(
                        UnresolvedType::Function(actual_input, actual_output),
                        UnresolvedType::Function(expected_input, expected_output),
                    ));
                }

                if self
                    .unify((*actual_output).clone(), (*expected_output).clone())
                    .is_err()
                {
                    return Err(UnificationError::Mismatch(
                        UnresolvedType::Function(actual_input, actual_output),
                        UnresolvedType::Function(expected_input, expected_output),
                    ));
                }

                Ok(())
            }
            (UnresolvedType::Builtin(actual), UnresolvedType::Builtin(expected)) => {
                if actual == expected {
                    Ok(())
                } else {
                    Err(UnificationError::Mismatch(
                        UnresolvedType::Builtin(actual),
                        UnresolvedType::Builtin(expected),
                    ))
                }
            }
            (UnresolvedType::Bottom(_), _) => Ok(()),
            (actual, expected) => Err(UnificationError::Mismatch(actual, expected)),
        }
    }

    pub fn instance_for(
        &mut self,
        tr: TraitId,
        mut ty: UnresolvedType,
    ) -> Result<ConstantId, FinalizeError> {
        ty.apply(self);

        let instances = self.instances.get(&tr).cloned().unwrap_or_default();

        let mut suitable_instances = Vec::new();

        for instance in instances {
            let mut ctx = self.clone();

            let scheme = instance.scheme.clone();
            let instance_ty = scheme.instantiate(&mut ctx);

            if ctx.unify(ty.clone(), instance_ty).is_ok() {
                suitable_instances.push((ctx, instance.constant));
            }
        }

        match suitable_instances.len() {
            0 => Err(FinalizeError::MissingInstance(tr, ty)),
            1 => {
                let (ctx, instance) = suitable_instances.pop().unwrap();
                *self = ctx;
                Ok(instance)
            }
            _ => Err(FinalizeError::UnknownType),
        }
    }

    pub fn create_instantiation(
        &mut self,
        scheme: &UnresolvedScheme,
    ) -> HashMap<TypeParameterId, TypeVariable> {
        let mut substitutions = HashMap::new();

        if let UnresolvedScheme::ForAll(forall) = scheme {
            for param in &forall.params {
                substitutions.insert(*param, self.new_variable());
            }
        }

        substitutions
    }
}

impl UnresolvedScheme {
    pub fn apply(&mut self, ctx: &Context) {
        match self {
            UnresolvedScheme::Type(ty) => ty.apply(ctx),
            UnresolvedScheme::ForAll(forall) => forall.ty.apply(ctx),
        }
    }

    pub fn instantiate(&self, ctx: &mut Context) -> UnresolvedType {
        let substitutions = ctx.create_instantiation(self);
        self.instantiate_with(&substitutions)
    }

    pub fn instantiate_with(
        &self,
        substitutions: &HashMap<TypeParameterId, TypeVariable>,
    ) -> UnresolvedType {
        match self {
            UnresolvedScheme::Type(ty) => ty.clone(),
            UnresolvedScheme::ForAll(forall) => {
                fn apply(
                    ty: &mut UnresolvedType,
                    substitutions: &HashMap<TypeParameterId, TypeVariable>,
                ) {
                    match ty {
                        UnresolvedType::Parameter(param) => {
                            if let Some(substitution) = substitutions.get(param) {
                                *ty = UnresolvedType::Variable(*substitution);
                            }
                        }
                        UnresolvedType::Function(input, output) => {
                            apply(input, substitutions);
                            apply(output, substitutions);
                        }
                        _ => {}
                    }
                }

                let mut ty = forall.ty.clone();
                apply(&mut ty, substitutions);

                ty
            }
        }
    }

    pub fn vars(&self) -> HashSet<TypeVariable> {
        match self {
            UnresolvedScheme::Type(ty) => ty.vars(),
            UnresolvedScheme::ForAll(forall) => forall.ty.vars(),
        }
    }

    pub fn params(&self) -> Cow<HashSet<TypeParameterId>> {
        match self {
            UnresolvedScheme::Type(ty) => Cow::Owned(ty.params()),
            UnresolvedScheme::ForAll(forall) => Cow::Borrowed(&forall.params),
        }
    }

    pub fn finalize(
        mut self,
        ctx: &mut Context,
        substitutions: &HashMap<TypeParameterId, TypeVariable>,
    ) -> Result<Type, FinalizeError> {
        self.apply(ctx);

        Ok(match self {
            UnresolvedScheme::Type(ty) => ty.finalize(ctx, substitutions)?,
            UnresolvedScheme::ForAll(forall) => forall.ty.finalize(ctx, substitutions)?,
        })
    }
}

impl UnresolvedType {
    pub fn id(&self) -> Option<TypeId> {
        match self {
            UnresolvedType::Named(id) => Some(*id),
            _ => None,
        }
    }

    pub fn contains(&self, var: &TypeVariable) -> bool {
        match self {
            UnresolvedType::Variable(v) => v == var,
            UnresolvedType::Function(input, output) => input.contains(var) || output.contains(var),
            _ => false,
        }
    }

    pub fn contains_error(&self) -> bool {
        match self {
            UnresolvedType::Function(input, output) => {
                input.contains_error() || output.contains_error()
            }
            UnresolvedType::Bottom(is_error) => *is_error,
            _ => false,
        }
    }

    pub fn apply(&mut self, ctx: &Context) {
        match self {
            UnresolvedType::Variable(var) => {
                if let Some(ty) = ctx.substitutions.get(var) {
                    *self = ty.clone();
                    self.apply(ctx);
                }
            }
            UnresolvedType::Function(input, output) => {
                input.apply(ctx);
                output.apply(ctx);
            }
            _ => {}
        }
    }

    pub fn vars(&self) -> HashSet<TypeVariable> {
        match self {
            UnresolvedType::Variable(var) => {
                let mut vars = HashSet::with_capacity(1);
                vars.insert(*var);
                vars
            }
            UnresolvedType::Function(input, output) => {
                let mut vars = input.vars();
                vars.extend(output.vars());
                vars
            }
            _ => HashSet::new(),
        }
    }

    pub fn params(&self) -> HashSet<TypeParameterId> {
        match self {
            UnresolvedType::Parameter(params) => {
                let mut vars = HashSet::with_capacity(1);
                vars.insert(*params);
                vars
            }
            UnresolvedType::Function(input, output) => {
                let mut params = input.params();
                params.extend(output.params());
                params
            }
            _ => HashSet::new(),
        }
    }

    pub fn finalize(
        mut self,
        ctx: &Context,
        substitutions: &HashMap<TypeParameterId, TypeVariable>,
    ) -> Result<Type, FinalizeError> {
        self.apply(ctx);

        match self {
            UnresolvedType::Variable(_) => Err(FinalizeError::UnknownType),
            UnresolvedType::Parameter(param) => substitutions
                .get(&param)
                .cloned()
                .and_then(|var| {
                    ctx.substitutions
                        .get(&var)
                        .cloned()
                        .map(|ty| ty.finalize(ctx, substitutions))
                })
                .ok_or(FinalizeError::UnknownType)
                .and_then(std::convert::identity),
            UnresolvedType::Named(name) => Ok(Type::Named(name)),
            UnresolvedType::Function(input, output) => Ok(Type::Function(
                Box::new(input.finalize(ctx, substitutions)?),
                Box::new(output.finalize(ctx, substitutions)?),
            )),
            UnresolvedType::Builtin(builtin) => Ok(Type::Builtin(builtin)),
            UnresolvedType::Bottom(is_error) => Ok(Type::Bottom(is_error)),
        }
    }
}
