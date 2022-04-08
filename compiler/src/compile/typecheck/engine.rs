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
    Trait(TraitId),
    Named(TypeId),
    Function(Box<UnresolvedType>, Box<UnresolvedType>),
    Builtin(BuiltinType),
    Bottom,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnresolvedForAll {
    pub params: HashSet<TypeParameterId>,
    pub ty: UnresolvedType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedType {
    pub instance: Option<ConstantId>,
    pub kind: ResolvedTypeKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResolvedTypeKind {
    Variable(TypeVariable),
    Parameter(TypeParameterId),
    Named(TypeId),
    Function(Box<ResolvedType>, Box<ResolvedType>),
    Builtin(BuiltinType),
    Bottom,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Scheme {
    Type(Type),
    ForAll(ForAll),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Parameter(TypeParameterId),
    Named(TypeId),
    Function(Box<Type>, Box<Type>),
    Builtin(BuiltinType),
    Bottom,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForAll {
    pub params: HashSet<TypeParameterId>,
    pub ty: Type,
}

impl From<Scheme> for UnresolvedScheme {
    fn from(scheme: Scheme) -> Self {
        match scheme {
            Scheme::Type(ty) => UnresolvedScheme::Type(ty.into()),
            Scheme::ForAll(forall) => UnresolvedScheme::ForAll(UnresolvedForAll {
                params: forall.params,
                ty: forall.ty.into(),
            }),
        }
    }
}

impl From<ResolvedType> for UnresolvedType {
    fn from(ty: ResolvedType) -> Self {
        match ty.kind {
            ResolvedTypeKind::Variable(var) => UnresolvedType::Variable(var),
            ResolvedTypeKind::Parameter(param) => UnresolvedType::Parameter(param),
            ResolvedTypeKind::Named(name) => UnresolvedType::Named(name),
            ResolvedTypeKind::Function(input, output) => {
                UnresolvedType::Function(Box::new((*input).into()), Box::new((*output).into()))
            }
            ResolvedTypeKind::Builtin(builtin) => UnresolvedType::Builtin(builtin),
            ResolvedTypeKind::Bottom => UnresolvedType::Bottom,
        }
    }
}

impl From<Type> for UnresolvedType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Parameter(param) => UnresolvedType::Parameter(param),
            Type::Named(name) => UnresolvedType::Named(name),
            Type::Function(input, output) => {
                UnresolvedType::Function(Box::new((*input).into()), Box::new((*output).into()))
            }
            Type::Builtin(builtin) => UnresolvedType::Builtin(builtin),
            Type::Bottom => UnresolvedType::Bottom,
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
    MissingInstance(TraitId, UnresolvedType),
}

#[derive(Debug, Clone)]
pub enum FinalizeError {
    UnknownType,
    UnresolvedTrait(TraitId),
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
    ) -> Result<ResolvedType, UnificationError> {
        actual.apply(self);
        expected.apply(self);

        match (actual, expected) {
            (UnresolvedType::Variable(var), ty) | (ty, UnresolvedType::Variable(var)) => {
                if ty.contains(&var) {
                    Err(UnificationError::Recursive(var))
                } else {
                    self.substitutions.insert(var, ty);

                    Ok(ResolvedType {
                        instance: None,
                        kind: ResolvedTypeKind::Variable(var),
                    })
                }
            }
            (UnresolvedType::Parameter(param), _) => Ok(ResolvedType {
                instance: None,
                kind: ResolvedTypeKind::Parameter(param),
            }),
            (actual, expected @ UnresolvedType::Parameter(_)) => {
                Err(UnificationError::Mismatch(actual, expected))
            }
            (UnresolvedType::Trait(tr), ty) | (ty, UnresolvedType::Trait(tr)) => self
                .instances
                .get(&tr)
                .cloned()
                .and_then(|instances| {
                    instances.iter().find_map(|instance| {
                        let mut ctx = self.clone();

                        let instance_ty = instance.scheme.clone().instantiate(&mut ctx);

                        match ctx.unify(ty.clone(), instance_ty) {
                            Ok(ResolvedType { kind, .. }) => {
                                *self = ctx;

                                Some(ResolvedType {
                                    instance: Some(instance.constant),
                                    kind,
                                })
                            }
                            Err(_) => None,
                        }
                    })
                })
                .ok_or(UnificationError::MissingInstance(tr, ty)),
            (UnresolvedType::Named(actual_id), UnresolvedType::Named(expected_id)) => {
                if actual_id == expected_id {
                    Ok(ResolvedType {
                        instance: None,
                        kind: ResolvedTypeKind::Named(actual_id),
                    })
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
                let input = match self.unify((*actual_input).clone(), (*expected_input).clone()) {
                    Ok(input) => input,
                    Err(_) => {
                        return Err(UnificationError::Mismatch(
                            UnresolvedType::Function(actual_input, actual_output),
                            UnresolvedType::Function(expected_input, expected_output),
                        ))
                    }
                };

                let output = match self.unify((*actual_output).clone(), (*expected_output).clone())
                {
                    Ok(output) => output,
                    Err(_) => {
                        return Err(UnificationError::Mismatch(
                            UnresolvedType::Function(actual_input, actual_output),
                            UnresolvedType::Function(expected_input, expected_output),
                        ))
                    }
                };

                Ok(ResolvedType {
                    instance: None,
                    kind: ResolvedTypeKind::Function(Box::new(input), Box::new(output)),
                })
            }
            (UnresolvedType::Builtin(actual), UnresolvedType::Builtin(expected)) => {
                if actual == expected {
                    Ok(ResolvedType {
                        instance: None,
                        kind: ResolvedTypeKind::Builtin(actual),
                    })
                } else {
                    Err(UnificationError::Mismatch(
                        UnresolvedType::Builtin(actual),
                        UnresolvedType::Builtin(expected),
                    ))
                }
            }
            (UnresolvedType::Bottom, _) => Ok(ResolvedType {
                instance: None,
                kind: ResolvedTypeKind::Bottom,
            }),
            (actual, expected) => Err(UnificationError::Mismatch(actual, expected)),
        }
    }
}

impl UnresolvedScheme {
    pub fn apply(&mut self, ctx: &Context) {
        match self {
            UnresolvedScheme::Type(ty) => ty.apply(ctx),
            UnresolvedScheme::ForAll(forall) => forall.apply(ctx),
        }
    }

    pub fn instantiate(&self, ctx: &mut Context) -> UnresolvedType {
        match self {
            UnresolvedScheme::Type(ty) => ty.clone(),
            UnresolvedScheme::ForAll(forall) => {
                let mut substitutions = HashMap::new();

                for param in &forall.params {
                    substitutions.insert(param, UnresolvedType::Variable(ctx.new_variable()));
                }

                fn apply(
                    ty: &mut UnresolvedType,
                    substitutions: &HashMap<&TypeParameterId, UnresolvedType>,
                ) {
                    match ty {
                        UnresolvedType::Parameter(param) => {
                            if let Some(substitution) = substitutions.get(param) {
                                *ty = substitution.clone();
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
                apply(&mut ty, &substitutions);

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

    pub fn finalize(mut self, ctx: &Context) -> Result<Scheme, FinalizeError> {
        self.apply(ctx);

        Ok(match self {
            UnresolvedScheme::Type(ty) => Scheme::Type(ty.finalize(ctx)?),
            UnresolvedScheme::ForAll(forall) => Scheme::ForAll(forall.finalize(ctx)?),
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

    pub fn generalize(
        mut self,
        ctx: &Context,
        params: &HashSet<TypeParameterId>,
    ) -> UnresolvedScheme {
        self.apply(ctx);

        let vars = self
            .params()
            .intersection(params)
            .cloned()
            .collect::<HashSet<_>>();

        if vars.is_empty() {
            UnresolvedScheme::Type(self)
        } else {
            UnresolvedScheme::ForAll(UnresolvedForAll {
                params: vars,
                ty: self,
            })
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

    pub fn finalize(mut self, ctx: &Context) -> Result<Type, FinalizeError> {
        self.apply(ctx);

        match self {
            UnresolvedType::Variable(_) => Err(FinalizeError::UnknownType),
            UnresolvedType::Parameter(param) => Ok(Type::Parameter(param)),
            UnresolvedType::Trait(tr) => Err(FinalizeError::UnresolvedTrait(tr)),
            UnresolvedType::Named(name) => Ok(Type::Named(name)),
            UnresolvedType::Function(input, output) => Ok(Type::Function(
                Box::new(input.finalize(ctx)?),
                Box::new(output.finalize(ctx)?),
            )),
            UnresolvedType::Builtin(builtin) => Ok(Type::Builtin(builtin)),
            UnresolvedType::Bottom => Ok(Type::Bottom),
        }
    }
}

impl UnresolvedForAll {
    pub fn apply(&mut self, ctx: &Context) {
        self.ty.apply(ctx);
    }

    pub fn finalize(self, ctx: &Context) -> Result<ForAll, FinalizeError> {
        Ok(ForAll {
            params: self.params,
            ty: self.ty.finalize(ctx)?,
        })
    }
}
