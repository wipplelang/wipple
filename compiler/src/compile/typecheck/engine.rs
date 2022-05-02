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
    Bottom(bool),
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
    Bottom(bool),
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
            ResolvedTypeKind::Bottom(is_error) => UnresolvedType::Bottom(is_error),
        }
    }
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
    MissingInstance(TraitId, UnresolvedType),
    AmbiguousTrait(TraitId, Vec<UnresolvedType>),
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

    pub fn register(&mut self, tr: TraitId, mut instance: Instance) {
        instance.scheme.as_ty_mut().apply(self);
        self.instances.entry(tr).or_default().push(instance);
    }

    pub fn unify(
        &mut self,
        actual: UnresolvedType,
        expected: UnresolvedType,
    ) -> Result<ResolvedType, UnificationError> {
        self.unify_internal(actual, expected, false)
    }

    pub fn unify_generic(
        &mut self,
        actual: UnresolvedType,
        expected: UnresolvedType,
    ) -> Result<ResolvedType, UnificationError> {
        self.unify_internal(actual, expected, true)
    }

    fn unify_internal(
        &mut self,
        mut actual: UnresolvedType,
        mut expected: UnresolvedType,
        generic: bool,
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
            (UnresolvedType::Parameter(actual), UnresolvedType::Parameter(expected)) if generic => {
                if actual == expected {
                    Ok(ResolvedType {
                        instance: None,
                        kind: ResolvedTypeKind::Parameter(actual),
                    })
                } else {
                    Err(UnificationError::Mismatch(
                        UnresolvedType::Parameter(actual),
                        UnresolvedType::Parameter(expected),
                    ))
                }
            }
            (UnresolvedType::Parameter(param), _) if !generic => Ok(ResolvedType {
                instance: None,
                kind: ResolvedTypeKind::Parameter(param),
            }),
            (actual, expected @ UnresolvedType::Parameter(_)) if !generic => {
                Err(UnificationError::Mismatch(actual, expected))
            }
            (UnresolvedType::Trait(tr), ty) | (ty, UnresolvedType::Trait(tr)) => {
                self.instance_for(tr, ty)
            }
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
            (UnresolvedType::Bottom(is_error), _) => Ok(ResolvedType {
                instance: None,
                kind: ResolvedTypeKind::Bottom(is_error),
            }),
            (actual, expected) => Err(UnificationError::Mismatch(actual, expected)),
        }
    }

    pub fn instance_for(
        &mut self,
        tr: TraitId,
        mut ty: UnresolvedType,
    ) -> Result<ResolvedType, UnificationError> {
        ty.apply(self);

        let instances = self.instances.get(&tr).cloned().unwrap_or_default();

        let mut suitable_instances = Vec::new();
        for instance in instances {
            let mut ctx = self.clone();

            // TODO: Monomorphize generic instances
            let instance_ty = instance.scheme.clone().instantiate(self);

            if let Ok(ResolvedType { kind, .. }) =
                ctx.unify_generic(ty.clone(), instance_ty.clone())
            {
                suitable_instances.push((
                    ctx,
                    ResolvedType {
                        instance: Some(instance.constant),
                        kind,
                    },
                ));
            }
        }

        match suitable_instances.len() {
            0 => Err(UnificationError::MissingInstance(tr, ty)),
            1 => {
                let (ctx, instance) = suitable_instances.pop().unwrap();
                *self = ctx;
                Ok(instance)
            }
            _ => Err(UnificationError::AmbiguousTrait(
                tr,
                suitable_instances
                    .into_iter()
                    .map(|ty| ty.1.into())
                    .collect(),
            )),
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
    pub fn into_ty(self) -> UnresolvedType {
        match self {
            UnresolvedScheme::Type(ty) => ty,
            UnresolvedScheme::ForAll(forall) => forall.ty,
        }
    }

    pub fn as_ty(&self) -> &UnresolvedType {
        match self {
            UnresolvedScheme::Type(ty) => ty,
            UnresolvedScheme::ForAll(forall) => &forall.ty,
        }
    }

    pub fn as_ty_mut(&mut self) -> &mut UnresolvedType {
        match self {
            UnresolvedScheme::Type(ty) => ty,
            UnresolvedScheme::ForAll(forall) => &mut forall.ty,
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
        self,
        ctx: &mut Context,
        substitutions: &HashMap<TypeParameterId, TypeVariable>,
    ) -> Option<Type> {
        Some(match self {
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

    pub fn substitute_parameters(
        &mut self,
        substitutions: &HashMap<TypeParameterId, UnresolvedType>,
    ) {
        match self {
            UnresolvedType::Parameter(param) => {
                if let Some(ty) = substitutions.get(param) {
                    *self = ty.clone();
                }
            }
            UnresolvedType::Function(input, output) => {
                input.substitute_parameters(substitutions);
                output.substitute_parameters(substitutions);
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
    ) -> Option<Type> {
        self.apply(ctx);

        match self {
            UnresolvedType::Variable(_) | UnresolvedType::Trait(_) => None,
            UnresolvedType::Parameter(param) => substitutions
                .get(&param)
                .and_then(|var| UnresolvedType::Variable(*var).finalize(ctx, substitutions)),
            UnresolvedType::Named(name) => Some(Type::Named(name)),
            UnresolvedType::Function(input, output) => Some(Type::Function(
                Box::new(input.finalize(ctx, substitutions)?),
                Box::new(output.finalize(ctx, substitutions)?),
            )),
            UnresolvedType::Builtin(builtin) => Some(Type::Builtin(builtin)),
            UnresolvedType::Bottom(is_error) => Some(Type::Bottom(is_error)),
        }
    }
}
