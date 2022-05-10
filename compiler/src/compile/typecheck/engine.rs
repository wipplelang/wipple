#![allow(clippy::single_match)]

use crate::{ConstantId, TraitId, TypeId, TypeParameterId};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnresolvedType {
    Variable(TypeVariable),
    Parameter(TypeParameterId),
    Named(TypeId),
    Function(Box<UnresolvedType>, Box<UnresolvedType>),
    Builtin(BuiltinType),
    Bottom(BottomTypeReason),
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
    Bottom(BottomTypeReason),
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
            Type::Bottom(reason) => UnresolvedType::Bottom(reason),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVariable(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinType {
    Unit,
    Text,
    Number,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum BottomTypeReason {
    Annotated,
    Error,
    Internal,
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub ty: UnresolvedType,
    pub constant: ConstantId,
    pub from_bound: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Context {
    pub next_var: usize,
    pub substitutions: HashMap<TypeVariable, UnresolvedType>,
    pub instances: HashMap<TraitId, Vec<Instance>>,
}

#[derive(Debug, Clone)]
pub enum TypeError {
    Recursive(TypeVariable),
    Mismatch(UnresolvedType, UnresolvedType),
    MissingInstance(TraitId, UnresolvedType),
    AmbiguousTrait(TraitId, Vec<(ConstantId, UnresolvedType)>),
    UnresolvedType,
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
        instance.ty.apply(self);
        self.instances.entry(tr).or_default().push(instance);
    }

    pub fn unify(
        &mut self,
        actual: UnresolvedType,
        expected: UnresolvedType,
    ) -> Result<(), TypeError> {
        self.unify_internal(actual, expected, false)
    }

    pub fn unify_generic(
        &mut self,
        actual: UnresolvedType,
        expected: UnresolvedType,
    ) -> Result<(), TypeError> {
        self.unify_internal(actual, expected, true)
    }

    fn unify_internal(
        &mut self,
        mut actual: UnresolvedType,
        mut expected: UnresolvedType,
        generic: bool,
    ) -> Result<(), TypeError> {
        actual.apply(self);
        expected.apply(self);

        match (actual, expected) {
            (UnresolvedType::Variable(var), ty) | (ty, UnresolvedType::Variable(var)) => {
                if ty.contains(&var) {
                    Err(TypeError::Recursive(var))
                } else {
                    self.substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (UnresolvedType::Parameter(actual), UnresolvedType::Parameter(expected)) if generic => {
                if actual == expected {
                    Ok(())
                } else {
                    Err(TypeError::Mismatch(
                        UnresolvedType::Parameter(actual),
                        UnresolvedType::Parameter(expected),
                    ))
                }
            }
            (UnresolvedType::Parameter(_), _) if !generic => Ok(()),
            (actual, expected @ UnresolvedType::Parameter(_)) if !generic => {
                Err(TypeError::Mismatch(actual, expected))
            }
            (UnresolvedType::Named(actual_id), UnresolvedType::Named(expected_id)) => {
                if actual_id == expected_id {
                    Ok(())
                } else {
                    Err(TypeError::Mismatch(
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
                    .unify_internal((*actual_input).clone(), (*expected_input).clone(), generic)
                    .is_err()
                {
                    return Err(TypeError::Mismatch(
                        UnresolvedType::Function(actual_input, actual_output),
                        UnresolvedType::Function(expected_input, expected_output),
                    ));
                };

                if self
                    .unify_internal(
                        (*actual_output).clone(),
                        (*expected_output).clone(),
                        generic,
                    )
                    .is_err()
                {
                    return Err(TypeError::Mismatch(
                        UnresolvedType::Function(actual_input, actual_output),
                        UnresolvedType::Function(expected_input, expected_output),
                    ));
                };

                Ok(())
            }
            (UnresolvedType::Builtin(actual), UnresolvedType::Builtin(expected)) => {
                if actual == expected {
                    Ok(())
                } else {
                    Err(TypeError::Mismatch(
                        UnresolvedType::Builtin(actual),
                        UnresolvedType::Builtin(expected),
                    ))
                }
            }
            (UnresolvedType::Bottom(_), _) => Ok(()),
            (actual, expected) => Err(TypeError::Mismatch(actual, expected)),
        }
    }

    pub fn instance_for(
        &mut self,
        tr: TraitId,
        mut ty: UnresolvedType,
    ) -> Result<ConstantId, TypeError> {
        ty.apply(self);

        let instances = self.instances.get(&tr).cloned().unwrap_or_default();

        let mut bound_instances = Vec::new();
        let mut defined_instances = Vec::new();
        for instance in instances {
            let mut ctx = self.clone();

            // TODO: Support generic instances
            instance.ty.clone().instantiate(self);

            if ctx.unify_generic(ty.clone(), instance.ty.clone()).is_ok() {
                let info = (ctx, instance.constant, instance.ty);

                if instance.from_bound {
                    bound_instances.push(info);
                } else {
                    defined_instances.push(info);
                }
            }
        }

        macro_rules! find_instance {
            ($instances:ident) => {
                match &$instances.len() {
                    0 => {}
                    1 => {
                        let (ctx, instance, _) = $instances.pop().unwrap();
                        *self = ctx;
                        return Ok(instance);
                    }
                    _ => {
                        return Err(TypeError::AmbiguousTrait(
                            tr,
                            $instances
                                .into_iter()
                                .map(|(_, instance, ty)| (instance, ty))
                                .collect(),
                        ))
                    }
                };
            };
        }

        find_instance!(bound_instances);
        find_instance!(defined_instances);

        Err(TypeError::MissingInstance(tr, ty))
    }

    pub fn create_instantiation(
        &mut self,
        ty: &mut UnresolvedType,
    ) -> HashMap<TypeParameterId, UnresolvedType> {
        ty.apply(self);

        let mut substitutions = HashMap::new();

        fn create(
            ctx: &mut Context,
            ty: &mut UnresolvedType,
            substitutions: &mut HashMap<TypeParameterId, UnresolvedType>,
        ) {
            if let UnresolvedType::Parameter(param) = ty {
                substitutions
                    .entry(*param)
                    .or_insert_with(|| UnresolvedType::Variable(ctx.new_variable()));
            }

            match ty {
                UnresolvedType::Function(input, output) => {
                    create(ctx, input, substitutions);
                    create(ctx, output, substitutions);
                }
                _ => {}
            }
        }

        create(self, ty, &mut substitutions);

        substitutions
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
            UnresolvedType::Bottom(BottomTypeReason::Error) => true,
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

    pub fn instantiate(&mut self, ctx: &mut Context) {
        let substitutions = ctx.create_instantiation(self);
        self.instantiate_with(&substitutions);
    }

    pub fn instantiate_with(&mut self, substitutions: &HashMap<TypeParameterId, UnresolvedType>) {
        match self {
            UnresolvedType::Parameter(param) => {
                if let Some(ty) = substitutions.get(param) {
                    *self = ty.clone();
                }
            }
            UnresolvedType::Function(input, output) => {
                input.instantiate_with(substitutions);
                output.instantiate_with(substitutions);
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

    pub fn finalize(mut self, ctx: &Context) -> Option<Type> {
        self.apply(ctx);

        match self {
            UnresolvedType::Variable(_) => None,
            UnresolvedType::Parameter(_) => panic!("cannot finalize generic type"),
            UnresolvedType::Named(name) => Some(Type::Named(name)),
            UnresolvedType::Function(input, output) => Some(Type::Function(
                Box::new(input.finalize(ctx)?),
                Box::new(output.finalize(ctx)?),
            )),
            UnresolvedType::Builtin(builtin) => Some(Type::Builtin(builtin)),
            UnresolvedType::Bottom(is_error) => Some(Type::Bottom(is_error)),
        }
    }
}
