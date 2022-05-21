#![allow(clippy::single_match)]

use crate::{GenericConstantId, TraitId, TypeId, TypeParameterId};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

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
    pub params: BTreeSet<TypeParameterId>,
    pub ty: UnresolvedType,
}

pub type GenericSubstitutions = BTreeMap<TypeParameterId, UnresolvedType>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Parameter(TypeParameterId),
    Named(TypeId),
    Function(Box<Type>, Box<Type>),
    Builtin(BuiltinType),
    Bottom(BottomTypeReason),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForAll {
    pub params: BTreeSet<TypeParameterId>,
    pub ty: Type,
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

#[derive(Debug, Clone, Default)]
pub struct Context {
    pub next_var: usize,
    pub substitutions: BTreeMap<TypeVariable, UnresolvedType>,
}

#[derive(Debug, Clone)]
pub enum TypeError {
    ErrorExpression,
    Recursive(TypeVariable),
    Mismatch(UnresolvedType, UnresolvedType),
    MissingInstance(TraitId, UnresolvedType),
    AmbiguousTrait(TraitId, Vec<GenericConstantId>),
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
            (ty, UnresolvedType::Variable(var)) | (UnresolvedType::Variable(var), ty) => {
                if let UnresolvedType::Variable(other) = ty {
                    if var == other {
                        return Ok(());
                    }
                }

                if ty.contains(&var) {
                    Err(TypeError::Recursive(var))
                } else {
                    self.substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (
                UnresolvedType::Parameter(actual_param),
                UnresolvedType::Parameter(expected_param),
            ) if generic => {
                if actual_param == expected_param {
                    Ok(())
                } else {
                    Err(TypeError::Mismatch(
                        UnresolvedType::Parameter(actual_param),
                        UnresolvedType::Parameter(expected_param),
                    ))
                }
            }
            (_, UnresolvedType::Parameter(_)) if !generic => Ok(()),
            (UnresolvedType::Parameter(actual), expected) if !generic => Err(TypeError::Mismatch(
                UnresolvedType::Parameter(actual),
                expected,
            )),
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
                self.unify_internal((*actual_input).clone(), (*expected_input).clone(), generic)?;

                self.unify_internal(
                    (*actual_output).clone(),
                    (*expected_output).clone(),
                    generic,
                )?;

                Ok(())
            }
            (
                UnresolvedType::Builtin(actual_builtin),
                UnresolvedType::Builtin(expected_builtin),
            ) => {
                if actual_builtin == expected_builtin {
                    Ok(())
                } else {
                    Err(TypeError::Mismatch(
                        UnresolvedType::Builtin(actual_builtin),
                        UnresolvedType::Builtin(expected_builtin),
                    ))
                }
            }
            (UnresolvedType::Bottom(_), _) => Ok(()),
            (actual, expected) => Err(TypeError::Mismatch(actual, expected)),
        }
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

    pub fn instantiate_with(&mut self, substitutions: &GenericSubstitutions) {
        match self {
            UnresolvedType::Parameter(param) => {
                *self = substitutions
                    .get(param)
                    .unwrap_or_else(|| panic!("missing parameter {:?} in substitution", param))
                    .clone();
            }
            UnresolvedType::Function(input, output) => {
                input.instantiate_with(substitutions);
                output.instantiate_with(substitutions);
            }
            _ => {}
        }
    }

    pub fn vars(&self) -> BTreeSet<TypeVariable> {
        match self {
            UnresolvedType::Variable(var) => BTreeSet::from([*var]),
            UnresolvedType::Function(input, output) => {
                let mut vars = input.vars();
                vars.extend(output.vars());
                vars
            }
            _ => BTreeSet::new(),
        }
    }

    pub fn params(&self) -> BTreeSet<TypeParameterId> {
        match self {
            UnresolvedType::Parameter(param) => BTreeSet::from([*param]),
            UnresolvedType::Function(input, output) => {
                let mut params = input.params();
                params.extend(output.params());
                params
            }
            _ => BTreeSet::new(),
        }
    }

    pub fn finalize(mut self, ctx: &Context, generic: bool) -> Option<Type> {
        self.apply(ctx);

        Some(match self {
            UnresolvedType::Variable(_) => return None,
            UnresolvedType::Parameter(param) => {
                if generic {
                    Type::Parameter(param)
                } else {
                    panic!("cannot finalize type parameter in non-generic context")
                }
            }
            UnresolvedType::Named(name) => Type::Named(name),
            UnresolvedType::Function(input, output) => Type::Function(
                Box::new(input.finalize(ctx, generic)?),
                Box::new(output.finalize(ctx, generic)?),
            ),
            UnresolvedType::Builtin(builtin) => Type::Builtin(builtin),
            UnresolvedType::Bottom(is_error) => Type::Bottom(is_error),
        })
    }
}
