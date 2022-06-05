use crate::{GenericConstantId, TraitId, TypeId, TypeParameterId};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnresolvedType {
    Variable(TypeVariable),
    Parameter(TypeParameterId),
    Named(TypeId, Vec<UnresolvedType>),
    Function(Box<UnresolvedType>, Box<UnresolvedType>),
    Builtin(BuiltinType<Box<UnresolvedType>>),
    Bottom(BottomTypeReason),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Parameter(TypeParameterId),
    Named(TypeId, Vec<Type>),
    Function(Box<Type>, Box<Type>),
    Builtin(BuiltinType<Box<Type>>),
    Bottom(BottomTypeReason),
}

impl From<Type> for UnresolvedType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Parameter(param) => UnresolvedType::Parameter(param),
            Type::Named(id, params) => {
                UnresolvedType::Named(id, params.into_iter().map(|param| param.into()).collect())
            }
            Type::Function(input, output) => {
                UnresolvedType::Function(Box::new((*input).into()), Box::new((*output).into()))
            }
            Type::Builtin(builtin) => UnresolvedType::Builtin(match builtin {
                BuiltinType::Unit => BuiltinType::Unit,
                BuiltinType::Text => BuiltinType::Text,
                BuiltinType::Number => BuiltinType::Number,
                BuiltinType::List(ty) => BuiltinType::List(Box::new((*ty).into())),
            }),
            Type::Bottom(reason) => UnresolvedType::Bottom(reason),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVariable(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinType<Ty> {
    Unit,
    Text,
    Number,
    List(Ty),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum BottomTypeReason {
    Annotated,
    Error,
    Placeholder,
}

pub type GenericSubstitutions = BTreeMap<TypeParameterId, UnresolvedType>;

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
            (
                UnresolvedType::Named(actual_id, actual_params),
                UnresolvedType::Named(expected_id, expected_params),
            ) => {
                if actual_id == expected_id {
                    for (actual, expected) in actual_params.iter().zip(&expected_params) {
                        if let Err(error) =
                            self.unify_internal(actual.clone(), expected.clone(), generic)
                        {
                            return Err(if let TypeError::Mismatch(_, _) = error {
                                TypeError::Mismatch(
                                    UnresolvedType::Named(actual_id, actual_params),
                                    UnresolvedType::Named(expected_id, expected_params),
                                )
                            } else {
                                error
                            });
                        }
                    }

                    Ok(())
                } else {
                    Err(TypeError::Mismatch(
                        UnresolvedType::Named(actual_id, actual_params),
                        UnresolvedType::Named(expected_id, expected_params),
                    ))
                }
            }
            (
                UnresolvedType::Function(actual_input, actual_output),
                UnresolvedType::Function(expected_input, expected_output),
            ) => {
                if let Err(error) =
                    self.unify_internal((*actual_input).clone(), (*expected_input).clone(), generic)
                {
                    return Err(if let TypeError::Mismatch(_, _) = error {
                        TypeError::Mismatch(
                            UnresolvedType::Function(actual_input, actual_output),
                            UnresolvedType::Function(expected_input, expected_output),
                        )
                    } else {
                        error
                    });
                }

                if let Err(error) = self.unify_internal(
                    (*actual_output).clone(),
                    (*expected_output).clone(),
                    generic,
                ) {
                    return Err(if let TypeError::Mismatch(_, _) = error {
                        TypeError::Mismatch(
                            UnresolvedType::Function(actual_input, actual_output),
                            UnresolvedType::Function(expected_input, expected_output),
                        )
                    } else {
                        error
                    });
                }

                Ok(())
            }
            (
                UnresolvedType::Builtin(actual_builtin),
                UnresolvedType::Builtin(expected_builtin),
            ) => match (actual_builtin, expected_builtin) {
                (BuiltinType::Unit, BuiltinType::Unit) => Ok(()),
                (BuiltinType::Text, BuiltinType::Text) => Ok(()),
                (BuiltinType::Number, BuiltinType::Number) => Ok(()),
                (BuiltinType::List(actual_element), BuiltinType::List(expected_element)) => {
                    if let Err(error) = self.unify_internal(
                        (*actual_element).clone(),
                        (*expected_element).clone(),
                        generic,
                    ) {
                        return Err(if let TypeError::Mismatch(_, _) = error {
                            TypeError::Mismatch(
                                UnresolvedType::Builtin(BuiltinType::List(actual_element)),
                                UnresolvedType::Builtin(BuiltinType::List(expected_element)),
                            )
                        } else {
                            error
                        });
                    }

                    Ok(())
                }
                (actual_builtin, expected_builtin) => Err(TypeError::Mismatch(
                    UnresolvedType::Builtin(actual_builtin),
                    UnresolvedType::Builtin(expected_builtin),
                )),
            },
            (UnresolvedType::Bottom(_), _) => Ok(()),
            (actual, expected) => Err(TypeError::Mismatch(actual, expected)),
        }
    }
}

impl UnresolvedType {
    pub fn id(&self) -> Option<TypeId> {
        match self {
            UnresolvedType::Named(id, _) => Some(*id),
            _ => None,
        }
    }

    pub fn contains(&self, var: &TypeVariable) -> bool {
        match self {
            UnresolvedType::Variable(v) => v == var,
            UnresolvedType::Function(input, output) => input.contains(var) || output.contains(var),
            UnresolvedType::Named(_, params) => params.iter().any(|param| param.contains(var)),
            _ => false,
        }
    }

    pub fn contains_error(&self) -> bool {
        match self {
            UnresolvedType::Function(input, output) => {
                input.contains_error() || output.contains_error()
            }
            UnresolvedType::Bottom(BottomTypeReason::Error) => true,
            UnresolvedType::Named(_, params) => params.iter().any(|param| param.contains_error()),
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
            UnresolvedType::Named(_, params) => {
                for param in params {
                    param.apply(ctx);
                }
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
            UnresolvedType::Named(_, params) => {
                for param in params {
                    param.instantiate_with(substitutions);
                }
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
            UnresolvedType::Named(_, params) => params.iter().flat_map(|ty| ty.vars()).collect(),
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
            UnresolvedType::Named(_, params) => params.iter().flat_map(|ty| ty.params()).collect(),
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
            UnresolvedType::Named(id, params) => Type::Named(
                id,
                params
                    .into_iter()
                    .map(|param| param.finalize(ctx, generic))
                    .collect::<Option<_>>()?,
            ),
            UnresolvedType::Function(input, output) => Type::Function(
                Box::new(input.finalize(ctx, generic)?),
                Box::new(output.finalize(ctx, generic)?),
            ),
            UnresolvedType::Builtin(builtin) => Type::Builtin(match builtin {
                BuiltinType::Unit => BuiltinType::Unit,
                BuiltinType::Text => BuiltinType::Text,
                BuiltinType::Number => BuiltinType::Number,
                BuiltinType::List(ty) => BuiltinType::List(Box::new(ty.finalize(ctx, generic)?)),
            }),
            UnresolvedType::Bottom(is_error) => Type::Bottom(is_error),
        })
    }
}
