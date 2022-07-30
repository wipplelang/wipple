use crate::{parse::Span, GenericConstantId, TraitId, TypeId, TypeParameterId};
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum UnresolvedType {
    Variable(TypeVariable),
    Parameter(TypeParameterId),
    Named(TypeId, Vec<UnresolvedType>, TypeStructure<UnresolvedType>),
    Function(Box<UnresolvedType>, Box<UnresolvedType>),
    Tuple(Vec<UnresolvedType>),
    Builtin(BuiltinType<Box<UnresolvedType>>),
    Bottom(BottomTypeReason),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum Type {
    Parameter(TypeParameterId),
    Named(TypeId, Vec<Type>, TypeStructure<Type>),
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Builtin(BuiltinType<Box<Type>>),
    Bottom(BottomTypeReason),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum TypeStructure<Ty> {
    Marker,
    Structure(Vec<Ty>),
    Enumeration(Vec<Vec<Ty>>),
}

impl From<Type> for UnresolvedType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Parameter(param) => UnresolvedType::Parameter(param),
            Type::Named(id, params, structure) => UnresolvedType::Named(
                id,
                params.into_iter().map(|param| param.into()).collect(),
                structure.into(),
            ),
            Type::Function(input, output) => {
                UnresolvedType::Function(Box::new((*input).into()), Box::new((*output).into()))
            }
            Type::Tuple(tys) => {
                UnresolvedType::Tuple(tys.into_iter().map(|ty| ty.into()).collect())
            }
            Type::Builtin(builtin) => UnresolvedType::Builtin(match builtin {
                BuiltinType::Number => BuiltinType::Number,
                BuiltinType::Integer => BuiltinType::Integer,
                BuiltinType::Text => BuiltinType::Text,
                BuiltinType::List(ty) => BuiltinType::List(Box::new((*ty).into())),
                BuiltinType::Mutable(ty) => BuiltinType::Mutable(Box::new((*ty).into())),
            }),
            Type::Bottom(reason) => UnresolvedType::Bottom(reason),
        }
    }
}

impl From<TypeStructure<Type>> for TypeStructure<UnresolvedType> {
    fn from(structure: TypeStructure<Type>) -> Self {
        match structure {
            TypeStructure::Marker => TypeStructure::Marker,
            TypeStructure::Structure(tys) => {
                TypeStructure::Structure(tys.into_iter().map(From::from).collect())
            }
            TypeStructure::Enumeration(variants) => TypeStructure::Enumeration(
                variants
                    .into_iter()
                    .map(|tys| tys.into_iter().map(From::from).collect())
                    .collect(),
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct TypeVariable(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum BuiltinType<Ty> {
    Number,
    Integer,
    Text,
    List(Ty),
    Mutable(Ty),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
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
    MissingInstance(TraitId, Vec<UnresolvedType>, Option<Span>),
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

    pub fn unify_params(
        &mut self,
        actual: UnresolvedType,
        expected: UnresolvedType,
    ) -> (
        BTreeMap<TypeParameterId, UnresolvedType>,
        Result<(), TypeError>,
    ) {
        let mut params = BTreeMap::new();
        let result = self.unify_internal(actual, expected, false, &mut params);
        (params, result)
    }

    pub fn unify(
        &mut self,
        actual: UnresolvedType,
        expected: UnresolvedType,
    ) -> Result<(), TypeError> {
        self.unify_internal(actual, expected, false, &mut BTreeMap::new())
    }

    pub fn unify_generic(
        &mut self,
        actual: UnresolvedType,
        expected: UnresolvedType,
    ) -> Result<(), TypeError> {
        self.unify_internal(actual, expected, true, &mut BTreeMap::new())
    }

    fn unify_internal(
        &mut self,
        mut actual: UnresolvedType,
        mut expected: UnresolvedType,
        generic: bool,
        params: &mut BTreeMap<TypeParameterId, UnresolvedType>,
    ) -> Result<(), TypeError> {
        actual.apply(self);
        expected.apply(self);

        match (actual, expected) {
            (UnresolvedType::Variable(var), ty) | (ty, UnresolvedType::Variable(var)) => {
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
            (ty, UnresolvedType::Parameter(param)) if !generic => {
                params.insert(param, ty);
                Ok(())
            }
            (UnresolvedType::Parameter(actual), expected) if !generic => Err(TypeError::Mismatch(
                UnresolvedType::Parameter(actual),
                expected,
            )),
            (
                UnresolvedType::Named(actual_id, actual_params, actual_structure),
                UnresolvedType::Named(expected_id, expected_params, expected_structure),
            ) => {
                if actual_id == expected_id {
                    for (actual, expected) in actual_params.iter().zip(&expected_params) {
                        if let Err(error) =
                            self.unify_internal(actual.clone(), expected.clone(), generic, params)
                        {
                            return Err(if let TypeError::Mismatch(_, _) = error {
                                TypeError::Mismatch(
                                    UnresolvedType::Named(
                                        actual_id,
                                        actual_params,
                                        actual_structure,
                                    ),
                                    UnresolvedType::Named(
                                        expected_id,
                                        expected_params,
                                        expected_structure,
                                    ),
                                )
                            } else {
                                error
                            });
                        }
                    }

                    Ok(())
                } else {
                    Err(TypeError::Mismatch(
                        UnresolvedType::Named(actual_id, actual_params, actual_structure),
                        UnresolvedType::Named(expected_id, expected_params, expected_structure),
                    ))
                }
            }
            (
                UnresolvedType::Function(actual_input, actual_output),
                UnresolvedType::Function(expected_input, expected_output),
            ) => {
                if let Err(error) = self.unify_internal(
                    (*actual_input).clone(),
                    (*expected_input).clone(),
                    generic,
                    params,
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

                if let Err(error) = self.unify_internal(
                    (*actual_output).clone(),
                    (*expected_output).clone(),
                    generic,
                    params,
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
            (UnresolvedType::Tuple(actual_tys), UnresolvedType::Tuple(expected_tys)) => {
                if actual_tys.len() != expected_tys.len() {
                    return Err(TypeError::Mismatch(
                        UnresolvedType::Tuple(actual_tys),
                        UnresolvedType::Tuple(expected_tys),
                    ));
                }

                for (actual, expected) in std::iter::zip(&actual_tys, &expected_tys) {
                    if let Err(error) =
                        self.unify_internal(actual.clone(), expected.clone(), generic, params)
                    {
                        return Err(if let TypeError::Mismatch(_, _) = error {
                            TypeError::Mismatch(
                                UnresolvedType::Tuple(actual_tys),
                                UnresolvedType::Tuple(expected_tys),
                            )
                        } else {
                            error
                        });
                    }
                }

                Ok(())
            }
            (
                UnresolvedType::Builtin(actual_builtin),
                UnresolvedType::Builtin(expected_builtin),
            ) => match (actual_builtin, expected_builtin) {
                (BuiltinType::Number, BuiltinType::Number) => Ok(()),
                (BuiltinType::Integer, BuiltinType::Integer) => Ok(()),
                (BuiltinType::Text, BuiltinType::Text) => Ok(()),
                (BuiltinType::List(actual_element), BuiltinType::List(expected_element))
                | (BuiltinType::Mutable(actual_element), BuiltinType::Mutable(expected_element)) => {
                    if let Err(error) = self.unify_internal(
                        (*actual_element).clone(),
                        (*expected_element).clone(),
                        generic,
                        params,
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
            (_, UnresolvedType::Bottom(reason)) if matches!(reason, BottomTypeReason::Error) => {
                Ok(())
            }
            (actual, expected) => Err(TypeError::Mismatch(actual, expected)),
        }
    }
}

impl UnresolvedType {
    pub fn id(&self) -> Option<TypeId> {
        match self {
            UnresolvedType::Named(id, _, _) => Some(*id),
            _ => None,
        }
    }

    pub fn contains(&self, var: &TypeVariable) -> bool {
        match self {
            UnresolvedType::Variable(v) => v == var,
            UnresolvedType::Function(input, output) => input.contains(var) || output.contains(var),
            UnresolvedType::Named(_, params, _) => params.iter().any(|param| param.contains(var)),
            UnresolvedType::Tuple(tys) => tys.iter().any(|ty| ty.contains(var)),
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.contains(var),
                _ => false,
            },
            _ => false,
        }
    }

    pub fn contains_error(&self) -> bool {
        match self {
            UnresolvedType::Function(input, output) => {
                input.contains_error() || output.contains_error()
            }
            UnresolvedType::Bottom(BottomTypeReason::Error) => true,
            UnresolvedType::Named(_, params, _) => {
                params.iter().any(|param| param.contains_error())
            }
            UnresolvedType::Tuple(tys) => tys.iter().any(|ty| ty.contains_error()),
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.contains_error(),
                _ => false,
            },
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
            UnresolvedType::Named(_, params, structure) => {
                for param in params {
                    param.apply(ctx);
                }

                structure.apply(ctx);
            }
            UnresolvedType::Tuple(tys) => {
                for ty in tys {
                    ty.apply(ctx);
                }
            }
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.apply(ctx),
                _ => {}
            },
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
            UnresolvedType::Named(_, params, structure) => {
                for param in params {
                    param.instantiate_with(substitutions);
                }

                structure.instantiate_with(substitutions);
            }
            UnresolvedType::Tuple(tys) => {
                for ty in tys {
                    ty.instantiate_with(substitutions);
                }
            }
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => {
                    ty.instantiate_with(substitutions)
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn params(&self) -> Vec<TypeParameterId> {
        match self {
            UnresolvedType::Parameter(param) => vec![*param],
            UnresolvedType::Function(input, output) => {
                let mut params = input.params();
                params.extend(output.params());
                params
            }
            UnresolvedType::Named(_, params, structure) => params
                .iter()
                .flat_map(|ty| ty.params())
                .chain(structure.params())
                .collect(),
            UnresolvedType::Tuple(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.params(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
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
                    return None;
                }
            }
            UnresolvedType::Named(id, params, structure) => Type::Named(
                id,
                params
                    .into_iter()
                    .map(|param| param.finalize(ctx, generic))
                    .collect::<Option<_>>()?,
                structure.finalize(ctx, generic)?,
            ),
            UnresolvedType::Function(input, output) => Type::Function(
                Box::new(input.finalize(ctx, generic)?),
                Box::new(output.finalize(ctx, generic)?),
            ),
            UnresolvedType::Tuple(tys) => Type::Tuple(
                tys.into_iter()
                    .map(|ty| ty.finalize(ctx, generic))
                    .collect::<Option<_>>()?,
            ),
            UnresolvedType::Builtin(builtin) => Type::Builtin(match builtin {
                BuiltinType::Number => BuiltinType::Number,
                BuiltinType::Integer => BuiltinType::Integer,
                BuiltinType::Text => BuiltinType::Text,
                BuiltinType::List(ty) => BuiltinType::List(Box::new(ty.finalize(ctx, generic)?)),
                BuiltinType::Mutable(ty) => {
                    BuiltinType::Mutable(Box::new(ty.finalize(ctx, generic)?))
                }
            }),
            UnresolvedType::Bottom(is_error) => Type::Bottom(is_error),
        })
    }
}

impl TypeStructure<UnresolvedType> {
    pub fn apply(&mut self, ctx: &Context) {
        match self {
            TypeStructure::Marker => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.apply(ctx);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.apply(ctx);
                    }
                }
            }
        }
    }

    pub fn instantiate_with(&mut self, substitutions: &GenericSubstitutions) {
        match self {
            TypeStructure::Marker => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.instantiate_with(substitutions);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.instantiate_with(substitutions);
                    }
                }
            }
        }
    }

    pub fn params(&self) -> Vec<TypeParameterId> {
        match self {
            TypeStructure::Marker => Vec::new(),
            TypeStructure::Structure(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            TypeStructure::Enumeration(variants) => variants
                .iter()
                .flat_map(|tys| tys.iter().flat_map(|ty| ty.params()))
                .collect(),
        }
    }

    pub fn finalize(self, ctx: &Context, generic: bool) -> Option<TypeStructure<Type>> {
        Some(match self {
            TypeStructure::Marker => TypeStructure::Marker,
            TypeStructure::Structure(tys) => TypeStructure::Structure(
                tys.into_iter()
                    .map(|ty| ty.finalize(ctx, generic))
                    .collect::<Option<_>>()?,
            ),
            TypeStructure::Enumeration(variants) => TypeStructure::Enumeration(
                variants
                    .into_iter()
                    .map(|tys| {
                        tys.into_iter()
                            .map(|ty| ty.finalize(ctx, generic))
                            .collect::<Option<_>>()
                    })
                    .collect::<Option<_>>()?,
            ),
        })
    }
}
