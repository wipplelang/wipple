use crate::{parse::Span, TraitId, TypeId, TypeParameterId};
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
pub enum UnresolvedType {
    Variable(TypeVariable),
    Parameter(TypeParameterId),
    TerminatingVariable(TypeVariable),
    NumericVariable(TypeVariable),
    Named(TypeId, Vec<UnresolvedType>, TypeStructure<UnresolvedType>),
    Function(Box<UnresolvedType>, Box<UnresolvedType>),
    Tuple(Vec<UnresolvedType>),
    Builtin(BuiltinType<Box<UnresolvedType>>),
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
pub enum Type {
    Parameter(TypeParameterId),
    Named(TypeId, Vec<Type>, TypeStructure<Type>),
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Builtin(BuiltinType<Box<Type>>),
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
pub enum TypeStructure<Ty> {
    Marker,
    Structure(Vec<Ty>),
    Enumeration(Vec<Vec<Ty>>),
    Recursive(TypeId),
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
                BuiltinType::Natural => BuiltinType::Natural,
                BuiltinType::Byte => BuiltinType::Byte,
                BuiltinType::Signed => BuiltinType::Signed,
                BuiltinType::Unsigned => BuiltinType::Unsigned,
                BuiltinType::Float => BuiltinType::Float,
                BuiltinType::Double => BuiltinType::Double,
                BuiltinType::Text => BuiltinType::Text,
                BuiltinType::List(ty) => BuiltinType::List(Box::new((*ty).into())),
                BuiltinType::Mutable(ty) => BuiltinType::Mutable(Box::new((*ty).into())),
            }),
            Type::Error => UnresolvedType::Error,
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
            TypeStructure::Recursive(id) => TypeStructure::Recursive(id),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct TypeVariable(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
pub enum BuiltinType<Ty> {
    Number,
    Integer,
    Natural,
    Byte,
    Signed,
    Unsigned,
    Float,
    Double,
    Text,
    List(Ty),
    Mutable(Ty),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum BottomTypeReason {
    Annotated,
    Error,
}

pub type GenericSubstitutions = BTreeMap<TypeParameterId, UnresolvedType>;

#[derive(Debug, Clone, Default)]
pub struct Context {
    pub next_var: usize,
    pub substitutions: im::HashMap<TypeVariable, UnresolvedType>,
    pub terminating_substitutions: im::HashMap<TypeVariable, UnresolvedType>,
    pub numeric_substitutions: im::HashMap<TypeVariable, UnresolvedType>,
}

#[derive(Debug, Clone)]
pub enum TypeError {
    ErrorExpression,
    Recursive(TypeVariable),
    Mismatch(UnresolvedType, UnresolvedType),
    MissingInstance(TraitId, Vec<UnresolvedType>, Option<Span>, Vec<Span>),
    AmbiguousTrait(TraitId, Vec<UnresolvedType>, Vec<Span>),
    UnresolvedType(UnresolvedType),
    InvalidNumericLiteral(UnresolvedType),
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
        expected: impl Into<UnresolvedType>,
    ) -> (GenericSubstitutions, Result<(), TypeError>) {
        let mut params = GenericSubstitutions::new();
        let result = self.unify_internal(actual, expected.into(), false, false, &mut params);
        (params, result)
    }

    pub fn unify(
        &mut self,
        actual: UnresolvedType,
        expected: impl Into<UnresolvedType>,
    ) -> Result<(), TypeError> {
        self.unify_internal(
            actual,
            expected.into(),
            false,
            false,
            &mut GenericSubstitutions::new(),
        )
    }

    pub fn unify_reverse(
        &mut self,
        actual: impl Into<UnresolvedType>,
        expected: UnresolvedType,
    ) -> Result<(), TypeError> {
        self.unify_internal(
            actual.into(),
            expected,
            false,
            true,
            &mut GenericSubstitutions::new(),
        )
    }

    pub fn unify_generic(
        &mut self,
        actual: UnresolvedType,
        expected: impl Into<UnresolvedType>,
    ) -> Result<(), TypeError> {
        self.unify_internal(
            actual,
            expected.into(),
            true,
            false,
            &mut GenericSubstitutions::new(),
        )
    }

    fn unify_internal(
        &mut self,
        mut actual: UnresolvedType,
        mut expected: UnresolvedType,
        generic: bool,
        reverse: bool,
        params: &mut BTreeMap<TypeParameterId, UnresolvedType>,
    ) -> Result<(), TypeError> {
        actual.apply(self);
        expected.apply(self);

        if !generic {
            if let UnresolvedType::Parameter(param) = expected {
                params.insert(param, actual.clone());
            }
        }

        macro_rules! mismatch {
            ($actual:expr, $expected:expr $(,)?) => {
                if reverse {
                    TypeError::Mismatch($expected, $actual)
                } else {
                    TypeError::Mismatch($actual, $expected)
                }
            };
        }

        match (actual, expected) {
            (
                UnresolvedType::Parameter(actual_param),
                UnresolvedType::Parameter(expected_param),
            ) if generic => {
                if actual_param == expected_param {
                    Ok(())
                } else {
                    Err(mismatch!(
                        UnresolvedType::Parameter(actual_param),
                        UnresolvedType::Parameter(expected_param),
                    ))
                }
            }
            (_, UnresolvedType::Parameter(_)) if !generic => Ok(()),
            // FIXME: Determine if removing this is sound
            // (UnresolvedType::Parameter(actual), expected) if !generic => {
            //     Err(mismatch!(UnresolvedType::Parameter(actual), expected))
            // }
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
            (UnresolvedType::TerminatingVariable(var), ty) => {
                match &ty {
                    UnresolvedType::TerminatingVariable(other) => {
                        if var == *other {
                            return Ok(());
                        }
                    }
                    _ => {}
                }

                if ty.contains(&var) {
                    Err(TypeError::Recursive(var))
                } else {
                    self.terminating_substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (ty, UnresolvedType::TerminatingVariable(var)) => {
                match &ty {
                    UnresolvedType::TerminatingVariable(other) => {
                        if var == *other {
                            return Ok(());
                        }
                    }
                    UnresolvedType::Tuple(tys) if tys.is_empty() => {}
                    _ => return Err(mismatch!(ty, UnresolvedType::TerminatingVariable(var))),
                }

                if ty.contains(&var) {
                    Err(TypeError::Recursive(var))
                } else {
                    self.terminating_substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (UnresolvedType::NumericVariable(var), ty) => {
                match &ty {
                    UnresolvedType::NumericVariable(other) => {
                        if var == *other {
                            return Ok(());
                        }
                    }
                    UnresolvedType::Builtin(ty) if ty.is_numeric() => {}
                    _ => {
                        return Err(mismatch!(UnresolvedType::NumericVariable(var), ty));
                    }
                }

                if ty.contains(&var) {
                    Err(TypeError::Recursive(var))
                } else {
                    self.numeric_substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (ty, UnresolvedType::NumericVariable(var)) => {
                match &ty {
                    UnresolvedType::NumericVariable(other) => {
                        if var == *other {
                            return Ok(());
                        }
                    }
                    UnresolvedType::Builtin(ty) if ty.is_numeric() => {}
                    _ => {
                        return Err(mismatch!(ty, UnresolvedType::NumericVariable(var)));
                    }
                }

                if ty.contains(&var) {
                    Err(TypeError::Recursive(var))
                } else {
                    self.numeric_substitutions.insert(var, ty);
                    Ok(())
                }
            }
            (
                UnresolvedType::Named(actual_id, actual_params, actual_structure),
                UnresolvedType::Named(expected_id, expected_params, expected_structure),
            ) => {
                if actual_id == expected_id {
                    let mut error = false;
                    for (actual, expected) in actual_params.iter().zip(&expected_params) {
                        if let Err(e) = self.unify_internal(
                            actual.clone(),
                            expected.clone(),
                            generic,
                            reverse,
                            params,
                        ) {
                            if let TypeError::Mismatch(_, _) = e {
                                error = true;
                            } else {
                                return Err(e);
                            }
                        }
                    }

                    if error {
                        return Err(mismatch!(
                            UnresolvedType::Named(actual_id, actual_params, actual_structure),
                            UnresolvedType::Named(expected_id, expected_params, expected_structure),
                        ));
                    }

                    Ok(())
                } else {
                    Err(mismatch!(
                        UnresolvedType::Named(actual_id, actual_params, actual_structure),
                        UnresolvedType::Named(expected_id, expected_params, expected_structure),
                    ))
                }
            }
            (
                UnresolvedType::Function(actual_input, actual_output),
                UnresolvedType::Function(expected_input, expected_output),
            ) => {
                let mut error = false;

                if let Err(e) = self.unify_internal(
                    (*actual_input).clone(),
                    (*expected_input).clone(),
                    generic,
                    reverse,
                    params,
                ) {
                    if let TypeError::Mismatch(_, _) = e {
                        error = true;
                    } else {
                        return Err(e);
                    }
                }

                if let Err(e) = self.unify_internal(
                    (*actual_output).clone(),
                    (*expected_output).clone(),
                    generic,
                    reverse,
                    params,
                ) {
                    if let TypeError::Mismatch(_, _) = e {
                        error = true;
                    } else {
                        return Err(e);
                    }
                }

                if error {
                    return Err(mismatch!(
                        UnresolvedType::Function(actual_input, actual_output),
                        UnresolvedType::Function(expected_input, expected_output),
                    ));
                }

                Ok(())
            }
            (UnresolvedType::Tuple(actual_tys), UnresolvedType::Tuple(expected_tys)) => {
                if actual_tys.len() != expected_tys.len() {
                    return Err(mismatch!(
                        UnresolvedType::Tuple(actual_tys),
                        UnresolvedType::Tuple(expected_tys),
                    ));
                }

                let mut error = false;
                for (actual, expected) in std::iter::zip(&actual_tys, &expected_tys) {
                    if let Err(e) = self.unify_internal(
                        actual.clone(),
                        expected.clone(),
                        generic,
                        reverse,
                        params,
                    ) {
                        if let TypeError::Mismatch(_, _) = e {
                            error = true;
                        } else {
                            return Err(e);
                        }
                    }
                }

                if error {
                    return Err(mismatch!(
                        UnresolvedType::Tuple(actual_tys),
                        UnresolvedType::Tuple(expected_tys),
                    ));
                }

                Ok(())
            }
            (
                UnresolvedType::Builtin(actual_builtin),
                UnresolvedType::Builtin(expected_builtin),
            ) => match (actual_builtin, expected_builtin) {
                (BuiltinType::Number, BuiltinType::Number)
                | (BuiltinType::Integer, BuiltinType::Integer)
                | (BuiltinType::Natural, BuiltinType::Natural)
                | (BuiltinType::Byte, BuiltinType::Byte)
                | (BuiltinType::Signed, BuiltinType::Signed)
                | (BuiltinType::Unsigned, BuiltinType::Unsigned)
                | (BuiltinType::Float, BuiltinType::Float)
                | (BuiltinType::Double, BuiltinType::Double)
                | (BuiltinType::Text, BuiltinType::Text) => Ok(()),
                (BuiltinType::List(actual_element), BuiltinType::List(expected_element)) => {
                    if let Err(error) = self.unify_internal(
                        (*actual_element).clone(),
                        (*expected_element).clone(),
                        generic,
                        reverse,
                        params,
                    ) {
                        return Err(if let TypeError::Mismatch(_, _) = error {
                            mismatch!(
                                UnresolvedType::Builtin(BuiltinType::List(actual_element)),
                                UnresolvedType::Builtin(BuiltinType::List(expected_element)),
                            )
                        } else {
                            error
                        });
                    }

                    Ok(())
                }
                (BuiltinType::Mutable(actual_element), BuiltinType::Mutable(expected_element)) => {
                    if let Err(error) = self.unify_internal(
                        (*actual_element).clone(),
                        (*expected_element).clone(),
                        generic,
                        reverse,
                        params,
                    ) {
                        return Err(if let TypeError::Mismatch(_, _) = error {
                            mismatch!(
                                UnresolvedType::Builtin(BuiltinType::Mutable(actual_element)),
                                UnresolvedType::Builtin(BuiltinType::Mutable(expected_element)),
                            )
                        } else {
                            error
                        });
                    }

                    Ok(())
                }
                (actual_builtin, expected_builtin) => Err(mismatch!(
                    UnresolvedType::Builtin(actual_builtin),
                    UnresolvedType::Builtin(expected_builtin),
                )),
            },
            (_, UnresolvedType::Error) | (UnresolvedType::Error, _) => Ok(()),
            (actual, expected) => Err(mismatch!(actual, expected)),
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
            UnresolvedType::Variable(v)
            | UnresolvedType::TerminatingVariable(v)
            | UnresolvedType::NumericVariable(v) => v == var,
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
            UnresolvedType::Error => true,
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
            UnresolvedType::TerminatingVariable(var) => {
                if let Some(ty) = ctx.terminating_substitutions.get(var) {
                    *self = ty.clone();
                    self.apply(ctx);
                }
            }
            UnresolvedType::NumericVariable(var) => {
                if let Some(ty) = ctx.numeric_substitutions.get(var) {
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

    pub fn instantiate_with(&mut self, ctx: &mut Context, substitutions: &GenericSubstitutions) {
        self.apply(ctx);

        match self {
            UnresolvedType::Parameter(param) => {
                *self = substitutions.get(param).cloned().unwrap_or_else(|| {
                    // HACK: If the typechecker behaves erratically, try panicking here instead
                    // of returning a new type variable to get to the root cause earlier.
                    UnresolvedType::Variable(ctx.new_variable())
                });
            }
            UnresolvedType::Function(input, output) => {
                input.instantiate_with(ctx, substitutions);
                output.instantiate_with(ctx, substitutions);
            }
            UnresolvedType::Named(_, params, structure) => {
                for param in params {
                    param.instantiate_with(ctx, substitutions);
                }

                structure.instantiate_with(ctx, substitutions);
            }
            UnresolvedType::Tuple(tys) => {
                for ty in tys {
                    ty.instantiate_with(ctx, substitutions);
                }
            }
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => {
                    ty.instantiate_with(ctx, substitutions)
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn vars(&self) -> Vec<TypeVariable> {
        match self {
            UnresolvedType::Variable(var) => vec![*var],
            UnresolvedType::Function(input, output) => {
                let mut vars = input.vars();
                vars.extend(output.vars());
                vars
            }
            UnresolvedType::Named(_, params, structure) => params
                .iter()
                .flat_map(|ty| ty.vars())
                .chain(structure.vars())
                .collect(),
            UnresolvedType::Tuple(tys) => tys.iter().flat_map(|ty| ty.vars()).collect(),
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.vars(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
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

    pub fn finalize_default_variables(&mut self, ctx: &Context) {
        self.apply(ctx);

        match self {
            UnresolvedType::Variable(var) => {
                if let Some(ty) = ctx.substitutions.get(var) {
                    *self = ty.clone();
                    self.finalize_default_variables(ctx);
                }
            }
            UnresolvedType::TerminatingVariable(var) => {
                if let Some(ty) = ctx.terminating_substitutions.get(var) {
                    *self = ty.clone();
                    self.finalize_default_variables(ctx);
                } else {
                    *self = UnresolvedType::Tuple(Vec::new());
                }
            }
            UnresolvedType::NumericVariable(var) => {
                if let Some(ty) = ctx.numeric_substitutions.get(var) {
                    *self = ty.clone();
                    self.finalize_default_variables(ctx);
                } else {
                    *self = UnresolvedType::Builtin(BuiltinType::Number);
                }
            }
            UnresolvedType::Function(input, output) => {
                input.finalize_default_variables(ctx);
                output.finalize_default_variables(ctx);
            }
            UnresolvedType::Named(_, params, structure) => {
                for param in params {
                    param.finalize_default_variables(ctx);
                }

                structure.finalize_default_variables(ctx);
            }
            UnresolvedType::Tuple(tys) => {
                for ty in tys {
                    ty.finalize_default_variables(ctx);
                }
            }
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => {
                    ty.finalize_default_variables(ctx)
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn finalize(&self, ctx: &Context) -> Result<Type, TypeError> {
        let mut ty = self.clone();
        ty.apply(ctx);
        ty.finalize_default_variables(ctx);

        let result = (|| {
            Ok(match ty.clone() {
                UnresolvedType::Variable(var) => {
                    return Err(TypeError::UnresolvedType(UnresolvedType::Variable(var)));
                }
                UnresolvedType::Parameter(param) => Type::Parameter(param),
                UnresolvedType::TerminatingVariable(_) | UnresolvedType::NumericVariable(_) => {
                    unreachable!()
                }
                UnresolvedType::Named(id, params, structure) => Type::Named(
                    id,
                    params
                        .into_iter()
                        .map(|param| param.finalize(ctx))
                        .collect::<Result<_, _>>()?,
                    structure.finalize(ctx)?,
                ),
                UnresolvedType::Function(input, output) => Type::Function(
                    Box::new(input.finalize(ctx)?),
                    Box::new(output.finalize(ctx)?),
                ),
                UnresolvedType::Tuple(tys) => Type::Tuple(
                    tys.into_iter()
                        .map(|ty| ty.finalize(ctx))
                        .collect::<Result<_, _>>()?,
                ),
                UnresolvedType::Builtin(builtin) => Type::Builtin(match builtin {
                    BuiltinType::Number => BuiltinType::Number,
                    BuiltinType::Integer => BuiltinType::Integer,
                    BuiltinType::Natural => BuiltinType::Natural,
                    BuiltinType::Byte => BuiltinType::Byte,
                    BuiltinType::Signed => BuiltinType::Signed,
                    BuiltinType::Unsigned => BuiltinType::Unsigned,
                    BuiltinType::Float => BuiltinType::Float,
                    BuiltinType::Double => BuiltinType::Double,
                    BuiltinType::Text => BuiltinType::Text,
                    BuiltinType::List(ty) => BuiltinType::List(Box::new(ty.finalize(ctx)?)),
                    BuiltinType::Mutable(ty) => BuiltinType::Mutable(Box::new(ty.finalize(ctx)?)),
                }),
                UnresolvedType::Error => Type::Error,
            })
        })();

        match result {
            Ok(ty) => Ok(ty),
            Err(_) => Err(TypeError::UnresolvedType(ty)),
        }
    }
}

impl TypeStructure<UnresolvedType> {
    pub fn apply(&mut self, ctx: &Context) {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => {}
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

    pub fn instantiate_with(&mut self, ctx: &mut Context, substitutions: &GenericSubstitutions) {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.instantiate_with(ctx, substitutions);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.instantiate_with(ctx, substitutions);
                    }
                }
            }
        }
    }

    pub fn vars(&self) -> Vec<TypeVariable> {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => Vec::new(),
            TypeStructure::Structure(tys) => tys.iter().flat_map(|ty| ty.vars()).collect(),
            TypeStructure::Enumeration(variants) => variants
                .iter()
                .flat_map(|tys| tys.iter().flat_map(|ty| ty.vars()))
                .collect(),
        }
    }

    pub fn params(&self) -> Vec<TypeParameterId> {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => Vec::new(),
            TypeStructure::Structure(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            TypeStructure::Enumeration(variants) => variants
                .iter()
                .flat_map(|tys| tys.iter().flat_map(|ty| ty.params()))
                .collect(),
        }
    }

    pub fn finalize_default_variables(&mut self, ctx: &Context) {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.finalize_default_variables(ctx);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.finalize_default_variables(ctx);
                    }
                }
            }
        }
    }

    pub fn finalize(self, ctx: &Context) -> Result<TypeStructure<Type>, TypeError> {
        Ok(match self {
            TypeStructure::Marker => TypeStructure::Marker,
            TypeStructure::Structure(tys) => TypeStructure::Structure(
                tys.into_iter()
                    .map(|ty| ty.finalize(ctx))
                    .collect::<Result<_, _>>()?,
            ),
            TypeStructure::Enumeration(variants) => TypeStructure::Enumeration(
                variants
                    .into_iter()
                    .map(|tys| {
                        tys.into_iter()
                            .map(|ty| ty.finalize(ctx))
                            .collect::<Result<_, _>>()
                    })
                    .collect::<Result<_, _>>()?,
            ),
            TypeStructure::Recursive(id) => TypeStructure::Recursive(id),
        })
    }
}

impl TypeStructure<Type> {
    pub fn params(&self) -> Vec<TypeParameterId> {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => Vec::new(),
            TypeStructure::Structure(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            TypeStructure::Enumeration(variants) => variants
                .iter()
                .flat_map(|tys| tys.iter().flat_map(|ty| ty.params()))
                .collect(),
        }
    }
}

impl Type {
    pub fn params(&self) -> Vec<TypeParameterId> {
        match self {
            Type::Parameter(param) => vec![*param],
            Type::Function(input, output) => {
                let mut params = input.params();
                params.extend(output.params());
                params
            }
            Type::Named(_, params, structure) => params
                .iter()
                .flat_map(|ty| ty.params())
                .chain(structure.params())
                .collect(),
            Type::Tuple(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            Type::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.params(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }
}

impl<Ty> BuiltinType<Ty> {
    fn is_numeric(&self) -> bool {
        matches!(
            self,
            BuiltinType::Number
                | BuiltinType::Integer
                | BuiltinType::Natural
                | BuiltinType::Byte
                | BuiltinType::Signed
                | BuiltinType::Unsigned
                | BuiltinType::Float
                | BuiltinType::Double
        )
    }
}
