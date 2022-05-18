#![allow(clippy::single_match)]

use crate::{parser::Span, GenericConstantId, TraitId, TypeId, TypeParameterId};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnresolvedType {
    pub span: Span,
    pub kind: UnresolvedTypeKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnresolvedTypeKind {
    Variable(TypeVariable),
    Parameter(TypeParameter),
    Named(TypeId),
    Function(Box<UnresolvedType>, Box<UnresolvedType>),
    Builtin(BuiltinType),
    Bottom(BottomTypeReason),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnresolvedForAll {
    pub params: BTreeSet<TypeParameter>,
    pub ty: UnresolvedType,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct TypeParameter {
    pub span: Span,
    pub id: TypeParameterId,
}

impl PartialEq for TypeParameter {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TypeParameter {}

impl PartialOrd for TypeParameter {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for TypeParameter {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

pub type GenericSubstitutions = BTreeMap<TypeParameter, UnresolvedType>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeKind {
    Parameter(TypeParameter),
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
        UnresolvedType {
            span: ty.span,
            kind: match ty.kind {
                TypeKind::Parameter(param) => UnresolvedTypeKind::Parameter(param),
                TypeKind::Named(name) => UnresolvedTypeKind::Named(name),
                TypeKind::Function(input, output) => UnresolvedTypeKind::Function(
                    Box::new((*input).into()),
                    Box::new((*output).into()),
                ),
                TypeKind::Builtin(builtin) => UnresolvedTypeKind::Builtin(builtin),
                TypeKind::Bottom(reason) => UnresolvedTypeKind::Bottom(reason),
            },
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
pub struct TypeError {
    pub span: Span,
    pub kind: TypeErrorKind,
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
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

        match (&actual.kind, &expected.kind) {
            (kind, UnresolvedTypeKind::Variable(var)) => {
                if let UnresolvedTypeKind::Variable(other) = kind {
                    if var == other {
                        return Ok(());
                    }
                }

                if actual.contains(var) {
                    Err(TypeError {
                        span: actual.span,
                        kind: TypeErrorKind::Recursive(*var),
                    })
                } else {
                    self.substitutions.insert(*var, actual);
                    Ok(())
                }
            }
            (UnresolvedTypeKind::Variable(var), kind) => {
                if let UnresolvedTypeKind::Variable(other) = kind {
                    if var == other {
                        return Ok(());
                    }
                }

                if expected.contains(var) {
                    Err(TypeError {
                        span: expected.span,
                        kind: TypeErrorKind::Recursive(*var),
                    })
                } else {
                    self.substitutions.insert(*var, expected);
                    Ok(())
                }
            }
            (
                UnresolvedTypeKind::Parameter(actual_param),
                UnresolvedTypeKind::Parameter(expected_param),
            ) if generic => {
                if actual_param == expected_param {
                    Ok(())
                } else {
                    Err(TypeError {
                        span: actual.span,
                        kind: TypeErrorKind::Mismatch(actual, expected),
                    })
                }
            }
            (_, UnresolvedTypeKind::Parameter(_)) if !generic => Ok(()),
            (UnresolvedTypeKind::Parameter(_), _) if !generic => Err(TypeError {
                span: actual.span,
                kind: TypeErrorKind::Mismatch(actual, expected),
            }),
            (UnresolvedTypeKind::Named(actual_id), UnresolvedTypeKind::Named(expected_id)) => {
                if actual_id == expected_id {
                    Ok(())
                } else {
                    Err(TypeError {
                        span: actual.span,
                        kind: TypeErrorKind::Mismatch(actual, expected),
                    })
                }
            }
            (
                UnresolvedTypeKind::Function(actual_input, actual_output),
                UnresolvedTypeKind::Function(expected_input, expected_output),
            ) => {
                self.unify_internal(
                    (**actual_input).clone(),
                    (**expected_input).clone(),
                    generic,
                )?;

                self.unify_internal(
                    (**actual_output).clone(),
                    (**expected_output).clone(),
                    generic,
                )?;

                Ok(())
            }
            (
                UnresolvedTypeKind::Builtin(actual_builtin),
                UnresolvedTypeKind::Builtin(expected_builtin),
            ) => {
                if actual_builtin == expected_builtin {
                    Ok(())
                } else {
                    Err(TypeError {
                        span: actual.span,
                        kind: TypeErrorKind::Mismatch(actual, expected),
                    })
                }
            }
            (UnresolvedTypeKind::Bottom(_), _) => Ok(()),
            _ => Err(TypeError {
                span: actual.span,
                kind: TypeErrorKind::Mismatch(actual, expected),
            }),
        }
    }
}

impl UnresolvedType {
    pub fn id(&self) -> Option<TypeId> {
        match &self.kind {
            UnresolvedTypeKind::Named(id) => Some(*id),
            _ => None,
        }
    }

    pub fn contains(&self, var: &TypeVariable) -> bool {
        match &self.kind {
            UnresolvedTypeKind::Variable(v) => v == var,
            UnresolvedTypeKind::Function(input, output) => {
                input.contains(var) || output.contains(var)
            }
            _ => false,
        }
    }

    pub fn contains_error(&self) -> bool {
        match &self.kind {
            UnresolvedTypeKind::Function(input, output) => {
                input.contains_error() || output.contains_error()
            }
            UnresolvedTypeKind::Bottom(BottomTypeReason::Error) => true,
            _ => false,
        }
    }

    pub fn apply(&mut self, ctx: &Context) {
        match &mut self.kind {
            UnresolvedTypeKind::Variable(var) => {
                if let Some(ty) = ctx.substitutions.get(var) {
                    *self = ty.clone();
                    self.apply(ctx);
                }
            }
            UnresolvedTypeKind::Function(input, output) => {
                input.apply(ctx);
                output.apply(ctx);
            }
            _ => {}
        }
    }

    pub fn instantiate_with(&mut self, substitutions: &GenericSubstitutions) {
        match &mut self.kind {
            UnresolvedTypeKind::Parameter(param) => {
                *self = substitutions
                    .get(param)
                    .unwrap_or_else(|| panic!("missing parameter {:?} in substitution", param))
                    .clone();
            }
            UnresolvedTypeKind::Function(input, output) => {
                input.instantiate_with(substitutions);
                output.instantiate_with(substitutions);
            }
            _ => {}
        }
    }

    pub fn vars(&self) -> BTreeSet<TypeVariable> {
        match &self.kind {
            UnresolvedTypeKind::Variable(var) => BTreeSet::from([*var]),
            UnresolvedTypeKind::Function(input, output) => {
                let mut vars = input.vars();
                vars.extend(output.vars());
                vars
            }
            _ => BTreeSet::new(),
        }
    }

    pub fn params(&self) -> BTreeSet<TypeParameter> {
        match &self.kind {
            UnresolvedTypeKind::Parameter(param) => BTreeSet::from([*param]),
            UnresolvedTypeKind::Function(input, output) => {
                let mut params = input.params();
                params.extend(output.params());
                params
            }
            _ => BTreeSet::new(),
        }
    }

    pub fn finalize(mut self, ctx: &Context, generic: bool) -> Option<Type> {
        self.apply(ctx);

        Some(Type {
            span: self.span,
            kind: match self.kind {
                UnresolvedTypeKind::Variable(_) => return None,
                UnresolvedTypeKind::Parameter(param) => {
                    if generic {
                        TypeKind::Parameter(param)
                    } else {
                        panic!("cannot finalize type parameter in non-generic context")
                    }
                }
                UnresolvedTypeKind::Named(name) => TypeKind::Named(name),
                UnresolvedTypeKind::Function(input, output) => TypeKind::Function(
                    Box::new(input.finalize(ctx, generic)?),
                    Box::new(output.finalize(ctx, generic)?),
                ),
                UnresolvedTypeKind::Builtin(builtin) => TypeKind::Builtin(builtin),
                UnresolvedTypeKind::Bottom(is_error) => TypeKind::Bottom(is_error),
            },
        })
    }
}
