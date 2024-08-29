//! Types and type variables.

pub mod context;
pub mod instantiate;
pub mod unify;

use crate::{
    infer::{
        types::context::{TrackedExpressionId, TypeContext},
        FormatSegment, ResolveContext,
    },
    Driver,
};
use derivative::Derivative;
use std::{collections::btree_map, fmt::Debug};

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), PartialEq(bound = ""))]
pub struct Type<D: Driver> {
    pub kind: TypeKind<D>,
    pub info: D::Info,
    #[derivative(PartialEq = "ignore")]
    pub expression: Option<TrackedExpressionId<D>>,
    #[derivative(PartialEq = "ignore")]
    pub parent_expression: Option<TrackedExpressionId<D>>,
}

impl<D: Driver> Type<D> {
    pub fn new(kind: TypeKind<D>, info: D::Info) -> Self {
        Type {
            kind,
            info,
            expression: None,
            parent_expression: None,
        }
    }

    pub fn with_expression(
        mut self,
        expression: TrackedExpressionId<D>,
        parent: Option<TrackedExpressionId<D>>,
    ) -> Self {
        self.expression = Some(expression);
        self.parent_expression = parent;
        self
    }

    pub fn is_currently_unknown(&self) -> bool {
        matches!(
            self.kind,
            TypeKind::Variable(_) | TypeKind::Opaque(_) | TypeKind::Unknown
        )
    }

    pub fn contains_variable(&self, variable: &TypeVariable<D>) -> bool {
        match &self.kind {
            TypeKind::Variable(var) => var.counter == variable.counter,
            TypeKind::Declared { parameters, .. } => parameters
                .iter()
                .any(|r#type| r#type.contains_variable(variable)),
            TypeKind::Function { inputs, output } => {
                inputs
                    .iter()
                    .any(|r#type| r#type.contains_variable(variable))
                    || output.contains_variable(variable)
            }
            TypeKind::Tuple(elements) => elements
                .iter()
                .any(|r#type| r#type.contains_variable(variable)),
            TypeKind::Block(r#type) => r#type.contains_variable(variable),
            TypeKind::Message { segments, .. } => segments
                .iter()
                .any(|segment| segment.value.contains_variable(variable)),
            TypeKind::Equal { left, right } => {
                left.contains_variable(variable) || right.contains_variable(variable)
            }
            TypeKind::Opaque(_)
            | TypeKind::Parameter(_)
            | TypeKind::Unknown
            | TypeKind::Intrinsic => false,
        }
    }

    #[must_use]
    pub fn apply_in_context(&self, context: &mut TypeContext<D>) -> Self {
        let mut r#type = self.clone();
        r#type.apply_in_context_mut(context);
        r#type
    }

    pub fn apply_in_context_mut(&mut self, context: &mut TypeContext<D>) {
        match &mut self.kind {
            TypeKind::Variable(variable) => {
                let r#type =
                    match variable.with_substitution_mut(context, |substitution| match substitution
                    {
                        btree_map::Entry::Vacant(_) => None,
                        btree_map::Entry::Occupied(entry) => Some(entry.get().clone()),
                    }) {
                        Some(r#type) => r#type,
                        _ => return,
                    };

                assert!(!r#type.contains_variable(variable), "recursive type");

                self.kind = r#type.kind;

                if r#type.expression.is_some() {
                    self.info = r#type.info;
                }

                self.expression = self.expression.or(r#type.expression);
                self.parent_expression = self.parent_expression.or(r#type.parent_expression);
                self.apply_in_context_mut(context);
            }
            TypeKind::Opaque(_) => {}
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.apply_in_context_mut(context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.apply_in_context_mut(context);
                }

                output.apply_in_context_mut(context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.apply_in_context_mut(context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.apply_in_context_mut(context);
            }
            TypeKind::Message { segments, .. } => {
                for segment in segments {
                    segment.value.apply_in_context_mut(context);
                }
            }
            TypeKind::Equal { left, right } => {
                left.apply_in_context_mut(context);
                right.apply_in_context_mut(context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }

    pub fn set_source_info(&mut self, context: &mut ResolveContext<'_, D>, info: &D::Info) {
        self.apply_in_context_mut(context.type_context);

        match &mut self.kind {
            TypeKind::Declared { path, parameters } => {
                if context
                    .driver
                    .path_for_language_type("source")
                    .is_some_and(|source_path| *path == source_path)
                {
                    self.info = info.clone();
                }

                for r#type in parameters {
                    r#type.set_source_info(context, info);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.set_source_info(context, info);
                }

                output.set_source_info(context, info);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.set_source_info(context, info);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.set_source_info(context, info);
            }
            TypeKind::Message { segments, .. } => {
                for segment in segments {
                    segment.value.set_source_info(context, info);
                }
            }
            TypeKind::Equal { left, right } => {
                left.set_source_info(context, info);
                right.set_source_info(context, info);
            }
            TypeKind::Variable(_)
            | TypeKind::Opaque(_)
            | TypeKind::Parameter(_)
            | TypeKind::Unknown
            | TypeKind::Intrinsic => {}
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), PartialEq(bound = ""))]
pub enum TypeKind<D: Driver> {
    Variable(TypeVariable<D>),
    Opaque(TypeVariable<D>),
    Parameter(D::Path),
    Declared {
        path: D::Path,
        parameters: Vec<Type<D>>,
    },
    Function {
        inputs: Vec<Type<D>>,
        output: Box<Type<D>>,
    },
    Tuple(Vec<Type<D>>),
    Block(Box<Type<D>>),
    Intrinsic,
    Message {
        segments: Vec<FormatSegment<Type<D>>>,
        trailing: String,
    },
    Equal {
        left: Box<Type<D>>,
        right: Box<Type<D>>,
    },
    Unknown,
}

#[derive(Derivative)]
#[derivative(
    Copy(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct TypeVariable<D: Driver> {
    pub _driver: std::marker::PhantomData<D>,
    pub counter: u32,
}

impl<D: Driver> Clone for TypeVariable<D> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<D: Driver> Debug for TypeVariable<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeVariable({})", self.counter)
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), PartialEq(bound = ""))]
pub struct Instance<D: Driver> {
    pub r#trait: D::Path,
    pub parameters: Vec<Type<D>>,
}
