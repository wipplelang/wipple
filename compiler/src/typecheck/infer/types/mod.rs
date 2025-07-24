//! Types and type variables.

pub mod context;
pub mod instantiate;
pub mod unify;

use crate::{
    lower::Path,
    syntax::Location,
    typecheck::infer::{
        FormatSegment, ResolveContext,
        types::context::{TrackedExpressionId, TypeContext},
    },
};
use std::{collections::btree_map, fmt::Debug};

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub info: Location,
    pub expression: Option<TrackedExpressionId>,
    pub parent_expression: Option<TrackedExpressionId>,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.info == other.info
    }
}

impl Eq for Type {}

impl Type {
    pub fn new(kind: TypeKind, info: Location) -> Self {
        Type {
            kind,
            info,
            expression: None,
            parent_expression: None,
        }
    }

    pub fn with_expression(
        mut self,
        expression: TrackedExpressionId,
        parent: Option<TrackedExpressionId>,
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

    pub fn contains_variable(&self, variable: &TypeVariable) -> bool {
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
    pub fn apply_in_context(&self, context: &mut TypeContext) -> Self {
        let mut r#type = self.clone();
        r#type.apply_in_context_mut(context);
        r#type
    }

    pub fn apply_in_context_mut(&mut self, context: &mut TypeContext) {
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

    pub fn set_source_info(&mut self, context: &mut ResolveContext<'_>, info: &Location) {
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

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Variable(TypeVariable),
    Opaque(TypeVariable),
    Parameter(Path),
    Declared {
        path: Path,
        parameters: Vec<Type>,
    },
    Function {
        inputs: Vec<Type>,
        output: Box<Type>,
    },
    Tuple(Vec<Type>),
    Block(Box<Type>),
    Intrinsic,
    Message {
        segments: Vec<FormatSegment<Type>>,
        trailing: String,
    },
    Equal {
        left: Box<Type>,
        right: Box<Type>,
    },
    Unknown,
}

#[derive(Copy, PartialEq, Eq, Hash)]
pub struct TypeVariable {
    pub counter: u32,
}

impl Clone for TypeVariable {
    fn clone(&self) -> Self {
        *self
    }
}

impl Debug for TypeVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeVariable({})", self.counter)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub r#trait: Path,
    pub parameters: Vec<Type>,
}
