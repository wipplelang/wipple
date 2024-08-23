pub mod errors;
pub mod expression;
pub mod item;
pub mod pattern;
pub mod r#trait;
pub mod r#type;
pub mod types;

use crate::{
    infer::{
        errors::QueuedError,
        types::{context::TypeContext, Instance, Type, TypeVariable},
    },
    Driver,
};
use derivative::Derivative;
use std::collections::{HashMap, HashSet};
use wipple_util::WithInfo;

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Expression<D: Driver> {
    pub r#type: Type<D>,
    pub kind: ExpressionKind<D>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub enum ExpressionKind<D: Driver> {
    Unknown(Option<Box<ExpressionKind<D>>>),
    Variable(String, D::Path),
    UnresolvedConstant(D::Path),
    UnresolvedTrait(D::Path),
    ResolvedConstant {
        path: D::Path,
        parameters: Vec<Type<D>>,
        bounds: Vec<WithInfo<D::Info, Result<D::Path, Instance<D>>>>,
    },
    ResolvedTrait {
        trait_path: D::Path,
        parameters: Vec<Type<D>>,
        instance: WithInfo<D::Info, Result<D::Path, Instance<D>>>,
    },
    Number(String),
    Text(String),
    Block {
        statements: Vec<WithInfo<D::Info, Expression<D>>>,
        captures: Vec<D::Path>,
        top_level: bool,
    },
    Do(WithInfo<D::Info, Box<Expression<D>>>),
    Function {
        inputs: Vec<WithInfo<D::Info, crate::Pattern<D>>>,
        body: WithInfo<D::Info, Box<Expression<D>>>,
        captures: Vec<D::Path>,
    },
    Call {
        function: WithInfo<D::Info, Box<Expression<D>>>,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    When {
        input: WithInfo<D::Info, Box<Expression<D>>>,
        arms: Vec<WithInfo<D::Info, Arm<D>>>,
    },
    Intrinsic {
        name: String,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Initialize {
        pattern: WithInfo<D::Info, crate::Pattern<D>>,
        value: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Mutate {
        name: String,
        path: WithInfo<D::Info, D::Path>,
        value: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Marker(D::Path),
    UnresolvedStructure(Vec<WithInfo<D::Info, StructureFieldValue<D>>>),
    ResolvedStructure {
        structure: D::Path,
        fields: Vec<WithInfo<D::Info, StructureFieldValue<D>>>,
    },
    Variant {
        variant: WithInfo<D::Info, D::Path>,
        values: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Wrapper(WithInfo<D::Info, Box<Expression<D>>>),
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),
    Format {
        segments: Vec<FormatSegment<WithInfo<D::Info, Expression<D>>>>,
        trailing: String,
    },
}

impl<D: Driver> ExpressionKind<D> {
    pub fn is_reference(&self) -> bool {
        matches!(
            self,
            ExpressionKind::UnresolvedConstant(_)
                | ExpressionKind::UnresolvedTrait(_)
                | ExpressionKind::ResolvedConstant { .. }
                | ExpressionKind::ResolvedTrait { .. }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormattedText<T> {
    pub segments: Vec<FormatSegment<T>>,
    pub trailing: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatSegment<T> {
    pub text: String,
    pub value: T,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct StructureFieldValue<D: Driver> {
    pub name: String,
    pub value: WithInfo<D::Info, Expression<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Arm<D: Driver> {
    pub pattern: WithInfo<D::Info, crate::Pattern<D>>,
    pub body: WithInfo<D::Info, Expression<D>>,
}

pub struct InferContext<'a, D: Driver> {
    pub driver: &'a D,
    pub type_context: &'a mut TypeContext<D>,
    pub error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    pub errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    pub variables: &'a mut HashMap<D::Path, Type<D>>,
}

pub struct ResolveContext<'a, D: Driver> {
    pub driver: &'a D,
    pub type_context: &'a mut TypeContext<D>,
    pub error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    pub errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    pub variables: &'a mut HashMap<D::Path, Type<D>>,
    pub recursion_stack: &'a mut Vec<D::Info>,
    pub bound_instances: Vec<Vec<WithInfo<D::Info, Instance<D>>>>,
}

pub struct FinalizeContext<'a, D: Driver> {
    pub driver: &'a D,
    pub type_context: &'a mut TypeContext<D>,
    pub bound_instances: Vec<Vec<WithInfo<D::Info, Instance<D>>>>,
    pub error_queue: Option<&'a mut Vec<WithInfo<D::Info, QueuedError<D>>>>,
    pub errors: Option<&'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
    pub unresolved_variables: Option<&'a mut HashSet<TypeVariable<D>>>,
    pub contains_unknown: bool,
    pub subexpression_types: Option<&'a mut Vec<crate::Type<D>>>, // in the order of traversal
}

impl<'a, 'b: 'a, D: Driver> InferContext<'a, D> {
    pub fn from_resolve_context(context: &'a mut ResolveContext<'b, D>) -> Self {
        InferContext {
            driver: context.driver,
            type_context: context.type_context,
            error_queue: context.error_queue,
            errors: context.errors,
            variables: context.variables,
        }
    }
}
