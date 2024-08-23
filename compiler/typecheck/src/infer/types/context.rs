//! The type context.

use crate::{
    infer::{errors::ErrorReason, types::Type, Expression, InferContext},
    Driver,
};
use derivative::Derivative;
use std::{collections::BTreeMap, fmt::Debug};
use wipple_util::WithInfo;

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct TypeContext<D: Driver> {
    pub next_variable: u32,
    pub tracked_expressions: Vec<Option<WithInfo<D::Info, Expression<D>>>>,
    pub substitutions: BTreeMap<u32, Type<D>>,
    pub defaults: BTreeMap<u32, Type<D>>,
    pub reasons: Vec<WithInfo<D::Info, ErrorReason<D>>>,
}

impl<D: Driver> TypeContext<D> {
    pub fn replace_with(&mut self, other: Self) {
        self.next_variable = other.next_variable;
        self.substitutions = other.substitutions;
        self.defaults = other.defaults;

        self.reasons = other.reasons;
        self.reasons.sort_by_key(|reason| reason.info.clone());
        self.reasons.dedup();
    }

    pub fn tracked_expression(
        &self,
        id: TrackedExpressionId<D>,
    ) -> &WithInfo<D::Info, Expression<D>> {
        self.tracked_expressions[id.counter as usize]
            .as_ref()
            .expect("uninitialized tracked expression")
    }

    pub fn add_reason(&mut self, reason: WithInfo<D::Info, ErrorReason<D>>) {
        self.reasons.push(reason);
    }
}

#[derive(Derivative)]
#[derivative(
    Copy(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct TrackedExpressionId<D: Driver> {
    _driver: std::marker::PhantomData<D>,
    counter: u32,
}

impl<D: Driver> Clone for TrackedExpressionId<D> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<D: Driver> Debug for TrackedExpressionId<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TrackedExpressionId({})", self.counter)
    }
}

impl<D: Driver> InferContext<'_, D> {
    pub fn with_tracked_expression(
        &mut self,
        parent_id: Option<TrackedExpressionId<D>>,
        expression: impl FnOnce(&mut Self, TrackedExpressionId<D>) -> WithInfo<D::Info, Expression<D>>,
    ) -> WithInfo<D::Info, Expression<D>> {
        let id = TrackedExpressionId {
            _driver: std::marker::PhantomData,
            counter: self.type_context.tracked_expressions.len() as u32,
        };

        self.type_context.tracked_expressions.push(None);

        let mut expression = expression(self, id);
        expression.item.r#type = expression.item.r#type.with_expression(id, parent_id);

        self.type_context.tracked_expressions[id.counter as usize] = Some(expression.clone());

        expression
    }
}
