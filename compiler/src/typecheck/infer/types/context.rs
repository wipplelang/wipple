//! The type context.

use crate::{
    typecheck::infer::{Expression, InferContext, types::Type},
    util::WithInfo,
};
use std::{collections::BTreeMap, fmt::Debug};

#[derive(Clone, Default)]
pub struct TypeContext {
    pub next_variable: u32,
    pub tracked_expressions: Vec<Option<WithInfo<Expression>>>,
    pub substitutions: BTreeMap<u32, Type>,
    pub defaults: BTreeMap<u32, Type>,
}

impl TypeContext {
    pub fn replace_with(&mut self, other: Self) {
        self.next_variable = other.next_variable;
        self.substitutions = other.substitutions;
        self.defaults = other.defaults;
    }

    pub fn tracked_expression(&self, id: TrackedExpressionId) -> &WithInfo<Expression> {
        self.tracked_expressions[id.counter as usize]
            .as_ref()
            .expect("uninitialized tracked expression")
    }
}

#[derive(Copy, Hash)]
pub struct TrackedExpressionId {
    counter: u32,
}

impl Clone for TrackedExpressionId {
    fn clone(&self) -> Self {
        *self
    }
}

impl Debug for TrackedExpressionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TrackedExpressionId({})", self.counter)
    }
}

impl InferContext<'_> {
    pub fn with_tracked_expression(
        &mut self,
        parent_id: Option<TrackedExpressionId>,
        expression: impl FnOnce(&mut Self, TrackedExpressionId) -> WithInfo<Expression>,
    ) -> WithInfo<Expression> {
        let id = TrackedExpressionId {
            counter: self.type_context.tracked_expressions.len() as u32,
        };

        self.type_context.tracked_expressions.push(None);

        let mut expression = expression(self, id);
        expression.item.r#type = expression.item.r#type.with_expression(id, parent_id);

        self.type_context.tracked_expressions[id.counter as usize] = Some(expression.clone());

        expression
    }
}
