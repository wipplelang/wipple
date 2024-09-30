use std::ops::ControlFlow;
use wipple_linker::Driver;
use wipple_util::WithInfo;

pub trait Visitor<D: Driver> {
    fn expression(
        &mut self,
        expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
        parent: Option<WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>>,
    ) -> ControlFlow<()> {
        let _ = expression;
        let _ = parent;
        ControlFlow::Continue(())
    }

    fn pattern(
        &mut self,
        pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
        parent: Option<WithInfo<D::Info, &wipple_typecheck::Pattern<D>>>,
    ) -> ControlFlow<()> {
        let _ = pattern;
        let _ = parent;
        ControlFlow::Continue(())
    }

    fn r#type(
        &mut self,
        r#type: WithInfo<D::Info, &wipple_typecheck::Type<D>>,
        parent: Option<WithInfo<D::Info, &wipple_typecheck::Type<D>>>,
    ) -> ControlFlow<()> {
        let _ = r#type;
        let _ = parent;
        ControlFlow::Continue(())
    }
}

#[allow(unused)]
pub fn expression_visitor<D: Driver>(
    f: impl FnMut(
        WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
        Option<WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>>,
    ) -> ControlFlow<()>,
) -> impl Visitor<D> {
    struct Visitor<F>(F);

    impl<D: Driver, F> self::Visitor<D> for Visitor<F>
    where
        F: FnMut(
            WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
            Option<WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>>,
        ) -> ControlFlow<()>,
    {
        fn expression(
            &mut self,
            expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
            parent: Option<WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>>,
        ) -> ControlFlow<()> {
            (self.0)(expression, parent)
        }
    }

    Visitor(f)
}

#[allow(unused)]
pub fn pattern_visitor<D: Driver>(
    f: impl FnMut(
        WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
        Option<WithInfo<D::Info, &wipple_typecheck::Pattern<D>>>,
    ) -> ControlFlow<()>,
) -> impl Visitor<D> {
    struct Visitor<F>(F);

    impl<D: Driver, F> self::Visitor<D> for Visitor<F>
    where
        F: FnMut(
            WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
            Option<WithInfo<D::Info, &wipple_typecheck::Pattern<D>>>,
        ) -> ControlFlow<()>,
    {
        fn pattern(
            &mut self,
            pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
            parent: Option<WithInfo<D::Info, &wipple_typecheck::Pattern<D>>>,
        ) -> ControlFlow<()> {
            (self.0)(pattern, parent)
        }
    }

    Visitor(f)
}

#[allow(unused)]
pub fn type_visitor<D: Driver>(
    f: impl FnMut(
        WithInfo<D::Info, &wipple_typecheck::Type<D>>,
        Option<WithInfo<D::Info, &wipple_typecheck::Type<D>>>,
    ) -> ControlFlow<()>,
) -> impl Visitor<D> {
    struct Visitor<F>(F);

    impl<D: Driver, F> self::Visitor<D> for Visitor<F>
    where
        F: FnMut(
            WithInfo<D::Info, &wipple_typecheck::Type<D>>,
            Option<WithInfo<D::Info, &wipple_typecheck::Type<D>>>,
        ) -> ControlFlow<()>,
    {
        fn r#type(
            &mut self,
            r#type: WithInfo<D::Info, &wipple_typecheck::Type<D>>,
            parent: Option<WithInfo<D::Info, &wipple_typecheck::Type<D>>>,
        ) -> ControlFlow<()> {
            (self.0)(r#type, parent)
        }
    }

    Visitor(f)
}

#[allow(unused)]
pub fn traverse_expression<D: Driver>(
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
    mut visitor: impl Visitor<D>,
) {
    let _ = traverse_expression_inner(expression, None, &mut visitor);
}

#[allow(unused)]
pub fn traverse_pattern<D: Driver>(
    pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
    mut visitor: impl Visitor<D>,
) {
    let _ = traverse_pattern_inner(pattern, None, &mut visitor);
}

#[allow(unused)]
pub fn traverse_type<D: Driver>(
    r#type: WithInfo<D::Info, &wipple_typecheck::Type<D>>,
    mut visitor: impl Visitor<D>,
) {
    let _ = traverse_type_inner(r#type, None, &mut visitor);
}

#[allow(unused)]
pub fn find_expression_with_info<D: Driver>(
    root: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
    info: &D::Info,
) -> Option<(
    WithInfo<D::Info, wipple_typecheck::TypedExpression<D>>,
    Option<WithInfo<D::Info, wipple_typecheck::TypedExpression<D>>>,
)> {
    let mut result = None;

    traverse_expression(
        root,
        expression_visitor(|expression, parent| {
            if expression.info == *info {
                result = Some((expression.cloned(), parent.map(|parent| parent.cloned())));
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        }),
    );

    result
}

#[must_use]
fn traverse_expression_inner<D: Driver>(
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
    parent: Option<WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>>,
    visitor: &mut dyn Visitor<D>,
) -> ControlFlow<()> {
    use wipple_typecheck::TypedExpressionKind::*;

    visitor.expression(expression.as_deref(), parent)?;

    match &expression.item.kind {
        Block { statements, .. } => {
            for statement in statements {
                traverse_expression_inner(
                    statement.as_ref(),
                    Some(expression.as_deref()),
                    visitor,
                )?;
            }
        }
        Do(body) => {
            traverse_expression_inner(body.as_deref(), Some(expression.as_deref()), visitor)?;
        }
        Function { inputs, body, .. } => {
            for input in inputs {
                traverse_pattern_inner(input.as_ref(), None, visitor)?;
            }

            traverse_expression_inner(body.as_deref(), Some(expression.as_deref()), visitor)?;
        }
        Call { function, inputs } => {
            traverse_expression_inner(function.as_deref(), Some(expression.as_deref()), visitor)?;

            for input in inputs {
                traverse_expression_inner(input.as_ref(), Some(expression.as_deref()), visitor)?;
            }
        }
        When { input, arms } => {
            traverse_expression_inner(input.as_deref(), Some(expression.as_deref()), visitor)?;

            for arm in arms {
                traverse_pattern_inner(arm.item.pattern.as_ref(), None, visitor)?;

                traverse_expression_inner(
                    arm.item.body.as_ref(),
                    Some(expression.as_deref()),
                    visitor,
                )?;
            }
        }
        Intrinsic { inputs, .. } => {
            for input in inputs {
                traverse_expression_inner(input.as_ref(), Some(expression.as_deref()), visitor)?;
            }
        }
        Initialize { pattern, value } => {
            traverse_pattern_inner(pattern.as_ref(), None, visitor)?;

            traverse_expression_inner(value.as_deref(), Some(expression.as_deref()), visitor)?;
        }
        Mutate { value, .. } => {
            traverse_expression_inner(value.as_deref(), Some(expression.as_deref()), visitor)?;
        }
        Structure { fields, .. } => {
            for field in fields {
                traverse_expression_inner(
                    field.item.value.as_ref(),
                    Some(expression.as_deref()),
                    visitor,
                )?;
            }
        }
        Variant { values, .. } => {
            for value in values {
                traverse_expression_inner(value.as_ref(), Some(expression.as_deref()), visitor)?;
            }
        }
        Wrapper(value) => {
            traverse_expression_inner(value.as_deref(), Some(expression.as_deref()), visitor)?;
        }
        Tuple(elements) => {
            for element in elements {
                traverse_expression_inner(element.as_ref(), Some(expression.as_deref()), visitor)?;
            }
        }
        Format { segments, .. } => {
            for segment in segments {
                traverse_expression_inner(
                    segment.value.as_ref(),
                    Some(expression.as_deref()),
                    visitor,
                )?;
            }
        }
        Unknown(_)
        | Variable(_, _)
        | Constant { .. }
        | Trait { .. }
        | Text(_)
        | Number(_)
        | Marker(_) => {}
    }

    ControlFlow::Continue(())
}

#[must_use]
fn traverse_pattern_inner<D: Driver>(
    pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
    parent: Option<WithInfo<D::Info, &wipple_typecheck::Pattern<D>>>,
    visitor: &mut dyn Visitor<D>,
) -> ControlFlow<()> {
    use wipple_typecheck::Pattern::*;

    visitor.pattern(pattern.as_deref(), parent)?;

    match pattern.item {
        Destructure { field_patterns, .. } => {
            for field_pattern in field_patterns {
                traverse_pattern_inner(
                    field_pattern.item.pattern.as_ref(),
                    Some(pattern.as_deref()),
                    visitor,
                )?;
            }
        }
        Variant { value_patterns, .. } => {
            for value_pattern in value_patterns {
                traverse_pattern_inner(value_pattern.as_ref(), Some(pattern.as_deref()), visitor)?;
            }
        }
        Wrapper { value_pattern, .. } => {
            traverse_pattern_inner(value_pattern.as_deref(), Some(pattern.as_deref()), visitor)?;
        }
        Tuple(elements) => {
            for element in elements {
                traverse_pattern_inner(element.as_ref(), Some(pattern.as_deref()), visitor)?;
            }
        }
        Or { left, right } => {
            traverse_pattern_inner(left.as_deref(), Some(pattern.as_deref()), visitor)?;
            traverse_pattern_inner(right.as_deref(), Some(pattern.as_deref()), visitor)?;
        }
        Annotate { pattern, r#type } => {
            traverse_pattern_inner(pattern.as_deref(), Some(pattern.as_deref()), visitor)?;
            traverse_type_inner(r#type.as_ref(), None, visitor)?;
        }
        Unknown | Wildcard | Number(_) | Text(_) | Variable(_, _) | Marker(_) => {}
    }

    ControlFlow::Continue(())
}

#[must_use]
fn traverse_type_inner<D: Driver>(
    r#type: WithInfo<D::Info, &wipple_typecheck::Type<D>>,
    parent: Option<WithInfo<D::Info, &wipple_typecheck::Type<D>>>,
    visitor: &mut dyn Visitor<D>,
) -> ControlFlow<()> {
    use wipple_typecheck::Type::*;

    visitor.r#type(r#type.as_deref(), parent);

    match r#type.item {
        Declared { parameters, .. } => {
            for parameter in parameters {
                traverse_type_inner(parameter.as_ref(), Some(r#type.as_deref()), visitor)?;
            }
        }
        Function { inputs, output } => {
            for input in inputs {
                traverse_type_inner(input.as_ref(), Some(r#type.as_deref()), visitor)?;
            }

            traverse_type_inner(output.as_deref(), Some(r#type.as_deref()), visitor)?;
        }
        Tuple(elements) => {
            for element in elements {
                traverse_type_inner(element.as_ref(), Some(r#type.as_deref()), visitor)?;
            }
        }
        Block(body) => {
            traverse_type_inner(body.as_deref(), Some(r#type.as_deref()), visitor)?;
        }
        Message { segments, .. } => {
            for segment in segments {
                traverse_type_inner(segment.r#type.as_ref(), Some(r#type.as_deref()), visitor)?;
            }
        }
        Equal { left, right } => {
            traverse_type_inner(left.as_deref(), Some(r#type.as_deref()), visitor)?;
            traverse_type_inner(right.as_deref(), Some(r#type.as_deref()), visitor)?;
        }
        Unknown | Parameter(_) | Intrinsic => {}
    }

    ControlFlow::Continue(())
}
