use std::ops::ControlFlow;
use wipple_linker::Driver;
use wipple_util::WithInfo;

pub trait Visitor<D: Driver> {
    fn expression(
        &mut self,
        expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
    ) -> ControlFlow<()> {
        let _ = expression;
        ControlFlow::Continue(())
    }

    fn pattern(
        &mut self,
        pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
    ) -> ControlFlow<()> {
        let _ = pattern;
        ControlFlow::Continue(())
    }

    fn r#type(&mut self, r#type: WithInfo<D::Info, &wipple_typecheck::Type<D>>) -> ControlFlow<()> {
        let _ = r#type;
        ControlFlow::Continue(())
    }
}

#[allow(unused)]
pub fn expression_visitor<D: Driver>(
    f: impl FnMut(WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>) -> ControlFlow<()>,
) -> impl Visitor<D> {
    struct Visitor<F>(F);

    impl<D: Driver, F> self::Visitor<D> for Visitor<F>
    where
        F: FnMut(WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>) -> ControlFlow<()>,
    {
        fn expression(
            &mut self,
            expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
        ) -> ControlFlow<()> {
            (self.0)(expression)
        }
    }

    Visitor(f)
}

#[allow(unused)]
pub fn pattern_visitor<D: Driver>(
    f: impl FnMut(WithInfo<D::Info, &wipple_typecheck::Pattern<D>>) -> ControlFlow<()>,
) -> impl Visitor<D> {
    struct Visitor<F>(F);

    impl<D: Driver, F> self::Visitor<D> for Visitor<F>
    where
        F: FnMut(WithInfo<D::Info, &wipple_typecheck::Pattern<D>>) -> ControlFlow<()>,
    {
        fn pattern(
            &mut self,
            pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
        ) -> ControlFlow<()> {
            (self.0)(pattern)
        }
    }

    Visitor(f)
}

#[allow(unused)]
pub fn type_visitor<D: Driver>(
    f: impl FnMut(WithInfo<D::Info, &wipple_typecheck::Type<D>>) -> ControlFlow<()>,
) -> impl Visitor<D> {
    struct Visitor<F>(F);

    impl<D: Driver, F> self::Visitor<D> for Visitor<F>
    where
        F: FnMut(WithInfo<D::Info, &wipple_typecheck::Type<D>>) -> ControlFlow<()>,
    {
        fn r#type(
            &mut self,
            r#type: WithInfo<D::Info, &wipple_typecheck::Type<D>>,
        ) -> ControlFlow<()> {
            (self.0)(r#type)
        }
    }

    Visitor(f)
}

#[allow(unused)]
pub fn traverse_expression<D: Driver>(
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
    mut visitor: impl Visitor<D>,
) {
    let _ = traverse_expression_inner(expression, &mut visitor);
}

#[allow(unused)]
pub fn traverse_pattern<D: Driver>(
    pattern: WithInfo<D::Info, &wipple_typecheck::Pattern<D>>,
    mut visitor: impl Visitor<D>,
) {
    let _ = traverse_pattern_inner(pattern, &mut visitor);
}

#[allow(unused)]
pub fn traverse_type<D: Driver>(
    r#type: WithInfo<D::Info, &wipple_typecheck::Type<D>>,
    mut visitor: impl Visitor<D>,
) {
    let _ = traverse_type_inner(r#type, &mut visitor);
}

#[must_use]
fn traverse_expression_inner<D: Driver>(
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
    visitor: &mut dyn Visitor<D>,
) -> ControlFlow<()> {
    use wipple_typecheck::TypedExpressionKind::*;

    visitor.expression(expression.as_deref())?;

    match &expression.item.kind {
        Block { statements, .. } => {
            for statement in statements {
                traverse_expression_inner(statement.as_ref(), visitor)?;
            }
        }
        Do(expression) => {
            traverse_expression_inner(expression.as_deref(), visitor)?;
        }
        Function { inputs, body, .. } => {
            for input in inputs {
                traverse_pattern_inner(input.as_ref(), visitor)?;
            }

            traverse_expression_inner(body.as_deref(), visitor)?;
        }
        Call { function, inputs } => {
            traverse_expression_inner(function.as_deref(), visitor)?;

            for input in inputs {
                traverse_expression_inner(input.as_ref(), visitor)?;
            }
        }
        When { input, arms } => {
            traverse_expression_inner(input.as_deref(), visitor)?;

            for arm in arms {
                traverse_pattern_inner(arm.item.pattern.as_ref(), visitor)?;
                traverse_expression_inner(arm.item.body.as_ref(), visitor)?;
            }
        }
        Intrinsic { inputs, .. } => {
            for input in inputs {
                traverse_expression_inner(input.as_ref(), visitor)?;
            }
        }
        Initialize { pattern, value } => {
            traverse_pattern_inner(pattern.as_ref(), visitor)?;

            traverse_expression_inner(value.as_deref(), visitor)?;
        }
        Mutate { value, .. } => {
            traverse_expression_inner(value.as_deref(), visitor)?;
        }
        Structure { fields, .. } => {
            for field in fields {
                traverse_expression_inner(field.item.value.as_ref(), visitor)?;
            }
        }
        Variant { values, .. } => {
            for value in values {
                traverse_expression_inner(value.as_ref(), visitor)?;
            }
        }
        Wrapper(expression) => {
            traverse_expression_inner(expression.as_deref(), visitor)?;
        }
        Tuple(elements) => {
            for element in elements {
                traverse_expression_inner(element.as_ref(), visitor)?;
            }
        }
        Format { segments, .. } => {
            for segment in segments {
                traverse_expression_inner(segment.value.as_ref(), visitor)?;
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
    visitor: &mut dyn Visitor<D>,
) -> ControlFlow<()> {
    use wipple_typecheck::Pattern::*;

    visitor.pattern(pattern.as_deref())?;

    match pattern.item {
        Destructure { field_patterns, .. } => {
            for field_pattern in field_patterns {
                traverse_pattern_inner(field_pattern.item.pattern.as_ref(), visitor)?;
            }
        }
        Variant { value_patterns, .. } => {
            for pattern in value_patterns {
                traverse_pattern_inner(pattern.as_ref(), visitor)?;
            }
        }
        Wrapper { value_pattern, .. } => {
            traverse_pattern_inner(value_pattern.as_deref(), visitor)?;
        }
        Tuple(elements) => {
            for element in elements {
                traverse_pattern_inner(element.as_ref(), visitor)?;
            }
        }
        Or { left, right } => {
            traverse_pattern_inner(left.as_deref(), visitor)?;
            traverse_pattern_inner(right.as_deref(), visitor)?;
        }
        Annotate { pattern, r#type } => {
            traverse_pattern_inner(pattern.as_deref(), visitor)?;
            traverse_type_inner(r#type.as_ref(), visitor)?;
        }
        Unknown | Wildcard | Number(_) | Text(_) | Variable(_, _) => {}
    }

    ControlFlow::Continue(())
}

#[must_use]
fn traverse_type_inner<D: Driver>(
    r#type: WithInfo<D::Info, &wipple_typecheck::Type<D>>,
    visitor: &mut dyn Visitor<D>,
) -> ControlFlow<()> {
    use wipple_typecheck::Type::*;

    visitor.r#type(r#type.as_deref());

    match r#type.item {
        Declared { parameters, .. } => {
            for parameter in parameters {
                traverse_type_inner(parameter.as_ref(), visitor)?;
            }
        }
        Function { inputs, output } => {
            for input in inputs {
                traverse_type_inner(input.as_ref(), visitor)?;
            }

            traverse_type_inner(output.as_deref(), visitor)?;
        }
        Tuple(elements) => {
            for element in elements {
                traverse_type_inner(element.as_ref(), visitor)?;
            }
        }
        Block(body) => {
            traverse_type_inner(body.as_deref(), visitor)?;
        }
        Message { segments, .. } => {
            for segment in segments {
                traverse_type_inner(segment.r#type.as_ref(), visitor)?;
            }
        }
        Equal { left, right } => {
            traverse_type_inner(left.as_deref(), visitor)?;
            traverse_type_inner(right.as_deref(), visitor)?;
        }
        Unknown | Parameter(_) | Intrinsic => {}
    }

    ControlFlow::Continue(())
}
