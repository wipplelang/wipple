use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext, WhenBody, WhenBodySyntax,
        WhenBodySyntaxContext,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct WhenExpression<D: Driver> {
    pub span: D::Span,
    pub when_span: D::Span,
    pub input: Result<Box<Expression<D>>, SyntaxError<D>>,
    pub body: Result<WhenBody<D>, SyntaxError<D>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for WhenExpression<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(WhenExpression {
            span: Default::default(),
            when_span: Default::default(),
            input: arbitrary::Arbitrary::arbitrary(u)?,
            body: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> WhenExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for WhenExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "(when {} {})",
            self.input?.format()?,
            self.body?.format()?,
        ))
    }
}

pub struct WhenExpressionSyntax;

impl<D: Driver> Syntax<D> for WhenExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "when",
            |context, span, when_span, exprs, scope| async move {
                if exprs.len() != 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`when` accepts 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let input = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), exprs.next().unwrap(), scope)
                    .await;

                let body = context
                    .ast_builder
                    .build_expr::<WhenBodySyntax>(
                        WhenBodySyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        exprs.next().unwrap(),
                        scope,
                    )
                    .await;

                Ok(WhenExpression {
                    span,
                    when_span,
                    input: input.map(Box::new),
                    body,
                }
                .into())
            },
        ))
    }
}
