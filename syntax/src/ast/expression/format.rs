use crate::{
    ast::{
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    parse, Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct FormatExpression<D: Driver> {
    pub span: D::Span,
    pub format_span: D::Span,
    pub text_span: D::Span,
    pub segments: Vec<(D::InternedString, Result<Expression<D>, SyntaxError<D>>)>,
    pub trailing_segment: Option<D::InternedString>,
}

impl<D: Driver> FormatExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct FormatExpressionSyntax;

impl<D: Driver> Syntax<D> for FormatExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "format",
            |context, span, format_span, exprs, scope| async move {
                if exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`format` accepts at least 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let format_text = exprs.next().unwrap();
                let text_span = format_text.span;
                let format_text = match format_text.kind {
                    parse::ExprKind::Text(text, _) => text,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected text here");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let mut segments = format_text
                    .as_ref()
                    .split('_')
                    .map(|segment| context.ast_builder.driver.intern(segment))
                    .collect::<Vec<_>>();

                let inputs = stream::iter(exprs)
                    .then(|expr| {
                        context.ast_builder.build_expr::<ExpressionSyntax>(
                            context.clone(),
                            expr,
                            scope,
                        )
                    })
                    .collect::<Vec<_>>()
                    .await;

                let trailing_segment =
                    (segments.len() > inputs.len()).then(|| segments.pop().unwrap());

                if segments.len() != inputs.len() {
                    context.ast_builder.driver.syntax_error(
                        span,
                        format!(
                            "format string contains {} placeholders, but {} inputs were provided",
                            segments.len(),
                            inputs.len()
                        ),
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let segments = segments.into_iter().zip(inputs).collect();

                Ok(FormatExpression {
                    span,
                    format_span,
                    text_span,
                    segments,
                    trailing_segment,
                }
                .into())
            },
        ))
    }
}
