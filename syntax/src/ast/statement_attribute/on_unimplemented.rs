use crate::{
    ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct OnUnimplementedStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub on_unimplemented_span: D::Span,
    pub message_span: D::Span,
    pub segments: Vec<(
        D::InternedString,
        Result<(D::Span, D::InternedString), SyntaxError<D>>,
    )>,
    pub trailing_segment: Option<D::InternedString>,
}

impl<D: Driver> OnUnimplementedStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct OnUnimplementedStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for OnUnimplementedStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "on-unimplemented",
            |context, span, on_unimplemented_span, exprs, _scope| async move {
                if exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`on-unimplemented` accepts at least 1 input");
                }

                let mut exprs = exprs.into_iter();

                let expr = exprs.next().unwrap();
                let message = match expr.kind {
                    parse::ExprKind::Text(text, _) => text,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected text");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let mut segments = message
                    .as_ref()
                    .split('_')
                    .map(|segment| context.ast_builder.driver.intern(segment))
                    .collect::<Vec<_>>();

                let inputs = exprs
                    .map(|expr| match expr.kind {
                        parse::ExprKind::Name(name, _) => Ok((expr.span, name)),
                        _ => {
                            context
                                .ast_builder
                                .driver
                                .syntax_error(expr.span, "expected a type parameter");

                            Err(context.ast_builder.syntax_error(expr.span))
                        }
                    })
                    .collect::<Vec<_>>();

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

                let attribute = OnUnimplementedStatementAttribute {
                    span,
                    on_unimplemented_span,
                    message_span: expr.span,
                    segments,
                    trailing_segment,
                };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .on_unimplemented = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
