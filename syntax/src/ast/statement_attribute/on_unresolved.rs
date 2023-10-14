use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct OnUnresolvedStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub on_unresolved_span: D::Span,
    pub message_span: D::Span,
    pub segments: Vec<OnUnresolvedSegment<D>>,
    pub trailing_segment: Option<D::InternedString>,
}

#[derive(Debug, Clone)]
pub struct OnUnresolvedSegment<D: Driver> {
    pub string: D::InternedString,
    pub param: Result<(D::Span, D::InternedString), SyntaxError<D>>,
}

impl<D: Driver> OnUnresolvedStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for OnUnresolvedStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct OnUnresolvedStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for OnUnresolvedStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "on-unresolved",
            |context, span, on_unresolved_span, exprs, _scope_set| async move {
                if exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`on-unresolved` accepts at least 1 input");
                }

                let mut exprs = exprs.into_iter();

                let expr = exprs.next().unwrap();
                let message = match expr.kind {
                    parse::ExprKind::Text(text) => text.ignoring_escaped_underscores(),
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

                let segments = segments
                    .into_iter()
                    .zip(inputs)
                    .map(|(string, ty)| OnUnresolvedSegment { string, param: ty })
                    .collect();

                let attribute = OnUnresolvedStatementAttribute {
                    span,
                    on_unresolved_span,
                    message_span: expr.span,
                    segments,
                    trailing_segment,
                };

                context.statement_attributes.unwrap().lock().on_unresolved =
                    Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
