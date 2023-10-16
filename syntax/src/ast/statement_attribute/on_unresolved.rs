use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{parse_interpolated_text, Syntax, SyntaxRule, SyntaxRules},
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
    pub param: (D::Span, D::InternedString),
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
                let message_span = expr.span;
                let (segments, trailing_segment) =
                    parse_interpolated_text(&context.ast_builder, expr, |expr| match expr.kind {
                        parse::ExprKind::Name(name, _) => Ok((expr.span, name)),
                        _ => {
                            context
                                .ast_builder
                                .driver
                                .syntax_error(expr.span, "expected a type parameter");

                            Err(context.ast_builder.syntax_error(expr.span))
                        }
                    })?;

                let segments = segments
                    .into_iter()
                    .map(|(string, param)| OnUnresolvedSegment { string, param })
                    .collect();

                let attribute = OnUnresolvedStatementAttribute {
                    span,
                    on_unresolved_span,
                    message_span,
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
