use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct OnUnimplementedStatementAttribute {
    pub span: SpanList,
    pub on_unimplemented_span: SpanList,
    pub message_span: SpanList,
    pub segments: Vec<(
        InternedString,
        Result<(SpanList, InternedString), SyntaxError>,
    )>,
    pub trailing_segment: Option<InternedString>,
}

impl OnUnimplementedStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct OnUnimplementedStatementAttributeSyntax;

impl Syntax for OnUnimplementedStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "on-unimplemented",
            |context, span, on_unimplemented_span, exprs, _scope| async move {
                if exprs.is_empty() {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(
                            span,
                            "`on-unimplemented` accepts at least 1 input",
                        )],
                    );
                }

                let mut exprs = exprs.into_iter();

                let expr = exprs.next().unwrap();
                let message = match expr.kind {
                    parse::ExprKind::Text(text) => text,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected text")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let mut segments = message
                    .to_string()
                    .split('_')
                    .map(InternedString::new)
                    .collect::<Vec<_>>();

                let inputs = exprs
                    .map(|expr| {
                        match expr.kind {
                            parse::ExprKind::Name(name, _) => {
                                Ok((expr.span, name))
                            }
                            _ => {
                                context.ast_builder.compiler.add_error(
                                    "syntax error",
                                    vec![Note::primary(expr.span, "expected a type parameter")],
                                );

                                Err(context.ast_builder.syntax_error(expr.span))
                            }
                        }
                    })
                    .collect::<Vec<_>>();

                let trailing_segment =
                    (segments.len() > inputs.len()).then(|| segments.pop().unwrap());

                if segments.len() != inputs.len() {
                    context.ast_builder.compiler.add_error(
                        "wrong number of inputs to `on-unimplemented`",
                        vec![Note::primary(
                            span,
                            format!(
                                "format string contains {} placeholders, but {} inputs were provided",
                                segments.len(),
                                inputs.len()
                            ),
                        )],
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
