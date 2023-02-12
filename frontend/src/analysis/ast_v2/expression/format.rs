use crate::{
    analysis::ast_v2::{
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct FormatExpression {
    pub segments: Vec<(InternedString, Result<Expression, SyntaxError>)>,
    pub trailing_segment: Option<InternedString>,
}

pub struct FormatExpressionSyntax;

impl Syntax for FormatExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "format",
            |context, span, exprs, scope| async move {
                if exprs.is_empty() {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`format` accepts at least 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let format_text = exprs.next().unwrap();
                let format_text = match format_text.kind {
                    parse::ExprKind::Text(text) => text,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "expected text here")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let mut segments = format_text
                    .to_string()
                    .split('_')
                    .map(InternedString::new)
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
                    context.ast_builder.compiler.add_error(
                        "wrong number of inputs to `format`",
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

                Ok(FormatExpression {
                    segments,
                    trailing_segment
                }
                .into())
            },
        ))
    }
}
