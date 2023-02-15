use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct ExternalExpression {
    pub external_span: Span,
    pub namespace: InternedString,
    pub identifier: InternedString,
    pub inputs: Vec<Result<Expression, SyntaxError>>,
}

impl ExternalExpression {
    pub fn span(&self) -> Span {
        match self.inputs.last() {
            Some(input) => match input {
                Ok(expr) => Span::join(self.external_span, expr.span()),
                Err(error) => error.span,
            },
            None => self.external_span,
        }
    }
}

pub struct ExternalExpressionSyntax;

impl Syntax for ExternalExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "external",
            |context, span, exprs, scope| async move {
                if exprs.len() < 2 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`external` accepts at least 2 inputs")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let namespace = exprs.next().unwrap();
                let namespace = match namespace.kind {
                    parse::ExprKind::Text(text) => text,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "expected text here")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let identifier = exprs.next().unwrap();
                let identifier = match identifier.kind {
                    parse::ExprKind::Text(text) => text,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "expected text here")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let inputs = stream::iter(exprs)
                    .then(|expr| {
                        context.ast_builder.build_expr::<ExpressionSyntax>(
                            context.clone(),
                            expr,
                            scope,
                        )
                    })
                    .collect()
                    .await;

                Ok(ExternalExpression {
                    external_span: span,
                    namespace,
                    identifier,
                    inputs,
                }
                .into())
            },
        ))
    }
}
