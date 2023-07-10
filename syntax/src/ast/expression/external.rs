use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    parse, Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct ExternalExpression<D: Driver> {
    pub span: D::Span,
    pub external_span: D::Span,
    pub namespace_span: D::Span,
    pub namespace: D::InternedString,
    pub identifier_span: D::Span,
    pub identifier: D::InternedString,
    pub inputs: Vec<Result<Expression<D>, SyntaxError<D>>>,
}

impl<D: Driver> ExternalExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for ExternalExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "(external \"{}\" \"{}\" {})",
            self.namespace.as_ref(),
            self.identifier.as_ref(),
            self.inputs
                .into_iter()
                .map(|result| result?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

pub struct ExternalExpressionSyntax;

impl<D: Driver> Syntax<D> for ExternalExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "external",
            |context, span, external_span, exprs, scope| async move {
                if exprs.len() < 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`external` accepts at least 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let namespace = exprs.next().unwrap();
                let namespace_span = namespace.span;
                let namespace = match namespace.kind {
                    parse::ExprKind::Text(text, _) => text,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected text here");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let identifier = exprs.next().unwrap();
                let identifier_span = identifier.span;
                let identifier = match identifier.kind {
                    parse::ExprKind::Text(text, _) => text,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected text here");

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
                    span,
                    external_span,
                    namespace_span,
                    namespace,
                    identifier_span,
                    identifier,
                    inputs,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::EXTERNAL]
}
