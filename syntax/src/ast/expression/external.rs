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
    pub path_span: D::Span,
    pub path: D::InternedString,
    pub name_span: D::Span,
    pub name: D::InternedString,
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
            self.path.as_ref(),
            self.name.as_ref(),
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
            |context, span, external_span, exprs, scope_set| async move {
                if exprs.len() < 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`external` accepts at least 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let namespace = exprs.next().unwrap();
                let path_span = namespace.span;
                let path = match namespace.kind {
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
                let name_span = identifier.span;
                let name = match identifier.kind {
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
                            scope_set.clone(),
                        )
                    })
                    .collect()
                    .await;

                Ok(ExternalExpression {
                    span,
                    external_span,
                    path_span,
                    path,
                    name_span,
                    name,
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
