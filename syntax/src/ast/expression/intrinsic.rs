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
pub struct IntrinsicExpression<D: Driver> {
    pub span: D::Span,
    pub intrinsic_span: D::Span,
    pub name_span: D::Span,
    pub name: D::InternedString,
    pub inputs: Vec<Result<Expression<D>, SyntaxError<D>>>,
}

impl<D: Driver> IntrinsicExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for IntrinsicExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "(intrinsic \"{}\" {})",
            self.name.as_ref(),
            self.inputs
                .into_iter()
                .map(|result| result?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

pub struct IntrinsicExpressionSyntax;

impl<D: Driver> Syntax<D> for IntrinsicExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "intrinsic",
            |context, span, intrinsic_span, exprs, scope_set| async move {
                if exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`intrinsic` accepts at least 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let name = exprs.next().unwrap();
                let name_span = name.span;
                let name = match name.kind {
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

                Ok(IntrinsicExpression {
                    span,
                    intrinsic_span,
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
    Vec::new() // `intrinsic` is undocumented
}
