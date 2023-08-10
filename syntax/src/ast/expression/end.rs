use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct EndExpression<D: Driver> {
    pub span: D::Span,
    pub end_span: D::Span,
    pub value: Result<Box<Expression<D>>, SyntaxError<D>>,
}

impl<D: Driver> EndExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for EndExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("(end {})", self.value?.format()?))
    }
}

pub struct EndExpressionSyntax;

impl<D: Driver> Syntax<D> for EndExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "end",
            |context, span, end_span, exprs, scope_set| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`end` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let value = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        context.clone(),
                        exprs.next().unwrap(),
                        scope_set,
                    )
                    .await;

                Ok(EndExpression {
                    span,
                    end_span,
                    value: value.map(Box::new),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::END]
}
