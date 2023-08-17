use crate::{
    ast::{
        format::Format,
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct WhereExpression<D: Driver> {
    pub span: D::Span,
    pub where_span: D::Span,
    pub value: Result<Box<Expression<D>>, SyntaxError<D>>,
    pub fields: Result<Box<Expression<D>>, SyntaxError<D>>,
}

impl<D: Driver> WhereExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for WhereExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} where {{\n{}\n}})",
            self.value?.format()?,
            self.fields?.format()?,
        ))
    }
}

pub struct WhereExpressionSyntax;

impl<D: Driver> Syntax<D> for WhereExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "where",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs_exprs), where_span, (rhs_span, rhs_exprs), scope_set| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let value = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        context.clone(),
                        lhs,
                        scope_set.clone(),
                    )
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let fields = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        context.clone(),
                        rhs,
                        scope_set,
                    )
                    .await;

                Ok(WhereExpression {
                    span,
                    where_span,
                    value: value.map(Box::new),
                    fields: fields.map(Box::new),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::WHERE]
}
