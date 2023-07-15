use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        when_pattern::WhenPatternSyntaxContext,
        Expression, ExpressionSyntax, ExpressionSyntaxContext, Pattern, PatternSyntax,
        PatternSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct WhereWhenPattern<D: Driver> {
    pub span: D::Span,
    pub where_span: D::Span,
    pub pattern: Result<Box<Pattern<D>>, SyntaxError<D>>,
    pub condition: Result<Box<Expression<D>>, SyntaxError<D>>,
}

impl<D: Driver> WhereWhenPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for WhereWhenPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} where {})",
            self.pattern?.format()?,
            self.condition?.format()?,
        ))
    }
}

pub struct WhereWhenPatternSyntax;

impl<D: Driver> Syntax<D> for WhereWhenPatternSyntax {
    type Context = WhenPatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "where",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs_exprs), where_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let pattern = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(
                        PatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope,
                    )
                    .await;

                let rhs = parse::Expr::list(rhs_span, rhs_exprs);
                let condition = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        ExpressionSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                Ok(WhereWhenPattern {
                    span,
                    where_span,
                    pattern: pattern.map(Box::new),
                    condition: condition.map(Box::new),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::WHERE]
}
