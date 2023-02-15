use crate::{
    analysis::ast::{
        expression::ExpressionSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse::{self, Span},
    ScopeId,
};

#[derive(Debug, Clone)]
pub struct FunctionExpression {
    pub arrow_span: Span,
    pub pattern: Result<Pattern, SyntaxError>,
    pub body: Result<Box<Expression>, SyntaxError>,
    pub scope: ScopeId,
}

impl FunctionExpression {
    pub fn span(&self) -> Span {
        let pattern_span = match self.pattern {
            Ok(pattern) => pattern.span(),
            Err(error) => error.span,
        };

        let body_span = match self.body {
            Ok(body) => body.span(),
            Err(error) => error.span,
        };

        Span::join(pattern_span, body_span)
    }
}

pub struct FunctionExpressionSyntax;

impl Syntax for FunctionExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, (lhs_span, lhs), operator_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.child_scope(scope);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs);
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

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs);
                let body = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(FunctionExpression {
                    arrow_span: operator_span,
                    pattern,
                    body: body.map(Box::new),
                    scope,
                }
                .into())
            },
        ))
    }
}
