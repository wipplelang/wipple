use crate::{
    analysis::ast::{
        expression::ExpressionSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse::{self, SpanList},
    ScopeId,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct FunctionExpression {
    pub span: SpanList,
    pub arrow_span: SpanList,
    pub pattern: Result<Pattern, SyntaxError>,
    pub body: Result<Box<Expression>, SyntaxError>,
    pub scope: ScopeId,
}

impl FunctionExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct FunctionExpressionSyntax;

impl Syntax for FunctionExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (_lhs_span, lhs), arrow_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.child_scope(scope);

                let mut scopes = vec![scope];
                for _ in &lhs {
                    scopes.push(context.ast_builder.child_scope(*scopes.last().unwrap()));
                }

                let mut patterns = stream::iter(lhs)
                    .zip(stream::iter(scopes.iter().skip(1)))
                    .then(|(expr, &scope)| {
                        context.ast_builder.build_expr::<PatternSyntax>(
                            PatternSyntaxContext::new(context.ast_builder.clone())
                                .with_statement_attributes(
                                    context.statement_attributes.as_ref().unwrap().clone(),
                                ),
                            expr,
                            scope,
                        )
                    })
                    .collect::<Vec<_>>()
                    .await;

                let last_scope = scopes.pop().unwrap();
                let last_pattern = patterns.pop().unwrap();

                let rhs = parse::Expr::list(rhs_span, rhs);
                let body = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), rhs, last_scope)
                    .await;

                let function = patterns.into_iter().zip(scopes.into_iter().skip(1)).rfold(
                    FunctionExpression {
                        span,
                        arrow_span,
                        pattern: last_pattern,
                        body: body.map(Box::new),
                        scope: last_scope,
                    },
                    |function, (pattern, scope)| FunctionExpression {
                        span,
                        arrow_span,
                        pattern,
                        body: Ok(Box::new(function.into())),
                        scope,
                    },
                );

                Ok(function.into())
            },
        ))
    }
}
