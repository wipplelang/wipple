use crate::{
    ast::{
        expression::ExpressionSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse, Driver, File,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct FunctionExpression<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Result<Pattern<D>, SyntaxError<D>>,
    pub body: Result<Box<Expression<D>>, SyntaxError<D>>,
    pub scope: D::Scope,
}

impl<D: Driver> FunctionExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct FunctionExpressionSyntax;

impl<D: Driver> Syntax<D> for FunctionExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (_lhs_span, lhs), arrow_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.file.make_scope(scope);

                let mut scopes = vec![scope];
                for _ in &lhs {
                    scopes.push(context.ast_builder.file.make_scope(*scopes.last().unwrap()));
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
