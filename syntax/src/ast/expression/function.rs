use crate::{
    ast::{
        expression::ExpressionSyntaxContext,
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse, Driver, File,
};
use futures::{stream, StreamExt};
use std::collections::HashSet;
use wipple_util::Shared;

#[derive(Debug, Clone)]
pub struct FunctionExpression<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Result<Pattern<D>, SyntaxError<D>>,
    pub body: Result<Box<Expression<D>>, SyntaxError<D>>,
    pub scope_set: HashSet<D::Scope>,
}

impl<D: Driver> FunctionExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for FunctionExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} -> {})",
            self.pattern?.format()?,
            self.body?.format()?,
        ))
    }
}

pub struct FunctionExpressionSyntax;

impl<D: Driver> Syntax<D> for FunctionExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (_lhs_span, lhs), arrow_span, (rhs_span, rhs), scope_set| async move {
                let mut scope_set = scope_set.lock().clone();
                scope_set.insert(context.ast_builder.file.make_scope());
                let scope_set = Shared::new(scope_set);

                let mut scopes = vec![scope_set];
                for _ in &lhs {
                    let mut new = scopes.last().unwrap().lock().clone();
                    new.insert(context.ast_builder.file.make_scope());
                    let new = Shared::new(new);
                    scopes.push(new);
                }

                let mut patterns = stream::iter(lhs)
                    .zip(stream::iter(scopes.iter().skip(1)))
                    .then(|(expr, scope)| {
                        context.ast_builder.build_expr::<PatternSyntax>(
                            PatternSyntaxContext::new(context.ast_builder.clone())
                                .with_statement_attributes(
                                    context.statement_attributes.as_ref().unwrap().clone(),
                                ),
                            expr,
                            scope.clone(),
                        )
                    })
                    .collect::<Vec<_>>()
                    .await;

                let last_scope = scopes.pop().unwrap();
                let last_pattern = patterns.pop().unwrap();

                let rhs = parse::Expr::list(rhs_span, rhs);
                let body = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), rhs, last_scope.clone())
                    .await;

                let function = patterns.into_iter().zip(scopes.into_iter().skip(1)).rfold(
                    FunctionExpression {
                        span,
                        arrow_span,
                        pattern: last_pattern,
                        body: body.map(Box::new),
                        scope_set: last_scope.into_unique(),
                    },
                    |function, (pattern, scope_set)| FunctionExpression {
                        span,
                        arrow_span,
                        pattern,
                        body: Ok(Box::new(function.into())),
                        scope_set: scope_set.into_unique(),
                    },
                );

                Ok(function.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::FUNCTION]
}
