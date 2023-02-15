use crate::{
    analysis::ast::{
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        syntax_rule::SyntaxRuleSyntaxContext,
        SyntaxPattern, SyntaxPatternSyntax, SyntaxPatternSyntaxContext,
    },
    parse::{self, Span},
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct FunctionSyntaxRule {
    pub arrow_span: Span,
    pub pattern: Result<Vec<SyntaxPattern>, SyntaxError>,
    pub body: Result<Box<SyntaxPattern>, SyntaxError>,
}

pub struct FunctionSyntaxRuleSyntax;

impl Syntax for FunctionSyntaxRuleSyntax {
    type Context = SyntaxRuleSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, (_lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                let input = stream::iter(lhs_exprs)
                    .then(|expr| {
                        context
                            .ast_builder
                            .build_expr::<SyntaxPatternSyntax>(
                                SyntaxPatternSyntaxContext::new(context.ast_builder.clone())
                                    .with_statement_attributes(
                                        context.statement_attributes.as_ref().unwrap().clone(),
                                    ),
                                expr,
                                scope
                            )
                    })
                    .collect::<Vec<_>>()
                    .await
                    .into_iter()
                    .collect();

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let output = context
                    .ast_builder
                    .build_expr::<SyntaxPatternSyntax>(
                        SyntaxPatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope
                    )
                    .await;

                Ok(FunctionSyntaxRule {
                    arrow_span: operator_span,
                    pattern: input,
                    body: output.map(Box::new),
                }
                .into())
            },
        ))
    }
}
