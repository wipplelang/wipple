use crate::{
    analysis::ast_v2::{
        syntax::{
            FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, ExpressionSyntaxContext, WhenBody, WhenBodySyntax,
        WhenBodySyntaxContext,
    },
    diagnostics::Note,
};

#[derive(Debug, Clone)]
pub struct WhenExpression {
    pub input: Result<Box<Expression>, SyntaxError>,
    pub body: Result<WhenBody, SyntaxError>,
}

pub struct WhenExpressionSyntax;

impl Syntax for WhenExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "end",
            |context, span, exprs, scope| async move {
                if exprs.len() != 2 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`when` accepts 2 inputs")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let input = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), exprs.next().unwrap(), scope)
                    .await;

                let body = context
                    .ast_builder
                    .build_expr::<WhenBodySyntax>(
                        WhenBodySyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        exprs.next().unwrap(),
                        scope,
                    )
                    .await;

                Ok(WhenExpression {
                    input: input.map(Box::new),
                    body,
                }
                .into())
            },
        ))
    }
}
