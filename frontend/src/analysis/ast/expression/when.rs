use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext, WhenBody, WhenBodySyntax,
        WhenBodySyntaxContext,
    },
    diagnostics::Note,
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct WhenExpression {
    pub span: SpanList,
    pub when_span: SpanList,
    pub input: Result<Box<Expression>, SyntaxError>,
    pub body: Result<WhenBody, SyntaxError>,
}

impl WhenExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}
pub struct WhenExpressionSyntax;

impl Syntax for WhenExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "when",
            |context, span, when_span, exprs, scope| async move {
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
                    span,
                    when_span,
                    input: input.map(Box::new),
                    body,
                }
                .into())
            },
        ))
    }
}
