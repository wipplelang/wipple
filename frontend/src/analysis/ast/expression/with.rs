use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext, WithClause, WithClauseSyntax,
        WithClauseSyntaxContext,
    },
    diagnostics::Note,
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct WithExpression {
    pub span: SpanList,
    pub when_span: SpanList,
    pub clause: Result<WithClause, SyntaxError>,
    pub body: Result<Box<Expression>, SyntaxError>,
}

impl WithExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct WithExpressionSyntax;

impl Syntax for WithExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "with",
            |context, span, when_span, exprs, scope| async move {
                if exprs.len() != 2 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`with` accepts 2 inputs")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let clause = context
                    .ast_builder
                    .build_expr::<WithClauseSyntax>(
                        WithClauseSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        exprs.next().unwrap(),
                        scope,
                    )
                    .await;

                let body = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), exprs.next().unwrap(), scope)
                    .await;

                Ok(WithExpression {
                    span,
                    when_span,
                    clause,
                    body: body.map(Box::new),
                }
                .into())
            },
        ))
    }
}
