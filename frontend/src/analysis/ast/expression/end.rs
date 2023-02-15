use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    diagnostics::Note,
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct EndExpression {
    pub end_span: Span,
    pub value: Result<Box<Expression>, SyntaxError>,
}

impl EndExpression {
    pub fn span(&self) -> Span {
        let value_span = match self.value {
            Ok(value) => value.span(),
            Err(error) => error.span,
        };

        Span::join(self.end_span, value_span)
    }
}

pub struct EndExpressionSyntax;

impl Syntax for EndExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "end",
            |context, span, mut exprs, scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`end` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let value = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), exprs.pop().unwrap(), scope)
                    .await;

                Ok(EndExpression {
                    end_span: span,
                    value: value.map(Box::new),
                }
                .into())
            },
        ))
    }
}
