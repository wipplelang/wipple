use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, ExpressionSyntaxContext, WithClauseSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct AssignWithClause {
    pub span: SpanList,
    pub colon_span: SpanList,
    pub name: Result<(SpanList, InternedString), SyntaxError>,
    pub value: Result<Box<Expression>, SyntaxError>,
}

impl AssignWithClause {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct AssignWithClauseSyntax;

impl Syntax for AssignWithClauseSyntax {
    type Context = WithClauseSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ":",
            OperatorAssociativity::None,
            |context, span, (lhs_span, mut lhs_exprs), colon_span, (rhs_span, rhs_exprs), scope| async move {
                let name = if lhs_exprs.len() == 1 {
                    let expr = lhs_exprs.pop().unwrap();

                    match expr.kind {
                        parse::ExprKind::Name(name, _) => Ok((expr.span, name)),
                        _ => {
                            context.ast_builder.compiler.add_error(
                                "syntax error",
                                vec![Note::primary(lhs_span, "expected a name")],
                            );

                            Err(context.ast_builder.syntax_error(lhs_span))
                        }
                    }
                } else {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(lhs_span, "expected a name")],
                    );

                    Err(context.ast_builder.syntax_error(lhs_span))
                };

                let value_context = ExpressionSyntaxContext::new(context.ast_builder.clone())
                    .with_statement_attributes(context.statement_attributes.unwrap());

                let value = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        value_context,
                        parse::Expr::list_or_expr(rhs_span, rhs_exprs),
                        scope
                    )
                    .await;

                Ok(AssignWithClause {
                    span,
                    colon_span,
                    name,
                    value: value.map(Box::new),
                }
                .into())
            },
        ))
    }
}
