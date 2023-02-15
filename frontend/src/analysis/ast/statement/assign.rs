use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentPattern, AssignmentPatternSyntax, AssignmentPatternSyntaxContext,
        AssignmentValue, AssignmentValueSyntax, AssignmentValueSyntaxContext, Pattern,
        StatementAttributes, StatementSyntaxContext,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct AssignStatement {
    pub colon_span: Span,
    pub pattern: Result<AssignmentPattern, SyntaxError>,
    pub value: Result<AssignmentValue, SyntaxError>,
    pub attributes: StatementAttributes,
}

pub struct AssignStatementSyntax;

impl Syntax for AssignStatementSyntax {
    type Context = StatementSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ":",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let pattern = context
                    .ast_builder
                    .build_expr::<AssignmentPatternSyntax>(
                        AssignmentPatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope,
                    )
                    .await;

                let mut value_context = AssignmentValueSyntaxContext::new(context.ast_builder.clone())
                    .with_statement_attributes(context.statement_attributes.as_ref().unwrap().clone());

                if let Ok(AssignmentPattern::Pattern(pattern)) = &pattern {
                    if let Pattern::Name(pattern) = &pattern.pattern {
                        value_context = value_context.with_assigned_name(pattern.name, pattern.span);
                    }
                }

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let value = context
                    .ast_builder
                    .build_expr::<AssignmentValueSyntax>(
                        value_context,
                        rhs,
                        scope,
                    )
                    .await;

                Ok(AssignStatement {
                    colon_span: operator_span,
                    pattern,
                    value,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}
