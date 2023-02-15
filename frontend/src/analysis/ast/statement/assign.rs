use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentPattern, AssignmentPatternSyntax, AssignmentPatternSyntaxContext,
        AssignmentValue, AssignmentValueSyntax, AssignmentValueSyntaxContext, NamePattern, Pattern,
        PatternAssignmentPattern, StatementAttributes, StatementSyntaxContext,
    },
    helpers::Shared,
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct AssignStatement {
    pub colon_span: Span,
    pub pattern: Result<AssignmentPattern, SyntaxError>,
    pub value: Result<AssignmentValue, SyntaxError>,
    pub attributes: StatementAttributes,
}

impl AssignStatement {
    pub fn span(&self) -> Span {
        let pattern_span = match &self.pattern {
            Ok(pattern) => pattern.span(),
            Err(error) => error.span,
        };

        let value_span = match &self.value {
            Ok(value) => value.span(),
            Err(error) => error.span,
        };

        Span::join(pattern_span, value_span)
    }
}

pub struct AssignStatementSyntax;

impl Syntax for AssignStatementSyntax {
    type Context = StatementSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ":",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {

                let mut declared_name = None;
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let pattern = match &lhs.kind {
                    parse::ExprKind::Name(name, _) => {
                        declared_name = Some(*name);

                        Ok(AssignmentPattern::Pattern(PatternAssignmentPattern {
                            pattern: Pattern::Name(NamePattern {
                                span: lhs.span,
                                name: *name
                            }),
                        }))
                    }
                    _ => {
                        context
                            .ast_builder
                            .build_expr::<AssignmentPatternSyntax>(
                                AssignmentPatternSyntaxContext::new(context.ast_builder.clone())
                                    .with_statement_attributes(
                                        context.statement_attributes.as_ref().unwrap().clone(),
                                    ),
                                lhs,
                                scope,
                            )
                            .await
                    }
                };

                let mut value_context = AssignmentValueSyntaxContext::new(context.ast_builder.clone())
                    .with_statement_attributes(context.statement_attributes.as_ref().unwrap().clone());

                let did_create_syntax = Shared::new(false);
                if let Ok(AssignmentPattern::Pattern(pattern)) = &pattern {
                    if let Pattern::Name(pattern) = &pattern.pattern {
                        value_context = value_context.with_assigned_name(
                            pattern.name,
                            pattern.span,
                            scope,
                            did_create_syntax.clone()
                        );
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

                if let Some(name) = declared_name {
                    let did_create_syntax = did_create_syntax.into_unique();
                    if !did_create_syntax {
                        context.ast_builder.add_barrier(name, scope);
                    }
                }

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
