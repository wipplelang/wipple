use crate::{
    analysis::ast::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentValue, AssignmentValueSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse::{self, Span},
    ScopeId,
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentValue {
    pub arrow_span: Span,
    pub pattern: Result<TypePattern, SyntaxError>,
    pub value: Result<Box<AssignmentValue>, SyntaxError>,
    pub scope: ScopeId,
}

impl TypeFunctionAssignmentValue {
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

pub struct TypeFunctionAssignmentValueSyntax;

impl Syntax for TypeFunctionAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                let scope = context.ast_builder.child_scope(scope);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope
                    )
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);

                let value = context
                    .ast_builder
                    .build_expr::<AssignmentValueSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(TypeFunctionAssignmentValue {
                    arrow_span: operator_span,
                    pattern,
                    value: value.map(Box::new),
                    scope,
                }
                .into())
            },
        ))
    }
}