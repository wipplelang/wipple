use crate::{
    analysis::ast_v2::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        AssignmentValue, AssignmentValueSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentValue {
    pub arrow_span: Span,
    pub pattern: Result<TypePattern, SyntaxError>,
    pub value: Result<Box<AssignmentValue>, SyntaxError>,
}

pub struct TypeFunctionAssignmentValueSyntax;

impl Syntax for TypeFunctionAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs)| async move {
                let lhs = parse::Expr::list(lhs_span, lhs_exprs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                    )
                    .await;

                let rhs = parse::Expr::list(rhs_span, rhs_exprs);

                let value = context
                    .ast_builder
                    .build_expr::<AssignmentValueSyntax>(context.clone(), rhs)
                    .await;

                Ok(TypeFunctionAssignmentValue {
                    arrow_span: operator_span,
                    pattern,
                    value: value.map(Box::new),
                }
                .into())
            },
        ))
    }
}
