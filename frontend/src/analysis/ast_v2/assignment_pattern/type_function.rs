use crate::{
    analysis::ast_v2::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        Type, TypePattern, TypePatternSyntax, TypePatternSyntaxContext, TypeSyntax,
        TypeSyntaxContext,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentPattern {
    pub arrow_span: Span,
    pub pattern: Result<TypePattern, SyntaxError>,
    pub ty: Result<Type, SyntaxError>,
}

pub struct TypeFunctionAssignmentPatternSyntax;

impl Syntax for TypeFunctionAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs), operator_span, (rhs_span, rhs)| async move {
                let lhs = parse::Expr::list(lhs_span, lhs);

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

                let rhs = parse::Expr::list(rhs_span, rhs);

                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        TypeSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                    )
                    .await;

                Ok(TypeFunctionAssignmentPattern {
                    arrow_span: operator_span,
                    pattern,
                    ty,
                }
                .into())
            },
        ))
    }
}
