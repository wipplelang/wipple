use crate::{
    analysis::ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentPattern, AssignmentPatternSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse::{self, Span},
    ScopeId,
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentPattern {
    pub arrow_span: Span,
    pub type_pattern: Result<TypePattern, SyntaxError>,
    pub assignment_pattern: Result<Box<AssignmentPattern>, SyntaxError>,
    pub scope: ScopeId,
}

impl TypeFunctionAssignmentPattern {
    pub fn span(&self) -> Span {
        let type_pattern_span = match self.type_pattern {
            Ok(type_pattern) => type_pattern.span(),
            Err(error) => error.span,
        };

        let assignment_pattern_span = match self.assignment_pattern {
            Ok(assignment_pattern) => assignment_pattern.span(),
            Err(error) => error.span,
        };

        Span::join(type_pattern_span, assignment_pattern_span)
    }
}

pub struct TypeFunctionAssignmentPatternSyntax;

impl Syntax for TypeFunctionAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs), operator_span, (rhs_span, rhs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs);

                let type_pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope,
                    )
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs);

                let assignment_pattern = context
                    .ast_builder
                    .build_expr::<AssignmentPatternSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(TypeFunctionAssignmentPattern {
                    arrow_span: operator_span,
                    type_pattern,
                    assignment_pattern: assignment_pattern.map(Box::new),
                    scope,
                }
                .into())
            },
        ))
    }
}
