use crate::{
    analysis::ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentPattern, AssignmentPatternSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse::{self, SpanList},
    ScopeId,
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentPattern {
    pub span: SpanList,
    pub arrow_span: SpanList,
    pub type_pattern: Result<TypePattern, SyntaxError>,
    pub assignment_pattern: Result<Box<AssignmentPattern>, SyntaxError>,
    pub scope: ScopeId,
}

impl TypeFunctionAssignmentPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct TypeFunctionAssignmentPatternSyntax;

impl Syntax for TypeFunctionAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs), arrow_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.child_scope(scope);

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
                    span,
                    arrow_span,
                    type_pattern,
                    assignment_pattern: assignment_pattern.map(Box::new),
                    scope,
                }
                .into())
            },
        ))
    }
}
