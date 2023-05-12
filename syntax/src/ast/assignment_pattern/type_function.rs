use crate::{
    ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        AssignmentPattern, AssignmentPatternSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse, Driver, File,
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentPattern<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub type_pattern: Result<TypePattern<D>, SyntaxError<D>>,
    pub assignment_pattern: Result<Box<AssignmentPattern<D>>, SyntaxError<D>>,
    pub scope: D::Scope,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for TypeFunctionAssignmentPattern<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(TypeFunctionAssignmentPattern {
            span: Default::default(),
            arrow_span: Default::default(),
            type_pattern: arbitrary::Arbitrary::arbitrary(u)?,
            assignment_pattern: arbitrary::Arbitrary::arbitrary(u)?,
            scope: Default::default(),
        })
    }
}

impl<D: Driver> TypeFunctionAssignmentPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct TypeFunctionAssignmentPatternSyntax;

impl<D: Driver> Syntax<D> for TypeFunctionAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs), arrow_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.file.make_scope(scope);

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
