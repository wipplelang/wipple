use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Pattern, PatternSyntax, PatternSyntaxContext, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct AnnotatePattern {
    pub colon_span: Span,
    pub pattern: Result<Box<Pattern>, SyntaxError>,
    pub ty: Result<Type, SyntaxError>,
}

impl AnnotatePattern {
    pub fn span(&self) -> Span {
        let pattern_span = match &self.pattern {
            Ok(pattern) => pattern.span(),
            Err(error) => error.span,
        };

        let ty_span = match &self.ty {
            Ok(ty) => ty.span(),
            Err(error) => error.span,
        };

        Span::join(pattern_span, ty_span)
    }
}

pub struct AnnotatePatternSyntax;

impl Syntax for AnnotatePatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let pattern = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), lhs, scope)
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        TypeSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                Ok(AnnotatePattern {
                    colon_span: operator_span,
                    pattern: pattern.map(Box::new),
                    ty,
                }
                .into())
            },
        ))
    }
}
