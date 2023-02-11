use crate::{
    analysis::ast_v2::{
        r#type::{Type, TypeSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        TypeSyntax,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub arrow_span: Span,
    pub input: Result<Box<Type>, SyntaxError>,
    pub output: Result<Box<Type>, SyntaxError>,
}

pub struct FunctionTypeSyntax;

impl Syntax for FunctionTypeSyntax {
    type Context = TypeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs)| async move {
                let lhs = parse::Expr::list(lhs_span, lhs_exprs);
                let input = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), lhs)
                    .await;

                let rhs = parse::Expr::list(rhs_span, rhs_exprs);
                let output = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), rhs)
                    .await;

                Ok(FunctionType {
                    arrow_span: operator_span,
                    input: input.map(Box::new),
                    output: output.map(Box::new),
                }
                .into())
            },
        ))
    }
}
