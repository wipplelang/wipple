use crate::{
    analysis::ast::{
        r#type::{Type, TypeSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        TypeSyntax,
    },
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub span: SpanList,
    pub arrow_span: SpanList,
    pub input: Result<Box<Type>, SyntaxError>,
    pub output: Result<Box<Type>, SyntaxError>,
}

impl FunctionType {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct FunctionTypeSyntax;

impl Syntax for FunctionTypeSyntax {
    type Context = TypeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (lhs_span, lhs_exprs), arrow_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let input = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), lhs, scope)
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let output = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(FunctionType {
                    span,
                    arrow_span,
                    input: input.map(Box::new),
                    output: output.map(Box::new),
                }
                .into())
            },
        ))
    }
}
