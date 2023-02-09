use crate::analysis::ast_v2::builtin::{
    r#type::{Type, TypeSyntaxContext},
    syntax::{OperatorAssociativity, Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub input: Box<Type>,
    pub output: Box<Type>,
}

pub struct FunctionTypeSyntax;

impl Syntax for FunctionTypeSyntax {
    type Context = TypeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, (lhs_span, lhs), (rhs_span, rhs)| async move {
                // let lhs = context
                //     .builder
                //     .apply_syntax::<TypeSyntax>(context, lhs_span, lhs)
                //     .await?;

                // let rhs = context
                //     .builder
                //     .apply_syntax::<TypeSyntax>(context, rhs_span, rhs)
                //     .await?;

                // Ok(FunctionType {
                //     input: Box::new(lhs),
                //     output: Box::new(rhs),
                // })

                todo!()
            },
        ))
    }
}
