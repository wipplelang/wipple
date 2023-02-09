use crate::analysis::ast_v2::builtin::{
    pattern::PatternSyntaxContext,
    syntax::{OperatorAssociativity, Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct WherePattern {
    // TODO
}

pub struct WherePatternSyntax;

impl Syntax for WherePatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "where",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), (rhs_span, rhs_exprs)| async move {
                todo!();
            },
        ))
    }
}
