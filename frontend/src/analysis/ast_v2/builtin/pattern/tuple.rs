use crate::analysis::ast_v2::builtin::{
    pattern::PatternSyntaxContext,
    syntax::{OperatorAssociativity, Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct TuplePattern {
    // TODO
}

pub struct TuplePatternSyntax;

impl Syntax for TuplePatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, (span, exprs), (_unused_span, unused_exprs)| async move {
                // HACK: All of the expressions are contained in `lhs`. In the
                // future, handle variadic operators specially.
                assert!(unused_exprs.is_empty());

                todo!()
            },
        ))
    }
}
