use crate::{
    analysis::ast_v2::{
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Pattern, PatternSyntaxContext,
    },
    helpers::InternedString,
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct AnnotatePattern {
    pub colon_span: Span,
    pub name_span: Span,
    pub name: InternedString,
    pub pattern: Result<Pattern, SyntaxError>,
}

pub struct AnnotatePatternSyntax;

impl Syntax for AnnotatePatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, (lhs_span, mut lhs_exprs), operator_span, (rhs_span, rhs_exprs)| async move {
                todo!()
            },
        ))
    }
}
