use crate::{
    analysis::ast::{
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        syntax_rule::SyntaxRuleSyntaxContext,
        Expression, Pattern,
    },
    parse::Span,
    ScopeId,
};

#[derive(Debug, Clone)]
pub struct FunctionSyntaxRule {
    pub arrow_span: Span,
    pub pattern: Result<Pattern, SyntaxError>,
    pub body: Result<Expression, SyntaxError>,
    pub scope: ScopeId,
}

pub struct FunctionSyntaxRuleSyntax;

impl Syntax for FunctionSyntaxRuleSyntax {
    type Context = SyntaxRuleSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, (lhs_span, lhs), operator_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.child_scope(scope);

                todo!()
            },
        ))
    }
}
