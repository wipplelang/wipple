use crate::analysis::ast_v2::{
    assignment_value::AssignmentValueSyntaxContext,
    syntax::{OperatorAssociativity, Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentValue;

pub struct TypeFunctionAssignmentValueSyntax;

impl Syntax for TypeFunctionAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), span, (rhs_span, rhs_exprs)| async move {
                todo!();
            },
        ))
    }
}
