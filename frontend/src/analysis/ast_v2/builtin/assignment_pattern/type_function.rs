use crate::{
    analysis::ast_v2::builtin::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        syntax::{OperatorAssociativity, Syntax, SyntaxRule, SyntaxRules},
    },
    parse,
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentPattern {
    // TODO
}

pub struct TypeFunctionAssignmentPatternSyntax;

impl Syntax for TypeFunctionAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs), (rhs_span, rhs)| async move {
                // TODO: Parse lhs

                let rhs = parse::Expr::list(rhs_span, rhs);

                todo!()
            },
        ))
    }
}
