use crate::analysis::ast_v2::{
    assignment_value::AssignmentValueSyntaxContext,
    expression::{Expression, ExpressionSyntax, ExpressionSyntaxContext},
    syntax::{Syntax, SyntaxContext, SyntaxRules},
};

pub type ExpressionAssignmentValue = Expression;

pub struct ExpressionAssignmentValueSyntax;

impl Syntax for ExpressionAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().combine(ExpressionSyntax::rules())
    }
}

impl From<AssignmentValueSyntaxContext> for ExpressionSyntaxContext {
    fn from(context: AssignmentValueSyntaxContext) -> Self {
        ExpressionSyntaxContext::new(context.ast_builder)
    }
}
