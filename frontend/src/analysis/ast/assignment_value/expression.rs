use crate::{
    analysis::ast::{
        assignment_value::AssignmentValueSyntaxContext,
        expression::{Expression, ExpressionSyntax, ExpressionSyntaxContext},
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        AssignmentValue,
    },
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct ExpressionAssignmentValue {
    pub expression: Expression,
}

impl ExpressionAssignmentValue {
    pub fn span(&self) -> Span {
        self.expression.span()
    }
}

impl From<Expression> for AssignmentValue {
    fn from(expression: Expression) -> Self {
        ExpressionAssignmentValue { expression }.into()
    }
}

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
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}