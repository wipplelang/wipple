use crate::{
    ast::{
        assignment_value::AssignmentValueSyntaxContext,
        expression::{Expression, ExpressionSyntax, ExpressionSyntaxContext},
        format::Format,
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        AssignmentValue, SyntaxError,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct ExpressionAssignmentValue<D: Driver> {
    pub expression: Expression<D>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for ExpressionAssignmentValue<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(ExpressionAssignmentValue {
            expression: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> ExpressionAssignmentValue<D> {
    pub fn span(&self) -> D::Span {
        self.expression.span()
    }
}

impl<D: Driver> Format<D> for ExpressionAssignmentValue<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        self.expression.format()
    }
}

impl<D: Driver> From<Expression<D>> for AssignmentValue<D> {
    fn from(expression: Expression<D>) -> Self {
        ExpressionAssignmentValue { expression }.into()
    }
}

pub struct ExpressionAssignmentValueSyntax;

impl<D: Driver> Syntax<D> for ExpressionAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().combine(ExpressionSyntax::rules())
    }
}

impl<D: Driver> From<AssignmentValueSyntaxContext<D>> for ExpressionSyntaxContext<D> {
    fn from(context: AssignmentValueSyntaxContext<D>) -> Self {
        ExpressionSyntaxContext::new(context.ast_builder)
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
