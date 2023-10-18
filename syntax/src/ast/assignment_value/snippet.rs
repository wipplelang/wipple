use crate::{
    ast::{
        assignment_value::AssignmentValueSyntaxContext,
        format::Format,
        syntax::{Syntax, SyntaxRules},
        AssignmentValue, SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct SnippetAssignmentValue<D: Driver> {
    pub name: Option<D::InternedString>,
    pub expression: parse::Expr<D>,
    pub wrap: bool,
}

impl<D: Driver> SnippetAssignmentValue<D> {
    pub fn span(&self) -> D::Span {
        self.expression.span
    }
}

impl<D: Driver> Format<D> for SnippetAssignmentValue<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        self.expression.format()
    }
}

impl<D: Driver> From<parse::Expr<D>> for AssignmentValue<D> {
    fn from(expression: parse::Expr<D>) -> Self {
        SnippetAssignmentValue {
            name: None,
            expression,
            wrap: false,
        }
        .into()
    }
}

pub struct SnippetAssignmentValueSyntax;

impl<D: Driver> Syntax<D> for SnippetAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new()
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
