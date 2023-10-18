use crate::{
    ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct SnippetAssignmentPattern<D: Driver> {
    pub span: D::Span,
    pub snippet_span: D::Span,
    pub name_span: D::Span,
    pub name: D::InternedString,
}

impl<D: Driver> SnippetAssignmentPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for SnippetAssignmentPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("(snippet {:?})", self.name.as_ref()))
    }
}

pub struct SnippetAssignmentPatternSyntax;

impl<D: Driver> Syntax<D> for SnippetAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "snippet",
            |context, span, snippet_span, mut exprs, _scope_set| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`snippet` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let input = exprs.pop().unwrap();
                let name_span = input.span;

                let name = match input.kind {
                    parse::ExprKind::Text(text) => text.ignoring_escaped_underscores(),
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(name_span, "expected text");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                Ok(SnippetAssignmentPattern {
                    span,
                    snippet_span,
                    name_span,
                    name,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::INSTANCE]
}
