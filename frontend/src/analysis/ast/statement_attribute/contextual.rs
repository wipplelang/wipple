use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct ContextualStatementAttribute {
    pub span: SpanList,
}

impl ContextualStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct ContextualStatementAttributeSyntax;

impl Syntax for ContextualStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "context",
            |context, span, _specialize_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(
                            span,
                            "`contextual` does not accept parameters",
                        )],
                    );
                }

                let attribute = ContextualStatementAttribute { span };

                context.statement_attributes.unwrap().lock().contextual = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
