use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct KeywordStatementAttribute {
    pub span: Span,
}

pub struct KeywordStatementAttributeSyntax;

impl Syntax for KeywordStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "keyword",
            |context, span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`keyword` does not accept parameters")],
                    );
                }

                let attribute = KeywordStatementAttribute { span };

                context.statement_attributes.unwrap().lock().keyword = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
