use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct SpecializeStatementAttribute {
    pub span: SpanList,
}

impl SpecializeStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct SpecializeStatementAttributeSyntax;

impl Syntax for SpecializeStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "specialize",
            |context, span, _specialize_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(
                            span,
                            "`specialize` does not accept parameters",
                        )],
                    );
                }

                let attribute = SpecializeStatementAttribute { span };

                context.statement_attributes.unwrap().lock().specialize = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
