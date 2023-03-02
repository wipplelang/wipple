use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct AllowOverlappingInstancesStatementAttribute {
    pub span: SpanList,
}

impl AllowOverlappingInstancesStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct AllowOverlappingInstancesStatementAttributeSyntax;

impl Syntax for AllowOverlappingInstancesStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "allow-overlapping-instances",
            |context, span, _allow_overlapping_instances_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(
                            span,
                            "`allow-overlapping-instances` does not accept parameters",
                        )],
                    );
                }

                let attribute = AllowOverlappingInstancesStatementAttribute { span };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .allow_overlapping_instances = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
