use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct HelpStatementAttribute {
    pub span: SpanList,
    pub help_span: SpanList,
    pub help_text_span: SpanList,
    pub help_text: InternedString,
}

impl HelpStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct HelpStatementAttributeSyntax;

impl Syntax for HelpStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "help",
            |context, span, help_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`help` accepts 1 input")],
                    );
                }

                let expr = exprs.pop().unwrap();
                let help_text = match expr.kind {
                    parse::ExprKind::Text(text) => text,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected text")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = HelpStatementAttribute {
                    span,
                    help_span,
                    help_text_span: expr.span,
                    help_text,
                };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .help
                    .push(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
