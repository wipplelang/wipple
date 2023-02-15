use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct HelpStatementAttribute {
    pub help_span: Span,
    pub help_text_span: Span,
    pub help_text: InternedString,
}

impl HelpStatementAttribute {
    pub fn span(&self) -> Span {
        Span::join(self.help_span, self.help_text_span)
    }
}

pub struct HelpStatementAttributeSyntax;

impl Syntax for HelpStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "help",
            |context, span, mut exprs, _scope| async move {
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
                    help_span: span,
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
