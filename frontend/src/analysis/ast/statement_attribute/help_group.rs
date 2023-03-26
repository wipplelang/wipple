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
pub struct HelpGroupStatementAttribute {
    pub span: SpanList,
    pub help_group_span: SpanList,
    pub help_group_text_span: SpanList,
    pub help_group_text: InternedString,
}

impl HelpGroupStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct HelpGroupStatementAttributeSyntax;

impl Syntax for HelpGroupStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "help-group",
            |context, span, help_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`help-group` accepts 1 input")],
                    );
                }

                let expr = exprs.pop().unwrap();
                let help_group_text = match expr.kind {
                    parse::ExprKind::Text(text) => text,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected text")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = HelpGroupStatementAttribute {
                    span,
                    help_group_span: help_span,
                    help_group_text_span: expr.span,
                    help_group_text,
                };

                context.statement_attributes.unwrap().lock().help_group = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
