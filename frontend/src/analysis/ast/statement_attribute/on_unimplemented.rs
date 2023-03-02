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
pub struct OnUnimplementedStatementAttribute {
    pub span: SpanList,
    pub on_unimplemented_span: SpanList,
    pub message_span: SpanList,
    pub message: InternedString,
}

impl OnUnimplementedStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct OnUnimplementedStatementAttributeSyntax;

impl Syntax for OnUnimplementedStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "on-unimplemented",
            |context, span, on_unimplemented_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`on-unimplemented` accepts 1 input")],
                    );
                }

                let expr = exprs.pop().unwrap();
                let message = match expr.kind {
                    parse::ExprKind::Text(text) => text,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected text")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = OnUnimplementedStatementAttribute {
                    span,
                    on_unimplemented_span,
                    message_span: expr.span,
                    message,
                };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .on_unimplemented = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
