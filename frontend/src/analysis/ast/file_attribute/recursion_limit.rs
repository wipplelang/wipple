use crate::{
    analysis::ast::{
        file_attribute::FileAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct RecursionLimitFileAttribute {
    pub recursion_limit_span: Span,
    pub limit_span: Span,
    pub limit: usize,
}

impl RecursionLimitFileAttribute {
    pub fn span(&self) -> Span {
        Span::join(self.recursion_limit_span, self.limit_span)
    }
}

pub struct RecursionLimitFileAttributeSyntax;

impl Syntax for RecursionLimitFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "recursion-limit",
            |context, span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`recursion-limit` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let limit = match expr.kind {
                    parse::ExprKind::Number(number) => match number.parse::<usize>() {
                        Ok(limit) => limit,
                        Err(_) => {
                            context.ast_builder.compiler.add_error(
                                "syntax error",
                                vec![Note::primary(expr.span, "expected a positive integer")],
                            );

                            return Err(context.ast_builder.syntax_error(span));
                        }
                    },
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected a number")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = RecursionLimitFileAttribute {
                    recursion_limit_span: span,
                    limit_span: expr.span,
                    limit,
                };

                context.ast_builder.attributes.lock().recursion_limit = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}