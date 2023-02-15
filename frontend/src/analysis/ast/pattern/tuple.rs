use crate::{
    analysis::ast::{
        pattern::{Pattern, PatternSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        PatternSyntax,
    },
    parse::Span,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TuplePattern {
    pub comma_span: Span,
    pub patterns: Vec<Result<Pattern, SyntaxError>>,
}

impl TuplePattern {
    pub fn span(&self) -> Span {
        let first_pattern_span = match self.patterns.first().unwrap() {
            Ok(pattern) => pattern.span(),
            Err(error) => error.span,
        };

        if self.patterns.len() == 1 {
            Span::join(first_pattern_span, self.comma_span)
        } else {
            let last_pattern_span = match self.patterns.last().unwrap() {
                Ok(pattern) => pattern.span(),
                Err(error) => error.span,
            };

            Span::join(first_pattern_span, last_pattern_span)
        }
    }
}

pub struct TuplePatternSyntax;

impl Syntax for TuplePatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, (_span, exprs), operator_span, (_unused_span, unused_exprs), scope| async move {
                // HACK: All of the expressions are contained in `lhs`. In the
                // future, handle variadic operators specially.
                assert!(unused_exprs.is_empty());

                let patterns = stream::iter(exprs)
                    .then(|expr| {
                        context
                            .ast_builder
                            .build_expr::<PatternSyntax>(context.clone(), expr, scope)
                    })
                    .collect()
                    .await;

                Ok(TuplePattern {
                    comma_span: operator_span,
                    patterns,
                }
                .into())
            },
        ))
    }
}
