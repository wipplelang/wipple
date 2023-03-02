use crate::{
    analysis::ast::{
        pattern::{Pattern, PatternSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        PatternSyntax,
    },
    parse::SpanList,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TuplePattern {
    pub span: SpanList,
    pub comma_span: SpanList,
    pub patterns: Vec<Result<Pattern, SyntaxError>>,
}

impl TuplePattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct TuplePatternSyntax;

impl Syntax for TuplePatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, span, (_span, exprs), comma_span, (_unused_span, unused_exprs), scope| async move {
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
                    span,
                    comma_span,
                    patterns,
                }
                .into())
            },
        ))
    }
}
