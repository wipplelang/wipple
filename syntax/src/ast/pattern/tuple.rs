use crate::{
    ast::{
        format::Format,
        pattern::{Pattern, PatternSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        PatternSyntax,
    },
    Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TuplePattern<D: Driver> {
    pub span: D::Span,
    pub comma_span: D::Span,
    pub patterns: Vec<Result<Pattern<D>, SyntaxError<D>>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for TuplePattern<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(TuplePattern {
            span: Default::default(),
            comma_span: Default::default(),
            patterns: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> TuplePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TuplePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({})",
            self.patterns
                .into_iter()
                .map(|result| result?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" , ")
        ))
    }
}

pub struct TuplePatternSyntax;

impl<D: Driver> Syntax<D> for TuplePatternSyntax {
    type Context = PatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
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

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::COMMA]
}