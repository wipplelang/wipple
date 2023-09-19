use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct SemanticsExpression<D: Driver> {
    pub span: D::Span,
    pub semantics_span: D::Span,
    pub semantics_text_span: D::Span,
    pub semantics_text: D::InternedString,
    pub value: Result<Box<Expression<D>>, SyntaxError<D>>,
}

impl<D: Driver> SemanticsExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for SemanticsExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "(semantics \"{}\" {})",
            self.semantics_text.as_ref(),
            self.value?.format()?
        ))
    }
}

pub struct SemanticsExpressionSyntax;

impl<D: Driver> Syntax<D> for SemanticsExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "semantics",
            |context, span, semantics_span, exprs, scope_set| async move {
                if exprs.len() != 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`semantics` accepts 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let semantics_text = exprs.next().unwrap();
                let semantics_text_span = semantics_text.span;
                let semantics_text = match semantics_text.kind {
                    parse::ExprKind::Text(text) => text.ignoring_escaped_underscores(),
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected text here");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let value = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        context.clone(),
                        exprs.next().unwrap(),
                        scope_set,
                    )
                    .await;

                Ok(SemanticsExpression {
                    span,
                    semantics_span,
                    semantics_text_span,
                    semantics_text,
                    value: value.map(Box::new),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::END]
}
