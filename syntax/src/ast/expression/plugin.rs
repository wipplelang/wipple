use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext,
    },
    parse, Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct PluginExpression<D: Driver> {
    pub span: D::Span,
    pub plugin_span: D::Span,
    pub path_span: D::Span,
    pub path: D::InternedString,
    pub name_span: D::Span,
    pub name: D::InternedString,
    pub inputs: Vec<Result<Expression<D>, SyntaxError<D>>>,
}

impl<D: Driver> PluginExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for PluginExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "(syntax \"{}\" \"{}\" {})",
            self.path.as_ref(),
            self.name.as_ref(),
            self.inputs
                .into_iter()
                .map(|result| result?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

pub struct PluginExpressionSyntax;

impl<D: Driver> Syntax<D> for PluginExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "syntax",
            |context, span, plugin_span, exprs, scope_set| async move {
                if exprs.len() < 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`syntax` for plugins accepts at least 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let namespace = exprs.next().unwrap();
                let path_span = namespace.span;
                let path = match namespace.kind {
                    parse::ExprKind::Text(text, _) => text,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected text here");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let identifier = exprs.next().unwrap();
                let name_span = identifier.span;
                let name = match identifier.kind {
                    parse::ExprKind::Text(text, _) => text,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "expected text here");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let inputs = stream::iter(exprs)
                    .then(|expr| {
                        context.ast_builder.build_expr::<ExpressionSyntax>(
                            context.clone(),
                            expr,
                            scope_set.clone(),
                        )
                    })
                    .collect()
                    .await;

                Ok(PluginExpression {
                    span,
                    plugin_span,
                    path_span,
                    path,
                    name_span,
                    name,
                    inputs,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    Vec::new() // This use of `syntax` is undocumented
}
