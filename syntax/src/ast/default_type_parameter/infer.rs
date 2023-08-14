use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        DefaultTypeParameterSyntaxContext, ScopeSet,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct InferDefaultTypeParameter<D: Driver> {
    pub span: D::Span,
    pub infer_span: D::Span,
    pub name: Result<(D::Span, D::InternedString, ScopeSet<D::Scope>), SyntaxError<D>>,
}

impl<D: Driver> InferDefaultTypeParameter<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for InferDefaultTypeParameter<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("(infer {})", self.name?.1.as_ref()))
    }
}

pub struct InferDefaultTypeParameterSyntax;

impl<D: Driver> Syntax<D> for InferDefaultTypeParameterSyntax {
    type Context = DefaultTypeParameterSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "infer",
            |context, span, infer_span, mut exprs, scope_set| async move {
                let name = if exprs.len() == 1 {
                    let expr = exprs.pop().unwrap();

                    match expr.kind {
                        parse::ExprKind::Name(name, scope) => Ok((
                            expr.span,
                            name,
                            scope.unwrap_or_else(|| scope_set.lock().clone()),
                        )),
                        _ => {
                            context
                                .ast_builder
                                .driver
                                .syntax_error(expr.span, "expected a name");

                            Err(context.ast_builder.syntax_error(expr.span))
                        }
                    }
                } else {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "expected a name");

                    Err(context.ast_builder.syntax_error(infer_span))
                };

                Ok(InferDefaultTypeParameter {
                    span,
                    infer_span,
                    name,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::INFER]
}
