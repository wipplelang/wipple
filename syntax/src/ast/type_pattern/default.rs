use crate::ScopeSet;
use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Type, TypePatternSyntaxContext, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct DefaultTypePattern<D: Driver> {
    pub span: D::Span,
    pub colon_span: D::Span,
    pub name: Result<(D::Span, D::InternedString, ScopeSet<D::Scope>), SyntaxError<D>>,
    pub ty: Result<Type<D>, SyntaxError<D>>,
}

impl<D: Driver> DefaultTypePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for DefaultTypePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} : {})",
            self.name?.1.as_ref(),
            self.ty?.format()?
        ))
    }
}

pub struct DefaultTypePatternSyntax;

impl<D: Driver> Syntax<D> for DefaultTypePatternSyntax {
    type Context = TypePatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            ":",
            OperatorAssociativity::None,
            |context,
             span,
             (lhs_span, mut lhs_exprs),
             colon_span,
             (rhs_span, rhs_exprs),
             scope_set| async move {
                let name = if lhs_exprs.len() == 1 {
                    let expr = lhs_exprs.pop().unwrap();

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
                                .syntax_error(lhs_span, "expected a name");

                            Err(context.ast_builder.syntax_error(lhs_span))
                        }
                    }
                } else {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(lhs_span, "expected a name");

                    Err(context.ast_builder.syntax_error(lhs_span))
                };

                let type_context = TypeSyntaxContext::new(context.ast_builder.clone())
                    .with_statement_attributes(context.statement_attributes.unwrap());

                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        type_context,
                        parse::Expr::list_or_expr(rhs_span, rhs_exprs),
                        scope_set,
                    )
                    .await;

                Ok(DefaultTypePattern {
                    span,
                    colon_span,
                    name,
                    ty,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::ASSIGN]
}
