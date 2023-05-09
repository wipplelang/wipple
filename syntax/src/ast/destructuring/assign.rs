use crate::{
    ast::{
        destructuring::DestructuringSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse, Driver, File,
};

#[derive(Debug, Clone)]
pub struct AssignDestructuring<D: Driver> {
    pub span: D::Span,
    pub colon_span: D::Span,
    pub name_span: D::Span,
    pub name: D::InternedString,
    pub pattern: Result<Pattern<D>, SyntaxError<D>>,
}

impl<D: Driver> AssignDestructuring<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct AssignDestructuringSyntax;

impl<D: Driver> Syntax<D> for AssignDestructuringSyntax {
    type Context = DestructuringSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            ":",
            OperatorAssociativity::None,
            |context, span, (lhs_span, mut lhs_exprs), colon_span, (rhs_span, rhs_exprs), scope| async move {
                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);

                let pattern = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(
                        PatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                if lhs_exprs.len() != 1 {
                    context.ast_builder.driver.syntax_error(
                        lhs_span,
                        "expected name on left-hand side of destructuring pattern",
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let lhs = lhs_exprs.pop().unwrap();

                let name = match lhs.kind {
                    parse::ExprKind::Name(name, _) => name,
                    _ => {
                        context.ast_builder.driver.syntax_error(
                            lhs_span,
                            "expected name on left-hand side of destructuring pattern",
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                context.ast_builder.file.add_barrier(name, scope);

                Ok(AssignDestructuring {
                    span,
                    colon_span,
                    name_span: lhs.span,
                    name,
                    pattern,
                }
                .into())
            },
        ))
    }
}
