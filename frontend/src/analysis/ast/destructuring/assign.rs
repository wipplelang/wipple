use crate::{
    analysis::ast::{
        destructuring::DestructuringSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Pattern, PatternSyntax, PatternSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct AssignDestructuring {
    pub span: SpanList,
    pub colon_span: SpanList,
    pub name_span: SpanList,
    pub name: InternedString,
    pub pattern: Result<Pattern, SyntaxError>,
}

impl AssignDestructuring {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct AssignDestructuringSyntax;

impl Syntax for AssignDestructuringSyntax {
    type Context = DestructuringSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
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
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(
                            lhs_span,
                            "expected name on left-hand side of destructuring pattern",
                        )],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let lhs = lhs_exprs.pop().unwrap();

                let name = match lhs.kind {
                    parse::ExprKind::Name(name, _) => name,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(
                                lhs_span,
                                "expected name on left-hand side of destructuring pattern",
                            )],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                context.ast_builder.add_barrier(name, scope);

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
