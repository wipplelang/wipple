use crate::{
    analysis::ast::{
        destructuring::DestructuringSyntaxContext,
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        Pattern, PatternSyntax, PatternSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct AssignDestructuring {
    pub colon_span: Span,
    pub name_span: Span,
    pub name: InternedString,
    pub pattern: Result<Pattern, SyntaxError>,
}

pub struct AssignDestructuringSyntax;

impl Syntax for AssignDestructuringSyntax {
    type Context = DestructuringSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ":",
            OperatorAssociativity::None,
            |context, (lhs_span, mut lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                if lhs_exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(
                            lhs_span,
                            "expected name on left-hand side of destructuring pattern",
                        )],
                    );

                    return Err(context.ast_builder.syntax_error(operator_span));
                }

                let lhs = lhs_exprs.pop().unwrap();

                let name = match lhs.kind {
                    parse::ExprKind::Name(name) => name,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(
                                lhs_span,
                                "expected name on left-hand side of destructuring pattern",
                            )],
                        );

                        return Err(context.ast_builder.syntax_error(operator_span));
                    }
                };

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

                Ok(AssignDestructuring {
                    colon_span: operator_span,
                    name_span: lhs.span,
                    name,
                    pattern,
                }
                .into())
            },
        ))
    }
}
