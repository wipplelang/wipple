use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        ConstantTypeAnnotation, ConstantTypeAnnotationSyntax, ConstantTypeAnnotationSyntaxContext,
        StatementAttributes, StatementSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct AnnotateStatement {
    pub colon_span: Span,
    pub name: Result<(Span, InternedString), SyntaxError>,
    pub annotation: Result<ConstantTypeAnnotation, SyntaxError>,
    pub attributes: StatementAttributes,
}

impl AnnotateStatement {
    pub fn span(&self) -> Span {
        let name_span = match self.name {
            Ok((span, _)) => span,
            Err(error) => error.span,
        };

        let annotation_span = match self.annotation {
            Ok(annotation) => annotation.span(),
            Err(error) => error.span,
        };

        Span::join(name_span, annotation_span)
    }
}

pub struct AnnotateStatementSyntax;

impl Syntax for AnnotateStatementSyntax {
    type Context = StatementSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "::",
            OperatorAssociativity::None,
            |context, (lhs_span, mut lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                let name = if lhs_exprs.len() == 1 {
                    let lhs = lhs_exprs.pop().unwrap();
                    match lhs.kind {
                        parse::ExprKind::Name(name, _) => Ok((lhs.span, name)),
                        _ => {
                            context.ast_builder.compiler.add_error(
                                "syntax error",
                                vec![
                                    Note::primary(lhs.span, "expected a name in constant declaration"),
                                    Note::secondary(lhs.span, "to annotate the type of an expression, wrap this in parentheses"),
                                ],
                            );

                            Err(context.ast_builder.syntax_error(lhs.span))
                        }
                    }
                } else {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![
                            Note::primary(lhs_span, "expected a name in constant declaration"),
                            Note::secondary(lhs_span, "to annotate the type of an expression, wrap this in parentheses"),
                        ],
                    );

                    Err(context.ast_builder.syntax_error(lhs_span))
                };

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let ty = context
                    .ast_builder
                    .build_expr::<ConstantTypeAnnotationSyntax>(
                        ConstantTypeAnnotationSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                Ok(AnnotateStatement {
                    colon_span: operator_span,
                    name,
                    annotation: ty,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}
