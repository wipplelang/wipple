use crate::{
    analysis::ast_v2::{
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        type_pattern::TypePatternSyntaxContext,
        Type, TypePattern, TypePatternSyntax, TypeSyntax, TypeSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct WhereTypePattern {
    pub where_span: Span,
    pub pattern: Result<Box<TypePattern>, SyntaxError>,
    pub bounds: Vec<Result<WhereTypePatternBound, SyntaxError>>,
}

#[derive(Debug, Clone)]
pub struct WhereTypePatternBound {
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub parameters: Vec<Result<Type, SyntaxError>>,
}

pub struct WhereTypePatternSyntax;

impl Syntax for WhereTypePatternSyntax {
    type Context = TypePatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "where",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs)| async move {
                let lhs = parse::Expr::list(lhs_span, lhs_exprs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                    )
                    .await;

                let bounds = stream::iter(rhs_exprs)
                    .then(|expr| async {
                        match expr.try_into_list_exprs() {
                            Ok((span, list)) => {
                                let mut exprs = list.into_iter();

                                let trait_name = match exprs.next() {
                                    Some(expr) => expr,
                                    None => {
                                        context.ast_builder.compiler.add_error(
                                            "syntax error",
                                            vec![Note::primary(rhs_span, "expected a trait here")],
                                        );

                                        return Err(context.ast_builder.syntax_error(span));
                                    }
                                };

                                let trait_span = trait_name.span;

                                let trait_name = match trait_name.kind {
                                    parse::ExprKind::Name(name) => name,
                                    _ => {
                                        context.ast_builder.compiler.add_error(
                                            "syntax error",
                                            vec![Note::primary(
                                                trait_span,
                                                "expected a trait here",
                                            )],
                                        );

                                        return Err(context.ast_builder.syntax_error(trait_span));
                                    }
                                };

                                let context = TypeSyntaxContext::new(context.ast_builder.clone())
                                    .with_statement_attributes(
                                        context.statement_attributes.as_ref().unwrap().clone(),
                                    );

                                let parameters = stream::iter(exprs)
                                    .then(|expr| {
                                        context
                                            .ast_builder
                                            .build_expr::<TypeSyntax>(context.clone(), expr)
                                    })
                                    .collect()
                                    .await;

                                Ok(WhereTypePatternBound {
                                    trait_span,
                                    trait_name,
                                    parameters,
                                })
                            }
                            Err(expr) => {
                                let name = match expr.kind {
                                    parse::ExprKind::Name(name) => name,
                                    _ => {
                                        context.ast_builder.compiler.add_error(
                                            "syntax error",
                                            vec![Note::primary(expr.span, "expected a bound here")],
                                        );

                                        return Err(context.ast_builder.syntax_error(expr.span));
                                    }
                                };

                                Ok(WhereTypePatternBound {
                                    trait_span: expr.span,
                                    trait_name: name,
                                    parameters: Vec::new(),
                                })
                            }
                        }
                    })
                    .collect()
                    .await;

                Ok(WhereTypePattern {
                    where_span: operator_span,
                    pattern: pattern.map(Box::new),
                    bounds,
                }
                .into())
            },
        ))
    }
}
