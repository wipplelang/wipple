use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        type_pattern::TypePatternSyntaxContext,
        Type, TypePattern, TypePatternSyntax, TypeSyntax, TypeSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
    ScopeId,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct WhereTypePattern {
    pub where_span: Span,
    pub pattern: Result<Box<TypePattern>, SyntaxError>,
    pub bounds: Vec<Result<WhereTypePatternBound, SyntaxError>>,
}

impl WhereTypePattern {
    pub fn span(&self) -> Span {
        let pattern_span = match self.pattern {
            Ok(pattern) => pattern.span(),
            Err(error) => error.span,
        };

        let last_bound_span = match self.bounds.last().unwrap() {
            Ok(bound) => bound.span(),
            Err(error) => error.span,
        };

        Span::join(pattern_span, last_bound_span)
    }
}

#[derive(Debug, Clone)]
pub struct WhereTypePatternBound {
    pub span: Span,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_scope: ScopeId,
    pub parameters: Vec<Result<Type, SyntaxError>>,
}

impl WhereTypePatternBound {
    pub fn span(&self) -> Span {
        self.span
    }
}

pub struct WhereTypePatternSyntax;

impl Syntax for WhereTypePatternSyntax {
    type Context = TypePatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "where",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(context.clone(), lhs, scope)
                    .await;

                let bounds = stream::iter(rhs_exprs)
                    .then(|expr| async {
                        match expr.try_into_list_exprs() {
                            Ok((span, mut exprs)) => {
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

                                let (trait_name, trait_scope) = match trait_name.kind {
                                    parse::ExprKind::Name(name, name_scope) => (name, name_scope.unwrap_or(scope)),
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

                                let parameters = stream::iter(exprs)
                                    .then(|expr| {
                                        context.ast_builder.build_expr::<TypeSyntax>(
                                            TypeSyntaxContext::new(context.ast_builder.clone())
                                                .with_statement_attributes(
                                                    context
                                                        .statement_attributes
                                                        .as_ref()
                                                        .unwrap()
                                                        .clone(),
                                                ),
                                            expr,
                                            scope,
                                        )
                                    })
                                    .collect()
                                    .await;

                                Ok(WhereTypePatternBound {
                                    span,
                                    trait_span,
                                    trait_name,
                                    trait_scope,
                                    parameters,
                                })
                            }
                            Err(expr) => {
                                let (trait_name, trait_scope) = match expr.kind {
                                    parse::ExprKind::Name(name, name_scope) => (name, name_scope.unwrap_or(scope)),
                                    _ => {
                                        context.ast_builder.compiler.add_error(
                                            "syntax error",
                                            vec![Note::primary(expr.span, "expected a bound here")],
                                        );

                                        return Err(context.ast_builder.syntax_error(expr.span));
                                    }
                                };

                                Ok(WhereTypePatternBound {
                                    span: expr.span,
                                    trait_span: expr.span,
                                    trait_name,
                                    trait_scope,
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
