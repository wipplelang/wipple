use crate::{
    analysis::ast::{
        statement::StatementSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        StatementAttributes, Type, TypeSyntax, TypeSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
    ScopeId,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct InstanceStatement {
    pub instance_span: Span,
    pub pattern_span: Span,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_scope: ScopeId,
    pub trait_parameters: Vec<Result<Type, SyntaxError>>,
    pub attributes: StatementAttributes,
}

impl InstanceStatement {
    pub fn span(&self) -> Span {
        Span::join(self.instance_span, self.pattern_span)
    }
}

pub struct InstanceStatementSyntax;

impl Syntax for InstanceStatementSyntax {
    type Context = StatementSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "instance",
            |context, span, mut exprs, scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`instance` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let input = exprs.pop().unwrap();
                let pattern_span = input.span;

                let (trait_name, trait_span, trait_scope, trait_parameters) =
                    match input.try_into_list_exprs() {
                        Ok((span, mut exprs)) => {
                            let trait_name = match exprs.next() {
                                Some(expr) => expr,
                                None => {
                                    context.ast_builder.compiler.add_error(
                                        "syntax error",
                                        vec![Note::primary(span, "`instance` requires a trait")],
                                    );

                                    return Err(context.ast_builder.syntax_error(span));
                                }
                            };

                            let trait_span = trait_name.span;

                            let (trait_name, trait_scope) = match trait_name.kind {
                                parse::ExprKind::Name(name, name_scope) => {
                                    (name, name_scope.unwrap_or(scope))
                                }
                                _ => {
                                    context.ast_builder.compiler.add_error(
                                        "syntax error",
                                        vec![Note::primary(span, "`instance` requires a trait")],
                                    );

                                    return Err(context.ast_builder.syntax_error(span));
                                }
                            };

                            let context = TypeSyntaxContext::new(context.ast_builder)
                                .with_statement_attributes(
                                    context.statement_attributes.as_ref().unwrap().clone(),
                                );

                            let params = stream::iter(exprs)
                                .then(|expr| {
                                    context.ast_builder.build_expr::<TypeSyntax>(
                                        context.clone(),
                                        expr,
                                        scope,
                                    )
                                })
                                .collect()
                                .await;

                            (trait_name, trait_span, trait_scope, params)
                        }
                        Err(expr) => match expr.kind {
                            parse::ExprKind::Name(name, name_scope) => {
                                (name, expr.span, name_scope.unwrap_or(scope), Vec::new())
                            }
                            _ => {
                                context.ast_builder.compiler.add_error(
                                    "syntax error",
                                    vec![Note::primary(span, "`instance` requires a trait")],
                                );

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        },
                    };

                Ok(InstanceStatement {
                    instance_span: span,
                    pattern_span,
                    trait_span,
                    trait_name,
                    trait_scope,
                    trait_parameters,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}