use crate::{
    analysis::ast_v2::{
        statement::StatementSyntaxContext,
        syntax::{
            FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        StatementAttributes, Type, TypeSyntax, TypeSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct InstanceStatement {
    pub instance_span: Span,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_parameters: Vec<Result<Type, SyntaxError>>,
    pub attributes: StatementAttributes,
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
                let trait_span = input.span;

                let (trait_name, trait_parameters) = match input.try_into_list_exprs() {
                    Ok((span, exprs)) => {
                        let mut exprs = exprs.into_iter();

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

                        let trait_name = match trait_name.kind {
                            parse::ExprKind::Name(name) => name,
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

                        (trait_name, params)
                    }
                    Err(expr) => match expr.kind {
                        parse::ExprKind::Name(name) => (name, Vec::new()),
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
                    trait_span,
                    trait_name,
                    trait_parameters,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}
